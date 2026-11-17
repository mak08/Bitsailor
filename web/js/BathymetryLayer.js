// Static LAT clearance display. No forecast time dependency and no routing filter.
const ROOT = '/bathymetry/channel-2024';
const UNKNOWN = 0xffff;

export function combineDepths(values) {
    // A coarse pixel retains the shallowest cell and any uncertainty. Never
    // average away a shoal or treat missing neighbours as deep water.
    let depth = 0x7fff, uncertain = false;
    for (const value of values) {
        if (value === UNKNOWN) uncertain = true;
        else {
            depth = Math.min(depth, value & 0x7fff);
            uncertain ||= Boolean(value & 0x8000);
        }
    }
    return depth === 0x7fff ? UNKNOWN : depth | (uncertain ? 0x8000 : 0);
}

export function depthColour(value, required, hatch) {
    if (value === UNKNOWN) return hatch ? [90, 90, 90, 145] : [160, 160, 160, 25];
    const shallow = (value & 0x7fff) / 100 < required;
    if ((value & 0x8000) && hatch) return [90, 90, 90, 160];
    return shallow ? [220, 35, 35, 125] : [0, 0, 0, 0];
}

function pyramid(data, size) {
    const levels = [{data, size}];
    while (size > 1) {
        const nextSize = Math.ceil(size / 2), next = new Uint16Array(nextSize * nextSize);
        for (let y = 0; y < nextSize; y++) for (let x = 0; x < nextSize; x++) {
            const values = [];
            for (let dy = 0; dy < 2; dy++) for (let dx = 0; dx < 2; dx++) {
                const xx = x * 2 + dx, yy = y * 2 + dy;
                if (xx < size && yy < size) values.push(data[yy * size + xx]);
            }
            next[y * nextSize + x] = combineDepths(values);
        }
        data = next; size = nextSize; levels.push({data, size});
    }
    return levels;
}

export default class BathymetryLayer {
    constructor(map, L) {
        this.map = map;
        this.cache = new Map();
        this.required = 2.5;
        this.active = false;
        this.failed = false;
        this.loading = false;
        this.probeVersion = 0;
        const owner = this;
        map.createPane('depthPane');
        map.getPane('depthPane').style.zIndex = 240;
        map.getPane('depthPane').style.pointerEvents = 'none';
        const Tiles = L.GridLayer.extend({
            createTile(coords, done) {
                const canvas = document.createElement('canvas');
                canvas.width = canvas.height = 256;
                owner.render(canvas, coords).then(() => done(null, canvas), error => {
                    owner.failed = true;
                    if (owner.active) {
                        owner.status.textContent = 'Depth data unavailable';
                        owner.status.title = error.message;
                    }
                    // Failed tiles remain hatched, never silently transparent.
                    const ctx = canvas.getContext('2d');
                    ctx.fillStyle = 'rgba(100,100,100,.25)'; ctx.fillRect(0, 0, 256, 256);
                    ctx.strokeStyle = '#777';
                    for (let i = -256; i < 512; i += 12) {
                        ctx.beginPath(); ctx.moveTo(i, 0); ctx.lineTo(i + 256, 256); ctx.stroke();
                    }
                    done(null, canvas);
                });
                return canvas;
            }
        });
        this.tiles = new Tiles({pane: 'depthPane', tileSize: 256, minZoom: 7,
            bounds: [[48, -6.5], [52, 3.5]], noWrap: true, keepBuffer: 0,
            attribution: '<a href="https://emodnet.ec.europa.eu/en/bathymetry">EMODnet Bathymetry Consortium (2024)</a> · CC BY 4.0'});
        this.tiles.on('loading', () => { this.loading = true; this.updateStatus(); });
        this.tiles.on('load', () => { this.loading = false; this.updateStatus(); });
        this.outline = L.rectangle([[48, -6.375], [52, 3.5]], {
            pane: 'depthPane', color: '#666', weight: 1, dashArray: '5 5', fill: false, interactive: false});
        const control = L.control({position: 'bottomleft'});
        control.onAdd = () => {
            const box = L.DomUtil.create('div', 'depth-control leaflet-control');
            box.innerHTML = `<details><summary>Depth clearance · LAT</summary>
                <label><input type="checkbox" class="depth-enabled"> Show Channel depths</label>
                <label>Draft <input class="depth-draft" type="number" min="0" max="30" step="0.1" value="2"> m</label>
                <label>Clearance <input class="depth-margin" type="number" min="0" max="30" step="0.1" value="0.5"> m</label>
                <button type="button" class="depth-channel">View Channel</button>
                <p class="depth-status" aria-live="polite">Overlay off</p>
                <p class="depth-probe">Move the pointer over the map for cell depth.</p>
                <p><span class="depth-red">■</span> Below required depth · ▧ Missing/extrapolated<br>
                Unshaded within coverage: meets depth threshold.<br>
                Channel coverage: 48–52°N, 6.375°W–3.5°E.</p>
                <p>Static LAT; no tide allowance. ~115 m north–south cells.<br>
                Regional planning only. EMODnet data must not be used<br>for navigation or safety at sea. Routing does not use depths.</p>
                <a href="https://emodnet.ec.europa.eu/en/bathymetry" target="_blank" rel="noopener">EMODnet DTM 2024</a>
                </details>`;
            L.DomEvent.disableClickPropagation(box);
            L.DomEvent.disableScrollPropagation(box);
            this.status = box.querySelector('.depth-status');
            this.probe = box.querySelector('.depth-probe');
            const enabled = box.querySelector('.depth-enabled');
            const draft = box.querySelector('.depth-draft'), margin = box.querySelector('.depth-margin');
            const update = () => {
                if (!draft.checkValidity() || !margin.checkValidity() || draft.value === '' || margin.value === '') {
                    enabled.checked = false;
                    this.setEnabled(false);
                    this.status.textContent = 'Enter draft and clearance between 0 and 30 m';
                    return;
                }
                this.required = Number(draft.value) + Number(margin.value);
                this.setEnabled(enabled.checked);
            };
            enabled.addEventListener('change', update);
            draft.addEventListener('change', update);
            margin.addEventListener('change', update);
            box.querySelector('.depth-channel').addEventListener('click', () => {
                enabled.checked = true;
                update();
                map.fitBounds([[48, -6.375], [52, 3.5]], {maxZoom: 8});
            });
            return box;
        };
        control.addTo(map);
        map.on('zoomend', () => this.updateStatus());
        map.on('mousemove', event => this.inspect(event.latlng));
    }

    updateStatus() {
        this.status.textContent = !this.active ? 'Overlay off' : this.failed ? 'Depth data unavailable' : this.map.getZoom() < 7
            ? 'Zoom in to display depths (level 7+)' : this.loading ? 'Loading depths…'
                : `Red: depth < ${this.required.toFixed(1)} m at LAT`;
        this.status.title = '';
    }

    setEnabled(enabled) {
        this.active = enabled;
        if (enabled) {
            this.failed = false;
            if (!this.map.hasLayer(this.tiles)) this.tiles.addTo(this.map);
            else this.tiles.redraw();
            this.outline.addTo(this.map);
        } else {
            this.map.removeLayer(this.tiles);
            this.map.removeLayer(this.outline);
            ++this.probeVersion;
            this.probe.textContent = 'Move the pointer over the map for cell depth.';
        }
        this.updateStatus();
    }

    async inspect({lat, lng}) {
        const version = ++this.probeVersion;
        if (!this.active) return;
        if (lat < 48 || lat >= 52 || lng < -6.375 || lng >= 3.5) {
            this.probe.textContent = 'Outside Channel depth coverage'; return;
        }
        const x = Math.floor(lng * 2), y = Math.floor(lat * 2);
        const cached = this.cache.get(`${x}_${y}`);
        if (!cached) this.probe.textContent = 'Loading cell depth…';
        try {
            const levels = await (cached || this.chunk(x, y));
            if (version !== this.probeVersion) return;
            const column = Math.floor((lng - x / 2) * 960);
            const row = Math.floor(((y + 1) / 2 - lat) * 960);
            const value = levels[0].data[Math.min(479, row) * 480 + column];
            if (value === UNKNOWN) this.probe.textContent = 'Cell depth unknown';
            else {
                const depth = (value & 0x7fff) / 100;
                const qualifier = depth === 327.66 ? '≥' : depth === 0 ? '≤' : '';
                this.probe.textContent = `Cell minimum: ${qualifier}${depth.toFixed(2)} m LAT${value & 0x8000 ? ' · extrapolated/uncertain' : ''}`;
            }
        } catch (_) {
            if (version === this.probeVersion) this.probe.textContent = 'Cell depth unavailable';
        }
    }

    async chunk(x, y) {
        if (x < -13 || x >= 7 || y < 96 || y >= 104) return null;
        const key = `${x}_${y}`;
        if (this.cache.has(key)) {
            const value = this.cache.get(key);
            this.cache.delete(key); this.cache.set(key, value);
            return value;
        }
        const promise = (async () => {
            const response = await fetch(`${ROOT}/${key}.bin.gz`);
            if (!response.ok) throw new Error(`Bathymetry: HTTP ${response.status}`);
            const buffer = await new Response(response.body.pipeThrough(new DecompressionStream('gzip'))).arrayBuffer();
            if (buffer.byteLength !== 480 * 480 * 2) throw new Error('Incomplete bathymetry tile');
            const view = new DataView(buffer), values = new Uint16Array(480 * 480);
            for (let i = 0; i < values.length; i++) values[i] = view.getUint16(i * 2, true);
            return pyramid(values, 480);
        })();
        this.cache.set(key, promise);
        try {
            const result = await promise;
            // Bounded decoded cache; visible tiles have their own temporary references.
            while (this.cache.size > 40) this.cache.delete(this.cache.keys().next().value);
            return result;
        } catch (error) {
            this.cache.delete(key); throw error;
        }
    }

    async render(canvas, coords) {
        const required = this.required, z = coords.z;
        const lat = [], lon = [];
        for (let i = 0; i <= 256; i++) {
            lat.push(this.map.unproject([coords.x * 256, coords.y * 256 + i], z).lat);
            lon.push(this.map.unproject([coords.x * 256 + i, coords.y * 256], z).lng);
        }
        const chunks = new Map(), requests = [];
        for (let y = Math.max(96, Math.floor(lat[256] * 2)); y <= Math.min(103, Math.floor(lat[0] * 2)); y++) {
            for (let x = Math.max(-13, Math.floor(lon[0] * 2)); x <= Math.min(6, Math.floor(lon[256] * 2)); x++) {
                requests.push(this.chunk(x, y).then(data => chunks.set(`${x}_${y}`, data)));
            }
        }
        await Promise.all(requests);
        const ctx = canvas.getContext('2d'), image = ctx.createImageData(256, 256);
        for (let py = 0; py < 256; py++) for (let px = 0; px < 256; px++) {
            if (lon[px + 1] <= -6.5 || lon[px] >= 3.5 || lat[py] <= 48 || lat[py + 1] >= 52) continue;
            const values = [];
            // Every native cell intersecting a screen pixel contributes, using
            // conservative pyramid blocks at small scales (no bilinear smoothing).
            for (let y = Math.max(96, Math.floor(lat[py + 1] * 2)); y <= Math.min(103, Math.floor(lat[py] * 2)); y++) {
                for (let x = Math.max(-13, Math.floor(lon[px] * 2)); x <= Math.min(6, Math.floor(lon[px + 1] * 2)); x++) {
                    const levels = chunks.get(`${x}_${y}`);
                    const level = Math.min(levels.length - 1, Math.max(0,
                        Math.floor(Math.log2(Math.max(lon[px + 1] - lon[px], lat[py] - lat[py + 1]) * 960))));
                    const {data, size} = levels[level], scale = 960 / 2 ** level;
                    const x0 = Math.max(0, Math.floor((lon[px] - x / 2) * scale));
                    const x1 = Math.min(size - 1, Math.floor((lon[px + 1] - x / 2) * scale));
                    const y0 = Math.max(0, Math.floor(((y + 1) / 2 - lat[py]) * scale));
                    const y1 = Math.min(size - 1, Math.floor(((y + 1) / 2 - lat[py + 1]) * scale));
                    for (let yy = y0; yy <= y1; yy++) for (let xx = x0; xx <= x1; xx++) values.push(data[yy * size + xx]);
                }
            }
            const colour = depthColour(combineDepths(values), required, (px + py) % 10 < 2);
            image.data.set(colour, (py * 256 + px) * 4);
        }
        ctx.putImageData(image, 0, 0);
    }
}
