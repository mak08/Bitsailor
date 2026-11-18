import {decodeGRIB2ArrayBuffer} from './JSGrib/JSGrib.js?v=current-gribs-20261118';
import {ensureServerSettings, loadForecastCycle} from './ServerSettings.js?v=router-forecast-contract-20261031';

const wrap = value => ((value % 360) + 360) % 360;
const units = new Map([[0, 60000], [1, 3600000], [2, 86400000],
    [10, 10800000], [11, 21600000], [12, 43200000], [13, 1000]]);
function validTime(message) {
    const product = message.PRODUCT_DEFINITION_SECTION;
    const unit = units.get(product.indicatorOfUnitOfTimeRange);
    if (!unit) throw new Error('Unsupported forecast time unit');
    return Date.parse(message.timestamp()) + product.forecastTime * unit;
}
function component(message) {
    const p = message.PRODUCT_DEFINITION_SECTION;
    if (message.discipline === 0 && p.parameterCategory === 2 && [2, 3].includes(p.parameterNumber)) {
        return {kind: 'wind', name: p.parameterNumber === 2 ? 'u' : 'v'};
    }
    if (message.discipline !== 10) return;
    const name = p.parameterCategory === 1 ? ({2: 'u', 3: 'v'})[p.parameterNumber]
        : p.parameterCategory === 3 ? ({14: 'u', 15: 'v'})[p.parameterNumber] : undefined;
    return name ? {kind: 'current', name} : undefined;
}
function bracket(frames, time) {
    if (!frames.length) throw new Error('No forecast frames');
    const before = frames.findLast(frame => frame.time <= time) || frames[0];
    const after = frames.find(frame => frame.time >= time) || frames.at(-1);
    return {before, after};
}
function remember(cache, key, value, limit) {
    cache.delete(key); cache.set(key, value);
    while (cache.size > limit) cache.delete(cache.keys().next().value);
    return value;
}

// Use the grid's own origin, spacing and scan directions for regional and
// global fields. Speed is interpolated separately from the U/V components.
export function sampleField(field, lat, lon, kind = 'current') {
    const grid = field.grid;
    const scale = grid.basicAngleOfTheInitialProductionDomain && grid.subdivisionsOfBasicAngle
        && grid.subdivisionsOfBasicAngle !== 0xffffffff
        ? grid.basicAngleOfTheInitialProductionDomain / grid.subdivisionsOfBasicAngle : 1e-6;
    const dx = grid.iDirectionIncrement * scale, dy = grid.jDirectionIncrement * scale;
    const scan = grid.scanningMode;
    if (grid.gridDefinitionTemplateNumber !== 0 || scan & 0x3f || !(dx > 0 && dy > 0)) {
        throw new Error('Unsupported GRIB grid or scanning mode');
    }
    let x = wrap((scan & 128) ? grid.longitudeOfFirstGridPoint * scale - lon
        : lon - grid.longitudeOfFirstGridPoint * scale) / dx;
    let y = ((scan & 64) ? lat - grid.latitudeOfFirstGridPoint * scale
        : grid.latitudeOfFirstGridPoint * scale - lat) / dy;
    const periodic = Math.abs(grid.Ni * dx - 360) < dx * 0.01;
    const epsilon = 1e-7;
    if (y < -epsilon || y > grid.Nj - 1 + epsilon || (!periodic && x > grid.Ni - 1 + epsilon)) {
        return {u: 0, v: 0, magnitude: 0, valid: false};
    }
    x = Math.max(0, Math.min(x, periodic ? grid.Ni : grid.Ni - 1));
    y = Math.max(0, Math.min(y, grid.Nj - 1));
    const x0 = Math.floor(x) % grid.Ni, x1 = periodic ? (x0 + 1) % grid.Ni : Math.min(x0 + 1, grid.Ni - 1);
    const y0 = Math.floor(y), y1 = Math.min(y0 + 1, grid.Nj - 1);
    const fx = x - Math.floor(x), fy = y - y0;
    let u = 0, v = 0, magnitude = 0, weight = 0;
    for (const [row, wy] of [[y0, 1 - fy], [y1, fy]]) {
        for (const [column, wx] of [[x0, 1 - fx], [x1, fx]]) {
            const i = row * grid.Ni + column, w = wx * wy;
            if (w <= 1e-12) continue;
            if (Number.isFinite(field.u[i]) && Number.isFinite(field.v[i])) {
                u += w * field.u[i]; v += w * field.v[i];
                magnitude += w * Math.hypot(field.u[i], field.v[i]); weight += w;
            }
        }
    }
    // Currents: missing U or V makes the whole node zero, without reweighting.
    // Wind: omit missing nodes and renormalize, as the router does for AROME.
    if (kind === 'wind' && weight > 0) {u /= weight; v /= weight; magnitude /= weight;}
    return {u, v, magnitude, valid: weight > 0};
}

export default class GribCache {
    // Regional files contain all forecast times. Keep compressed files and
    // lightweight indices separately from decoded U/V pairs, shared by both overlays.
    #cycles = new Map();
    #files = new Map();
    #frames = new Map();
    #pending = new Map();
    #downloads = 0;
    #queue = [];

    async file(path) {
        if (this.#files.has(path)) return remember(this.#files, path, this.#files.get(path), 4);
        if (this.#pending.has(path)) return this.#pending.get(path);
        const pending = this.loadFile(path).then(file => remember(this.#files, path, file, 4));
        this.#pending.set(path, pending);
        try {return await pending;}
        finally {this.#pending.delete(path);}
    }

    async loadFile(path) {
        // Shared downloads survive view changes; stale views cannot repaint.
        // At most two files transfer concurrently, including during slider drags.
        if (this.#downloads >= 2) await new Promise(resolve => this.#queue.push(resolve));
        else this.#downloads++;
        try {
            const response = await fetch(`/weather/${path}`, {signal: AbortSignal.timeout(60000)});
            if (!response.ok) throw new Error(`GRIB request failed: HTTP ${response.status} (${path})`);
            const bytes = await response.arrayBuffer();
            const messages = decodeGRIB2ArrayBuffer(bytes, {decodeData: false}).messages;
            let offset = 0;
            const frames = new Map();
            for (const message of messages) {
                const part = component(message);
                if (part) {
                    const time = validTime(message), key = `${part.kind}/${time}`;
                    const frame = frames.get(key) || {time, path, kind: part.kind};
                    frame[part.name] = {offset, length: message.totalLength};
                    frames.set(key, frame);
                }
                offset += message.totalLength;
            }
            const index = [...frames.values()].filter(frame => frame.u && frame.v).sort((a, b) => a.time - b.time);
            if (!index.length) throw new Error('GRIB is missing U/V pairs');
            return {bytes, index};
        } finally {
            const next = this.#queue.shift();
            if (next) next();
            else this.#downloads--;
        }
    }

    async frame(entry, file) {
        const key = `${entry.kind}/${entry.path}/${entry.time}`;
        if (this.#frames.has(key)) return remember(this.#frames, key, this.#frames.get(key), 8);
        const decode = range => decodeGRIB2ArrayBuffer(
            new Uint8Array(file.bytes, range.offset, range.length)).messages[0];
        const u = decode(entry.u), v = decode(entry.v);
        if (JSON.stringify(u.GRID_DEFINITION_SECTION) !== JSON.stringify(v.GRID_DEFINITION_SECTION)) {
            throw new Error('GRIB U/V grid geometry differs');
        }
        return remember(this.#frames, key, {grid: u.GRID_DEFINITION_SECTION,
            u: u.DATA_SECTION.data, v: v.DATA_SECTION.data, cycle: u.timestamp()}, 8);
    }

    // Explicit cycles are retained for recorded wind. Current previews use the
    // latest fully downloaded cycle in the shared live server-settings metadata.
    async source(name, cycle) {
        name = String(name).replace(/^.*::/, '').toUpperCase();
        const key = cycle ? `${name}/${new Date(cycle).toISOString()}` : null;
        if (key && this.#cycles.has(key)) return remember(this.#cycles, key, this.#cycles.get(key), 8);
        const settings = await ensureServerSettings();
        const find = settings => settings?.datasources?.find(ds => String(ds.name).toUpperCase() === name);
        let ds = find(settings);
        if (cycle) {
            const date = new Date(cycle).toISOString().slice(0, 10).replaceAll('-', '');
            const run = new Date(cycle).getUTCHours();
            if (!ds?.gribpaths?.some(p => String(p.date) === date && Number(p.run) === run)) {
                ds = find(await loadForecastCycle(name, cycle));
            }
            if (!ds) throw new Error(`No metadata for ${name} cycle ${new Date(cycle).toISOString()}`);
            ds = {...ds, gribpaths: ds.gribpaths.filter(p => String(p.date) === date && Number(p.run) === run)};
        }
        if (!ds?.gribpaths?.length) throw new Error(`No GRIB steps for ${name}`);
        if (!['wind', 'current'].includes(ds.datakind)) throw new Error(`Unsupported GRIB datasource ${name}`);
        return key ? remember(this.#cycles, key, ds, 8) : ds;
    }

    async fields(source, time, {cycle, signal} = {}) {
        const requested = new Date(time).getTime();
        if (!Number.isFinite(requested)) throw new Error('Invalid forecast display time');
        signal?.throwIfAborted();
        const ds = await this.source(source, cycle);
        signal?.throwIfAborted();
        const paths = ds.gribpaths.map(entry => ({...entry,
            time: Date.parse(`${String(entry.date).slice(0,4)}-${String(entry.date).slice(4,6)}-${String(entry.date).slice(6,8)}T${String(entry.run).padStart(2, '0')}:00:00Z`) + Number(entry.step) * 3600000
        })).sort((a, b) => a.time - b.time);
        const selected = bracket(paths, requested);
        const downloads = await Promise.all([...new Set([selected.before.path, selected.after.path])].map(path => this.file(path)));
        signal?.throwIfAborted();
        const index = downloads.flatMap(file => file.index.filter(entry => entry.kind === ds.datakind)
            .map(entry => ({...entry, file}))).sort((a,b) => a.time - b.time);
        const {before, after} = bracket(index, requested);
        const first = await this.frame(before, before.file);
        signal?.throwIfAborted();
        const second = before.time === after.time ? first : await this.frame(after, after.file);
        const fraction = before.time === after.time ? 0 : (requested - before.time) / (after.time - before.time);
        return {source, cycle: first.cycle, time: new Date(time).toISOString(),
            sample(lat, lon) {
                const a = sampleField(first, lat, lon, ds.datakind);
                const b = first === second ? a : sampleField(second, lat, lon, ds.datakind);
                // AROME holds the valid frame when the other has no usable nodes.
                if (ds.datakind === 'wind') {
                    if (!a.valid) return b;
                    if (!b.valid) return a;
                }
                return {u: a.u + fraction * (b.u - a.u), v: a.v + fraction * (b.v - a.v),
                    magnitude: a.magnitude + fraction * (b.magnitude - a.magnitude), valid: a.valid || b.valid};
            }};
    }
}

// One instance across wind-model switches and both overlays. Four downloaded
// files and eight U/V pairs leave room for the two active time brackets.
export const sharedGribCache = new GribCache();
