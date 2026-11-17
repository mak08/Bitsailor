// Current arrows use the router's regional GRIB interpolation, sampled at screen-cell centres.
export default class CurrentLayer {
    #canvas;
    #legend;
    #version = 0;
    #request;

    constructor(canvas, legend) {
        this.#canvas = canvas;
        this.#legend = legend;
    }

    clear() {
        ++this.#version;
        this.#request?.abort();
        this.#canvas.getContext('2d').clearRect(0, 0, this.#canvas.width, this.#canvas.height);
        this.#canvas.style.display = 'none';
        if (this.#legend) this.#legend.hidden = true;
    }

    async update(bounds, time, source, enabled, forecastModel) {
        this.clear();
        if (!enabled || !source || source === 'NONE') return;
        const version = this.#version;
        const request = new AbortController();
        this.#request = request;
        this.#canvas.style.display = 'block';
        const rect = this.#canvas.getBoundingClientRect();
        if (!rect.width || !rect.height) return;
        const spacing = 15 * Math.SQRT2; // Same density as wind, half its former density.
        const columns = Math.max(1, Math.min(256, Math.floor(rect.width / spacing)));
        const rows = Math.max(1, Math.min(128, Math.floor(rect.height / spacing)));
        const query = new URLSearchParams({ source, time: new Date(time).toISOString(),
            north: Math.min(85.05112878, bounds.north), south: Math.max(-85.05112878, bounds.south),
            west: bounds.west, east: bounds.east, columns, rows });
        if (forecastModel) query.set('forecast_model', forecastModel);
        try {
            const response = await fetch(`/function/router.getCurrentField?${query}`, { signal: request.signal });
            if (!response.ok) throw new Error(`Current forecast: HTTP ${response.status}`);
            const field = await response.json();
            if (version !== this.#version) return;
            if (field.columns !== columns || field.rows !== rows || !Array.isArray(field.vectors)
                || field.vectors.length !== columns * rows) throw new Error('Invalid current field');
            const frame = this.#canvas.cloneNode(false);
            const ctx = frame.getContext('2d');
            const scaleX = frame.width / rect.width;
            const scaleY = frame.height / rect.height;
            ctx.scale(scaleX, scaleY);
            field.vectors.forEach(([u, v], index) => {
                const column = index % columns;
                const row = Math.floor(index / columns);
                this.drawArrow(ctx, (column + 0.5) * rect.width / columns,
                               (row + 0.5) * rect.height / rows, u, v);
            });
            if (version !== this.#version) return;
            this.#canvas.style.display = 'block';
            this.#canvas.getContext('2d').drawImage(frame, 0, 0);
            if (this.#legend) {
                this.#legend.hidden = false;
                const usedSource = field.source || source;
                this.#legend.textContent = `${String(usedSource).toUpperCase().includes('IFS-CURRENT') ? 'IFS currents' : 'Currents'}: → weak  ⟶ strong`;
                this.#legend.title = `${usedSource} · ${field.time} · cycle ${field.cycle}. Blue/small: weak; dark blue/large: strong (3 kn and above). White halos improve contrast.`;
            }
        } catch (error) {
            if (version !== this.#version || error.name === 'AbortError') return;
            if (this.#legend) {
                this.#legend.hidden = false;
                this.#legend.textContent = 'Currents unavailable';
                this.#legend.title = error.message;
            }
            console.error('Current display failed', error);
        }
    }

    drawArrow(ctx, x, y, u, v) {
        if (!Number.isFinite(u) || !Number.isFinite(v)) return;
        const speed = Math.hypot(u, v);
        if (speed < 0.01) return; // Calm and cells outside regional forecast coverage.
        const strength = Math.min(1, speed * 1.943844 / 3);
        const length = 10 + 8 * strength;
        const head = 3.5 + strength;
        const weak = [40, 139, 210], strong = [0, 48, 145];
        const color = weak.map((component, i) => Math.round(component + strength * (strong[i] - component)));
        ctx.save();
        ctx.translate(x, y);
        // Canvas Y points south; GRIB U/V point east/north. This is flow direction.
        ctx.rotate(Math.atan2(-v, u));
        const width = 1.6 + 0.5 * strength;
        ctx.lineCap = 'round';
        ctx.lineJoin = 'round';
        ctx.beginPath();
        ctx.moveTo(-length / 2, 0);
        ctx.lineTo(length / 2, 0);
        ctx.moveTo(length / 2 - head, -head * 0.65);
        ctx.lineTo(length / 2, 0);
        ctx.lineTo(length / 2 - head, head * 0.65);
        ctx.globalAlpha = 0.95;
        ctx.strokeStyle = '#ffffff';
        ctx.lineWidth = width + 2;
        ctx.stroke();
        ctx.globalAlpha = 1;
        ctx.strokeStyle = `rgb(${color.join(',')})`;
        ctx.lineWidth = width;
        ctx.stroke();
        ctx.restore();
    }
}
