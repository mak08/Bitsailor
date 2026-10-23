////////////////////////////////////////////////////////////////////////////////
/// Polar diagram viewer (Polars tab of the start page)

import * as Util from './Util.js';

(function () {
    const canvas = document.getElementById('polar');
    const select = document.getElementById('polarSelect');
    const slider = document.getElementById('twsSlider');
    const twsValue = document.getElementById('twsValue');
    const vmgBox = document.getElementById('vmgBox');
    if (!canvas || !select || !slider) return;

    // polar: {twa: [..], tws: [..], table: [twaIndex][twsIndex]}
    let polar = null;
    let selectedTwa = 45;
    let draggingHandle = false;

    ////////////////////////////////////////////////////////////////////////////
    /// Data

    function loadPolarsList() {
        Util.doGET('/function/router.getPolarsList',
            (xhr) => {
                const list = JSON.parse(xhr.responseText);
                list.forEach(entry => {
                    const option = document.createElement('option');
                    option.value = entry.id;
                    option.textContent = entry.label || entry.id;
                    select.appendChild(option);
                });
                if (list.length > 0) loadPolar(select.value);
            },
            () => console.error('Could not load polars list'));
    }

    function loadPolar(id) {
        Util.doGET(`/polars/${encodeURIComponent(id)}.json`,
            (xhr) => {
                const data = JSON.parse(xhr.responseText).data_json;
                polar = { twa: data.twa, tws: data.tws, table: data.table };
                slider.min = polar.tws[0];
                slider.max = polar.tws[polar.tws.length - 1];
                slider.value = Math.min(Math.max(15, slider.min), slider.max);
                draw();
            },
            () => console.error('Could not load polars ' + id));
    }

    // Index i and fraction f such that value lies between axis[i] and axis[i+1] (clamped)
    function locate(axis, value) {
        const last = axis.length - 1;
        if (value <= axis[0]) return [0, 0];
        if (value >= axis[last]) return [Math.max(last - 1, 0), 1];
        let i = 0;
        while (value > axis[i + 1]) i++;
        return [i, (value - axis[i]) / (axis[i + 1] - axis[i])];
    }

    function interpolateSpeed(tws, twa) {
        const [ti, tf] = locate(polar.tws, tws);
        const [ai, af] = locate(polar.twa, twa);
        const t = polar.table;
        const ti2 = Math.min(ti + 1, polar.tws.length - 1);
        const ai2 = Math.min(ai + 1, polar.twa.length - 1);
        const r1 = t[ai][ti] + (t[ai][ti2] - t[ai][ti]) * tf;
        const r2 = t[ai2][ti] + (t[ai2][ti2] - t[ai2][ti]) * tf;
        return r1 + (r2 - r1) * af;
    }

    ////////////////////////////////////////////////////////////////////////////
    /// Drawing

    function drawPolar(selectedTws) {
        const ctx = canvas.getContext('2d');
        const cx = canvas.width / 2;
        const cy = canvas.height / 2;
        const maxRadius = canvas.width * 0.45;

        ctx.clearRect(0, 0, canvas.width, canvas.height);
        ctx.fillStyle = '#f7f7f7';
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        if (!polar) {
            vmgBox.innerHTML = '';
            return;
        }

        const maxSpeed = Math.max(...polar.table.flat()) || 1;

        ctx.strokeStyle = '#bbb';
        ctx.lineWidth = 1;
        ctx.textAlign = 'left';
        ctx.textBaseline = 'alphabetic';
        for (let r = 0.2; r <= 1.001; r += 0.2) {
            ctx.beginPath();
            ctx.arc(cx, cy, r * maxRadius, 0, 2 * Math.PI);
            ctx.stroke();
            ctx.font = '12px Arial';
            ctx.fillStyle = '#888';
            ctx.fillText((r * maxSpeed).toFixed(1), cx + 5, cy - r * maxRadius + 15);
        }

        for (let angle = 0; angle < 360; angle += 30) {
            const rad = (90 - angle) * Math.PI / 180;
            ctx.beginPath();
            ctx.moveTo(cx, cy);
            ctx.lineTo(cx + maxRadius * Math.cos(rad), cy - maxRadius * Math.sin(rad));
            ctx.strokeStyle = '#ccc';
            ctx.stroke();
            ctx.fillStyle = '#444';
            ctx.font = '13px Arial';
            ctx.textAlign = 'center';
            ctx.textBaseline = 'middle';
            ctx.fillText(angle + '°', cx + (maxRadius + 20) * Math.cos(rad), cy - (maxRadius + 20) * Math.sin(rad));
        }

        const color = '#e41a1c';
        let bestUp = { vmg: -Infinity, twa: null };
        let bestDown = { vmg: -Infinity, twa: null };
        ctx.beginPath();
        for (let twa = 0; twa <= 180; twa += 2) {
            const speed = interpolateSpeed(selectedTws, twa);
            const rad = (90 - twa) * Math.PI / 180;
            const radius = (speed / maxSpeed) * maxRadius;
            const x = cx + radius * Math.cos(rad);
            const y = cy - radius * Math.sin(rad);
            const vmg = speed * Math.cos(twa * Math.PI / 180);
            if (twa <= 90 && vmg > bestUp.vmg) bestUp = { vmg, twa };
            if (twa >= 90 && Math.abs(vmg) > bestDown.vmg) bestDown = { vmg: Math.abs(vmg), twa };
            if (twa === 0) ctx.moveTo(x, y); else ctx.lineTo(x, y);
        }
        ctx.strokeStyle = color;
        ctx.lineWidth = 3;
        ctx.stroke();

        ctx.fillStyle = color;
        ctx.font = '16px Arial';
        ctx.textAlign = 'left';
        ctx.textBaseline = 'alphabetic';
        ctx.fillText(`TWS ${selectedTws.toFixed(2)} kt`, 30, 50);

        vmgBox.innerHTML =
            `<b>VMG up:</b> ${bestUp.vmg.toFixed(2)} @ TWA ${bestUp.twa}°<br>` +
            `<b>VMG down:</b> ${bestDown.vmg.toFixed(2)} @ TWA ${bestDown.twa}°`;

        // Interactive TWA line with draggable handle
        const rad = (90 - selectedTwa) * Math.PI / 180;
        const hx = cx + maxRadius * Math.cos(rad);
        const hy = cy - maxRadius * Math.sin(rad);
        ctx.beginPath();
        ctx.moveTo(cx, cy);
        ctx.lineTo(hx, hy);
        ctx.strokeStyle = '#377eb8';
        ctx.lineWidth = 2;
        ctx.setLineDash([8, 6]);
        ctx.stroke();
        ctx.setLineDash([]);

        const speed = interpolateSpeed(selectedTws, selectedTwa);
        const vmg = speed * Math.cos(selectedTwa * Math.PI / 180);
        ctx.fillStyle = '#377eb8';
        ctx.font = '15px Arial';
        ctx.textAlign = 'left';
        ctx.textBaseline = 'top';
        ctx.fillText(`TWA: ${selectedTwa.toFixed(1)}°  Speed: ${speed.toFixed(2)}  VMG: ${vmg.toFixed(2)}`, cx + 20, cy + 20);

        ctx.beginPath();
        ctx.arc(hx, hy, 10, 0, 2 * Math.PI);
        ctx.fillStyle = '#377eb8';
        ctx.strokeStyle = '#fff';
        ctx.lineWidth = 3;
        ctx.fill();
        ctx.stroke();
    }

    function draw() {
        const tws = Number(slider.value);
        twsValue.textContent = tws.toFixed(1);
        drawPolar(tws);
    }

    ////////////////////////////////////////////////////////////////////////////
    /// Events

    // Mouse position in canvas coordinates (the canvas may be scaled by CSS)
    function canvasPoint(e) {
        const rect = canvas.getBoundingClientRect();
        return {
            x: (e.clientX - rect.left) * canvas.width / rect.width,
            y: (e.clientY - rect.top) * canvas.height / rect.height
        };
    }

    select.addEventListener('change', () => loadPolar(select.value));
    slider.addEventListener('input', draw);

    canvas.addEventListener('mousedown', (e) => {
        const p = canvasPoint(e);
        const maxRadius = canvas.width * 0.45;
        const rad = (90 - selectedTwa) * Math.PI / 180;
        const hx = canvas.width / 2 + maxRadius * Math.cos(rad);
        const hy = canvas.height / 2 - maxRadius * Math.sin(rad);
        if (Math.hypot(p.x - hx, p.y - hy) < 15) draggingHandle = true;
    });

    document.addEventListener('mousemove', (e) => {
        if (!draggingHandle || !polar) return;
        const p = canvasPoint(e);
        const angle = Math.atan2(canvas.height / 2 - p.y, p.x - canvas.width / 2) * 180 / Math.PI;
        selectedTwa = Math.min(Math.max(90 - angle, 0), 180);
        draw();
    });

    document.addEventListener('mouseup', () => { draggingHandle = false; });

    drawPolar(0);
    loadPolarsList();
})();
