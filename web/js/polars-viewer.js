////////////////////////////////////////////////////////////////////////////////
/// Polar diagram viewer (Polars tab of the start page)

import * as Util from './Util.js';

(function () {
    const canvas = document.getElementById('polar');
    const select = document.getElementById('polarSelect');
    const slider = document.getElementById('twsSlider');
    const twsValue = document.getElementById('twsValue');
    const vmgBox = document.getElementById('vmgBox');
    const tableBox = document.getElementById('polarTable');
    if (!canvas || !select || !slider) return;

    // polar: {twa: [..], tws: [..], table: [twaIndex][twsIndex]}
    let polar = null;
    let selectedTwa = 45;
    let draggingHandle = false;
    let cellRefs = [];       // cellRefs[twaIndex][twsIndex] -> <td>
    let highlighted = [];

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
                buildTable();
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

    // Table cells that contribute to the interpolated value (non-zero weight)
    function cellsUsed(tws, twa) {
        const [ti, tf] = locate(polar.tws, tws);
        const [ai, af] = locate(polar.twa, twa);
        const ti2 = Math.min(ti + 1, polar.tws.length - 1);
        const ai2 = Math.min(ai + 1, polar.twa.length - 1);
        const cells = [];
        for (const [a, wa] of [[ai, 1 - af], [ai2, af]]) {
            for (const [t, wt] of [[ti, 1 - tf], [ti2, tf]]) {
                if (wa > 0 && wt > 0) cells.push([a, t]);
            }
        }
        return cells;
    }

    ////////////////////////////////////////////////////////////////////////////
    /// Table

    function buildTable() {
        const table = document.createElement('table');
        const head = table.createTHead().insertRow();
        head.appendChild(document.createElement('th')).textContent = 'TWA \\ TWS';
        polar.tws.forEach(tws => {
            head.appendChild(document.createElement('th')).textContent = tws;
        });
        const body = table.createTBody();
        cellRefs = polar.twa.map((twa, a) => {
            const row = body.insertRow();
            row.appendChild(document.createElement('th')).textContent = twa;
            return polar.table[a].map(speed => {
                const td = row.insertCell();
                td.textContent = speed;
                return td;
            });
        });
        highlighted = [];
        tableBox.replaceChildren(table);
    }

    function updateTableHighlight(tws, twa) {
        highlighted.forEach(td => td.classList.remove('used'));
        const used = cellsUsed(tws, twa);
        highlighted = used.map(([a, t]) => cellRefs[a][t]);
        highlighted.forEach(td => td.classList.add('used'));
        if (used.length === 0) return;
        // Scroll within the table box only (not the page) to keep the cells visible
        const first = highlighted[0];
        const headHeight = tableBox.querySelector('thead').offsetHeight;
        const rowTop = first.offsetTop;
        const rowHeight = first.offsetHeight;
        const viewTop = tableBox.scrollTop + headHeight;
        const viewBottom = tableBox.scrollTop + tableBox.clientHeight;
        if (rowTop < viewTop || rowTop + rowHeight > viewBottom) {
            tableBox.scrollTop = rowTop - tableBox.clientHeight / 2;
        }
        const colLeft = first.offsetLeft;
        if (colLeft < tableBox.scrollLeft + 60 || colLeft + first.offsetWidth > tableBox.scrollLeft + tableBox.clientWidth) {
            tableBox.scrollLeft = colLeft - tableBox.clientWidth / 2;
        }
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
        // Optimal VMG angles: scan in 0.1 deg steps (integer loop avoids float drift)
        let bestUp = { vmg: -Infinity, twa: null, speed: 0 };
        let bestDown = { vmg: -Infinity, twa: null, speed: 0 };
        for (let i = 0; i <= 1800; i++) {
            const twa = i / 10;
            const speed = interpolateSpeed(selectedTws, twa);
            const vmg = speed * Math.cos(twa * Math.PI / 180);
            if (twa <= 90 && vmg > bestUp.vmg) bestUp = { vmg, twa, speed };
            if (twa >= 90 && -vmg > bestDown.vmg) bestDown = { vmg: -vmg, twa, speed };
        }

        ctx.beginPath();
        for (let twa = 0; twa <= 180; twa += 2) {
            const speed = interpolateSpeed(selectedTws, twa);
            const rad = (90 - twa) * Math.PI / 180;
            const radius = (speed / maxSpeed) * maxRadius;
            const x = cx + radius * Math.cos(rad);
            const y = cy - radius * Math.sin(rad);
            if (twa === 0) ctx.moveTo(x, y); else ctx.lineTo(x, y);
        }
        ctx.strokeStyle = color;
        ctx.lineWidth = 3;
        ctx.stroke();

        // Yellow marks at the optimal upwind and downwind TWA
        for (const best of [bestUp, bestDown]) {
            const brad = (90 - best.twa) * Math.PI / 180;
            const r = (best.speed / maxSpeed) * maxRadius;
            ctx.beginPath();
            ctx.arc(cx + r * Math.cos(brad), cy - r * Math.sin(brad), 7, 0, 2 * Math.PI);
            ctx.fillStyle = '#ffd400';
            ctx.strokeStyle = '#333';
            ctx.lineWidth = 2;
            ctx.fill();
            ctx.stroke();
        }

        ctx.fillStyle = color;
        ctx.font = '16px Arial';
        ctx.textAlign = 'left';
        ctx.textBaseline = 'alphabetic';
        ctx.fillText(`TWS ${selectedTws.toFixed(2)} kt`, 30, 50);

        vmgBox.innerHTML =
            `<b>VMG up:</b> ${bestUp.vmg.toFixed(2)} @ TWA ${bestUp.twa.toFixed(1)}°<br>` +
            `<b>VMG down:</b> ${bestDown.vmg.toFixed(2)} @ TWA ${bestDown.twa.toFixed(1)}°`;

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
        if (polar) updateTableHighlight(tws, selectedTwa);
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
