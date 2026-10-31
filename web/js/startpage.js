////////////////////////////////////////////////////////////////////////////////
/// Sailsphere Router UI


( function () {

    function setUp () {
        document.getElementById('status-refresh').addEventListener('click', updateStatistics);
        updateStatistics();
        setInterval(updateStatistics, 60000);
    };
  
    ////////////////////////////////////////////////////////////////////////////////
    /// Event handlers
    
    function onWindowResize (event) {
    }

    //////////////////////////////////////////////////////////////////////
    /// XHR requests

    // http://aguas-13:8080/start?app=router&race=8SUUWHr4JD


    // "2026-10-24T12:00:00Z" -> "2026-10-24 12Z"
    function formatCycle (timestamp) {
        return timestamp ? `${timestamp.substring(0, 10)} ${timestamp.substring(11, 13)}Z` : null;
    }

    function renderDownloads (tbody, downloads) {
        if (!tbody) return;
        tbody.replaceChildren();
        const sources = Array.isArray(downloads) ? downloads : [];
        if (!sources.length) {
            const cell = tbody.insertRow().insertCell();
            cell.colSpan = 3;
            cell.textContent = 'Download status unavailable.';
        }
        for (const d of sources) {
            const row = tbody.insertRow();
            const cells = [row.insertCell(), row.insertCell(), row.insertCell()];
            cells.forEach(c => c.style.padding = '2px 8px');

            cells[0].textContent = d.name;

            const completed = formatCycle(d.completed);
            cells[1].textContent = completed ? `\u25CF ${completed}` : '\u25CF none';
            cells[1].style.color = completed ? 'darkgreen' : 'darkred';

            const progress = formatCycle(d.progress);
            if (progress) {
                const pct = d["progress-total"] > 0
                    ? Math.round(100 * d["progress-done"] / d["progress-total"]) : 0;
                cells[2].textContent = `\u25D0 ${progress} (${d["progress-done"]}/${d["progress-total"]}, ${pct}%)`;
                cells[2].style.color = 'darkorange';
            } else {
                cells[2].textContent = '-';
            }
        }
    }

    function text(id, value) {
        document.getElementById(id).textContent = value;
    }

    function formatBytes(value) {
        if (!Number.isFinite(value)) return 'Unavailable';
        const unit = value >= 1024 ** 3 ? 'GiB' : 'MiB';
        return `${(value / (unit === 'GiB' ? 1024 ** 3 : 1024 ** 2)).toFixed(1)} ${unit}`;
    }

    function number(value) {
        return Number.isFinite(value) ? value.toLocaleString() : '—';
    }

    function tableRows(id, rows, empty) {
        const body = document.querySelector(`#${id} tbody`);
        body.replaceChildren();
        if (!rows.length) {
            const cell = body.insertRow().insertCell();
            cell.textContent = empty;
            cell.colSpan = document.querySelector(`#${id} thead tr`).cells.length;
        }
        for (const values of rows) {
            const row = body.insertRow();
            for (const value of values) row.insertCell().textContent = value ?? '—';
        }
    }

    function displayStatistics(request) {
        const status = JSON.parse(request.responseText);
        text('tb_serverstart', status['server-start'] || 'Unavailable');
        text('tb_requests', number(status.requestcount));
        renderDownloads(document.querySelector('#tb_downloads tbody'), status.downloads);
        const memory = status.memory || {};
        const used = memory['heap-used'], limit = memory['heap-limit'];
        const percent = Number.isFinite(used) && limit > 0 ? 100 * used / limit : null;
        text('status-heap', percent === null ? 'Unavailable' : `${formatBytes(used)} / ${formatBytes(limit)} (${percent.toFixed(1)}%)`);
        const bar = document.getElementById('status-heap-bar');
        bar.hidden = percent === null;
        bar.value = Math.min(100, Math.max(0, percent ?? 0));
        text('status-forecast-bytes', formatBytes(memory['forecast-bytes']));
        text('tb_fc_cache', number(status['fc-cache-size']));
        text('status-polars-cache', `${number(status.caches?.polars)} / ${number(status.caches?.['combined-polars'])}`);
        text('status-races-cache', number(status.caches?.races));
        tableRows('status-cache-groups', (Array.isArray(memory.groups) ? memory.groups : []).map(g => [
            g.source, `${g.date} ${String(g.run).padStart(2, '0')}Z`, g.resolution, number(g.entries), formatBytes(g.bytes)
        ]), 'No decoded forecasts cached.');
        tableRows('status-parameters', (Array.isArray(status.parameters) ? status.parameters : []).map(p => [p.group, p.name, p.value]), 'Configuration unavailable. Reload the updated server.');
        const details = document.getElementById('status-last-route');
        details.replaceChildren();
        const rs = status.last_routestats;
        const fields = rs ? [
            ['Start', rs.start], ['Sailing duration', rs.duration],
            ['Calculation time', Number.isFinite(rs.calctime) ? `${rs.calctime.toFixed(2)} s` : '—'],
            ['Iterations', number(rs.steps)], ['Generated positions', number(rs.points)],
            ['Positions / second', rs.calctime > 0 ? number(Math.round(rs.points / rs.calctime)) : '—'],
            ['Seconds / iteration', rs.steps > 0 ? (rs.calctime / rs.steps).toFixed(3) : '—'],
            ['Positions / iteration', rs.steps > 0 ? number(Math.round(rs.points / rs.steps)) : '—']
        ] : [['No routing completed yet', '—']];
        for (const [label, value] of fields) {
            const dt = document.createElement('dt'), dd = document.createElement('dd');
            dt.textContent = label;
            dd.textContent = value ?? '—';
            details.append(dt, dd);
        }
        document.getElementById('status-error').hidden = true;
        text('status-updated', `Updated ${new Date().toLocaleTimeString()} · refreshes every minute`);
    }

    let pending = false;
    async function updateStatistics() {
        if (pending) return;
        pending = true;
        document.getElementById('status-refresh').disabled = true;
        try {
            const response = await fetch('/function/router.getStatistics', { signal: AbortSignal.timeout(15000), cache: 'no-store' });
            if (!response.ok) throw new Error(`HTTP ${response.status}`);
            displayStatistics({ responseText: await response.text() });
        } catch (error) {
            handleError(error);
        } finally {
            pending = false;
            document.getElementById('status-refresh').disabled = false;
        }
    }

    function handleError(error) {
        const element = document.getElementById('status-error');
        element.hidden = false;
        element.textContent = 'Status could not be refreshed. Previously displayed values may be stale. Try Refresh.';
        console.error('Status refresh failed', error);
    }

    document.addEventListener("DOMContentLoaded", function(event) {
        setUp()
    });

}) ()

/// EOF
////////////////////////////////////////////////////////////////////////////////
