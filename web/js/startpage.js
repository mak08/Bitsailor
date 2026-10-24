////////////////////////////////////////////////////////////////////////////////
/// Sailsphere Router UI

import * as Util from './Util.js';

( function () {

    function setUp () {
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
        for (const d of (Array.isArray(downloads) ? downloads : [])) {
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

    function displayStatistics (request) {
        let routerstatus = JSON.parse(request.responseText);

        let tbServerstart = document.getElementById('tb_serverstart');
        let tbDownloads = document.querySelector('#tb_downloads tbody');
        let tbFCCacheSize = document.getElementById('tb_fc_cache');
        let tbRequests = document.getElementById('tb_requests');
        let tbMaxIso = document.getElementById('tb_max_iso_points');
        let tbTwaSteps = document.getElementById('tb_twa_steps');
        let tbLastStats = document.getElementById('tb_last_routestats');

        tbServerstart.textContent = routerstatus["server-start"]; 
        renderDownloads(tbDownloads, routerstatus.downloads);
        tbFCCacheSize.textContent = routerstatus["fc-cache-size"];
        tbRequests.textContent   = routerstatus.requestcount;

        if (tbMaxIso && routerstatus.max_iso_points != null) {
            tbMaxIso.textContent = routerstatus.max_iso_points.toLocaleString();
        }
        if (tbTwaSteps && routerstatus.twa_steps != null) {
            // Show degrees with 1 decimal if needed
            const val = Number(routerstatus.twa_steps);
            tbTwaSteps.textContent = Number.isFinite(val) ? `${val % 1 ? val.toFixed(1) : val}°` : '-';
        }

        if (tbLastStats) {
            const rs = routerstatus.last_routestats;
            if (rs && Number.isFinite(rs.calctime) && Number.isFinite(rs.steps) && Number.isFinite(rs.points)) {
                const duration = rs.duration;
                const elapsed = `${rs.calctime.toFixed(2)}s`;
                const points  = Number(rs.points).toFixed(0);
                const steps   = Number(rs.steps);
                const pps     = rs.calctime > 0 ? (points / rs.calctime).toFixed() : '∞';
                const sPerI   = steps > 0 ? (rs.calctime / steps).toFixed(2) : '—';
                const pPerI   = steps > 0 ? Math.round(points / steps).toFixed() : '—';

                tbLastStats.textContent =
                    `Duration ${duration} | Elapsed ${elapsed} | Positions ${points.toLocaleString()} | ` +
                    `Isochrones ${steps} | p/s=${pps} | s/i=${sPerI} | p/i=${pPerI}`;
            } else {
                tbLastStats.textContent = '-';
            }
        }
    }
    
    function handleError (xhr) {
        alert(xhr.responseText);
    }
    
    function updateStatistics () {
        Util.doGET("/function/router.getStatistics", displayStatistics, handleError);
    }
    
    
    function appendTextCell (row, text) {
        var cell = row.insertCell(-1);
        var textNode = document.createTextNode(text);
        cell.appendChild(textNode);
    }
  
    function appendLinkCell (row, text, url) {
        var cell = row.insertCell(-1);
        var link = document.createElement('a');
        link.setAttribute('href', url);
        link.innerHTML = text;
        cell.appendChild(link);
    }
  
    document.addEventListener("DOMContentLoaded", function(event) {
        setUp()
    });

}) ()

/// EOF
////////////////////////////////////////////////////////////////////////////////
