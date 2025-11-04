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


    function displayStatistics (request) {
        let routerstatus = JSON.parse(request.responseText);

        let tbServerstart = document.getElementById('tb_serverstart');
        let tbForecasts = document.getElementById('tb_forecasts');
        let tbFCCacheSize = document.getElementById('tb_fc_cache');
        let tbRequests = document.getElementById('tb_requests');
        let tbDatasource = document.getElementById('tb_datasource');
        let tbMaxIso = document.getElementById('tb_max_iso_points');
        let tbTwaSteps = document.getElementById('tb_twa_steps');
        let tbLastStats = document.getElementById('tb_last_routestats');

        tbServerstart.textContent = routerstatus["server-start"]; 
        tbForecasts.textContent = routerstatus["latest-forecast"];
        tbFCCacheSize.textContent = routerstatus["fc-cache-size"];
        tbRequests.textContent   = routerstatus.requestcount;
        tbDatasource.textContent = routerstatus.datasource ?? '';

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
                const elapsed = `${rs.calctime.toFixed(1)}s`;
                const points  = Number(rs.points).toFixed(0);
                const steps   = Number(rs.steps);
                const pps     = rs.calctime > 0 ? (points / rs.calctime).toFixed(1) : '∞';
                const sPerI   = steps > 0 ? (rs.calctime / steps).toFixed(2) : '—';
                const pPerI   = steps > 0 ? Math.round(points / steps).toLocaleString() : '—';

                tbLastStats.textContent =
                    `Elapsed ${elapsed} | Positions ${points.toLocaleString()} | ` +
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
