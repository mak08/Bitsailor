////////////////////////////////////////////////////////////////////////////////
/// Sailsphere Router UI

import * as Util from './Util.js';

( function () {

    function setUp () {
        
        setupColors();
        getRaceList();
        document.getElementById('bt_racelist_reload').addEventListener(
            'click',
            function (event) {
                getRaceListAdmin(event);
            });
        updateStatistics();
        setInterval(updateStatistics, 600000);

    };
  
    ////////////////////////////////////////////////////////////////////////////////
    /// Event handlers
    
    function onWindowResize (event) {
    }

    //////////////////////////////////////////////////////////////////////
    /// XHR requests

    // http://aguas-13:8080/start?app=router&race=8SUUWHr4JD


    function buildRaceList (request) {
        console.log(request);
        var tbodyVR = document.getElementById('tbody_VR');
        tbodyVR.innerHTML = '';
        var races = JSON.parse(request.responseText);
        // var tableRS = document.getElementById("race_list_rs");
        // var tableVR = document.getElementById("race_list_vr");
        for (const race of races) {
            var row = tbodyVR.insertRow(-1);
            var date = new Date(race["start-time"]).toISOString();
            appendTextCell(row, race.name);
            appendTextCell(row, race.category);
            appendTextCell(row, race.level);
            appendTextCell(row, race.class);
            appendTextCell(row, race.record || 'no');
            appendTextCell(row, race.gfs025);
            appendTextCell(row, date.substring(0, 10) + ' ' + date.substring(11, 16));
            appendLinkCell(row, race.id, "/router?race=" + race.id);
        }
    }


    function displayStatistics (request) {
        let tbRequests = document.getElementById('tb_requests');
        let tbDatasource = document.getElementById('tb_datasource');
        let routerstatus = JSON.parse(request.responseText);
        tbRequests.value = routerstatus.requestcount;
        tbDatasource.value = routerstatus.datasource;
    }
    
    function handleError (xhr) {
        alert(xhr.responseText);
    }
    
    function getRaceList (event) {
        Util.doGET("/function/router.getRaceList", buildRaceList, handleError);
    }
    
    function getRaceListAdmin (event) {
        Util.doGET("/admin/router.getRaceListAdmin", buildRaceList, handleError, {"forceReload": "true"});
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
