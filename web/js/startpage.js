////////////////////////////////////////////////////////////////////////////////
/// Sailsphere Router UI

import * as Util from './Util.js';

( function () {

    function setUp () {
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


    function displayStatistics (request) {
        let tbRequests = document.getElementById('tb_requests');
        let tbDatasource = document.getElementById('tb_datasource');
        let routerstatus = JSON.parse(request.responseText);
        if (tbRequests) tbRequests.value = routerstatus.requestcount;
        if (tbDatasource) tbDatasource.value = routerstatus.datasource;
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
