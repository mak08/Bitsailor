////////////////////////////////////////////////////////////////////////////////
/// Sailsphere Router UI

import * as Util from './Util.js';

( function () {

    function setUp () {
        
        setupColors();
        getRaceList();

    };
  
    ////////////////////////////////////////////////////////////////////////////////
    /// Event handlers
    
    function onWindowResize (event) {
    }

    
    //////////////////////////////////////////////////////////////////////
    /// XHR requests

    // http://aguas-13:8080/start?app=router&race=8SUUWHr4JD
    
    function getRaceList (event) {
        Util.doGET("/public/vh:getRaceList",
                   function (request) {
                       console.log(request);
                       var races = JSON.parse(request.responseText);
                       var table = document.getElementById("race_list");
                       for (const race of races) {
                           var row = table.insertRow(-1);
                           appendTextCell(row, race.name);
                           appendTextCell(row, race.class);
                           appendTextCell(row, race["start-time"]);
                           appendLinkCell(row, race.id, "/start?app=router&race=" + race.id);
                       }
                   },
                   function (jqXHR, textStatus, errorThrown) {
                       console.log(errorThrown);
                       alert(errorThrown);
                   });
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
