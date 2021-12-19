////////////////////////////////////////////////////////////////////////////////
/// Sailsphere Router UI

import * as Util from './Util.js';

( function () {

    function setUp () {
        document.getElementById("bt_submit").addEventListener("click", onSubmit);
    };

    ////////////////////////////////////////////////////////////////////////////////
    /// Event handlers
    
    function onWindowResize (event) {
    }

    function displayDialog (text, callback) {
        var lbDialog = document.getElementById("lb_dialog");
        lbDialog.innerHTML = text;

        var btDialogConfirm = document.getElementById("bt_dialog_confirm");
        btDialogConfirm.addEventListener("click", callback);

        var mdDialog = document.getElementById("md_dialog");
        mdDialog.style.display = "block";
    }

    function cbErrorConfirm (event) {
        var mdDialog = document.getElementById("md_dialog");
        mdDialog.style.display = "none";
    }
    
    function cbSuccessConfirm (event) {
        var mdSuccess = document.getElementById("md_dialog");
        mdSuccess.style.display = "none";
    }
    
    async function onSubmit (event) {
        var startlat = document.getElementById("startlat").value;
        var startlon = document.getElementById("startlon").value;
        var destlat = document.getElementById("destlat").value;
        var destlon = document.getElementById("destlon").value;
        var threads = document.getElementById("threads").value;
        var duration = document.getElementById("duration").value;
        var resolution = document.getElementById("resolution").value;
        var delay = document.getElementById("delay").value;
        var polars = document.getElementById("polars").value

        document.getElementById("div_routing").style.cursor = "progress";
        document.getElementById("bt_submit").style.cursor = "progress";

        for (var i = 0; i<threads; i++ ) {
            console.log('Started #' + i); 
            Util.doGET("/function/vh.getRouteRS",
                       function (request) {
                           document.getElementById("div_routing").style.cursor = "default";
                           document.getElementById("bt_submit").style.cursor = "default";
                           
                           console.log('Finished #' + i); 
                           
                       },
                       function (request) {
                           document.getElementById("div_routing").style.cursor = "default";
                           document.getElementById("bt_submit").style.cursor = "default";
                           alert(request.responseText);
                           console.log('Error: #' + i); 

                       },
                       {
                           "polarsID": polars,
                           "duration": duration * 3600,
                           "resolution": resolution,
                           "latStart": startlat,
                           "lonStart": startlon,
                           "latDest": destlat,
                           "lonDest": destlon
                       });
            await sleep(delay);
        }
    }


    function sleep (ms) {
        return new Promise(resolve => setTimeout(resolve, ms));
    }

    
    document.addEventListener("DOMContentLoaded", function(event) {
        setUp()
    });

}) ()

/// EOF
////////////////////////////////////////////////////////////////////////////////
