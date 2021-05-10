////////////////////////////////////////////////////////////////////////////////
/// Sailsphere Router UI

import * as Util from './Util.js';

( function () {

    function setUp () {
        
        setupColors();
        getRaceList();

        document.getElementById("bt_submit").addEventListener("click", onSubmit);

    };
  
    ////////////////////////////////////////////////////////////////////////////////
    /// Event handlers
    
    function onWindowResize (event) {
    }

    const SIGNUPTEXT = `Thank you for requesting a user! 
You should immediately receive an e-mail containing a link to complete your registration.
If you do not receive an e-mail within a few minutes, please verify your e-mail address and check the spam folder.`

    function validateEmail(email) {
        const re = /^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
        return re.test(String(email).toLowerCase());
    }
    
    function onSubmit (event) {
        var email = document.getElementById("in_email").value;
        var boat = document.getElementById("in_boatname").value;
        var pw1 = document.getElementById("in_pw1").value;
        var pw2 = document.getElementById("in_pw2").value;

        if ( !validateEmail(email) ) {
            alert("Invalid email address. Not submitted.");
        } else if ( pw1 != pw2 ) {
            alert("Passwords don't match. Not submitted.");
        } else if ( !pw1 || pw1.length < 6 ) {
            alert("Short password. Please use at least 6 characters.");
        } else {
            Util.doGET("/public/vh.signUp",
                       function (request) {
                           alert(SIGNUPTEXT);
                       },
                       function (request) {
                           alert(request.responseText);
                       },
                       {
                           "emailAddress": email,
                           "boatName": boat,
                           "password": Util.MD5(pw1)
                       });
        }
    }
    
    //////////////////////////////////////////////////////////////////////
    /// XHR requests

    // http://aguas-13:8080/start?app=router&race=8SUUWHr4JD
    
    function getRaceList (event) {
        Util.doGET("/public/vh.getRaceList",
                   function (request) {
                       console.log(request);
                       var races = JSON.parse(request.responseText);
                       var table = document.getElementById("race_list");
                       for (const race of races) {
                           var row = table.insertRow(-1);
                           var date = race["start-time"];
                           appendTextCell(row, race.name);
                           appendTextCell(row, race.class);
                           appendTextCell(row, date.substring(0, 10) + ' ' + date.substring(11, 16));
                           appendLinkCell(row, race.id, "/start?app=router&race=" + race.id);
                       }
                   },
                   function (xhr) {
                       alert(xhr.responseText);
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
