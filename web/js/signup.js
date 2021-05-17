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
        } else if ( boat.length < 3 ) {
            alert("Please make the boat name at least 3 characters long.");
        } else {
            document.getElementById("div_signup").style.cursor = "progress";
            document.getElementById("bt_submit").style.cursor = "progress";
            Util.doGET("/public/vh.signUp",
                       
                       function (request) {
                           document.getElementById("div_signup").style.cursor = "default";
                           document.getElementById("bt_submit").style.cursor = "default";
                           alert(SIGNUPTEXT);
                       },
                       function (request) {
                           document.getElementById("div_signup").style.cursor = "default";
                           document.getElementById("bt_submit").style.cursor = "default";
                           alert(request.responseText);
                       },
                       {
                           "emailAddress": email,
                           "boatName": boat,
                           "password": Util.MD5(pw1)
                       });
        }
    }
    
    document.addEventListener("DOMContentLoaded", function(event) {
        setUp()
    });

}) ()

/// EOF
////////////////////////////////////////////////////////////////////////////////
