////////////////////////////////////////////////////////////////////////////////
/// Sailsphere Router UI

import * as Util from './Util.js';

( function () {

    function setUp () {
        document.getElementById("bt_submit").addEventListener("click", onSubmit);
    };


    const SIGNUP_TEXT = `Thank you for requesting a user! 
            You should immediately receive an e-mail containing a link to complete your registration.
            If you do not receive an e-mail within a few minutes, please verify your e-mail address and check the spam folder.`;

    ////////////////////////////////////////////////////////////////////////////////
    /// Event handlers
    
    function onWindowResize (event) {
    }

    function validateEmail (email) {
        const re = /^(([^<>()[\]\\.,;:\s@"]+(\.[^<>()[\]\\.,;:\s@"]+)*)|(".+"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;
        return re.test(String(email).toLowerCase());
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
        history.back();
    }
    
    function onSubmit (event) {
        var email = document.getElementById("in_email").value;
        var boat = document.getElementById("in_boatname").value;
        var pw1 = document.getElementById("in_pw1").value;
        var pw2 = document.getElementById("in_pw2").value;
        if ( !validateEmail(email) ) {
            displayDialog("Invalid email address. Not submitted.", cbErrorConfirm);
        } else if ( pw1 != pw2 ) {
            displayDialog("Passwords don't match. Not submitted.", cbErrorConfirm);
        } else if ( !pw1 || pw1.length < 6 ) {
            displayDialog("Short password. Please use at least 6 characters.", cbErrorConfirm);
        } else if ( boat.length < 3 ) {
            displayDialog("Please make the boat name at least 3 characters long.", cbErrorConfirm);
        } else {
            document.getElementById("div_signup").style.cursor = "progress";
            document.getElementById("bt_submit").style.cursor = "progress";

            Util.doGET("/public/vh.signUp",
                       
                       function (request) {
                           document.getElementById("div_signup").style.cursor = "default";
                           document.getElementById("bt_submit").style.cursor = "default";

                           displayDialog(SIGNUP_TEXT, cbSuccessConfirm);
                           
                       },
                       function (request) {
                           document.getElementById("div_signup").style.cursor = "default";
                           document.getElementById("bt_submit").style.cursor = "default";

                           displayDialog(request.responseText, cbErrorConfirm);
                       },
                       {
                           "emailAddress": encodeURIComponent(email),
                           "boatName": encodeURIComponent(boat),
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
