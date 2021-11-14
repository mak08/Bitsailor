////////////////////////////////////////////////////////////////////////////////
/// BitSailor Router UI

import * as Util from './Util.js';
import * as Router from './router.js';

( function () {

    function getSession () {
        Util.doGET("/function/vh.getSession",
                   function(request) {
                       var routing = JSON.parse(request.responseText);
                       if (routing.start) {
                           Router.updateStartPosition(routing.start.lat, routing.start.lng);
                           var start  = new google.maps.LatLng(routing.start.lat, routing.start.lng);
                           Router.googleMap.setCenter(start);
                       }
                       
                       if (routing.dest) {
                           var dest  = new google.maps.LatLng(routing.dest.lat, routing.dest.lng);
                           Router.destinationMarker.setPosition(dest);
                       }
                       
                       var irIndex = document.getElementById("ir_index");
                       
                       var startTime = routing.starttime;
                       var cbStartDelayed = document.getElementById("cb_startdelayed");
                       if ( startTime != false && startTime != 'NIL' ) {
                           cbStartDelayed.checked = true;
                           document.getElementById("tb_starttime").value = startTime;
                           
                       } else {
                           cbStartDelayed.checked = false;
                       }
            
                       var duration = routing.stepmax/3600;
                       var selDuration = document.getElementById("sel_duration");
                       selDuration.value = duration;
                       
                       Router.courseGCLine.setMap(Router.googleMap);
                       Router.courseGCLine.setPath([Router.startMarker.getPosition(), Router.destinationMarker.getPosition()]);
                       
                       if (routing["nmea-connection"]) {
                           var nmeaInfo = routing["nmea-connection"];
                           if (nmeaInfo.port) {
                               document.getElementById("tb_nmeaport").value = nmeaInfo.port;
                           }
                       }
                   },
                   function (request) {
                       alert(request.statusText + ' ' + request.responseText);
                   });
    }

    function getRaceInfo () {
        Util.doGET(
            "/function/vh.getRaceInfo",
            function(xhr) {
                if (xhr.responseText) {
                    var raceinfo = JSON.parse(xhr.responseText);
                    if (raceinfo.data.checkpoints) {
                        alert("Wrong race type");
                    } else if (raceinfo.data.objectId) {
                        setupLegRS(raceinfo);
                    }
                } else {
                    alert("No leg info for race");
                }
            },
            function (xhr) {
                alert(`${xhr.status} ${xhr.statusText}: ${xhr.responseText}`);
            });
    }

    function setupLegRS (raceinfo) {
        var rsData = raceinfo.data;
        
        document.title = rsData.name;
        
        var markPort = 'img/mark_red.png';
        
        Router.setParameter("polars", rsData.polar.objectId);
        Router.loadPolars(rsData.polar.objectId);

        var start = Router.positionFromDocumentURL();
        if (start) {
        } else {
            Router.startMarker.setPosition( {"lat": rsData.startLocation.latitude,
                                             "lng": rsData.startLocation.longitude});
            var startPos = new google.maps.LatLng(rsData.startLocation.latitude, rsData.startLocation.longitude);
            Router.setRoutePoint('start', startPos);
        }

        // Destination 
        var lastP0 = rsData.gates[rsData.gates.length-1][0]
        var lastP1 = rsData.gates[rsData.gates.length-1][1]
        // Gate midpoint - this will go wrong if the gate spans the 0 or 180 meridian
        var destLat = (lastP0.latitude + lastP1.latitude)/2;
        var destLon = (lastP0.longitude + lastP1.longitude)/2; 
        var destPos = new google.maps.LatLng(destLat, destLon);
        Router.setRoutePoint('dest', destPos);
        
        Router.googleMap.panTo(Router.startMarker.getPosition());

        var gateCount = 1; 
        for (const gate of rsData.gates) {
            var mark0 = new google.maps.Marker({
                position: {"lat": gate[0].latitude,
                           "lng": gate[0].longitude},
                map: Router.googleMap,
                // icon: markPort,
                label: `${ gateCount }`,
                title: `Gate ${ gateCount }`,
                draggable: false
            });
            var mark1 = new google.maps.Marker({
                position: {"lat": gate[1].latitude,
                           "lng": gate[1].longitude},
                map: Router.googleMap,
                // icon: markPort,
                label: `${ gateCount }`,
                title: `Gate ${ gateCount }`,
                draggable: false
            });
            gateCount++;
        }
    }
    
    function getBoatPosition (event) {
        var port= document.getElementById("tb_nmeaport").value
        Util.doGET(
            "/function/vh.getBoatPosition?port=" + port,
            function (xhr) {
                console.log(xhr);
                if (xhr.responseText) {
                    var data = JSON.parse(xhr.responseText);
                    var startPos = new google.maps.LatLng(data.position.lat, data.position.lng);
                    setRoutePoint('start', startPos);
                    alert('Position ' + JSON.stringify(startPos) + ' at ' + data.time);
                    var curTime = new Date(data.time);
                    var isoDate = curTime.toISOString().substring(0,16);
                    var dateInput = document.getElementById("tb_starttime");
                    dateInput.value = isoDate;
                    setParameter('starttime', isoDate);
                } else {
                    alert('No position update');
                }
            },
            function (xhr) {
                alert(`${xhr.status} ${xhr.statusText}: ${xhr.responseText}`);
            });
    }
    
    function resetNMEAConnection (event) {
        var port= document.getElementById("tb_nmeaport").value
        $.ajax({
            url: "/function/vh.resetNMEAConnection?port=" + port,
            dataType: 'json'
        }).done( function (data) {
            console.log(data);
            alert("Connected to " + data.peer);
        }).fail( function (request, textStatus, errorThrown) {
            console.log(errorThrown);
            alert(request.responseText);
        });
    }

    function setUpRS () {
        Router.setUp();

        document.getElementById("bt_nmeaupdate").addEventListener("click",getBoatPosition);
        document.getElementById("bt_nmeareset").addEventListener("click",resetNMEAConnection);

        getRaceInfo()
        getSession();
    }

    window.addEventListener("load", function (event) {
        setUpRS();
    });

}) ()

/// EOF
////////////////////////////////////////////////////////////////////////////////
