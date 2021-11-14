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
                        setupLegVR(raceinfo);
                    } else if (raceinfo.data.objectId) {
                        alert("Wrong race type");
                    }
                } else {
                    alert("No leg info for race");
                }
            },
            function (xhr) {
                alert(`${xhr.status} ${xhr.statusText}: ${xhr.responseText}`);
            });
    }
    
    function setupLegVR (raceinfo) {
        var vrData = raceinfo.data;
        
        document.title = vrData.name;

        var markStbd = 'img/mark_green.png';
        var markPort = 'img/mark_red.png';

        Router.setParameter("polars", vrData.boat.polar_id);
        Router.loadPolars( vrData.boat.polar_id);

        var start = Router.positionFromDocumentURL();
        if (start) {
        } else {
            Router.startMarker.setPosition( {"lat": vrData.start.lat, "lng": vrData.start.lon});
            var startPos = new google.maps.LatLng(vrData.start.lat, vrData.start.lon);
            Router.setRoutePoint('start', startPos);
        }
        
        Router.destinationMarker.setPosition( {"lat": vrData.end.lat, "lng": vrData.end.lon});
        var destPos = new google.maps.LatLng(vrData.end.lat, vrData.end.lon);
        Router.setRoutePoint('dest', destPos);
        
        Router.googleMap.panTo(Router.startMarker.getPosition());

        for (const c of vrData.checkpoints) {
            var mark = new google.maps.Marker({
                position: {"lat": c.start.lat, "lng": c.start.lon},
                map: Router.googleMap,
                icon: (c.side=='port')?markPort:markStbd,
                title: c.group + "-" + c.id + " " + c.name,
                draggable: false
            });
            mark.addListener('click', function () { Router.onMarkerClicked(mark) });
        }
        if (vrData.ice_limits) {
            var iceLimit = [];
            for (const p of vrData.ice_limits.south) {
                iceLimit.push({"lat": p.lat, "lng": p.lon});
            }
            var iceLine = new google.maps.Polyline({
                geodesic: false,
                strokeColor: '#ff0000',
                strokeOpacity: 1.0,
                strokeWeight: 1
            });
            iceLine.setMap(Router.googleMap);
            iceLine.setPath(iceLimit);
        }
    }
    
    function setUpVR () {
        Router.setUp();
        getRaceInfo()
        getSession();
    }

    window.addEventListener("load", function (event) {
        setUpVR();
    });

}) ()

/// EOF
////////////////////////////////////////////////////////////////////////////////
