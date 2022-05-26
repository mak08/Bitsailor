////////////////////////////////////////////////////////////////////////////////
/// BitSailor Router UI

import * as Util from './Util.js';
import * as Router from './router.js';

( function () {

    var rsData = {};
    
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

    function getVMG (windSpeed) {
        var polars = Router.polars;
        if (polars && polars.polarData) {
            var vJ = getMaxSpeed(polars.polarData.jib, windSpeed);
            var vG = getMaxSpeed(polars.polarData.gennaker, windSpeed);
            var vS = getMaxSpeed(polars.polarData.spi, windSpeed);
            
            return {
                "up": vJ.up.vmg.toFixed(2) + '@' + vJ.up.twa.split('.')[0],
                "down": Math.abs(vS.down.vmg).toFixed(2) + '@' + vS.down.twa.split('.')[0]
            }
        } else {
            return {
                "up": "-",
                "down": "-"
            }
        }           
    }
    
    function getMaxSpeed(sail, windSpeed) {
        
        var vmgUp = sail.reduce ((acc, next) => {
            let vmgNext =  twaVMG(next, windSpeed);
            if (acc.vmg > vmgNext.vmg) {
                return acc;
            } else {
                return vmgNext;
            }
        }, 0);
        var vmgDown = sail.reduce ((acc, next) => {
            let vmgNext =  twaVMG(next, windSpeed);
            if (acc.vmg < vmgNext.vmg) {
                return acc;
            } else {
                return vmgNext;
            }
        }, 0);
        
        return {"up": vmgUp, "down": vmgDown };
    }
    
    function twaVMG (twaSpeeds, windSpeed) {
        var s0 = 0;
        var v0 = 0;
        var s1 = 0;
        var v1 = 0;
        var twa = twaSpeeds.twa;
        for (const m in twaSpeeds) {
            if ( m != "twa" ) {
                v0 = v1;
                s0 = s1;
                v1 = Number.parseFloat(twaSpeeds[m]);
                s1 = Number.parseFloat(m);
                if (s1 >= windSpeed) {
                    break;
                }
            }
        }
        return {
            "twa": twa,
            "vmg": Math.cos(Util.toRad(twa)) * Util.linear(windSpeed, s0, s1, v0, v1)
        };
    }

    function setupLegRS (raceinfo) {
        rsData = raceinfo.data;
        document.title = rsData.name;

        Router.loadPolars(rsData.polar.objectId);

        if (rsData.gfs025 == "TRUE") {
            Router.setResolution("0p25");
        } else {
            Router.setResolution("1p00");
        }
        
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
                label: `${ gateCount }`,
                title: `Gate ${ gateCount }`,
                draggable: false
            });
            var mark1 = new google.maps.Marker({
                position: {"lat": gate[1].latitude,
                           "lng": gate[1].longitude},
                map: Router.googleMap,
                label: `${ gateCount }`,
                title: `Gate ${ gateCount }`,
                draggable: false
            });
            gateCount++;
        }

        // NMEA port
        var storage = window.localStorage;
        var nmeaPort = storage.getItem(`${rsData.objectId}.nmea_port`);
        var portTB = document.getElementById("tb_nmeaport");
        portTB.value = nmeaPort;
        
    }
    
    function getBoatPosition (event) {
        var port= document.getElementById("tb_nmeaport").value
        Util.doGET(
            "/function/vh.getBoatPosition?port=" + port,
            function (xhr) {
                console.log(xhr);
                var data = xhr.responseText && JSON.parse(xhr.responseText);
                if (data) {
                    var startPos = new google.maps.LatLng(data.position.lat, data.position.lng);
                    Router.setRoutePoint('start', startPos);
                    var currTime = new Date();
                    var nmeaTime = new Date(data.time);
                    if ((currTime - nmeaTime) < 300000) {
                        alert(`Time ${data.time}\nPos ${Router.formatLatLngPosition(startPos)}\nSpeed ${parseFloat(data.speed).toFixed(1)}\nCourse ${data.course}`);
                    } else {
                        alert(`Outdated position, please update again in a few seconds`);
                    }
                    var isoDate = currTime.toISOString().substring(0,16);
                    var dateInput = document.getElementById("tb_starttime");
                    dateInput.value = isoDate;
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
        var storage = window.localStorage;
        storage.setItem(`${rsData.objectId}.nmea_port`, port);
        
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
 
        Router.setUp(getVMG);
        Router.settings.presets = "RS";
        
        document.getElementById("bt_nmeaupdate").addEventListener("click", getBoatPosition);
        document.getElementById("bt_nmeareset").addEventListener("click", resetNMEAConnection);
        document.getElementById("sel_resolution").addEventListener("change", Router.onSetResolution);

        getRaceInfo()

        Router.updateMap();
        
    }

    window.addEventListener("load", function (event) {
        setUpRS();
    });

}) ()

/// EOF
////////////////////////////////////////////////////////////////////////////////
