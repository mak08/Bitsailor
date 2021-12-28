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

    function getVMG (windSpeed) {
        var polars = Router.polars.scriptData.polar;

        if (polars) {
            var vmg = bestVMG(windSpeed, polars, ["heavy", "light", "reach", "hull", "foil"])
            return {
                "up": vmg.vmgUp.toFixed(2) + '@' + vmg.twaUp.toFixed(0),
                "down": Math.abs(vmg.vmgDown).toFixed(2) + '@' + vmg.twaDown.toFixed(0)
            }
        }
    }

    function bestVMG (tws, polars, options) {
        var best = {"vmgUp": 0, "twaUp": 0, "vmgDown": 0, "twaDown": 0};
        var twaSteps = polars.twa;
        for (var twa = twaSteps[0]; twa < twaSteps[twaSteps.length-1]; twa++) {
            var speed = theoreticalSpeed(tws, twa, options, polars).speed;
            var vmg = speed * Math.cos(twa / 180 * Math.PI);
            if (vmg > best.vmgUp) {
                best.twaUp = twa;
                best.vmgUp = vmg;
            } else if (vmg < best.vmgDown) {
                best.twaDown = twa;
                best.vmgDown = vmg;
            }
        }
        return  best;
    }


    function theoreticalSpeed (tws, twa, options, boatPolars) {
        const sailNames = [0, "Jib", "Spi", "Stay", "LJ", "C0", "HG", "LG"];
        var foil = foilingFactor(options, tws, twa, boatPolars.foil);
        var hull = options.includes("hull") ? 1.003 : 1.0;
        var ratio = boatPolars.globalSpeedRatio;
        var twsLookup = fractionStep(tws, boatPolars.tws);
        var twaLookup = fractionStep(twa, boatPolars.twa);
        var speed = maxSpeed(options, twsLookup, twaLookup, boatPolars.sail);
        return {
            "speed": Util.roundTo(speed.speed * foil * hull * ratio, 2),
            "sail": sailNames[speed.sail]
        };
    }
    
    function maxSpeed (options, iS, iA, sailDefs) {
        var maxSpeed = 0;
        var maxSail = "";
        for (const sailDef of sailDefs) {
            if (sailDef.id === 1
                || sailDef.id === 2
                || (sailDef.id === 3 && options.includes("heavy"))
                || (sailDef.id === 4 && options.includes("light"))
                || (sailDef.id === 5 && options.includes("reach"))
                || (sailDef.id === 6 && options.includes("heavy"))
                || (sailDef.id === 7 && options.includes("light"))) {
                var speed = pSpeed(iA, iS, sailDef.speed);
                if (speed > maxSpeed) {
                    maxSpeed = speed;
                    maxSail = sailDef.id;
                }
            }
        }
        return {
            speed: maxSpeed,
            sail: maxSail
        }
    }

    function pSpeed (iA, iS, speeds) {
        return bilinear(iA.fraction, iS.fraction,
                        speeds[iA.index - 1][iS.index - 1],
                        speeds[iA.index][iS.index - 1],
                        speeds[iA.index - 1][iS.index],
                        speeds[iA.index][iS.index]);
    }

    function foilingFactor (options, tws, twa, foil) {
        var speedSteps = [0, foil.twsMin - foil.twsMerge, foil.twsMin, foil.twsMax, foil.twsMax + foil.twsMerge, Infinity];
        var twaSteps = [0, foil.twaMin - foil.twaMerge, foil.twaMin, foil.twaMax, foil.twaMax + foil.twaMerge, Infinity];
        var foilMat = [[1, 1, 1, 1, 1, 1],
                       [1, 1, 1, 1, 1, 1],
                       [1, 1, foil.speedRatio, foil.speedRatio, 1, 1],
                       [1, 1, foil.speedRatio, foil.speedRatio, 1, 1],
                       [1, 1, 1, 1, 1, 1],
                       [1, 1, 1, 1, 1, 1]];
        
        if (options.includes("foil")) {
            var iS = fractionStep(tws, speedSteps);
            var iA = fractionStep(twa, twaSteps);
            return bilinear(iA.fraction, iS.fraction,
                            foilMat[iA.index - 1][iS.index - 1],
                            foilMat[iA.index][iS.index - 1],
                            foilMat[iA.index - 1][iS.index],
                            foilMat[iA.index][iS.index]);
        } else {
            return 1.0;
        }
    }
    
    function bilinear (x, y, f00, f10, f01, f11) {
        return f00 * (1 - x) * (1 - y)
            + f10 * x * (1 - y)
            + f01 * (1 - x) * y
            + f11 * x * y;
    }
    
    function fractionStep (value, steps) {
        var absVal = Math.abs(value);
        var index = 0;
        while (index < steps.length && steps[index] <= absVal) {
            index++;
        }
        if (index < steps.length) {
            return {
                index: index,
                fraction: (absVal - steps[index - 1]) / (steps[index] - steps[index - 1])
            }
        } else {
            return {
                index: index - 1,
                fraction: 1.0
            }
        }
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

    function  onContextMenuSetStart (event) {
        var textBox = document.getElementById("tb_position");
        var position = Router.mapEvent.latLng;
        textBox.value = Util.formatPosition(position.lat(), position.lng()); 
    }
    
    function  onUpdateStartMarker (marker) {
        var textBox = document.getElementById("tb_position");
        var position = marker.getPosition();
        textBox.value = Util.formatPosition(position.lat(), position.lng()); 
    }
    
    function onSetPosition (event) {
        var position = document.getElementById("tb_position").value;
        var latlon = parsePosition(position);
        if (latlon) {
            var latLon = new google.maps.LatLng(latlon.lat, latlon.lon);
            Router.setRoutePoint('start', latLon);
        }
    }


    const patterns = {
        lat: {
            isDMS: /^(([1-8]?[0-9])\D+([1-5]?[0-9]|60)\D+([1-5]?[0-9]|60)(\.[0-9]+)?|90\D+0\D+0)\D+[NSns]$/,
            isDDM: /^(([1-8]?[0-9])\D+[1-6]?[0-9](\.\d{1,3})?|90(\D+0)?)\D+([NSns])$/,
            isDD:  /^[\+-]?(([1-8]?[0-9])(\.\d{1,6})?|90)\D*[NSns]?$/
        },
        lng: {
            isDMS: /^((1[0-7][0-9]|[1-9]?[0-9])\D+([1-5]?[0-9]|60)\D+([1-5]?[0-9]|60)(\.[0-9]+)?|180\D+0\D+0)\D+[EWew]$/,
            isDDM: /^((1[0-7][0-9]|[1-9]?[0-9])\D+[1-6]?[0-9](\.\d{1,3})?|180(\D+0)?)\D+([EWew])$/,
            isDD:  /^[\+-]?((1[0-7][0-9]|[1-9]?[0-9])(\.\d{1,6})?|180)\D*[EWew]?$/
        }
    };

    function parsePosition (string) {
        try {
            // Assume two comma separated DMS values
            var parts = string.split(',');
            if (parts.length != 2) {
                // Alternatively try blank separated numbers.
                // In this cas we don't support blanks inside the DMS values
                parts = string.split(' ');
            }
            if (parts.length != 2) {
                throw new Error(`Invalid LatLng ${string}`)
            }

            // We assume the first value to designate Lat, second Lon.
            var lat, lon;
            if (parts[0].match(/E|W/)) {
                lon = parts[0].trim();
                lat = parts[1].trim();
            } else {
                lat = parts[0].trim();
                lon = parts[1].trim();
            }
            return {
                "lat": parseDMS(lat),
                "lon": parseDMS(lon)
            }
        } catch (e) {
            alert(e);
        }
    }

    function parseDMS (string) {
        // nnn.nnnnn or nnn°nn.nnnnn' or nnn°nn'nn.nnnnn"
        var sign = string.match(/W|S/)?-1:1;
        string = string.split(/[NESW]/)[0];
        var parts = string.split('°');
        var degrees = parseFloat(parts[0]);
        if (parts[1]) {
            parts = parts[1].split('\'');
            degrees += parseFloat(parts[0])/60;
            if (parts[1]) {
                degrees += parseFloat(parts[1].split('"')[0])/3600;
            }
        }
        if (isNaN(degrees)) {
            throw new Error(`Invalid DMS ${string}, valid formats are nnn.nnnnn, nnn°nn.nnnnn', nnn°nn'nn.nnnnn`);
        } else {
            return sign * degrees;
        }
    }
    
    
    function setUpVR () {

        Router.setUp(getVMG);
        
        document.getElementById("bt_position").addEventListener("click", onSetPosition);
        document.getElementById("bt_setstart").addEventListener("click", onContextMenuSetStart);
        google.maps.event.addListener(Router.startMarker,'dragend', function () {
            // Update position entry/display
            onUpdateStartMarker(Router.startMarker);
        });
      
        getRaceInfo()
        getSession();

        Router.updateMap();
      
    }
    
    window.addEventListener("load", function (event) {
        setUpVR();
    });
    
}) ()

/// EOF
////////////////////////////////////////////////////////////////////////////////
