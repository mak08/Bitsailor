////////////////////////////////////////////////////////////////////////////////
/// BitSailor Router UI

import * as Util from './Util.js';
import * as Router from './router.js';

( function () {

    var rsData = {};
    let polarsMap = new Map();
    
    function getRaceInfo () {
        Util.doGET(
            "/function/router.getRaceInfo",
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
        let polars = Router.getPolars();
        if (polars && polars.polarData) {
            let data = polars.polarData;
            if (data.boat) {
                let vS = getMaxSpeed(data.boat, windSpeed);
                return  {
                    "up": vS.up.vmg.toFixed(2) + '@' + vS.up.twa.split('.')[0],
                    "down": Math.abs(vS.down.vmg).toFixed(2) + '@' + vS.down.twa.split('.')[0]
                }
            } else {
                let vJ = getMaxSpeed(data.jib, windSpeed);
                let vG = getMaxSpeed(data.gennaker, windSpeed);
                let vS = getMaxSpeed(data.spi, windSpeed);
                
                return {
                    "up": vJ.up.vmg.toFixed(2) + '@' + vJ.up.twa.split('.')[0],
                    "down": Math.abs(vS.down.vmg).toFixed(2) + '@' + vS.down.twa.split('.')[0]
                }
            }
        } else {
            return {
                "up": "-",
                "down": "-"
            }
        }           
    }
    
    function boatSpeed (tws, twa, options, polars) {
        if (polars.polarData.boat) {
            return {
                "speed": getTWASpeed(tws, twa, polars.polarData.boat)
            }
        } else {
            let jib = getTWASpeed(tws, twa, polars.polarData.jib);
            let gen = getTWASpeed(tws, twa, polars.polarData.gennaker);
            let spi = getTWASpeed(tws, twa, polars.polarData.spi);
            return {
                "speed":  Math.max(jib, Math.max(gen, spi))
            };
        }
    }

    function getTWASpeed (tws, twa, sail) {
        let twaSpeeds = sail[Math.abs(twa)];
        let s0 = 0;
        let v0 = 0;
        let s1 = 0;
        let v1 = 0;
        for (const m in twaSpeeds) {
            if ( m != "twa" ) {
                v0 = v1;
                s0 = s1;
                v1 = Number.parseFloat(twaSpeeds[m]);
                s1 = Number.parseFloat(m);
                if (s1 >= tws) {
                    break;
                }
            }
        }
        let speed = Util.linear(tws, s0, s1, v0, v1);
        return speed;
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

    function onSelectPolars (event) {
        let polars = polarsMap.get(event.currentTarget.value);
        Router.setPolars(polars);
        Router.settings.polarsId = polars.objectId;
    }

    function addPolars (data) {
        polarsMap.set(data.classBoat, data);
        let selPolars = document.getElementById("sel_polars");
        let option = document.createElement("option");
        option.text = data.classBoat;
        selPolars.add(option);
        selPolars.value = data.classBoat;
    }
    
    let curTWA;

    async function computePath (event) {
        if ( Router.twaAnchor ) {
            let twaAnchor = Router.twaAnchor;

            // Start time
            let time = twaAnchor.get('time');
            let startTime = new Date(time);

            // Start and target postion
            let slat = twaAnchor.getPosition().lat();
            let slon = twaAnchor.getPosition().lng();
            let dlat = event.latLng.lat();
            let dlon = event.latLng.lng();
            let targetDist = Util.gcDistance({"lat": slat, "lon": slon}, {"lat": dlat, "lon": dlon});
            let pathDist = 0;
            // Heading and  TWA
            let heading = Util.toDeg(Util.courseAngle(slat, slon, dlat, dlon));
            let wind = await Router.windTile.getWind(slat, slon, startTime);
            let twa = Math.round(Util.toTWA(heading, wind.direction));

            if (twa != curTWA) {
                document.getElementById('lb_twa').innerHTML = twa;
                document.getElementById('lb_twa_heading').innerHTML = heading.toFixed(1);
                let options = Router.settings.options;
                let polars = Router.getPolars();
                let newTWAPos = {
                    "lat": slat,
                    "lon": slon
                }
                let newHDGPos = {
                    "lat": slat,
                    "lon": slon
                }
                
                let twaPath = [[startTime, {"lat": newTWAPos.lat, "lng": newTWAPos.lon}]];
                let hdgPath = [[startTime, {"lat": newHDGPos.lat, "lng": newHDGPos.lon}]];

                let stepTime = startTime;
                let step0 = 600 - startTime.getSeconds() - 60 * (startTime.getMinutes() % 10);
                let delta = step0;
                
                for (var step = step0; step < 86400 && pathDist < targetDist; step += 600) {

                    // Calc TWA and HDG step
                    let windTWA = await Router.windTile.getWind(newTWAPos.lat, newTWAPos.lon, stepTime);
                    let windHDG = await Router.windTile.getWind(newHDGPos.lat, newHDGPos.lon, stepTime);

                    let twaHeading = Util.toHeading(twa, windTWA.direction);
                    let hdgTWA = Util.toTWA(heading, windHDG.direction);

                    let speedTWA = boatSpeed(Util.ms2knots(windTWA.speed), twa, options, polars).speed;
                    let speedHDG = boatSpeed(Util.ms2knots(windHDG.speed),  Math.round(hdgTWA), options, polars).speed;

                    let distTWA = delta * (speedTWA/3600);
                    let distHDG = delta * (speedHDG/3600);
                    
                    newTWAPos = Util.addDistance(newTWAPos, distTWA, twaHeading);
                    newHDGPos = Util.addDistance(newHDGPos, distHDG, heading);

                    // Increment stepTime AFTER step distance calc but BEFORE adding new point
                    stepTime = new Date(startTime.getTime() + step * 1000);

                    // Add new point
                    twaPath.push([stepTime, {"lat": newTWAPos.lat, "lng": newTWAPos.lon}]);
                    hdgPath.push([stepTime, {"lat": newHDGPos.lat, "lng": newHDGPos.lon}]);

                    // Termination distance
                    pathDist += distTWA;

                    // Step width after initial step
                    delta = 600;

                }
                Router.drawTWAPath(twaPath);
                Router.drawHDGPath(hdgPath);
                curTWA = twa;
            }
        }
    }
     
    function setupLegRS (raceinfo) {
        rsData = raceinfo.data;
        document.title = rsData.name;

        if (rsData.multiClass == "TRUE") {
            for (const id of rsData.classes) {
                Router.loadPolars(id, addPolars);
            }
        } else {
            Router.loadPolars(rsData.polar.objectId);
        }

        // Resolution
        if (rsData.gfs025 == "TRUE") {
            Router.setResolution("0p25");
        } else {
            Router.setResolution("1p00");
        }
        let resolution = Router.getValue('resolution');
        if (resolution) {
            Router.setResolution(resolution);
        }
        
        // Duration
        let duration = Router.getValue('duration');
        if (duration) {
            Router.setDuration(duration);
        }
        
        var queryParams = Router.getURLParams()
        var start = queryParams.startPos;
        if (start) {
        } else {
            start = Router.getValue('start');
            if (start) {
                start = JSON.parse(start);
            } else {
                start = {'lat': rsData.startLocation.latitude,
                         'lon': rsData.startLocation.longitude};
            }
        }
        let startPos = new google.maps.LatLng(start.lat, start.lon);
        // setRoutePoint also updates storage via updateStartPosition
        Router.setRoutePoint('start', startPos);
        Router.googleMap.panTo(Router.startMarker.getPosition());


        // Destination
        if (rsData.hunt != 'TRUE') {
            var lastP0 = rsData.gates[rsData.gates.length-1][0]
            var lastP1 = rsData.gates[rsData.gates.length-1][1]

            let dest = Router.getValue('dest');
            if (dest) {
                dest = JSON.parse(dest);
            } else {
                // Gate midpoint - this will go wrong if the gate spans the 0 or 180 meridian
                dest = {
                    'lat': (lastP0.latitude + lastP1.latitude)/2,
                    'lon': (lastP0.longitude + lastP1.longitude)/2
                }
            }
            var destPos = new google.maps.LatLng(dest.lat, dest.lon);
            Router.setRoutePoint('dest', destPos);
            
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
        }

        // NMEA port
        var nmeaPort = Router.getValue('nmea_port');
        var portTB = document.getElementById("tb_nmeaport");
        portTB.value = nmeaPort;
        
    }
    
    function getBoatPosition (event) {
        var port= document.getElementById("tb_nmeaport").value
        Util.doGET(
            "/function/router.getBoatPosition?port=" + port,
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
        Router.storeValue('nmea_port', port);
        
        $.ajax({
            url: "/function/router.resetNMEAConnection?port=" + port,
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
        Router.settings.options = ["realsail"];
        Router.setSailnames(["Jib", "Spi", "Code0"]);
        
        document.getElementById("bt_nmeaupdate").addEventListener("click", getBoatPosition);
        document.getElementById("bt_nmeareset").addEventListener("click", resetNMEAConnection);
        document.getElementById("sel_resolution").addEventListener("change", Router.onSetResolution);
        document.getElementById("sel_gfsmode").addEventListener("change", Router.onSetGFSMode);
        let selPolars = document.getElementById("sel_polars");
        if (selPolars) {
            selPolars.addEventListener("change", onSelectPolars);
        }

        getRaceInfo()

        google.maps.event.addListener(Router.googleMap, 'click', computePath);
        // google.maps.event.addListener(Router.googleMap, 'mousemove', computePath);

        Router.updateMap();
    }

    window.addEventListener("load", function (event) {
        setUpRS();
    });

}) ()

/// EOF
////////////////////////////////////////////////////////////////////////////////
