////////////////////////////////////////////////////////////////////////////////
/// BitSailor Router UI

import * as Util from './Util.js';
import * as Router from './router.js';

( function () {

    var vrData = {};
    
    function getRaceInfo () {
        Util.doGET(
            "/function/router.getRaceInfo",
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
        var polars = Router.getPolars().scriptData.polar;

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
            var speed = boatSpeed(tws, twa, options, polars).speed;
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

    function boatSpeed (tws, twa, options, polars) {
        const sailNames = [0, "Jib", "Spi", "Stay", "LJ", "C0", "HG", "LG"];
        var foil = foilingFactor(options, tws, twa, polars.foil);
        var hull = options.includes("hull") ? 1.003 : 1.0;
        var ratio = polars.globalSpeedRatio;
        var twsLookup = fractionStep(tws, polars.tws);
        var twaLookup = fractionStep(twa, polars.twa);
        var speed = maxSpeed(options, twsLookup, twaLookup, polars.sail);
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
                let polars = Router.getPolars().scriptData.polar;
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
                    let speedHDG = boatSpeed(Util.ms2knots(windHDG.speed), hdgTWA, options, polars).speed;

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
    
    
    function setupLegVR (raceinfo) {
        vrData = raceinfo.data;
        document.title = vrData.name;

        var markStbd = 'img/mark_green.png';
        var markPort = 'img/mark_red.png';

        Router.loadPolars( vrData.boat.polar_id);

        // Resolution
        if (vrData.fineWinds == "TRUE") {
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
                start = vrData.start;
            }
        }
        start = new google.maps.LatLng(start.lat, start.lon);
        // setRoutePoint also updates storage via updateStartPosition
        Router.setRoutePoint('start', start);
        var textBox = document.getElementById("tb_position");
        textBox.value = Util.formatPosition(start.lat(), start.lng()); 

        if (queryParams.startTime) {
            var cbDelayed = document.getElementById('cb_startdelayed');
            var tbStartTime = document.getElementById('tb_starttime');
            cbDelayed.checked = true;
            tbStartTime.value = queryParams.startTime.toISOString().substring(0,16);
        }

        let dest = Router.getValue('dest');
        if (dest) {
            dest = JSON.parse(dest);
        } else {
            dest =  {"lat": vrData.end.lat, "lon": vrData.end.lon};
        }
        var destPos = new google.maps.LatLng(dest.lat, dest.lon);
        Router.setRoutePoint('dest', destPos);
        
        Router.googleMap.panTo(Router.startMarker.getPosition());

        for (const c of vrData.checkpoints) {
            var mark = new google.maps.Marker({
                position: {"lat": c.start.lat,
                           "lng": c.start.lon},
                map: Router.googleMap,
                icon: (c.side=='port')?markPort:markStbd,
                label: `${ c.group }`,
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

    function onOptionToggled (event) {
        Router.settings.options = Router.settings.options.filter(e => e !== event.currentTarget.name);
        if (event.currentTarget.checked) {
            Router.settings.options.unshift(event.currentTarget.name);
        }
        Router.storeValue('options', JSON.stringify(Router.settings.options));
    }

    function setUpVR () {
        Router.setUp(getVMG);
        Router.settings.presets = "VR";
        let options =  Router.getValue('options');
        if (options) {
            Router.settings.options = JSON.parse(options);
        } else {
            Router.settings.options = ['winch'];
        }

        document.getElementById("sel_resolution").addEventListener("change", Router.onSetResolution);

        document.getElementById("cb_hull").checked  = Router.settings.options.includes('hull');
        document.getElementById("cb_winch").checked = Router.settings.options.includes('winch');
        document.getElementById("cb_foil").checked  = Router.settings.options.includes('foil');
        document.getElementById("cb_heavy").checked = Router.settings.options.includes('heavy');
        document.getElementById("cb_light").checked = Router.settings.options.includes('light');
        document.getElementById("cb_reach").checked = Router.settings.options.includes('reach');
        document.getElementById("cb_hull").addEventListener("click", onOptionToggled);
        document.getElementById("cb_winch").addEventListener("click", onOptionToggled);
        document.getElementById("cb_foil").addEventListener("click", onOptionToggled);
        document.getElementById("cb_heavy").addEventListener("click", onOptionToggled);
        document.getElementById("cb_light").addEventListener("click", onOptionToggled);
        document.getElementById("cb_reach").addEventListener("click", onOptionToggled);

        getRaceInfo();

        google.maps.event.addListener(Router.googleMap, 'click', getTWAPath);
        // google.maps.event.addListener(Router.googleMap, 'mousemove', computePath);

        Router.updateMap();
    }
    
    window.addEventListener("load", function (event) {
        setUpVR();
    });
    
}) ()

/// EOF
////////////////////////////////////////////////////////////////////////////////
