////////////////////////////////////////////////////////////////////////////////
/// BitSailor Router VR UI (Leaflet/OpenStreetMap version)

import * as Util from './Util.js';
import * as Router from './router.js';

// Global variables
let map = null;
let startMarker = null;
let destinationMarker = null;
let routeLine = null;
let trackMarkers = [];
let routeTracks = [];
let polarsList = [];
let raceInfo = {};
let vrData = null;
let curTWA;

// Initialize everything when the page loads
window.addEventListener("load", function() {
    initializeRouterVR();
});

// Main initialization function
function initializeRouterVR() {
    // Initialize map first
    Router.initMap();
    
    // Set up event handlers
    setupEventHandlers();
    
    // Then load data (which will add markers once map is ready)
    loadPolarsAndRaceInfo();
}


// Set up UI controls and event handlers
function setupEventHandlers() {
    // Map event handlers
    Router.map.on('contextmenu', onMapRightClick);
    Router.map.on('click', onMapClick);
    Router.map.on('click', computePath);
    Router.map.on('dblclick', getTWAPath);
    
    // Button handlers
    document.getElementById('bt_setstart').onclick = function() {
        setRoutePoint('start', Router.map.getCenter());
    };
    document.getElementById('bt_setdest').onclick = function() {
        setRoutePoint('dest', Router.map.getCenter());
    };
    
    // Set up other UI elements
    document.getElementById("sel_resolution").addEventListener("change", Router.onSetResolution);
    document.getElementById("sel_polars").addEventListener("change", Router.onSetPolars);
    
    // Checkbox handlers
    const options = ["hull", "winch", "foil", "heavy", "light", "reach"];
    options.forEach(option => {
        const checkbox = document.getElementById(`cb_${option}`);
        if (checkbox) {
            checkbox.checked = Router.settings.options.includes(option);
            checkbox.addEventListener("click", onOptionToggled);
        }
    });
}

// Load polars and race info
function loadPolarsAndRaceInfo() {
    // Set up Router
    Router.setUp(getVMG);
    Router.settings.presets = "VR";
    
    // Load options from URL if available
    const queryParams = Router.getURLParams();
    if (queryParams.options) {
        try {
            Router.settings.options = JSON.parse(queryParams.options);
        } catch (e) {
            console.warn(e);
            Router.settings.options = ["hull", "winch", "foil", "heavy", "light", "reach"];
        }
    }
    
    // Get polars list
    getPolarsList();
    
    // Get race info (this will create markers)
    getRaceInfo();
}

// Get polars list
function getPolarsList() {
    Util.doGET(
        "/function/router.getPolarsList",
        function(xhr) {
            polarsList = JSON.parse(xhr.responseText);
        },
        function(xhr) {
            console.error('Could not load polars list');
        }
    );
}

// Get race info
function getRaceInfo() {
    Util.doGET(
        "/function/router.getRaceInfo",
        function(xhr) {
            raceInfo = JSON.parse(xhr.responseText);
            setupLegVR(raceInfo);
        },
        function(xhr) {
            console.error('Could not load race info');
        }
    );
}

// Set up VR leg based on race info
function setupLegVR(raceinfo) {
    if (!raceinfo || !raceinfo.data) return;
    
    vrData = raceinfo.data;
    document.title = vrData.name;

    // Initialize polars
    let selPolars = document.getElementById('sel_polars');
    if (selPolars) {
        selPolars.value = vrData.boat.polar_id;
    }
    
    Router.loadPolars(vrData.boat.polar_id);

    // Set resolution
    if (vrData.fineWinds == "TRUE") {
        Router.setResolution("0p25");
    } else {
        Router.setResolution("1p00");
    }
    
    // Override with stored settings if available
    let resolution = Router.getValue('resolution');
    if (resolution) {
        Router.setResolution(resolution);
    }

    let duration = Router.getValue('duration');
    if (duration) {
        Router.setDuration(duration);
    }

    // Create markers
    createStartAndDestinationMarkers();
    
    // Create checkpoints
    createCheckpointMarkers();
    
    // Create ice limits and restricted zones
    createRouteBoundaries();
    
    // Update map
    Router.updateMap();
}

// Create start and destination markers
function createStartAndDestinationMarkers() {
    const queryParams = Router.getURLParams();
    
    // Start marker
    let start = queryParams.startPos;
    if (!start) {
        start = Router.getValue('start');
        if (start) {
            start = JSON.parse(start);
        } else {
            start = vrData.start;
        }
    }
    
    // Create start marker
    startMarker = L.marker([start.lat, start.lon], {
        title: 'Start',
        icon: L.icon({
            iconUrl: 'img/start_45x32.png',
            iconAnchor: [16, 16],
            popupAnchor: [0, -16],
        }),
        draggable: true
    }).addTo(Router.map);
    
    // Start marker drag event
    startMarker.on('dragend', function(e) {
        setRoutePoint('start', startMarker.getLatLng());
    });
    
    // Update position display
    Router.setRoutePoint('start', L.latLng(start.lat, start.lon));
    let textBox = document.getElementById("tb_position");
    if (textBox) {
        textBox.value = Util.formatPosition(start.lat, start.lon);
    }
    
    // Handle start time if specified
    if (queryParams.startTime) {
        let cbDelayed = document.getElementById('cb_startdelayed');
        let tbStartTime = document.getElementById('tb_starttime');
        if (cbDelayed && tbStartTime) {
            cbDelayed.checked = true;
            tbStartTime.value = queryParams.startTime.toISOString().substring(0,16);
        }
    }

    // Destination marker
    let dest = Router.getValue('dest');
    if (dest) {
        dest = JSON.parse(dest);
    } else {
        dest = {"lat": vrData.end.lat, "lon": vrData.end.lon};
    }
    
    // Create destination marker
    destinationMarker = L.marker([dest.lat, dest.lon], {
        title: 'Destination',
        icon: L.icon({
            iconUrl: 'img/finish_32x20.png',
            iconAnchor: [16, 16],
            popupAnchor: [0, -16],
        }),
        draggable: true
    }).addTo(Router.map);
    
    // Destination marker drag event
    destinationMarker.on('dragend', function(e) {
        setRoutePoint('dest', destinationMarker.getLatLng());
    });
    
    // Update router destination
    Router.setRoutePoint('dest', L.latLng(dest.lat, dest.lon));
    
    // Draw route line between markers
    drawRouteLine();
    
    // Center map on start position
    Router.map.panTo([start.lat, start.lon]);
    
    // Handle additional query parameters
    let energy = queryParams.energy;
    if (energy) {
        let energyField = document.getElementById('tb_currentenergy');
        if (energyField) {
            energyField.value = (energy*1).toFixed();
        }
    }
    
    let twa = queryParams.twa;
    if (twa) {
        let tackSelect = document.getElementById('sel_currenttack');
        if (tackSelect) {
            tackSelect.value = twa < 0 ? 'port' : 'stbd';
        }
    }

    let sail = queryParams.sail;
    if (sail) {
        let sailSelect = document.getElementById('sel_currentsail');
        if (sailSelect) {
            sailSelect.value = sail;
        }
    }
}

// Create checkpoint markers
function createCheckpointMarkers() {
    if (!vrData.checkpoints) return;
    
    const markStbd = 'img/mark_green.png';
    const markPort = 'img/mark_red.png';
    
    for (const c of vrData.checkpoints) {
        const mark = L.marker([c.start.lat, c.start.lon], {
            icon: L.icon({
                iconUrl: (c.side == 'port') ? markPort : markStbd,
                iconAnchor: [16, 16],
                popupAnchor: [0, -16],
            }),
            title: c.group + "-" + c.id + " " + c.name,
            draggable: false
        }).addTo(Router.map);
        
        mark.on('click', function() { 
            Router.onMarkerClicked(mark);
        });
    }
}

// Create ice limits and restricted zones
function createRouteBoundaries() {
    // Ice limits
    if (vrData.ice_limits) {
        const iceLimit = vrData.ice_limits.south.map(p => [p.lat, p.lon]);
        
        L.polyline(iceLimit, {
            color: '#ff0000',
            weight: 1,
            opacity: 1.0
        }).addTo(Router.map);
    }
    
    // Restricted zones
    if (vrData.restrictedZones) {
        for (const zone of vrData.restrictedZones) {
            if (!zone.vertices || zone.vertices.length === 0) continue;
            
            const path = zone.vertices.map(p => [p.lat, p.lon]);
            // Close the polygon by adding the first point again
            path.push([zone.vertices[0].lat, zone.vertices[0].lon]);
            
            L.polyline(path, {
                color: '#ff0000',
                weight: 1,
                opacity: 1.0
            }).addTo(map);
        }
    }
}

// Set route point (start or destination)
function setRoutePoint(type, latlng) {
    if (type === 'start' && startMarker) {
        startMarker.setLatLng(latlng);
        Router.setRoutePoint('start', latlng);
    } else if (type === 'dest' && destinationMarker) {
        destinationMarker.setLatLng(latlng);
        Router.setRoutePoint('dest', latlng);
    }
    drawRouteLine();
}

// Draw route line between start and destination
function drawRouteLine() {
    if (!startMarker || !destinationMarker) return;
    
    if (routeLine) {
        Router.map.removeLayer(routeLine);
    }
    
    routeLine = L.polyline([
        startMarker.getLatLng(),
        destinationMarker.getLatLng()
    ], {
        color: '#d00000',
        weight: 2,
        opacity: 1.0
    }).addTo(Router.map);
}

// Map right-click handler
function onMapRightClick(event) {
    let mapMenu = document.getElementById('mapMenu');
    if (!mapMenu) return false;
    
    mapMenu.style.display = 'block';
    mapMenu.style.zIndex = 400;
    mapMenu.style.top = event.originalEvent.pageY + 'px';
    mapMenu.style.left = event.originalEvent.pageX + 'px';
    window._lastMapEvent = event;
    return false;
}

// Map click handler
function onMapClick(event) {
    let mapMenu = document.getElementById('mapMenu');
    if (mapMenu) {
        mapMenu.style.display = 'none';
    }
}

// Option toggled handler
function onOptionToggled(event) {
    Router.settings.options = Router.settings.options.filter(e => e !== event.currentTarget.name);
    if (event.currentTarget.checked) {
        Router.settings.options.unshift(event.currentTarget.name);
    }
    Router.storeValue('options', JSON.stringify(Router.settings.options));
}

// VMG calculation
function getVMG(windSpeed) {
    let polars = Router.getPolars().scriptData.polar;
    if (polars) {
        var vmg = bestVMG(windSpeed, polars, ["heavy", "light", "reach", "hull", "foil"])
        return {
            "up": vmg.vmgUp.toFixed(2) + '@' + vmg.twaUp.toFixed(0),
            "down": Math.abs(vmg.vmgDown).toFixed(2) + '@' + vmg.twaDown.toFixed(0)
        }
    }
}

// Best VMG calculation
function bestVMG(tws, polars, options) {
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
    return best;
}

// Boat speed calculation
function boatSpeed(tws, twa, options, polars) {
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

// Max speed calculation
function maxSpeed(options, iS, iA, sailDefs) {
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

// Speed calculation
function pSpeed(iA, iS, speeds) {
    return bilinear(iA.fraction, iS.fraction,
                    speeds[iA.index - 1][iS.index - 1],
                    speeds[iA.index][iS.index - 1],
                    speeds[iA.index - 1][iS.index],
                    speeds[iA.index][iS.index]);
}

// Foiling factor calculation
function foilingFactor(options, tws, twa, foil) {
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

// Bilinear interpolation
function bilinear(x, y, f00, f10, f01, f11) {
    return f00 * (1 - x) * (1 - y)
        + f10 * x * (1 - y)
        + f01 * (1 - x) * y
        + f11 * x * y;
}

// Fraction step calculation
function fractionStep(value, steps) {
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

// Compute path for constant TWA
async function computePath(event) {
    if (!Router.twaAnchor) return;
    
    let twaAnchor = Router.twaAnchor;

    // Start time
    let time = twaAnchor.time;
    let startTime = new Date(time);

    // Start and target position
    let slat = twaAnchor.getLatLng().lat;
    let slon = twaAnchor.getLatLng().lng;
    let dlat = event.latlng.lat;
    let dlon = event.latlng.lng;
    let targetDist = Util.gcDistance({"lat": slat, "lon": slon}, {"lat": dlat, "lon": dlon});
    let pathDist = 0;
    
    // Heading and TWA
    let heading = Util.toDeg(Util.courseAngle(slat, slon, dlat, dlon));
    let wind = await Router.gribCache.getWindVR(slat, slon, startTime);
    let twa = Math.round(Util.toTWA(heading, wind.direction));

    if (twa != curTWA) {
        updateTWADisplay(twa, heading);
        calculateAndDisplayPaths(slat, slon, startTime, twa, heading, targetDist);
        curTWA = twa;
    }
}

// Update TWA display
function updateTWADisplay(twa, heading) {
    const twaElement = document.getElementById('lb_twa');
    const headingElement = document.getElementById('lb_twa_heading');
    
    if (twaElement) twaElement.innerHTML = twa;
    if (headingElement) headingElement.innerHTML = heading.toFixed(1);
}

// Calculate and display TWA and HDG paths
async function calculateAndDisplayPaths(slat, slon, startTime, twa, heading, targetDist) {
    let options = Router.settings.options;
    let polars = Router.getPolars().scriptData.polar;
    
    let newTWAPos = { "lat": slat, "lon": slon };
    let newHDGPos = { "lat": slat, "lon": slon };
    
    let twaPath = [[startTime, {"lat": newTWAPos.lat, "lng": newTWAPos.lon}]];
    let hdgPath = [[startTime, {"lat": newHDGPos.lat, "lng": newHDGPos.lon}]];

    let stepTime = startTime;
    let step0 = 600 - startTime.getSeconds() - 60 * (startTime.getMinutes() % 10);
    let delta = step0;
    let pathDist = 0;
    
    for (var step = step0; step < 86400 && pathDist < targetDist; step += 600) {
        // Calculate TWA and HDG step
        let windTWA = await Router.gribCache.getWindVR(newTWAPos.lat, newTWAPos.lon, stepTime);
        let windHDG = await Router.gribCache.getWindVR(newHDGPos.lat, newHDGPos.lon, stepTime);

        let twaHeading = Util.toHeading(twa, windTWA.direction);
        let hdgTWA = Util.toTWA(heading, windHDG.direction);

        let speedTWA = boatSpeed(Util.ms2knots(windTWA.speed), twa, options, polars).speed;
        let speedHDG = boatSpeed(Util.ms2knots(windHDG.speed), hdgTWA, options, polars).speed;

        let distTWA = delta * (speedTWA/3600);
        let distHDG = delta * (speedHDG/3600);
        
        newTWAPos = Util.addDistance(newTWAPos, distTWA, twaHeading);
        newHDGPos = Util.addDistance(newHDGPos, distHDG, heading);

        stepTime = new Date(startTime.getTime() + step * 1000);

        twaPath.push([stepTime, {"lat": newTWAPos.lat, "lng": newTWAPos.lon}]);
        hdgPath.push([stepTime, {"lat": newHDGPos.lat, "lng": newHDGPos.lon}]);

        pathDist += distTWA;
        delta = 600;
    }
    
    Router.drawTWAPath(twaPath);
    Router.drawHDGPath(hdgPath);
}

// Get TWA path from server
function getTWAPath(event) {
    if (!Router.twaAnchor || !vrData || !vrData._id) return;
    
    let twaAnchor = Router.twaAnchor;
    let time = twaAnchor.time;
    let slat = twaAnchor.getLatLng().lat;
    let slon = twaAnchor.getLatLng().lng;
    let dlat = event.latlng.lat;
    let dlon = event.latlng.lng;

    Util.doGET(
        "/function/router.getTWAPath",
        function(request) {
            let data = JSON.parse(request.responseText);
            updateTWADisplay(data.twa, data.heading);
            Router.drawTWAPath(data.twapath);
            Router.drawHDGPath(data.hdgpath);
        },
        function(request) {
            console.error(request.responseText);
        },
        {
            "presets": "VR",
            "gfsMode": "06h",
            "raceId": vrData._id.race_id + '.' + vrData._id.leg_num,
            "cycle": Router.getCurrentCycle(),
            "resolution": Router.settings.resolution,
            "options": Router.settings.options,
            "time": time,
            "latA": slat,
            "lngA": slon,
            "lat": dlat,
            "lng": dlon
        }
    );
}

////////////////////////////////////////////////////////////////////////////////
/// EOF
