////////////////////////////////////////////////////////////////////////////////
/// BitSailor Router VR UI (Leaflet/OpenStreetMap version)

import * as Util from './Util.js';
import * as GPX from './GPX.js';
import GribCache from './GribCache.js';
import PolarManager from './PolarManager.js';


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



// Replace global variables
var sailNames = ["Jib", "Spi", "Stay", "LJ", "C0", "HG", "LG"];
var settings = {
    "resolution": "1p00",
    "gfsMode": "06h",
    "presets": "VR",
    "options": ["hull", "winch", "foil", "heavy", "light", "reach"],
    "polarsId": "1"
};

// Add a PolarManager instance
var polarManager = null;
let selectedCursor = 'crosshair';
var gribCache = undefined;

var mapEvent;

// Bounds and width from the map are kept here for convenience
var geometry = {};

// Time index
var ir_index;

var currentCycle = undefined;

var twaAnchor = undefined;

var courseGCLine = null;

var routeIsochrones = [];

var routeInfo = {};
var twaPath = [];
var hdgPath = [];

// Main initialization function
function initializeRouterVR() {

    currentCycle = getCurrentCycle();
    // Initialize map first
    initMap();
    
    // Set up event handlers
    setupEventHandlers();
    
    // Then load data (which will add markers once map is ready)
    loadPolarsAndRaceInfo();
}


// Set up UI controls and event handlers
function setupEventHandlers() {
    // Map event handlers
    map.on('click', onMapClick);
    map.on('click', computePath);
    map.on('dblclick', getTWAPath);
    
    // Button handlers
    document.getElementById('bt_setstart').onclick = function() {
        setRoutePoint('start', map.getCenter());
    };
    document.getElementById('bt_setdest').onclick = function() {
        setRoutePoint('dest', map.getCenter());
    };
    
    // Checkbox handlers
    const options = ["hull", "winch", "foil", "heavy", "light", "reach"];
    options.forEach(option => {
        const checkbox = document.getElementById(`cb_${option}`);
        if (checkbox) {
            checkbox.checked = settings.options.includes(option);
            checkbox.addEventListener("click", onOptionToggled);
        }
    });
}

// Load polars and race info
function loadPolarsAndRaceInfo() {
    // Set up Router
    setUp(getVMG);
    settings.presets = "VR";
    
    // Load options from URL if available
    const queryParams = getURLParams();
    if (queryParams.options) {
        try {
            settings.options = JSON.parse(queryParams.options);
        } catch (e) {
            console.warn(e);
            settings.options = ["hull", "winch", "foil", "heavy", "light", "reach"];
        }
    }
    
    // Get polars list
    getPolarsList();
    
}

// Get polars list
function getPolarsList() {
    Util.doGET(
        "/function/router.getPolarsList",
        function(xhr) {
            polarsList = JSON.parse(xhr.responseText);
            let selPolars = document.getElementById('sel_polars');
            polarsList.map(entry => {
                let option = document.createElement('option');
                option.value = entry.id;
                option.innerHTML = entry.label;
                selPolars.add(option);
            });
        },
        function(xhr) {
            console.error('Could not load polars list');
        }
    );
}

// Create start and destination markers
function createStartAndDestinationMarkers() {
    const queryParams = getURLParams();
    
    // Start marker
    let start = queryParams.startPos;
    if (!start) {
        start = getValue('start');
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
    }).addTo(map);
    
    // Start marker drag event
    startMarker.on('dragend', function(e) {
        setRoutePoint('start', startMarker.getLatLng());
    });
    
    // Update position display
    setRoutePoint('start', L.latLng(start.lat, start.lon));
    let textBox = document.getElementById("tb_position");
    if (textBox) {
        textBox.value = Util.formatPosition(start.lat, start.lon);
    }
    
    // Handle start time if specified
    if (queryParams.startTime) {
        let cbDelayed = document.getElementById('cb_startdelayed');
        let tbStartTime = document.getElementById('tb_starttime');
        if (cbDelayed && tbDelayed) {
            cbDelayed.checked = true;
            tbStartTime.value = queryParams.startTime.toISOString().substring(0,16);
        }
    }

    // Destination marker
    let dest = getValue('dest');
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
    }).addTo(map);
    
    // Destination marker drag event
    destinationMarker.on('dragend', function(e) {
        setRoutePoint('dest', destinationMarker.getLatLng());
    });
    
    // Update router destination
    setRoutePoint('dest', L.latLng(dest.lat, dest.lon));
    
    // Draw route line between markers
    drawRouteLine();
    
    // Center map on start position
    map.panTo([start.lat, start.lon]);
    
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
        }).addTo(map);
        
        mark.on('click', function() { 
            onMarkerClicked(mark);
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
        }).addTo(map);
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

// Draw route line between start and destination
function drawRouteLine() {
    if (!startMarker || !destinationMarker) return;
    
    if (routeLine) {
        map.removeLayer(routeLine);
    }
    
    routeLine = L.polyline([
        startMarker.getLatLng(),
        destinationMarker.getLatLng()
    ], {
        color: '#d00000',
        weight: 2,
        opacity: 1.0
    }).addTo(map);
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
    settings.options = settings.options.filter(e => e !== event.currentTarget.name);
    if (event.currentTarget.checked) {
        settings.options.unshift(event.currentTarget.name);
    }
    storeValue('options', JSON.stringify(settings.options));
}

// VMG calculation
function getVMG(windSpeed) {
    return polarManager.getVMG(windSpeed, settings.options);
}

// Compute path for constant TWA
async function computePath(event) {
    if (!twaAnchor) return;
    
    let twaAnchor = twaAnchor;

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
    let wind = await gribCache.getWindVR(slat, slon, startTime);
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
    let options = settings.options;
    let polarsData = getPolars();
    
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
        let windTWA = await gribCache.getWindVR(newTWAPos.lat, newTWAPos.lon, stepTime);
        let windHDG = await gribCache.getWindVR(newHDGPos.lat, newHDGPos.lon, stepTime);

        let twaHeading = Util.toHeading(twa, windTWA.direction);
        let hdgTWA = Util.toTWA(heading, windHDG.direction);

        // Use the PolarManager instead of direct function calls
        let speedTWA = polarManager.boatSpeed(
            Util.ms2knots(windTWA.speed), 
            twa, 
            options, 
            polarsData.data_json
        ).speed;
        
        let speedHDG = polarManager.boatSpeed(
            Util.ms2knots(windHDG.speed), 
            hdgTWA, 
            options, 
            polarsData.data_json
        ).speed;

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
    
    drawTWAPath(twaPath);
    drawHDGPath(hdgPath);
}

// Get TWA path from server
function getTWAPath(event) {
    if (!twaAnchor || !vrData || !vrData._id) return;
    
    let twaAnchor = twaAnchor;
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
            drawTWAPath(data.twapath);
            drawHDGPath(data.hdgpath);
        },
        function(request) {
            console.error(request.responseText);
        },
        {
            "presets": "VR",
            "gfsMode": "06h",
            "raceId": vrData._id.race_id + '.' + vrData._id.leg_num,
            "cycle": getCurrentCycle(),
            "resolution": settings.resolution,
            "options": settings.options,
            "time": time,
            "latA": slat,
            "lngA": slon,
            "lat": dlat,
            "lng": dlon
        }
    );


}
////////////////////////////////////////////////////////////////////////////////
/// Bitsailor Router UI Stateless (Leaflet/OpenStreetMap version)


function setUp(getVMG) {
    // Handle window resize
    window.addEventListener('resize', onWindowResize);

    // Map events
    map.on('zoomend moveend', updateMap);
    map.on('contextmenu', onMapRightClick);
    map.on('mousemove', async function (event) {
        return await updateWindInfo(event, getVMG);
    });
    map.on('click', drawOrthodromic);

    // Connect button events
    document.getElementById("bt_getroute").addEventListener("click",getRoute);
    document.getElementById("bt_downloadroute").addEventListener("click",onDownloadRoute);

    document.getElementById("bt_inc").addEventListener("click",onAdjustIndex);
    document.getElementById("bt_dec").addEventListener("click",onAdjustIndex);
    document.getElementById("bt_inc6").addEventListener("click",onAdjustIndex);
    document.getElementById("bt_dec6").addEventListener("click",onAdjustIndex);
    document.getElementById("ir_index").addEventListener("change",onAdjustIndex);

    document.getElementById("cb_startdelayed").addEventListener("click",onDelayedStart);

    // Connect option selectors
    document.getElementById("sel_duration").addEventListener("change",onSetDuration);
    document.getElementById("cb_displaywind").addEventListener("change",onDisplaywind);
    document.getElementById("cb_displaytracks").addEventListener("change",onDisplayTracks);


    // Disable default contextmenu
    window.oncontextmenu = function (event) { event.preventDefault(); };

    // Connect menu events
    document.getElementById("bt_setstart").addEventListener("click",function () { onContextMenu('start'); });
    document.getElementById("bt_setdest").addEventListener("click",function () { onContextMenu('dest'); });
    document.getElementById("bt_copypos").addEventListener("click",onCopyPosition);

    // Set up other UI elements
    document.getElementById("sel_resolution").addEventListener("change", onSetResolution);
    document.getElementById("sel_polars").addEventListener("change", onSetPolars);
    
    var mapMenu = document.getElementById("mapMenu");
    mapMenu.onmouseleave = onMapMenuMouseLeave;

    ir_index = document.getElementById("ir_index");

    startMarker = initMarker('start', 'Start', 'img/start_45x32.png', 0, 45, 16, -10);
    destinationMarker = initMarker('dest', 'Destination', 'img/finish_32x20.png', 0, 32, 10, -10);

    // Wind canvas
    let canvas = setupCanvas();

    document.getElementById("tb_position").addEventListener("keyup", function (event) {
        if (event.keyCode === 13) {
            onSetStartPosition(event);
        }
    });
    document.getElementById("bt_position").addEventListener("click", onSetStartPosition);
    document.getElementById("bt_setstart").addEventListener("click", onContextMenuSetStart);

    startMarker.on('dragend', function (e) {
        onUpdateStartMarker(startMarker);
    });
}

function initMap() {
    if (!map) {
        map = L.map('map', {
            center: [49.187, 8.473],
            zoom: 5,
            minZoom: 3,
            maxZoom: 14,
            doubleClickZoom: false,
            dragging: true,
            zoomControl: true,
        });

        L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
            attribution: '&copy; OpenStreetMap contributors'
        }).addTo(map);
    }
    return map;
}

function initMarker(type, title, url, iconX=0, iconY=0, popupX=0, popupY=0) {
    var marker = L.marker([0, 0], {
        title: title,
        icon: L.icon({
            iconUrl: url,
            iconAnchor: [iconX, iconY],
            popupAnchor: [popupX, popupY],
        }),
        draggable: true
    }).addTo(map);

    marker.on('click', function () { onMarkerClicked(marker); });

    marker.on('dragend', function (e) {
        setRoutePoint(type, marker.getLatLng());
    });

    return marker;
}

////////////////////////////////////////////////////////////////////////////////
/// Event handlers

function onWindowResize(event) {
    map.invalidateSize();
}

function onDownloadRoute(event) {
    if (routeInfo && routeInfo.best) {
        let rbGPX = document.getElementById('rb_gpx');
        let format = rbGPX.checked ? 'gpx' : 'csv';
        let content = GPX.exportRoute(routeInfo, format, sailNames);
        let file = new Blob([content], { type: 'text/plain' });
        let link = document.createElement("a");
        link.href = URL.createObjectURL(file);
        link.download = `route.${format}`;
        link.click();
        URL.revokeObjectURL(link.href);
    } else {
        alert('No route information');
    }
}

function updateMap() {
    var bounds = getMapBounds();

    // Load wind
    if (!gribCache) {
        let canvas = document.getElementById('wind-canvas');
        gribCache = new GribCache(canvas, bounds || { "north": 50, "south": 40, "west": 0, "east": 10 }, settings.resolution, new Date());
    }

    redrawWindByOffset(ir_index.value);
}

function availableForecastCycle(d = new Date()) {
    var availDate = d - 300 * 60 * 1000;
    var fc = truncate(availDate, 6 * 3600 * 1000);
    return new Date(fc).toISOString();
}

function getCurrentCycle(d = new Date()) {
    var availDate = d - 210 * 60 * 1000;
    var fc = truncate(availDate, 6 * 3600 * 1000);
    return new Date(fc).toISOString();
}

function onContextMenu(point) {
    var mapMenu = document.getElementById("mapMenu");
    mapMenu.style.display = "none";
    setRoutePoint(point, mapEvent.latlng);
}

function onCopyPosition(event) {
    let label = document.getElementById('lb_position');
    let text = label.__pos.lat + ' ' + label.__pos.lng;
    navigator.clipboard.writeText(text);
}

function onSelectIsochrone(isochrone) {
    currentCycle = isochrone.time;
    var offset = isochrone.offset;
    document.getElementById("ir_index").value = offset;
    updateIsochrones();
    redrawWindByOffset(offset);
}

function onDisplaywind(event) {
    var cbDisplaywind = document.getElementById("cb_displaywind");
    if (cbDisplaywind.checked) {
        document.getElementById("wind-canvas").style.display = "block";
    } else {
        document.getElementById("wind-canvas").style.display = "none";
    }
}

function onDisplayTracks(event) {
    clearRoute();
    displayRouting(routeInfo);
}

function onCursorSelect(event, type) {
    selectedCursor = type;
    map.getContainer().style.cursor = type;
}

function setBusyCursor() {
    document.getElementById('bt_getroute').style.cursor = "wait";
    document.getElementById("body").style.cursor = "wait";
    map.getContainer().style.cursor = "wait";
}

function restoreCursor() {
    document.getElementById("body").style.cursor = "pointer";
    document.getElementById('bt_getroute').style.cursor = "pointer";
    map.getContainer().style.cursor = selectedCursor;
}

function onAdjustIndex(event) {
    var source = event.target.id;
    if (source == "bt_dec6")
        ir_index.valueAsNumber = ir_index.valueAsNumber - 6;
    else if (source == "bt_dec")
        ir_index.valueAsNumber = ir_index.valueAsNumber - 1;
    else if (source == "bt_inc")
        ir_index.valueAsNumber = ir_index.valueAsNumber + 1;
    else if (source == "bt_inc6")
        ir_index.valueAsNumber = ir_index.valueAsNumber + 6;
    updateIsochrones();
    redrawWindByOffset(ir_index.value);
}

function onDelayedStart(event) {
    var dateInput = document.getElementById("tb_starttime");
    if (event.target.checked === true) {
        var d = new Date();
        var isoDate = d.toISOString().substring(0, 16);
        dateInput.value = isoDate;
    } else {
        dateInput.value = null;
    }
}

function getStartTime() {
    var cbDelayed = document.getElementById("cb_startdelayed");
    if (cbDelayed.checked === true) {
        var dateInput = document.getElementById("tb_starttime");
        return dateInput.value;
    } else {
        var d = new Date();
        return d.toISOString().substring(0, 16);
    }
}

function onContextMenuSetStart(event) {
    var textBox = document.getElementById("tb_position");
    var position = mapEvent.latlng;
    textBox.value = Util.formatPosition(position.lat, position.lng);
}

function onUpdateStartMarker(marker) {
    var textBox = document.getElementById("tb_position");
    var position = marker.getLatLng();
    textBox.value = Util.formatPosition(position.lat, position.lng);
}

function onSetStartPosition(event) {
    var position = document.getElementById("tb_position").value;
    var latlon = parsePosition(position);
    if (latlon) {
        setRoutePoint('start', L.latLng(latlon.lat, latlon.lon));
    }
}

function onSetResolution(event) {
    let resolution = event.currentTarget.value;
    settings.resolution = resolution;
    storeValue('resolution', resolution);
    redrawWindByOffset(ir_index.value);
}

function onSetPolars(event) {
    let polarsId = event.currentTarget.value;
    polarManager = new PolarManager(polarsId);
}

function onSetDuration(event) {
    let duration = event.target.value;
    settings.duration = duration * 3600;
    storeValue('duration', duration);
}

function storeValue(name, value) {
    try {
        let storage = window.localStorage;
        let query = new URL(document.URL).searchParams;
        let raceId = query.get('race');
        storage.setItem(`${raceId}.${name}`, value);
    } catch (e) {
    }
}

function getValue(name) {
    let storage = window.localStorage;
    let query = new URL(document.URL).searchParams;
    let raceId = query.get('race');
    return storage.getItem(`${raceId}.${name}`);
}

function parsePosition(string) {
    try {
        string = string.replace(`/`, `,`);
        var parts = string.split(',');
        if (parts.length != 2) {
            parts = string.split(' ');
        }
        if (parts.length != 2) {
            throw new Error(`Invalid LatLng ${string}`);
        }
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
        };
    } catch (e) {
        alert(e);
    }
}

function parseDMS(string) {
    string = string.replace(`''`, `'`);
    string = string.replace(`º`, `°`);
    var sign = string.match(/W|S/) ? -1 : 1;
    string = string.split(/[NESW]/)[0];
    var parts = string.split('°');
    var degrees = parseFloat(parts[0]);
    if (parts[1]) {
        parts = parts[1].split('\'');
        degrees += parseFloat(parts[0]) / 60;
        if (parts[1]) {
            degrees += parseFloat(parts[1].split('"')[0]) / 3600;
        }
    }
    if (isNaN(degrees)) {
        throw new Error(`Invalid DMS ${string}, valid formats are nnn.nnnnn, nnn°nn.nnnnn', nnn°nn'nn.nnnnn`);
    } else {
        return sign * degrees;
    }
}

function truncate(n, q) {
    return n - (n % q);
}

function pad0(val, length = 2, base = 10) {
    var result = val.toString(base);
    while (result.length < length) result = '0' + result;
    return result;
}

function getManualCycle() {
    var dateInput = document.getElementById("tb_cycledate");
    var hourInput = document.getElementById("sel_cyclehour");
    return dateInput.value + "T" + pad0(hourInput.value) + ":00:00Z";
}


function onMapMenuMouseLeave(event) {
    var mapMenu = document.getElementById("mapMenu");
    mapMenu.style.display = "none";
}

function onMapRightClick(event) {
    mapEvent = event;
    let mapMenu = document.getElementById("mapMenu");
    mapMenu.style.display = "block";
    mapMenu.style["z-index"] = 400;
    mapMenu.style.top = event.originalEvent.pageY + "px";
    mapMenu.style.left = event.originalEvent.pageX + "px";
    return false;
}

function onMarkerClicked(marker) {
    twaAnchor = marker;

    var time = marker.time;
    var isochrone = getIsochroneByTime(time);
    var baseTime;

    if (isochrone) {
        baseTime = isochrone.time;
    } else {
        baseTime = availableForecastCycle();
    }

    redrawWindByTime(new Date(time));
}

//////////////////////////////////////////////////////////////////////
/// Accessors

function setResolution(resolution) {
    settings.resolution = resolution;
    document.getElementById("sel_resolution").value = resolution;
}

function setDuration(duration) {
    settings.duration = duration * 3600;
    document.getElementById('sel_duration').value = duration;
}

function setSailnames(sailnames) {
    polarManager.setSailNames(sailnames);
    sailNames = sailnames; // Keep for compatibility
}

function getSailnames() {
    return polarManager.getSailNames();
}

//////////////////////////////////////////////////////////////////////
/// XHR requests

function makeQuery(object) {
    var s = "";
    for (const m in object) {
        if (object[m]) {
            if (s == "") {
                s = "?";
            } else {
                s += "&";
            }
            s += `${m}=${encodeURIComponent(object[m])}`;
        }
    }
    return s;
}

function getRoute() {
    setBusyCursor();
    var bt_execute = document.getElementById("bt_getroute");
    bt_execute.disabled = true;

    let documentQuery = new URL(document.URL).searchParams;
    let raceId = documentQuery.get('race');

    var mapMenu = document.getElementById("mapMenu");
    mapMenu.style.display = "none";

    var pgGetRoute = document.getElementById("pg_getroute");
    pgGetRoute.value = 5;

    var selDuration = document.getElementById('sel_duration');
    var duration = selDuration.value || 48;
    var timer = window.setInterval(updateGetRouteProgress, 10 * duration);

    var startTime = getStartTime();
    var startPos = startMarker.getLatLng();
    var destPos = destinationMarker.getLatLng();

    var query = makeQuery(settings);
    query += `&raceId=${raceId}`;
    query += `&startTime=${startTime}`;
    query += `&slat=${startPos.lat}&slon=${startPos.lng}&dlat=${destPos.lat}&dlon=${destPos.lng}`;

    var tackInput = document.getElementById('sel_currenttack');
    if (tackInput) {
        query += `&tack=${tackInput.value}`;
    }

    var sailInput = document.getElementById('sel_currentsail');
    if (sailInput) {
        query += `&sail=${sailInput.value}`;
    }

    Util.doGET(
        "/function/router.getRoute" + query,
        function (data) {
            routeInfo = JSON.parse(data.response);
            window.clearInterval(timer);
            pgGetRoute.value = pgGetRoute.max;
            clearRoute();
            displayRouting(routeInfo);
            bt_execute.disabled = false;
            restoreCursor();
        },
        function (jqXHR, textStatus, errorThrown) {
            bt_execute.disabled = false;
            restoreCursor();
            window.clearInterval(timer);
            pgGetRoute.value = pgGetRoute.max;
            alert(textStatus + ' ' + errorThrown);
        });
}



// Set route point (start or destination)
function setRoutePoint(type, latlng) {
    if (type === 'start' && startMarker) {
        startMarker.setLatLng(latlng);
        setRoutePointCore('start', latlng);
    } else if (type === 'dest' && destinationMarker) {
        destinationMarker.setLatLng(latlng);
        setRoutePointCore('dest', latlng);
    }
    drawRouteLine();
}


function setRoutePointCore(point, latLng) {
    storeValue(point, `{"lat":${latLng.lat}, "lon": ${latLng.lng}}`);

    if (point === 'start') {
        startMarker.setLatLng(latLng);
    } else if (point === 'dest') {
        destinationMarker.setLatLng(latLng);
    }

    if (courseGCLine) {
        map.removeLayer(courseGCLine);
    }
    courseGCLine = L.polyline([startMarker.getLatLng(), destinationMarker.getLatLng()], {
        color: '#d00000',
        weight: 1,
        opacity: 1.0
    }).addTo(map);
}

function displayRouting(data) {
    var best = data.best;

    createIsochrones(data.isochrones);

    startMarker.time = best[0].time;

    var markerIcon = L.icon({ iconUrl: "img/marker_32x12.png", iconAnchor: [6, 32], tooltipAnchor: [0, -36],  popupAnchor: [0, -36]});
    var redMarkerIcon = L.icon({ iconUrl: "img/marker_red_32x12.png", iconAnchor: [6, 32], tooltipAnchor: [0, -36], popupAnchor: [0, -36]});

    var isDisplayTracks = document.getElementById('cb_displaytracks').checked;
    if (isDisplayTracks) {
        var tracks = data.tracks;
        for (var i = 0; i < tracks.length; i++) {
            var track = L.polyline(tracks[i].map(p => [p.lat, p.lng]), {
                color: '#d00000',
                weight: 1.5,
                opacity: 1.0
            }).addTo(map);
            routeTracks[i] = track;
        }
    }

    var bestPath = best.map(entry => [entry.position.lat, entry.position.lng]);
    var bestTrack = L.polyline(bestPath, {
        color: '#d00000',
        weight: 3,
        opacity: 1.0
    }).addTo(map);
    routeTracks[routeTracks.length] = bestTrack;

    for (var i = 0; i < best.length; i++) {
        if (i == 0
            || best[i].penalty
            || best[i].twa != best[i - 1].twa
            || best[i].sail != best[i - 1].sail) {
            var trackMarker = L.marker([best[i].position.lat, best[i].position.lng], {
                icon: ((best[i].penalty === "sailChange") || (best[i].penalty === "tack") || (best[i].penalty === "gybe")) ? redMarkerIcon : markerIcon,
                draggable: false
            }).addTo(map);
            addMarkerListener(trackMarker);
            addWaypointInfo(trackMarker, new Date(best[0].time), best[i]);
            trackMarkers.unshift(trackMarker);
        }
    }

    document.getElementById("lb_from").textContent = (data.stats.start);
    document.getElementById("lb_duration").textContent = (data.stats.duration);
    document.getElementById("lb_sails").textContent = (formatSails(data));
    document.getElementById("lb_minwind").textContent = (data.stats["min-wind"].toFixed(1) + " - " + data.stats["max-wind"].toFixed(1));
    document.getElementById("lb_mintwa").textContent = (data.stats["min-twa"] + " - " + data.stats["max-twa"]);
    document.getElementById("lb_polars").textContent = (data.polars);
    document.getElementById("lb_options").textContent = (data.options);
}

function getURLParams() {
    let query = new URL(document.URL).searchParams;
    let slat = query.get('slat');
    let slon = query.get('slon');
    let starttime = query.get('starttime');
    let res = {};
    if (slat) {
        res.startPos = {
            "lat": Number(slat),
            "lon": Number(slon)
        };
    }
    if (starttime) {
        res.startTime = new Date(starttime + 'Z');
    }

    res.options = query.get('options') || '';

    var sail = query.get('sail');
    if (sail) {
        res.sail = sail;
    }

    var twa = query.get('twa');
    if (twa) {
        res.twa = twa;
    }

    return res;
}

function setupCanvas() {
    var mapCanvas = document.getElementById('wind-canvas');
    var mapRect = mapCanvas.getBoundingClientRect();
    geometry.width = mapRect.width * 6;
    geometry.height = mapRect.height * 6;
    mapCanvas.width = geometry.width;
    mapCanvas.height = geometry.height;
    return mapCanvas;
}

function createIsochrones(isochrones) {
    var c = new Date(currentCycle);
    for (var i = 0; i < isochrones.length; i++) {
        var style = getIsoStyle(isochrones[i], 1, c);
        var isochrone = L.polyline(isochrones[i].path.map(p => [p.lat, p.lng]), {
            color: style.color,
            weight: style.weight,
            opacity: 0.8
        }).addTo(map);
        addInfo(isochrone, isochrones[i].time, isochrones[i].offset);
        routeIsochrones[i] = isochrone;
    }
}

function getIsochroneTime(isochrone) {
    var basetime = new Date(isochrone.time);
    var offset = isochrone.offset;
    return new Date(basetime - 0 + offset * 3600 * 1000);
}

function getIsoStyle(isochrone, selectedOffset, availableCycle) {
    var c = availableCycle;
    var d = new Date(isochrone.time);
    var i = getIsochroneTime(isochrone);
    var h = i.getUTCHours();
    var weight = (h % 6) ? 1 : 3;
    var color;
    if (isochrone.offset === selectedOffset) {
        color = '#ffffff';
        weight = 3;
    } else {
        if (d < c) {
            color = (h % 12) ? '#D0a0b0' : '#D080a0';
        } else {
            color = (h % 12) ? '#8080a0' : '#000000';
        }
    }
    return { "color": color, "weight": weight };
}

function updateIsochrones() {
    var c = new Date(currentCycle);
    for (const isochrone of routeIsochrones) {
        var style = getIsoStyle(isochrone, ir_index.valueAsNumber, c);
        isochrone.setStyle({ color: style.color, weight: style.weight });
    }
}

function clearRoute() {
    clearPath(twaPath);
    clearPath(hdgPath);

    for (var i = 0; i < trackMarkers.length; i++) {
        map.removeLayer(trackMarkers[i]);
    }
    trackMarkers = [];
    for (var i = 0; i < routeTracks.length; i++) {
        map.removeLayer(routeTracks[i]);
    }
    routeTracks = [];
    for (var i = 0; i < routeIsochrones.length; i++) {
        map.removeLayer(routeIsochrones[i]);
    }
    routeIsochrones = [];
}

function updateGetRouteProgress() {
    var pgGetRoute = document.getElementById("pg_getroute");
    if (pgGetRoute.value < pgGetRoute.max) {
        pgGetRoute.value = pgGetRoute.value + 10;
    }
}

function addWaypointInfo(trackMarker, startTime, point) {
    var popupContent = makeWaypointInfo(startTime, point);
    // trackMarker.bindPopup(popupContent);
    trackMarker.bindTooltip(popupContent, {"permanent": false});
    trackMarker.time = point.time;
}

function makeWaypointInfo(startTime, point) {
    var time = new Date(point.time);
    var elapsed = Util.formatDHM((time - startTime) / 1000);
    var result = "<div style='color:#000;'>";
    result += "<b>T+" + elapsed + " - " + point.time + "</b><br>";

    result += "<hr>";

    result += "<b>Pos</b>: " + formatPointPosition(point.position);
    result += "<br>";

    result += "<b>DTF</b>:" + Util.m2nm(point.dtf).toFixed(2) + "nm ";
    result += "<b>Speed</b>: " + Util.ms2knots(point.speed).toFixed(1) + "kts";
    result += "<br>";

    result += "<b> TWA</b>: " + point.twa.toFixed(1);
    result += "<b> HDG</b>: " + point.heading.toFixed(1) + "°  " + sailNames[point.sail];
    result += "<br>";

    result += "<hr>";
    result += "<b>Wind</b>: " + Util.ms2knots(point.tws).toFixed(2) + "kts / " + point.twd.toFixed(0) + "°<br>";


    result += "</div>";

    return result;
}

function addMarkerListener(marker) {
    marker.on('click', function () { onMarkerClicked(marker); });
}

function clearPath(path) {
    for (var i = 0; i < path.length; i++) {
        map.removeLayer(path[i]);
    }
    path = [];
}

var ortho;
function drawOrthodromic(event) {
    if (ortho) {
        map.removeLayer(ortho);
    }
    var start;
    if (twaAnchor) {
        start = twaAnchor.getLatLng();
    } else {
        start = startMarker.getLatLng();
    }
    ortho = L.polyline([start, event.latlng], {
        color: '#1f1f1f',
        weight: 1,
        opacity: 1
    }).addTo(map);
}

function drawTWAPath(data) {
    clearPath(twaPath);
    drawPath(twaPath, data, '#60B260');
}

function drawHDGPath(data) {
    clearPath(hdgPath);
    drawPath(hdgPath, data, '#00c2f8');
}

function drawPath(bPath, data, color) {
    for (var i = 1; i < data.length; i++) {
        var d = new Date(data[i][0]);
        let minutes = d.getMinutes();
        var segment = L.polyline([data[i - 1][1], data[i][1]], {
            color: color,
            weight: (minutes == 0) ? 4 : 2,
            opacity: 1.0
        }).addTo(map);
        bPath[i - 1] = segment;
    }
}

function addInfo(isochrone, time, offset) {
    isochrone.time = time;
    isochrone.offset = offset;
    isochrone.on('click', function () {
        var iso = isochrone;
        onSelectIsochrone(iso);
    });
}

function isochroneTime(isochrone) {
    var millis = new Date(isochrone.time).getTime();
    var date = new Date(millis + isochrone.offset * 3600 * 1000);
    return date;
}

function getIsochroneByTime(time) {
    var refTime = new Date(time);
    var isochrones = routeInfo.isochrones;
    if (isochrones) {
        isochrones = isochrones.slice().reverse();
    }
    for (const iso of isochrones) {
        var isoT = isochroneTime(iso);
        if (isoT >= refTime) {
            return iso;
        }
    }
    return null;
}

function redrawWindByTime(time) {
    getWind(currentCycle, time);
}

function getOffsetTime(offset, cycle = currentCycle) {
    let d = new Date(cycle);
    let time = d.getTime();
    time = time + parseInt(offset) * 3600000;
    return new Date(time);
}

function redrawWindByOffset(offset) {
    redrawWindByTime(getOffsetTime(offset));
}

async function getWind(cycle, time) {
    let bounds = getMapBounds();

    let usedCycle = new Date(cycle);

    try {
        await gribCache.update(bounds, usedCycle, time, settings.resolution);
    } catch (e1) {
        console.log(e1);
        try {
            usedCycle = availableForecastCycle();
            gribCache.update(bounds, usedCycle, time, settings.resolution);
        } catch (e2) {
            console.log(e2);
        }
    }
    document.getElementById('lb_modelrun').innerHTML = usedCycle.toISOString().substring(11, 13) + 'Z';
    document.getElementById('lb_index').innerHTML = time.toISOString().substring(0, 16) + 'Z';

    var offset = (new Date(time) - new Date(usedCycle)) / 3600000;
    ir_index.value = offset;
}

async function updateWindInfo(event, getVMG) {
    let label = document.getElementById('lb_position');
    label.textContent = '[ ' + formatLatLngPosition(event.latlng) + ' ]';
    label.__pos = event.latlng;

    if (gribCache) {
        var zoom = map.getZoom();

        var bounds = getMapBounds();

        var curLat = event.latlng.lat;
        var curLng = event.latlng.lng;

        var destLat = destinationMarker.getLatLng().lat;
        var destLng = destinationMarker.getLatLng().lng;

        var dtf = Util.gcDistance({ "lat": curLat, "lon": curLng },
                                  { "lat": destLat, "lon": destLng });

        var destBearing = Util.toDeg(Util.courseAngle(curLat, curLng, destLat, destLng));

        var lbDTF = document.getElementById("lb_dtf");
        lbDTF.innerText = `${dtf.toFixed(1)} | ${destBearing.toFixed(1)}°`;

        var startLat = startMarker.getLatLng().lat;
        var startLng = startMarker.getLatLng().lng;

        var dfs = Util.gcDistance({ "lat": curLat, "lon": curLng },
                                  { "lat": startLat, "lon": startLng });

        var startBearing = Util.toDeg(Util.courseAngle(curLat, curLng, startLat, startLng));

        var lbDFS = document.getElementById("lb_dfs");
        lbDFS.innerText = `${dfs.toFixed(1)} | ${startBearing.toFixed(1)}°`;

        var wind = await gribCache.getWind(curLat, curLng);
        if (wind) {
            var windDir = wind.direction.toFixed();
            var windSpeed = Util.ms2knots(wind.speed).toFixed(1);
            document.getElementById("lb_windatposition").textContent = (pad0(windDir, 3) + "° | " + windSpeed + "kn");

            var lbVMGUp = document.getElementById("lb_vmg_up");
            var lbVMGDown = document.getElementById("lb_vmg_down");
            var vmg = getVMG(windSpeed);

            lbVMGUp.innerHTML = vmg.up;
            lbVMGDown.innerHTML = vmg.down;

        } else {
            console.log(`No wind data at ${curLat}, ${curLng}`);
        }
    } else {
        console.log(`No wind data loaded`);
    }
}

function getMapBounds() {
    var bounds = map.getBounds();
    var sw = bounds.getSouthWest();
    var ne = bounds.getNorthEast();

    return {
        northEast: ne,
        southWest: sw,
        north: ne.lat,
        south: sw.lat,
        west: sw.lng,
        east: ne.lng,
        map: map
    };
}

////////////////////////////////////////////////////////////////////////////////
/// Formatting

function formatLatLngPosition(latlng) {
    return Util.formatPosition(latlng.lat, latlng.lng);
}

function formatPointPosition(point) {
    return Util.formatPosition(point.lat, point.lng);
}

function formatSails(data) {
    var best = data.best;
    var t0 = new Date(best[0].time);
    var start = t0;
    var t1;
    var sail = best[0].sail;
    var m = {};
    for (const s of best) {
        t1 = new Date(s.time);
        if (s.sail != sail) {
            var dt = t1 - t0;
            if (!m[sail]) {
                m[sail] = dt;
            } else {
                m[sail] += dt;
            }
            t0 = t1;
            sail = s.sail;
        }
    }

    var dt = t1 - t0;
    if (!m[sail]) {
        m[sail] = dt;
    } else {
        m[sail] += dt;
    }

    dt = t1 - start;

    var result = "";
    for (const e in m) {
        let sail = sailNames[e];
        if (result) {
            result = result + " - " + sail + ":" + (m[e] / dt * 100).toFixed() + "%";
        } else {
            result = sail + ":" + (m[e] / dt * 100).toFixed() + "%";
        }
    }
    return result;
}

// Initialize everything when the page loads
window.addEventListener("load", function() {
    initializeRouterVR();
});
