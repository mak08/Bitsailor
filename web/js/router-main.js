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
let courseGCLine = null;

let trackMarkers = [];
let routeTracks = [];
let routeIsochrones = [];
let twaPath = [];
let hdgPath = [];
let polarsList = [];
let races = [];

let currentRaceName = null;
let curTWA = null;
let orthodromicLine = null;
let twaAnchor = null;
let mapEvent = null;
let ir_index = null;
let currentCycle = null;
let gribCache = null;

let sailNames = ["Jib", "Spi", "Stay", "LJ", "C0", "HG", "LG"];
let settings = {
    "resolution": "0p25",
    "duration": 345600,
    "polarsId": undefined
};

let polarManager = new PolarManager();
let selectedCursor = 'crosshair';
let geometry = {};
let routeInfo = {};

let courseGateLayers = []; // layers created by GPX import (markers + polylines)

// Main initialization function
function setupPage() {
    currentCycle = getCurrentCycle();
    // Initialize map first
    initMap();
    setUp(getVMG);
    getPolarsList();
    setupEventHandlers();
    setupRaces();

    // Ensure ir_index is set up before calling updateMap
    // (setUp assigns ir_index = document.getElementById("ir_index");)
    updateMap(); 
}

// Hook dropdown
function setupRaces () {
  loadRaces();
  const sel = document.getElementById('sel_race');
  if (sel) sel.addEventListener('change', () => {
    currentRaceName = sel.value || null;
    const r = races.find(x => x.name === currentRaceName);
    applyRaceSettings(r);
  });
}

// Set up UI controls and event handlers
function setupEventHandlers() {
    // Map event handlers
    map.on('click', onMapClick);
    map.on('click', computePath);
    
    // Checkbox handlers
    const options = ["hull", "winch", "foil", "heavy", "light", "reach"];
    options.forEach(option => {
        const checkbox = document.getElementById(`cb_${option}`);
        if (checkbox) {
            checkbox.checked = settings.options.includes(option);
            checkbox.addEventListener("click", onOptionToggled);
        }
    });

    // Race removal button
    const btRemoveRace = document.getElementById('bt_removerace');
    if (btRemoveRace) btRemoveRace.addEventListener('click', removeRace);
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
            settings.polarsId = selPolars.value;
        },
        function(xhr) {
            console.error('Could not load polars list');
        }
    );
}

function loadRaces() {
  try { 
    races = JSON.parse(localStorage.getItem('xx.races') || '[]'); 
  } catch(e) { 
    races = []; 
    console.error('Could not load races from localStorage', e);
}
  fillRaceDropdown();
}

function saveRaces() {
  try { 
    localStorage.setItem('xx.races', JSON.stringify(races)); 
  } catch(e) {
    console.error('Could not save races to localStorage', e);
  }
}

function fillRaceDropdown() {
  const sel = document.getElementById('sel_race');
  if (!sel) return;
  sel.innerHTML = '<option value="">--</option>';
  races.forEach(r => {
    const o = document.createElement('option');
    o.value = r.name; o.textContent = r.name;
    sel.appendChild(o);
  });
  sel.value = currentRaceName || '';
}

function captureRaceSettings(name, value) {
  if (!currentRaceName) return;
  const race = races.find(r => r.name === currentRaceName);
  if (!race) return;
  race.settings[name] = value;
  saveRaces();
}

// Capture a complete snapshot of current UI/routing settings for the active race
function captureAllRaceSettings() {
    if (!currentRaceName) return;
    const race = races.find(r => r.name === currentRaceName);
    if (!race) return;
    const startPos = startMarker ? startMarker.getLatLng() : null;
    const destPos  = destinationMarker ? destinationMarker.getLatLng() : null;
    const nmeaHost = document.getElementById('tb_nmeahost')?.value;
    const nmeaPort = document.getElementById('tb_nmeaport')?.value;
    const polarsSel = document.getElementById('sel_polars');
    const polarsId = settings.polarsId || (polarsSel ? polarsSel.value : undefined);
    const resolution = settings.resolution;
    const duration = settings.duration; // stored in seconds already
    const options = Array.isArray(settings.options) ? settings.options.slice() : (settings.options ? JSON.parse(JSON.stringify(settings.options)) : []);
    // Preserve existing gates if already captured via GPX import
    const gates = race.settings.gates || undefined;

    race.settings = {
        start: startPos ? { lat: startPos.lat, lng: startPos.lng } : undefined,
        dest: destPos ? { lat: destPos.lat, lng: destPos.lng } : undefined,
        nmeaHost,
        nmeaPort,
        polarsId,
        resolution,
        duration,
        options,
        gates
    };
    saveRaces();
}

function applyRaceSettings(race) {
  if (!race || !race.settings) return;
  const s = race.settings.start; if (s) setRoutePointCore('start', L.latLng(s.lat, s.lng));
  const d = race.settings.dest;  if (d) setRoutePointCore('dest', L.latLng(d.lat, d.lng));
  if (race.settings.nmeaHost) { const h = document.getElementById('tb_nmeahost'); if (h) h.value = race.settings.nmeaHost; }
  if (race.settings.nmeaPort) { const p = document.getElementById('tb_nmeaport'); if (p) p.value = race.settings.nmeaPort; }
  if (race.settings.polarsId) { const sp = document.getElementById('sel_polars'); if (sp) { sp.value = race.settings.polarsId; settings.polarsId = race.settings.polarsId; } }
  if (race.settings.resolution) setResolution(race.settings.resolution);
  if (race.settings.duration)   setDuration(race.settings.duration / 3600);
    // Restore persisted gates (clear existing overlays first)
    restoreRaceGates(race);
  drawRouteLine();
}

function addRace(name) {
  if (!name) return;
  let r = races.find(x => x.name === name);
  if (!r) { r = { name, settings:{} }; races.push(r); }
  currentRaceName = name;
    // Capture full snapshot of current settings at creation
    captureAllRaceSettings();
  saveRaces();
  fillRaceDropdown();
}

function removeRace() {
    const idx = races.findIndex(r => r.name === currentRaceName);
    if (idx === -1) {
        // No current race or not found: ask to remove ALL
        if (!races.length) return; // nothing to remove
        const okAll = window.confirm(`No race selected. Remove ALL (${races.length}) races? This cannot be undone.`);
        if (!okAll) return;
        races.length = 0;
        currentRaceName = null;
        saveRaces();
        fillRaceDropdown();
        clearCourseOverlays();
        return;
    }
    // Confirm removal of the selected race only
    const raceName = races[idx].name;
    const ok = window.confirm(`Remove race "${raceName}"? This will delete its stored settings and gates.`);
    if (!ok) return;
    races.splice(idx, 1);
    currentRaceName = null;
    saveRaces();
    fillRaceDropdown();
    clearCourseOverlays();
}

function restoreRaceGates(race) {
    clearCourseOverlays();
    if (!race || !race.settings || !race.settings.gates) return;
    for (const g of race.settings.gates) {
        const color = gateColor(g.key);
        const pts = g.points.map(p => ({ lat: p.lat, lon: p.lon, name: p.name, key: g.key }));
        drawGateGroup(pts, color);
    }
    fitMapToGates(race.settings.gates);
}


// Build shortest longitudinal connection, split at ±180, and triplicate into adjacent worlds
function buildShortestSegments(startLatLng, destLatLng) {
    const sLat = startLatLng.lat, sLon = wrapLon180(startLatLng.lng);
    const dLat = destLatLng.lat, dLonRaw = wrapLon180(destLatLng.lng);

    // Choose destination longitude adjusted by ±360 for minimal |Δlon|
    const candidates = [dLonRaw, dLonRaw + 360, dLonRaw - 360];
    let dLonAdj = candidates.reduce((best, cur) =>
        (Math.abs(cur - sLon) < Math.abs(best - sLon)) ? cur : best, candidates[0]);

    const points = [[sLat, sLon], [dLat, dLonAdj]];
    let segments = splitPathAtAntimeridian(points);
    segments = segmentsWithWorldTriplicate(segments);
    return segments;
}

// Draw route line between start and destination (shortest great-circle segment)
function drawRouteLine() {
    if (!startMarker || !destinationMarker) return;

    if (routeLine) {
        map.removeLayer(routeLine);
        routeLine = null;
    }

    const start = startMarker.getLatLng();
    const dest = destinationMarker.getLatLng();

    const segments = buildShortestSegments(start, dest);

    const fg = L.featureGroup();
    for (const seg of segments) {
        L.polyline(seg, {
            color: '#d00000',
            weight: 2,
            opacity: 1.0
        }).addTo(fg);
    }
    fg.addTo(map);
    routeLine = fg;
}

// VMG calculation
async function getVMG(windSpeed) {
    let selPolars = document.getElementById('sel_polars');
    let analyzer = await polarManager.getAnalyzer(selPolars.value);
    return analyzer.getVMG(windSpeed, settings.options);
}

// Compute path for constant TWA
async function computePath(event) {
    if (!twaAnchor) return;
    
    // Start time
    let startTime = new Date(twaAnchor.time);

    // Start and target position
    let slat = twaAnchor.getLatLng().lat;
    let slon = twaAnchor.getLatLng().lng;
    let dlat = event.latlng.lat;
    let dlon = event.latlng.lng;
    let targetDist = Util.gcDistance({"lat": slat, "lon": slon}, {"lat": dlat, "lon": dlon});
    let pathDist = 0;
    
    // Heading and TWA
    let heading = Util.toDeg(Util.courseAngle(slat, slon, dlat, dlon));
    let wind = await gribCache.getWind(slat, slon, startTime);
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
    
    let newTWAPos = { "lat": slat, "lon": slon };
    let newHDGPos = { "lat": slat, "lon": slon };
    
    let twaPath = [[startTime, {"lat": newTWAPos.lat, "lng": newTWAPos.lon}]];
    let hdgPath = [[startTime, {"lat": newHDGPos.lat, "lng": newHDGPos.lon}]];

    let stepTime = startTime;
    let step0 = 600 - startTime.getSeconds() - 60 * (startTime.getMinutes() % 10);
    let delta = step0;
    let pathDist = 0;

    let selPolars = document.getElementById('sel_polars');
    
    for (var step = step0; step < 86400 && pathDist < targetDist; step += 600) {
        // Calculate TWA and HDG step
        let windTWA = await gribCache.getWind(newTWAPos.lat, newTWAPos.lon, stepTime);
        let windHDG = await gribCache.getWind(newHDGPos.lat, newHDGPos.lon, stepTime);

        let twaHeading = Util.toHeading(twa, windTWA.direction);
        let hdgTWA = Util.toTWA(heading, windHDG.direction);

        // Use the PolarManager instead of direct function calls
        let analyzer = await polarManager.getAnalyzer(selPolars.value);
        let speedTWA = await analyzer.boatSpeed(Util.ms2knots(windTWA.speed), twa).speed;
        let speedHDG = await analyzer.boatSpeed(Util.ms2knots(windHDG.speed), hdgTWA).speed;

        let distTWA = delta * (speedTWA/3600);
        let distHDG = delta * (speedHDG/3600);
        
        newTWAPos = Util.addDistance(newTWAPos, distTWA, twaHeading);
        newHDGPos = Util.addDistance(newHDGPos, distHDG, heading);

        stepTime = new Date(startTime.getTime() + step * 1000);

        twaPath.push([
            stepTime,
            { lat: newTWAPos.lat, lng: newTWAPos.lon },
            {
                windSpeed: Util.ms2knots(windTWA.speed),
                windDir: windTWA.direction,
                boatSpeed: speedTWA
            }
        ]);
        hdgPath.push([
            stepTime,
            { lat: newHDGPos.lat, lng: newHDGPos.lon },
            {
                windSpeed: Util.ms2knots(windHDG.speed),
                windDir: windHDG.direction,
                boatSpeed: speedHDG
            }
        ]);

        pathDist += distTWA;
        delta = 600;
    }
    
    drawTWAPath(twaPath);
    drawHDGPath(hdgPath);
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

    document.getElementById("bt_nmeaupdate").addEventListener("click", getBoatPosition);

    document.getElementById("tb_nmeaport").addEventListener("change", onSetNMEAPort);
    document.getElementById("tb_nmeahost").addEventListener("blur", onSetNMEAPort);
    // GPX import button
    const btSetCourse = document.getElementById("bt_setcourse");
    if (btSetCourse) {
        btSetCourse.addEventListener("click", onImportGPX);
    }

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

    // Ensure world copies exist initially
    updateRouteMarkerCopies(startMarker);
    updateRouteMarkerCopies(destinationMarker);

    // Wind canvas
    let canvas = setupCanvas();

    document.getElementById("tb_position").addEventListener("keyup", function (event) {
        if (event.keyCode === 13) {
            onSetStartPosition(event);
        }
    });
    document.getElementById("bt_position").addEventListener("click", onSetStartPosition);
    
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
    const marker = L.marker([0, 0], {
        title: title,
        icon: L.icon({
            iconUrl: url,
            iconAnchor: [iconX, iconY],
            popupAnchor: [popupX, popupY],
        }),
        draggable: true
    }).addTo(map);

    marker.on('click', function () { onMarkerClicked(marker); });
    marker.on('dragend', function () { setRoutePoint(type, marker.getLatLng()); });

    return marker;
}

////////////////////////////////////////////////////////////////////////////////
/// Event handlers

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

function onWindowResize(event) {
    map.invalidateSize();
}

function onSetNMEAPort(event) {
    const port = event.currentTarget.value;
    storeValue('nmeaport', port);
    captureRaceSettings('nmeaPort', port);
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


function onContextMenu(point) {
    var mapMenu = document.getElementById("mapMenu");
    mapMenu.style.display = "none";
    setRoutePoint(point, map.mouseEventToLatLng(mapEvent.originalEvent));
}

function onCopyPosition(event) {
    let label = document.getElementById('lb_position');
    let text = label.__pos.lat + ' ' + label.__pos.lng;
    navigator.clipboard.writeText(text);
}

function onSelectIsochrone(isochrone) {
    currentCycle = isochrone.cycle;
    var offset = isochrone.offset;
    document.getElementById("ir_index").value = offset;
    updateIsochrones();
    redrawWindByOffset(offset);
}

function onDisplaywind(event) {
    var cbDisplaywind = document.getElementById("cb_displaywind");
    if (cbDisplaywind.checked) {
        document.getElementById("wind-canvas").style.display = "block";
        updateMap()
    } else {
        document.getElementById("wind-canvas").style.display = "none";
    }
}

function onDisplayTracks(event) {
    clearRoute();
    displayRouting(routeInfo);
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

function onSetStartPosition(event) {
    var position = document.getElementById("tb_position").value;
    var latlon = parsePosition(position);
    if (latlon) {
        setRoutePoint('start', L.latLng(latlon.lat, latlon.lon));
        // setRoutePointCore updates the input; no extra action needed here
    }
}

function onSetResolution(event) {
    let resolution = event.currentTarget.value;
    settings.resolution = resolution;
    storeValue('resolution', resolution);
    redrawWindByOffset(ir_index.value);
    captureRaceSettings('resolution', resolution);
}

function onSetPolars(event) {
    let polarsId = event.currentTarget.value;
    settings.polarsId = polarsId; // <-- Update settings
    storeValue('polarsId', polarsId); // (optional: persist selection)
    captureRaceSettings('polarsId', polarsId);
}

function onSetDuration(event) {
    let duration = event.target.value;
    settings.duration = duration * 3600;
    storeValue('duration', duration);
    captureRaceSettings('duration', duration);
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
        baseTime = isochrone.cycle;
    } else {
        baseTime = availableForecastCycle();
    }

    redrawWindByTime(new Date(time));
}

function getBoatPosition (event) {
    let host = document.getElementById("tb_nmeahost").value
    let port = document.getElementById("tb_nmeaport").value
    Util.doGET(
        `/function/router.getBoatPosition?host=${host}&port=${port}`,
        function (data) {
            console.log(data);
            if (data) {
                let gprmc = JSON.parse(data.response);
                var startPos = {
                    "lat": gprmc.position.lat,
                    "lng": gprmc.position.lng
                };
                setRoutePoint('start', startPos); // will also update the “Start position” field
                alert('Position ' + JSON.stringify(startPos) + ' at ' + gprmc.time);
                var curTime = new Date(gprmc.time);
                var isoDate = curTime.toISOString().substring(0,16);
                var dateInput = document.getElementById("tb_starttime");
                dateInput.value = isoDate;
            } else {
                alert('No position update');
            }
        },
        function (response) {
            console.log(response.statusText);
            alert('Could not get boat position: ' + response.statusText);
        });
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


function updateMap() {
    var bounds = getMapBounds();

    // Load wind
    if (!gribCache) {
        let canvas = document.getElementById('wind-canvas');
        gribCache = new GribCache(canvas, bounds || { "north": 50, "south": 40, "west": 0, "east": 10 }, settings.resolution, new Date());
    }

    redrawWindByOffset(ir_index.value);
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


function storeValue(name, value) {
    try {
        let storage = window.localStorage;
        let query = new URL(document.URL).searchParams;
        storage.setItem(`xx.${name}`, value);
    } catch (e) {
    }
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
    startPos.lng = wrapLon180(startPos.lng);
    destPos.lng = wrapLon180(destPos.lng);

    var query = makeQuery(settings);
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
        function (response) {
            bt_execute.disabled = false;
            restoreCursor();
            window.clearInterval(timer);
            pgGetRoute.value = pgGetRoute.max;
            alert(`Request\n${response.responseURL}\n\nreturned\n\n ${response.status} - ${response.statusText}`);
        });
}



// Set route point (start or destination)
function setRoutePoint(type, latlng) {
    if (type === 'start' && startMarker) {
        setRoutePointCore('start', latlng);
    } else if (type === 'dest' && destinationMarker) {
        setRoutePointCore('dest', latlng);
    }
    drawRouteLine();
}

function setRoutePointCore(point, latLng) {
    storeValue(point, `{"lat":${latLng.lat}, "lon": ${latLng.lng}}`);

    if (point === 'start') {
        startMarker.setLatLng(latLng);
        updateRouteMarkerCopies(startMarker);
        // Keep the Start position input in sync (DMS if needed)
        updateStartPositionDisplay(startMarker.getLatLng());
    } else if (point === 'dest') {
        destinationMarker.setLatLng(latLng);
        updateRouteMarkerCopies(destinationMarker);
    }

    // Draw a short-segment great-circle line between start and destination
    if (courseGCLine) {
        map.removeLayer(courseGCLine);
        courseGCLine = null;
    }
    const s = startMarker.getLatLng();
    const d = destinationMarker.getLatLng();
    const gcSegments = buildShortestSegments(s, d);
    const gcGroup = L.featureGroup();
    for (const seg of gcSegments) {
        L.polyline(seg, {
            color: '#d00000',
            weight: 1,
            opacity: 1.0
        }).addTo(gcGroup);
    }
    gcGroup.addTo(map);
    courseGCLine = gcGroup;
    captureRaceSettings(point, latLng);
}

// Keep the “Start position” input up to date in DMS format
function updateStartPositionDisplay(latlng) {
    const input = document.getElementById('tb_position');
    if (!input || !latlng) return;
    // Always show normalized longitude in display
    const dispLng = wrapLon180(latlng.lng);
    input.value = Util.formatPosition(latlng.lat, dispLng);
}

function createBestRouteMarkerAndCopies(entry, icon, startTime) {
    const lat = entry.position.lat;
    const baseLng = wrapLon180(entry.position.lng);

    // Build tooltip once
    const tooltipHtml = makeWaypointInfo(startTime, entry);

    // Base marker (interactive)
    const baseMarker = L.marker([lat, baseLng], {
        icon: icon,
        draggable: false
    }).addTo(map);
    addMarkerListener(baseMarker);
    addWaypointInfo(baseMarker, startTime, entry);
    trackMarkers.unshift(baseMarker);

    // Left world copy (-360)
    const leftMarker = L.marker([lat, baseLng - 360], {
        icon: icon,
        draggable: false
    }).addTo(map);
    addMarkerListener(leftMarker);
    leftMarker.bindTooltip(tooltipHtml, { permanent: false });
    trackMarkers.unshift(leftMarker);

    // Right world copy (+360)
    const rightMarker = L.marker([lat, baseLng + 360], {
        icon: icon,
        draggable: false
    }).addTo(map);
    addMarkerListener(rightMarker);
    rightMarker.bindTooltip(tooltipHtml, { permanent: false });
    trackMarkers.unshift(rightMarker);
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
            const points = tracks[i].map(p => [p.lat, wrapLon180(p.lng)]);
            let segments = splitPathAtAntimeridian(points);
            segments = segmentsWithWorldCopies(segments);
            for (const seg of segments) {
                const track = L.polyline(seg, {
                    color: '#d00000',
                    weight: 1.5,
                    opacity: 1.0
                }).addTo(map);
                routeTracks.push(track);
            }
        }
    }

    const bestPoints = best.map(entry => [entry.position.lat, wrapLon180(entry.position.lng)]);
    let bestSegments = splitPathAtAntimeridian(bestPoints);
    bestSegments = segmentsWithWorldCopies(bestSegments);
    for (const seg of bestSegments) {
        const bestTrack = L.polyline(seg, {
            color: '#d00000',
            weight: 3,
            opacity: 1.0
        }).addTo(map);
        routeTracks.push(bestTrack);
    }

    // Duplicate best-route markers across world copies
    const baseTime = new Date(best[0].time);
    for (var i = 0; i < best.length; i++) {
        if (i == 0 || i == best.length - 1
            || best[i].penalty
            || best[i].twa != best[i - 1].twa
            || best[i].sail != best[i - 1].sail) {
            const icon = ((best[i].penalty === "sailChange") || (best[i].penalty === "tack") || (best[i].penalty === "gybe"))
                  ? redMarkerIcon : markerIcon;
            
            createBestRouteMarkerAndCopies(best[i], icon, baseTime);
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
        const isoData = isochrones[i];
        const style = getIsoStyle(isoData, 1, c);

        const points = isoData.path.map(p => [p.lat, wrapLon180(p.lng)]);
        let segments = splitPathAtAntimeridian(points);
        // Previously: segments = segmentsWithWorldCopies(segments);
        // Now: always provide ±360 copies so fully-east (or west) isochrones render on adjacent worlds
        segments = segmentsWithWorldTriplicate(segments);

        for (const seg of segments) {
            const isoLayer = L.polyline(seg, {
                color: style.color,
                weight: style.weight,
                opacity: 0.8
            }).addTo(map);
            addInfo(isoLayer, isoData.cycle, isoData.offset);
            routeIsochrones.push(isoLayer);
        }
    }
}

function getIsochroneTime(isochrone) {
    var basetime = new Date(isochrone.cycle);
    var offset = isochrone.offset;
    return new Date(basetime - 0 + offset * 3600 * 1000);
}

function getIsoStyle(isochrone, selectedOffset, availableCycle) {
    var c = availableCycle;
    var d = new Date(isochrone.cycle);
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

    result += "<b>Pos</b>: " + formatPointPosition(point);
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
    path.length = 0; // <-- This clears the array in-place!
}

function drawOrthodromic(event) {
    if (orthodromicLine) {
        map.removeLayer(orthodromicLine);
    }
    var start;
    if (twaAnchor) {
        start = twaAnchor.getLatLng();
    } else {
        start = startMarker.getLatLng();
    }
    orthodromicLine = L.polyline([start, event.latlng], {
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
    const weight = 2;
    for (let i = 1; i < data.length; i++) {
        const d = new Date(data[i][0]);
        const minutes = d.getMinutes();
        const from = data[i - 1][1];
        const to = data[i][1];
        const info = data[i][2]; // {windSpeed, windDir, boatSpeed}

        // Draw the segment (always thin)
        const segment = L.polyline([from, to], {
            color: color,
            weight: weight,
            opacity: 1.0
        }).addTo(map);
        bPath.push(segment);

        // If endpoint is a full hour, draw a thick dot at the endpoint
        if (minutes % 10 === 0) {
            const dot = L.circleMarker(to, {
                radius: (minutes === 0 ? weight * 4 : weight * 2.5),
                color: color,
                fillColor: color,
                fillOpacity: 1.0,
                weight: 0
            }).addTo(map);

            // Use precomputed info for the popup
            if (info) {
                const popupContent = `
                    <b>Time:</b> ${d.toISOString().substring(0,16)}<br>
                    <b>Wind:</b> ${info.windSpeed.toFixed(1)} kn @ ${info.windDir.toFixed(0)}°<br>
                    <b>Boat speed:</b> ${info.boatSpeed.toFixed(2)} kn
                `;
                dot.bindPopup(popupContent);
            }
            bPath.push(dot);
        }
    }
}

function addInfo(isochrone, cycle, offset) {
    isochrone.cycle = cycle;
    isochrone.offset = offset;
    isochrone.on('click', function () {
        var iso = isochrone;
        onSelectIsochrone(iso);
    });
}

function isochroneTime(isochrone) {
    var millis = new Date(isochrone.cycle).getTime();
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

    let usedCycle = cycle;

    try {
        await gribCache.update(bounds, new Date(usedCycle), time, settings.resolution);
    } catch (e1) {
        console.log(e1);
        try {
            usedCycle = availableForecastCycle();
            gribCache.update(bounds, new Date(usedCycle), time, settings.resolution);
        } catch (e2) {
            console.log(e2);
        }
    }
    document.getElementById('lb_modelrun').innerHTML = usedCycle.substring(11, 13) + 'Z';
    document.getElementById('lb_index').innerHTML = time.toISOString().substring(0, 16) + 'Z';

    var offset = (new Date(time) - new Date(usedCycle)) / 3600000;
    ir_index.value = offset;
}

async function updateWindInfo(event, getVMG) {
    let label = document.getElementById('lb_position');
    label.textContent = `[${formatLatLngPosition(event.latlng)}] - [${event.latlng.lat.toFixed(3)},${event.latlng.lng.toFixed(3)}]`;
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
            var vmg = await getVMG(windSpeed);

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
    return Util.formatPosition(point.position.lat, point.position.lng);
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

// Split a path into segments that do not cross the antimeridian.
// points: Array of [lat, lng]
function wrapLon180(lon) {
    return ((lon + 180) % 360 + 360) % 360 - 180;
}

function splitPathAtAntimeridian(points) {
    if (!points || points.length < 2) return [points];
    const segments = [];
    let current = [[points[0][0], wrapLon180(points[0][1])]];

    for (let i = 1; i < points.length; i++) {
        let lat0 = current[current.length - 1][0];
        let lon0 = current[current.length - 1][1];
        let lat1 = points[i][0];
        let lon1 = wrapLon180(points[i][1]);

        let d = lon1 - lon0;
        let lon1Adj = lon1;
        if (d > 180) lon1Adj = lon1 - 360;
        else if (d < -180) lon1Adj = lon1 + 360;

        if (lon1Adj > 180 || lon1Adj < -180) {
            const boundary = (lon1Adj > 180) ? 180 : -180;
            const t = (boundary - lon0) / (lon1Adj - lon0);
            const latX = lat0 + t * (lat1 - lat0);

            current.push([latX, boundary]);
            segments.push(current);

            const boundaryOpp = (boundary === 180) ? -180 : 180;
            current = [[latX, boundaryOpp], [lat1, wrapLon180(lon1Adj)]];
        } else {
            current.push([lat1, lon1]);
        }
    }
    if (current.length > 1) segments.push(current);
    return segments;
}

// Duplicate segments that touch the antimeridian into adjacent world copies
function hasBoundary(seg, boundary) {
    // boundary is +180 or -180
    return seg.some(p => p[1] === boundary);
}

function shiftSegment(seg, delta) {
    // delta = ±360
    return seg.map(p => [p[0], p[1] + delta]);
}

function segmentsWithWorldCopies(segments) {
    const out = [];
    for (const seg of segments) {
        out.push(seg);
        // If a segment ends on +180, add a copy shifted -360 (left world)
        if (hasBoundary(seg, 180)) out.push(shiftSegment(seg, -360));
        // If a segment ends on -180, add a copy shifted +360 (right world)
        if (hasBoundary(seg, -180)) out.push(shiftSegment(seg, +360));
    }
    return out;
}

// Duplicate segments into all adjacent worlds to ensure visibility regardless of pan
function segmentsWithWorldTriplicate(segments) {
    const out = [];
    for (const seg of segments) {
        out.push(seg);
        out.push(seg.map(p => [p[0], p[1] - 360])); // left world
        out.push(seg.map(p => [p[0], p[1] + 360])); // right world
    }
    return out;
}

function updateRouteMarkerCopies(marker) {
    if (!marker) return;
    const base = marker.getLatLng();
    const baseLng = wrapLon180(base.lng);
    const baseLatLng = L.latLng(base.lat, baseLng);

    // Keep the base marker normalized to [-180, 180]
    if (base.lng !== baseLng) marker.setLatLng(baseLatLng);

    const icon = marker.options.icon;
    const opts = { icon, interactive: false, draggable: false };

    // Create or update left/right world copies
    if (!marker.__leftCopy) {
        marker.__leftCopy = L.marker([baseLatLng.lat, baseLng - 360], opts).addTo(map);
    } else {
        marker.__leftCopy.setLatLng([baseLatLng.lat, baseLng - 360]);
    }
    if (!marker.__rightCopy) {
        marker.__rightCopy = L.marker([baseLatLng.lat, baseLng + 360], opts).addTo(map);
    } else {
        marker.__rightCopy.setLatLng([baseLatLng.lat, baseLng + 360]);
    }
}

function onImportGPX() {
    const input = document.createElement('input');
    input.type = 'file';
    input.accept = '.gpx,application/gpx+xml,.xml,text/xml';
    input.onchange = async (e) => {
        const file = e.target.files && e.target.files[0];
        if (!file) return;
        try {
            const text = await file.text();
            const result = importGPXFromString(text);
            if (result && result.raceName) {
                addRace(result.raceName);
                if (result.gates) {
                    captureRaceSettings('gates', result.gates);
                }
                // Set start/destination markers to gate midpoints (after race added so settings persist)
                if (result.startMidpoint) {
                    setRoutePointCore('start', L.latLng(result.startMidpoint.lat, result.startMidpoint.lon));
                }
                if (result.finishMidpoint) {
                    setRoutePointCore('dest', L.latLng(result.finishMidpoint.lat, result.finishMidpoint.lon));
                }
            }
        } catch (err) {
            console.error('Failed to read GPX file', err);
            alert('Failed to read GPX file.');
        }
    };
    input.click();
}

function importGPXFromString(xmlString) {
    // Clear any previous GPX course overlays
    clearCourseOverlays();

    let doc;
    let raceName;
    try {
        const parser = new DOMParser();
        doc = parser.parseFromString(xmlString, 'application/xml');
        const parserErr = doc.getElementsByTagName('parsererror')[0];
        if (parserErr) {
            console.error(parserErr.textContent);
            throw new Error('Invalid GPX XML');
        }
        raceName = doc.getElementsByTagName('metadata')[0]?.getElementsByTagName('name')[0].textContent;
    } catch (err) {
        console.error('Invalid GPX', err);
        alert('Invalid GPX file.');
        return;
    }

    const wpts = Array.from(doc.getElementsByTagName('wpt'));
    if (!wpts.length) {
        alert('No waypoints found in GPX.');
        return;
    }

    // Group waypoints by gate token: S, F, G1, G2, ...
    // Names like: "FW 2290 - S-A", "FW 2290 - G1-Mid", "FW 2290 - F-B"
    const groups = new Map(); // key -> array of {lat, lon, name}
    for (const wpt of wpts) {
        const lat = parseFloat(wpt.getAttribute('lat'));
        const lon = parseFloat(wpt.getAttribute('lon'));
        const nameEl = wpt.getElementsByTagName('name')[0];
        const rawName = nameEl ? nameEl.textContent.trim() : '';

        const groupKey = extractGateGroupKey(rawName);
        if (!groupKey) continue;

        const arr = groups.get(groupKey) || [];
        arr.push({ lat, lon, name: rawName, key: groupKey });
        groups.set(groupKey, arr);
    }

    // Prepare serialized gates structure before drawing
    const gates = [];
    for (const [key, pts] of groups.entries()) {
        gates.push({
            key,
            points: pts.map(p => ({ lat: p.lat, lon: p.lon, name: p.name }))
        });
        // Draw each group with its color
        const color = gateColor(key);
        drawGateGroup(pts, color);
    }

    // Determine explicit midpoints for S and F gates.
    // Midpoint waypoints have names ending in "S-Mid" or "F-Mid" respectively.
    const findExplicitMidpoint = (arr, gateLetter) => {
        if (!arr) return null;
        const suffix = gateLetter.toUpperCase() + '-MID';
        for (const p of arr) {
            const nameUpper = (p.name || '').trim().toUpperCase();
            if (nameUpper.endsWith(suffix)) {
                return { lat: p.lat, lon: p.lon };
            }
        }
        return null;
    };
    const startMidpoint = findExplicitMidpoint(groups.get('S'), 'S');
    const finishMidpoint = findExplicitMidpoint(groups.get('F'), 'F');

    // After drawing, pan & zoom to show all gates
    fitMapToGates(groups);

    return { raceName, gates, startMidpoint, finishMidpoint };
}

function extractGateGroupKey(name) {
    // Expect format "<race> - <group>-<rest>"
    // Fallback: find first token after '-' that looks like S, F or G<number>
    // Examples:
    //  "FW 2290 - S-A"     -> "S"
    //  "FW 2290 - F-Mid"   -> "F"
    //  "FW 2290 - G2-A"    -> "G2"
    //  "G3-B"              -> "G3" (handles minimal format too)
    if (!name) return null;
    const parts = name.split('-').map(s => s.trim()).filter(Boolean);
    // Try to find a token matching S, F, or G\d+
    for (const token of parts) {
        if (token === 'S' || token === 'F' || /^G\d+$/i.test(token)) {
            return token.toUpperCase();
        }
    }
    // As a fallback, if we have at least 2 parts, the second may be the group
    if (parts.length >= 2) {
        const t = parts[1].toUpperCase();
        if (t === 'S' || t === 'F' || /^G\d+$/i.test(t)) return t;
    }
    return null;
}

// Choose the nearest wrapped longitude to a reference (handles ±360 copies)
function nearestWrapped(refLon, lon) {
    const l = wrapLon180(lon);
    const candidates = [l, l + 360, l - 360];
    return candidates.reduce((best, cur) =>
        Math.abs(cur - refLon) < Math.abs(best - refLon) ? cur : best, candidates[0]);
}

// Fit map to include both Start (S) and Finish (F) groups with minimal longitudinal span
function fitMapToGates(groups) {
    // Accept either a Map(key -> array of point objects) OR an array of gate objects
    if (!map || !groups) return;

    if (Array.isArray(groups)) {
        // Convert array [{key, points:[...]}, ...] to Map format used originally
        const m = new Map();
        for (const g of groups) {
            if (!g || !g.key || !g.points) continue;
            // Normalize to objects with lat/lon/name/key like original import
            const arr = g.points.map(p => ({ lat: p.lat, lon: p.lon, name: p.name, key: g.key }));
            m.set(g.key, arr);
        }
        groups = m;
    }

    if (!(groups instanceof Map) || groups.size === 0) return;

    // Choose reference group for longitudinal wrapping: prefer 'S', else first available
    let refGroup = groups.get('S');
    if (!refGroup || !refGroup.length) {
        for (const [, arr] of groups.entries()) { if (arr && arr.length) { refGroup = arr; break; } }
    }
    if (!refGroup || !refGroup.length) return;

    const refLon = wrapLon180(refGroup[0].lon);
    const pts = [];
    for (const [, arr] of groups.entries()) {
        if (!arr || !arr.length) continue;
        for (const p of arr) {
            pts.push(L.latLng(p.lat, nearestWrapped(refLon, p.lon)));
        }
    }
    if (!pts.length) return;
    const bounds = L.latLngBounds(pts);
    map.fitBounds(bounds, { padding: [24, 24] });
}

function gateColor(key) {
    if (key === 'S') return '#60B260';    // green
    if (key === 'F') return '#d00000';    // red
    if (/^G\d+$/i.test(key)) return '#00c2f8'; // blue
    return '#808080';
}

function drawGateGroup(points, color) {
    // Draw markers (with world copies) and connect with a polyline
    // Keep original order as in the GPX file
    const latlngs = points.map(p => [p.lat, wrapLon180(p.lon)]);

    // Markers for each waypoint
    for (const p of points) {
        const baseLng = wrapLon180(p.lon);
        const markers = createCircleMarkerWithWorldCopies([p.lat, baseLng], color, 5, p.name);
        for (const m of markers) {
            courseGateLayers.push(m);
        }
    }

    // Connect the group with a colored line, splitting at antimeridian and triplicating
    let segments = splitPathAtAntimeridian(latlngs);
    segments = segmentsWithWorldTriplicate(segments);

    for (const seg of segments) {
        const line = L.polyline(seg, {
            color,
            weight: 2,
            opacity: 1.0
        }).addTo(map);
        courseGateLayers.push(line);
    }
}

function createCircleMarkerWithWorldCopies(latlng, color, radius = 5, tooltipText) {
    const [lat, lng] = latlng;

    const mk = (L.marker ? L.circleMarker([lat, lng], {
        radius,
        color,
        fillColor: color,
        fillOpacity: 1.0,
        weight: 1
    }) : null);

    const base = L.circleMarker([lat, lng], {
        radius,
        color,
        fillColor: color,
        fillOpacity: 1.0,
        weight: 1
    }).addTo(map);
    if (tooltipText) base.bindTooltip(tooltipText, { permanent: false });

    const left = L.circleMarker([lat, lng - 360], {
        radius,
        color,
        fillColor: color,
        fillOpacity: 1.0,
        weight: 1
    }).addTo(map);
    if (tooltipText) left.bindTooltip(tooltipText, { permanent: false });

    const right = L.circleMarker([lat, lng + 360], {
        radius,
        color,
        fillColor: color,
        fillOpacity: 1.0,
        weight: 1
    }).addTo(map);
    if (tooltipText) right.bindTooltip(tooltipText, { permanent: false });

    return [base, left, right];
}

function clearCourseOverlays() {
    for (const layer of courseGateLayers) {
        map.removeLayer(layer);
    }
    courseGateLayers.length = 0;
}




// Initialize everything when the page loads
window.addEventListener("load", function() {
    setupPage();
});
