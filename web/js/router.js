////////////////////////////////////////////////////////////////////////////////
/// Bitsailor Router UI Stateless

import * as Util from './Util.js';
import WindTile from './WindTile.js';
import * as GPX from './GPX.js';
// import GRIB2 from './grib22json/grib2.js'
// import * as GRIB2UTILS from './grib22json/grib2utils.js'


var sailNames = ["Jib", "Spi", "Stay", "LJ", "C0", "HG", "LG"];
var settings = {
    "resolution": "1p00",
    "gfsMode": "06h",
    "presets": "VR",
    "options": ["hull", "winch", "foil", "heavy", "light", "reach"],
    "polarsId": "1"
};

let selectedCursor = 'crosshair';

var windTile = undefined;

var googleMap = null;
var mapEvent;

// Bounds and width from the map are kept here for convenience
var geometry = {};

// Time index
var ir_index;

var currentCycle = getCurrentCycle()

var startMarker = {};
var destinationMarker = {};
var twaAnchor = undefined;

var courseGCLine = new google.maps.Polyline({
    geodesic: true,
    strokeColor: '#ff0000',
    strokeOpacity: 1.0,
    strokeWeight: 1
});

var routeTracks = [];
var routeIsochrones = [];
var trackMarkers = [];

var polars = null;
var routeInfo = {};
var twaPath = [];
var hdgPath = [];

function setUp (getVMG) {

   //  loadWind('http://localhost:8080/Grib2JS/data/20240218.12.015.0p25.orig.grib2');

    setupColors();
    
    // Create a map object, and include the MapTypeId to add
    // to the map type control.
    var mapProp = {
        center:new google.maps.LatLng(49.187, 8.473),
        zoom:5,
        minZoom:3,
        maxZoom:14,
        scaleControl: true,
        mapTypeId:google.maps.MapTypeId.ROADMAP,
        draggableCursor: "crosshair"
    };
    var mapDiv = $("#map")[0];
    googleMap = new google.maps.Map(mapDiv, mapProp);
    
    // Handle window resize
    window.addEventListener('resize', onWindowResize);
    
    // Connect map events
    google.maps.event.addListener(googleMap, 'zoom_changed', updateMap);
    google.maps.event.addListener(googleMap, 'dragend', updateMap);
    google.maps.event.addDomListener(googleMap, 'rightclick', onMapRightClick);
    
    // Track cursor position
    google.maps.event.addListener(googleMap, 'mousemove', async function (event) {
        return await updateWindInfo(event, getVMG);
    });

    // Moved to -vr.js
    // google.maps.event.addListener(googleMap, 'click', getTWAPath);
    google.maps.event.addListener(googleMap, 'click', drawOrthodromic);

    // Connect button events
    $("#bt_getroute").click(getRoute);
    $("#bt_downloadroute").click(onDownloadRoute);
    
    $("#bt_inc").click(onAdjustIndex);
    $("#bt_dec").click(onAdjustIndex);
    $("#bt_inc6").click(onAdjustIndex);
    $("#bt_dec6").click(onAdjustIndex);
    $("#ir_index").change(onAdjustIndex);
    
    $("#cb_startdelayed").click(onDelayedStart);
    
    $("#cb_manualcycle").click(onManualCycle);
    $("#tb_cycledate").change(onSetCycleTS);
    $("#sel_cyclehour").change(onSetCycleTS);
    
    // Connect option selectors
    $("#sel_duration").change(onSetDuration);
    $("#cb_displaywind").change(onDisplaywind);
    $("#cb_displaytracks").change(onDisplayTracks);
    $("#rb_crosshair").change(function (event) { onCursorSelect(event, 'crosshair'); });
    $("#rb_default").change(function (event) { onCursorSelect(event, 'default'); });
    
    // Disable default contextmenu
    window.oncontextmenu = function (event) { event.preventDefault() }; 
    
    // Connect menu events
    $("#bt_setstart" ).click(function () { onContextMenu('start') });
    $("#bt_setdest"  ).click(function () { onContextMenu('dest') });
    $("#bt_copypos"  ).click(onCopyPosition);
    $("#bt_ltpmark"  ).click(function () { onContextMenu('ltp') });
    $("#bt_ltsmark"  ).click(function () { onContextMenu('lts') });
    
    var mapMenu = $("#mapMenu")[0];
    mapMenu.onmouseleave = onMapMenuMouseLeave;
    
    ir_index = $("#ir_index")[0];
    
    startMarker = initMarker('start', 'Start', 'img/start_45x32.png', 1, 45);
    destinationMarker = initMarker('dest', 'Destination',  'img/finish_32x20.png', 1, 32);
    
    // Wind canvas
    let canvas = setupCanvas();
 
    document.getElementById("tb_position").addEventListener("keyup", function (event) {
        if (event.keyCode === 13) {
            onSetStartPosition(event);
        }
    });
    document.getElementById("bt_position").addEventListener("click", onSetStartPosition);
    document.getElementById("bt_setstart").addEventListener("click", onContextMenuSetStart);
    google.maps.event.addListener(startMarker,'dragend', function () {
        // Update position entry/display
        onUpdateStartMarker(startMarker);
    });
}

function loadWind (path) {
    fetch(path)
        .then(response => response.arrayBuffer())
        .then(buffer => GRIB2UTILS.decodeGRIB2File(buffer))
        .catch(error => console.log(error));
}

function initMarker (type, title, url, x, y) {
    var marker = new google.maps.Marker({
        position: {"lat": 0, "lng": 0},
        map: googleMap,
        title: title,
        icon: {
            url: url,
            origin: new google.maps.Point(0, 0),
            anchor: new google.maps.Point(x, y)
        },
        draggable: true
    });
    
    marker.addListener('click', function () { onMarkerClicked(marker) });
    
    google.maps.event.addListener(marker,'dragend',function() {
        setRoutePoint(type, marker.getPosition());
    });
    
    return marker;
}

////////////////////////////////////////////////////////////////////////////////
/// Event handlers

function onWindowResize (event) {
}

function onDownloadRoute (event) {
    if (routeInfo && routeInfo.best) {
        let rbGPX = document.getElementById('rb_gpx');
        let format = rbGPX.checked?'gpx':'csv';
        let content = GPX.exportRoute(routeInfo, format);
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

function updateMap () {
    // Delegate to callback -
    // This causes the update to be executed only when the map bounds are valid.
    // Probably only matters on the first update after page load.
    google.maps.event.addListenerOnce(googleMap, 'idle', function(){
        if ((googleMap.getMapTypeId() == google.maps.MapTypeId.ROADMAP)
            || (googleMap.getMapTypeId() == google.maps.MapTypeId.TERRAIN)) {
            if ( googleMap.zoom < 6 ) {
                googleMap.setMapTypeId(google.maps.MapTypeId.ROADMAP);
            } else {
                googleMap.setMapTypeId(google.maps.MapTypeId.TERRAIN);
            }
        }
        var bounds = getMapBounds();

        // Load wind
        if (!windTile) {
            let canvas = document.getElementById('wind-canvas');
            windTile = new WindTile(canvas, bounds || {"north": 50, "south": 40, "west": 0, "east": 10}, settings.resolution, new Date());
        }

        var label = "⌊" + formatLatLngPosition(bounds.southWest) + " \\ " +  formatLatLngPosition(bounds.northEast) + "⌉";
        $("#lb_map_bounds").text("Kartenausschnitt: " + label);
        redrawWindByOffset(ir_index.value);
    });
}

function availableForecastCycle (d=new Date()) {
    // Display cycle only when it's (supposed to be) fully available
    var availDate = d - 300 * 60 * 1000;
    var fc = truncate(availDate, 6 * 3600 * 1000);
    return new Date(fc).toISOString();
}

function getCurrentCycle (d=new Date()) {
    var availDate = d - 210 * 60 * 1000;
    var fc = truncate(availDate, 6 * 3600 * 1000);
    return new Date(fc).toISOString();
}

// Event handler for context menu mapMenu 
function onContextMenu (point) {
    var mapMenu=$("#mapMenu")[0];
    mapMenu.style.display = "none";
    setRoutePoint(point, mapEvent.latLng);
}

function onCopyPosition (event) {
    let label = document.getElementById('lb_position');
    let text = label.__pos.lat() + ' ' + label.__pos.lng()
    navigator.clipboard.writeText(text);
}

function onSelectIsochrone (isochrone) {
    currentCycle = isochrone.get('time');
    var offset = isochrone.get('offset');
    $("#ir_index")[0].value = offset;
    updateIsochrones();
    redrawWindByOffset(offset);
}

function onDisplaywind (event) {
    var cbDisplaywind = $("#cb_displaywind")[0];
    if (cbDisplaywind.checked) {
        $("#wind-canvas").show();
    } else {
        $("#wind-canvas").hide();
    }
}

function onDisplayTracks (event) {
    clearRoute();
    displayRouting(routeInfo);
}

function onCursorSelect (event, type) {
    selectedCursor = type;
    googleMap.setOptions({draggableCursor:type});
}


function setBusyCursor () {
    document.getElementById('bt_getroute').style.cursor = "wait";
    document.getElementById("body").style.cursor = "wait";
    googleMap.setOptions({draggableCursor:"wait"});
}

function restoreCursor () {
    document.getElementById("body").style.cursor = "pointer";
    document.getElementById('bt_getroute').style.cursor = "pointer";
    googleMap.setOptions({draggableCursor:selectedCursor});
}

function onAdjustIndex (event) {
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

function onDelayedStart (event) {
    var dateInput =  $("#tb_starttime")[0];
    if (event.target.checked === true) {
        var d = new Date();
        var isoDate = d.toISOString().substring(0,16);
        dateInput.value = isoDate;
    } else {
        dateInput.value = null;
    }
}

function getStartTime () {
    var cbDelayed =  $("#cb_startdelayed")[0];
    if (cbDelayed.checked === true) {
        var dateInput =  $("#tb_starttime")[0];
        return dateInput.value;
    } else {
        var d = new Date();
        return d.toISOString().substring(0,16);
    }
}

function onContextMenuSetStart (event) {
    var textBox = document.getElementById("tb_position");
    var position = mapEvent.latLng;
    textBox.value = Util.formatPosition(position.lat(), position.lng()); 
}

function onUpdateStartMarker (marker) {
    var textBox = document.getElementById("tb_position");
    var position = marker.getPosition();
    textBox.value = Util.formatPosition(position.lat(), position.lng()); 
}

function onSetStartPosition (event) {
    var position = document.getElementById("tb_position").value;
    var latlon = parsePosition(position);
    if (latlon) {
        var latLon = new google.maps.LatLng(latlon.lat, latlon.lon);
        setRoutePoint('start', latLon);
    }
}

function onSetResolution (event) {
    let resolution= event.currentTarget.value;
    settings.resolution = resolution;
    storeValue('resolution', resolution);
    redrawWindByOffset(ir_index.value);
}

function onSetDuration (event) {
    let duration =  event.target.value;
    settings.duration = duration * 3600;
    storeValue('duration', duration);
}

function storeValue (name, value) {
    try {
        let storage = window.localStorage;
        let query = new URL(document.URL).searchParams;
        let raceId = query.get('race');
        storage.setItem(`${raceId}.${name}`, value);
    } catch (e) {
    }
}

function getValue (name) {
    let storage = window.localStorage;
    let query = new URL(document.URL).searchParams;
    let raceId = query.get('race');
    return storage.getItem(`${raceId}.${name}`);
}


function parsePosition (string) {
    try {
        // Assume two comma separated DMS values
        string = string.replace(`/`,`,`);
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
    string = string.replace(`''`,`'`);
    string = string.replace(`º`,`°`);
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

function truncate (n, q) {
    return n - (n % q);
}

function pad0 (val, length=2, base=10) {
    var result = val.toString(base)
    while (result.length < length) result = '0' + result;
    return result;
}


function getManualCycle () {
    var dateInput =  $("#tb_cycledate")[0];
    var hourInput =  $("#sel_cyclehour")[0];
    return dateInput.value + "T" + pad0(hourInput.value) + ":00:00Z";
}

function onManualCycle (event) {

    var manualCycle = document.getElementById("cb_manualcycle");
    var dateInput =  document.getElementById("tb_cycledate");
    var hourInput =  document.getElementById("sel_cyclehour");
    var cycle = availableForecastCycle();

    // Whenever manual cycle is toggled, initialize date with available cycle.
    dateInput.value = cycle.substring(0,10);
    hourInput.value = Number.parseInt(cycle.substring(11,13));

    // When it was switched off, value remains set to available cycle.
    // When it was switched on, user may modify the cycle using date and hour controls.
    var params = { "name": "cycle" }; 
    if (manualCycle.checked) {
        cycle =  getManualCycle();
        params.value = cycle
    }

    Util.doGET(
        "/function/router.setParameter",
        function (request) {
            console.log(request.responseText);
            redrawWindByOffset(0);
        },
        function (request) {
            alert('Could not set cycle: ' + request.textStatus + ' ' + request.errorThrown + ' ' + request.responseText);
        },
        params
    );
}

function onSetCycleTS (event) {
    var manualCycle = document.getElementById("cb_manualcycle");
    if ( manualCycle.checked ) {
        var dateInput =  document.getElementById("tb_cycledate");
        var hourInput =  document.getElementById("sel_cyclehour");
        setttings.cycleTS = dateInput.value + "T" + pad0(hourInput.value) + ":00:00Z";
        redrawWindByOffset("0");
    }
}

function onSetGFSMode (event) {
    settings.gfsMode = event.currentTarget.value;
}

function onMapMenuMouseLeave (event) {
    var mapMenu=$("#mapMenu")[0];
    mapMenu.style.display = "none";
}

function onMapRightClick (event) {
    mapEvent = event;
    var windowEvent = window.event;
    var mapMenu=$("#mapMenu")[0];
    var pageY;
    var pageX;
    if (windowEvent != undefined) {
        pageX = windowEvent.pageX;
        pageY = windowEvent.pageY;
    } else {
        pageX = event.pixel.x;
        pageY = event.pixel.y;
    }
    
    mapMenu.style.display = "block";
    mapMenu.style["z-index"] = 400;
    mapMenu.style.top = pageY + "px";
    mapMenu.style.left = pageX + "px";
    return false;
}

function onMarkerClicked (marker) {
    twaAnchor = marker;
    
    var time = marker.get('time');
    var isochrone = getIsochroneByTime(time);
    var baseTime;

    if (isochrone) {
        baseTime = isochrone.time;
    } else {
        baseTime = availableForecastCycle();
    }
    
    redrawWindByTime(time);
}

//////////////////////////////////////////////////////////////////////
/// Accessors

function setResolution (resolution) {
    settings.resolution = resolution;
    document.getElementById("sel_resolution").value = resolution;
}

function setDuration (duration) {
    settings.duration = duration * 3600;
    document.getElementById('sel_duration').value = duration;
}


function setPolars (data) {
    polars = data;
}

function getPolars () {
    return polars;
}

function setSailnames (sailnames) {
    sailNames = sailnames;
}

function getSailnames () {
    return sailNames;
}


//////////////////////////////////////////////////////////////////////
/// XHR requests

function makeQuery (object) {
    var s = "";
    for (const m in object) {
        if (object[m]) {
            if (s == "") {
                s = "?";
            } else {
                s += "&";
            }
            s += `${ m }=${ object[m]}`;
        }
    }
    return s;
}

function getRoute () {
    setBusyCursor();
    var bt_execute=$("#bt_getroute")[0];
    bt_execute.disabled = true;

    let documentQuery = new URL(document.URL).searchParams;
    let raceId = documentQuery.get('race');
    
    var mapMenu=$("#mapMenu")[0];
    var windowEvent = window.event;
    mapMenu.style.display = "none";

    var pgGetRoute = $("#pg_getroute")[0];
    pgGetRoute.value = 5;

    var selDuration = document.getElementById('sel_duration');
    var duration = selDuration.value || 48;
    var timer = window.setInterval(updateGetRouteProgress, 10 * duration);

    var startTime = getStartTime();
    var startPos = startMarker.getPosition();
    var destPos = destinationMarker.getPosition();
    
    var query = makeQuery(settings);
    query += `&raceId=${raceId}`;
    query += `&startTime=${startTime}`;
    query += `&slat=${startPos.lat()}&slon=${startPos.lng()}&dlat=${destPos.lat()}&dlon=${destPos.lng()}`;

    var energyInput = document.getElementById('tb_currentenergy');
    if (energyInput) {
        query += `&energy=${energyInput.value}`;
    }
    
    var tackInput = document.getElementById('sel_currenttack');
    if (tackInput) {
        query += `&tack=${tackInput.value}`;
    }

    var sailInput = document.getElementById('sel_currentsail');
    if (sailInput) {
        query += `&sail=${sailInput.value}`;
    }

    $.ajax({
        url: "/function/router.getRoute" + query,
        dataType: 'json'
    }).done( function (data) {
        // $('div, button, input').css('cursor', 'auto');

        // Remember routing data
        routeInfo = data;

        // Reset timer
        window.clearInterval(timer);
        pgGetRoute.value = pgGetRoute.max;

        // Display new data
        clearRoute();
        displayRouting(data);
        bt_execute.disabled = false;
        restoreCursor();
    }).fail( function (jqXHR, textStatus, errorThrown) {
        // $('div, button, input').css('cursor', 'auto');

        bt_execute.disabled = false;
        restoreCursor();
        window.clearInterval(timer);
        pgGetRoute.value = pgGetRoute.max;
        alert(textStatus + ' ' + errorThrown);
    });
}


function setRoutePoint(point, latLng) {

    storeValue(point, `{"lat":${latLng.lat()}, "lon": ${latLng.lng()}}`);

    if ( point === 'start' ) {
        startMarker.setPosition(latLng);
    } else if ( point === 'dest' ) {
        destinationMarker.setPosition(latLng);
    }
    
    if (courseGCLine) {
        courseGCLine.setMap(null);
    };
    courseGCLine = new google.maps.Polyline({
        geodesic: true,
        strokeColor: '#d00000',
        strokeOpacity: 1.0,
        strokeWeight: 1
    });
    courseGCLine.setMap(googleMap);
    courseGCLine.setPath([startMarker.getPosition(), destinationMarker.getPosition()]);
}


function displayRouting (data) {
    var best = data.best;

    createIsochrones(data.isochrones);

    // Sometimes the first track mark is covered by the startMarker.
    startMarker.set('time', best[0].time);

    var markerIcon = "img/marker_32x12.png";
    var redMarkerIcon = "img/marker_red_32x12.png";

    var isDisplayTracks = document.getElementById('cb_displaytracks').checked;
    if (isDisplayTracks) {
        var tracks = data.tracks;
        for ( var i = 0; i < tracks.length; i++ ) {
            var track = new google.maps.Polyline({
                geodesic: true,
                strokeColor: '#d00000',
                strokeOpacity: 1.0,
                strokeWeight: 1.5
            });
            track.setPath(tracks[i]);
            track.setMap(googleMap);
            routeTracks[i] = track;
        }
    }

    var bestPath = best.map(entry => entry.position);
    var bestTrack = new google.maps.Polyline({
        geodesic: true,
        strokeColor: '#d00000',
        strokeOpacity: 1.0,
        strokeWeight: 3
    });
    bestTrack.setPath(bestPath);
    bestTrack.setMap(googleMap);
    routeTracks[routeTracks.length] = bestTrack;
    
    for ( var i = 0; i < best.length; i++ ) {
        if ( i==0
             || best[i].penalty
             || best[i].twa != best[i-1].twa
             || best[i].sail != best[i-1].sail ) {
            var trackMarker = new google.maps.Marker({
                position: best[i].position,
                map: googleMap,
                icon: ((best[i].penalty === "sailChange") || (best[i].penalty === "tack") || (best[i].penalty === "gybe")) ?redMarkerIcon:markerIcon,
                draggable: false
            });
            addMarkerListener(trackMarker);
            addWaypointInfo(trackMarker, new Date(best[0].time), best[i]);
            trackMarkers.unshift(trackMarker);
        }
    }

    
    $("#lb_from").text(data.stats.start);
    $("#lb_duration").text(data.stats.duration);
    $("#lb_sails").text(formatSails(data));
    $("#lb_minwind").text(data.stats["min-wind"].toFixed(1) + " - " + data.stats["max-wind"].toFixed (1));
    $("#lb_mintwa").text(data.stats["min-twa"] + " - " +  data.stats["max-twa"]);
    $("#lb_polars").text(data.polars);
    $("#lb_options").text(data.options);
    // $("#lb_maxspeed").text(data.maxspeed);
}

function getURLParams () {
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
    var energy = query.get('energy');
    if (energy) {
        res.energy = energy;
    }
    var twa = query.get('twa');
    if (twa) {
        res.twa = twa;
    }
    
    return res;
}

function loadPolars (id, callback) {
    Util.doGET(`/polars/${ id }.json`,
               function (request) {
                   var data = JSON.parse(request.responseText);
                   if (data) {
                       console.log('Loaded ' + id);
                       settings.polarsId = id;
                       polars = data;
                       if (callback) {
                           callback(data);
                       }
                   } else {
                       alert("No leg info for race");
                   }
               },
               function (request) {
                   alert(request.responseText);
               });
}

function setupCanvas () {
    var mapCanvas = document.getElementById('wind-canvas');
    var mapRect = mapCanvas.getBoundingClientRect();
    geometry.width = mapRect.width * 6;
    geometry.height = mapRect.height * 6;
    mapCanvas.width = geometry.width;
    mapCanvas.height = geometry.height;
    return mapCanvas;
}

function createIsochrones (isochrones) {
    var c = new Date(currentCycle);
    for ( var i = 0; i < isochrones.length; i++ ) {
        var startSymbol = {
            path: google.maps.SymbolPath.CIRCLE,
            title: i.offset
        }
        var style = getIsoStyle(isochrones[i], 1, c);
        var isochrone = new google.maps.Polyline({
            geodesic: true,
            strokeColor: style.color,
            strokeOpacity: 0.8,
            strokeWeight: style.weight,
            icons: [{icon: startSymbol,  offset: '0%'}]
        });
        isochrone.setPath(isochrones[i].path);
        isochrone.setMap(googleMap);
        addInfo(isochrone, isochrones[i].time, isochrones[i].offset)
        routeIsochrones[i] = isochrone;
    }
}

function getIsochroneTime(isochrone) {
    var basetime = new Date(isochrone.time);
    var offset = isochrone.offset;
    return new Date(basetime - 0 + offset * 3600 * 1000);
}

function getIsoStyle (isochrone, selectedOffset, availableCycle) {
    var c = availableCycle;
    var d = new Date(isochrone.time);
    var i = getIsochroneTime(isochrone);
    var h = i.getUTCHours();
    var weight =  (h%6)?1:3;
    var color;
    if (isochrone.offset === selectedOffset) {
        color = '#ffffff';
        weight = 3;
    } else {
        if (d < c) {
            color = (h%12)?'#D0a0b0':'#D080a0';
        } else {
            color = (h%12)?'#8080a0':'#000000';
        }
    }
    return {"color": color, "weight": weight };
}

function updateIsochrones () {
    var c = new Date(currentCycle);
    for (const isochrone of routeIsochrones ) {
        var style = getIsoStyle(isochrone, ir_index.valueAsNumber, c);
        isochrone.setOptions({strokeColor: style.color, strokeWeight: style.weight});
    }
}

function clearRoute() {
    clearPath(twaPath);
    clearPath(hdgPath);
    
    for ( var i = 0; i<trackMarkers.length; i++ ) {
        trackMarkers[i].setMap(undefined);
    }
    trackMarkers = [];
    for ( var i = 0; i<routeTracks.length; i++ ) {
        routeTracks[i].setMap(undefined);
    }
    routeTracks = [];
    for ( var i = 0; i<routeIsochrones.length; i++ ) {
        routeIsochrones[i].setMap(undefined);
    }
    routeIsochrones = [];
}

function updateGetRouteProgress () {
    var pgGetRoute = $("#pg_getroute")[0];
    if ( pgGetRoute.value < pgGetRoute.max ) {
        pgGetRoute.value = pgGetRoute.value + 10;
    }
}

function addWaypointInfo(trackMarker, startTime, point) {
    var infoWindow = new google.maps.InfoWindow({
        content: makeWaypointInfo(startTime, point)
    });
    trackMarker.set('time', point.time);
    trackMarker.addListener('mouseover', function() {
        infoWindow.open(googleMap, trackMarker);
    });
    trackMarker.addListener('mouseout', function() {
        if (!this.noClose) {
            infoWindow.close();
        }
    });
    trackMarker.addListener('click', function() {
        this.noClose = true;
    });
    infoWindow.addListener('closeclick', function() {
        trackMarker.noClose = false;
    });
}

function makeWaypointInfo(startTime, point) {
    var time = new Date(point.time);
    var elapsed = Util.formatDHM((time - startTime)/1000);
    var result =  "<div style='color:#000;'>";
    result += "<p><b>T+" + elapsed + " - " + point.time + "</b></p>"
    result += "<hr>";

    result += "<p><b>Pos</b>: " + formatPointPosition(point.position) + "</p>";

    result += "<p><b>DTF</b>:" + Util.m2nm(point.dtf).toFixed(2) + "nm ";
    result += "<b> Speed</b>: " + Util.ms2knots(point.speed).toFixed(1) + "kts";
    
    result += "</p><hr>";
    
    result += "<p><b>Wind</b>: " + Util.ms2knots(point.tws).toFixed(2) + "kts / " + point.twd.toFixed(0) + "°</p>";

    result += "<p>";
    result += "<b> TWA</b>: " + point.twa.toFixed(1);
    result += "<b> HDG</b>: " + point.heading.toFixed(1) + "°  " + sailNames[point.sail];
    result += "</p>";
    result += "<p>";
    result += "<b> Penalty</b>: " + point.penalty;
    result += "<b> E</b>: " + point.energy.toFixed(0);
    result += "<b> T</b>: " + point.ptime.toFixed(0);
    result += "</p>";
    
    result += "</div>";
    
    return result;
}

function addMarkerListener(marker) {
    marker.addListener('click', function () { onMarkerClicked(marker) });
}

function clearPath (path) {
    for ( var i=0; i < path.length; i++ ) {
        path[i].setMap(null);
    }
    path = [];
}

var ortho;
function drawOrthodromic(data) {
    if (ortho) {
        ortho.setMap(null);
    }
    var start;
    if (twaAnchor) {
        start = twaAnchor.getPosition();
    } else {
        start = startMarker.position;
    }
    ortho = new google.maps.Polyline({
        geodesic: true,
        strokeColor: '#1f1f1f',
        strokeOpacity: 1,
        strokeWeight: 1,
    });
    ortho.setPath([start, data.latLng]);
    ortho.setMap(googleMap);
}

function drawTWAPath (data) {
    clearPath(twaPath);
    drawPath(twaPath, data, '#60B260');
}

function drawHDGPath (data) {
    clearPath(hdgPath);
    drawPath(hdgPath, data, '#00c2f8');
}

function drawPath (bPath, data, color) {
    var lineSymbol = {
        path: google.maps.SymbolPath.CIRCLE
    }
    for ( var i=1; i<data.length; i++ ) {
        var twaPathSegment;
        var d = new Date(data[i][0]);
        let minutes = d.getMinutes();
        if ( ( minutes % 10) == 0 ) {
            twaPathSegment = new google.maps.Polyline({
                geodesic: true,
                strokeColor: color,
                strokeOpacity: 1,
                strokeWeight: (minutes == 0) ? 4 : 2,
                icons: [{icon: lineSymbol,  offset: '100%'}]
            });
        } else {
            twaPathSegment = new google.maps.Polyline({
                geodesic: true,
                strokeColor: color,
                strokeOpacity: 1,
                strokeWeight: 2
            });
        }
        twaPathSegment.setPath([data[i-1][1], data[i][1]]);
        twaPathSegment.setMap(googleMap);
        bPath[i-1] = twaPathSegment;
    }
}

function addInfo (isochrone, time, offset) {
    isochrone.set("time", time);
    isochrone.set("offset", offset);
    isochrone.addListener('click', function () {
        var iso = isochrone;
        onSelectIsochrone(iso);
    });
}


function isochroneTime (isochrone) {
    var millis = new Date(isochrone.time).getTime();
    var date =  new Date(millis + isochrone.offset * 3600 * 1000);
    return date;
}

function getIsochroneByTime (time) {
    var refTime = new Date(time);
    var isochrones = routeInfo.isochrones;
    if (isochrones) {
        isochrones = isochrones.slice().reverse();
    }
    for (const iso of isochrones) {
        var isoT = isochroneTime(iso);
        if ( isoT >= refTime) {
            return iso;
        }
    }
    return null;
}

function redrawWindByTime (time) {
    getWind(currentCycle, time);
}

function getOffsetTime (offset, cycle=currentCycle) {
    let d = new Date(cycle);
    let time = d.getTime();
    time = time + parseInt(offset) * 3600000;
    return new Date(time);
}

function redrawWindByOffset (offset) {
    redrawWindByTime(getOffsetTime(offset).toISOString());
}

async function getWind (cycle, time) {
    let bounds = getMapBounds();

    let usedCycle = cycle;

    try {
        await windTile.update(bounds, usedCycle, time, settings.resolution);
    } catch (e1) {
        console.log(e1);
        try {
            usedCycle = availableForecastCycle();
            windTile.update(bounds, usedCycle, time, settings.resolution);
        } catch (e2) {
            console.log(e2);
        }
    }
    document.getElementById('lb_modelrun').innerHTML = usedCycle.substring(11, 13) + 'Z';
    document.getElementById('lb_index').innerHTML = time.substring(0, 16) + 'Z';
    
    var offset = (new Date(time) - new Date(usedCycle)) / 3600000;
    ir_index.value = offset;

}

async function updateWindInfo (event, getVMG) {
    let label = document.getElementById('lb_position');
    label.textContent = '[ ' + formatLatLngPosition(event.latLng) + ' ]';
    label.__pos = event.latLng;
    
    if (windTile) {
        var zoom = googleMap.getZoom();
            
        var bounds = getMapBounds();

        var curLat = event.latLng.lat();
        var curLng = event.latLng.lng();

        var destLat = destinationMarker.getPosition().lat();
        var destLng = destinationMarker.getPosition().lng();

        var dtf = Util.gcDistance({"lat": curLat, "lon": curLng},
                                  {"lat": destLat, "lon": destLng});

        var destBearing = Util.toDeg(Util.courseAngle(curLat, curLng, destLat, destLng));

        var lbDTF = document.getElementById("lb_dtf");
        lbDTF.innerText = `${dtf.toFixed(1)} | ${destBearing.toFixed(1)}°`;

        var startLat = startMarker.getPosition().lat();
        var startLng = startMarker.getPosition().lng();

        var dfs = Util.gcDistance({"lat": curLat, "lon": curLng},
                                  {"lat": startLat, "lon": startLng});

        var startBearing = Util.toDeg(Util.courseAngle(curLat, curLng, startLat, startLng));

        var lbDFS = document.getElementById("lb_dfs");
        lbDFS.innerText = `${dfs.toFixed(1)} | ${startBearing.toFixed(1)}°`;
        
        var wind = await windTile.getWind(curLat, curLng);
        if (wind) {
            var windDir = wind.direction.toFixed();
            var windSpeed = Util.ms2knots(wind.speed).toFixed(1);
            $("#lb_windatposition").text(pad0(windDir,3) + "° | " + windSpeed + "kn");

            var lbVMGUp = document.getElementById("lb_vmg_up");
            var lbVMGDown = document.getElementById("lb_vmg_down");
            var vmg = getVMG(windSpeed);

            lbVMGUp.innerHTML = vmg.up;
            lbVMGDown.innerHTML = vmg.down;
            
        } else {
            console.log(`No wind data at ${iLat}, ${iLng}`);
        }
    } else {
        console.log(`No wind data loaded`);
    }

}

function getMapBounds () {
    var bounds = googleMap.getBounds();
    var sw = bounds.getSouthWest();
    var ne = bounds.getNorthEast();
    var mapProjection = googleMap.getProjection();

    return { northEast: ne,
             southWest: sw,
             north: ne.lat(),
             south: sw.lat(),
             west: sw.lng(),
             east: ne.lng(),
             projection: mapProjection
           };
}

////////////////////////////////////////////////////////////////////////////////
/// Formatting

function formatLatLngPosition (latlng) {
    return Util.formatPosition(latlng.lat(), latlng.lng());
}

function formatPointPosition (point) {
    return Util.formatPosition(point.lat, point.lng);
}

function formatSails (data) {
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
            if ( ! m[sail]) {
                m[sail] = dt;
            } else {
                m[sail] += dt;
            }
            t0 = t1;
            sail = s.sail;
        }
    }

    var dt = t1 - t0;
    if ( ! m[sail]) {
        m[sail] = dt;
    } else {
        m[sail] += dt;
    }

    dt = t1 - start;

    var result = "";
    for (const e in m) {
        let sail  = sailNames[e];
        if (result) {
            result = result + " - " + sail + ":" + (m[e]/dt*100).toFixed() + "%" ;
        } else {
            result = sail + ":" + (m[e]/dt*100).toFixed() + "%";
        }
    }
    return result;
}

export {
    // alphabetical
    courseGCLine,
    destinationMarker,
    drawTWAPath,
    drawHDGPath,
    formatLatLngPosition,
    getCurrentCycle,
    getIsochroneByTime,
    getRoute,
    getURLParams,
    getValue,
    googleMap,
    loadPolars,
    makeQuery,
    mapEvent,
    onMarkerClicked,
    onSetResolution,
    onSetGFSMode,
    setPolars,
    getPolars,
    getSailnames,
    restoreCursor,
    setSailnames,
    setBusyCursor,
    settings,
    setResolution,
    setDuration,
    setRoutePoint,
    setUp,
    startMarker,
    storeValue,
    twaAnchor,
    updateMap,
    windTile
}

/// EOF
////////////////////////////////////////////////////////////////////////////////
