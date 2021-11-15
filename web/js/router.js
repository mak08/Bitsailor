////////////////////////////////////////////////////////////////////////////////
/// Bitsailor Router UI

import * as Util from './Util.js';


var googleMap = null;
var mapEvent;

// Bounds and width from the map are kept here for convenience
var geometry = {};

// Number of wind arrows
var xSteps = 70;
var ySteps = 40;

var east;

// Time index
var ir_index;

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
var forecastData = {};
var forecastCycle;
var windData = null;
var currentRouting = {};
var twaPath = [];
var hdgPath = [];

function setUp (getVMG) {
    
    setupColors();
    
    // Create a map object, and include the MapTypeId to add
    // to the map type control.
    var mapProp = {
        center:new google.maps.LatLng(49.187, 8.473),
        zoom:5,
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
    google.maps.event.addListener(googleMap, 'mousemove', function (event) {
        return updateWindInfo(event, getVMG);
    });
    google.maps.event.addListener(googleMap, 'click', getTWAPath);
    google.maps.event.addListener(googleMap, 'click', drawOrthodromic);
    
    // Connect button events
    $("#bt_getroute").click(getRoute);
    
    $("#bt_inc").click(onAdjustIndex);
    $("#bt_dec").click(onAdjustIndex);
    $("#bt_inc6").click(onAdjustIndex);
    $("#bt_dec6").click(onAdjustIndex);
    $("#ir_index").change(onAdjustIndex);
    
    $("#cb_startdelayed").click(onDelayedStart);
    $("#tb_starttime").change(onSetParameter);
    
    $("#cb_manualcycle").click(onManualCycle);
    $("#tb_cycledate").change(onSetParameter);
    $("#sel_cyclehour").change(onSetParameter);
    
    // Connect option selectors
    $("#sel_polars").change(onSetParameter);
    $("#sel_forecastbundle").change(onSetParameter);
    $("#sel_duration").change(onSetParameter);
    $("#cb_displaywind").change(onDisplaywind);
    $("#cb_displaytracks").change(onDisplayTracks);
    $("#rb_crosshair").change(function (event) { onCursorSelect(event, 'crosshair'); });
    $("#rb_default").change(function (event) { onCursorSelect(event, 'default'); });
    
    // Tracks & Isochrones display is handled by the client directly
    $("#cb_tracks").change(onSetClientParameter);
    $("#cb_isochrones").change(onSetClientParameter);
    
    // Disable default contextmenu
    window.oncontextmenu = function (event) { event.preventDefault() }; 
    
    // Connect menu events
    $("#bt_setstart" ).click(function () { onContextMenu('start') });
    $("#bt_setdest"  ).click(function () { onContextMenu('dest') });
    $("#bt_ltpmark"  ).click(function () { onContextMenu('ltp') });
    $("#bt_ltsmark"  ).click(function () { onContextMenu('lts') });
    
    var mapMenu = $("#mapMenu")[0];
    mapMenu.onmouseleave = onMapMenuMouseLeave;
    
    ir_index = $("#ir_index")[0];
    
    startMarker = initMarker('start', 'Start', 'img/start_45x32.png', 1, 45);
    destinationMarker = initMarker('dest', 'Destination',  'img/finish_32x20.png', 1, 32);
    
    setupCanvas();
    
    google.maps.event.addListenerOnce(googleMap, 'idle', function(){
        updateMap();
    });
    
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

function updateMap () {
    if ((googleMap.getMapTypeId() == google.maps.MapTypeId.ROADMAP)
        || (googleMap.getMapTypeId() == google.maps.MapTypeId.TERRAIN)) {
        if ( googleMap.zoom < 6 ) {
            googleMap.setMapTypeId(google.maps.MapTypeId.ROADMAP);
        } else {
            googleMap.setMapTypeId(google.maps.MapTypeId.TERRAIN);
        }
    }
    var bounds = getMapBounds();
    var label = "⌊" + formatLatLngPosition(bounds.southWest) + " \\ " +  formatLatLngPosition(bounds.northEast) + "⌉";
    $("#lb_map_bounds").text("Kartenausschnitt: " + label);
    if (forecastData.basetime) {
        redrawWindByOffset(forecastData.basetime, ir_index.value);
    } else {
        var baseTime = availableForecastCycle();
        redrawWindByOffset(baseTime, ir_index.value);
    }
}

function availableForecastCycle (d=new Date()) {
    // Display cycle only when it's (supposed to be) fully available
    var availDate = d - 300 * 60 * 1000;
    var fc = truncate(availDate, 6 * 3600 * 1000);
    return new Date(fc).toISOString();
}

function currentCycle (d=new Date()) {
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

function onSelectIsochrone (isochrone) {
    var baseTime = isochrone.get('time');
    var offset = isochrone.get('offset');
    $("#ir_index")[0].value = offset;
    updateIsochrones();
    redrawWindByOffset(baseTime, offset);
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
    displayRouting(currentRouting);
}

function onCursorSelect (event, type) {
    googleMap.setOptions({draggableCursor:type});
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
    redrawWindByOffset(forecastData.basetime, ir_index.value);
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
    setParameter('starttime', dateInput.value);
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
        "/function/vh.setParameter",
        function (request) {
            console.log(request.responseText);
            redrawWindByOffset(cycle, 0);
        },
        function (request) {
            alert('Could not set cycle: ' + request.textStatus + ' ' + request.errorThrown + ' ' + request.responseText);
        },
        params
    );
}

function onSetClientParameter (event) {
    if ( event.currentTarget === 'foo' ) {
    } else {
        alert('later.');
    }
}

function onSetParameter (event) {
    var paramName = event.currentTarget.name;
    var paramValue;
    // tb starttime has a 'checked' field but we don't want to use it.
    if ( paramName === 'starttime' ) {
        paramValue = event.currentTarget.value;
        setParameter(paramName, paramValue);
    } else if ( paramName === 'cycledate' || paramName === 'cyclehour' ) {
        var manualCycle = document.getElementById("cb_manualcycle");
        if ( manualCycle.checked ) {
            var dateInput =  document.getElementById("tb_cycledate");
            var hourInput =  document.getElementById("sel_cyclehour");
            paramName = 'cycle';
            paramValue = dateInput.value + "T" + pad0(hourInput.value) + ":00:00Z";
            setParameter(paramName, paramValue);
            redrawWindByOffset(paramValue, "0");
        }
    } else {
        // default: if there is a 'checked' field use it, otherwise use the value field.
        paramValue = event.currentTarget.checked;
        if ( paramValue === undefined ) {
            paramValue = event.currentTarget.value;
        }
        setParameter(paramName, paramValue);
    }

}

function setParameter (paramName, paramValue) {
    $.ajax({
        url: "/function/vh.setParameter" + "?name=" + paramName + "&value=" + paramValue,
        dataType: 'json'
    }).done( function(data, status, xhr ) {
        console.log(`Set ${paramName}=${paramValue}`);
    }).fail( function (jqXHR, textStatus, errorThrown) {
        alert('Could not set ' + paramName + ': ' + textStatus + ' ' + errorThrown);
    });
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
    
    redrawWindByTime(time, baseTime);
}


//////////////////////////////////////////////////////////////////////
/// XHR requests

function getRoute () {
    var bt_execute=$("#bt_getroute")[0];
    bt_execute.disabled = true;
    
    var mapMenu=$("#mapMenu")[0];
    var windowEvent = window.event;
    mapMenu.style.display = "none";
    var that = this;
    var pgGetRoute = $("#pg_getroute")[0];
    pgGetRoute.value = 5;
    var selDuration = $("#sel_duration")[0];
    var duration = selDuration.value;
    var timer = window.setInterval(updateGetRouteProgress, 10 * duration);

    // $('div, button, input').css('cursor', 'wait');

    $.ajax({
        url: "/function/vh.getRoute",
        dataType: 'json'
    }).done( function (data) {
        // $('div, button, input').css('cursor', 'auto');

        // Remember routing data
        currentRouting = data;

        // Reset timer
        window.clearInterval(timer);
        pgGetRoute.value = pgGetRoute.max;

        // Display new data
        clearRoute();
        displayRouting(data);
        bt_execute.disabled = false;
        
    }).fail( function (jqXHR, textStatus, errorThrown) {
        // $('div, button, input').css('cursor', 'auto');

        bt_execute.disabled = false;
        window.clearInterval(timer);
        pgGetRoute.value = pgGetRoute.max;
        alert(textStatus + ' ' + errorThrown);
    });
}


function setRoutePoint(point, latlng) {
    var lat =  latlng.lat();
    var lng =  latlng.lng();
    var that = this;
    $.ajax({
        url: "/function/vh.setRoute"
            + "?pointType=" + point
            + "&lat=" + lat
            + "&lng=" + lng,
        dataType: 'json'
    }).done( function(data) {
        // alert(point + " at " + lat + ", " + lng + " " + JSON.stringify(data));
        if ( point === 'start' ) {
            updateStartPosition(lat, lng);
        } else if ( point === 'dest' ) {
            destinationMarker.setPosition(latlng);
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
        
    }).fail( function (jqXHR, textStatus, errorThrown) {
        alert("Could not set " + point + ': ' + textStatus + ' ' + errorThrown);
    });
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
        var trackMarker = new google.maps.Marker({
            position: best[i].position,
            map: googleMap,
            icon: (best[i].penalty === "Sail Change")?redMarkerIcon:markerIcon,
            draggable: false
        });
        addMarkerListener(trackMarker);
        addWaypointInfo(trackMarker, new Date(best[0].time), best[i]);
        trackMarkers[i] = trackMarker;
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

function removeSession () {
    $.ajax({
        url: "/function/vh.removeSession",
        dataType: 'json'
    }).done( function(result, status, xhr) {
        console.log("Removed " + result); 
    }).fail( function (jqXHR, textStatus, errorThrown) {
        alert(textStatus + ' ' + errorThrown);
    });
}

function positionFromDocumentURL () {
    var query = new URL(document.URL).search.split('&');
    var lat;
    var lon;
    for (const e of query) {
        var p = e.split('=');
        if (p[0] == 'slat') {
            lat = p[1].split('d')[0];
        } else if (p[0] == 'slon') {
            lon = p[1].split('d')[0];
        }
    }
    if ( lat && lon ) {
        return {"lat": lat, "lon": lon};
    } else {
        return null;
    }
}

function loadPolars (id) {
    Util.doGET(`/polars/${ id }.json`,
               function (request) {
                   var data = JSON.parse(request.responseText);
                   if (data) {
                       console.log('Loaded ' + id);
                       polars = data;
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
}

function updateStartPosition (lat, lng) {
    var latLng = new google.maps.LatLng(lat, lng);
    startMarker.setPosition(latLng);
}

function createIsochrones (isochrones) {
    var c = new Date(currentCycle());
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
    var c = new Date(currentCycle());
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
    result += "<b> HDG</b>: " + point.heading.toFixed(1) + "°  " + point.sail;
    result += "</p>";
    result += "<p>";
    result += "<b> Penalty</b>: " + point.penalty;
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
        if ( d.getMinutes() === 0 ) {
            twaPathSegment = new google.maps.Polyline({
                geodesic: true,
                strokeColor: color,
                strokeOpacity: 1,
                strokeWeight: 4,
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
    var isochrones = currentRouting.isochrones;
    for (const iso of isochrones) {
        var isoT = isochroneTime(iso);
        if ( isoT <= refTime) {
            return iso;
        }
    }
    return null;
}

function getTWAPath (event) {
    if ( twaAnchor ) {
        var latA = twaAnchor.getPosition().lat();
        var lngA = twaAnchor.getPosition().lng();
        var time = twaAnchor.get('time');
        var lat = event.latLng.lat();
        var lng = event.latLng.lng();
        var isochrone = getIsochroneByTime(time);
        var baseTime;
        if (isochrone) {
            baseTime = isochrone.time;
        } else {
            baseTime = availableForecastCycle();
        }
        $.ajax({
            url: "/function/vh.getTWAPath?basetime=" + baseTime + "&time=" + time + "&latA=" + latA + "&lngA=" + lngA + "&lat=" + lat + "&lng=" + lng,
            dataType: 'json'
        }).done( function(data) {
            drawTWAPath(data.twapath);
            drawHDGPath(data.hdgpath);
            $("#lb_twa").text(data.twa);
            $("#lb_twa_heading").text(data.heading);
        }).fail( function (jqXHR, textStatus, errorThrown) {
            alert(textStatus + ' ' + errorThrown);
        });
    } else {
        console.log('No TWA anchor');
    }
}

function redrawWindByTime (time, baseTime) {
    getWind("basetime=" + baseTime + "&" + "time=" + time);
}

function redrawWindByOffset (basetime, offset) {
    var timeSpec = "basetime=" + basetime + "&" + "offset=" + offset;
    getWind(timeSpec);
}

function getWind (timeSpec) {
    var bounds = getMapBounds();
    
    var lat0 = bounds.north + ((bounds.north - bounds.south) / ySteps)/2;
    var lon0 = bounds.east + (Util.arcLength(bounds.west, bounds.east) / xSteps)/2;
    var ddx = (Util.arcLength(bounds.west, bounds.east)/xSteps).toFixed(8);
    var ddy = ((bounds.north-bounds.south)/ySteps).toFixed(8);
    // $('div, button, input').css('cursor', 'wait');
    $.ajax({
        url: "/function/vh.getWind"
            + "?" + timeSpec
            + "&north=" + lat0.toFixed(6)
            + "&south=" + bounds.south.toFixed(6)
            + "&west=" + bounds.west.toFixed(6)
            + "&east=" + lon0.toFixed(6)
            + "&ddx=" + ddx
            + "&ddy=" + ddy
            + "&xSteps=" + xSteps
            + "&ySteps=" + ySteps,
        dataType: 'json'
    }).done( function(data) {
        // $('div, button, input').css('cursor', 'auto');
        forecastData = data;
        drawWind(data)
    }).fail( function (jqXHR, textStatus, errorThrown) {
        // $('div, button, input').css('cursor', 'auto');
        alert("Could not get wind data:" + textStatus + ' ' + errorThrown);
        console.log("Could not get wind data:" + textStatus + ' ' + errorThrown);
    });
}

function drawWind (data) {

    // Update time
    forecastCycle = data.basetime;
    $("#lb_modelrun").text(data.cycle);
    $("#lb_index").text(data.time);
    $("#lb_fcmax").text(' ' + data.maxoffset + 'hrs');

    var offset = (new Date(data.time) - new Date(data.basetime)) / 3600000;
    ir_index.value = offset;

    // Keep new wind data in global var for displaying wind data at mouse position
    windData = data.data;
    
    var mapCanvas = document.getElementById('wind-canvas');
    var ctx = mapCanvas.getContext("2d");
    ctx.globalAlpha = 0.6;
    ctx.clearRect(0, 0, geometry.width, geometry.height);

    // Needed to transform Map points to Canvas points (origin (N,W) = (0,0))
    var bounds = getMapBounds();

    var mapProjection = googleMap.getProjection();
    var nwLatLng =  new google.maps.LatLng(bounds.north, bounds.west);
    var seLatLng =  new google.maps.LatLng(bounds.south, bounds.east);
    var nwPoint = mapProjection.fromLatLngToPoint(nwLatLng);
    var sePoint = mapProjection.fromLatLngToPoint(seLatLng);
    var mapWidth = (sePoint.x - nwPoint.x);
    var mapHeight = (sePoint.y - nwPoint.y);

    // Wind values are at evenly distributed lat/lng values
    var width = (bounds.east-bounds.west);
    var height = (bounds.south-bounds.north);
    var dlng = (width/xSteps);
    var dlat = (height/ySteps);

    // Shift Lat/Lng by 1/2 delta to center wind arrows on screen.
    for ( var lat = bounds.north+dlat/4, y=0; y < ySteps; lat += dlat, y++ ) {
        for ( var lng = bounds.west+dlng/4, x=0; x < xSteps; lng += dlng, x++ ) {
            var latLng = new google.maps.LatLng(lat, lng);
            var mapPoint = mapProjection.fromLatLngToPoint(latLng);
            var pointX = (mapPoint.x - nwPoint.x) / mapWidth * ctx.canvas.width;
            var pointY = (mapPoint.y - nwPoint.y) / mapHeight * ctx.canvas.height;
            drawWindArrow(ctx, pointX, pointY, windData[y][x][0], windData[y][x][1]);
        }
    }
}

function updateWindInfo (event, getVMG) {
    if (windData) {
        var zoom = googleMap.getZoom();
        
        $("#lb_position").text(formatLatLngPosition(event.latLng));
        
        var bounds = getMapBounds();

        var curLat = event.latLng.lat();
        var curLng = event.latLng.lng();

        var destLat = destinationMarker.getPosition().lat();
        var destLng = destinationMarker.getPosition().lng();

        var dtf = Util.gcDistance({"lat": curLat, "lon": curLng},
                                  {"lat": destLat, "lon": destLng});

        var lbDTF = document.getElementById("lb_dtf");
        lbDTF.innerText = dtf.toFixed(2);

        var startLat = startMarker.getPosition().lat();
        var startLng = startMarker.getPosition().lng();

        var dfs = Util.gcDistance({"lat": curLat, "lon": curLng},
                                  {"lat": startLat, "lon": startLng});

        var lbDFS = document.getElementById("lb_dfs");
        lbDFS.innerText = dfs.toFixed(2);
        
        
        var iLat = Math.round((curLat - bounds.north) / (bounds.south - bounds.north) * ySteps);
        var iLng = Math.round(Util.arcLength(bounds.west, curLng) / Util.arcLength(bounds.west, bounds.east) * xSteps);

        var wind = windData[iLat][iLng];
        if (wind) {
            var windDir = wind[0].toFixed(0);
            var windSpeed = Util.ms2knots(windData[iLat][iLng][1]).toFixed(1);
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
    
    return { northEast: ne,
             southWest: sw,
             north: ne.lat(),
             south: sw.lat(),
             west: sw.lng(),
             east: ne.lng()
           };
}

function drawWindArrow(ctx, x, y, direction, speed) {
    direction = direction + 90;
    if (direction > 360) {
        direction = direction - 360;
    }
    ctx.fillStyle = colors[ms2bf(speed)];
    ctx.strokeStyle = colors[ms2bf(speed)];
    ctx.save();
    ctx.translate(x , y);
    ctx.rotate((direction*Math.PI/180));
    var scale = (speed>0)?0.4 + speed/30:0;
    ctx.scale(scale, scale);
    ctx.beginPath();
    ctx.moveTo(-0, 0);
    ctx.lineTo(-15 * 6, 12 * 6);
    ctx.lineTo(18 * 6, 0);
    ctx.lineTo(-15 * 6, -12 * 6);
    ctx.closePath()
    ctx.fill();
    ctx.stroke();
    ctx.restore();
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
        if (result) {
            result = result + " - " + e + ":" + (m[e]/dt*100).toFixed() + "%" ;
        } else {
            result = e + ":" + (m[e]/dt*100).toFixed() + "%";
        }
    }
    return result;
}

function pickleSession () {
    // Attempt to tell tell the server that the client is gone.
    removeSession();
    console.log('Goodbye');
}

window.addEventListener("beforeunload", function (event) {
    pickleSession();
});

export {
    // alphabetical
    courseGCLine,
    destinationMarker,
    googleMap,
    loadPolars,
    onMarkerClicked,
    polars,
    positionFromDocumentURL,
    setParameter,
    setRoutePoint,
    setUp,
    startMarker,
    updateStartPosition
}

/// EOF
////////////////////////////////////////////////////////////////////////////////
