////////////////////////////////////////////////////////////////////////////////
/// Sailsphere Router UI

import * as Util from './Util.js';

( function () {

    var raceId = "default";
    
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
    var twaAnchor = {};
    var twaTime = {};
    
    var courseGCLine = null;
    var routeTracks = [];
    var routeIsochrones = [];
    var trackMarkers = [];

    var forecastData = {};
    var forecastCycle;
    var windData = [];
    var currentRouting = {};
    var twaPath = [];

    function setUp () {
        
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
        google.maps.event.addListener(googleMap, 'mousemove', updateWindInfo);
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
        $("#cb_minwind").change(onSetParameter);
        $("#cb_hidewind").change(onHideWind);
        
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

        startMarker = initMarker('start', 'Start', 'img/start_45x32.png');
        destinationMarker = initMarker('dest', 'Destination',  'img/finish_32x20.png');

        setupCanvas();
        
        google.maps.event.addListenerOnce(googleMap, 'idle', function(){
            updateMap();
        });
        
        getLegInfo()
        getSession();

    }

    function initMarker (type, title, icon) {
        var marker = new google.maps.Marker({
            position: {"lat": 0, "lng": 0},
            map: googleMap,
            title: title,
            icon: icon,
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
            redrawWindByTime(baseTime, ir_index.value);
        }
    }

    function availableForecastCycle (d=new Date()) {
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

    function onHideWind (event) {
        var cbHideWind = $("#cb_hidewind")[0];
        if (cbHideWind.checked) {
            $("#wind-canvas").hide();
        } else {
            $("#wind-canvas").show();
        }
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
        if (event.target.checked === true) {
            var d = new Date();
            var isoDate = d.toISOString().substring(0,16);
            var dateInput =  $("#tb_starttime")[0];
            dateInput.value = isoDate;
        } else {
            $("#tb_starttime")[0].value = null;
            $.ajax({
                // No paramValue == reset (value defaults to nil)
                url: "/function/vh:setParameter" + "?name=" + 'starttime',
                dataType: 'json'
            }).done( function(data) {
                console.log(data);
            }).fail( function (jqXHR, textStatus, errorThrown) {
                alert('Could not set ' + paramName + ': ' + textStatus + ' ' + errorThrown);
            });
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
        var dateInput =  $("#tb_cycledate")[0];
        var hourInput =  $("#sel_cyclehour")[0];
        var d = new Date();
        var isoDate = d.toISOString().substring(0,10);
        dateInput.value = isoDate;
        var hour = truncate(d.getUTCHours(), 6);
        hourInput.value = hour;
        var valueSpec = "";
        if (document.getElementById("cb_manualcycle").checked) {
            valueSpec = "&value=" + getManualCycle();
        }
        $.ajax({
            // No paramValue == reset (value defaults to nil)
            url: "/function/vh:setParameter" + "?name=" + 'cycle' + valueSpec,
            dataType: 'json'
        }).done( function(data) {
            console.log(data);
            redrawWindByOffset(getManualCycle(), 0);
        }).fail( function (jqXHR, textStatus, errorThrown) {
            alert('Could not set cycle: ' + textStatus + ' ' + errorThrown);
        });
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
        } else if ( paramName === 'cycledate' || paramName === 'cyclehour' ) {
            var dateInput =  $("#tb_cycledate")[0];
            var hourInput =  $("#sel_cyclehour")[0];
            paramName = 'cycle';
            paramValue = dateInput.value + "T" + pad0(hourInput.value) + ":00:00Z";
            redrawWindByOffset(paramValue, "0");
        } else {
            // default: if there is a 'checked' field use it, otherwise use the value field.
            paramValue = event.currentTarget.checked;
            if ( paramValue === undefined ) {
                paramValue = event.currentTarget.value;
            }
        }
        $.ajax({
            url: "/function/vh:setParameter" + "?name=" + paramName + "&value=" + paramValue,
            dataType: 'json'
        }).done( function(data, status, xhr ) {
            if ( paramName === "forecastbundle" ) {
                var irIndex = $("#ir_index")[0];
                var lbFCMax = $("#lb_fcmax")[0];
                irIndex.value = 0;
                if ( paramValue === "DWD-ICON-BUNDLE" ) {
                    irIndex.max = 72;
                    lbFCMax.innerText = 72;
                } else if ( paramValue === "NOAA-BUNDLE" ) {
                    irIndex.max = 240;
                    lbFCMax.innerText = 240;
                }
                redrawWindByOffset(forecastData.basetime, irIndex.value);
            }
            console.log("OK");
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
        twaAnchor = marker.getPosition();
        twaTime = marker.get('time');
        redrawWindByTime(twaTime);
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
            url: "/function/vh:getRoute",
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
            url: "/function/vh:setRoute"
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

        // Sometimes the first track mark is covered by the startMarker.
        startMarker.set('time', best[0].time);

        var markerIcon = "img/marker_32x12.png";
        var redMarkerIcon = "img/marker_red_32x12.png";
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

        createIsochrones(data.isochrones);
        
        $("#lb_from").text(JSON.stringify(data.stats.start));
        $("#lb_duration").text(JSON.stringify(data.stats.duration));
        $("#lb_sails").text(formatSails(data));
        $("#lb_minwind").text(Util.roundTo(data.stats["min-wind"], 1) + " - " + Util.roundTo(data.stats["max-wind"], 1));
        $("#lb_mintwa").text(data.stats["min-twa"] + " - " +  data.stats["max-twa"]);
        $("#lb_polars").text(data.polars);
        $("#lb_maxspeed").text(data.maxspeed);
    }


    function getSession () {
        $.ajax({
            url: "/function/vh:getSession",
            dataType: 'json'
        }).done( function(routing, status, xhr) {
            
            updateStartPosition(routing.start.lat, routing.start.lng);
            
            var start  = new google.maps.LatLng(routing.start.lat, routing.start.lng);
            googleMap.setCenter(start);
            
            var dest  = new google.maps.LatLng(routing.dest.lat, routing.dest.lng);
            destinationMarker.setPosition(dest);
            
            var selForecast = $("#sel_forecastbundle")[0];
            var irIndex = $("#ir_index")[0];
            var lbFCMax = $("#lb_fcmax")[0];
            
            var startTime = routing.starttime;
            var cbStartDelayed = $("#cb_startdelayed")[0];
            if ( startTime != false && startTime != 'NIL' ) {
                cbStartDelayed.checked = true;
                $("#tb_starttime")[0].value = startTime;
                
            } else {
                cbStartDelayed.checked = false;
            }
            
            var polars = routing.polars;
            var selPolars = $("#sel_polars")[0];
            selPolars.value = polars;
            
            var duration = routing.stepmax/3600;
            var selDuration = $("#sel_duration")[0];
            selDuration.value = duration;
            
            var minWind = routing.minwind;
            var cbMinWind = $("#cb_minwind")[0];
            cbMinWind.checked = minWind;
            
            courseGCLine = new google.maps.Polyline({
                geodesic: true,
                strokeColor: '#ff0000',
                strokeOpacity: 1.0,
                strokeWeight: 1
            });
            courseGCLine.setMap(googleMap);
            courseGCLine.setPath([startMarker.getPosition(), destinationMarker.getPosition()]);

        }).fail( function (jqXHR, textStatus, errorThrown) {
            alert(textStatus + ' ' + errorThrown);
        });
    }

    function getLegInfo () {
        $.ajax({
            url: "/function/vh:getLegInfo",
            dataType: 'json'
        }).done( function(leg, status, xhr) {
            if (leg) {
                var checkpoints = leg.checkpoints;
                var markStbd = 'img/mark_green.png';
                var markPort = 'img/mark_red.png';
                
                startMarker.setPosition( {"lat": leg.start.lat, "lng": leg.start.lon});
                destinationMarker.setPosition( {"lat": leg.end.lat, "lng": leg.end.lon});
                
                for (const c of checkpoints) {
                    var mark = new google.maps.Marker({
                        position: {"lat": c.start.lat, "lng": c.start.lon},
                        map: googleMap,
                        icon: (c.side=='port')?markPort:markStbd,
                        title: c.group + "-" + c.id + " " + c.name,
                        draggable: false
                    });
                    mark.addListener('click', function () { onMarkerClicked(mark) });
                }
                if (leg.ice_limits) {
                    var iceLimit = [];
                    for (const p of leg.ice_limits.south) {
                        iceLimit.push({"lat": p.lat, "lng": p.lon});
                    }
                    var iceLine = new google.maps.Polyline({
                        geodesic: false,
                        strokeColor: '#ff0000',
                        strokeOpacity: 1.0,
                        strokeWeight: 1
                    });
                    iceLine.setMap(googleMap);
                    iceLine.setPath(iceLimit);
                }
            } else {
                alert("No leg info for race");
            }
        }).fail( function (jqXHR, textStatus, errorThrown) {
            alert(textStatus + ' ' + errorThrown);
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
        var startSymbol = {
            path: google.maps.SymbolPath.CIRCLE
        }
        for ( var i = 0; i < isochrones.length; i++ ) {
            var style = getIsoStyle(isochrones[i], 1);
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
    
    function getIsoStyle (isochrone, selectedOffset) {
        var d = getIsochroneTime(isochrone);
        var h = d.getUTCHours();
        var weight =  (h%6)?1:3;
        var color;
        if (isochrone.offset === selectedOffset) {
            color = '#ffffff';
            weight = 3;
        } else {
            if (h%6 === 4) {
                color = '#d00000';
            } else {
                color = (h%12)?'#8080a0':'#000000';
            }
        }
        return {"color": color, "weight": weight };
    }

    function updateIsochrones () {
        for (const isochrone of routeIsochrones ) {
            var style = getIsoStyle(isochrone, ir_index.valueAsNumber);
            isochrone.setOptions({strokeColor: style.color, strokeWeight: style.weight});
        }
    }
    
    function clearRoute() {
        clearTWAPath();
        
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

        result += "<p><b>DTF</b>:" + Util.roundTo(Util.m2nm(point.dtf), 2) + "nm ";
        result += "<b> Speed</b>: " + Util.roundTo(Util.ms2knots(point.speed), 1) + "kts";
        
        result += "</p><hr>";
        
        result += "<p><b>Wind</b>: " + Util.roundTo(Util.ms2knots(point.tws), 2) + "kts / " + Util.roundTo(point.twd, 0) + "°</p>";

        result += "<p>";
        result += "<b> TWA</b>: " + Util.roundTo(point.twa, 1);
        result += "<b> HDG</b>: " + Util.roundTo(point.heading, 1) + "°  " + point.sail;
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
    
    function clearTWAPath() {
        for ( var i=0; i<twaPath.length; i++ ) {
            twaPath[i].setMap(null);
        }
        twaPath = [];
    }

    var ortho;
    function drawOrthodromic(data) {
        if (ortho) {
            ortho.setMap(null);
        }
        var start;
        if (twaAnchor.lat) {
            start = twaAnchor;
        } else {
            start = startMarker.position;
        }
        ortho = new google.maps.Polyline({
            geodesic: true,
            strokeColor: '#0f0f0f',
            strokeOpacity: 1,
            strokeWeight: 2,
        });
        ortho.setPath([start, data.latLng]);
        ortho.setMap(googleMap);
    }
    
    function drawTWAPath(data) {
        clearTWAPath();
        var color = '#00a0c0';
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
                    strokeWeight: 2,
                    icons: [{icon: lineSymbol,  offset: '100%'}]
                });
            } else {
                twaPathSegment = new google.maps.Polyline({
                    geodesic: true,
                    strokeColor: color,
                    strokeOpacity: 1,
                    strokeWeight: 1
                });
            }
            twaPathSegment.setPath([data[i-1][1], data[i][1]]);
            twaPathSegment.setMap(googleMap);
            twaPath[i-1] = twaPathSegment;
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
    
    function getTWAPath(event) {
        var latA, lngA, time ;
        if ( twaAnchor.lat === undefined || twaTime === undefined ) {
            latA = startMarker.getPosition().lat();
            lngA = startMarker.getPosition().lng();
            time = $('#lb_index').text();
        } else {
            latA = twaAnchor.lat();
            lngA = twaAnchor.lng();
            time = twaTime;
        }
        var lat = event.latLng.lat();
        var lng = event.latLng.lng();
        var baseTime;
        if (document.getElementById("cb_manualcycle").checked) {
            baseTime = getManualCycle();
        } else {
            baseTime  = availableForecastCycle();
        }
        $.ajax({
            url: "/function/vh:getTWAPath?basetime=" + baseTime + "&time=" + time + "&latA=" + latA + "&lngA=" + lngA + "&lat=" + lat + "&lng=" + lng,
            dataType: 'json'
        }).done( function(data) {
            drawTWAPath(data.path);
            $("#lb_twa").text(data.twa);
            $("#lb_twa_heading").text(data.heading);
        }).fail( function (jqXHR, textStatus, errorThrown) {
            alert(textStatus + ' ' + errorThrown);
        });
    }
    
    function redrawWindByTime (time) {
        var baseTime;
        if (document.getElementById("cb_manualcycle").checked) {
            baseTime = getManualCycle();
        } else {
            baseTime  = availableForecastCycle();
        }
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
        var ddx = Util.roundTo(Util.arcLength(bounds.west, bounds.east)/xSteps, 8);
        var ddy = Util.roundTo((bounds.north-bounds.south)/ySteps, 8);
        // $('div, button, input').css('cursor', 'wait');
        $.ajax({
            url: "/function/vh:getWind"
                + "?" + timeSpec
                + "&north=" + Util.roundTo(lat0, 6)
                + "&south=" + Util.roundTo(bounds.south, 6)
                + "&west=" + Util.roundTo(bounds.west, 6)
                + "&east=" + Util.roundTo(lon0, 6)
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
        var dlng = Util.roundTo(width/xSteps, 8);
        var dlat = Util.roundTo(height/ySteps, 8);

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

    function updateWindInfo (event) {
        
        var zoom = googleMap.getZoom();
        
        $("#lb_position").text(formatLatLngPosition(event.latLng));
        
        var bounds = getMapBounds();

        var iLat = Math.round((event.latLng.lat() - bounds.north) / (bounds.south - bounds.north) * ySteps);
        var iLng = Math.round(Util.arcLength(bounds.west, event.latLng.lng()) / Util.arcLength(bounds.west, bounds.east) * xSteps);
        var windDir = Util.roundTo(windData[iLat][iLng][0], 0);
        var windSpeed = Util.roundTo(Util.ms2knots(windData[iLat][iLng][1]), 1);
        $("#lb_windatposition").text(windDir + "° | " + windSpeed + "kn");
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
        var result = "";
        for (const sail of data.stats.sails) {
            if (result) {
                result = sail + "," + result;
            } else {
                result = sail;
            }
        }
        return result;
    }

    $(document).ready(setUp);
    
}) ()

/// EOF
////////////////////////////////////////////////////////////////////////////////
