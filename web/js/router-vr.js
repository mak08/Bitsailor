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
    var twaAnchor = undefined;
    
    var courseGCLine = null;
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
        var mapDiv = document.getElementById("map");
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
        document.getElementById("bt_getroute").addEventListener("click", getRoute);

        document.getElementById("bt_inc").addEventListener("click", onAdjustIndex);
        document.getElementById("bt_dec").addEventListener("click", onAdjustIndex);
        document.getElementById("bt_inc6").addEventListener("click", onAdjustIndex);
        document.getElementById("bt_dec6").addEventListener("click", onAdjustIndex);
        document.getElementById("ir_index").addEventListener("change", onAdjustIndex);
        
        document.getElementById("cb_startdelayed").addEventListener("click", onDelayedStart);
        document.getElementById("tb_starttime").addEventListener("change", onSetParameter);
        
        document.getElementById("cb_manualcycle").addEventListener("click", onManualCycle);
        document.getElementById("tb_cycledate").addEventListener("change", onSetParameter);
        document.getElementById("sel_cyclehour").addEventListener("change", onSetParameter);
        
        // Connect option selectors
        document.getElementById("sel_duration").addEventListener("change", onSetParameter);
        document.getElementById("cb_displaywind").addEventListener("change", onDisplaywind);
        document.getElementById("cb_displaytracks").addEventListener("change", onDisplayTracks);
        document.getElementById("rb_crosshair").addEventListener("change", function (event) { onCursorSelect(event, 'crosshair'); });
        document.getElementById("rb_default").addEventListener("change", function (event) { onCursorSelect(event, 'default'); });
        
        // Disable default contextmenu
        window.oncontextmenu = function (event) { event.preventDefault() }; 
        
        // Connect menu events
        document.getElementById("bt_setstart" ).addEventListener("click", function () { onContextMenu('start') });
        document.getElementById("bt_setdest"  ).addEventListener("click", function () { onContextMenu('dest') });
        
        var mapMenu = document.getElementById("mapMenu");
        mapMenu.onmouseleave = onMapMenuMouseLeave;
        
        ir_index = document.getElementById("ir_index");

        startMarker = initMarker('start', 'Start', 'img/start_45x32.png', 1, 45);
        destinationMarker = initMarker('dest', 'Destination',  'img/finish_32x20.png', 1, 32);

        setupCanvas();
        
        google.maps.event.addListenerOnce(googleMap, 'idle', function(){
            updateMap();
        });
        
        getRaceInfo()
        getSession();

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
        var mapMenu=document.getElementById("mapMenu");
        mapMenu.style.display = "none";
        setRoutePoint(point, mapEvent.latLng);
    }
    
    function onSelectIsochrone (isochrone) {
        var baseTime = isochrone.get('time');
        var offset = isochrone.get('offset');
        document.getElementById("ir_index").value = offset;
        updateIsochrones();
        redrawWindByOffset(baseTime, offset);
    }

    function onDisplaywind (event) {
        var cbDisplaywind = document.getElementById("cb_displaywind");
        if (cbDisplaywind.checked) {
            document.getElementById("wind-canvas").show();
        } else {
            document.getElementById("wind-canvas").hide();
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
        var dateInput =  document.getElementById("tb_starttime");
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
        var dateInput =  document.getElementById("tb_cycledate");
        var hourInput =  document.getElementById("sel_cyclehour");
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
        Util.doGET(
            "/function/vh.setParameter",
            function(data, status, xhr ) {
                console.log(`Set ${paramName}=${paramValue}`);
            },
            function (xhr) {
                alert(`${xhr.status} ${xhr.statusText}: ${xhr.responseText}`);
            },
            {
                "name": paramName,
                "value": paramValue
            }
        );
    }
    
    function onMapMenuMouseLeave (event) {
        var mapMenu=document.getElementById("mapMenu");
        mapMenu.style.display = "none";
    }
    
    function onMapRightClick (event) {
        mapEvent = event;
        var windowEvent = window.event;
        var mapMenu=document.getElementById("mapMenu");
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
        var bt_execute=document.getElementById("bt_getroute");
        bt_execute.disabled = true;
        
        var mapMenu=document.getElementById("mapMenu");
        var windowEvent = window.event;
        mapMenu.style.display = "none";
        var that = this;
        var pgGetRoute = document.getElementById("pg_getroute");
        pgGetRoute.value = 5;
        var selDuration = document.getElementById("sel_duration");
        var duration = selDuration.value;
        var timer = window.setInterval(updateGetRouteProgress, 10 * duration);

        // $('div, button, input').css('cursor', 'wait');

        Util.doGET(
            "/function/vh.getRoute",
            function (xhr) {
                var data = JSON.parse(xhr.responseText);

                // Remember routing data
                currentRouting = data;
                
                // Reset timer
                window.clearInterval(timer);
                pgGetRoute.value = pgGetRoute.max;
                
                // Display new data
                clearRoute();
                displayRouting(data);
                bt_execute.disabled = false;
            },
            function (xhr) {
                bt_execute.disabled = false;
                window.clearInterval(timer);
                pgGetRoute.value = pgGetRoute.max;
                alert(`${xhr.status} ${xhr.statusText}: ${xhr.responseText}`);
            });
    }

    function setRoutePoint(point, latlng) {
        var lat =  latlng.lat();
        var lng =  latlng.lng();
        var that = this;
        Util.doGET(
            "/function/vh.setRoute" + "?pointType=" + point + "&lat=" + lat + "&lng=" + lng,
            function(data) {
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
            },
            function (xhr) {
                alert(`${xhr.status} ${xhr.statusText}: ${xhr.responseText}`);
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

        document.getElementById("lb_from").innerText = data.stats.start;
        document.getElementById("lb_duration").innerText = data.stats.duration;
        document.getElementById("lb_sails").innerText = formatSails(data);
        document.getElementById("lb_minwind").innerText = data.stats["min-wind"].toFixed(1) + " - " + data.stats["max-wind"].toFixed (1);
        document.getElementById("lb_mintwa").innerText = data.stats["min-twa"] + " - " +  data.stats["max-twa"];
        document.getElementById("lb_polars").innerText = data.polars;
        document.getElementById("lb_options").innerText = data.options;
        // document.getElementById("lb_maxspeed").innerText = data.maxspeed;
    }


    function getSession () {
        Util.doGET("/function/vh.getSession",
                   function(request) {
                       var routing = JSON.parse(request.responseText);
                       if (routing.start) {
                           updateStartPosition(routing.start.lat, routing.start.lng);
                           var start  = new google.maps.LatLng(routing.start.lat, routing.start.lng);
                           googleMap.setCenter(start);
                       }
                       
                       if (routing.dest) {
                           var dest  = new google.maps.LatLng(routing.dest.lat, routing.dest.lng);
                           destinationMarker.setPosition(dest);
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
                       
                       courseGCLine = new google.maps.Polyline({
                           geodesic: true,
                           strokeColor: '#ff0000',
                           strokeOpacity: 1.0,
                           strokeWeight: 1
                       });
                       courseGCLine.setMap(googleMap);
                       courseGCLine.setPath([startMarker.getPosition(), destinationMarker.getPosition()]);
                       
                   },
                   function (request) {
                       alert(request.statusText + ' ' + request.responseText);
                   });
    }


    function removeSession () {
        Util.doGET(
            "/function/vh.removeSession",
            function(result, status, xhr) {
                console.log("Removed " + result); 
            },
            function (xhr) {
                alert("No leg info for race");
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
                   function (xhr) {
                       var data = JSON.parse(xhr.responseText);
                       if (data) {
                           console.log('Loaded ' + id);
                           polars = data.scriptData.polar;
                       } else {
                           alert("No leg info for race");
                       }
                   },
                   function (xhr) {
                       alert(`${xhr.status} ${xhr.statusText}: ${xhr.responseText}`);
                   });
    }
    
    function setupLegVR (raceinfo) {

        var vrData = raceinfo.data;
        
        document.title = vrData.name;

        var markStbd = 'img/mark_green.png';
        var markPort = 'img/mark_red.png';

        setParameter("polars", vrData.boat.polar_id);
        loadPolars( vrData.boat.polar_id);

        var start = positionFromDocumentURL();
        if (start) {
        } else {
            startMarker.setPosition( {"lat": vrData.start.lat, "lng": vrData.start.lon});
            var startPos = new google.maps.LatLng(vrData.start.lat, vrData.start.lon);
            setRoutePoint('start', startPos);
        }
        
        destinationMarker.setPosition( {"lat": vrData.end.lat, "lng": vrData.end.lon});
        var destPos = new google.maps.LatLng(vrData.end.lat, vrData.end.lon);
        setRoutePoint('dest', destPos);
        
        googleMap.panTo(startMarker.getPosition());

        for (const c of vrData.checkpoints) {
            var mark = new google.maps.Marker({
                position: {"lat": c.start.lat, "lng": c.start.lon},
                map: googleMap,
                icon: (c.side=='port')?markPort:markStbd,
                title: c.group + "-" + c.id + " " + c.name,
                draggable: false
            });
            mark.addListener('click', function () { onMarkerClicked(mark) });
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
            iceLine.setMap(googleMap);
            iceLine.setPath(iceLimit);
        }
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
        var pgGetRoute = document.getElementById("pg_getroute");
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
    
    function addMarkerListener (marker) {
        marker.addListener('click', function () { onMarkerClicked(marker) });
    }
    
    function clearPath (path) {
        for ( var i=0; i < path.length; i++ ) {
            path[i].setMap(null);
        }
        path = [];
    }

    var ortho;
    function drawOrthodromic (data) {
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
            Util.doGET(
                "/function/vh.getTWAPath?basetime=" + baseTime + "&time=" + time + "&latA=" + latA + "&lngA=" + lngA + "&lat=" + lat + "&lng=" + lng,
                function (xhr) {
                    var data = JSON.parse(xhr.responseText);
                    drawTWAPath(data.twapath);
                    drawHDGPath(data.hdgpath);
                    document.getElementById("lb_twa").innerText = data.twa;
                    document.getElementById("lb_twa_heading").innerText = data.heading;
                },
                function (xhr) {
                    alert(`${xhr.status} ${xhr.statusText}: ${xhr.responseText}`);
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
        Util.doGET(
            "/function/vh.getWind"
                + "?" + timeSpec
                + "&north=" + lat0.toFixed(6)
                + "&south=" + bounds.south.toFixed(6)
                + "&west=" + bounds.west.toFixed(6)
                + "&east=" + lon0.toFixed(6)
                + "&ddx=" + ddx
                + "&ddy=" + ddy
                + "&xSteps=" + xSteps
                + "&ySteps=" + ySteps,
            function (xhr) {
                // $('div, button, input').css('cursor', 'auto');
                var data = JSON.parse(xhr.responseText);
                forecastData = data;
                drawWind(data)
            },
            function (xhr) {
                // $('div, button, input').css('cursor', 'auto');
                alert(`${xhr.status} ${xhr.statusText}: ${xhr.responseText}`);
            });
    }

    function drawWind (data) {

        // Update time
        forecastCycle = data.basetime;
        document.getElementById("lb_modelrun").innerText = data.cycle;
        document.getElementById("lb_index").innerText = data.time;

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

    function updateWindInfo (event) {
        if (windData) {
            var zoom = googleMap.getZoom();
            
            document.getElementById("lb_position").innerText = formatLatLngPosition(event.latLng);
            
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
                document.getElementById("lb_windatposition").innerText = pad0(windDir,3) + "° | " + windSpeed + "kn";

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

    function getVMG (windSpeed) {
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
            "vmg": Math.cos(Util.toRad(twa)) * linear(windSpeed, s0, s1, v0, v1)
        };
    }

    function linear (x, x0, x1, y0, y1) {
        var  y = y0 + (y1 - y0) * (x - x0) / (x1 - x0)
        return y;
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
    
    window.addEventListener("load", function (event) {
        setUp();
    });
    
}) ()

/// EOF
////////////////////////////////////////////////////////////////////////////////
