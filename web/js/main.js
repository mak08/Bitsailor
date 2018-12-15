////////////////////////////////////////////////////////////////////////////////
/// Sailsphere Router UI

( function () {

    var raceId = "anon";
    
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
        
        // Connect button events
        $("#bt_getroute").click(getRoute);

        $("#bt_inc").click(onAdjustIndex);
        $("#bt_dec").click(onAdjustIndex);
        $("#bt_inc6").click(onAdjustIndex);
        $("#bt_dec6").click(onAdjustIndex);
        $("#ir_index").change(onAdjustIndex);
        
        $("#cb_startdelayed").click(onDelayedStart);
        $("#tb_starttime").change(onSetParameter);
        
        // Connect option selectors
        $("#sel_polars").change(onSetParameter);
        $("#sel_forecastbundle").change(onSetParameter);
        $("#sel_duration").change(onSetParameter);
        $("#cb_minwind").change(onSetParameter);
        $("#cb_hidewind").change(onHideWind);
        
        // Tracks & Isochrones display is handled by the client directly
        $("#cb_tracks").change(onSetClientParameter);
        $("#cb_isochrones").change(onSetClientParameter);
        
        // Connect menu events
        $("#bt_setstart" ).click(function () { onContextMenu('start') });
        $("#bt_setdest"  ).click(function () { onContextMenu('dest') });
        $("#bt_ltpmark"  ).click(function () { onContextMenu('ltp') });
        $("#bt_ltsmark"  ).click(function () { onContextMenu('lts') });
        
        var mapMenu = $("#mapMenu")[0];
        mapMenu.onmouseleave = onMapMenuMouseLeave;
        
        ir_index = $("#ir_index")[0];

        var startFlag = 'img/start_45x32.png';
        startMarker = new google.maps.Marker({
            position: {"lat": 54.434403, "lng": 11.361632},
            map: googleMap,
            title: 'Start',
            icon: startFlag,
            draggable: true
        });
        startMarker.addListener('click', function () { onMarkerClicked(startMarker) });
        
        google.maps.event.addListener(startMarker,'dragend',function() {
            setRoutePoint('start', startMarker.getPosition());
        });
        
        var finishFlag = 'img/finish_32x20.png';
        destinationMarker = new google.maps.Marker({
            position: {"lat": 55.391123, "lng": 13.792635},
            map: googleMap,
            title: 'Destination',
            icon: finishFlag,
            draggable: true
        });
        destinationMarker.addListener('click', function () { onMarkerClicked(destinationMarker) });
        
        google.maps.event.addListener(destinationMarker,'dragend',function() {
            setRoutePoint('dest', destinationMarker.getPosition());
        });
        
        setupCanvas();
        
        google.maps.event.addListenerOnce(googleMap, 'idle', function(){
            updateMap();
        });
        
        getSession();
        
    }

    ////////////////////////////////////////////////////////////////////////////////
    /// Event handlers
    
    function onWindowResize (event) {
    }

    function updateMap () {
        if ( googleMap.zoom < 6 ) {
            googleMap.setMapTypeId(google.maps.MapTypeId.ROADMAP);
        } else {
            googleMap.setMapTypeId(google.maps.MapTypeId.TERRAIN);
        }
        var bounds = getMapBounds();
        var label = "⌊" + formatLatLngPosition(bounds.southWest) + " \\ " +  formatLatLngPosition(bounds.northEast) + "⌉";
        $("#lb_map_bounds").text("Kartenausschnitt: " + label);
        redrawWind("offset", ir_index.value);
    }

    // Event handler for context menu mapMenu 
    function onContextMenu (point) {
        var mapMenu=$("#mapMenu")[0];
        mapMenu.style.display = "none";
        setRoutePoint(point, mapEvent.latLng);
    }
    
    function onSelectIsochrone (isochrone) {
        var offset = isochrone.get('offset');
        $("#ir_index")[0].value = offset;
        var time = isochrone.get('time');
        redrawWind("time", time);
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
        redrawWind("offset", ir_index.value);
    }
    
    function onDelayedStart (event) {
        if (event.target.checked === true) {
            var d = new Date();
            $("#tb_starttime")[0].value = d.toISOString().substring(0,16);
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
                redrawWind("offset", irIndex.value);
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
        redrawWind("time", twaTime);
    }
    
    //////////////////////////////////////////////////////////////////////
    /// XHR requests
    
    function getRoute () {
        var mapMenu=$("#mapMenu")[0];
        var windowEvent = window.event;
        mapMenu.style.display = "none";
        var that = this;
        var pgGetRoute = $("#pg_getroute")[0];
        pgGetRoute.value = 5;
        var selDuration = $("#sel_duration")[0];
        var duration = selDuration.value;
        var timer = window.setInterval(updateGetRouteProgress, 10 * duration);
        $.ajax({
            url: "/function/vh:getRoute",
            dataType: 'json'
        }).done( function (data) {
            // Remember routing data
            currentRouting = data;

            // Reset timer
            window.clearInterval(timer);
            pgGetRoute.value = pgGetRoute.max;

            // Display new data
            clearRoute();
            displayRouting(data);
            
        }).fail( function (jqXHR, textStatus, errorThrown) {
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
        var startTime = new Date(best[0].time);

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
            addWaypointInfo(trackMarker, startTime, best[i]);
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
        var isochrones = data.isochrones;
        var startSymbol = {
            path: google.maps.SymbolPath.CIRCLE
        }
        for ( var i = 0; i < isochrones.length; i++ ) {
            var h = new Date(isochrones[i].time).getUTCHours();
            var color;
            if (h%6 === 4) {
                color = '#D00000';
            } else {
                color = (h%12)?'#8080a0':'#000000';
            }
            var isochrone = new google.maps.Polyline({
                geodesic: true,
                strokeColor: color,
                strokeOpacity: 0.8,
                strokeWeight: (h%6)?1:3,
                icons: [{icon: startSymbol,  offset: '0%'}]
            });
            isochrone.setPath(isochrones[i].path);
            isochrone.setMap(googleMap);
            addInfo(isochrone, isochrones[i].time, isochrones[i].offset)
            routeIsochrones[i] = isochrone;
        }
        
        $("#lb_from").text(JSON.stringify(data.stats.start));
        $("#lb_duration").text(JSON.stringify(data.stats.duration));
        $("#lb_sails").text(formatSails(data));
        $("#lb_minwind").text(roundTo(data.stats["min-wind"], 1) + " - " + roundTo(data.stats["max-wind"], 1));
        $("#lb_mintwa").text(data.stats["min-twa"] + " - " +  data.stats["max-twa"]);
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
            
            var forecast = routing["forecast-bundle"];
            var selForecast = $("#sel_forecastbundle")[0];
            var irIndex = $("#ir_index")[0];
            var lbFCMax = $("#lb_fcmax")[0];
            
            var starttime = routing.starttime;
            var cbStartDelayed = $("#cb_startdelayed")[0];
            if ( starttime != false && starttime != 'NIL' ) {
                cbStartDelayed.checked = true;
                $("#tb_starttime")[0].value = starttime;
                
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
            console.log(infoWindow.content);
            infoWindow.open(googleMap, trackMarker);
        });
        trackMarker.addListener('mouseout', function() {
            infoWindow.close();
        });
    }
    
    function makeWaypointInfo(startTime, point) {
        var time = new Date(point.time);
        var elapsed = formatDHM((time - startTime)/1000);
        result =  "<div style='color:#000;'>";
        result += "<p><b>T+" + elapsed + " - " + point.time + "</b></p>"
        result += "<hr>";

        result += "<p><b>Pos</b>: " + formatPointPosition(point.position) + "</p>";

        result += "<p><b>DTF</b>:" + roundTo(m2nm(point.dtf), 2) + "nm ";
        result += "<b> Speed</b>: " + roundTo(ms2knots(point.speed), 1) + "kts";
        
        result += "</p><hr>";
        
        result += "<p><b>Wind</b>: " + roundTo(ms2knots(point.tws), 2) + "kts / " + roundTo(point.twd, 0) + "°</p>";

        result += "<p>";
        result += "<b> TWA</b>: " + roundTo(point.twa, 1);
        result += "<b> HDG</b>: " + roundTo(point.heading, 1) + "°  " + point.sail;
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
    
    function drawTWAPath(data) {
        clearTWAPath();
        var color = '#00a0c0';
        var lineSymbol = {
            path: google.maps.SymbolPath.CIRCLE
        }
        for ( var i=1; i<data.length; i++ ) {
            var twaPathSegment;
            if ( (i % 6) === 0 ) {
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
                    strokeWeight: 4
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
        $.ajax({
            url: "/function/vh:getTWAPath?time=" + time + "&latA=" + latA + "&lngA=" + lngA + "&lat=" + lat + "&lng=" + lng,
            dataType: 'json'
        }).done( function(data) {
            drawTWAPath(data.path);
            $("#lb_twa").text(data.twa);
            $("#lb_twa_heading").text(data.heading);
        }).fail( function (jqXHR, textStatus, errorThrown) {
            alert(textStatus + ' ' + errorThrown);
        });
    }
    
    function redrawWind (timeParamName, timeParamValue) {
        var bounds = getMapBounds();
        
        var lat0 = bounds.north + ((bounds.north - bounds.south) / ySteps)/2;
        var lon0 = bounds.east + (arcLength(bounds.west, bounds.east) / xSteps)/2;
        var ddx = roundTo(arcLength(bounds.west, bounds.east)/xSteps, 8);
        var ddy = roundTo((bounds.north-bounds.south)/ySteps, 8);

        $.ajax({
            url: "/function/vh:getWind"
                + "?" + timeParamName + "=" + timeParamValue
                + "&north=" + roundTo(lat0, 6)
                + "&south=" + roundTo(bounds.south, 6)
                + "&west=" + roundTo(bounds.west, 6)
                + "&east=" + roundTo(lon0, 6)
                + "&ddx=" + ddx
                + "&ddy=" + ddy
                + "&xSteps=" + xSteps
                + "&ySteps=" + ySteps,
            dataType: 'json'
        }).done( function(data) {
            drawWind(data)
        }).fail( function (jqXHR, textStatus, errorThrown) {
            console.log("Could not get wind data:" + textStatus + ' ' + errorThrown);
        });
    }

    function drawWind (data) {

        // Update time
        forecastCycle = data[0];
        $("#lb_modelrun").text(data[3]);
        $("#lb_index").text(data[1]);
        $("#lb_fcmax").text(' ' + data[2] + 'hrs');

        var offset = (new Date(data[1]) - new Date(data[0])) / 3600000;
        ir_index.value = offset;

        // Keep new wind data in global var for displaying wind data at mouse position
        windData = data[4];
        
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
        var dlng = roundTo(width/xSteps, 8);
        var dlat = roundTo(height/ySteps, 8);

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
        var iLng = Math.round(arcLength(bounds.west, event.latLng.lng()) / arcLength(bounds.west, bounds.east) * xSteps);
        var windDir = roundTo(windData[iLat][iLng][0], 0);
        var windSpeed = roundTo(ms2knots(windData[iLat][iLng][1]), 1);
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
        return formatPosition(latlng.lat(), latlng.lng());
    }
    
    function formatPointPosition (point) {
        return formatPosition(point.lat, point.lng);
    }

    function formatPosition(lat, lon) {
        var latDMS = toDMS(lat);
        var lonDMS = toDMS(lon);
        var latString = latDMS.g + "°" + pad0(latDMS.m) + "'" + pad0(latDMS.s) + '"';
        var lonString = lonDMS.g + "°" + pad0(lonDMS.m) + "'" + pad0(lonDMS.s) + '"';
        return latString + ((latDMS.u == 1) ? "N" : "S") + " | " + lonString + ((lonDMS.u == 1) ? "E" : "W");
    }

    function formatDHM (seconds) {
        var dhm = toDHM(seconds);
        return dhm.days + ":" +  pad0(dhm.hours) + ":" + pad0(dhm.minutes) + ":" + pad0(dhm.seconds);
    }
    
    function formatSails (data) {
        var result = "";
        for (sail of data.stats.sails) {
            if (result) {
                result = sail + "," + result;
            } else {
                result = sail;
            }
        }
        return result;
    }

    ////////////////////////////////////////////////////////////////////////////////
    /// Conversion
    
    function m2nm (dist) {
        return dist / 1852;
    }
    
    function ms2knots (speed) {
        return 900.0 * speed / 463.0;
    }
    
    function fromDeg (deg) {
        var sign = deg.u || 1;
        var abs = deg.g + (deg.m / 60.0) + (deg.s / 3600.0) + (deg.cs / 360000.0);
        return sign * abs
    }
    
    function toDHM (seconds) {
        return {
            "days":  Math.floor(seconds / 86400),
            "hours": Math.floor(seconds/3600) % 24,
            "minutes": Math.floor(seconds/60) % 60,
            "seconds": seconds % 60
        };
    }

    function toDeg (number) {
        var u = sign(number);
        number = Math.abs(number);
        var g = Math.floor(number);
        var frac = number - g;
        var m = Math.floor(frac * 60);
        frac = frac - m/60;
        var s = Math.floor(frac * 3600);
        var cs = roundTo(360000 * (frac - s/3600), 0);
        while ( cs >= 100 ) {
            cs = cs - 100;
            s = s + 1;
        }
        return {"u":u, "g":g, "m":m, "s":s, "cs":cs};
    }
    
    function toDMS(number) {
        var u = sign(number);
        number = Math.abs(number);
        var g = Math.floor(number);
        var frac = number - g;
        var m = Math.floor(frac * 60);
        frac = frac - m / 60;
        var s = Math.floor(frac * 3600);
        var cs = roundTo(360000 * (frac - s / 3600), 0);
        while (cs >= 100) {
            cs = cs - 100;
            s = s + 1;
        }
        return {"u":u, "g":g, "m":m, "s":s, "cs":cs};
    }

    ////////////////////////////////////////////////////////////////////////////////
    /// Stuff

    function roundTo (number, digits) {
        var scale = Math.pow(10, digits);
        return Math.round(number * scale) / scale;
    }

    function arcLength(a, b) {
        if ( a < 0 ) a += 360;
        if ( b < 0 ) b += 360;
        if ( b < a ) b += 360;
        return b - a;
    }

    function sign(x) {
        return (x < 0) ? -1 : 1;
    }

    function pad0(val) {
        if (val < 10) {
            val = "0" + val;
        }
        return val;
    }

    $(document).ready(setUp);
    
}) ()

/// EOF
////////////////////////////////////////////////////////////////////////////////
