////////////////////////////////////////////////////////////////////////////////
/// VirtualHelm UI

var googleMap = null;
var mapContextMenu = null;
// The SVG element used for drawing tracking data
var svgArea = {};

// Bounds and width from the map are kept here for convenience
var geometry = {};

// Number of wind arrows
var xSteps = 40;
var ySteps = 25;
// Screen resolution
var dx = 1100;
var dy = 800;
// Increments
var ddx = dx / xSteps;
var ddy = dy / ySteps;

// Map bounds
var north;
var south;
var west;
var east;

// Time index
var ir_index;

var mapEvent;

var startMarker = {};
var twaAnchor = {};
var twaTime = {};

var destinationMarker = {};

var windData = [];

var routeTracks = [];
var routeIsochrones = [];
var trackMarkers = [];

var oldLat = 0;
var oldLng = 0;


function setUp () {
	
	setupColors();

	mapCanvas = document.getElementById('map_canvas');
	var mapRect = mapCanvas.getBoundingClientRect();
	geometry.width = mapRect.width;
	geometry.height = mapRect.height;

	// Map styles: hide everything by default, except landcover.
	var mapStyles = [
		{ //hide all fills
			elementType: 'geometry.fill',
			stylers: [
				{ visibility: 'off' }
			]
		},
		{ //hide all fills
			featureType: 'landscape.natural.landcover',
			elementType: 'geometry.fill',
			stylers: [
				{ visibility: 'on' }
			]
		}
	];

	// Create a map object, and include the MapTypeId to add
	// to the map type control.
	var mapProp = {
		center:new google.maps.LatLng(49.187, 8.473),
		zoom:5,
		scaleControl: true,
		styles: mapStyles,
		backgroundColor: 'hsla(0, 0%, 0%, 0)',
		noClear: true,
		mapTypeId:google.maps.MapTypeId.ROADMAP,
		draggableCursor: "crosshair"
	};
	var mapDiv = $("#googleMap")[0];
	googleMap = new google.maps.Map(mapDiv, mapProp);

	mapContextMenu = $("#mymenu")[0];


	// Connect map events
	google.maps.event.addListener(googleMap, 'zoom_changed', updateMap);
	// google.maps.event.addListener(googleMap, 'bounds_changed', updateMap);
	google.maps.event.addListener(googleMap, 'dragend', updateMap);
	google.maps.event.addDomListener(googleMap, 'rightclick', onMapRightClick);

	// Track cursor position
	google.maps.event.addListener(googleMap, 'mousemove', updateWindInfo);
	google.maps.event.addListener(googleMap, 'click', getTWAPath);

	// Connect button events
	$("#bt_inc").click(onAdjustIndex);
	$("#bt_dec").click(onAdjustIndex);
	$("#bt_inc6").click(onAdjustIndex);
	$("#bt_dec6").click(onAdjustIndex);
	$("#ir_index").change(onAdjustIndex);

	$("#bt_setstartlat").click(onStartDMSUpdated);
	
	$("#bt_setstartlng").click(onStartDMSUpdated);
	
	// Connect option selectors
	$("#sel_polars").change(onSetParameter);
	$("#sel_forecastbundle").change(onSetParameter);
	$("#sel_duration").change(onSetParameter);
	$("#sel_searchangle").change(onSetParameter);
	$("#sel_angleincrement").change(onSetParameter);
	$("#sel_pointsperisochrone").change(onSetParameter);
	$("#cb_usefoils").change(onSetParameter);
	$("#cb_fastmanoeuvres").change(onSetParameter);
	$("#cb_minwind").change(onSetParameter);

	// Tracks & Isochrones display is handled by the client directly
	$("#cb_tracks").change(onSetClientParameter);
	$("#cb_isochrones").change(onSetClientParameter);

	// Connect menu events
	var mapMenu = $("#mapMenu")[0];
	mapMenu.onmouseleave = onMapMenuMouseLeave;

	ir_index = $("#ir_index")[0];

	startMarker = new google.maps.Marker({
		position: {"lat": 54.434403, "lng": 11.361632},
		map: googleMap,
		title: 'Start',
		draggable: true
	});
	startMarker.addListener('click', function () { markerClicked(startMarker) });

	google.maps.event.addListener(startMarker,'dragend',function() {
		setRoutePoint('start', startMarker.getPosition());
	});

	destinationMarker = new google.maps.Marker({
		position: {"lat": 55.391123, "lng": 13.792635},
		map: googleMap,
		title: 'Destination',
		draggable: true
	});
	destinationMarker.addListener('click', function () { markerClicked(destinationMarker) });


	google.maps.event.addListener(destinationMarker,'dragend',function() {
		setRoutePoint('dest', destinationMarker.getPosition());
	});
	
	getSession();

	google.maps.event.addListenerOnce(googleMap, 'idle', function(){
		updateMap();
	});
}

function markerClicked (marker) {
	twaAnchor = marker.getPosition();
	twaTime = marker.get('time');
}

function updateStartPosition (lat, lng) {
	var latLng = new google.maps.LatLng(lat, lng);
	startMarker.setPosition(latLng);
	var latDMS = toDeg(lat);
	$("#tb_dstartlat")[0].value = latDMS.u * latDMS.g;
	$("#tb_mstartlat")[0].value = latDMS.m;
	$("#tb_sstartlat")[0].value = latDMS.s;
	var lngDMS = toDeg(lng);
	$("#tb_dstartlng")[0].value = lngDMS.u * lngDMS.g;
	$("#tb_mstartlng")[0].value = lngDMS.m;
	$("#tb_sstartlng")[0].value = lngDMS.s;
}

function onStartDMSUpdated (component) {
	var d = $("#tb_dstartlat")[0].value;
	var u = sign(d);
	var g = Math.abs(d);
	var latDMS =  {
		"u": u,
		"g": g,
		"m": $("#tb_mstartlat")[0].value,
		"s": $("#tb_sstartlat")[0].value,
		"cs": 0
	}
	d = $("#tb_dstartlng")[0].value;
	u = sign(d);
	g = Math.abs(d);
	var lngDMS =  {
		"u": u,
		"g": g,
		"m": $("#tb_mstartlng")[0].value,
		"s": $("#tb_sstartlng")[0].value,
		"cs": 0
	}
	var lat = fromDeg(latDMS);
	var lng = fromDeg(lngDMS);
	var latLng = new google.maps.LatLng(lat, lng);
	setRoutePoint('start', latLng);
}


function getSession () {
	$.ajax({ 
		url: "/function/vh:getSession",
		dataType: 'json'
	}).done( function(session) {

		updateStartPosition(session.routing.start.lat, session.routing.start.lng);

		var dest  = new google.maps.LatLng(session.routing.dest.lat, session.routing.dest.lng);
		destinationMarker.setPosition(dest);

		var forecast = session.routing["forecast-bundle"];
		var selForecast = $("#sel_forecastbundle")[0];
		var irIndex = $("#ir_index")[0];
		var lbFCMax = $("#lb_fcmax")[0];
		if 	( selForecast.value !== forecast ) {
			irIndex.value = 0;
			if ( forecast === "DWD-ICON-BUNDLE" ) {
				irIndex.max = 72;
				lbFCMax.innerText = '' + 72;
			} else if ( forecast === "NOAA-BUNDLE" ) {
				irIndex.max = 240;
				lbFCMax.innerText = '' + 240;
			}
			selForecast.value = forecast;
			redrawWind("offset", irIndex.value);
		}

		var polars = session.routing.polars;
		var selPolars = $("#sel_polars")[0];
		selPolars.value = polars;

		var duration = session.routing.stepmax;
		var selDuration = $("#sel_duration")[0];
		if ( duration === 21600 ) {
			selDuration.value = 6;
		} else if ( duration === 43200 ) {
			selDuration.value = 12;
		} else if ( duration === 86400 ) {
			selDuration.value = 24;
		} else if ( duration === 129600 ) {
			selDuration.value = 36;
		} else if ( duration === 172800 ) {
			selDuration.value = 48;
		} else if ( duration === 345600 ) {
			selDuration.value = 96;
		} else if ( duration === 518400 ) {
			selDuration.value = 144;
		}

		var searchAngle = session.routing.fan;
		var selSearchAngle = $("#sel_searchangle")[0];
		selSearchAngle.value = searchAngle;

		var maxPoints = session.routing["max-points-per-isochrone"];
		var selMaxPoints = $("#sel_pointsperisochrone")[0];
		selMaxPoints.value = maxPoints;

		var useFoils = session.routing.foils;
		var cbUseFoils = $("#cb_usefoils")[0];
		cbUseFoils.checked = useFoils;

		var fastManoeuvres = session.routing.fastmanoeuvres;
		var cbFastManoeuvres = $("#cb_fastmanoeuvres")[0];
		cbFastManoeuvres.checked = fastManoeuvres;

		var minWind = session.routing.minwind;
		var cbMinWind = $("#cb_minwind")[0];
		cbMinWind.checked = minWind;


	}).fail( function (jqXHR, textStatus, errorThrown) {
		alert('Error: ' + textStatus + ' ' + errorThrown);
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
	var paramValue = event.currentTarget.checked;
	if ( paramValue === undefined ) {
		paramValue = event.currentTarget.value;
	}
	$.ajax({ 
		url: "/function/vh:setParameter" + "?name=" + paramName + "&value=" + paramValue,
		dataType: 'json'
	}).done( function(data) {
		if ( paramName === "forecastbundle" ) {
			var selForecast = $("#sel_forecastbundle")[0];
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
		alert("OK");
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
	mapMenu.style.top = pageY + "px";
	mapMenu.style.left = pageX + "px";
	return false;
}

var boatPath = new google.maps.Polyline({
    geodesic: true,
    strokeColor: '#FF0000',
    strokeOpacity: 1.0,
    strokeWeight: 2
});

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
	}).fail( function (jqXHR, textStatus, errorThrown) {
		alert("Could not set " + point + ': ' + textStatus + ' ' + errorThrown);
	});
}

function setRoute (point) {
	var mapMenu=$("#mapMenu")[0];
	mapMenu.style.display = "none";
	setRoutePoint(point, mapEvent.latLng);
}

function clearRoute() {
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


function addWaypointInfo(trackMarker, point, nextPoint) {
	var infowindow = new google.maps.InfoWindow({
		content: makeWaypointInfo(point, nextPoint)
	});
	trackMarker.set('time', point.time);
	trackMarker.addListener('mouseover', function() {
		infowindow.open(googleMap, trackMarker);
	});
	trackMarker.addListener('mouseout', function() {
		infowindow.close();
	});
}
function makeWaypointInfo(point, nextPoint) {
	result =  "<div>";
	result = result 
		+ "<b>Time</b>: " + point.time + "<p>" 
		+ "<b>Position</b>: " + formatPosition(point.position) + "<p>";
	if ( nextPoint !== undefined ) {
		result = result + "<p><b>Wind</b>: " + roundTo(ms2knots(nextPoint["wind-speed"]), 2) + "kts / " + roundTo(nextPoint["wind-dir"], 0) + "°</p>"
			+ "<p><b> TWA</b>: " + nextPoint.twa + "<b> Heading</b>: " + nextPoint.heading + "°</p>"
			+ "<p><b>Speed</b>: " + roundTo(ms2knots(nextPoint.speed), 2) + "kts</p>" 
			+ "<p><b>Sail</b>: " + nextPoint.sail + "</p>";
	}
	result = result + "<b>DTF</b>:" + roundTo(m2nm(point["destination-distance"]), 2) + "nm";
		+ "</div>";
	return result;
}

function getRoute () {
	var mapMenu=$("#mapMenu")[0];
	var windowEvent = window.event;
	mapMenu.style.display = "none";
	var that = this;
	var pgGetRoute = $("#pg_getroute")[0];
	pgGetRoute.value = 5;
	var selMaxPoints = $("#sel_pointsperisochrone")[0];
	var maxPoints = selMaxPoints.value;
	var selDuration = $("#sel_duration")[0];
	var duration = selDuration.value; 
	var timer = window.setInterval(updateGetRouteProgress, maxPoints * duration / 6);
	$.ajax({ 
		url: "/function/vh:getRoute",
		dataType: 'json'
	}).done( function(data) {
		clearRoute();
		window.clearInterval(timer);
		pgGetRoute.value = pgGetRoute.max;
		var best = data.best;
		for ( var i = 0; i < best.length; i++ ) {
			var trackMarker = new google.maps.Marker({
				position: best[i].position,
				map: googleMap,
				draggable: false
			});
			addMarkerListener(trackMarker);
			addWaypointInfo(trackMarker, best[i], best[i+1]);
			trackMarkers[i] = trackMarker;
		}

		var tracks = data.tracks;
		for ( var i = 0; i < tracks.length; i++ ) {
			var track = new google.maps.Polyline({
				geodesic: true,
				strokeColor: '#d00000',
				strokeOpacity: 1.0,
				strokeWeight: 2
			});
			track.setPath(tracks[i]);
			track.setMap(googleMap);
			routeTracks[i] = track;
		}
		var isochrones = data.isochrones;
		for ( var i = 0; i < isochrones.length; i++ ) {
			var isochrone = new google.maps.Polyline({
				geodesic: true,
				strokeColor: '#8080a0',
				strokeOpacity: 0.8,
				strokeWeight: 4
			});
			isochrone.setPath(isochrones[i].path);
			isochrone.setMap(googleMap);
			addInfo(isochrone, isochrones[i].time, isochrones[i].offset)
			routeIsochrones[i] = isochrone;
		}
	}).fail( function (jqXHR, textStatus, errorThrown) {
		alert("Error:" + textStatus + ' ' + errorThrown);
	});
}

function addMarkerListener(marker) {
	marker.addListener('click', function () { markerClicked(marker) });
}

var twaPath = [];

function drawTWAPath(data) {
	for ( var i=0; i<twaPath.length; i++ ) {
		twaPath[i].setMap(null);
	}
	twaPath = [];
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
				icons: [{icon: lineSymbol,	offset: '100%'}]
			});
		} else {
			twaPathSegment = new google.maps.Polyline({
				geodesic: true,
				strokeColor: color,
				strokeOpacity: 1,
				strokeWeight: 4
			});
		}
		twaPathSegment.setPath([data[i-1], data[i]]);
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

function onSelectIsochrone (isochrone) {
	var offset = isochrone.get('offset');
	$("#ir_index")[0].value = offset;
	var time = isochrone.get('time');
	redrawWind("time", time);
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

function updateMap () {
	if ( googleMap.zoom < 6 ) {
		googleMap.setMapTypeId(google.maps.MapTypeId.ROADMAP);
	} else {
		googleMap.setMapTypeId(google.maps.MapTypeId.TERRAIN);
	}
	var mapBounds = googleMap.getBounds();
	var sw = mapBounds.getSouthWest();
	var ne = mapBounds.getNorthEast();
	north = ne.lat();
	south = sw.lat();
	west = sw.lng();
	east = ne.lng();
	var label = "⌊" + formatLatLng(sw) + " \\ " +  formatLatLng(ne) + "⌉"; 
	$("#lb_map_bounds").text("Kartenausschnitt: " + label);
	redrawWind("offset", ir_index.value);
}

function getTWAPath(event) {
	if ( twaAnchor.lat === undefined || twaTime === undefined ) {
		console.log('No TWA anchor point selected');
	} else {
		var latA = twaAnchor.lat();
		var lngA = twaAnchor.lng();
		var lat = event.latLng.lat();
		var lng = event.latLng.lng();
		$.ajax({ 
			url: "/function/vh:getTWAPath?time=" + twaTime + "&latA=" + latA + "&lngA=" + lngA + "&lat=" + lat + "&lng=" + lng,
			dataType: 'json'
		}).done( function(data) {
			drawTWAPath(data.path);
			$("#lb_twa").text(data.twa);
		}).fail( function (jqXHR, textStatus, errorThrown) {
			alert(textStatus + ' ' + errorThrown);
		});
	}
}

function redrawWind (timeParamName, timeParamValue) {
	
	var lat0 = north + ((north - south) / ySteps)/2;
	var lon0 = east + ((east - west) / xSteps)/2;

	$.ajax({ 
		url: "/function/vh:getWind"
		    + "?" + timeParamName + "=" + timeParamValue
			+ "&north=" + roundTo(lat0, 6)
			+ "&south=" + roundTo(south, 6)
			+ "&west=" + roundTo(west, 6)
			+ "&east=" + roundTo(lon0, 6)
		    + "&ddx=" + roundTo((east-west)/xSteps, 8)
		    + "&ddy=" + roundTo((north-south)/ySteps, 8),
		dataType: 'json'
	}).done( function(data) {
		drawWind(data)
	}).fail( function (jqXHR, textStatus, errorThrown) {
		console.log("Could not get wind data:" + textStatus + ' ' + errorThrown);
	});
}

function drawWind (data) {
	$("#lb_modelrun").text(data[0]);
	$("#lb_index").text(data[1]);
	$("#lb_fcmax").text(' ' + data[2] + 'hrs');
	windData = data[3];
	var ctx = mapCanvas.getContext("2d");
	ctx.globalAlpha = 0.6;
	ctx.clearRect(0, 0, geometry.width, geometry.height);
	for ( var y = 0; y < ySteps; y++ ) {
		var yOffset = y * ddy + (ddy / 2);
		for ( var x = 0; x < xSteps; x++ ) {
			var xOffset = x * ddx + (ddx / 2);
			drawWindArrow(ctx, xOffset, yOffset, windData[y][x][0], windData[y][x][1]);
		}
	}
}

function updateWindInfo (event) {

	var zoom = googleMap.getZoom();
	var lat = roundTo(event.latLng.lat(), Math.floor(zoom/5));
	var lng = roundTo(event.latLng.lng(), Math.floor(zoom/5));

	var gN = 'N';
	if ( lat < 0 ) { gN = 'S'; lat = -lat; }
	var gE = 'E';
	if ( lng < 0 ) { gE = 'W'; lng = -lng; }

	$("#lb_position").text(formatLatLng(event.latLng));

	var mapBounds = googleMap.getBounds();

	var sw = mapBounds.getSouthWest();
	var ne = mapBounds.getNorthEast();
	north = ne.lat();
	south = sw.lat();
	west = sw.lng();
	east = ne.lng();
	var iLat = Math.round((event.latLng.lat() - north) / (south - north) * ySteps);
	var iLng = xSteps - Math.round((event.latLng.lng() - east) / (west - east) * xSteps);
	var windDir = roundTo(windData[iLat][iLng][0], 0);
	var windSpeed = roundTo(ms2knots(windData[iLat][iLng][1]), 1);
	$("#lb_windatposition").text(windDir + "° | " + windSpeed + "kts");
}

function drawWindArrow(ctx, x, y, direction, speed) {
	direction = direction + 90;
	if (direction > 360) {
		direction = direction - 360;
	} 
	ctx.fillStyle = colors[ms2bf(speed)];
	ctx.strokeStyle = colors[ms2bf(speed)];
	ctx.save();
	ctx.translate(x, y);
	ctx.rotate((direction*Math.PI/180));
	var scale = (speed>0)?0.4 + speed/30:0;
	ctx.scale(scale, scale);
	ctx.beginPath();
	ctx.moveTo(-0, 0);
	ctx.lineTo(-15, 12);
	ctx.lineTo(18, 0);
	ctx.lineTo(-15, -12);
	ctx.closePath()
	ctx.fill();
	ctx.stroke();
	ctx.restore();
}

function m2nm (dist) {
	return dist / 1852;
}

function ms2knots (speed) {
	return 900.0 * speed / 463.0;
}

function formatLatLng (latlng) {
	return formatDeg(toDeg(latlng.lat())) + "N | " + formatDeg(toDeg(latlng.lng())) +"E";
}

function formatPosition (latlng) {
	return formatDeg(toDeg(latlng.lat)) + "N | " + formatDeg(toDeg(latlng.lng)) + "E";
}

function formatDeg (deg) {
	var val = deg.g + "°" + deg.m + "'" + deg.s;
	return (deg.u < 0) ? "-" + val : val;
}

function fromDeg (deg) {
	var sign = deg.u || 1.0;
	var abs = 0.0 + deg.g + (deg.m / 60.0) + (deg.s / 3600.0) + (deg.cs / 360000);
	return 1.0 * sign * abs
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

function roundTo (number, digits) {
	var scale = Math.pow(10, digits);
	return Math.round(number * scale) / scale;
}

function sign (x) {
	if (x < 0) {
		return -1;
	} else {
		return 1;
	}
}

/// EOF
////////////////////////////////////////////////////////////////////////////////
