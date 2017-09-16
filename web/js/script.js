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

var startMarker;
var destinationMarker = {};

var routeTracks = [];
var routeIsochrones = [];

function setUp () {
	
	setupColors();

	mapCanvas = document.getElementById('map_canvas');
	var mapRect = mapCanvas.getBoundingClientRect();
	geometry.width = mapRect.width;
	geometry.height = mapRect.height;

	var mapProp = {
		center:new google.maps.LatLng(49.187, 8.473),
		zoom:5,
		scaleControl: true,
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

	// Connect button events
	$("#bt_inc").click(onAdjustIndex);
	$("#bt_dec").click(onAdjustIndex);
	$("#bt_inc6").click(onAdjustIndex);
	$("#bt_dec6").click(onAdjustIndex);
	
	// Connect option selectors
	$("#sel_polars").change(onSetParameter);
	$("#sel_forecastbundle").change(onSetParameter);
	$("#sel_duration").change(onSetParameter);
	$("#sel_searchangle").change(onSetParameter);
	$("#sel_pointsperisochrone").change(onSetParameter);

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

	google.maps.event.addListener(startMarker,'dragend',function() {
		setRoutePoint('start', startMarker.getPosition());
	});

	destinationMarker = new google.maps.Marker({
		position: {"lat": 55.391123, "lng": 13.792635},
		map: googleMap,
		title: 'Destination',
		draggable: true
	});

	google.maps.event.addListener(destinationMarker,'dragend',function() {
		setRoutePoint('dest', destinationMarker.getPosition());
	});
	
	getSession();
	updateMap();
}

function getSession () {
	$.ajax({ 
		url: "/function/vh:getSession",
		dataType: 'json'
	}).done( function(session) {

		var start = new google.maps.LatLng(session.routing.start.lat, session.routing.start.lng);
		startMarker.setPosition(start);
		var dest  = new google.maps.LatLng(session.routing.dest.lat, session.routing.dest.lng);
		destinationMarker.setPosition(dest);

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

	}).fail( function (jqXHR, textStatus, errorThrown) {
		alert('Error: ' + textStatus + ' ' + errorThrown);
	});
}

function onSetClientParameter (event) {
	alert('later.');
}

function onSetParameter (event) {
	var paramName = event.currentTarget.name;
	var paramValue = event.currentTarget.value;
	$.ajax({ 
		url: "/function/vh:setParameter" + "?name=" + paramName + "&value=" + paramValue,
		dataType: 'json'
	}).done( function(data) {
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
	mapMenu.style.display = "block";
	mapMenu.style.top = windowEvent.pageY + "px";
	mapMenu.style.left = windowEvent.pageX + "px";
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
			startMarker.setPosition(latlng);
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
	for ( var i = 0; i<routeTracks.length; i++ ) {
		routeTracks[i].setMap(undefined);
	}
	routeTracks = [];
	for ( var i = 0; i<routeIsochrones.length; i++ ) {
		routeIsochrones[i].setMap(undefined);
	}
	routeIsochrones = [];
}


function getRoute () {
	var mapMenu=$("#mapMenu")[0];
	var windowEvent = window.event;
	mapMenu.style.display = "none";
	var that = this;
	$.ajax({ 
		url: "/function/vh:getRoute",
		dataType: 'json'
	}).done( function(data) {
		clearRoute();
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
			addInfo(isochrone, isochrones[i].time)
			routeIsochrones[i] = isochrone;
		}
	}).fail( function (jqXHR, textStatus, errorThrown) {
		console.log("Could not set route point:" + textStatus + ' ' + errorThrown);
	});
}

function addInfo (isochrone, info) {
	isochrone.set("time", info);
	isochrone.addListener('click', function () {
		var iso = isochrone;
		onSelectIsochrone(iso);
	});
}

function onSelectIsochrone (isochrone) {
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
	var mapBounds = googleMap.getBounds();
	var sw = mapBounds.getSouthWest();
	var ne = mapBounds.getNorthEast();
	north = ne.lat();
	south = sw.lat();
	west = sw.lng();
	east = ne.lng();
	var label = "⌊" + formatLatLng(sw) + " \\\\ " +  formatLatLng(ne) + "⌉"; 
	$("#lb_map_bounds").text("Kartenausschnitt: " + label);
	redrawWind("offset", ir_index.value);
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
	data = data[2];
	var ctx = mapCanvas.getContext("2d");
	ctx.globalAlpha = 0.6;
	ctx.clearRect(0, 0, geometry.width, geometry.height);
	for ( var y = 0; y < ySteps; y++ ) {
		var yOffset = y * ddy + (ddy / 2);
		for ( var x = 0; x < xSteps; x++ ) {
			var xOffset = x * ddx + (ddx / 2);
			drawWindArrow(ctx, xOffset, yOffset, data[y][x][0], data[y][x][1]);
		}
	}
}

function getWind () {
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

function formatLatLng (latlng) {
	return formatDeg(toDeg(latlng.lat())) + ", " + formatDeg(toDeg(latlng.lng()));
}

function formatDeg (deg) {
	var val = deg.g + "°" + deg.m + "'" + deg.s + "." + deg.cs + "\"" ;
	return (deg.u < 0) ? "-" + val : val;
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
	return {u:u, g:g, m:m, s:s, cs:cs};
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
