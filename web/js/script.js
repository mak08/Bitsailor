////////////////////////////////////////////////////////////////////////////////
/// VirtualHelm UI

var googleMap = null;
var mapContextMenu = null;
// The SVG element used for drawing tracking data
var svgArea = {};

// Bounds and width from the map are kept here for convenience
var geometry = {};

var xSteps = 40;
var ySteps = 25;

var bt_inc;
var bt_dec;
var ir_index;

var mapEvent;

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
	
	// Connect menu events
	var mapMenu = $("#mapMenu")[0];
	mapMenu.onmouseleave = onMapMenuMouseLeave;

	ir_index = $("#ir_index")[0];
	updateMap();

}

function onMapMenuMouseLeave(event) {
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

function setRoute (point) {
	var lat =  mapEvent.latLng.lat();
	var lng =  mapEvent.latLng.lng();
	var mapMenu=$("#mapMenu")[0];
	var windowEvent = window.event;
	mapMenu.style.display = "none";
	var that = this;
	$.ajax({ 
		url: "/function/vh:setRoute"
		    + "?pointType=" + point
			+ "&lat=" + lat
			+ "&lng=" + lng,
		dataType: 'json'
	}).done( function(data) {
		// alert(point + " at " + lat + ", " + lng + " " + JSON.stringify(data));
		boatPath.setMap(undefined);
		boatPath.setPath(data.route.twapath);
		boatPath.setMap(googleMap);
		console.log("Success: " + point + " set.");
	}).fail( function (jqXHR, textStatus, errorThrown) {
		console.log("Could not set route point:" + textStatus + ' ' + errorThrown);
	});
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
	onRedrawWind(event);
}

function updateMap () {
	var mapBounds = googleMap.getBounds();
	if ( mapBounds != undefined ) {
		var sw = mapBounds.getSouthWest();
		var ne = mapBounds.getNorthEast();
		var label = "⌊" + formatLatLng(sw) + " \\\\ " +  formatLatLng(ne) + "⌉"; 
		$("#lb_map_bounds").text("Kartenausschnitt: " + label);
		onRedrawWind();
	}
}

function onRedrawWind () {
	var mapBounds = googleMap.getBounds();

	var sw = mapBounds.getSouthWest();
	var ne = mapBounds.getNorthEast();
	var north = ne.lat();
	var south = sw.lat();
	var west = sw.lng();
	var east = ne.lng();
	redrawWind(north, south, east, west);
}

function redrawWind (north, south, east, west) {
	var dx = 1100;
	var dy = 800;

	// Compute number of wind marker, taking window ratio into account
	var ddx = dx / xSteps;
	var ddy = dy / ySteps;
	
	var ddlat = (north - south) / ySteps;
	var ddlon = (east - west) / xSteps;
	var lat0 = north + ddlat/2;
	var lon0 = east + ddlon/2;

	$.ajax({ 
		url: "/function/vh:getWind"
		    + "?offset=" + ir_index.value
			+ "&north=" + roundTo(lat0, 6)
			+ "&south=" + roundTo(south, 6)
			+ "&west=" + roundTo(west, 6)
			+ "&east=" + roundTo(lon0, 6)
		    + "&ddx=" + roundTo((east-west)/xSteps, 8)
		    + "&ddy=" + roundTo((north-south)/ySteps, 8),
		dataType: 'json'
	}).done( function(data) {
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

	}).fail( function (jqXHR, textStatus, errorThrown) {
		console.log("Could not get wind data:" + textStatus + ' ' + errorThrown);
	});
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
