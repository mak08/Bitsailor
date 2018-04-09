////////////////////////////////////////////////////////////////////////////////
/// UI Controller




var controller = function () {

    function updateWindDisplay (map, clientRect) {

        var now = new Date().toISOString();

        var xSteps = 50;
        var ySteps = Math.trunc(xSteps/clientRect.width * clientRect.height);

        var mapBounds = map.getBounds();
        
        var north = mapBounds.getNorth();
        var east = mapBounds.getEast();
        var south = mapBounds.getSouth();
        var west = mapBounds.getWest();
        
        var lat0 = north + ((north - south) / ySteps)/2;
        var lon0 = east + ((east - west) / xSteps)/2;
        var ddx = roundTo((east-west) / xSteps, 8);
        var ddy = roundTo((north-south) / ySteps, 8);


        $.ajax({ 
            url: "/function/vh:getWind"
                + "?" + "time" + "=" + now
                + "&north=" + roundTo(lat0, 6)
                + "&south=" + roundTo(south, 6)
                + "&west=" + roundTo(west, 6)
                + "&east=" + roundTo(lon0, 6)
                + "&ddx=" + ddx
                + "&ddy=" + ddy,
            dataType: 'json'
        }).done( function (data) {
            var windCanvas = document.getElementById('windCanvas');
            drawWind(windCanvas, xSteps, ySteps, data);
        }).fail( function (jqXHR, textStatus, errorThrown) {
            console.log("Could not get wind data:" + textStatus + ' ' + errorThrown);
        });
    }

    return {
        onMapMouseMoved: function (event) {
            var lb_curpos = document.getElementById('lb_curpos');
            lb_curpos.textContent = formatPosition(event.latlng.lat, event.latlng.lng);
        },
        onMapBoundsChanged: function (event) {
            updateWindDisplay(event.target,
                              event.target.getContainer().getBoundingClientRect());
        }
    }
    
}();


function initPage () {
    
    return function () {
        // Create a map object, and include the MapTypeId to add
        // to the map type control.

        var leafletMap = L.map('leafletMap',{
            renderer: L.canvas()
        });
        leafletMap.setView([40.0, 5.0], 6);
        leafletMap.addLayer(L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png', {
            attribution: '&copy; <a href="http://osm.org/copyright">OpenStreetMap</a> contributors'
        })); //.addTo(leafletMap);

        L.gridLayer.windCanvas = function() {
            return new L.GridLayer.WindCanvas();
        }
        
        L.gridLayer.windCanvas().addTo(leafletMap);
        
        // Connect map events
        // leafletMap.on("mousemove", controller.onMapMouseMoved);
        // leafletMap.on("moveend", controller.onMapBoundsChanged);
        alert('Ready');
    }
        
}

/// EOF
////////////////////////////////////////////////////////////////////////////////
