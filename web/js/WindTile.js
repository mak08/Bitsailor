////////////////////////////////////////////////////////////////////////////////
/// Bitsailor Router UI

`use strict`;

/// https://developer.mozilla.org/en-US/docs/Web/API/Response/arrayBuffer
/// https://medium.com/@patarkf/synchronize-your-asynchronous-code-using-javascripts-async-await-5f3fa5b1366d

import * as Util from './Util.js';

const STEPS_X = 70;
const STEPS_Y = 40;

const MAX_WORD = 2**15;
const POS_MASK = 2**15 - 1;

////////////////////////////////////////////////////////////////////////////////
/// Packed floats
function unpack16 (w) {
    let sign = (w > MAX_WORD)? -1.0 : 1.0;
    let u = w & POS_MASK;
    let m = (u / (POS_MASK >>> 3));
    return sign * (m**2);
}

////////////////////////////////////////////////////////////////////////////////
/// Cart to Polar

function deg (rad) {
    return rad * (180/Math.PI);
}

function angle (u, v) {
    let a = Math.PI + Math.atan2(u, v);
    if ( a >= Math.PI * 2) {
        a -= Math.PI * 2;
    }
    let d = deg(a);
    return d;
}

function abs (u, v) {
    let a = Math.sqrt(u**2 + v**2);
    return a;
}

////////////////////////////////////////////////////////////////////////////////
/// Tile Cache 

let tileCache = new Map();

function ensureCache(parent, key) {
    let cache = parent.get(key);
    if (!cache) {
        cache = new Map();
        parent.set(key, cache);
    }
    return cache;
}
function cacheGetTile (res, offset, lat0, lon0) {
    let offsetCache = tileCache.get(res);
    if (!offsetCache) return undefined;
    let latCache = offsetCache.get(offset);
    if (!latCache) return undefined;
    let lonCache = latCache.get(lat0);
    if (!lonCache) return undefined;
    return lonCache.get(lon0);
}

function cachePutTile (res, offset, lat0, lon0, tile) {
    let offsetCache = ensureCache(tileCache, res);
    let latCache = ensureCache(offsetCache, offset);
    let lonCache = ensureCache(latCache, lat0);
    lonCache.set(lon0, tile);
}

////////////////////////////////////////////////////////////////////////////////
/// Retrieve wind data from tile cache

function getValue(tile, offset, lat, lon) {
    let data = tile.data.find(entry => entry.offset == offset);
    let idx = lat * data.nLat + lon;
    return {
        "u": data.u[idx],
        "v": data.v[idx]
    }
}

function windData (offset, lat, lon) {
    let south = Math.floor(lat / 10) * 10;
    let north = Math.ceil(lat / 10) * 10;
    let west = Math.floor(lon / 10) * 10;
    let east = Math.ceil(lon / 10) * 10;
    let tile = cacheGetTile('1p00', 15, south, west);
    if (tile) {
        let rlat = lat - south;
        let lat0 = Math.floor(rlat);
        let lat1 = Math.ceil(rlat)
        let rlon = lon - west;
        let lon0 = Math.floor(rlon);
        let lon1 = Math.ceil(rlon);
        let w00 = getValue(tile, offset, lat0, lon0);
        let w01 = getValue(tile, offset, lat0, lon1);
        let w10 = getValue(tile, offset, lat1, lon0);
        let w11 = getValue(tile, offset, lat1, lon1);
        let u = bilinear(rlat-lat0, rlon-lon0, w00.u, w10.u, w01.u, w11.u);
        let v = bilinear(rlat-lat0, rlon-lon0, w00.v, w10.v, w01.v, w11.v);
        return {
            "direction": angle(u, v),
            "speed": abs(u, v)
        }
    } else {
        return {
            "direction": 315.0,
            "speed": 11.5
        }
    }
}

function bilinear(x, y, f00, f10, f01, f11) {
    return f00 * (1 - x) * (1 - y)
        + f10 * x * (1 - y)
        + f01 * (1 - x) * y
        + f11 * x * y;
}



////////////////////////////////////////////////////////////////////////////////
/// Plotting wind data
function drawWind (bounds, offset, canvas) {

    let rect = canvas.getBoundingClientRect();
    let geometry = {
        "width": rect.width * 6,
        "height": rect.height * 6
    }

    var ctx = canvas.getContext("2d");
    ctx.globalAlpha = 0.6;
    ctx.clearRect(0, 0, geometry.width, geometry.height);

    // Needed to transform Map points to Canvas points (origin (N,W) = (0,0))
    var projection = bounds.projection;
    var nwLatLng =  new google.maps.LatLng(bounds.north, bounds.west);
    var seLatLng =  new google.maps.LatLng(bounds.south, bounds.east);
    var nwPoint = projection.fromLatLngToPoint(nwLatLng);
    var sePoint = projection.fromLatLngToPoint(seLatLng);
    var mapWidth = (sePoint.x - nwPoint.x);
    var mapHeight = (sePoint.y - nwPoint.y);

    // Wind values are at evenly distributed lat/lng values
    var width = (bounds.east-bounds.west);
    var height = (bounds.south-bounds.north);
    var dlng = (width/STEPS_X);
    var dlat = (height/STEPS_Y);
    var dx = (sePoint.x - nwPoint.x) / STEPS_X;
    var dy = (sePoint.y - nwPoint.y) / STEPS_Y;

    // Shift Lat/Lng by 1/2 delta to center wind arrows on screen.
    for (var y = nwPoint.y; y < sePoint.y; y += dy) {
        for (var x = nwPoint.x; x < sePoint.x; x += dx) {
            var mapPoint = new google.maps.Point(x, y);
            var latLng = projection.fromPointToLatLng(mapPoint);
            var pointX = (mapPoint.x - nwPoint.x) / mapWidth * ctx.canvas.width;
            var pointY = (mapPoint.y - nwPoint.y) / mapHeight * ctx.canvas.height;
            let w = windData(offset, latLng.lat(), latLng.lng()); 
            drawWindArrow(ctx, pointX, pointY, w.direction, w.speed);
        }
    }

    // for ( var lat = bounds.north+dlat/2, y=0; y < STEPS_Y; lat += dlat, y++ ) {
    //     for ( var lng = bounds.west+dlng/2, x=0; x < STEPS_X; lng += dlng, x++ ) {
    //         var latLng = new google.maps.LatLng(lat, lng);
    //         var mapPoint = projection.fromLatLngToPoint(latLng);
    //         var pointX = (mapPoint.x - nwPoint.x) / mapWidth * ctx.canvas.width;
    //         var pointY = (mapPoint.y - nwPoint.y) / mapHeight * ctx.canvas.height;
    //         let w = windData(lat, lng); 
    //         drawWindArrow(ctx, pointX, pointY, w.direction, w.speed);
    //     }
    // }
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
/// Class WindTile 

/// ToDo: keep cache in class?

export default class WindTile {

    #canvas = undefined;
    #curBounds = undefined;
    #curTime = undefined;
    #curRes = undefined; 

    constructor (canvas, bounds, resolution, time) {
        this.#canvas = canvas;
        this.update(canvas, bounds, resolution, time);
    }

    async update (bounds, resolution = "1p00", time = new Date()) {
        this.#curBounds = bounds;
        this.#curTime = time;
        this.#curRes = resolution;
        await this.loadTiles(bounds);
        this.updateCanvas();
    }

    updateCanvas () {
        let offset = 6;
        drawWind(this.#curBounds, offset, this.#canvas);
        console.log('Updating canvas');
    }
    
    decodeTile (buffer) {
        const IDX_CYCLE = 5;
        const IDX_RES = 25;
        const IDX_NUMFC = 26;
        const IDX_DATA = 26;
        
        const tileData = new Uint16Array(buffer);
        let tile = {};
        
        let latLon = new Int16Array(tileData.slice(0, 4));
        tile.north = latLon[0];
        tile.south = latLon[1];
        tile.west = latLon[2];
        tile.east = latLon[3];
        
        let ts = String.fromCharCode.apply(String, tileData.slice(IDX_CYCLE, IDX_CYCLE+20))
        tile.cycle = new Date(ts);
        tile.resolution = tileData[IDX_RES];
        tile.nForecasts = tileData[IDX_NUMFC];
        
        tile.data = new Array(tile.nForecasts);
        let index = IDX_DATA;
        for (var k = 0; k < tile.nForecasts; k++) {
            let offset =  tileData[++index];
            let nLat =  tileData[++index];
            let nLon =  tileData[++index];
            index += 1;
            let valNum = new Int32Array(tileData.slice(index, index + 2))[0];
            index += 1;
            tile.data[k] = {
                "offset": offset,
                "nLat": nLat,
                "nLon": nLon,
                "u": new Array(valNum),
                "v": new Array(valNum)
            }
            for (var u = 0; u < tile.data[k].u.length; u++) {
                tile.data[k].u[u] = unpack16(tileData[++index]);
            }
            for (var v = 0; v < tile.data[k].v.length; v++) {
                tile.data[k].v[v] = unpack16(tileData[++index]);
            }
        }
        return tile;
    };

    handleError (request) {
    };

    tilePath (res, offset, lat0, lon0) {
        return `/tile/${res}/${offset}/${lat0}/${lon0}.dat`;
    }

    async getTile (res, offset, lat0, lon0) {
        let response = await fetch(this.tilePath(res, offset, lat0, lon0));
        if (!response.ok) {
            throw new Error(`HTTP error, status = ${response.status}`);
        }
        let buffer = await response.arrayBuffer();
        let tile = this.decodeTile(buffer);
        cachePutTile(res, offset, lat0, lon0, tile);
        return tile;
    }

    async getTileCached (res, offset, lat0, lon0) {
        let cached = cacheGetTile(res, offset, lat0, lon0);
        if (! cached) {
            cached = await this.getTile(res, offset, lat0, lon0);
        }
        return cached;
    }

    async loadTiles (bounds) {
        let south = Math.floor(bounds.south / 10) * 10;
        let north = Math.ceil(bounds.north / 10) * 10;
        let west = Math.floor(bounds.west / 10) * 10;
        let east = Math.ceil(bounds.east / 10) * 10;
        for (var lat = south; lat<north; lat += 10) {
            for (var lon = west; lon<east; lon += 10) {
                this.getTileCached('1p00', 15, lat, lon);
            }
        }
    }


    

}

/// EOF
////////////////////////////////////////////////////////////////////////////////
