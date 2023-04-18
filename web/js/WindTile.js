////////////////////////////////////////////////////////////////////////////////
/// Retrieving, interpolatiing & plotting wind data

`use strict`;

import * as Util from './Util.js';

const MAX_WORD = 2**15;
const POS_MASK = 2**15 - 1;

////////////////////////////////////////////////////////////////////////////////
/// Aux math functions

function unpack16 (w) {
    let sign = (w > MAX_WORD)? -1.0 : 1.0;
    let u = w & POS_MASK;
    let m = (u / (POS_MASK >>> 3));
    return sign * (m**2);
}

function floor (n, d=1) {
    return  Math.floor(n/d)*d;
}

function ceil (n, d=1) {
    return  Math.ceil(n/d)*d;
}

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

function linear (a, b, lambda) {
    return (1 - lambda) * a + lambda * b;
}

function bilinear(x, y, f00, f10, f01, f11) {
    let w0 = f00 + x * (f10 - f00);
    let w1 = f01 + x * (f11 - f01);
    return w0 + y * (w1 - w0);
    
}

////////////////////////////////////////////////////////////////////////////////
/// Interpolation

function getValue(tile, offset, lat, lon) {
    let data = tile.data.find(entry => entry.offset == offset);
    let idx = lat * data.nLat + lon;
    return {
        "u": data.u[idx],
        "v": data.v[idx]
    }
}

function interpolateSpatial (tile, offset, lat0, lat1, lon0, lon1, lat, lon) {
    let w = {
        "w00": getValue(tile, offset, lat0, lon0),
        "w01": getValue(tile, offset, lat0, lon1),
        "w10": getValue(tile, offset, lat1, lon0),
        "w11": getValue(tile, offset, lat1, lon1)
    }
    return interpolateSpatial_UV(lat-lat0, lon-lon0, w);
}

function interpolateSpatial_UV (lambdaU, lambdaV, w) {
    return {
        "u": bilinear(lambdaU, lambdaV, w.w00.u, w.w10.u, w.w01.u, w.w11.u),
        "v": bilinear(lambdaU, lambdaV, w.w00.v, w.w10.v, w.w01.v, w.w11.v)
    }
}

function interpolatePoint (u0, u1, v0, v1, fraction) {
    let u = linear(u0, u1, fraction);
    let v = linear(v0, v1, fraction);
    return  {
        "u": u,
        "v": v,
        "s": linear(abs(u0, v0), abs(u1, v1), fraction),
        "d": angle(u, v)
    }
}

function interpolateTemporal (tile0, tile1, offset, time, lat0, lon0, lat1, lon1) {
    let timeFraction = ((time - tile0.cycle) / 3600000 - offset) / 3;
    let p00 = getValue(tile0, offset, lat0, lon0);
    let p01 = getValue(tile0, offset, lat0, lon1);
    let p10 = getValue(tile0, offset, lat1, lon0);
    let p11 = getValue(tile0, offset, lat1, lon1);
    let q00 = getValue(tile1, offset+3, lat0, lon0);
    let q01 = getValue(tile1, offset+3, lat0, lon1);
    let q10 = getValue(tile1, offset+3, lat1, lon0);
    let q11 = getValue(tile1, offset+3, lat1, lon1);
    return {
        "w00": interpolatePoint(p00.u, q00.u, p00.v, q00.v, timeFraction),
        "w01": interpolatePoint(p01.u, q01.u, p01.v, q01.v, timeFraction),
        "w10": interpolatePoint(p10.u, q10.u, p10.v, q10.v, timeFraction),
        "w11": interpolatePoint(p11.u, q11.u, p11.v, q11.v, timeFraction)
    }
}


function windDataBilinear (cycle, resolution, offset, time, lat, lon) {
    let south = Math.floor(lat / 10) * 10;
    let north = Math.ceil(lat / 10) * 10;
    let west = Math.floor(lon / 10) * 10;
    let east = Math.ceil(lon / 10) * 10;
    let tile0 = cacheGetTile(cycle, resolution, offset, south, west);
    let tile1 = cacheGetTile(cycle, resolution, offset + 3, south, west);
    if (tile0 && tile1) {
        let d = tile0.resolution/100;
        let rlat = lat - south,  rlon = lon - west;
        let drlat = rlat / d, drlon = rlon / d;
        let lat0 = floor(rlat, d)/d, lat1 = ceil(rlat, d)/d
        let lon0 = floor(rlon, d)/d, lon1 = ceil(rlon, d)/d;
        let w0 = interpolateSpatial(tile0, offset, lat0, lat1, lon0, lon1, drlat, drlon);
        let w1 = interpolateSpatial(tile1, offset + 3, lat0, lat1, lon0, lon1, drlat, drlon);
        let timeFraction = ((time - tile0.cycle) / 3600000 - offset) / 3;
        let w = {
            "u": linear(w0.u, w1.u, timeFraction),
            "v": linear(w0.v, w1.v, timeFraction)
        }
        return {
            "direction": angle(w.u, w.v),
            "speed": abs(w.u, w.v)
        }
    } else {
        return {
            "direction": 315.0,
            "speed": 11.5
        }
    }
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
    var scale = (speed>0)?0.5 + speed/50:0;
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
    #curCycle = undefined;
    #curOffset = undefined;
    #curTime = undefined;
    #curRes = undefined; 

    constructor (canvas, bounds, resolution, time) {
        this.#canvas = canvas;
    }

    ////////////////////////////////////////////////////////////////////////////////
    /// Interpolation
    
    async windDataMagnitude (cycle, resolution, time, lat, lon) {
        let curTime = new  Date(time);
        let baseTime = new Date(cycle);
        let cycleStr = this.formatCycle(cycle);
        let offset = floor((curTime - baseTime) / 3600000, 3);
        let south = Math.floor(lat / 10) * 10;
        let north = Math.ceil(lat / 10) * 10;
        let west = Math.floor(lon / 10) * 10;
        let east = Math.ceil(lon / 10) * 10;
        let tile0 = await this.getTileCached(cycleStr, resolution, offset, south, west);
        let tile1 = await this.getTileCached(cycleStr, resolution, offset + 3, south, west);
        if (tile0 && tile1) {
            let d = tile0.resolution/100;
            let rlat = lat - south,  rlon = lon - west;
            let drlat = rlat / d, drlon = rlon / d;
            let lat0 = floor(rlat, d)/d, lat1 = ceil(rlat, d)/d
            let lon0 = floor(rlon, d)/d, lon1 = ceil(rlon, d)/d;
            let lambdaU = drlat-lat0, lambdaV = drlon-lon0;
            let w_t = interpolateTemporal(tile0, tile1, offset, time, lat0, lon0, lat1, lon1)
            let w_s = interpolateSpatial_UV(lambdaU, lambdaV, w_t);
            let direction = angle(w_s.u, w_s.v);
            let speed = bilinear(lambdaU, lambdaV, w_t.w00.s, w_t.w10.s, w_t.w01.s, w_t.w11.s);
            return {
                "direction": direction,
                "speed": speed
            }
        } else {
            return {
                "direction": 315.0,
                "speed": 11.5
            }
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    /// Plotting wind data
    
    async drawWind (bounds, cycle, resolution, offset, time, canvas) {
        
        let rect = canvas.getBoundingClientRect();
        // Only draw if canvas is displayed (wind display enabled)
        if (rect) {
            let STEPS_X = floor(rect.width/15);
            let STEPS_Y = floor(rect.height/15);
            
            var ctx = canvas.getContext("2d");
            ctx.globalAlpha = 0.3;
            ctx.clearRect(0, 0, canvas.width, canvas.height);
            
            // Draw wind arrows are at evenly spaced map points
            // Needed to transform Map points to Canvas points (origin (N,W) = (0,0))
            var projection = bounds.projection;
            var nwLatLng =  new google.maps.LatLng(bounds.north, bounds.west);
            var seLatLng =  new google.maps.LatLng(bounds.south, bounds.east);
            var nwPoint = projection.fromLatLngToPoint(nwLatLng);
            var sePoint = projection.fromLatLngToPoint(seLatLng);
            var top = nwPoint.y;
            var bottom = sePoint.y;
            var left =  nwPoint.x;
            var right = sePoint.x;
            
            var dy = (bottom - top) / STEPS_Y;
            
            if (left < right) {
                var dx = (right - left) / STEPS_X;
                await this.drawMap(ctx, projection, cycle, resolution, offset, time, dx, dy, top, bottom, left, right, 0, right - left);
            } else {
                // left > right
                var dx = (256 - left + right) / STEPS_X;
                let mapWidthLeft = 256 - left;
                let mapWidth = mapWidthLeft + right;
                let canvasLeft = mapWidthLeft/mapWidth * ctx.canvas.width;
                await this.drawMap(ctx, projection, cycle, resolution, offset, time, dx, dy, top, bottom, left, 256, 0, mapWidth);
                await this.drawMap(ctx, projection, cycle, resolution, offset, time, dx, dy, top, bottom, 0, right, canvasLeft, mapWidth);
            }
        }
    }

    async drawMap (ctx, projection, cycle, resolution, offset, time, dx, dy, top, bottom, left, right, canvasLeft, mapWidth) {
        var mapHeight = bottom - top;
        for (var y = top; y < bottom; y += dy) {
            for (var x = left; x < right; x += dx) {
                var mapPoint = new google.maps.Point(x, y);
                var latLng = projection.fromPointToLatLng(mapPoint);
                var pointX = canvasLeft + ((x - left) /  mapWidth) * ctx.canvas.width;
                var pointY = (y - top) / mapHeight * ctx.canvas.height;
                let w = await this.windDataMagnitude(cycle, resolution, time, latLng.lat(), latLng.lng()); 
                drawWindArrow(ctx, pointX, pointY, w.direction, w.speed);
            }
        }
    }
    
    ////////////////////////////////////////////////////////////////////////////////
    /// Tile Cache 

    #tileCache = new Map();

    ensureCache(parent, key) {
        let cache = parent.get(key);
        if (!cache) {
            cache = new Map();
            parent.set(key, cache);
        }
        return cache;
    }

    cacheGetTile (cycleStr, res, offset, lat0, lon0) {
        let cycleCache = this.#tileCache.get(cycleStr);
        if (!cycleCache) {
            return undefined;
        }
        let offsetCache = cycleCache.get(res);
        if (!offsetCache) {
            return undefined;
        }
        let latCache = offsetCache.get(offset);
        if (!latCache) {
            return undefined;
        }
        let lonCache = latCache.get(lat0);
        if (!lonCache) {
            return undefined;
        }
        let tile = lonCache.get(lon0);
        if (!tile) {
            return undefined;
        }
        return tile;
    }

    cachePutTile (cycle, res, offset, lat0, lon0, tile) {
        let cycleCache = this.ensureCache(this.#tileCache, cycle);
        let offsetCache = this.ensureCache(cycleCache, res);
        let latCache = this.ensureCache(offsetCache, offset);
        let lonCache = this.ensureCache(latCache, lat0);
        lonCache.set(lon0, tile);
    }

    ////////////////////////////////////////////////////////////////////////////////
    /// Public methods
    async getWind (lat, lon, time = this.#curTime) {
        return await this.windDataMagnitude(this.#curCycle, this.#curRes, time, lat, lon);
    }
    
    async update (bounds, cycle, time = new Date(), resolution = "1p00") {
        let curTime = new  Date(time);
        let baseTime = new Date(cycle);
        let cycleStr = this.formatCycle(baseTime);
        let offset = floor((curTime - baseTime) / 3600000, 3);
        this.#curBounds = bounds;
        this.#curCycle = baseTime;
        this.#curOffset = offset;
        this.#curTime = curTime;
        this.#curRes = resolution;
        await this.loadTiles(bounds, cycleStr, offset, resolution);
        this.updateCanvas();
    }

    ////////////////////////////////////////////////////////////////////////////////
    /// Internal methods
    
    updateCanvas () {
        this.drawWind(this.#curBounds, this.#curCycle, this.#curRes, this.#curOffset, this.#curTime, this.#canvas);
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

    tilePath (cycle, res, offset, lat0, lon0) {
        return `/tile/${cycle}/${res}/${offset}/${lat0}/${lon0}.dat`;
    }

    formatCycle (d) {
        let month = (d.getUTCMonth()+1).toFixed().padStart(2, '0');
        let day = d.getUTCDate().toFixed().padStart(2, '0');
        let hour = (Math.floor(d.getUTCHours()/6)*6).toFixed().padStart(2, '0');
        return `${d.getUTCFullYear()}${month}${day}-${hour}`
    }
    
    async getTile (cycleString, res, offset, lat0, lon0) {
        let offsetStr = offset.toFixed().padStart(3, '0');
        let latStr = lat0.toFixed().padStart(3, '0');
        let lonStr = lon0.toFixed().padStart(3, '0');
        try {
            let response = await fetch(this.tilePath(cycleString, res, offsetStr, latStr, lonStr));
            if (!response.ok) {
                throw new Error(`${response.statusText}`);
            }
            let buffer = await response.arrayBuffer();
            let tile = this.decodeTile(buffer);
            this.cachePutTile(cycleString, res, offset, lat0, lon0, tile);
            return tile;
        } catch (e) {
            throw new Error(e);
        }
    }

    async getTileCached (cycleStr, res, offset, lat0, lon0) {
        let cached = this.cacheGetTile(cycleStr, res, offset, lat0, lon0);
        if (! cached) {
            cached = await this.getTile(cycleStr, res, offset, lat0, lon0);
        }
        return cached;
    }

    async loadTilesInRange (cycleStr, resolution, offset, north, south, west, east) {
        for (var lat = south; lat<north; lat += 10) {
            for (var lon = west; lon<east; lon += 10) {
                await this.getTileCached(cycleStr, resolution, offset, lat, lon);
            }
        }
    }
        
    async loadTiles (bounds, cycleStr, offset, resolution = '1p00') {
        let south = Math.floor(bounds.south / 10) * 10;
        let north = Math.ceil(bounds.north / 10) * 10;
        let west = Math.floor(bounds.west / 10) * 10;
        let east = Math.ceil(bounds.east / 10) * 10;
        if (west < east) {
            await this.loadTilesInRange(cycleStr, resolution, offset, north, south, west, east);
            await this.loadTilesInRange(cycleStr, resolution, offset + 3, north, south, west, east);
        } else {
            await this.loadTilesInRange(cycleStr, resolution, offset, north, south, west, 180);
            await this.loadTilesInRange(cycleStr, resolution, offset, north, south, -180, east);
            await this.loadTilesInRange(cycleStr, resolution, offset + 3, north, south, west, 180);
            await this.loadTilesInRange(cycleStr, resolution, offset + 3, north, south, -180, east);
        }
    }
        
}

/// EOF
////////////////////////////////////////////////////////////////////////////////
