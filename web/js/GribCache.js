////////////////////////////////////////////////////////////////////////////////
/// Retrieving, interpolatiing & plotting wind data

`use strict`;

import * as Util from './Util.js';
import {parseGrib2ByteArray} from './JSGrib/grib22json/index.js';

////////////////////////////////////////////////////////////////////////////////
/// Aux math functions

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

function getValue(grib, offset, lat, lon) {

    lat = 90 - lat;
    lon = lon>=0 ? lon : lon+360;
    
    let uData = grib[0].data.values;
    let vData = grib[1].data.values;

    let nLon = grib[0].data.grid.numLongPoints;
    let nLat = grib[0].data.grid.numLatPoints;

    let resLon = nLon/360;
    let resLat = (nLat-1)/180;

    let iLon = lon * resLon;
    let iLat = lat * resLon;

    let idx = iLat * nLon + iLon;
    
    return {
        "u": uData[idx],
        "v": vData[idx]
    }
}

function interpolateSpatial (grib, offset, lat0, lat1, lon0, lon1, lat, lon) {
    let w = {
        "w00": getValue(grib, offset, lat0, lon0),
        "w01": getValue(grib, offset, lat0, lon1),
        "w10": getValue(grib, offset, lat1, lon0),
        "w11": getValue(grib, offset, lat1, lon1)
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

function interpolateTemporal (grib0, grib1, offset0, offset1, timeFraction, lat0, lon0, lat1, lon1) {
    let p00 = getValue(grib0, offset0, lat0, lon0);
    let p01 = getValue(grib0, offset0, lat0, lon1);
    let p10 = getValue(grib0, offset0, lat1, lon0);
    let p11 = getValue(grib0, offset0, lat1, lon1);
    let q00 = getValue(grib1, offset1, lat0, lon0);
    let q01 = getValue(grib1, offset1, lat0, lon1);
    let q10 = getValue(grib1, offset1, lat1, lon0);
    let q11 = getValue(grib1, offset1, lat1, lon1);
    return {
        "w00": interpolatePoint(p00.u, q00.u, p00.v, q00.v, timeFraction),
        "w01": interpolatePoint(p01.u, q01.u, p01.v, q01.v, timeFraction),
        "w10": interpolatePoint(p10.u, q10.u, p10.v, q10.v, timeFraction),
        "w11": interpolatePoint(p11.u, q11.u, p11.v, q11.v, timeFraction)
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
/// Class GribCache 

/// ToDo: keep cache in class?

export default class GribCache {

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
        let offset = floor((curTime - baseTime) / 3600000, 3);
        let grib0 = await this.getGribCached(cycle, resolution, offset);
        let grib1 = await this.getGribCached(cycle, resolution, offset + 3);
        if (grib0 && grib1) {
            let lat0 = floor(lat), lat1 = ceil(lat)
            let lon0 = floor(lon), lon1 = ceil(lon);
            let lambdaU = lat-lat0, lambdaV = lon-lon0;
            let timeFraction = ((time - cycle) / 3600000 - offset) / 3;
            let w_t = interpolateTemporal(grib0, grib1, offset, offset+3, timeFraction, lat0, lon0, lat1, lon1)
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

    async windDataMagnitudeVR25 (cycle, resolution, time, lat, lon) {
        let south = Math.floor(lat / 10) * 10;
        let north = Math.ceil(lat / 10) * 10;
        let west = Math.floor(lon / 10) * 10;
        let east = Math.ceil(lon / 10) * 10;

        let curTime = new  Date(time);
        
        let baseTime1 = new Date(cycle);
        let offset1 = ceil((curTime - baseTime1) / 3600000, 3);
        let baseTime0, offset0;
        
        if (offset1 <= 6) {
            // use previous forecast only
            baseTime0 = new Date(baseTime1 - (6 * 60 * 60 * 1000));
            offset0 = floor((curTime - baseTime0) / 3600000, 3);
            baseTime1 = baseTime0;
            offset1 = offset0 + 3;
        } else if (offset1 == 9) {
            // merge with previous
            baseTime0 = new Date(baseTime1 - (6 * 60 * 60 * 1000));
            offset0 = floor((curTime - baseTime0) / 3600000, 3);
        } else {
            // use current forecast only
            baseTime0 = baseTime1;
            offset0 = offset1 - 3;
        }
        
        let grib0 = await this.getGribCached(baseTime0, resolution, offset0);
        let grib1 = await this.getGribCached(baseTime1, resolution, offset1);
        if (grib0 && grib1) {
            let rlat = lat - south,  rlon = lon - west;
            let lat0 = floor(rlat), lat1 = ceil(rlat)
            let lon0 = floor(rlon), lon1 = ceil(rlon);
            let lambdaU = rlat-lat0, lambdaV = rlon-lon0;
            let w_t = interpolateTemporal(grib0, grib1, offset0, offset1, time, lat0, lon0, lat1, lon1)
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
    /// Grib Cache 

    #gribCache = new Map();

    cacheGetGrib (cycle, res, offset) {
        let cycleStr = this.formatCycle(cycle);
        let key = `${cycleStr}+${res}+${offset}`;
        let grib = this.#gribCache.get(key);
        if (! grib) {
            return undefined;
        }
        return grib;
    }

    cachePutGrib (cycle, res, offset, grib) {
        let cycleStr = this.formatCycle(cycle);
        let key = `${cycleStr}+${res}+${offset}`;
        this.#gribCache.set(key, grib);
    }

    ////////////////////////////////////////////////////////////////////////////////
    /// Public methods
    async getWind (lat, lon, time = this.#curTime) {
        return await this.windDataMagnitude(this.#curCycle, this.#curRes, time, lat, lon);
    }
    
    async getWindVR (lat, lon, time = this.#curTime, cycle=this.#curCycle, res=this.#curRes) {
        return await this.windDataMagnitudeVR25(cycle, res, time, lat, lon);
    }
    
    async update (bounds, cycle, time = new Date(), resolution = "1p00") {
        let curTime = new  Date(time);
        let baseTime = new Date(cycle);
        let offset = floor((curTime - baseTime) / 3600000, 3);
        this.#curBounds = bounds;
        this.#curCycle = baseTime;
        this.#curOffset = offset;
        this.#curTime = curTime;
        this.#curRes = resolution;
        await this.getGribCached(cycle, resolution, offset);
        this.updateCanvas();
    }

    ////////////////////////////////////////////////////////////////////////////////
    /// Internal methods
    
    updateCanvas () {
        this.drawWind(this.#curBounds, this.#curCycle, this.#curRes, this.#curOffset, this.#curTime, this.#canvas);
    }
    
    handleError (request) {
    };

    gribPath (cycle, res, offset) {
        let cycleDirStr = this.formatCycle(cycle, '/');
        let cycleFileStr = this.formatCycle(cycle, '.');
        let offsetStr = offset.toFixed().padStart(3, '0');
        return `/weather/vr/${res}/${cycleDirStr}/${cycleFileStr}.${offsetStr}.${res}.grb`;
    }

    formatCycle (d, sep = '-') {
        let month = (d.getUTCMonth()+1).toFixed().padStart(2, '0');
        let day = d.getUTCDate().toFixed().padStart(2, '0');
        let hour = (Math.floor(d.getUTCHours()/6)*6).toFixed().padStart(2, '0');
        return `${d.getUTCFullYear()}${month}${day}${sep}${hour}`;
    }
    
    async getGrib (cycle, res, offset) {
        try {
            let response = await fetch(this.gribPath(cycle, res, offset));
            if (!response.ok) {
                throw new Error(`${response.statusText}`);
            }
            let buffer = await response.arrayBuffer();
            let grib = parseGrib2ByteArray(buffer);
            this.cachePutGrib(cycle, res, offset, grib);
            return grib;
        } catch (e) {
            throw new Error(e);
        }
    }

    async getGribCached (cycle, res, offset) {
        let cached = this.cacheGetGrib(cycle, res, offset);
        if (! cached) {
            cached = await this.getGrib(cycle, res, offset);
        }
        return cached;
    }
        
}

/// EOF
////////////////////////////////////////////////////////////////////////////////
