////////////////////////////////////////////////////////////////////////////////
/// Retrieving, interpolatiing & plotting wind data

`use strict`;

import * as Util from './Util.js';
import {decodeGRIB2ArrayBuffer} from './JSGrib/JSGrib.js';
import {colors, ms2bf} from './colors.js'; 
import { ensureServerSettings, getServerSettings, loadServerSettings, loadForecastCycle } from './ServerSettings.js?v=router-forecast-contract-20261031';

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

function getValue(grib, lat, lon) {
    const grid = grib.messages[0].GRID_DEFINITION_SECTION;
    const uMessage = ['UGRD', '10U', '10u']
        .map((name) => grib.getMessageByParameterName(name))
        .find(Boolean);
    const vMessage = ['VGRD', '10V', '10v']
        .map((name) => grib.getMessageByParameterName(name))
        .find(Boolean);
    if (!uMessage || !vMessage) {
        throw new Error('GRIB is missing 10 m U/V wind components');
    }

    const uData = uMessage.DATA_SECTION.data;
    const vData = vMessage.DATA_SECTION.data;
    const nLon = grid.Ni;
    const nLat = grid.Nj;

    let resLon = nLon/360;
    let resLat = (nLat-1)/180;

    const basicAngle = grid.basicAngleOfTheInitialProductionDomain;
    const subdivisions = grid.subdivisionsOfBasicAngle;
    const angleScale = basicAngle && subdivisions ? basicAngle / subdivisions : 1e-6;
    const firstLongitude = grid.longitudeOfFirstGridPoint * angleScale;
    const firstLatitude = grid.latitudeOfFirstGridPoint * angleScale;
    const gridLongitude = wrapLonDeg(lon - firstLongitude);
    const gridLatitude = firstLatitude - lat;
    const iLon = Math.round(gridLongitude * resLon) % nLon;
    const iLat = Math.round(gridLatitude * resLat);

    let idx = iLat * nLon + iLon;
    
    return {
        "u": uData[idx],
        "v": vData[idx]
    }
}

function interpolateSpatial_UV (lambdaU, lambdaV, w) {
    return {
        "u": bilinear(lambdaU, lambdaV, w.w00.u, w.w10.u, w.w01.u, w.w11.u),
        "v": bilinear(lambdaU, lambdaV, w.w00.v, w.w10.v, w.w01.v, w.w11.v),
        "magnitude": bilinear(lambdaU, lambdaV, w.w00.magnitude, w.w10.magnitude, w.w01.magnitude, w.w11.magnitude)
    }
}

function interpolatePoint (u0, u1, v0, v1, fraction) {
    let u = linear(u0, u1, fraction);
    let v = linear(v0, v1, fraction);
    return { "u": u, "v": v, "magnitude": linear(abs(u0, v0), abs(u1, v1), fraction) };
}

function interpolateTemporal (grib0, grib1, timeFraction, lat0, lon0, lat1, lon1) {
    let p00 = getValue(grib0, lat0, lon0);
    let p01 = getValue(grib0, lat0, lon1);
    let p10 = getValue(grib0, lat1, lon0);
    let p11 = getValue(grib0, lat1, lon1);
    let q00 = getValue(grib1, lat0, lon0);
    let q01 = getValue(grib1, lat0, lon1);
    let q10 = getValue(grib1, lat1, lon0);
    let q11 = getValue(grib1, lat1, lon1);
    return {
        "w00": interpolatePoint(p00.u, q00.u, p00.v, q00.v, timeFraction),
        "w01": interpolatePoint(p01.u, q01.u, p01.v, q01.v, timeFraction),
        "w10": interpolatePoint(p10.u, q10.u, p10.v, q10.v, timeFraction),
        "w11": interpolatePoint(p11.u, q11.u, p11.v, q11.v, timeFraction)
    }
}

// Add a tiny helper to wrap longitude into [0, 360)
function wrapLonDeg(lon) {
    return ((lon % 360) + 360) % 360;
}

////////////////////////////////////////////////////////////////////////////////
/// Class GribCache 

/// ToDo: keep cache in class?

export default class GribCache {

    #canvas = undefined;
    #curCycle = undefined;
    #curTime = undefined;
    #forecastModel = 'NOAA-GFS-WIND';
    #updateVersion = 0;
    #cycleSources = new Map();
    // Server settings are handled by shared ServerSettings module

    constructor (canvas, forecastModel = 'NOAA-GFS-WIND') {
        // Prefetch server settings via shared module
        loadServerSettings();
        this.#canvas = canvas;
        this.#forecastModel = forecastModel || 'NOAA-GFS-WIND';
    }
    
    
    
    ////////////////////////////////////////////////////////////////////////////////
    /// Interpolation
    
    async windDataMagnitude (cycle, time, lat, lon) {
        let curTime = new  Date(time);
        let baseTime = new Date(cycle);
        let offset = (curTime - baseTime) / 3600000;
        let frames = await this.getForecastFrames(baseTime, offset);
        let grib0 = await this.getGribCached(baseTime, frames.before);
        let grib1 = frames.before === frames.after
            ? grib0
            : await this.getGribCached(baseTime, frames.after);
        if (grib0 && grib1) {
            const nLon = grib0.messages[0].GRID_DEFINITION_SECTION.Ni;
            const nLat = grib0.messages[0].GRID_DEFINITION_SECTION.Nj;
            const stepLon = 360 / nLon;
            const stepLat = 180 / (nLat - 1);
            const wLon = wrapLonDeg(lon);
            const lat0 = floor(lat, stepLat), lat1 = ceil(lat, stepLat);
            let   lon0 = floor(wLon, stepLon), lon1 = lon0 + stepLon;
            if (lon1 >= 360) lon1 -= 360;
            const lambdaU = (lat - lat0) / stepLat;
            const lambdaV = (wLon - lon0) / stepLon;
            const timeFraction = frames.before === frames.after
                ? 0
                : (offset - frames.before) / (frames.after - frames.before);
            let w_t = interpolateTemporal(grib0, grib1, timeFraction, lat0, lon0, lat1, lon1);
            let w_s = interpolateSpatial_UV(lambdaU, lambdaV, w_t);
            let direction = angle(w_s.u, w_s.v);
            // Match CL-WEATHER: interpolate magnitudes separately from U/V.
            let speed = w_s.magnitude;
            return { "direction": direction, "speed": speed };
        } else {
            return { "direction": 315.0, "speed": 11.5 };
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    /// Plotting wind data
    
    async drawWind (bounds, cycle, offset, time, canvas, isCurrent) {
        
        let rect = canvas.getBoundingClientRect();
        // Only draw if canvas is displayed (wind display enabled)
        if (rect) {
            let STEPS_X = floor(rect.width/15);
            let STEPS_Y = floor(rect.height/15);
            
            // Keep the displayed forecast intact until the entire new frame is ready.
            const frame = canvas.cloneNode(false);
            var ctx = frame.getContext("2d");
            ctx.globalAlpha = 0.3;
            
            // Draw wind arrows are at evenly spaced map points
            // Needed to transform Map points to Canvas points (origin (N,W) = (0,0))
            var map = bounds.map;
            var nwLatLng =  L.latLng(bounds.north, bounds.west);
            var seLatLng =  L.latLng(bounds.south, bounds.east);
            var nwPoint = map.project(nwLatLng);
            var sePoint = map.project(seLatLng);
            var top = nwPoint.y;
            var bottom = sePoint.y;
            var left =  nwPoint.x;
            var right = sePoint.x;
            
            var dy = (bottom - top) / STEPS_Y;
            
            if (left < right) {
                var dx = (right - left) / STEPS_X;
                await this.drawMap(ctx, map, cycle, offset, time, dx, dy, top, bottom, left, right, 0, right - left);
            } else {
                // left > right
                var dx = (256 - left + right) / STEPS_X;
                let mapWidthLeft = 256 - left;
                let mapWidth = mapWidthLeft + right;
                let canvasLeft = mapWidthLeft/mapWidth * ctx.canvas.width;
                await this.drawMap(ctx, map, cycle, offset, time, dx, dy, top, bottom, left, 256, 0, mapWidth);
                await this.drawMap(ctx, map, cycle, offset, time, dx, dy, top, bottom, 0, right, canvasLeft, mapWidth);
            }
            if (isCurrent()) {
                const display = canvas.getContext("2d");
                display.clearRect(0, 0, canvas.width, canvas.height);
                display.drawImage(frame, 0, 0);
            }
        }
    }

    async drawMap (ctx, projection, cycle, offset, time, dx, dy, top, bottom, left, right, canvasLeft, mapWidth) {
        var mapHeight = bottom - top;
        for (var y = top; y < bottom; y += dy) {
            for (var x = left; x < right; x += dx) {
                var mapPoint = L.point(x, y);
                var latLng = projection.unproject(mapPoint);
                var pointX = canvasLeft + ((x - left) /  mapWidth) * ctx.canvas.width;
                var pointY = (y - top) / mapHeight * ctx.canvas.height;
                let w = await this.windDataMagnitude(cycle, time, latLng.lat, latLng.lng);
                this.drawWindArrow(ctx, pointX, pointY, w.direction, w.speed);
            }
        }
    }

    drawWindArrow(ctx, x, y, direction, speed) {
        direction = direction + 90;
        if (direction > 360) {
            direction = direction - 360;
        }
        ctx.save();
        ctx.beginPath();
        ctx.fillStyle = colors[ms2bf(speed)];
        ctx.strokeStyle = colors[ms2bf(speed)];
        ctx.translate(x , y);
        ctx.rotate((direction*Math.PI/180));
        var scale = (speed>0)?0.5 + speed/50:0;
        ctx.scale(scale, scale);
        ctx.moveTo(-0.00, 0.00);
        ctx.lineTo(-15 * 6, 12 * 6);
        ctx.lineTo(18 * 6, 0);
        ctx.lineTo(-15 * 6, -12 * 6);
        ctx.closePath()
        ctx.fill();
        ctx.stroke();
        ctx.restore();
    }
    
    ////////////////////////////////////////////////////////////////////////////////
    /// Grib Cache 

    #gribCache = new Map();
    // Track in-flight fetch/decode promises to dedupe concurrent requests
    #gribInflight = new Map();

    clearCache () {
        this.#gribCache.clear();
        this.#gribInflight.clear();
    }

    cacheGetGrib (cycle, offset) {
        let cycleStr = this.formatCycle(cycle);
        let key = `${cycleStr}+${offset}`;
        let grib = this.#gribCache.get(key);
        if (! grib) {
            return undefined;
        }
        return grib;
    }

    cachePutGrib (cycle, offset, grib) {
        let cycleStr = this.formatCycle(cycle);
        let key = `${cycleStr}+${offset}`;
        this.#gribCache.set(key, grib);
    }

    ////////////////////////////////////////////////////////////////////////////////
    /// Public methods
    async getWind (lat, lon, time = this.#curTime) {
        if (!this.#curCycle || !time) return null;
        return await this.windDataMagnitude(this.#curCycle, time, lat, lon);
    }
    
    async update (bounds, cycle, time = new Date(), requestIsCurrent = () => true) {
        let curTime = new  Date(time);
        let baseTime = new Date(cycle);
        let offset = (curTime - baseTime) / 3600000;
        const version = ++this.#updateVersion;
        // Prepare both fields before publishing the new state. Failed loads
        // must not redirect mouse lookups to an unavailable cycle/time.
        let frames = await this.getForecastFrames(baseTime, offset);
        await this.getGribCached(baseTime, frames.before);
        if (frames.after !== frames.before) {
            await this.getGribCached(baseTime, frames.after);
        }
        const isCurrent = () => version === this.#updateVersion && requestIsCurrent();
        if (!isCurrent()) return;
        await this.drawWind(bounds, baseTime, offset, curTime, this.#canvas, isCurrent);
        if (!isCurrent()) return;
        this.#curCycle = baseTime;
        this.#curTime = curTime;
    }

    ////////////////////////////////////////////////////////////////////////////////
    /// Internal methods
    
    async ensureCycle(cycle) {
        const key = cycle.toISOString();
        if (this.#cycleSources.has(key)) return;
        await ensureServerSettings();
        const name = String(this.#forecastModel).replace(/^.*::/, '').toUpperCase();
        const date = key.slice(0, 10).replace(/-/g, '');
        const run = cycle.getUTCHours();
        const find = settings => settings?.datasources?.find(ds => String(ds.name).toUpperCase() === name);
        let source = find(getServerSettings());
        if (!source?.gribpaths?.some(path => String(path.date) === date && Number(path.run) === run)) {
            source = find(await loadForecastCycle(name, cycle));
        }
        if (!source) throw new Error(`No metadata for ${name} cycle ${key}`);
        this.#cycleSources.set(key, source);
    }

    async getForecastFrames (cycle, offset) {
        await this.ensureCycle(cycle);
        const ss = { datasources: [this.#cycleSources.get(cycle.toISOString())] };
        const selectedName = String(this.#forecastModel).replace(/^.*::/, '').toUpperCase();
        const dataSource = ss && Array.isArray(ss.datasources)
            ? ss.datasources.find((entry) => String(entry.name).toUpperCase() === selectedName)
            : undefined;
        if (!dataSource) {
            throw new Error(`Server settings missing datasource ${selectedName}`);
        }

        const date = cycle.toISOString().substring(0, 10).replace(/-/g, '');
        const run = cycle.getUTCHours();
        const paths = Array.isArray(dataSource.gribpaths) ? dataSource.gribpaths : [];
        const steps = paths
            .filter((entry) => String(entry.date) === date && Number(entry.run) === run)
            .map((entry) => Number(entry.step))
            .filter(Number.isFinite)
            .sort((a, b) => a - b);
        if (steps.length === 0) {
            throw new Error(`No GRIB steps for ${selectedName} cycle ${date} ${run}Z`);
        }

        const beforeSteps = steps.filter((step) => step <= offset);
        const before = beforeSteps[beforeSteps.length - 1];
        const after = steps.find((step) => step >= offset);
        if (before === undefined) {
            return { before: steps[0], after: steps[0] };
        }
        if (after === undefined) {
            return { before: steps[steps.length - 1], after: steps[steps.length - 1] };
        }
        return { before, after };
    }

    gribPath (cycle, offset) {
        // Assumes settings are loaded; caller should gate via ensureServerSettings()
        const ss = { datasources: [this.#cycleSources.get(cycle.toISOString())] };
        let selectedName = typeof this.#forecastModel === 'string'
            ? this.#forecastModel
            : String(this.#forecastModel);
        selectedName = selectedName.replace(/^.*::/, '').toUpperCase();

        const dataSource = ss && ss.datasources
            ? ss.datasources.find((entry) => {
                const name = typeof entry.name === 'string' ? entry.name : String(entry.name);
                return name.toUpperCase() === selectedName;
            })
            : undefined;
        if (!dataSource) {
            throw new Error(`Server settings missing datasource ${selectedName}`);
        }
        let run = cycle.getUTCHours().toFixed();
        const date = cycle.toISOString().substring(0, 10).replace(/-/g, '');
        let entry = dataSource.gribpaths.find((p) =>
            p.step == offset && p.run == run && String(p.date) === date);
        if (!entry) {
            throw new Error(`No GRIB path for run ${run} step ${offset}`);
        }
        return `/weather/${entry.path}`;
    }

    formatCycle (d, sep = '-') {
        let month = (d.getUTCMonth()+1).toFixed().padStart(2, '0');
        let day = d.getUTCDate().toFixed().padStart(2, '0');
        let hour = d.getUTCHours().toFixed().padStart(2, '0');
        return `${d.getUTCFullYear()}${month}${day}${sep}${hour}`;
    }
    
    async getGrib (cycle, offset) {
        await this.ensureCycle(cycle);
        const path = this.gribPath(cycle, offset);
        const response = await fetch(path);
        if (!response.ok) {
            throw new Error(`GRIB request failed: HTTP ${response.status} ${response.statusText} (${path})`);
        }
        const buffer = await response.arrayBuffer();
        const grib = decodeGRIB2ArrayBuffer(buffer);
        this.cachePutGrib(cycle, offset, grib);
        return grib;
    }

    async getGribCached (cycle, offset) {
        // Fast path: decoded object already cached
        let cached = this.cacheGetGrib(cycle, offset);
        if (cached) return cached;

        // Key for in-flight dedup
        const key = `${this.formatCycle(cycle)}+${offset}`;

        // Share in-flight fetch if present
        let inflight = this.#gribInflight.get(key);
        if (inflight) return await inflight;

        // Start and register new in-flight fetch
        inflight = (async () => {
            try {
                return await this.getGrib(cycle, offset);
            } finally {
                // Ensure cleanup regardless of success/failure
                this.#gribInflight.delete(key);
            }
        })();

        this.#gribInflight.set(key, inflight);
        return await inflight;
    }
        
}

/// EOF
////////////////////////////////////////////////////////////////////////////////
