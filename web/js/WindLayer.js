import {sharedGribCache} from './GribCache.js?v=shared-grib-cache-20261118';
import {colors, ms2bf} from './colors.js';

function windSample({u, v, magnitude}) {
    return {direction: (180 + Math.atan2(u, v) * 180 / Math.PI) % 360, speed: magnitude};
}

// Wind rendering and published hover state; forecast loading/sampling is shared
// with currents. Publish new state only after a complete successful map redraw.
export default class WindLayer {
    #canvas;
    #model;
    #cache;
    #curCycle;
    #curTime;
    #field;
    #updateVersion = 0;

    constructor(canvas, model = 'NOAA-GFS-WIND', cache = sharedGribCache) {
        this.#canvas = canvas;
        this.#model = model;
        this.#cache = cache;
    }

    async windDataMagnitude(cycle, time, lat, lon) {
        const field = this.#field && new Date(cycle).getTime() === this.#curCycle.getTime()
            && new Date(time).getTime() === this.#curTime.getTime() ? this.#field
            : await this.#cache.fields(this.#model, time, {cycle});
        return windSample(field.sample(lat, lon));
    }

    async getWind(lat, lon, time = this.#curTime) {
        if (!this.#curCycle || !time) return null;
        return this.windDataMagnitude(this.#curCycle, time, lat, lon);
    }

    async update(bounds, cycle, time = new Date(), requestIsCurrent = () => true) {
        const version = ++this.#updateVersion;
        const curTime = new Date(time), baseTime = new Date(cycle);
        const field = await this.#cache.fields(this.#model, curTime, {cycle: baseTime});
        const isCurrent = () => version === this.#updateVersion && requestIsCurrent();
        if (!isCurrent()) return;
        await this.drawWind(bounds, field, this.#canvas, isCurrent);
        if (!isCurrent()) return;
        this.#curCycle = baseTime;
        this.#curTime = curTime;
        this.#field = field;
    }

    async drawWind (bounds, field, canvas, isCurrent) {

        let rect = canvas.getBoundingClientRect();
        // Only draw if canvas is displayed (wind display enabled)
        if (rect) {
            // sqrt(2) wider spacing halves the number of arrows per screen area.
            const spacing = 15 * Math.SQRT2;
            let STEPS_X = Math.max(1, Math.floor(rect.width/spacing));
            let STEPS_Y = Math.max(1, Math.floor(rect.height/spacing));

            // Keep the displayed forecast intact until the entire new frame is ready.
            const frame = canvas.cloneNode(false);
            var ctx = frame.getContext("2d");
            ctx.globalAlpha = 1;

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
                await this.drawMap(ctx, map, field, dx, dy, top, bottom, left, right, 0, right - left);
            } else {
                // left > right
                var dx = (256 - left + right) / STEPS_X;
                let mapWidthLeft = 256 - left;
                let mapWidth = mapWidthLeft + right;
                let canvasLeft = mapWidthLeft/mapWidth * ctx.canvas.width;
                await this.drawMap(ctx, map, field, dx, dy, top, bottom, left, 256, 0, mapWidth);
                await this.drawMap(ctx, map, field, dx, dy, top, bottom, 0, right, canvasLeft, mapWidth);
            }
            if (isCurrent()) {
                const display = canvas.getContext("2d");
                display.clearRect(0, 0, canvas.width, canvas.height);
                display.drawImage(frame, 0, 0);
            }
        }
    }

    async drawMap (ctx, projection, field, dx, dy, top, bottom, left, right, canvasLeft, mapWidth) {
        var mapHeight = bottom - top;
        for (var y = top; y < bottom; y += dy) {
            for (var x = left; x < right; x += dx) {
                var mapPoint = L.point(x, y);
                var latLng = projection.unproject(mapPoint);
                var pointX = canvasLeft + ((x - left) /  mapWidth) * ctx.canvas.width;
                var pointY = (y - top) / mapHeight * ctx.canvas.height;
                let w = windSample(field.sample(latLng.lat, latLng.lng));
                this.drawWindArrow(ctx, pointX, pointY, w.direction, w.speed);
            }
        }
    }

    drawWindArrow(ctx, x, y, direction, speed) {
        if (!Number.isFinite(direction) || !Number.isFinite(speed) || speed <= 0) return;
        direction = direction + 90;
        if (direction > 360) {
            direction = direction - 360;
        }
        ctx.save();
        ctx.beginPath();
        ctx.fillStyle = colors[ms2bf(speed)];
        ctx.translate(x , y);
        ctx.rotate((direction*Math.PI/180));
        var scale = (speed>0)?0.5 + speed/50:0;
        ctx.scale(scale, scale);
        ctx.moveTo(-0.00, 0.00);
        ctx.lineTo(-15 * 6, 12 * 6);
        ctx.lineTo(18 * 6, 0);
        ctx.lineTo(-15 * 6, -12 * 6);
        ctx.closePath()
        // Display opacity belongs to the canvas, so the slider can composite
        // this bitmap without recalculating arrows or multiplying opacity twice.
        ctx.globalAlpha = 1;
        ctx.fill();
        ctx.restore();
    }

}
