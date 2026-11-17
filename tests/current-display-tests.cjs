// Run with node tests/current-display-tests.cjs.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const frames = [], calls = [];
function canvas() {
    const commands = [];
    const ctx = { commands, clearRect() {}, scale() {}, save() {}, restore() {},
        translate() {}, rotate(a) { commands.push(['rotate', a]); }, beginPath() {},
        moveTo(x, y) { commands.push(['move', x, y]); },
        lineTo(x, y) { commands.push(['line', x, y]); },
        stroke() { commands.push(['stroke', this.strokeStyle, this.lineWidth, this.globalAlpha]); },
        drawImage(frame) { frames.push(frame); } };
    const result = { width: 1800, height: 1200, style: {},
        getBoundingClientRect: () => ({width: 300, height: 200}),
        getContext: () => ctx, cloneNode: () => canvas() };
    ctx.canvas = result;
    return result;
}
let pending;
const context = vm.createContext({ AbortController, URLSearchParams, Date, Math,
    console: {error() {}},
    fetch: async (url, options) => {
        const query = new URL(url, 'http://localhost').searchParams;
        calls.push({query, options});
        if (pending) await pending;
        return {ok: true, json: async () => ({columns: +query.get('columns'), rows: +query.get('rows'),
            vectors: Array.from({length: +query.get('columns') * +query.get('rows')}, () => [1, 0]),
            source: query.get('forecast_model')?.startsWith('ECMWF') ? 'ECMWF-IFS-CURRENT' : query.get('source'),
            time: query.get('time'), cycle: '2026-11-15T00:00:00Z'})};
    }
});
vm.runInContext(fs.readFileSync('web/js/CurrentLayer.js', 'utf8')
    .replace('export default class CurrentLayer', 'class CurrentLayer') + '\nglobalThis.CurrentLayer = CurrentLayer;', context);
const display = canvas(), legend = {};
const layer = new context.CurrentLayer(display, legend);
const bounds = {north: 50, south: 49, west: -3, east: -1};
const time = '2026-11-15T20:00:00Z';
(async () => {
    // The wind grid should contain about half as many arrows as the old 15 px grid.
    const windContext = vm.createContext({Math, loadServerSettings() {},
        L: {latLng: (lat, lng) => ({lat, lng})}});
    vm.runInContext(fs.readFileSync('web/js/GribCache.js', 'utf8')
        .replace(/^import .*;[ \t]*\n/gm, '')
        .replace('export default class GribCache', 'class GribCache') + '\nglobalThis.GribCache = GribCache;', windContext);
    const wind = new windContext.GribCache(display);
    let cells;
    wind.drawMap = async (_ctx, _map, _cycle, _offset, _time, dx, dy) => { cells = 1 / dx / dy; };
    await wind.drawWind({north: 1, south: 0, west: 0, east: 1,
        map: {project: p => ({x: p.lng, y: -p.lat})}}, null, 0, null, display, () => false);
    assert.ok(cells / (Math.floor(300 / 15) * Math.floor(200 / 15)) > 0.45);
    assert.ok(cells / (Math.floor(300 / 15) * Math.floor(200 / 15)) < 0.55);
    const arrow = (u, v) => { const c = canvas().getContext(); layer.drawArrow(c, 0, 0, u, v); return c.commands; };
    assert.ok(arrow(1, 0).find(c => c[0] === 'rotate')[1] === 0); // East
    assert.equal(arrow(0, 1).find(c => c[0] === 'rotate')[1], -Math.PI / 2); // North
    assert.equal(arrow(0, -1).find(c => c[0] === 'rotate')[1], Math.PI / 2); // South
    const weak = arrow(0.1, 0), strong = arrow(2, 0);
    assert.ok(strong.find(c => c[0] === 'line')[1] > weak.find(c => c[0] === 'line')[1]);
    assert.notEqual(strong.filter(c => c[0] === 'stroke').at(-1)[1], weak.filter(c => c[0] === 'stroke').at(-1)[1]);
    assert.ok(weak.find(c => c[0] === 'line')[1] >= 5); // At least 10 CSS px long.
    const strokes = weak.filter(c => c[0] === 'stroke');
    assert.equal(strokes[0][1], '#ffffff');
    assert.ok(strokes[0][2] > strokes[1][2]);
    assert.ok(strokes[1][2] >= 1.6);
    assert.equal(strokes[1][3], 1);
    assert.equal(arrow(0, 0).length, 0);
    assert.equal(arrow(NaN, 1).length, 0);
    await layer.update(bounds, time, 'FW-CURRENT-ENGLISH-CHANNEL', false);
    await layer.update(bounds, time, 'NONE', true);
    assert.equal(calls.length, 0);
    await layer.update(bounds, time, 'FW-CURRENT-ENGLISH-CHANNEL', true, 'ECMWF-AIFS-WIND');
    assert.equal(calls.length, 1);
    assert.equal(+calls[0].query.get('columns'), Math.floor(300 / (15 * Math.SQRT2)));
    assert.equal(+calls[0].query.get('rows'), Math.floor(200 / (15 * Math.SQRT2)));
    assert.equal(calls[0].query.get('time'), new Date(time).toISOString());
    assert.equal(calls[0].query.get('forecast_model'), 'ECMWF-AIFS-WIND');
    assert.equal(frames.length, 1);
    assert.equal(legend.hidden, false);
    assert.match(legend.textContent, /IFS currents/);
    // A disabled/changed source invalidates an in-flight response.
    let resolve;
    pending = new Promise(r => { resolve = r; });
    const loading = layer.update(bounds, time, 'FW-CURRENT-ENGLISH-CHANNEL', true);
    layer.clear(); resolve(); await loading;
    assert.equal(calls[1].options.signal.aborted, true);
    assert.equal(frames.length, 1);
    assert.equal(legend.hidden, true);
    assert.equal(display.style.display, 'none');
    console.log('Current flow direction, strength, density, time sync, toggles and stale responses passed.');
})().catch(error => { console.error(error); process.exitCode = 1; });
