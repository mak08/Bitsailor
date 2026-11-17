// Run with node tests/bathymetry-display-tests.cjs. Real local chunks, no browser/GIS dependency.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const zlib = require('node:zlib');
let requests = 0;
const context = vm.createContext({Map, Math, Uint16Array, DataView, Response, DecompressionStream,
    fetch: async url => {
        requests++;
        const path = 'web' + url;
        return fs.existsSync(path) ? new Response(fs.readFileSync(path)) : new Response('', {status: 404});
    }});
vm.runInContext(fs.readFileSync('web/js/BathymetryLayer.js', 'utf8')
    .replace(/export default /g, '').replace(/export /g, '')
    + '\nglobalThis.api = {combineDepths, depthColour, pyramid, BathymetryLayer};', context);
const {combineDepths, depthColour, pyramid, BathymetryLayer} = context.api;
assert.equal(combineDepths([300, 220, 1000]), 220);
assert.equal(combineDepths([300, 0xffff]), 300 | 0x8000);
assert.equal(combineDepths([300, 500 | 0x8000]), 300 | 0x8000);
assert.equal(combineDepths([0xffff, 0xffff]), 0xffff);
assert.equal(combineDepths([]), 0xffff);
assert.equal(depthColour(250, 2.5, false)[3], 0); // Equality meets threshold.
assert.ok(depthColour(249, 2.5, false)[3] > 0);
assert.ok(depthColour(0xffff, 2.5, true)[3] > 0);
assert.ok(depthColour(1000 | 0x8000, 2.5, true)[3] > 0);
assert.equal(pyramid(new Uint16Array([1000, 1000, 1000, 1]), 2)[1].data[0], 1);

// Render a synthetic cell grid with a tiny isolated shoal and one unknown cell.
// The shoal must survive when many native cells contribute to one display pixel.
const layer = Object.create(BathymetryLayer.prototype);
layer.required = 2.5;
layer.cache = new Map();
layer.map = {unproject: ([x, y]) => ({lng: x / 256 * 0.5, lat: 50.5 - y / 256 * 0.5})};
const cells = new Uint16Array(480 * 480).fill(10000);
cells[200 * 480 + 200] = 100;
cells[300 * 480 + 300] = 0xffff;
const levels = pyramid(cells, 480);
layer.chunk = async () => levels;
let painted;
const canvas = {getContext: () => ({createImageData: () => ({data: new Uint8ClampedArray(256 * 256 * 4)}),
    putImageData: image => {painted = image.data;}})};
(async () => {
    await layer.render(canvas, {x: 0, y: 0, z: 10});
    const shoalPixel = Math.floor(200 / 480 * 256);
    assert.ok(painted[(shoalPixel * 256 + shoalPixel) * 4 + 3] > 0);
    assert.equal(painted[0 * 4 + 3], 0); // Known deep water.
    // Sample a pixel where the shoal is not the centre cell.
    assert.ok(Array.from(painted).filter((v, i) => i % 4 === 3 && v > 0).length > 1);

    // Generated data is an optional deployment asset, not a Git fixture.
    if (fs.existsSync('web/bathymetry/channel-2024/manifest.json')) {
        delete layer.chunk;
        const a = await layer.chunk(-4, 100), b = await layer.chunk(-4, 100);
        assert.equal(a, b); assert.equal(requests, 1); // Threshold changes reuse decoded data.
        assert.equal(a[0].data.length, 480 * 480);
        assert.equal(await layer.chunk(-100, 100), null);
        const normalFetch = context.fetch;
        context.fetch = async () => new Response('', {status: 404});
        await assert.rejects(layer.chunk(-12, 99), /HTTP 404/);
        assert.equal(layer.cache.has('-12_99'), false); // A later redraw may retry.
        context.fetch = normalFetch;
        // Verify north-first placement independently using the encoded native tile.
        const raw = zlib.gunzipSync(fs.readFileSync('web/bathymetry/channel-2024/-4_100.bin.gz'));
        assert.equal(a[0].data[1234], raw.readUInt16LE(1234 * 2));
        for (let x = -13; x < 7; x++) await layer.chunk(x, 97);
        for (let x = -13; x < 7; x++) await layer.chunk(x, 98);
        assert.ok(layer.cache.size <= 40);
    }
    console.log('Bathymetry display tests passed');
})().catch(error => {console.error(error); process.exitCode = 1;});
