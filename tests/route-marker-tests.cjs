// Marker selection must work for both raw Lisp trackpoints and fine-tuned commands.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync('web/js/router-main.js', 'utf8');
const exportsSource = fs.readFileSync('web/js/GPX.js', 'utf8')
    .replace(/^import .*;\n/gm, '').replace(/export\s*\{[^}]+\}/m, '');
const exportsContext = vm.createContext({ Date });
vm.runInContext(exportsSource, exportsContext);
const start = source.indexOf('function displayRouting(');
const end = source.indexOf('\nfunction setupCanvas(', start);
assert.ok(start >= 0 && end > start);
const markers = [];
const nodes = new Map();
const node = id => {
    if (!nodes.has(id)) nodes.set(id, { checked: false });
    return nodes.get(id);
};
const ctx = vm.createContext({
    Date, GPX: { steeringControl: exportsContext.steeringControl },
    displayFineTuning() {}, createIsochrones() {}, startMarker: {},
    L: { icon: x => x, polyline: () => ({ addTo: () => ({}) }) },
    document: { getElementById: node }, routeTracks: [], map: {},
    wrapLon180: x => x, splitPathAtAntimeridian: x => [x], segmentsWithWorldCopies: x => x,
    createBestRouteMarkerAndCopies: entry => markers.push(entry)
});
vm.runInContext(source.slice(start, end), ctx);
const time = Date.parse('2026-11-04T19:00:00Z');
const best = [105, 105, 110, 110, 105, 105].map((twa, i) => ({
    twa, time: new Date(time + i * 120000).toISOString(), sail: 0,
    position: { lat: 43, lng: -41 - i * 0.01 },
    'control-mode': false, 'control-value': false
}));
const stats = { start: '', duration: '', 'min-wind': 10, 'max-wind': 12 };
function render(track) {
    markers.length = 0;
    ctx.displayRouting({ best: track, isochrones: [], stats });
    return markers.map(p => track.indexOf(p));
}
assert.deepEqual(render(best), [0, 2, 4, 5]);
assert.deepEqual(render(best.map(p => ({ ...p, 'control-mode': 'twa', 'control-value': p.twa }))),
    [0, 2, 4, 5]);
// An integer heading is one command even when wind changes the sampled TWA.
assert.deepEqual(render(best.map(p => ({ ...p, 'control-mode': 'heading', 'control-value': 110 }))),
    [0, 5]);
// Zero is a real command, unlike the JSON false used for unset Lisp slots.
assert.deepEqual(render(best.map((p, i) => ({ ...p,
    'control-mode': 'heading', 'control-value': i < 2 ? 0 : 5 }))), [0, 2, 5]);
for (const path of process.argv.slice(2)) {
    const response = JSON.parse(fs.readFileSync(path, 'utf8'));
    ctx.displayRouting(response);
    const actual = render(response.best);
    assert.ok(actual.length > 2, `${path}: expected intermediate route markers`);
    console.log(`${path}: ${actual.length} route marker positions`);
}
console.log('Raw/fine-tuned route markers, command changes, heading holds and zero commands passed.');
