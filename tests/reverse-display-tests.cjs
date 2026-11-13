const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync('web/js/router-main.js', 'utf8');
function body(name) {
    const start = source.indexOf('function ' + name + '(');
    return source.slice(start, source.indexOf('\n}', start) + 2);
}
const elements = { cb_reverse: { checked: true }, reverse_percent: { value: '0' } };
const layers = [];
const context = vm.createContext({
    Date, currentCycle: '2026-11-12T00:00Z', routeWindSelection: null, routeIsochrones: [],
    waypointMarkers: [], map: {}, document: { getElementById: id => elements[id] },
    wrapLon180: x => x, splitPathAtAntimeridian: p => [p],
    L: { polyline: layer, circleMarker: layer },
    onSelectIsochrone: point => { context.selected = point; }
});
function layer(geometry, style) {
    const result = { geometry, style, addTo() { layers.push(this); return this; },
        on(e, fn) { this[e] = fn; }, bindTooltip(text) { this.tooltip = text; } };
    return result;
}
vm.runInContext(['updateReverseControls', 'segmentsWithWorldTriplicate', 'addInfo',
    'isochroneTime', 'getIsoStyle', 'createIsochrones', 'makeQuery', 'precedingIsochrone'].map(body).join('\n'), context);
context.updateReverseControls();
assert.equal(elements.reverse_percent.disabled, false);
context.waypointMarkers.push({});
context.updateReverseControls();
assert.equal(elements.cb_reverse.disabled, true);
assert.equal(elements.cb_reverse.checked, false);
assert.equal(elements.reverse_percent.disabled, true);
assert.match(context.makeQuery({reverse: 'true', reverse_percent: '0'}), /reverse_percent=0/);
const forward = {cycle: context.currentCycle, time: '2026-11-12T02:00Z'};
const reverse = {...forward, 'wind-time': forward.time, 'forecast-model': 'ECMWF-IFS-WIND',
    path: [{lat: 1, lng: 2}, {lat: 1.5, lng: 3}]};
context.createIsochrones([reverse], 'reverse');
context.createIsochrones([{...reverse, path: [reverse.path[0]]}], 'overlap');
assert.equal(layers.length, 6);
assert.equal(layers[0].style.dashArray, '5 4');
assert.equal(layers[3].style.radius, 3);
layers[0].click();
assert.equal(context.selected.kind, 'reverse');
assert.equal(context.selected['forecast-model'], 'ECMWF-IFS-WIND');
context.routeWindSelection = {isochrone: context.selected};
assert.equal(context.getIsoStyle(layers[0], new Date()).color, '#ffffff');
assert.notEqual(context.getIsoStyle(forward, new Date()).color, '#ffffff');
context.routeInfo = {isochrones: [forward], 'reverse-routing': {isochrones: [reverse]}};
assert.equal(context.precedingIsochrone(new Date(forward.time)), forward);
console.log('Reverse controls, map layers, contact points and wind selection passed.');
