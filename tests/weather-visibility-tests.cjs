// Run with node tests/weather-visibility-tests.cjs.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const context = vm.createContext({Math, sharedGribCache: {}, loadServerSettings() {}});
vm.runInContext(fs.readFileSync('web/js/colors.js', 'utf8').replace(/export\s*\{[^}]*\}/, ''), context);
vm.runInContext(fs.readFileSync('web/js/WindLayer.js', 'utf8')
    .replace(/^import .*;[ \t]*\n/gm, '')
    .replace('export default class WindLayer', 'class WindLayer')
    + '\nglobalThis.WindLayer = WindLayer;', context);
const wind = new context.WindLayer({});
function draw(speed) {
    const strokes = [], fills = [], rotations = [];
    const ctx = {save() {}, restore() {}, beginPath() {}, translate() {},
        rotate(a) {rotations.push(a);}, scale(x) {this.effectiveScale = x;},
        moveTo() {}, lineTo() {}, closePath() {},
        stroke() {strokes.push({colour: this.strokeStyle, width: this.lineWidth * this.effectiveScale / 6,
            opacity: this.globalAlpha});},
        fill() {fills.push({colour: this.fillStyle, opacity: this.globalAlpha});}};
    wind.drawWindArrow(ctx, 0, 0, 270, speed);
    return {strokes, fills, rotations};
}
const light = draw(1), strong = draw(20);
assert.equal(light.fills[0].colour, '#00ffff'); // Beaufort palette retained.
assert.equal(light.fills[0].opacity, 1); // The canvas applies the user-selected opacity once.
assert.equal(light.strokes.length, 0);
assert.equal(strong.strokes.length, 0);
assert.equal(light.rotations[0], Math.PI * 2); // Existing wind direction convention.
assert.equal(draw(0).fills.length, 0);
assert.equal(draw(NaN).fills.length, 0);

// Map muting only toggles a class on the map and remembers the user's choice.
const classes = new Set(), storage = new Map([['xx.mutedMap', 'true']]);
let change, controlInput;
const map = {getContainer: () => ({classList: {toggle(name, on) {on ? classes.add(name) : classes.delete(name);}}})};
controlInput = {checked: false, addEventListener(_type, callback) {change = callback;}};
context.localStorage = {getItem: key => storage.get(key), setItem: (key, value) => storage.set(key, value)};
vm.runInContext(fs.readFileSync('web/js/MapAppearance.js', 'utf8').replace('export default ', '')
    + '\nglobalThis.appearance = bindMapAppearance;', context);
context.appearance(map, controlInput);
assert.ok(classes.has('muted-map'));
assert.equal(controlInput.checked, true);
controlInput.checked = false; change();
assert.equal(classes.has('muted-map'), false);
assert.equal(storage.get('xx.mutedMap'), 'false');
context.localStorage = {getItem() {throw Error('blocked');}, setItem() {throw Error('blocked');}};
context.appearance(map, controlInput);
controlInput.checked = true; change();
assert.ok(classes.has('muted-map'));
console.log('Weak wind/current contrast and map appearance tests passed.');

// Opacity changes composite the same bitmap; wind visibility and current
// requests must remain independent, including at 0% and after an off/on cycle.
const nodes = {
    ir_wind_opacity: {value: '50', setAttribute(name, value) {this[name] = value;}},
    wind_opacity_value: {}, 'wind-canvas': {style: {}},
    cb_displaycurrents: {checked: true}, sel_currents: {value: 'ECMWF-IFS-CURRENT'}
};
let clears = 0; const updates = [];
const ui = vm.createContext({Math, sharedGribCache: {}, Number, String,
    document: {getElementById: id => nodes[id]},
    currentLayer: {clear() {clears++;}, update(...args) {updates.push(args);}},
    currentDisplayTime: new Date('2026-11-18T00:00:00Z'),
    getMapBounds: () => ({}), gribCacheModel: 'NOAA-GFS-WIND', settings: {},
    updateMap() {throw Error('Opacity must not trigger a map redraw');},
    fetch() {throw Error('Opacity must not trigger a forecast request');}
});
const main = fs.readFileSync('web/js/router-main.js', 'utf8');
for (const name of ['onWindOpacity', 'onDisplayCurrents', 'updateCurrentDisplay']) {
    const start = main.indexOf(`function ${name}(`);
    vm.runInContext(main.slice(start, main.indexOf('\n}', start) + 2), ui);
}
ui.onWindOpacity(); assert.equal(nodes['wind-canvas'].style.opacity, '0.5');
for (let percent = 0; percent <= 100; percent += 10) {
    nodes.ir_wind_opacity.value = String(percent); ui.onWindOpacity();
    assert.equal(Number(nodes['wind-canvas'].style.opacity), percent / 100);
    assert.equal(nodes.wind_opacity_value.textContent, `${percent}%`);
}
assert.equal(clears, 0); assert.equal(updates.length, 0);
nodes.ir_wind_opacity.value = '0'; ui.onWindOpacity();
assert.equal(nodes.cb_displaycurrents.checked, true);
ui.onDisplayCurrents(); assert.equal(updates[0][3], true);
nodes.cb_displaycurrents.checked = false; ui.onDisplayCurrents(); assert.equal(clears, 1);
nodes.ir_wind_opacity.value = '100'; ui.onWindOpacity();
assert.equal(updates.length, 1); assert.equal(nodes.cb_displaycurrents.checked, false);
console.log('Composited wind opacity and independent current visibility passed.');
