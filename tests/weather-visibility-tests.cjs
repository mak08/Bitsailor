// Run with node tests/weather-visibility-tests.cjs.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const context = vm.createContext({Math, loadServerSettings() {}});
vm.runInContext(fs.readFileSync('web/js/colors.js', 'utf8').replace(/export\s*\{[^}]*\}/, ''), context);
vm.runInContext(fs.readFileSync('web/js/GribCache.js', 'utf8')
    .replace(/^import .*;[ \t]*\n/gm, '')
    .replace('export default class GribCache', 'class GribCache')
    + '\nglobalThis.GribCache = GribCache;', context);
const wind = new context.GribCache({});
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
assert.ok(light.fills[0].opacity >= 0.7);
assert.equal(light.strokes.length, 0);
assert.equal(strong.strokes.length, 0);
assert.equal(light.rotations[0], Math.PI * 2); // Existing wind direction convention.
assert.equal(draw(0).fills.length, 0);
assert.equal(draw(NaN).fills.length, 0);

// Map muting only toggles a class on the map and remembers the user's choice.
const classes = new Set(), storage = new Map([['xx.mutedMap', 'true']]);
let change, controlInput;
const map = {getContainer: () => ({classList: {toggle(name, on) {on ? classes.add(name) : classes.delete(name);}}})};
const L = {control: () => ({addTo() {this.onAdd();}}),
    DomUtil: {create() {controlInput = {checked: false, addEventListener(_type, callback) {change = callback;}};
        return {querySelector: () => controlInput};}},
    DomEvent: {disableClickPropagation() {}, disableScrollPropagation() {}}};
context.localStorage = {getItem: key => storage.get(key), setItem: (key, value) => storage.set(key, value)};
vm.runInContext(fs.readFileSync('web/js/MapAppearance.js', 'utf8').replace('export default ', '')
    + '\nglobalThis.appearance = addMapAppearanceControl;', context);
context.appearance(map, L);
assert.ok(classes.has('muted-map'));
assert.equal(controlInput.checked, true);
controlInput.checked = false; change();
assert.equal(classes.has('muted-map'), false);
assert.equal(storage.get('xx.mutedMap'), 'false');
context.localStorage = {getItem() {throw Error('blocked');}, setItem() {throw Error('blocked');}};
context.appearance(map, L);
controlInput.checked = true; change();
assert.ok(classes.has('muted-map'));
console.log('Weak wind/current contrast and map appearance tests passed.');
