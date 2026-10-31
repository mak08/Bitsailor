const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync(require('node:path').join(__dirname, '../web/js/router-main.js'), 'utf8');
function extract(name) {
    const start = source.indexOf(`function ${name}(`);
    const end = source.indexOf('\n}', start) + 2;
    assert(start >= 0 && end > start);
    return source.slice(start, end);
}
const ctx = vm.createContext({ Date, selectedIsochroneStage: 1, selectedRouteTime: null });
vm.runInContext(extract('getIsochroneTime') + '\n' + extract('getIsoStyle'), ctx);
const cycle = new Date('2026-10-30T12:00:00Z');
const style = (iso) => ctx.getIsoStyle(iso, 18, cycle);
assert.equal(style({ cycle, offset: 18, stage: 1 }).color, '#ffffff');
assert.notEqual(style({ cycle, offset: 18, stage: 0 }).color, '#ffffff');
// Same offset from a different cycle is a different time.
assert.notEqual(style({ cycle: '2026-10-30T06:00:00Z', offset: 18, stage: 1 }).color, '#ffffff');
// Different cycle and offset can identify the same absolute time.
assert.equal(style({ cycle: '2026-10-30T06:00:00Z', offset: 24, stage: 1 }).color, '#ffffff');
ctx.selectedIsochroneStage = null;
assert.equal(style({ cycle, offset: 18 }).color, '#ffffff');
assert.equal(style({ cycle, offset: 18, stage: 0 }).dashArray, null);
assert.equal(style({ cycle, offset: 18, stage: 1 }).dashArray, '8 4');
assert.equal(style({ cycle, offset: 18, stage: 2 }).dashArray, '2 4');
console.log('Isochrone time/stage display regressions passed.');
