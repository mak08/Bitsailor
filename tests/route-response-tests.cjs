// Run with node tests/route-response-tests.cjs.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync('web/js/router-main.js', 'utf8');
const context = vm.createContext({});
vm.runInContext(source.slice(source.indexOf('function validateRouteResponse('),
                            source.indexOf('function getRoute()')), context);
const validate = context.validateRouteResponse;
for (const isochrones of [[], false]) {
    const result = { status: 'reached', best: [{time: '2026-11-15T20:52:00Z'}], isochrones };
    assert.equal(validate(result), result);
    assert.ok(Array.isArray(result.isochrones));
    assert.equal(result.isochrones.length, 0);
}
const fronts = [{time: '2026-11-15T21:00:00Z', path: []}];
assert.equal(validate({best: [{}], isochrones: fronts}).isochrones, fronts);
for (const invalid of [null, {}, {best: [], isochrones: []}, {best: [{}]},
                       {best: [{}], isochrones: {}}, {best: false, isochrones: false}]) {
    assert.throws(() => validate(invalid), /Invalid route response/);
}
console.log('Empty and legacy isochrones accepted; malformed routes rejected.');
