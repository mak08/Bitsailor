// Run with node tests/wind-sync-tests.cjs.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const rawCache = fs.readFileSync('web/js/GribCache.js', 'utf8');
const cacheSource = rawCache.replace(/^import .*;[ \t]*\n/gm, '').replace('export default class GribCache', 'class GribCache');
const main = fs.readFileSync('web/js/router-main.js', 'utf8').replace(/^import .*;[ \t]*\n/gm, '');
const base = '2026-10-31T06:00:00Z';
const later = '2026-10-31T12:00:00Z';
let fail = false;
const paths = [0, 3, 6, 9, 12, 15, 18].map(step => ({ date: '20261031', run: 6, step, path: `test-${step}` }));
const context = vm.createContext({
  console: { error() {} }, Date,
  loadServerSettings: async () => {}, ensureServerSettings: async () => {},
  loadForecastCycle: async () => ({ datasources: [] }),
  getServerSettings: () => ({ datasources: [{ name: 'NOAA-GFS-WIND', gribpaths: paths }] }),
  fetch: async () => ({ ok: !fail, status: 500, statusText: 'An error occurred', arrayBuffer: async () => new ArrayBuffer(1) }),
  decodeGRIB2ArrayBuffer: () => ({}),
});
vm.runInContext(cacheSource, context);
const cache = vm.runInContext('new GribCache(null)', context);
cache.updateCanvas = async () => {};
(async () => {
  // A failed update must leave the previous valid cycle/time available to hover lookups.
  await cache.update({}, new Date(base), new Date('2026-10-31T09:00:00Z'));
  const seen = [];
  cache.windDataMagnitude = async (cycle, time) => { seen.push([cycle.toISOString(), time.toISOString()]); return {}; };
  await assert.rejects(cache.update({}, new Date(later), new Date('2026-10-31T21:00:00Z')), /No metadata/);
  await cache.getWind(0, 0);
  assert.deepEqual(seen[0], ['2026-10-31T06:00:00.000Z', '2026-10-31T09:00:00.000Z']);
  fail = true;
  await assert.rejects(cache.getGribCached(new Date(base), 18), /HTTP 500.*test-18/);
  fail = false;
  await cache.getGribCached(new Date(base), 18); // Failed in-flight entries must be retryable.

  // Explicit archived metadata is loaded separately and its exact paths are fetched.
  const archivedRequests = [];
  context.loadForecastCycle = async (model, cycle) => {
    assert.equal(model, 'NOAA-GFS-WIND');
    assert.equal(cycle.toISOString(), '2026-10-31T12:00:00.000Z');
    return { datasources: [{ name: model, gribpaths: paths.map(p => ({ ...p, run: 12, path: `archive-${p.step}` })) }] };
  };
  context.fetch = async path => { archivedRequests.push(path); return { ok: true, arrayBuffer: async () => new ArrayBuffer(1) }; };
  const archived = vm.runInContext('new GribCache(null)', context);
  archived.updateCanvas = async () => {};
  await archived.update({}, new Date(later), new Date('2026-10-31T21:00:00Z'));
  assert.deepEqual(archivedRequests, ['/weather/archive-9']);

  const nodes = new Map();
  const node = id => { if (!nodes.has(id)) nodes.set(id, {}); return nodes.get(id); };
  let refreshed = 0, failDisplay = false;
  const attempts = [];
  const ui = vm.createContext({
    console: { error() {} }, Date, PolarManager: class {},
    document: { getElementById: node },
    reloadServerSettings: async () => { refreshed++; },
  });
  vm.runInContext(main.slice(0, main.lastIndexOf('// Initialize everything when the page loads')), ui);
  ui.attempts = attempts;
  ui.shouldFail = () => failDisplay;
  vm.runInContext(`
    currentCycle = '${base}';
    windReady = true;
    ir_index = document.getElementById('ir_index');
    getMapBounds = () => ({});
    updateIsochrones = () => {};
    updateForecastRange = () => {};
    updateCycleDisplay = () => {};
    getLatestCycleFromSettings = () => '${later}';
    gribCache = { update: async (_, cycle, time) => {
      attempts.push([cycle.toISOString(), time.toISOString()]);
      if (shouldFail() || cycle.getTime() !== new Date('${later}').getTime()) throw new Error('missing forecast');
    }};
  `, ui);
  // Recorded-cycle inspection must never switch to the newer run on failure.
  vm.runInContext(`routeInfo = { 'forecast-model': 'NOAA-GFS-WIND', 'initial-cycle': '${base}', isochrones: [
    { cycle: '${later}', time: '2026-10-31T23:00:00Z', offset: 11 },
    { cycle: '${base}', time: '2026-10-31T18:00:00Z', offset: 12 }
  ] };`, ui);
  failDisplay = true;
  assert.equal(await vm.runInContext(`onSelectIsochrone({ cycle: '${base}', offset: 15, stage: 1 })`, ui), false);
  assert.equal(refreshed, 1);
  assert.deepEqual(JSON.parse(JSON.stringify(attempts)), [
    ['2026-10-31T06:00:00.000Z', '2026-10-31T21:00:00.000Z'],
    ['2026-10-31T06:00:00.000Z', '2026-10-31T21:00:00.000Z']
  ]);
  assert.match(node('router-data-status').textContent, /Recorded forecast unavailable/);
  // A valid old run is used despite the newer run being advertised.
  failDisplay = false;
  vm.runInContext(`gribCache.update = async (_, cycle, time) => { attempts.push([cycle.toISOString(), time.toISOString()]); };`, ui);
  assert.equal(await vm.runInContext(`onMarkerClicked({ time: '2026-10-31T21:30:00Z' })`, ui), true);
  assert.equal(node('ir_index').value, 15.5);
  assert.equal(vm.runInContext('currentCycle', ui), base);
  assert.equal(vm.runInContext('getOffsetTime(9.5).toISOString()', ui), '2026-10-31T15:30:00.000Z');
  // Before the first isochrone, use the route's initial actual cycle.
  await vm.runInContext(`onMarkerClicked({ time: '2026-10-31T13:15:00Z' })`, ui);
  assert.equal(attempts.at(-1)[0], '2026-10-31T06:00:00.000Z');
  // Isochrone sampling time is distinct from its endpoint/selection time.
  await vm.runInContext(`onSelectIsochrone({ cycle: '${base}', offset: 15, stage: 1, 'wind-time': '2026-10-31T20:50:00Z' })`, ui);
  assert.equal(attempts.at(-1)[1], '2026-10-31T20:50:00.000Z');
  assert.equal(vm.runInContext('selectedRouteTime.toISOString()', ui), '2026-10-31T21:00:00.000Z');
  // Leaflet layers must keep provenance as well as their geometric cycle/offset.
  vm.runInContext(`var layer = { on() {} }; addInfo(layer, '${base}', 15, 0, { 'wind-time': '2026-10-31T20:50:00Z', 'forecast-model': 'NOAA-GFS-WIND' });`, ui);
  assert.equal(vm.runInContext(`layer['wind-time']`, ui), '2026-10-31T20:50:00Z');
  // Matching the backend's scalar interpolation matters when vector directions differ.
  const magnitude = vm.runInContext('interpolatePoint(10, -10, 0, 0, 0.5).magnitude', context);
  assert.equal(magnitude, 10);
  console.log('Recorded-cycle wind replay, preceding-isochrone inference, sample time and failure tests passed.');
})().catch(error => { console.error(error); process.exitCode = 1; });
