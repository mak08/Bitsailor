// Run with node tests/wind-sync-tests.cjs.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const strip = path => fs.readFileSync(path, 'utf8').replace(/^import .*;[ \t]*\n/gm, '')
  .replace('export default class', 'class').replace('export function', 'function').replace('export const', 'const');
const cacheSource = strip('web/js/GribCache.js') + '\n' + strip('web/js/WindLayer.js');
const main = fs.readFileSync('web/js/router-main.js', 'utf8').replace(/^import .*;[ \t]*\n/gm, '');
const base = '2026-10-31T06:00:00Z';
const later = '2026-10-31T12:00:00Z';
let fail = false;
const paths = [0, 3, 6, 9, 12, 15, 18].map(step => ({ date: '20261031', run: 6, step, path: `test-${step}` }));
const context = vm.createContext({
  console: { error() {} }, Date, AbortSignal,
  loadServerSettings: async () => {}, ensureServerSettings: async () => {},
  loadForecastCycle: async () => ({ datasources: [] }),
  getServerSettings: () => ({ datasources: [{ name: 'NOAA-GFS-WIND', datakind:'wind', gribpaths: paths }] }),
  fetch: async () => ({ ok: !fail, status: 500, statusText: 'An error occurred', arrayBuffer: async () => new ArrayBuffer(1) }),
  decodeGRIB2ArrayBuffer: (data, options) => {
    const bytes = data instanceof ArrayBuffer ? new Uint8Array(data) : data;
    const code = bytes[0], run = Math.floor(code / 20), step = code % 20;
    const message = number => ({discipline:0,totalLength:1,
      PRODUCT_DEFINITION_SECTION:{parameterCategory:2,parameterNumber:number,forecastTime:step,indicatorOfUnitOfTimeRange:1},
      GRID_DEFINITION_SECTION:{gridDefinitionTemplateNumber:0,Ni:2,Nj:2,scanningMode:64,
        iDirectionIncrement:1000000,jDirectionIncrement:1000000,latitudeOfFirstGridPoint:0,longitudeOfFirstGridPoint:0},
      DATA_SECTION:{data:Float32Array.from([10,10,10,10])},
      timestamp:()=>`2026-10-31T${String(run).padStart(2,'0')}:00:00Z`});
    return {messages: options?.decodeData === false ? [message(2),message(3)] : [message(2)]};
  },
});
context.ensureServerSettings = async () => context.getServerSettings();
context.fetch = async path => {
  const step = +path.split('-').at(-1), code = 6 * 20 + step;
  return {ok:!fail,status:500,statusText:'An error occurred',arrayBuffer:async()=>Uint8Array.from([code,code]).buffer};
};
vm.runInContext(cacheSource, context);
const cache = vm.runInContext('new WindLayer(null)', context);
cache.drawWind = async () => {};
(async () => {
  // A failed update must leave the previous valid cycle/time available to hover lookups.
  await cache.update({}, new Date(base), new Date('2026-10-31T09:00:00Z'));
  const seen = [];
  const originalWindData = cache.windDataMagnitude.bind(cache);
  cache.windDataMagnitude = async (cycle, time) => { seen.push([cycle.toISOString(), time.toISOString()]); return {}; };
  await assert.rejects(cache.update({}, new Date(later), new Date('2026-10-31T21:00:00Z')), /No metadata/);
  await cache.getWind(0, 0);
  assert.deepEqual(seen[0], ['2026-10-31T06:00:00.000Z', '2026-10-31T09:00:00.000Z']);
  fail = true;
  await assert.rejects(originalWindData(new Date(base), new Date('2026-11-01T00:00:00Z'), 0, 0), /HTTP 500.*test-18/);
  fail = false;
  await originalWindData(new Date(base), new Date('2026-11-01T00:00:00Z'), 0, 0); // Failed in-flight entries must be retryable.

  // Explicit archived metadata is loaded separately and its exact paths are fetched.
  const archivedRequests = [];
  context.loadForecastCycle = async (model, cycle) => {
    assert.equal(model, 'NOAA-GFS-WIND');
    assert.equal(cycle.toISOString(), '2026-10-31T12:00:00.000Z');
    return { datasources: [{ name: model, datakind:'wind', gribpaths: paths.map(p => ({ ...p, run: 12, path: `archive-${p.step}` })) }] };
  };
  context.fetch = async path => { archivedRequests.push(path); return {ok:true,arrayBuffer:async()=>Uint8Array.from([249,249]).buffer}; };
  const archived = vm.runInContext('new WindLayer(null)', context);
  archived.drawWind = async () => {};
  await archived.update({}, new Date(later), new Date('2026-10-31T21:00:00Z'));
  assert.deepEqual(archivedRequests, ['/weather/archive-9']);

  const nodes = new Map();
  const node = id => { if (!nodes.has(id)) nodes.set(id, {}); return nodes.get(id); };
  let refreshed = 0, failDisplay = false;
  const attempts = [];
  const currentUpdates = [];
  const alerts = [];
  const ui = vm.createContext({
    console: { error() {} }, Date, AbortSignal, PolarManager: class {},
    document: { getElementById: node }, alert: message => alerts.push(message),
    reloadServerSettings: async () => { refreshed++; },
  });
  vm.runInContext(main.slice(0, main.lastIndexOf('// Initialize everything when the page loads')), ui);
  ui.attempts = attempts;
  ui.currentUpdates = currentUpdates;
  node('sel_currents').value = 'FW-CURRENT-ENGLISH-CHANNEL';
  node('cb_displaycurrents').checked = true;
  ui.shouldFail = () => failDisplay;
  vm.runInContext(`
    currentCycle = '${base}';
    windReady = true;
    currentLayer = { clear() {}, update: (_, time, source, enabled) =>
        currentUpdates.push([time.toISOString(), source, enabled]) };
    ir_index = document.getElementById('ir_index');
    getMapBounds = () => ({});
    updateIsochrones = () => {};
    updateForecastRange = () => {};
    gribCacheModel = settings.forecast_model;
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
  node('sel_forecast_model').value = 'NOAA-GFS-WIND';
  failDisplay = true;
  assert.equal(await vm.runInContext(`onSelectIsochrone({ cycle: '${base}', offset: 15, stage: 1 })`, ui), false);
  assert.equal(refreshed, 1);
  assert.deepEqual(JSON.parse(JSON.stringify(attempts)), [
    ['2026-10-31T06:00:00.000Z', '2026-10-31T21:00:00.000Z'],
    ['2026-10-31T06:00:00.000Z', '2026-10-31T21:00:00.000Z']
  ]);
  assert.match(alerts.pop(), /Recorded forecast unavailable/);
  // A valid old run is used despite the newer run being advertised.
  failDisplay = false;
  vm.runInContext(`gribCache.update = async (_, cycle, time) => { attempts.push([cycle.toISOString(), time.toISOString()]); };`, ui);
  assert.equal(await vm.runInContext(`onMarkerClicked({ time: '2026-10-31T21:30:00Z' })`, ui), true);
  assert.equal(node('model-heading').textContent, 'Recorded model');
  assert.equal(node('ir_index').value, 15.5);
  assert.equal(vm.runInContext('currentCycle', ui), base);
  assert.equal(vm.runInContext('getOffsetTime(9.5).toISOString()', ui), '2026-10-31T15:30:00.000Z');
  // Before the first isochrone, use the route's initial actual cycle.
  await vm.runInContext(`onMarkerClicked({ time: '2026-10-31T13:15:00Z' })`, ui);
  assert.equal(attempts.at(-1)[0], '2026-10-31T06:00:00.000Z');
  // Clicking a frontier displays wind at its timestamp, preserving its actual forecast run.
  await vm.runInContext(`onSelectIsochrone({ cycle: '${base}', offset: 15, stage: 1, 'wind-time': '2026-10-31T20:50:00Z' })`, ui);
  assert.equal(attempts.at(-1)[1], '2026-10-31T21:00:00.000Z');
  await vm.runInContext(`onSelectIsochrone({ cycle: '${base}', time: '2026-10-31T22:00:00Z', kind: 'reverse', 'wind-time': '2026-10-31T21:30:00Z' })`, ui);
  assert.equal(attempts.at(-1)[1], '2026-10-31T22:00:00.000Z');
  assert.deepEqual(Array.from(currentUpdates.at(-1)), ['2026-10-31T22:00:00.000Z', 'FW-CURRENT-ENGLISH-CHANNEL', true]);
  await vm.runInContext(`onSelectIsochrone({ cycle: '${base}', offset: 15, stage: 1 })`, ui);
  assert.equal(node('ir_index').value, 15);
  assert.equal(vm.runInContext('routeWindSelection.time.toISOString()', ui), '2026-10-31T21:00:00.000Z');
  ui.WindLayer = class {
    constructor(canvas, model) { this.model = model; }
    async update() {}
  };
  assert.equal(await vm.runInContext(`onSelectIsochrone({ cycle: '${base}', offset: 15, 'forecast-model': 'router::ecmwf-ifs-wind' })`, ui), true);
  assert.equal(vm.runInContext('gribCache.model', ui), 'ECMWF-IFS-WIND');
  assert.equal(node('model-heading').textContent, 'Recorded model');
  assert.equal(node('lb_displayed_model').textContent, 'ECMWF-IFS-WIND');
  assert.equal(node('sel_forecast_model').value, 'NOAA-GFS-WIND');
  assert.equal(vm.runInContext('settings.forecast_model', ui), 'NOAA-GFS-WIND');
  assert.equal(node('model-heading').className, 'recorded');
  // Both slider and step buttons switch to the model recorded for the selected hour.
  for (const control of ['ir_index', 'bt_inc']) {
    const slider = node('ir_index');
    Object.defineProperty(slider, 'valueAsNumber', { configurable: true,
      get() { return Number(this.value); }, set(value) { this.value = value; } });
    slider.value = control === 'bt_inc' ? 11 : 12;
    assert.equal(await vm.runInContext(`onAdjustIndex({target: {id: '${control}'}})`, ui), true);
    assert.equal(slider.value, 12);
    assert.equal(node('model-heading').textContent, 'Recorded model');
    assert.equal(node('model-heading').className, 'recorded');
    assert.equal(node('lb_displayed_model').textContent, 'NOAA-GFS-WIND');
    await vm.runInContext(`onSelectIsochrone({ cycle: '${base}', offset: 15, 'forecast-model': 'ECMWF-IFS-WIND' })`, ui);
    assert.equal(node('model-heading').textContent, 'Recorded model');
    assert.equal(node('model-heading').className, 'recorded');
  }
  // Compare the model of the highlighted stage, including across cycle boundaries.
  vm.runInContext(`routeInfo.isochrones.push(
    { cycle: '${base}', offset: 13, stage: 0, 'forecast-model': 'NOAA-GFS-WIND' },
    { cycle: '${later}', offset: 7, stage: 1, 'forecast-model': 'router::ecmwf-ifs-wind' },
    { cycle: '${base}', offset: 14, stage: 1, 'forecast-model': 'NOAA-GFS-WIND' }
  );`, ui);
  await vm.runInContext(`onSelectIsochrone({ cycle: '${base}', offset: 15, stage: 1, 'forecast-model': 'ECMWF-IFS-WIND' })`, ui);
  // The same absolute hour can have a different slider offset on the new model's cycle.
  for (const [offset, model, cycle, newOffset] of [
    [13, 'ECMWF-IFS-WIND', later, 7], [8, 'NOAA-GFS-WIND', base, 14],
    [13, 'ECMWF-IFS-WIND', later, 7], [10, 'ECMWF-IFS-WIND', later, 10]
  ]) {
    node('ir_index').value = offset;
    assert.equal(await vm.runInContext("onAdjustIndex({target: {id: 'ir_index'}})", ui), true);
    assert.equal(node('model-heading').textContent, 'Recorded model');
    assert.equal(node('lb_displayed_model').textContent, model);
    assert.equal(node('ir_index').value, newOffset);
    assert.equal(vm.runInContext('currentCycle', ui), cycle);
    assert.equal(node('sel_forecast_model').value, 'NOAA-GFS-WIND');
  }
  assert.equal(vm.runInContext('routeWindSelection.isochrone', ui), null);
  await vm.runInContext(`onSelectIsochrone({ cycle: '${base}', offset: 15, 'forecast-model': 'ECMWF-IFS-WIND' })`, ui);
  // A failed model switch leaves both the active route replay and routing preferences intact.
  vm.runInContext(`getMaxForecastStep = () => 384;
    currentRaceName = 'test';
    races = [{name: currentRaceName, settings: {forecast_model: settings.forecast_model}}];`, ui);
  const stored = [];
  ui.storeValue = (...entry) => stored.push(entry);
  ui.localStorage = { setItem() {} };
  ui.WindLayer = class {
    constructor(canvas, model) { this.model = model; }
    async update() { if (this.model === 'AROME') throw new Error('missing forecast'); }
  };
  async function chooseModel(model) {
    node('sel_forecast_model').value = model;
    await vm.runInContext("onSetForecastModel({currentTarget: document.getElementById('sel_forecast_model')})", ui);
  }
  const previousCache = vm.runInContext('gribCache', ui);
  await chooseModel('AROME');
  assert.deepEqual(alerts, ['Could not display wind for AROME: missing forecast']);
  assert.equal(vm.runInContext('gribCache', ui), previousCache);
  assert.equal(vm.runInContext('currentCycle', ui), base);
  assert.equal(node('lb_displayed_model').textContent, 'ECMWF-IFS-WIND');
  assert.equal(node('sel_forecast_model').value, 'NOAA-GFS-WIND');
  assert.equal(vm.runInContext('races[0].settings.forecast_model', ui), 'NOAA-GFS-WIND');
  assert.deepEqual(stored, []);
  // Leaflet layers must keep provenance as well as their geometric cycle/offset.
  vm.runInContext(`var layer = { on() {} }; addInfo(layer, '${base}', 15, 0, { 'wind-time': '2026-10-31T20:50:00Z', 'forecast-model': 'NOAA-GFS-WIND' });`, ui);
  assert.equal(vm.runInContext(`layer['wind-time']`, ui), '2026-10-31T20:50:00Z');
  // New routing requests must not inherit the old cycle being inspected above.
  // Exercise the real request builder, retaining ordered point waypoints.
  let request;
  ui.Util = { doGETJSON: url => { request = new URL(url, 'http://localhost'); } };
  ui.window = { setInterval() { return 1; } };
  node('mapMenu').style = {};
  node('sel_duration').value = 96;
  node('cb_waves').checked = true;
  node('sel_currents').value = 'NONE';
  vm.runInContext(`
    setBusyCursor = () => {};
    getStartTime = () => '2026-10-31T22:14';
    startMarker = { getLatLng: () => ({ lat: 38.46, lng: 0.91 }) };
    destinationMarker = { getLatLng: () => ({ lat: 40.00, lng: 4.88 }) };
    waypointMarkers = [
      { markers: [{ getLatLng: () => ({ lat: 39.1, lng: 2.2 }) }] },
      { markers: [{ getLatLng: () => ({ lat: 40.0, lng: 4.0 }) }] }
    ];
    settings.cycle = '2026-10-05T12:00:00Z';
    getRoute();
  `, ui);
  assert.equal(request.pathname, '/function/router.getRoute');
  assert.deepEqual(request.searchParams.getAll('fine_tune'), ['false']);
  assert.equal(request.searchParams.get('search_box'), 'true');
  assert.deepEqual(request.searchParams.getAll('stepper'), ['standard']);
  assert.equal(request.searchParams.has('cycle'), false);
  const waypoints = request.searchParams.get('waypoints').split('~').map(p => p.split(',').map(Number));
  assert.equal(waypoints.length, 2);
  assert.equal(waypoints[0][0], 39.1);
  assert.ok(Math.abs(waypoints[0][1] - 2.2) < 1e-10);
  assert.deepEqual(waypoints[1], [40, 4]);
  assert.deepEqual(request.searchParams.getAll('forecast_model'), ['NOAA-GFS-WIND']);
  assert.deepEqual(request.searchParams.getAll('useWaves'), ['true']);
  assert.equal(request.searchParams.get('forecast_switch_hours'), 'off');
  node('sel_secondary_forecast_model').value = 'ECMWF-IFS-WIND';
  node('cb_hires_stepper').checked = true;
  node('cb_fine_tune').checked = true;
  node('cb_search_box').checked = false;
  vm.runInContext(`settings.forecast_switch_hours = '12'; getRoute();`, ui);
  assert.deepEqual(request.searchParams.getAll('stepper'), ['hires']);
  assert.deepEqual(request.searchParams.getAll('fine_tune'), ['true']);
  assert.equal(request.searchParams.get('search_box'), 'false');
  assert.deepEqual(request.searchParams.getAll('secondary_forecast_model'), ['ECMWF-IFS-WIND']);
  assert.deepEqual(request.searchParams.getAll('forecast_switch_hours'), ['12']);
  assert.equal(request.searchParams.get('startTime'), '2026-10-31T22:14');
  assert.equal(vm.runInContext('currentCycle', ui), base); // Replay remains intact.
  await chooseModel('ECMWF-IFS-WIND');
  assert.equal(node('model-heading').textContent, 'Displayed model');
  assert.equal(node('sel_forecast_model').value, 'ECMWF-IFS-WIND');
  assert.equal(vm.runInContext('routeWindSelection', ui), null);
  node('ir_index').value = 11; // An existing isochrone on the current cycle.
  await vm.runInContext("onAdjustIndex({target: {id: 'ir_index'}})", ui);
  assert.equal(vm.runInContext('routeWindSelection', ui), null);
  assert.equal(node('model-heading').textContent, 'Displayed model');
  assert.equal(vm.runInContext('races[0].settings.forecast_model', ui), 'ECMWF-IFS-WIND');
  assert.deepEqual(stored, [['forecast_model', 'ECMWF-IFS-WIND']]);
  // A slower failed switch must not undo a newer successful choice or show a stale error.
  let rejectSlow;
  ui.WindLayer = class {
    constructor(canvas, model) { this.model = model; }
    async update() {
      if (this.model === 'AROME') await new Promise((_, reject) => { rejectSlow = reject; });
    }
  };
  const slow = chooseModel('AROME');
  await chooseModel('NOAA-GFS-WIND');
  rejectSlow(new Error('late failure'));
  await slow;
  assert.equal(alerts.length, 1);
  assert.equal(vm.runInContext('gribCacheModel', ui), 'NOAA-GFS-WIND');
  assert.equal(node('sel_forecast_model').value, 'NOAA-GFS-WIND');
  assert.equal(vm.runInContext('races[0].settings.forecast_model', ui), 'NOAA-GFS-WIND');
  // Matching the backend's scalar interpolation matters when vector directions differ.
  // Fine-tuned samples carry explicit forecast provenance; marker copies must
  // retain it rather than inferring the old search isochrone's sampling time.
  vm.runInContext(`
    makeWaypointInfo = () => '';
    var refinedMarker = { bindTooltip() {} };
    addWaypointInfo(refinedMarker, new Date('${base}'), {
      time: '2026-10-31T21:12:00Z', 'wind-cycle': '${base}',
      'wind-model': 'NOAA-GFS-WIND', 'wind-time': '2026-10-31T21:10:00Z'
    });
  `, ui);
  await vm.runInContext('onMarkerClicked(refinedMarker)', ui);
  assert.equal(vm.runInContext('routeWindSelection.cycle', ui), base);
  assert.equal(vm.runInContext('routeWindSelection.sampleTime.toISOString()', ui), '2026-10-31T21:10:00.000Z');
  const magnitude = vm.runInContext(`sampleField({grid:{gridDefinitionTemplateNumber:0,Ni:2,Nj:2,scanningMode:64,latitudeOfFirstGridPoint:0,longitudeOfFirstGridPoint:0,iDirectionIncrement:1000000,jDirectionIncrement:1000000},u:[10,-10,10,-10],v:[0,0,0,0]},0.5,0.5,'wind').magnitude`, context);
  assert.equal(magnitude, 10);
  vm.runInContext('displayFineTuning({})', ui);
  assert.match(node('lb_fine_tuning').textContent, /Not run/);
  vm.runInContext(`displayFineTuning({'fine-tuning': {status: 'optimized', 'twa-step': 1,
    'integer-controls': true, 'original-changes': 91, 'optimized-changes': 84,
    reason: 'validated', evaluations: 2287}})`, ui);
  assert.match(node('lb_fine_tuning').textContent, /91 → 84/);
  vm.runInContext(`displayFineTuning({'fine-tuning': {status: 'unchanged', 'twa-step': 1,
    reason: 'window could not be re-sailed within constraints', evaluations: 520,
    'failed-window': 34, 'endpoint-error': 0.5225}})`, ui);
  assert.match(node('lb_fine_tuning').textContent, /Original route retained/);
  assert.match(node('lb_fine_tuning').title, /window 34.*0\.522/);
  vm.runInContext(`displayFineTuning({'fine-tuning': {status: 'optimized', 'twa-step': 1,
    objective: 'origin-distance', 'distance-gain': 12.345, 'optimized-origin-distance': 12000,
    approximation: true, 'corridor-limit': 1852,
    'rolling-windows': 33, 'rolling-improvements': 3, 'rolling-evaluations': 9000, 'rolling-position-limit': 9260,
    'original-changes': 15, 'optimized-changes': 0}})`, ui);
  assert.match(node('lb_fine_tuning').textContent, /Progress \+12.3 m/);
  assert.match(node('lb_fine_tuning').title, /Fixed final time/);
  assert.match(node('lb_fine_tuning').title, /lateral limit 1.0 nm/);
  assert.match(node('lb_fine_tuning').title, /3\/33 rolling windows improved/);
  assert.match(node('lb_fine_tuning').title, /rolling corridor 5.0 nm/);
  vm.runInContext(`displayFineTuning({'fine-tuning': {status: 'optimized'}})`, ui);
  assert.match(node('lb_fine_tuning').textContent, /Older solver/);
  vm.runInContext(`displayFineTuning({'fine-tuning': {status: 'unchanged', 'twa-step': 1,
    'minute-controls': false}})`, ui);
  assert.match(node('lb_fine_tuning').textContent, /Off-minute route/);
  assert.match(node('lb_fine_tuning').title, /schedule export unavailable/);
  console.log('Recorded-cycle wind replay, preceding-isochrone inference, sample time and failure tests passed.');
})().catch(error => { console.error(error); process.exitCode = 1; });
