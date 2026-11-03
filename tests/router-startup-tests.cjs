// Run with node tests/router-startup-tests.cjs.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const main = fs.readFileSync('web/js/router-main.js', 'utf8').replace(/^import .*;\n/gm, '');
// Detect HTML/script mismatches before exercising the startup coordinator.
const html = fs.readFileSync('web/router-fw.html', 'utf8');
const ids = new Set([...html.matchAll(/\bid="([^"]+)"/g)].map(match => match[1]));
assert.ok(ids.has('bt_addgate'), 'Gate creation control must be available');
assert.ok(ids.has('bt_addwaypoint'), 'Point waypoints must remain available');
const setup = main.slice(main.indexOf('function setUp(getVMG)'), main.indexOf('function initMap()'));
for (const [, id] of setup.matchAll(/getElementById\("([^"]+)"\)\.addEventListener/g)) {
  assert.ok(ids.has(id), `Missing required startup control: ${id}`);
}
const routerSource = fs.readFileSync('web/js/router-main.js', 'utf8');
const gribSource = fs.readFileSync('web/js/GribCache.js', 'utf8');
const revision = 'router-twa-fine-tuning-20261103';
const forecastRevision = 'router-forecast-contract-20261031';
assert.ok(html.includes(`js/router-main.js?v=${revision}`));
assert.ok(fs.readFileSync('web/startpage.html', 'utf8').includes(`router-fw.html?v=${revision}`));
assert.ok(routerSource.includes('./GribCache.js?v=router-forecast-selection-20261103'));
for (const source of [routerSource, gribSource]) {
  assert.ok(source.includes(`./ServerSettings.js?v=${forecastRevision}`));
}
const server = fs.readFileSync('web/js/ServerSettings.js', 'utf8').replace(/export /g, '');
const bootstrapAt = main.lastIndexOf('// Initialize everything when the page loads');
const tick = () => new Promise(resolve => setImmediate(resolve));
class Node {
  constructor() { this.options = []; this.hidden = true; this.textContent = ''; }
  get value() { return this.selected ?? this.options[0]?.value ?? ''; }
  set value(value) { this.selected = value; }
  add(option) { this.options.push(option); }
  replaceChildren() { this.options = []; this.selected = undefined; }
  addEventListener(_, callback) { this.click = callback; }
}
async function scenario(readyState, failFirst, settingsFirst = false) {
  const nodes = new Map();
  const get = id => { if (!nodes.has(id)) nodes.set(id, new Node()); return nodes.get(id); };
  const calls = []; const requests = []; let ready;
  const alerts = [];
  const context = vm.createContext({
    console: { error() {} }, AbortSignal, Date, alert: message => alerts.push(message),
    PolarManager: class {},
    document: { readyState, getElementById: get, createElement: () => new Node(),
      addEventListener: (event, callback) => { assert.equal(event, 'DOMContentLoaded'); ready = callback; } },
    window: {}, localStorage: { getItem: key => key === 'xx.polarsId' ? 'second' : null },
    setInterval() {},
    fetch: (url) => new Promise((resolve, reject) => {
      requests.push({ url, complete: () => failFirst ? reject(new Error('offline')) : resolve({ ok: true,
        json: async () => url.endsWith('getServerSettings') ? { datasources: [] } : [{ id: 'first', label: 'First' }, { id: 'second', label: 'Second' }] }) });
    })
  });
  vm.runInContext(server + main.slice(0, bootstrapAt), context);
  // Keep the real startup coordinator, polar population, server cache and handlers.
  vm.runInContext(`
    initMap = () => { map = { on() {} }; };
    setUp = () => { ir_index = document.getElementById('ir_index'); ir_index.value = 0; updateMap(); };
    setupRaces = () => {};
    fillForecastModelDropdown = () => {};
    fillCurrentsDropdown = () => {};
    updateForecastRange = () => {};
    getLatestCycleFromSettings = () => '2026-10-31T06:00:00Z';
    startWindDataUpdateCheck = () => {};
    getMapBounds = () => ({});
    GribCache = class {};
    redrawWindByOffset = async () => { calls.push('wind'); return true; };
  `, context);
  // Expose a log to the wind stub without replacing updateMap's readiness guard.
  context.calls = calls;
  vm.runInContext(main.slice(bootstrapAt), context);
  if (readyState === 'loading') { assert.equal(requests.length, 0); ready(); }
  assert.equal(requests.length, 2);
  assert.deepEqual(calls, []); // No premature wind draw, even on a map event.
  // Polars can complete before settings on a cold navigation.
  requests.find(r => r.url.endsWith(settingsFirst ? 'getServerSettings' : 'getPolarsList')).complete(); await tick();
  assert.deepEqual(calls, []);
  requests.find(r => r.url.endsWith(settingsFirst ? 'getPolarsList' : 'getServerSettings')).complete(); await tick();
  assert.deepEqual(alerts, failFirst ? ['Could not load forecast settings and polars.'] : []);
  assert.ok(!ids.has('router-data-status'));
  assert.ok(!ids.has('bt_retrydata'));
  if (!failFirst) {
    assert.equal(get('sel_polars').options.length, 2);
    assert.equal(get('sel_polars').value, 'second');
  }
  assert.deepEqual(calls, failFirst ? [] : ['wind']);
  vm.runInContext('setupPage()', context);
  assert.equal(requests.length, 2); // No duplicate initialization.
  assert.equal(get('cb_hull').checked, true); // Default options exist before handlers.
}
function testRaceDuration() {
  const selectHTML = html.match(/<select id="sel_duration"[^>]*>([\s\S]*?)<\/select>/)[1]
    .replace(/<!--[\s\S]*?-->/g, '');
  const options = [...selectHTML.matchAll(/<option value="(\d+)"([^>]*)>/g)]
    .map(([, value, attrs]) => ({ value, defaultSelected: attrs.includes('selected') }));
  const selector = { options };
  const context = vm.createContext({
    PolarManager: class {},
    document: { getElementById: id => id === 'sel_duration' ? selector : null, querySelectorAll: () => [] },
    window: {}, localStorage: { setItem() {} }
  });
  const run = code => vm.runInContext(code, context);
  run(main.slice(0, bootstrapAt));
  run("restoreRaceGates = drawRouteLine = () => {}; currentRaceName = 'test'; races = [{ name: 'test', settings: {} }];");
  for (const [saved, hours] of [['48', 48], [259200, 72], [24, 24], [0, 96], [undefined, 96], [12345, 96]]) {
    context.saved = saved;
    run('applyRaceSettings({ settings: { duration: saved } })');
    assert.equal(selector.value, String(hours));
    const query = new URLSearchParams(run('makeQuery(settings)'));
    assert.equal(query.get('duration'), String(hours * 3600));
  }
  selector.value = '120';
  run('onSetDuration({ target: document.getElementById("sel_duration") })');
  assert.equal(run('races[0].settings.duration'), 432000);
  run('applyRaceSettings(races[0])');
  assert.equal(selector.value, '120');
}
(async () => {
  testRaceDuration();
  await scenario('loading', false);
  await scenario('complete', false);
  await scenario('complete', true);
  await scenario('loading', false, true);
  console.log('Router cold/late startup, dependency timing, defaults and error dialog tests passed.');
})().catch(error => { console.error(error); process.exitCode = 1; });
