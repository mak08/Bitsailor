// Run with node tests/router-startup-tests.cjs.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const main = fs.readFileSync('web/js/router-main.js', 'utf8').replace(/^import .*;\n/gm, '');
// Detect HTML/script mismatches before exercising the startup coordinator.
const html = fs.readFileSync('web/router-fw.html', 'utf8');
const ids = new Set([...html.matchAll(/\bid="([^"]+)"/g)].map(match => match[1]));
assert.ok(!ids.has('bt_addgate'), 'Gate creation must remain unavailable');
assert.ok(ids.has('bt_addwaypoint'), 'Point waypoints must remain available');
const setup = main.slice(main.indexOf('function setUp(getVMG)'), main.indexOf('function initMap()'));
for (const [, id] of setup.matchAll(/getElementById\("([^"]+)"\)\.addEventListener/g)) {
  assert.ok(ids.has(id), `Missing required startup control: ${id}`);
}
const routerSource = fs.readFileSync('web/js/router-main.js', 'utf8');
const gribSource = fs.readFileSync('web/js/GribCache.js', 'utf8');
const revision = 'router-race-ready-20261031';
const forecastRevision = 'router-forecast-contract-20261031';
assert.ok(html.includes(`js/router-main.js?v=${revision}`));
assert.ok(fs.readFileSync('web/startpage.html', 'utf8').includes(`router-fw.html?v=${revision}`));
assert.ok(routerSource.includes(`./GribCache.js?v=${forecastRevision}`));
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
async function scenario(readyState, failFirst, settingsFirst = false, failWind = false) {
  const nodes = new Map();
  const get = id => { if (!nodes.has(id)) nodes.set(id, new Node()); return nodes.get(id); };
  const calls = []; const requests = []; let ready;
  let fail = failFirst;
  const context = vm.createContext({
    console: { error() {} }, AbortSignal, Date,
    PolarManager: class {},
    document: { readyState, getElementById: get, createElement: () => new Node(),
      addEventListener: (event, callback) => { assert.equal(event, 'DOMContentLoaded'); ready = callback; } },
    window: {}, localStorage: { getItem: key => key === 'xx.polarsId' ? 'second' : null },
    setInterval() {},
    fetch: (url) => new Promise((resolve, reject) => {
      requests.push({ url, complete: () => fail ? reject(new Error('offline')) : resolve({ ok: true,
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
    redrawWindByOffset = async () => { calls.push('wind'); return windFailures-- <= 0; };
  `, context);
  // Expose a log to the wind stub without replacing updateMap's readiness guard.
  context.calls = calls;
  context.windFailures = failWind ? 1 : 0;
  vm.runInContext(main.slice(bootstrapAt), context);
  if (readyState === 'loading') { assert.equal(requests.length, 0); ready(); }
  assert.equal(requests.length, 2);
  assert.deepEqual(calls, []); // No premature wind draw, even on a map event.
  // Polars can complete before settings on a cold navigation.
  requests.find(r => r.url.endsWith(settingsFirst ? 'getServerSettings' : 'getPolarsList')).complete(); await tick();
  assert.deepEqual(calls, []);
  requests.find(r => r.url.endsWith(settingsFirst ? 'getPolarsList' : 'getServerSettings')).complete(); await tick();
  if (failFirst || failWind) {
    assert.equal(get('bt_retrydata').hidden, false);
    fail = false;
    const previous = requests.length;
    get('bt_retrydata').click();
    assert.equal(requests.length, previous + (failFirst ? 2 : 1)); // Retry failed settings; reuse successful settings.
    for (const request of requests.slice(previous)) request.complete();
    await tick();
  }
  assert.equal(get('sel_polars').options.length, 2);
  assert.equal(get('sel_polars').value, 'second');
  assert.equal(get('bt_retrydata').hidden, true);
  assert.deepEqual(calls, failWind ? ['wind', 'wind'] : ['wind']);
  vm.runInContext('setupPage()', context);
  assert.equal(requests.length, failFirst ? 4 : failWind ? 3 : 2); // No duplicate initialization.
  assert.equal(get('cb_hull').checked, true); // Default options exist before handlers.
}
(async () => {
  await scenario('loading', false);
  await scenario('complete', false);
  await scenario('complete', true);
  await scenario('loading', false, true);
  await scenario('complete', false, true, true);
  console.log('Router cold/late startup, dependency timing, defaults and retry tests passed.');
})().catch(error => { console.error(error); process.exitCode = 1; });
