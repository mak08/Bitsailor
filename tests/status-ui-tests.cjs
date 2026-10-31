// Run with node tests/status-ui-tests.cjs. Exercises rendering and refresh failures.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
class Element {
  constructor() { this.children = []; this.cells = []; this.hidden = false; this.textContent = ''; }
  replaceChildren() { this.children = []; this.cells = []; }
  insertRow() { const row = new Element(); this.children.push(row); return row; }
  insertCell() { const cell = new Element(); this.cells.push(cell); return cell; }
  append(...nodes) { this.children.push(...nodes); }
  addEventListener(_, handler) { this.click = handler; }
}
const elements = new Map();
const get = id => { if (!elements.has(id)) elements.set(id, new Element()); return elements.get(id); };
let ready, payload, fail = false;
const context = vm.createContext({
  document: {
    getElementById: get,
    querySelector: selector => { const node = get(selector); if (selector.endsWith('thead tr')) node.cells = [1, 2, 3]; return node; },
    createElement: () => new Element(), addEventListener: (_, callback) => { ready = callback; }
  },
  console: { error() {} }, setInterval() {}, AbortSignal, Date,
  fetch: async () => { if (fail) throw new Error('offline'); return { ok: true, text: async () => JSON.stringify(payload) }; }
});
const tick = () => new Promise(resolve => setImmediate(resolve));
(async () => {
  payload = { 'server-start': '2026-10-31T13:00:00Z', requestcount: 7, 'fc-cache-size': 3,
    memory: { 'heap-used': 1024 ** 3, 'heap-limit': 2 * 1024 ** 3, 'forecast-bytes': 512 * 1024 ** 2,
      groups: [{ source: 'GFS', date: '20261031', run: 6, resolution: '0p25', entries: 3, bytes: 512 * 1024 ** 2 }] },
    parameters: [{ group: 'Gates', name: '*gate-retirement-coverage*', value: '1/2 (50.0%)' }],
    caches: { polars: 20, 'combined-polars': 2, races: 0 }, downloads: false, last_routestats: false };
  vm.runInContext(fs.readFileSync('web/js/startpage.js', 'utf8'), context);
  ready(); await tick();
  assert.match(get('status-heap').textContent, /1.0 GiB \/ 2.0 GiB \(50.0%\)/);
  assert.equal(get('status-heap-bar').value, 50);
  assert.equal(get('status-forecast-bytes').textContent, '512.0 MiB');
  assert.equal(get('#status-cache-groups tbody').children[0].cells[1].textContent, '20261031 06Z');
  assert.match(get('#status-parameters tbody').children[0].cells[2].textContent, /50.0%/);
  assert.equal(get('status-error').hidden, true);
  fail = true; get('status-refresh').click(); await tick();
  assert.equal(get('status-error').hidden, false);
  assert.equal(get('status-refresh').disabled, false);
  assert.match(get('status-heap').textContent, /50.0%/); // Preserve stale data on failure.
  fail = false; payload = { memory: {}, parameters: [], downloads: [], last_routestats: { start: 'now', duration: '1d', calctime: 0, steps: 0, points: 0 } };
  get('status-refresh').click(); await tick();
  assert.equal(get('status-error').hidden, true);
  assert.equal(get('status-heap-bar').hidden, true);
  assert.equal(get('status-heap').textContent, 'Unavailable');
  assert.equal(get('status-last-route').children.length, 16);
  assert.match(get('#status-cache-groups tbody').children[0].cells[0].textContent, /No decoded forecasts/);
  console.log('Status UI rendering, empty data and refresh recovery tests passed.');
})().catch(error => { console.error(error); process.exitCode = 1; });
