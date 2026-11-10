// Cycle rollover uses downloaded server metadata, commits after loading, and
// leaves working/recorded wind untouched during unavailable or raced updates.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync('web/js/router-main.js', 'utf8').replace(/^import .*;\n/gm, '');
const oldCycle = '2026-11-07T12:00:00.000Z';
const newCycle = '2026-11-07T18:00:00.000Z';
const nodes = new Map();
const node = id => {
    if (!nodes.has(id)) nodes.set(id, { style: {}, value: 0 });
    return nodes.get(id);
};
let metadata, reloads = 0, attempts = [], alerts = [], failure = false, pending, pause = false, reloadPause;
const ctx = vm.createContext({
    Date, PolarManager: class {}, console: { error() {} },
    document: { getElementById: node }, alert: text => alerts.push(text),
    getServerSettings: () => metadata,
    reloadServerSettings: async () => { reloads++; if (reloadPause) await reloadPause; },
    update: async (cycle) => {
        attempts.push(cycle.toISOString());
        if (pause) await new Promise(resolve => { pending = resolve; });
        if (failure) throw new Error('not downloaded');
    }
});
vm.runInContext(source.slice(0, source.lastIndexOf('// Initialize everything when the page loads')), ctx);
const run = code => vm.runInContext(code, ctx);
function ready(run) {
    metadata = { datasources: [{ name: 'NOAA-GFS-WIND', gribpaths: [0, 3].map(step =>
        ({ date: '20261107', run, step, path: `run-${run}-${step}` })) }] };
}
function reset() {
    ready(12); attempts = []; alerts = []; failure = false; pause = false; reloadPause = null;
    node('lb_modelrun').textContent = 'old';
    run(`currentCycle = '${oldCycle}'; routeWindSelection = null; windReady = true;
         ir_index = document.getElementById('ir_index'); ir_index.value = 0;
         gribCacheModel = settings.forecast_model = 'NOAA-GFS-WIND';
         getMapBounds = () => ({}); updateIsochrones = () => {};
         gribCache = { update: (_, cycle) => update(cycle),
                       clearCache: () => { throw new Error('Do not discard working data'); } };`);
}
(async () => {
    reset();
    // At/after nominal availability, the server still advertises downloaded 12Z.
    await run('checkForWindDataUpdates()');
    assert.equal(attempts.length, 0);
    assert.equal(run('currentCycle'), oldCycle);
    assert.equal(run('getLatestCycleFromSettings()'), oldCycle);

    ready(18); pause = true;
    const loading = run('checkForWindDataUpdates()');
    await new Promise(resolve => setImmediate(resolve));
    assert.equal(attempts[0], newCycle);
    assert.equal(run('currentCycle'), oldCycle);
    assert.equal(node('lb_modelrun').textContent, 'old');
    pending(); await loading;
    assert.equal(run('currentCycle'), newCycle);
    assert.equal(node('lb_modelrun').textContent, '2026-11-07 18Z');

    reset(); ready(18); failure = true;
    await run('checkForWindDataUpdates()');
    assert.equal(run('currentCycle'), oldCycle);
    assert.equal(run('windReady'), true);
    assert.equal(alerts.length, 0); // Background retries are not user-facing errors.
    assert.equal(node('lb_modelrun').textContent, 'old');

    reset(); ready(18);
    let finishReload;
    reloadPause = new Promise(resolve => { finishReload = resolve; });
    const checking = run('checkForWindDataUpdates()');
    run(`routeWindSelection = { cycle: '${oldCycle}' };`);
    finishReload(); await checking;
    assert.equal(attempts.length, 0); // Recorded selection during reload wins.
    const before = reloads;
    await run('checkForWindDataUpdates()');
    assert.equal(reloads, before);

    reset(); metadata.datasources[0].gribpaths = [];
    assert.equal(await run("setForecastModel('NOAA-GFS-WIND')"), false);
    assert.equal(attempts.length, 0); // No clock-based guess or /weather request.
    assert.match(alerts[0], /No available forecast cycle/);
    console.log('Downloaded-cycle rollover, atomic commit, quiet retry, recorded selection and no-clock-fallback tests passed.');
})().catch(error => { console.error(error); process.exitCode = 1; });
