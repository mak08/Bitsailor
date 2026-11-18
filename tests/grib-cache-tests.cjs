// Run with node tests/grib-cache-tests.cjs.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const uv = sample => [sample.u, sample.v];
const calls = [], decodes = [];
const base = Date.parse('2026-11-18T00:00:00Z');
const grid = {gridDefinitionTemplateNumber: 0, Ni: 2, Nj: 2, scanningMode: 64,
    iDirectionIncrement: 1000000, jDirectionIncrement: 1000000,
    longitudeOfFirstGridPoint: 10000000, latitudeOfFirstGridPoint: 20000000};
const message = (step, component, value) => ({discipline: 10, totalLength: 1,
    PRODUCT_DEFINITION_SECTION: {parameterCategory: 1, parameterNumber: component,
        forecastTime: step, indicatorOfUnitOfTimeRange: 1},
    GRID_DEFINITION_SECTION: grid, DATA_SECTION: {data: Float32Array.from([value,value,value,value])},
    timestamp: () => new Date(base).toISOString()});
const messages = [message(0,2,1),message(0,3,2),message(3,2,3),message(3,3,4),message(6,2,5),message(6,3,6)];
const settings = {datasources: [{name: 'FW-CURRENT-ENGLISH-CHANNEL', datakind:'current',
    gribpaths: [{date:'20261118', run:0, step:0, path:'ibi.grib2'}]},
    {name:'ECMWF-IFS-CURRENT',datakind:'current',gribpaths:[
        {date:'20261118',run:0,step:0,path:'ifs0.grib2'},
        {date:'20261118',run:0,step:3,path:'ifs3.grib2'}]}]};
let fail = false, waiting;
const context = vm.createContext({console, Math, Date, Map, Set, Float32Array, Uint8Array, Number, JSON,
    AbortSignal, ensureServerSettings: async () => settings,
    decodeGRIB2ArrayBuffer(data, options) {
        decodes.push(options?.decodeData === false ? 'index' : 'field');
        return {messages: options?.decodeData === false ? Array.from(new Uint8Array(data), i=>messages[i]) : [messages[data[0]]]};
    },
    fetch: async url => {
        calls.push(url); if (waiting) await waiting;
        if (fail) return {ok:false, status:404};
        return {ok:true, arrayBuffer: async () => Uint8Array.from(url.includes('ifs0') ? [0,1] : url.includes('ifs3') ? [2,3] : messages.map((_,i)=>i)).buffer};
    }
});
vm.runInContext(fs.readFileSync('web/js/GribCache.js', 'utf8').replace(/^import .*;\n/gm, '')
    .replace('export function', 'function').replace('export default class', 'class').replace('export const', 'const')
    + '\nglobalThis.GribCache = GribCache; globalThis.sampleField = sampleField;', context);
const sample = (field, lat, lon) => uv(context.sampleField(field, lat, lon));
const field = {grid, u:[1,3,5,7], v:[2,4,6,8]};
assert.deepEqual(sample(field,20.5,10.5),[4,5]);
assert.deepEqual(sample(field,20.5,370.5),[4,5]);
assert.deepEqual(sample(field,22,10.5),[0,0]);
assert.deepEqual(sample(field,20.5,9),[0,0]);
assert.deepEqual(sample({...field,u:[NaN,3,5,7]},20.5,10.5),[3.75,4.5]); // Entire pair zeroed.
assert.deepEqual(sample({...field,u:[0,3,5,7],v:[0,4,6,8]},20.5,10.5),[3.75,4.5]);
assert.deepEqual(sample({...field,grid:{...grid,latitudeOfFirstGridPoint:21000000,scanningMode:0}},20.5,10.5),[4,5]);
assert.deepEqual(sample({...field,grid:{...grid,longitudeOfFirstGridPoint:11000000,scanningMode:192}},20.5,10.5),[4,5]);
assert.deepEqual(sample({...field,grid:{...grid,Ni:4,iDirectionIncrement:90000000,longitudeOfFirstGridPoint:0},
    u:[1,2,3,4,1,2,3,4],v:[2,4,6,8,2,4,6,8]},20.5,-45),[2.5,5]);
assert.deepEqual(sample({...field,grid:{...grid,longitudeOfFirstGridPoint:10125000}},20,10.125),[1,2]);
(async () => {
    const cache = new context.GribCache();
    const source='FW-CURRENT-ENGLISH-CHANNEL', time=new Date(base+1.5*3600000);
    let [a,b] = await Promise.all([cache.fields(source,time),cache.fields(source,time)]);
    assert.deepEqual(uv(a.sample(20.5,10.5)),[2,3]);
    assert.equal(calls.length,1); assert.equal(calls[0],'/weather/ibi.grib2');
    assert.equal(decodes.filter(x=>x==='index').length,1);
    assert.equal(decodes.filter(x=>x==='field').length,4); // Only two U/V pairs decoded.
    await cache.fields(source,time); assert.equal(calls.length,1); assert.equal(decodes.length,5);
    a=await cache.fields(source,new Date(base+99*3600000)); assert.deepEqual(uv(a.sample(20.5,10.5)),[5,6]);
    a=await cache.fields(source,new Date(base-3600000)); assert.deepEqual(uv(a.sample(20.5,10.5)),[1,2]);
    // IFS brackets across separate step files, while regional times share one file.
    const separate = new context.GribCache(), start = calls.length;
    a=await separate.fields('ECMWF-IFS-CURRENT',time);
    assert.deepEqual(uv(a.sample(20.5,10.5)),[2,3]);
    assert.deepEqual(calls.slice(start).sort(),['/weather/ifs0.grib2','/weather/ifs3.grib2']);
    await separate.fields('ECMWF-IFS-CURRENT',time);assert.equal(calls.length,start+2);
    // A missing first-frame vector still interpolates towards the next frame.
    const original=messages[0].DATA_SECTION.data;
    messages[0].DATA_SECTION.data=Float32Array.from([NaN,NaN,NaN,NaN]);
    const coastal=await new context.GribCache().fields(source,time);
    assert.deepEqual(uv(coastal.sample(20.5,10.5)),[1.5,2]);
    messages[0].DATA_SECTION.data=original;
    const controller=new AbortController();controller.abort();
    await assert.rejects(cache.fields(source,time,{signal:controller.signal}),/abort/i);
    await assert.rejects(cache.fields('NONE',time),/No GRIB steps/);
    await assert.rejects(cache.fields(source,'invalid'),/Invalid forecast display time/);
    const retry=new context.GribCache();fail=true;
    await assert.rejects(retry.fields(source,time),/HTTP 404/);fail=false;
    await retry.fields(source,time);
    const delayed=new context.GribCache();let resolve;waiting=new Promise(r=>{resolve=r;});
    const abort=new AbortController();const first=delayed.fields(source,time,{signal:abort.signal});
    await new Promise(r=>setImmediate(r));abort.abort();const second=delayed.fields(source,time);
    resolve();waiting=null;await assert.rejects(first,/abort/i);await second;
    // Both kinds at identical times in one GRIB share the file but not U/V pairs.
    const windMessage = (step, component, value) => {
        const m=message(step,component,value);m.discipline=0;
        m.PRODUCT_DEFINITION_SECTION.parameterCategory=2;return m;
    };
    messages.push(windMessage(0,2,10),windMessage(0,3,0),windMessage(3,2,-10),windMessage(3,3,0));
    settings.datasources.push({name:'TEST-WIND',datakind:'wind',gribpaths:[
        {date:'20261118',run:0,step:0,path:'mixed.grib2'}]},
        {name:'TEST-CURRENT',datakind:'current',gribpaths:[
        {date:'20261118',run:0,step:0,path:'mixed.grib2'}]});
    const mixed=new context.GribCache(), mixedStart=calls.length;
    const [wind,current]=await Promise.all([mixed.fields('TEST-WIND',time),mixed.fields('TEST-CURRENT',time)]);
    assert.equal(calls.length,mixedStart+1);
    assert.deepEqual(uv(current.sample(20.5,10.5)),[2,3]);
    assert.deepEqual(uv(wind.sample(20.5,10.5)),[0,0]);
    assert.equal(wind.sample(20.5,10.5).magnitude,10); // Never take |interpolated U/V| as speed.
    const missingField={...field,u:[NaN,3,5,7]};
    assert.deepEqual(uv(context.sampleField(missingField,20.5,10.5,'wind')),[5,6]); // AROME weights renormalize.
    const limits=new context.GribCache();
    for(const n of [0,1,2,3,4,0]) {
        settings.datasources.push({name:`LIMIT-${n}`,datakind:'wind',gribpaths:[
            {date:'20261118',run:0,step:0,path:`limit-${n}.grib2`}]});
        await limits.fields(`LIMIT-${n}`,time);
    }
    assert.equal(calls.filter(url=>url.includes('limit-')).length,6); // Fifth file evicts first.
    const queued=new context.GribCache(), networkStart=calls.length;
    let release;waiting=new Promise(resolve=>{release=resolve;});
    const parallel=Promise.all([0,1,2].map(n=>queued.fields(`LIMIT-${n}`,time)));
    await new Promise(resolve=>setImmediate(resolve));
    assert.equal(calls.length,networkStart+2); // Wind/current transfers share the same two slots.
    waiting=null;release();await parallel;
    assert.equal(calls.length,networkStart+3);
    console.log('Shared wind/current cache, grids, magnitude interpolation, forecast times, limits and retry tests passed.');
})().catch(error=>{console.error(error);process.exitCode=1;});
