// Exercise actual module imports and the singleton used by both renderers.
import assert from 'node:assert/strict';
import {readFileSync} from 'node:fs';
import GribCache, {sharedGribCache} from '../web/js/GribCache.js?v=shared-grib-cache-20261118';
import WindLayer from '../web/js/WindLayer.js';
import CurrentLayer from '../web/js/CurrentLayer.js';
const current = readFileSync(new URL('../../JSGrib/tests/fixtures/ifs-current-ccsds-bitmap.grib2', import.meta.url));
const wind = Buffer.from(current);
// The same synthetic CCSDS fields, with wind parameter identifiers.
for (let offset = 0; offset < wind.length;) {
    const end = offset + Number(wind.readBigUInt64BE(offset + 8));
    wind[offset + 6] = 0;
    for (let section = offset + 16; section < end - 4; section += wind.readUInt32BE(section)) {
        if (wind[section + 4] === 4) {wind[section + 9] = 2; wind[section + 10] -= 12;}
    }
    offset = end;
}
const originalFetch = globalThis.fetch, originalLog = console.log;
const calls = [];
globalThis.fetch = async url => {
    calls.push(url);
    if (url === '/function/router.getServerSettings') return {ok:true,json:async()=>({datasources:[
        {name:'TEST-WIND',datakind:'wind',gribpaths:[{date:'20070323',run:12,step:0,path:'wind.grib2'}]},
        {name:'TEST-CURRENT',datakind:'current',gribpaths:[{date:'20070323',run:12,step:0,path:'current.grib2'}]}
    ]})};
    const data = url === '/weather/wind.grib2' ? wind : url === '/weather/current.grib2' ? current : null;
    assert.ok(data, `Unexpected request: ${url}`);
    return {ok:true,arrayBuffer:async()=>data.buffer.slice(data.byteOffset,data.byteOffset+data.byteLength)};
};
console.log = () => {};
try {
    assert.ok(sharedGribCache instanceof GribCache);
    const layer = new WindLayer(null, 'TEST-WIND');
    layer.drawWind = async () => {};
    await layer.update({}, '2007-03-23T12:00:00Z', '2007-03-23T12:00:00Z');
    const sample = await layer.getWind(59,3);
    assert.ok(sample.speed > 0);
    const data = await sharedGribCache.fields('TEST-CURRENT','2007-03-23T12:00:00Z');
    assert.equal(data.sample(59,3).u,-0.03125);
    // Real CurrentLayer must share this exact cache instance and metadata promise.
    const context = {clearRect(){},scale(){},save(){},restore(){},translate(){},rotate(){},beginPath(){},
        moveTo(){},lineTo(){},stroke(){},drawImage(){}};
    const canvas = {width:100,height:100,style:{},getContext:()=>context,
        getBoundingClientRect:()=>({width:100,height:100}),cloneNode(){return this;}};
    const currents = new CurrentLayer(canvas,{});
    await currents.update({north:60,south:58,west:0,east:6},'2007-03-23T12:00:00Z','TEST-CURRENT',true);
    await layer.getWind(59,3);
    assert.deepEqual(calls,['/function/router.getServerSettings','/weather/wind.grib2','/weather/current.grib2']);
} finally {
    globalThis.fetch = originalFetch; console.log = originalLog;
}
console.log('Actual wind/current module imports share one cache, metadata promise and downloaded fields.');
