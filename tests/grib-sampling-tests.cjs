// Actual decoder + client cache integration. Optional argument: cl-weather/weather directory.
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const vm = require('node:vm');
const uv = sample => [sample.u, sample.v];
(async () => {
    const {decodeGRIB2ArrayBuffer} = await import('../../JSGrib/JSGrib.js');
    const log = console.log; console.log = () => {};
    let calls = 0, settings, root;
    const context = vm.createContext({console, AbortSignal, Math, Date, Map, Set, Number, JSON,
        Float32Array, Uint8Array, decodeGRIB2ArrayBuffer,
        ensureServerSettings: async () => settings,
        fetch: async url => {calls++; const b=fs.readFileSync(path.join(root,url.slice('/weather/'.length)));
            return {ok:true,arrayBuffer:async()=>b.buffer.slice(b.byteOffset,b.byteOffset+b.byteLength)};}
    });
    vm.runInContext(fs.readFileSync('web/js/GribCache.js','utf8').replace(/^import .*;\n/gm,'')
        .replace('export function','function').replace('export default class','class').replace('export const','const')+'\nglobalThis.GribCache=GribCache;',context);
    root=path.resolve('../JSGrib/tests/fixtures');
    settings={datasources:[{name:'ECMWF-IFS-CURRENT',datakind:'current',gribpaths:[
        {date:'20070323',run:12,step:0,path:'ifs-current-ccsds-bitmap.grib2'}]}]};
    const cache=new context.GribCache();
    const field=await cache.fields('ECMWF-IFS-CURRENT','2007-03-23T12:00:00Z');
    assert.deepEqual(uv(field.sample(59,3)),[-0.03125,0.21875]);
    assert.equal(calls,1);
    await cache.fields('ECMWF-IFS-CURRENT','2007-03-23T13:00:00Z');assert.equal(calls,1);
    log('Actual CCSDS bitmap decoding, zero-filled sampling and client cache integration passed.');
    if(process.argv[2]) {
        root=path.resolve(process.argv[2]);
        for(const [name,file,date,run,time,lat,lon,kind="current"] of [
            ['FW-CURRENT-ENGLISH-CHANNEL','ibi_english_channel/20261118/ibi_english_channel.20261118.grib2','20261118',0,'2026-11-18T04:30:00Z',49,-4],
            ['FW-CURRENT-AGULHAS','global_agulhas/20261118/global_agulhas.20261118.grib2','20261118',0,'2026-11-18T09:00:00Z',-35,25],
            ['ECMWF-IFS-CURRENT','ifs-current/0p25/20261118/12/ifs-current.0p25.20261118.t12z.f000.grib2','20261118',12,'2026-11-18T12:00:00Z',50,-3.5],
            ['NOAA-GFS-WIND','gfs/0p25/20261118/12/gfs.0p25.20261118.t12z.f000.grib2','20261118',12,'2026-11-18T12:00:00Z',50,-3.5,'wind'],
            ['ECMWF-IFS-WIND','ifs/0p25/20261118/12/ifs.0p25.20261118.t12z.f000.grib2','20261118',12,'2026-11-18T12:00:00Z',50,-3.5,'wind'],
            ['AROME-WIND-WESTERN-MED','arome/western_med/20261118/12/arome.t12z.western_med.f006-f048.grib2','20261118',12,'2026-11-19T07:30:00Z',41,6,'wind']
        ]) {
            settings={datasources:[{name,datakind:kind,gribpaths:[{path:file,date,run,step:0}]}]};
            const instance=new context.GribCache();const before=calls;
            const loaded=await instance.fields(name,time);
            const vector=uv(loaded.sample(lat,lon));assert.ok(vector.every(Number.isFinite));
            await instance.fields(name,time);assert.equal(calls,before+1);
            log(`${name}: sampled ${lat}, ${lon} at ${time}: ${vector.map(n=>n.toFixed(6))} m/s; redraw reused GRIB`);
        }
    }
    console.log = log;
})().catch(error=>{console.error(error);process.exitCode=1;});
