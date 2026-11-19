#!/usr/bin/env python3
"""Exact-vector prototype checks using tiny local geometry and system GDAL."""
import importlib.util
import json
from pathlib import Path
import random
import tempfile

module_path = Path(__file__).resolve().parents[1] / 'diagnostics/vector-land-benchmark.py'
spec = importlib.util.spec_from_file_location('vector_land_benchmark', module_path)
m = importlib.util.module_from_spec(spec)
spec.loader.exec_module(m)

def polygon(rings): return {'type':'Polygon','coordinates':rings}
def ring(x0,y0,x1,y1): return [[x0,y0],[x1,y0],[x1,y1],[x0,y1],[x0,y0]]
features = [polygon([ring(0,0,3,3),ring(1,1,2,2)]), polygon([ring(4,1,4.05,1.05)]),
    {'type':'MultiPolygon','coordinates':[[ring(5,0,6,1)],[ring(5,2,6,3)]]}]
segments = [(-1,.5,.5,.5),(.5,.5,.6,.6),(1.2,1.2,1.8,1.8),(.5,1.5,1.5,1.5),
    (3.9,1.025,4.15,1.025),(3.9,1.1,4.15,1.1),(5.5,1.2,5.5,1.8),
    (5.5,-1,5.5,4),(3,-1,3,4),(-2,-2,-1,-1)]
# Fixed seed exercises long segments, multi-tile queries, boundaries and empty tiles.
rng = random.Random(9019)
segments += [tuple(rng.uniform(-1,7) for _ in range(4)) for _ in range(1000)]
with tempfile.TemporaryDirectory() as directory:
    path = Path(directory) / 'land.geojson'
    path.write_text(json.dumps({'type':'FeatureCollection','features':[
        {'type':'Feature','id':i,'properties':{},'geometry':g} for i,g in enumerate(features)]}))
    baseline = m.Map(path)
    try: reference = [baseline.check(s) for s in segments]
    finally: baseline.close()
    assert reference[:10] == [True,True,False,True,True,False,False,True,True,False]
    for prepared in [False,True]:
        # A tiny budget forces eviction/random rereads, even in this small dataset.
        cache = m.CachedMap(path, prepared=prepared, cell=.25, limit_mib=.0002)
        try:
            for repeat in range(2):
                assert [cache.check(s) for s in segments] == reference
        finally: cache.close()
print('Exact vector-cache holes, islands, multipolygons, tile boundaries and eviction checks passed (1,010 segments).')
