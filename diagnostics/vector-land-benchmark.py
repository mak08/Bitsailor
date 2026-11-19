#!/usr/bin/env python3
"""Replay captured sailing segments against exact GDAL land predicates.
Requires system libgdal (including GEOS support); no Python packages or installs.
Prototype only: a separate process/dataset, never the running router's map layer.
"""
import argparse
from collections import OrderedDict
import ctypes as C
import ctypes.util
import csv
import json
import math
from pathlib import Path
import resource
import time

P = C.c_void_p
D = C.c_double
I = C.c_int
G = C.CDLL(ctypes.util.find_library('gdal'))
def api(name, restype, *args):
    fn = getattr(G, name)
    fn.restype = restype
    fn.argtypes = list(args)
    return fn
register = api('GDALAllRegister', None)
open_ds = api('GDALOpenEx', P, C.c_char_p, C.c_uint, P, P, P)
close_ds = api('GDALClose', None, P)
get_layer = api('GDALDatasetGetLayer', P, P, I)
filter_geom = api('OGR_L_SetSpatialFilter', None, P, P)
filter_rect = api('OGR_L_SetSpatialFilterRect', None, P, D, D, D, D)
reset = api('OGR_L_ResetReading', None, P)
next_feature = api('OGR_L_GetNextFeature', P, P)
get_feature = api('OGR_L_GetFeature', P, P, C.c_longlong)
fid = api('OGR_F_GetFID', C.c_longlong, P)
feature_geom = api('OGR_F_GetGeometryRef', P, P)
destroy_feature = api('OGR_F_Destroy', None, P)
new_geom = api('OGR_G_CreateGeometry', P, I)
clone_geom = api('OGR_G_Clone', P, P)
destroy_geom = api('OGR_G_DestroyGeometry', None, P)
set_point = api('OGR_G_SetPoint_2D', None, P, I, D, D)
intersects = api('OGR_G_Intersects', I, P, P)
wkb_size = api('OGR_G_WkbSize', I, P)
prepare = api('OGRCreatePreparedGeometry', P, P)
destroy_prepared = api('OGRDestroyPreparedGeometry', None, P)
prepared_intersects = api('OGRPreparedGeometryIntersects', I, P, P)

class Map:
    def __init__(self, filename):
        register()
        self.dataset = open_ds(str(filename).encode(), 4, None, None, None)
        if not self.dataset: raise RuntimeError('Cannot open land shapefile')
        self.layer = get_layer(self.dataset, 0)
        self.segment = new_geom(2)
        self.queries = self.features = self.predicates = 0

    def set_segment(self, coordinates):
        x0, y0, x1, y1 = coordinates
        set_point(self.segment, 0, x0, y0)
        set_point(self.segment, 1, x1, y1)

    def check(self, coordinates):
        self.set_segment(coordinates)
        self.queries += 1
        filter_geom(self.layer, self.segment)
        reset(self.layer)
        while feature := next_feature(self.layer):
            self.features += 1
            try:
                self.predicates += 1
                if intersects(feature_geom(feature), self.segment): return True
            finally: destroy_feature(feature)
        return False

    def close(self):
        destroy_geom(self.segment)
        close_ds(self.dataset)

class CachedMap(Map):
    def __init__(self, filename, prepared=True, cell=0.25, limit_mib=64):
        super().__init__(filename)
        self.prepared = prepared
        self.cell = cell
        self.limit = int(limit_mib * 1024**2)
        self.tiles = OrderedDict()
        self.geometries = OrderedDict()
        self.payload = self.peak_payload = self.peak_geometries = 0
        self.tile_hits = self.tile_misses = self.geometry_hits = self.geometry_misses = 0

    def dispose(self, entry):
        geometry, prepared, size = entry
        if prepared: destroy_prepared(prepared)
        destroy_geom(geometry)

    def remember(self, key, original):
        geometry = clone_geom(original)
        entry = geometry, prepare(geometry) if self.prepared else None, wkb_size(geometry)
        if self.prepared and not entry[1]:
            destroy_geom(geometry)
            raise RuntimeError('Prepared geometry unavailable')
        self.geometry_misses += 1
        self.geometries[key] = entry
        self.payload += entry[2]
        self.peak_payload = max(self.peak_payload, self.payload)
        self.peak_geometries = max(self.peak_geometries, len(self.geometries))
        # Keep one oversize geometry, so it remains alive through this query.
        while self.payload > self.limit and len(self.geometries) > 1:
            _, old = self.geometries.popitem(last=False)
            self.payload -= old[2]
            self.dispose(old)
        return entry

    def geometry(self, key):
        if key in self.geometries:
            self.geometry_hits += 1
            self.geometries.move_to_end(key)
            return self.geometries[key]
        feature = get_feature(self.layer, key)
        self.features += 1
        if not feature: raise RuntimeError(f'Missing feature {key}')
        try: return self.remember(key, feature_geom(feature))
        finally: destroy_feature(feature)

    def tile(self, x, y):
        key = x, y
        if key in self.tiles:
            self.tile_hits += 1
            self.tiles.move_to_end(key)
            return self.tiles[key]
        self.tile_misses += 1
        filter_rect(self.layer, x*self.cell, y*self.cell, (x+1)*self.cell, (y+1)*self.cell)
        reset(self.layer)
        ids = []
        while feature := next_feature(self.layer):
            self.features += 1
            try:
                key_id = fid(feature)
                ids.append(key_id)
                if key_id not in self.geometries: self.remember(key_id, feature_geom(feature))
            finally: destroy_feature(feature)
        self.tiles[key] = ids
        while len(self.tiles) > 4096: self.tiles.popitem(last=False)
        return ids

    def check(self, coordinates):
        self.set_segment(coordinates)
        self.queries += 1
        x0, y0, x1, y1 = coordinates
        ids = set()
        for y in range(math.floor(min(y0,y1)/self.cell), math.floor(max(y0,y1)/self.cell)+1):
            for x in range(math.floor(min(x0,x1)/self.cell), math.floor(max(x0,x1)/self.cell)+1):
                ids.update(self.tile(x,y))
        for key in ids:
            geometry, prepared, _ = self.geometry(key)
            self.predicates += 1
            if (prepared_intersects(prepared, self.segment) if prepared else intersects(geometry, self.segment)):
                return True
        return False

    def close(self):
        for entry in self.geometries.values(): self.dispose(entry)
        super().close()

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('shapefile', type=Path)
    parser.add_argument('segments', type=Path, help='CSV: start-lon,start-lat,end-lon,end-lat')
    parser.add_argument('--mode', choices=['baseline','cached','prepared'], default='baseline')
    parser.add_argument('--cell', type=float, default=.25)
    parser.add_argument('--limit-mib', type=float, default=64)
    parser.add_argument('--repeat', type=int, default=2)
    parser.add_argument('--reference', type=Path)
    parser.add_argument('--write-reference', type=Path)
    args = parser.parse_args()
    coordinates = [tuple(map(float, row)) for row in csv.reader(args.segments.open())]
    reference = args.reference.read_bytes() if args.reference else None
    obj = Map(args.shapefile) if args.mode == 'baseline' else CachedMap(args.shapefile,
        prepared=args.mode=='prepared', cell=args.cell, limit_mib=args.limit_mib)
    try:
        for repeat in range(args.repeat):
            counts = obj.features, obj.predicates
            start = time.perf_counter()
            results = bytes(obj.check(c) for c in coordinates)
            seconds = time.perf_counter() - start
            if reference is not None:
                if len(reference) != len(results): raise AssertionError('Reference length differs')
                mismatches = sum(a != b for a,b in zip(reference,results))
                if mismatches: raise AssertionError(f'{mismatches} land-check mismatches')
            if args.write_reference: args.write_reference.write_bytes(results)
            report = dict(mode=args.mode, repeat=repeat, seconds=seconds, queries=len(results),
                blocked=sum(results), features_read=obj.features-counts[0], predicates=obj.predicates-counts[1],
                max_rss_mib=resource.getrusage(resource.RUSAGE_SELF).ru_maxrss/1024)
            if isinstance(obj,CachedMap):
                report.update(tile_hits=obj.tile_hits,tile_misses=obj.tile_misses,
                    geometry_hits=obj.geometry_hits,geometry_misses=obj.geometry_misses,
                    payload_mib=obj.payload/1024**2, peak_payload_mib=obj.peak_payload/1024**2,
                    peak_geometries=obj.peak_geometries)
            print(json.dumps(report),flush=True)
    finally: obj.close()

if __name__ == '__main__': main()
