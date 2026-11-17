#!/usr/bin/env python3
"""Build locally served Channel depth tiles from the official EMODnet 2024 E4 NetCDF.

Requires numpy and the system libnetcdf. No runtime GIS dependency in Bitsailor.
See diagnostics/BATHYMETRY.md for download, deployment and encoding details.
"""
import argparse
import ctypes as C
import ctypes.util
import hashlib
import gzip
import json
from pathlib import Path

import numpy as np


class NetCDF:
    """Read bounded arrays without loading the full multi-variable DTM into memory."""
    def __init__(self, path):
        self.lib = C.CDLL(ctypes.util.find_library("netcdf") or "libnetcdf.so")
        self.lib.nc_strerror.restype = C.c_char_p
        self.id = C.c_int()
        self.check(self.lib.nc_open(str(path).encode(), 0, C.byref(self.id)))

    def check(self, result):
        if result:
            raise RuntimeError(self.lib.nc_strerror(result).decode())

    def read(self, name, start, count):
        var = C.c_int()
        self.check(self.lib.nc_inq_varid(self.id, name.encode(), C.byref(var)))
        result = np.empty(count, dtype=np.float64)
        self.check(self.lib.nc_get_vara_double(
            self.id, var, (C.c_size_t * len(start))(*start),
            (C.c_size_t * len(count))(*count), result.ctypes.data_as(C.POINTER(C.c_double))))
        return result

    def length(self, name):
        dim, length = C.c_int(), C.c_size_t()
        self.check(self.lib.nc_inq_dimid(self.id, name.encode(), C.byref(dim)))
        self.check(self.lib.nc_inq_dimlen(self.id, dim, C.byref(length)))
        return length.value

    def attribute(self, name, attribute):
        var, length = C.c_int(), C.c_size_t()
        self.check(self.lib.nc_inq_varid(self.id, name.encode(), C.byref(var)))
        self.check(self.lib.nc_inq_attlen(self.id, var, attribute.encode(), C.byref(length)))
        text = C.create_string_buffer(length.value + 1)
        self.check(self.lib.nc_get_att_text(self.id, var, attribute.encode(), text))
        return text.value.decode()

    def close(self):
        self.check(self.lib.nc_close(self.id))


def encode(elevation, extrapolated):
    """Round shallowest depth down to centimetres; retain an uncertainty bit."""
    valid = np.isfinite(elevation)
    depth = np.floor(np.clip(-np.where(valid, elevation, 0), 0, 327.66) * 100)
    values = depth.astype(np.uint16)
    values[extrapolated != 0] |= 0x8000
    values[~valid] = 0xffff
    return values.astype('<u2')


def build(source, output):
    nc = NetCDF(source)
    try:
        if ('Lowest Astronomical Tide' not in nc.attribute('elevation_max', 'long_name')
                or nc.attribute('elevation_max', 'units') != 'm'):
            raise ValueError('Expected elevations in metres relative to LAT')
        lat = nc.read('lat', [0], [nc.length('lat')])
        lon = nc.read('lon', [0], [nc.length('lon')])
        step = 1 / 960
        if not (np.allclose(np.diff(lat), step, atol=1e-9, rtol=0)
                and np.allclose(np.diff(lon), step, atol=1e-9, rtol=0)):
            raise ValueError('Expected ascending EMODnet 1/16 arc-minute grid')
        output.mkdir(parents=True, exist_ok=True)
        # Half-degree chunks, native resolution, north-first rows. The westmost
        # chunk is partly outside E4 and deliberately retains unknown cells.
        for y in range(96, 104):
            for x in range(-13, 7):
                west, south = x / 2, y / 2
                target_lon = west + (np.arange(480) + 0.5) * step
                target_lat = south + (np.arange(480) + 0.5) * step
                ix = np.rint((target_lon - lon[0]) / step).astype(int)
                iy = np.rint((target_lat - lat[0]) / step).astype(int)
                if not (np.allclose(lon[0] + ix * step, target_lon, atol=1e-8, rtol=0)
                        and np.allclose(lat[0] + iy * step, target_lat, atol=1e-8, rtol=0)):
                    raise ValueError('Input grid is not aligned with output cell centres')
                xx, yy = np.where((ix >= 0) & (ix < len(lon)))[0], np.where((iy >= 0) & (iy < len(lat)))[0]
                tile = np.full((480, 480), 0xffff, dtype='<u2')
                if xx.size and yy.size:
                    start = [int(iy[yy[0]]), int(ix[xx[0]])]
                    count = [len(yy), len(xx)]
                    elevation = nc.read('elevation_max', start, count)
                    flags = nc.read('interpolation_flag', start, count)
                    tile[np.ix_(yy, xx)] = encode(elevation, flags)
                (output / f'{x}_{y}.bin.gz').write_bytes(gzip.compress(tile[::-1].tobytes(), mtime=0))
        digest = hashlib.sha256()
        with open(source, 'rb') as stream:
            for block in iter(lambda: stream.read(1024 * 1024), b''):
                digest.update(block)
        manifest = dict(version=1, dataset='EMODnet DTM 2024 E4', datum='LAT',
                        statistic='minimum depth (-elevation_max)', step=step,
                        chunkDegrees=0.5, size=480, bounds=[-6.5, 48, 3.5, 52],
                        sourceBounds=[-6.375, 48, 3.5, 52],
                        source='https://downloads.emodnet-bathymetry.eu/v12/E4_2024.nc.zip',
                        sourceSha256=digest.hexdigest(),
                        attribution='EMODnet Bathymetry Consortium (2024), CC BY 4.0',
                        doi='https://doi.org/10.12770/cf51df64-56f9-4a99-b1aa-36b8d7b743a1')
        # Publish metadata last. Build in a fresh directory for atomic deployments.
        (output / 'manifest.json').write_text(json.dumps(manifest, indent=2) + '\n')
        print(f'Built 160 native-resolution tiles in {output}')
    finally:
        nc.close()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('netcdf', type=Path)
    parser.add_argument('--output', type=Path, default=Path(__file__).resolve().parents[1] / 'web/bathymetry/channel-2024')
    args = parser.parse_args()
    build(args.netcdf, args.output)
