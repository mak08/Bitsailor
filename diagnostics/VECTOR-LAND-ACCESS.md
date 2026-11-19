# Exact vector land checks: cache/preparation experiment

19 November 2026. **Vector map access can be substantially improved.** The next
step should be productionizing a bounded exact-vector cache. The measurements
do not justify starting bitmap precomputation yet.

## What was tested

The previous investigation identified detailed GDAL/GEOS land checks as the
main cost. The existing 0.1° tile cache only records land presence. It does not
retain polygons or prepared intersection structures.

The experiments use the existing indexed land-polygon shapefile, GDAL 3.12.4
and GEOS 3.14.1. No new libraries were installed. Tests ran in separate processes;
no serving router or CL-MAP source files were modified. Existing pending
fine-tuning/UI changes were left intact.

The route was the supplied La Rochelle–Azores example: `c_bass_polar`, all six
polar options, standard stepping, four-day limit, departure
`2026-11-18T21:31`, `(46.083333333333336, -1.4166666666666288)` to
`(38.35, -26.433333333333394)`, waves enabled, currents/marks/gates/reverse
routing disabled. GFS was pinned to 18 November 12Z and the wave file to the
18 November combined forecast. The tests time forward search, excluding tuning.

## Identical-query replay

Captured **15,229 detailed queries** from 147,914 bucket winners before land
filtering. A query is four doubles: the start and end longitude/latitude.
All variants processed the same queries, in the same order, and returned exactly
the same **4,418 blocked results**, with zero mismatches.

| Method | First pass | Warm pass | Warm improvement |
| --- | ---: | ---: | ---: |
| Current indexed per-segment GDAL access | 9.266 s | 9.331 s | 1× |
| Cached OGR polygons, ordinary intersection | 2.713 s | 2.520 s | 3.7× |
| Cached prepared polygons, 64 MiB payload budget | 0.537 s | 0.153 s | 61× |
| Cached prepared polygons, 4 MiB payload budget | 0.468 s | 0.291 s | 32× |

These are single first/warm pass pairs, not confidence intervals. The 4 MiB
first pass happening to beat the larger cache's first pass should not be read
as a general advantage: loading order, allocator state and filesystem cache
vary. The repeated-query improvement is large enough to warrant the next step.

The replay prototype uses Python only to orchestrate native GDAL calls through
`ctypes`. Geometry algorithms still run in GDAL/GEOS. It uses 0.25° cache cells,
a 4,096-cell LRU index and a payload-budgeted geometry LRU, keyed by feature ID.
The larger-cache run read 9,957 features to initialize 440 cache cells, prepared
9,362 distinct geometries, and performed no feature reads on the warm pass.
The small cache reread/evicted geometries but retained most of the speed gain.

0.1° cache cells were also tested: 0.854 s first, 0.096 s warm, with identical
answers. They required 1,647 tile loads rather than 440. Thus cell size is a
tradeoff between initialization work and the number of candidate polygons per
query; it is not a raster resolution or a change in land-check accuracy.

## Full Common Lisp routing experiment

Implemented the same approach as an isolated CFFI prototype, retaining the
existing map mutex. It temporarily substitutes the detailed predicate in the
existing isochrone filter, then restores the original function and frees the
cache on exit. Search, pruning, polar/wind/wave calculations and step sizes
remain the same. The existing coarse endpoint-tile gate is also retained to
isolate the access change.

| Method | Time | Generated candidates | Candidates/sec |
| --- | ---: | ---: | ---: |
| Current vector access, first route | 27.016 s | 5,846,630 | 216,416 |
| Current vector access, warm route | 10.545 s | 5,846,630 | 554,436 |
| Prepared cache, first route | 1.385 s | 5,846,630 | 4,220,303 |
| Prepared cache, warm route | 1.311 s | 5,846,630 | 4,460,097 |

The full-search warm improvement was **8.0×**. Both prepared-cache runs passed
exact comparisons of the serialized best track **and all emitted isochrones**
against the warm baseline. Candidate counts also matched. Fine-tuning was not
part of these timings.

This demonstrates that Common Lisp is suitable: keeping the expensive work in
GDAL/GEOS and reusing its results matters much more than the orchestration
language. A bitmap generator is not required to reach the expected throughput
on this representative route.

## Why it helps

1. Select candidate polygon IDs once per spatial cache cell, using the existing
   shapefile index and a rectangular spatial filter.
2. Cache each polygon by feature ID, rather than duplicating it in every cell.
3. Build its prepared geometry once, retaining both the cloned OGR geometry and
   the prepared handle until eviction.
4. For each sailing segment, visit all cache cells touched by its bounding box,
   deduplicate polygon IDs and perform exact prepared intersection predicates.
5. On eviction, destroy prepared handles before the corresponding OGR clones.
   Tile entries contain IDs, not pointers; an evicted geometry can be reread
   by feature ID without invalidating the tile index.

The existing shapefile index already narrows the geographical search. The
improvement additionally avoids repeated polygon reading, conversion and
intersection preparation. Caching only the query line is insufficient.

The APIs are available directly through GDAL's C interface:
[prepared geometry creation and predicates](https://gdal.org/en/stable/api/vector_c_api.html#_CPPv425OGRCreatePreparedGeometry12OGRGeometryH),
[spatial filtering and feature access](https://gdal.org/en/stable/api/vector_c_api.html).

## Memory cost and production work

In separate Python processes, peak RSS was about **109 MiB** for the current
access method, **263 MiB** for the larger prepared cache, and **151 MiB** for
the 4 MiB prepared-cache budget. The latter used roughly **42 MiB additional
peak RSS**. These are whole replay-process measurements, not native-cache
allocation counters or full-router memory measurements.

The cache budget counts serialized WKB bytes, which **is not a strict bound on
native memory**. Prepared indexes, OGR clones, allocator overhead and object
metadata consume more. The experiment can transiently exceed the payload
budget while preparing a new geometry, and retains one oversize geometry.
A production version needs an explicit oversize/fallback policy and conservative
native-memory accounting, plus feature/tile count limits. Native allocations
are outside SBCL's dynamic-space limit.

Recommended production scope:

- Per-map cache ownership and invalidation when the dataset changes, with
  configurable limits and hit/miss/eviction statistics.
- Keep the existing mutex initially. Cached geometry lifetime and GEOS context
  use must remain synchronized; relaxing locks is a separate optimization.
- Correct exception-safe geometry cleanup. Existing point/segment helpers
  destroy their temporary query geometry *inside* the feature loop: no-feature
  paths leak it, and multi-feature paths can reuse a freed handle. Cleanup
  belongs once after iteration, including early returns. The prototypes avoid
  that lifetime pattern.
- Preserve exact predicates and conservative candidate selection. The test
  cache covers a segment's entire bounding box. Handle the antimeridian and
  unusually long segments explicitly before making this a general API.
- Benchmark additional sailing regions and concurrent requests. One route
  establishes feasibility, not universal speedup or a remote-host result.

The existing endpoint-tile shortcut can miss intermediate land on a segment;
that behavior is unchanged by this comparison. This experiment does not adopt
any new approximation or exploit the newly permitted short-land-crossing
tradeoff.

## Reproduce

Configure and load Bitsailor/maps/polars as usual, but use an **isolated Lisp
process**, never a running service: the benchmark temporarily replaces the
isochrone filter's global function.

```lisp
(load "diagnostics/vector-land-prototype.cl")
;; ROUTING is a configured single-leg request with locally available forecasts.
(router::benchmark-vector-land-access routing :payload-mib 4 :cell 0.25d0)
(router::capture-vector-land-checks routing "/tmp/land-segments.csv")
```

Replay the corpus with the system GDAL library; no Python packages are needed:

```sh
python3 diagnostics/vector-land-benchmark.py /path/to/land_polygons.shp \
  /tmp/land-segments.csv --mode baseline --write-reference /tmp/land-reference.bin
python3 diagnostics/vector-land-benchmark.py /path/to/land_polygons.shp \
  /tmp/land-segments.csv --mode prepared --limit-mib 4 \
  --reference /tmp/land-reference.bin
python3 tests/vector-land-cache-tests.py
```

The synthetic regression suite checks 1,010 segments across polygon holes,
small islands, multipolygons, exact tile boundaries, empty cells and forced
geometry eviction, with zero mismatches against ordinary GDAL predicates.
The corpus stays outside Git. These tools are diagnostics, not enabled backends.

## Production implementation (19 November)

The prepared cache is now in CL-MAP, enabled by default with a **1000 MiB global
estimated-native accounting budget** (increased from the initial 500 MiB). It adds bounded feature/cell LRUs, exact
fallbacks for oversized inputs, per-map ownership, synchronized reload/close,
exception-safe cleanup and Status counters. The memory charge is conservative
(16 × WKB + 4096 per entry), not measured RSS; see `cl-map/README.md`.

With the actual production implementation and its initial **500 MiB** budget,
the same isolated route produced:

| Production mode | Seconds | Candidates/sec |
| --- | ---: | ---: |
| Indexed first run | 23.708 | 246,606 |
| Indexed warm | 10.720 | 545,410 |
| Prepared first | 1.469 | 3,978,829 |
| Prepared warm | 1.084 | 5,391,367 |

All runs generated 5,846,630 candidates and passed exact serialized best-track
and isochrone comparisons. The final cache contained 9,362 features, 440 cells,
21.0 MiB WKB payload and 373.1 MiB conservative accounting charge, with no
evictions or fallbacks on this run. No claim is made that the accounting charge
is actual native allocation. Synthetic lifecycle/concurrency tests also pass.

Load `diagnostics/production-vector-cache-benchmark.cl` and call
`(router::benchmark-production-vector-cache routing)` for the new benchmark.
It uses the production enable switch and does not replace global functions.
