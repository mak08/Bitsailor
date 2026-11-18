# Routing throughput investigation — 18 November 2026

The reported slowdown is reproducible locally. The supplied La Rochelle–Azores
route is dominated by coastline intersection checks, and the displayed
positions/sec number additionally includes fine-tuning time in its denominator.
The investigation did not reproduce a multi-fold regression in candidate
expansion or find evidence that missing optimization declarations explain it.

## Reproduction

Used the supplied coordinates, `c_bass_polar`, all six polar options, standard
stepper, departure `2026-11-18T21:31`, four-day limit, GFS wind, no currents,
marks, gates, secondary-model switch or reverse routing. For reproducibility,
the local benchmark pinned GFS to `2026-11-18T12:00Z`; waves used the local
18 November combined forecast. The request to aguas-00 omitted a cycle, so its
actual selected/fallback cycle has not been independently verified.

Tests ran in isolated SBCL processes with 6144 MiB dynamic space, local forecast
files and the normal indexed land-polygon shapefile. They did not start an HTTP
service, send requests to aguas-00 or change the running application's settings.
Warm measurements followed a complete execution in the same process.

| Case | Generated candidates | Reported calculation time | Positions/sec |
| --- | ---: | ---: | ---: |
| Cold search, waves | 5,853,300 | 22.867 s | 255,974 |
| Warm search, no waves | 5,516,700 | 10.693 s | 515,911 |
| Warm search, waves, sampled profile | 5,853,300 | 10.621 s | 551,103 |
| Warm full computation, no waves | 5,516,700 | 13.234 s | 416,872 |
| Warm full computation, waves | 5,853,300 | 18.511 s | 316,213 |

Both full computations used all 12,000 fine-tuning trial evaluations. Comparing
the separate warm runs, the added tuning time is approximately 2.5 seconds
without waves and 7.9 seconds with waves. These differences are estimates from
separate executions, not direct phase-timer readings. The helper below records
phase timings directly for future comparisons. All these runs reached the
destination.

A separate IMOCA run along the same course also reproduced the pattern: warm
search around 0.68M candidates/sec with and without waves, falling to 0.39M with
waves when fine-tuning was included.

## What positions/sec measures

`expand-routepoint` increments its candidate counter for sampled TWA commands
before box/fan rejection and bucket pruning. It is not a count of retained
positions, successful moves, or trial integrations during fine-tuning.

`get-route-leg` measures search time and writes a search-only trace through
`log-stats`. Subsequently, `fine-tune-route` replaces `routestats.calctime` with
search plus post-processing time, preserving the original candidate count.
`web/js/startpage.js` divides `points` by that combined `calctime`. Consequently,
Status throughput and the search trace describe different amounts of work.
Forecast loading during the search is included; response serialization and
network transfer are not included in this statistic.

This makes the fine-tuning feature and its increased trial budgets visible as
lower positions/sec even when candidate expansion has not slowed down. Waves
add height/direction interpolation and heading-dependent speed calculations to
the many trial integrations. Their cost is more noticeable in post-processing
than in this route's coastline-dominated search.

## Profile findings

The warm waves search yielded 1,041 CPU samples at 10 ms intervals:

- `filter-isochrone`: 923 samples, **88.7% inclusive**.
- Frontier expansion lambda: 115 samples, **11.0% inclusive**.
- Most filtering samples were in GDAL/GEOS: decoding polygon coordinate
  sequences, building intersection structures, reading shapefile records, and
  testing intersections. Wave circular-interpolation helper frames appeared
  in around 1% of the inclusive samples, rather than dominating the search.

The coastline path is `filter-isochrone` → `cl-map:intersects-land-p` → cached
coarse-tile lookup and, near land, `line-intersects-land-p`. The latter repeatedly
reads geometries from the roughly 1.3 GB local shapefile and calls GDAL/GEOS.
The coarse tile cache avoids detailed checks over empty ocean; it does not
cache decoded coastal geometries or their prepared intersection structures.
The detailed checks also use CL-MAP's shared map lock, so concurrent routing or
other map operations can further increase wall time. Remote lock contention was
not measured in this investigation.

As a diagnostic **only**, bypassing `filter-isochrone` in an isolated process
produced 10,794,844 candidates in 1.975 s, or **5.47M candidates/sec**. This changes
the frontier and allows land crossings, so it is not a valid result or a direct
same-route speedup comparison. It demonstrates that candidate generation can
still reach the expected order of throughput when coastline work is removed.
No land-check bypass was applied to production code.

## Regression and compiler checks

Loaded optimization policy was `speed 3`, `safety 1`, `debug 1`. Inspection of the
compiled search showed optimized floating-point instructions. Adding an
unspecified global optimization declaration is not supported as the remedy by
these measurements.

An additional controlled comparison used `expand-routepoint` from commit
`e6a3dd3` with the current leg driver, weather, polar, bucket filtering and
stepper. This isolates the older expansion implementation from changes to
forecast selection and schedules:

| Expansion implementation | Candidates | Time | Candidates/sec |
| --- | ---: | ---: | ---: |
| Current | 5,853,300 | 10.982 s | 532,989 |
| Historical expansion | 5,853,300 | 10.678 s | 548,139 |

The approximately 3% difference in this single comparison does not establish a
large expansion regression. It is not a full historical application benchmark:
it deliberately keeps the current weather and route driver. The recent client
GRIB/cache/UI refactor does not change server-side forward-search code.

## Recommended next changes

1. Report search time, fine-tuning time and search candidates/sec separately,
   retaining total calculation time. This makes throughput comparisons useful
   without reducing the requested optimization budget.
2. Optimize the exact coastline path: instrument detailed-check counts and
   times, then evaluate a bounded cache of decoded/prepared coastal geometries.
   Preserve exact intersections and thread safety. A warm GRIB cache alone
   does not eliminate this work.
3. Profile fine-tuning separately before changing its wave handling or budgets.
   It should retain the same physical wave effects and no-worse-ETA constraints.
4. Run the phase benchmark on aguas-00 with identical forecast cycles and
   parameters. The historical 1.5–4M figures cannot yet be attributed to a
   particular version, dataset, system-library version or concurrency level.

## Reusable benchmark

Load `diagnostics/routing-performance.cl` after loading Bitsailor and preparing
maps and polars as for a normal request. It starts no services and modifies no
production settings. Use locally available forecasts. For example:

```lisp
(load "diagnostics/routing-performance.cl")
(router::benchmark-routing-performance
 (router::get-routing-presets
  :polars-id "c_bass_polar"
  :forecast-model 'cl-weather:noaa-gfs-wind
  :cycle (cl-weather::make-cycle%
          :timestamp (router::parse-datetime "2026-11-18T12:00:00Z"))
  :starttime "2026-11-18T21:31"
  :stepmax 345600
  :slat 46.083333333333336d0 :slon -1.4166666666666288d0
  :dlat 38.35d0 :dlon -26.433333333333394d0))
```

The default performs two search-only repetitions for each wave setting, then
one full computation for each, returning and printing measurements. Initial
runs warm both forecasts and coastline caches. It uses the configured
fine-tuning settings; those are not disabled or reduced by the helper.
