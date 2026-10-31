# Twelve-day routing memory investigation

The reported no-waypoint request was reproduced with `duration=1036800` (12 days), start `2026-10-31T13:00`, start position `(46.09037664604301, -1.3836558255284217)`, destination `(-8.928487062665504, -28.828125)`, `c_bass_polar`, GFS wind and waves enabled.

## Measured result

A fresh SBCL process with a **3072 MiB dynamic space** completed the 12-day search and JSON serialization. It returned `max_duration`, having simulated the full 12 days; it did not reach the destination within the requested duration. Search time was about 36 seconds in this environment. This does not reproduce the running server's heap exhaustion.

After a full GC with the result still retained:

| Item | MiB |
| --- | ---: |
| Decoded GFS wind arrays | 1552.5 |
| Decoded wave-height arrays | 117.0 |
| Decoded wave-direction arrays | 117.0 |
| Total forecast array payload | 1786.5 |
| Total live dynamic heap | 1891.0 |

There were 216 forecast-cache entries. The full JSON response contained 2,484,918 characters. After releasing the routing result and collecting again, live dynamic heap was 1887.3 MiB: most memory remains reachable through global data, principally the forecast cache. The difference is not an exact size measurement of the routing result because GC roots and temporaries differ between samples.

Cumulative allocation through the search was about 4403 MiB, rising to about 4603 MiB after serialization. Cumulative allocation is not simultaneously live memory and is not evidence that a 6 GiB heap is required.

## Cache retention mechanism

`cl-weather/forecast.cl` stores decoded global grids in `cl-weather::*forecast-ht*`. A 0.25-degree GFS snapshot contains two double-float arrays and occupies about **15.8 MiB**. Each three-hour forecast requested along the simulated route adds a snapshot. The geographical route box does not crop these global grids.

The combined-wave loader decodes height and direction for all steps in the file at the first request. Together these accounted for 234 MiB in this reproduction. The recently added direction field therefore adds memory, but does not alone explain exhausting 6 GiB.

The cache is global across requests and keyed by forecast step, source, run, date and resolution. Running the same request against the same cycle reuses entries. Requests using another cycle or another model can accumulate another substantial collection of grids.

`forecast-ht-cleanup` removes entries whose **cycle base time** is more than 12 hours old. It does not limit cache bytes or entry count and does not release already-used steps as a route advances. The cleanup timer is scheduled at 05:10, 11:10, 17:10 and 23:10. At those boundaries a cycle exactly 12 hours old is removed by the next cleanup because the comparison is strictly older-than. Several large recent cycles can therefore coexist, particularly between scheduled cleanups.

Consequently, accumulated forecast caches are the leading explanation for the difference between a fresh process completing below 3 GiB and an established server exhausting 6 GiB. This remains a hypothesis about that server until its live cache and actual configured heap limit are measured. Background work and simultaneous requests can add further memory pressure.

## Inspect the running image

Load the diagnostic into the existing server's Lisp REPL:

```lisp
(load "/home/michael/Repository/Bitsailor/diagnostics/memory-report.cl")
(router::report-routing-memory)
```

It reports the actual SBCL dynamic-space limit, current dynamic usage, decoded forecast-array payload and a breakdown by source/cycle/resolution. It neither clears caches nor changes routing. To distinguish live data from reclaimable allocations, optionally run:

```lisp
(router::report-routing-memory :gc t)
```

The optional full collection pauses the Lisp image. Heap figures exclude foreign/native allocations such as GDAL and ecCodes. Array totals deduplicate shared array objects and count specialized single/double-float payloads, excluding headers and cache metadata.

If multiple forecast cycles/models account for most of the live heap, the durable remedy belongs in forecast-cache management: a byte budget with eviction of cache references, while active interpolation parameters continue holding any arrays they need. Simply retaining only one global interpolation pair would cause requests at different times to evict each other's data. If cache totals are small despite a large post-GC heap, the next step is a live-object breakdown of that server rather than increasing cache eviction.

No routing behaviour or auxiliary-system code was changed during this investigation.
