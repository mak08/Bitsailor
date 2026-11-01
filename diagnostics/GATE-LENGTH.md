# Sensitivity to gate length

## Current experiment: common artificial origin

The working-tree search now uses a common artificial origin behind each gate,
away from the next target, at `max(1000 metres, 2 × gate width)` by default.
`*gate-origin-distance-factor*` adjusts the multiplier, clamped to at least two.
Buckets share this geographic origin, with a bounded nonlinear bearing transform
which improves forward resolution. Steering remains tied to actual crossings.
A target behind the incoming course can be pursued during the remaining
fractional crossing step. See [MULTI-LEG-ROUTING.md](../MULTI-LEG-ROUTING.md).

This is an experimental replacement for the midpoint frame, not a proof that
longer gates can never produce worse approximate routes. Gate endpoints still
influence the reference frame and coverage retirement. Gate UI controls have been re-enabled for the artificial-origin implementation. The diagnosis and measurements below describe the **previous
midpoint-origin implementation**, before this experiment.

### Controlled comparison of the experiment

Both gate geometries were run at the identical `2026-10-31T22:14` start time,
with the original pinned October 5 wind cycle, the same polars/options, real land
checks, and **waves disabled in all four runs**:

| Implementation | Long gate | Short gate |
| --- | --- | --- |
| Previous midpoint frame | 16h 16m | 15h 01m |
| Artificial origin, transformed buckets and turn-back remainder | 15h 01m | 15h 16m |

The longer gate no longer gives the worse result in this comparison. The
remaining 15-minute difference shows that this is still a geometry-sensitive
approximation. These numbers assess the combined experiment, not the origin
placement in isolation. The invalid old cycle is retained only to hold wind
inputs fixed; it is not a forecast recommendation.

The requested wave-enabled replay failed in the baseline implementation when
wave selection switched to the November 1 combined file: one interpolation
frame was NIL (`forecast-fraction NIL ...`). Thus the table is a wind-only
comparison, not a successful replay of the original wave-enabled request.
Constant-field regressions separately exercise wave/current adjustments during
a turned remainder, multi-gate touches, land along a bent step, and the shared
endpoint clock. Gate controls have since been re-enabled; the forecast-cycle request behavior is unchanged.

## Bug: crossing histories share a radial dominance rule

The longer gate must not produce a worse optimum if it admits the shorter
gate's winning route. The reported result violates that expectation. Moving the
pruning frame explains what triggers the difference, but is not a justification
for discarding the better route. The defect is in frontier pruning after a gate;
the evidence below does not implicate the segment/gate intersection calculation.

`gate-state-key` identifies a state only by `(stage, bearing bucket)`, with the
bearing measured from the gate midpoint for a post-crossing stage. Neither the
actual crossing position nor its crossing time is part of the key.
`retain-gate-state` then applies two filters to that key:

1. Within a step, keep only the candidate furthest from the midpoint.
2. Across steps, reject any candidate whose radius does not exceed the maximum
   previously recorded by `record-gate-progress` for that key.

This pools separate crossing histories. An earlier arrival through one part of
the gate can establish a large radius in a bucket and suppress later arrivals
through another part. Those later candidates can be making useful progress
toward the next gate while still lying inside that unrelated radial maximum.
Moreover, their permitted search fans depend on their own actual crossing
positions, so a shared midpoint bucket does not even imply identical permitted
continuations. A larger midpoint radius does not establish dominance between
these states. The same problem affects the within-step winner comparison as
well as historical pruning.

This is close to the suspected mixing of bucket times, but the timestamp values
themselves are not being confused. All candidates in a given expansion step have
the same absolute endpoint time; fractional crossing times remain on the
crossing routepoints. The historical table stores only a radius, with no time or
crossing-origin identity. It is the **meaning of that radius as evidence of
progress** that is incorrectly shared across crossing histories.

### Instrumented evidence

An instrumented long-gate reproduction checked every candidate endpoint against
the expansion's scheduled endpoint time, and every current-bucket comparison
against its existing winner's time. It observed:

| Check | Count |
| --- | ---: |
| Comparisons against an existing current-step winner | 5,681,014 |
| Endpoint timestamp mismatches | 0 |
| Current-step comparisons involving different crossing origins | 361,451 |
| Historical radial rejections involving different crossing origins | 257,474 |

For example, stage 1 / bucket 644 rejected this land-valid candidate against a
historical state from a different crossing. Times below are UTC on November 1:

| Property | Rejected candidate | Historical state |
| --- | --- | --- |
| Endpoint time | 04:20:00 | 04:10:00 |
| Gate crossing time | 04:11:02 | 03:28:35 |
| Radius from stage midpoint | 62,677 m | 119,033 m |
| Distance to next gate | 179,935 m | 184,946 m |

The candidate is about 5 km closer to the next gate, but the old 119 km radial
mark rejects its 63 km radius. The historical state is not a current-step
competitor, and its different origin gives it a different continuation fan.
This demonstrates the cross-history rejection concretely; it does not establish
that this particular rejected candidate belongs to the shorter winning track.
The reference-frame experiments below independently connect this pruning
mechanism to the reproduced ETA difference.

These measurements instrumented the existing algorithm without changing its
selection rules. At the time of those measurements no routing correction had yet been applied; the subsequent experiment is described above.

The two reported requests were reproduced against the current working-tree router, including the crossing-centred fan change. Both were also run with the same start time, `2026-10-31T22:14`, to isolate gate geometry. These are duration measurements from this reproduction, not claims of globally optimal ETA.

## Controlled comparisons

| Configuration | Long gate | Short gate |
| --- | --- | --- |
| Current midpoint-based pruning | 16h 16m | 15h 01m |
| Gate retirement disabled for all stages | 16h 16m | 15h 01m |
| Short gate, but stage-1 pruning reference fixed to the long gate's midpoint | — | 16h 16m |
| Same reference override, retirement disabled | — | 16h 16m |
| Experimental reference: original leg start for every stage | 15h 16m | No route |
| Experimental reference: first land-valid crossing for each stage | 15h 01m | 15h 16m |

The original short-gate start time, `22:15`, produced a 15h 00m duration, giving the same minute of arrival as the `22:14` short-gate reproduction. The original one-minute difference therefore does not explain the large route change here.

The long-gate selected track also intersects the shortened gate, and the short-gate selected track intersects the long gate. This was checked against the serialized track segments using the router's local-projection intersection geometry; small serialization rounding remains. The geometry change is therefore not simply excluding the compared routes.

## Dominant cause: a movable pruning frame

Stage 1 uses the midpoint of gate 1 as its common angular-bucket origin and radial-progress reference. Shortening gate 1 moves that reference by roughly 50 km. It changes:

- the bearings and bucket assignments of candidate endpoints;
- which candidate has the largest radius and wins each bucket;
- which movements are rejected by historical radial high-water marks;
- the coarse search box (whose large margins do not appear restrictive in this example).

The fan continues to use each state's actual crossing point. The experiment forcing the short gate to use the long gate's midpoint changes the shared pruning reference and box while leaving gate targets, crossing geometry and actual-crossing fan logic intact. It restores the long gate's duration. Disabling retirement leaves the two durations unchanged. This identifies the movable pruning frame as the dominant source of this example's sensitivity.

In particular, a crossing south of the long gate's midpoint may initially need to travel toward that midpoint to reach the next gate. Such travel can decrease radius relative to the pruning reference, despite being useful progress toward the next target. Bucket reassignment and the one-winner rule also discard alternative crossing histories. Keeping a crossing point feasible does not guarantee that its useful continuation survives those filters.

The short gate does retire its preceding stage earlier: 04:50 UTC versus 07:00 UTC in the current-reference runs. That is a separate length-dependent heuristic, but the retirement-disabled comparison shows it does not account for the ETA difference here.

The first endpoint is also moved slightly off the original gate's projected supporting line (about 0.9 km), so this is not an exactly collinear truncation. That does not invalidate the reference experiment or the track-crossing checks, but it matters when designing an exact invariance regression.

## Why a reference replacement needs care

Both replacement schemes above were tested in isolated diagnostic processes, without changing production source. Using the original start for all stages fails the shortened-gate case. Anchoring each stage at its first valid crossing reduces this example's duration gap to 15 minutes, but still changes candidate selection and is itself dependent on which crossing is encountered first. Neither experiment establishes a generally reliable replacement.

A correction must remove the unsupported dominance comparison between crossing histories. Simply moving the fan origin was insufficient; choosing a different common origin alone does not establish that a candidate dominates another. Keeping one winner per bucket requires defining buckets whose candidates can legitimately compete under the chosen approximation. Adding every crossing object's identity to the key would isolate histories, but would also multiply frontier size and retained ancestry; it must not be introduced without a bounded-state design and memory/performance checks. Endpoint time alone is not a sufficient additional key: the within-step candidates already share it.

The acceptance criterion for a correction includes the feasible-route comparison above, with identical start times, alongside an exactly collinear gate-shortening regression. Merely improving one ETA by moving the reference again would not resolve the demonstrated state aliasing.

## Separate forecast-cycle issue

Both requests explicitly pin `2026-10-05T12:00:00Z`, with a start on October 31. That GFS run's 384-hour horizon ends on October 21. The weather library currently clamps later times to step 384 rather than rejecting the request. The reproduced responses confirm October 5 as their actual initial wind cycle.

Thus both routes use the final, frozen wind field from that old run, alongside separately selected wave data. This is not the cause of the length-dependent difference, because the controlled comparisons share those inputs, but these requests do not represent October 31 GFS wind.
