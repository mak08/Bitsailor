# Route steering fine-tuning

`fine-tune-route` in [fine-tune.cl](fine-tune.cl) runs after the complete
isochrone/waypoint search, through `get-route`. It re-sails short sections with
integer signed TWA controls, optional fixed compass headings and fitted durations to remove oscillation caused by the router's
coarse TWA samples. Low-level `get-route-leg` and `get-route-through-gates` remain
search primitives; they do not recursively fine-tune their results.

A control is explicitly **TWA plus duration** or **compass heading plus duration**.
TWA mode recomputes heading from local wind direction plus the commanded TWA.
Heading mode holds the compass command and recomputes physical TWA from the wind.
Both use the selected polar sail/speed, waves and currents. A fixed compass
heading is not a fixed course over ground when current causes drift.

## Integer TWA controls

The target application accepts whole-degree TWA only. Fine-tuning therefore tries
**integer angles on a 1-degree grid** and solves for segment durations. It never
rounds a continuous-angle solution and assumes the endpoint stayed valid.
`*fine-tune-angle-limit* = 5` is a deviation bound, not a steering step. Polar
lookup may use its 0.1-degree table internally, but accepted steering controls
are integer-valued.

For example, alternating 105/110-degree controls can become a 107/108-degree
pair with fitted durations. In a constant-wind, 5 m/s fixture the same endpoint
is reached in 1918.246 seconds rather than 1920 seconds. The earlier continuous
prototype used approximately 107.5 degrees and reached it in 1918.173 seconds;
that fractional plan is not executable by the application and is no longer used.
These measurements used 1 mm endpoint closure to distinguish the small ETA gaps.
They are a controlled example, not a general ETA guarantee.

`diagnostics/twa-granularity.cl` compares fixed 5-degree/1-degree controls with
the current bounded integer solver. Source code must be loaded into the running
image after a change; compiling files on disk does not replace live definitions.

## Fixed-heading alternative for TWA oscillation

With `*fine-tune-heading-enabled*` true (the default), a same-tack window may also
be fitted with one whole-degree compass heading. Eligibility requires at least
one reversal between nearby TWA controls, no tack/gybe, and a range no wider
than twice `*fine-tune-angle-limit*`. Monotone course changes are not eligible.

Candidate whole-degree headings are tried around both the endpoint bearing and
the time-weighted compass headings of the original route, so current-induced
leeway does not confuse compass heading with ground course. The heading trial
reuses the normal wind/polar/current/wave integrator. Physical TWA may be
fractional under a heading command; the command itself is still integer-valued.

When one heading cannot close the endpoint, a final window may use one heading
and a short integer-TWA tail; an intermediate window may use one heading and
two short integer-TWA corrections. Their durations are fitted, with at least
75% of the window under the fixed heading. This is an explicit mixed-mode plan,
not a constant-heading approximation silently labelled as TWA.

Each eligible window has at most `*fine-tune-heading-evaluations*` (default 80)
additional heading trial integrations. Candidates compete with the existing
integer-TWA fits under the same closure, corridor, land, ordered-target,
manoeuvre and no-later-ETA checks. Their realized TWA must remain within the
original local range plus the angle margin, and no new tack/gybe is allowed.
The original and TWA-only choices remain available if heading fitting fails.

Heading replay samples local wind, speed, polar VMG limits and TWA at least every
60 seconds, including when baseline replay retains longer original steps or the
configured trial step is larger. Each heading command's endpoint is evaluated
with fresh wind before switching to another control, even a TWA command. The
endpoint sample also participates in local TWA-range checks. Thus long heading
commands cannot hide adverse angles between five-minute checks. TWA trial replay
also defaults to 60 seconds; control and forecast-switch boundaries can shorten
steps. This uses the game's one-minute timescale without claiming to reproduce
its proprietary interpolation, integration or control-update timing exactly.

Change counts now measure **command mode/value changes**. A constant heading
with varying measured TWA is one command, not repeated TWA instructions.

## Objective and hard constraints

Completed (`reached`) routes retain their geographic endpoint and may never
arrive later. Among feasible candidates with the same arrival time, prefer fewer
steering-command changes, then smaller total measured TWA variation.

For duration-limited (`max_duration`) routes, keep the original final timestamp
exactly and maximize great-circle distance from `routing-start`. Greater distance
has priority over steering simplicity; equal-distance candidates use the steering
score below. Progress may not decrease. This optimizes the selected partial
route at its actual search horizon, which can precede the requested duration by
a search step. It does not claim that the destination has been reached.
A small secondary cost discourages unnecessary movement of manoeuvre times.
The implementation scores an equal-time window as:

```text
1000 × steering-command-change count
+ total absolute TWA variation in degrees
+ 0.001 × sum of absolute manoeuvre time changes in seconds
```

A control change smaller than 0.05 degrees is ignored in the score/count; the
exported control plan itself retains the actual fitted angles. The completed-route whole-route check additionally rejects an increase in the
control-change count. A partial route may add commands when it gains distance.

Every published candidate must satisfy:

- The same tack/gybe sequence and count, in order. A sign flip is classified as a
  tack when the mean absolute TWA on its two sides is below 90 degrees, otherwise
  as a gybe. The original route has no explicit manoeuvre labels, so this is the
  classification used consistently for both routes.
- Each manoeuvre within 300 seconds of its corresponding original one, with
  position constrained by the same one-sided corridor as other computed points.
- At each sample time, the reference position defines a great-circle ray from
  the original route start. A candidate may advance along this ray without a
  longitudinal cap, provided its absolute cross-track distance is at most
  `*fine-tune-position-limit*` (500 m). Backward displacement retains the old
  500 m great-circle distance bound. Along-track projection distinguishes the
  forward ray from the opposite half of its great circle, including at the date
  line. Near the origin the ordinary distance bound applies. Time-aligned
  reference interpolation wraps longitude; it is used only to measure deviation.
- Fixed endpoints close numerically within 1 m. There is no heading-based correction,
  position snap, waiting segment or appended straight-line final approach.
- Valid polar TWA/VMG angles and positive boat speed throughout replay.
- Land checks over every simulated segment, using the configured vector/bitmap
  land predicate. Bitmap mode retains its existing endpoint-only limitations.
- All originally passed targets remain passed in order; completed routes must
  pass every requested target. Unreached targets do not invalidate partial routes. Gate crossings within one
  segment must follow their spatial order; touches within the closure tolerance
  count. Point waypoints are window boundaries and must still be reached within
  that same numerical tolerance.
- Completed routes retain the final geographic endpoint within the closure
  tolerance and cannot arrive later, including when the search stopped slightly
  beyond the destination. Partial routes retain the exact final timestamp and
  must finish at least as far from the origin, within the lateral bound.

This is a local refinement of the selected route, not a new global route search.
It will not remove a tack or gybe, search another side of an island, or accept a
later ETA merely to make steering simpler.

## Constrained shooting

The reference route is divided into windows of approximately one hour, splitting
at point waypoints, before a second manoeuvre, and after substantial local TWA
range changes. A window can therefore contain zero or one manoeuvre. Intermediate
window endpoint times remain fixed. Only the completed route's final window may
end earlier. Computed intermediate anchors may move outward; user-requested
point waypoints remain fixed boundaries, and gates must still be touched in order.
The origin for measuring this movement is always the original route start, not
an artificial gate-pruning origin.

Each trial contains at most three constant integer-TWA segments:

1. Select whole-degree candidates near the time-weighted original TWA, including
   coarse angles and nearby one-degree neighbours. Preserve the original side
   of the wind, within five degrees of its original local angle range.
2. First try one/two integer controls. In an intermediate window, fit the split
   with total duration fixed. One split cannot generally satisfy two endpoint
   coordinates, so only actual closing solutions are accepted.
3. When necessary, try three integer controls with two independent durations;
   the third duration is the fixed remainder. Around a manoeuvre use two controls
   before it and one after, or one before and two after. This preserves the side
   sequence and lets the switch time move subject to the hard manoeuvre bounds.
4. In the final window, solve the total duration and split of two fixed integer
   controls. A single fixed integer TWA with variable duration is also tested.
   Earlier arrival wins; later arrival is forbidden.

For a free outward anchor, endpoint closure is replaced by forward progress and
lateral checks. Local backward movement within the old distance bound is still
allowed; only the full partial route must finish no closer to the origin. One integer TWA or compass heading is sailed for the full window;
two-TWA trials search the duration split, first at the original and quarter-window
fractions, then refining the best valid split at 5%, 1% and 0.2% increments. The
original controls remain an incumbent. These trials maximize radius rather than
shooting back to the old anchor. Later windows start at the actual new endpoint.

Oscillating equal-time windows additionally try both **A–B–A** and **B–A–B**
patterns before the general fits. Each uses two independent switch durations.
Intermediate points may have a residual within the corridor: following segments
cancel it by the window's fixed rejoin position and timestamp. This avoids
requiring a smoothed prefix to land on every discarded search trackpoint.
Originally the three-angle generator always chose different first/last edge
angles, accidentally omitting these correction patterns. The new trials share
the existing budgets and use the same integer controls and hard checks.

Finite-difference, damped shooting solves continuous **durations**, not angles.
One-parameter fits minimize endpoint residual but must still actually close;
two-parameter fits use a Newton solve for the two endpoint coordinates. There
are at most eight iterations per trial. Fitting closes to 1 cm internally,
tighter than the 1 m acceptance limit, so small position errors do not grow
through many later baseline windows and invalidate all previous improvements.

The endpoint acceptance tolerance is a positional comparison budget, not a time
allowance, integration step or land-clearance buffer. Its default is now 1 m;
shooting still aims for 1 cm. It lets slightly different numerical replays rejoin
without discarding a useful fit. No snapping or extra final leg is introduced.
Increasing it permits a different rejoin/final position at the same clock time,
which can make an apparent ETA gain partly reflect a shorter trip. For scale,
1 m is 0.1 seconds of sailing at 10 m/s. The independent corridor, land and
manoeuvre checks still apply. This parameter is also used for requested point
waypoints and numerical gate touches, so large values weaken those requirements
as well. It should not be enlarged to hide an infeasible continuation.

The original continuous implementation failed the supplied November 4 Atlantic
case at window 34: accumulated replay error reached 0.522 m against the 0.5 m
limit. Its response reported 520 evaluations and `unchanged`, explaining why
new controls were not visible. It did run; it then rolled the entire result back.

Each window has a 240-integration budget, in addition to the whole-route budget.
Only a few two-control trials are attempted in fixed-time windows, reserving
work for the three-control fits that rotating wind often needs. Trial integration
uses at most 60-second steps by default and splits exactly at control boundaries and the
absolute forecast-model switch. Parameters are cached by exact sample timestamp;
weather is still interpolated at each trial position.

Intermediate window times stay fixed. This remains a bounded local refinement,
not a search for the globally fastest manoeuvre schedule.

## Acceptance, rollback and cost limits

A window without an acceptable improvement retains its original TWA controls,
re-sailed from the actual preceding endpoint with the original step lengths.
This preserves position continuity without silently snapping junctions. Original
coarse controls are not subjected to a fictitious fine-step replay and then
assumed equivalent: if their replay cannot satisfy the endpoint, corridor,
land or manoeuvre checks, the entire optimization is rejected.

The complete proposed route is checked again for endpoint, arrival time,
control-change count, ordered targets and manoeuvres before replacing `best`.
An error, missing forecast, unsupported reference or failed validation retains
the original route. `no_route` results are not optimized. `max_duration` results
use the fixed-time progress objective.

For completed routes the established closed-anchor pass runs first as an
incumbent. A second pass permits outward intermediate anchors using only the
remaining evaluation/time budget. It replaces that incumbent only with an
earlier arrival, or an equal arrival with no worse steering score. Thus a greedy
outward anchor that makes the fixed destination impossible to close cannot
remove an improvement already found by the closed-anchor solver.

For partial routes an inexpensive incumbent first retains the original prefix
and refines only the final window. The full outward pass then competes against
that result. If accumulated displacement makes a later corridor infeasible, the
valid suffix-only improvement survives instead of rolling everything back.
Both passes share the same route-level budgets. No-op baseline replay is not
reported as an optimization.

Trial work is bounded by 3,000 integrations and 10 wall-clock seconds, checked
between integrations. These are trial budgets, not a hard interruption of an
individual GRIB load or the baseline replay/validation. After budget exhaustion,
remaining windows can retain their original controls. A report identifies a
budget-limited result. The algorithm does not retain a tree of trial routes.

All operational parameters are listed on the Status tab. Set
`router::*fine-tune-enabled*` to NIL to disable the pass. Other settings are
`*fine-tune-outward-enabled*`, `*fine-tune-heading-enabled*`, `*fine-tune-heading-evaluations*`,
`*fine-tune-step*`, `*fine-tune-window*`, `*fine-tune-angle-limit*`,
`*fine-tune-position-limit*`, `*fine-tune-time-limit*`,
`*fine-tune-endpoint-tolerance*`, `*fine-tune-fit-tolerance*`,
`*fine-tune-max-window-evaluations*`, `*fine-tune-max-evaluations*` and
`*fine-tune-max-seconds*`.

## API and display

The response adds:

- `steering-segments`: the general executable plan, with `mode` (`twa` or
  `heading`), integer `value`, `start-time`, `end-time` and fractional-second
  `duration`.
- `twa-segments`: for backward compatibility, pure-TWA plans only; empty for
  mixed/heading routes. Compact controls with `start-time`, `end-time`, signed integer `twa` and `duration` in seconds.
  Consecutive equal angles are coalesced; the `duration` field preserves fractional seconds even if the displayed timestamps are rounded. This is the executable integer-TWA plan. If an unmodified legacy route has fractional controls and cannot be converted safely, this field is empty rather than publishing it as an executable integer plan.
- `original-best`: the original track when an optimized track was accepted.
- `fine-tuning`: outcome/reason, evaluation count, original/optimized change
  counts and arrival times, `objective` (`earliest-arrival` or `origin-distance`),
  `outward-anchors`, original/optimized origin distances and `distance-gain` in
  metres, plus maximum accepted manoeuvre displacements (which can exceed 500 m
  in the permitted forward direction), `twa-step = 1`, `integer-controls` (command values, not physical TWA under
  heading mode), `heading-enabled`, accepted `heading-segments`, window counts and the failed window/endpoint error when relevant.

`best` is the re-simulated accepted track and its statistics are recalculated.
Existing route drawing and exports show the selected steering controls. Tooltips distinguish the command from measured TWA and ground course. Schedule CSV emits `HDG` rows for compass headings and `TWA` rows with the existing game sign conversion. GPX and route CSV include explicit control mode/value. Markers follow command changes rather than wind-induced TWA changes during a heading segment. The router statistics show whether fine-tuning improved the route, retained the original, was disabled, or was not loaded. Hover over that result for its reason and evaluation count. An older response without the integer-solver marker explicitly requests a server reload.
For duration-limited improvements the result label shows `Progress +… m`, and its
tooltip identifies the fixed-time objective. Search isochrones and alternative search tracks remain the original search
results; they are not regenerated by local post-processing.

Fine-tuned trackpoints carry `wind-cycle`, `wind-model` and `wind-time` from
actual integration samples. Map marker copies retain those fields and use them
for wind inspection. Older/unoptimized points continue to infer provenance from
the latest preceding isochrone. Model inspection does not alter future routing
settings or pin a new request to the inspected forecast cycle.

## Validation

Run from the Bitsailor directory:

```sh
sbcl --noinform --non-interactive \
  --load /home/michael/quicklisp/setup.lisp \
  --eval '(ql:quickload "bitsailor")' \
  --load tests/gate-search-tests.cl --load tests/fine-tune-tests.cl \
  --load tests/fine-heading-tests.cl --load tests/fine-progress-tests.cl \
  --load tests/fine-balance-tests.cl \
  --eval '(progn (router::test-fine-tuning) (router::test-fine-tune-physics) (router::test-fine-heading) (router::test-fine-progress) (router::test-fine-balance))'
```

Physical fixtures use real interpolation and constant polars, with constant and
rotating wind. They cover 105/110-degree oscillation, tack/gybe preservation,
actual manoeuvre movement in rotating wind, endpoint/time closure, budget and
land rollback, targets, and model-switch boundaries. Browser provenance tests
are in `tests/wind-sync-tests.cjs`.

The supplied Atlantic request, departing November 4 at 16:25 UTC from
`(44.45216, -39.99526)` toward `(16.2146564898, -61.5692507719)`, with NOAA-GFS
switching to ECMWF-AIFS after 24 hours and waves disabled, was re-run offline
against the same local datasets. The revised implementation reduced 91 control
changes to 84, returned only integer controls (including −139 degrees), and
retained the November 8 08:30 UTC arrival. It evaluated 2,287 trials over 91
windows and accepted one improved window. This is a validation case, not a
promise that every route can be smoothed at unchanged ETA.

A heading-specific physical fixture alternates 110/105-degree TWA in 0/5-degree
oscillating wind, with a cross-current. It fits one HDG 110-degree command,
removing all 15 steering changes while closing the endpoint at unchanged ETA.
It verifies that ground course differs from the compass command and that
fractional physical TWA is not exported as a fractional command. Export tests
are in `tests/steering-export-tests.cjs`.

The supplied Atlantic route was also compared with heading fits off/on against
identical raw search output. Both retained 84 steering changes and the November
8 08:30 UTC arrival; no heading candidate was accepted in that case. Trial
counts were 2,287 versus 2,736. This heuristic is optional and does not improve
every route.

Progress regressions cover fixed-time radial improvement, date-line crossing,
opposite-ray rejection, lateral limits, requested waypoints and manoeuvre bounds.
The constant-wind 105/110-degree fixture gains about 9.1 m at the identical
32-minute horizon, reducing 15 command changes to zero.

The same Atlantic setup truncated to two days was also replayed against local
NOAA-GFS/ECMWF-AIFS data. The bounded outward pass found no publishable
whole-route gain before its 3,000-trial budget, so the original endpoint and
controls were retained. Local radial improvements do not guarantee a feasible
improvement through all later corridors; this remains a bounded heuristic.

The saved November 4 19:08 route contains the reported 20:10–20:20 UTC
145/150-degree oscillation. With local forecast/polar replay at one-minute
resolution, holding signed TWA −149 for those ten minutes leaves about 6.7 m
radial gain and 3.7 m lateral residual. The missing A–B–A correction can reduce
the surrounding 20:08–21:08 window from five command changes to two while
closing its endpoint at the original timestamp. The full saved JSON replay
still fails a later 0.5 m closure check (about 0.62 m at window three); JSON
positions are rounded, so a window-level fit is not evidence of an accepted
whole-route improvement. No tolerance was relaxed to publish that result.

`tests/fine-balance-tests.cl` covers a physical rotating-wind fixture reducing
five changes to two with intermediate residuals, one-minute heading sampling
even with long baseline steps, and adverse wind at a heading endpoint or before
switching to a TWA tail.

A tolerance comparison on the saved JSON route found: 0.5 m rejects window three
at about 0.62 m; 1 m rejects window seven at about 1.16 m; 2 m and 5 m reach a
later, much larger displacement and still retain the original route. The new
1 m default therefore removes an overly strict early rejection without treating
large errors as acceptable. All routing/fine-tuning regression suites pass with
the 1 m default.
