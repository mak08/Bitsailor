# TWA route fine-tuning

`fine-tune-route` in [fine-tune.cl](fine-tune.cl) runs after the complete
isochrone/waypoint search, through `get-route`. It re-sails short sections with
continuous signed TWA controls to remove oscillation caused by the router's
coarse TWA samples. Low-level `get-route-leg` and `get-route-through-gates` remain
search primitives; they do not recursively fine-tune their results.

A control is **TWA plus duration**, not a heading or a straight geographic line.
At each integration sample the heading is recomputed as local wind direction
plus TWA. The selected polar sail/speed, waves and currents are applied as in
ordinary expansion. Thus one constant-TWA segment curves when wind rotates.

## Objective and hard constraints

The final arrival time may never increase. Among feasible candidates with the
same arrival time, prefer fewer TWA changes, then smaller total TWA variation.
A small secondary cost discourages unnecessary movement of manoeuvre times.
The implementation scores an equal-time window as:

```text
1000 × control-change count
+ total absolute TWA variation in degrees
+ 0.001 × sum of absolute manoeuvre time changes in seconds
```

A control change smaller than 0.05 degrees is ignored in the score/count; the
exported control plan itself retains the actual fitted angles. The whole-route
check additionally rejects an increase in the control-change count.

Every published candidate must satisfy:

- The same tack/gybe sequence and count, in order. A sign flip is classified as a
  tack when the mean absolute TWA on its two sides is below 90 degrees, otherwise
  as a gybe. The original route has no explicit manoeuvre labels, so this is the
  classification used consistently for both routes.
- Each manoeuvre within 500 m and 300 seconds of the corresponding original one.
- Every sampled position within a 500 m time-aligned corridor around the original
  route, with wrapped-longitude interpolation used only to measure that corridor.
- Numerical endpoint closure within 0.5 m. There is no heading-based correction,
  position snap, waiting segment or appended straight-line final approach.
- Valid polar TWA/VMG angles and positive boat speed throughout replay.
- Land checks over every simulated segment, using the configured vector/bitmap
  land predicate. Bitmap mode retains its existing endpoint-only limitations.
- All requested point waypoints and gates in order. Gate crossings within one
  segment must follow their spatial order; touches within the closure tolerance
  count. Point waypoints are window boundaries and must still be reached within
  that same numerical tolerance.
- No later final timestamp than the original route. Its final geographic endpoint
  is preserved within the closure tolerance, including when the original search
  stopped slightly beyond the destination under its usual arrival criterion.

This is a local refinement of the selected route, not a new global route search.
It will not remove a tack or gybe, search another side of an island, or accept a
later ETA merely to make steering simpler.

## Constrained shooting

The reference route is divided into windows of approximately one hour, splitting
at point waypoints, before a second manoeuvre, and after substantial local TWA
range changes. A window can therefore contain zero or one manoeuvre. Intermediate
window endpoint times remain fixed. The final window may end earlier.

Each trial contains at most two constant-TWA segments:

1. On a single side of the wind, replace repeated angles by one or two fitted
   angles. Two-segment trials use split fractions 1/4, 1/2 and 3/4.
2. Around a tack/gybe, use one segment on each original side. Try the original
   switch time and shifts of ±60, ±180 and ±300 seconds, subject to the hard
   position/time checks.
3. With a fixed split and total duration, solve the two endpoint coordinates for
   the two TWA angles using finite-difference, damped Newton shooting. Bounds
   preserve the original side and permit at most five degrees beyond that side's
   original local angle range.
4. In the final same-side window, also solve for one TWA and its duration. Final
   two-segment trials additionally try duration factors 0.999, 0.995, 0.99 and
   0.98, as well as the original duration. A successful earlier solution is
   preferred before applying the steering score.

The switch-time choices provide a bounded way to explore better tack/gybe
positions in rotating wind: alter the switch, solve for endpoint closure, and
retain only a physically valid improvement close to the original manoeuvre.
The search is discrete in those switch-time trials; it does not claim to find
the exact best switch time. Earlier intermediate-window arrival is deliberately
not optimized, which keeps subsequent forecast times and anchors stable.

The Newton solver uses at most eight iterations with damped steps and has no
external numerical-library dependency. Trial integration uses at most 120-second
steps and splits exactly at control boundaries and the absolute forecast-model
switch. Weather parameters are cached by exact sample timestamp within this
post-processing request; interpolation is still performed at each trial position.

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
the original route. Partial (`no_route`/`max_duration`) routes are not optimized.

Trial work is bounded by 3,000 integrations and 10 wall-clock seconds, checked
between integrations. These are trial budgets, not a hard interruption of an
individual GRIB load or the baseline replay/validation. After budget exhaustion,
remaining windows can retain their original controls. A report identifies a
budget-limited result. The algorithm does not retain a tree of trial routes.

All operational parameters are listed on the Status tab. Set
`router::*fine-tune-enabled*` to NIL to disable the pass. Other settings are
`*fine-tune-step*`, `*fine-tune-window*`, `*fine-tune-angle-limit*`,
`*fine-tune-position-limit*`, `*fine-tune-time-limit*`,
`*fine-tune-endpoint-tolerance*`, `*fine-tune-max-evaluations*` and
`*fine-tune-max-seconds*`.

## API and display

The response adds:

- `twa-segments`: compact controls with `start-time`, `end-time` and signed `twa`.
  Consecutive equal angles are coalesced; segment duration is the timestamp
  difference. This is the executable optimized plan.
- `original-best`: the original track when an optimized track was accepted.
- `fine-tuning`: outcome/reason, evaluation count, original/optimized change
  counts and arrival times, plus maximum accepted manoeuvre displacements.

`best` is the re-simulated accepted track and its statistics are recalculated.
Existing route drawing and exports therefore show the smoother TWA controls.
Search isochrones and alternative search tracks remain the original search
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
  --eval '(progn (router::test-fine-tuning) (router::test-fine-tune-physics))'
```

Physical fixtures use real interpolation and constant polars, with constant and
rotating wind. They cover 105/110-degree oscillation, tack/gybe preservation,
actual manoeuvre movement in rotating wind, endpoint/time closure, budget and
land rollback, targets, and model-switch boundaries. Browser provenance tests
are in `tests/wind-sync-tests.cjs`.

A representative GFS run from `(38.46219, 0.91566)` toward `(40.00237, 4.88067)`,
departing November 3 at 12:00 UTC with the November 3 12Z cycle, IMOCA 2024
polars and waves disabled, reduced 61 control changes to 47 and arrived about
two seconds earlier. This is one validation case, not a general speedup claim.
