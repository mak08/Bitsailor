# Multi-leg routing in Bitsailor

This document describes the implementation in the working tree, including the gate-retirement threshold and the removal of general-purpose sorting from the active multi-leg search. It distinguishes the search's arrival objective from the heuristics used to limit its size. The algorithm is an approximate isochrone search; it does not guarantee the globally earliest arrival.

A [six-figure sequence](figures/gate-frontier/README.md) records the earlier midpoint-origin implementation. Those figures have not been regenerated for the artificial-origin experiment described below. Gate creation is available again through the Web UI's 'Add gate' context-menu entry and through the API.

## 1. Entry points and target grouping

The HTTP handler `|getRoute|` in [http-api.cl](http-api.cl) parses `waypoints` as a `~`-separated sequence. Two coordinates specify a point; four specify the endpoints of a gate. The destination remains a separate point. `get-route` in [simulation.cl](simulation.cl) dispatches requests with waypoints to `get-route-via-waypoints` in [waypoints.cl](waypoints.cl). Requests without waypoints use `get-route-leg`.

`group-gate-targets` appends the destination and groups consecutive gates with the point immediately following them. For example:

```text
Input:   gate A, gate B, point P, gate C, destination D
Groups:  [gate A, gate B, point P], [gate C, destination D]
```

Each group is a sequential leg of the outer algorithm. A group containing gates uses `get-route-through-gates` in [gate-search.cl](gate-search.cl). A group containing only a point uses the ordinary single-leg engine.

The outer algorithm carries the selected arrival position and time into the next group and subtracts elapsed sailing time from the request's duration budget. It appends each group's track and display isochrones, removing the duplicated track junction. It stops on failure or duration exhaustion. Search effort is summed across groups.

Point waypoints therefore commit the router to one arrival before searching the following group. Alternative arrival times at that point are not retained for downstream optimization. With time-dependent weather, this sequential choice is another reason the entire route is not guaranteed globally optimal.

## 2. Gates are search stages within a leg

A group with N gates has N+1 stages:

| Stage | Meaning | Current target |
| --- | --- | --- |
| 0 | No gates crossed | Gate 0 |
| k, with 0 < k < N | First k gates crossed in order | Gate k |
| N | All gates crossed | The group's final point |

Crossing a gate changes a candidate's stage. It does not finish the search, restart its clock, or select one crossing for all subsequent routing. Several stages can be active in the same iteration.

A `gate-state` contains:

| Field | Purpose |
| --- | --- |
| `point` | Current routepoint at the shared simulation time |
| `stage` | Number of gates already crossed |
| `origin` | Initial routepoint or the latest actual gate-crossing routepoint |
| `source` | Original previous-step point, bounding the chain of sailed segments to check for land |
| `crossings` | Gate indices and crossing positions introduced on this step |
| `radius` | Cached distance from the fixed stage centre, assigned during candidate retention |

The routepoint contains position, time, sailing/weather data and a predecessor link. The algorithm retains a bounded frontier of such states, not an explicit tree containing every generated candidate.

### Two different origins

The fixed stage centre and the actual crossing origin have different purposes:

- Stage 0's fixed centre is the leg start. Stage k's fixed centre is an artificial point behind gate k−1, away from the next target. It determines angular buckets, radial progress and display ordering.
- A state's actual origin is the position where its own path last crossed a gate. The search fan is measured from this origin toward the next target. The final arrival test also uses it, and it supplies the boundary for the intermediate-point approach lookback.

Using one fixed centre lets crossings from different positions compete in the same geographic frame. The fan and search box follow each actual crossing; the boat is never moved to the artificial point.

For midpoint M, gate width W and next target position T, place the artificial origin O on the great-circle ray from M away from T:

```text
L = max(1000 metres, max(2, *gate-origin-distance-factor*) × W)
O = advance(M, L, bearing(M, T) + 180 degrees)
```

The default factor is two. The 1 km floor avoids unstable geometry for very short gates. A coincident M and T uses the defined zero-bearing fallback; there is no meaningful target direction in that case. The multiplier is exposed on the Status tab. Typical gate dimensions must be regional: extending a great-circle ray over planetary distances no longer represents a useful point behind a gate.

This places the entire gate ahead of the origin in the next-leg direction for local geometry. Useful departures are less likely to decrease radius than with a midpoint origin. It does not establish global dominance or invariance when the gate's unused endpoints change.

## 3. One clock for the whole gate group

All frontier endpoints share the same timestamp. `make-stepper` chooses the time increment: the first step ends on a ten-minute boundary, subsequent steps initially use two minutes and later grow to ten, fifteen and thirty minutes. The exact transition times depend on the start's hour alignment and requested horizon. The gate loop caps the last increment at the remaining duration.

Wind, optional currents, and optional wave parameters are obtained for the start of the step. Each frontier point samples the fields at its own position. `expand-routepoint` tries both tacks at the polar's sampled TWA values plus the current upwind and downwind VMG angles. Only positive TWA magnitudes within the VMG interval are expanded.

For each heading it computes polar speed, wave adjustment and optional current displacement, then an endpoint for the step. Candidates must lie within the stage's latitude/longitude box and angular fan. The fan is centred on the bearing from the state's actual last crossing origin (or the leg start in stage 0) to the target position; a gate's target position is its midpoint. Its endpoint bearing is measured from that same actual origin. The search box is based on the actual crossing origin and target, so the artificial origin does not enlarge or shift the sailed search region. These are geographic restrictions on endpoints, not a ranking by estimated arrival time.

When the gate search binds `*routepoint-sink*`, `add-routepoint` sends candidate routepoints to that consumer instead of the ordinary single-leg bucket array. A routepoint is currently allocated before gate-stage pruning, including for candidates that are immediately discarded.

## 4. Crossing a gate

`advance-gate-state` tests the sailed segment against the next required gate using `gate-crossing`. The intersection calculation uses a local flat projection with wrapped longitude differences. It checks both finite segments and does not enforce a prescribed crossing direction. Near-parallel segments are rejected by the determinant tolerance; collinear overlap does not count as a crossing.

On a crossing it:

1. Interpolates a position on the segment and a fractional timestamp between its endpoints.
2. Inserts that crossing routepoint into the predecessor chain.
3. Increments the stage and records the crossing position for coverage accounting.
4. If the new target is more than 90 degrees behind the incoming course, turns at the crossing and computes a new endpoint for the remaining fractional duration.
5. Tests the resulting remainder against the next gate, allowing multiple ordered touches and a turn within one step.

The endpoint remains at the common step-end time. For a turn, `gate-turn-endpoint` selects the direct polar angle when permitted, otherwise the better of the two VMG-boundary tacks, by velocity projected toward the next target. It interpolates the step's existing weather fields at the crossing position and applies the ordinary current and wave adjustments. It does not compute a two-tack manoeuvre or resample weather at a different forecast time. A target ahead of the incoming course keeps the original remainder; normal heading expansion resumes on the following iteration. The resulting endpoint must pass the new actual-crossing fan test.

A gate only needs to be touched: the returning segment may remain on its original side. Fractional touch times stay in the predecessor chain, but all frontier endpoints retain the common end time. Final destination arrival is still determined at that end time, without fractional refinement.

## 5. Pruning and bucket winners

Three quantities must not be confused:

| Quantity | Location/frame | Purpose |
| --- | --- | --- |
| Angular bucket | Bearing around a fixed stage centre | Limit the current frontier |
| Historical radial maximum | Same stage and angular bucket | Prevent regressions in later steps |
| Gate coverage interval | Fraction along a gate segment | Decide when to retire its preceding stage |

### Angular resolution and frontier size

`gate-angular-width` computes the bucket width in degrees:

```text
width = max(*gate-bucket-width*,
            360 / max(1, *max-iso-points*))
delta = bearing(common-origin, endpoint) - bearing(common-origin, gate-midpoint)
scale = max(1, 2 × L / max(W, 1000 metres))
angle = wrap360(degrees(atan2(scale × sin(delta), cos(delta))))
key   = (stage, floor(angle / width))
```

Stage zero uses ordinary absolute bearings. Later stages apply the monotone full-circle transform above, with delta evaluated in radians. It magnifies resolution near the forward axis and coarsens it toward the sides. This limits the loss of physical resolution caused by moving the origin back, without adding buckets or sorting. All paths in a stage use the same transform.

With the defaults, the width is 0.24 transformed degrees, giving 1500 possible buckets per stage. Normally only a subset lies within the fan or has reachable candidates. The retained frontier allows one survivor per occupied bucket. This is a per-stage budget, not a single budget shared by all stages.

### Historical progress

`*gate-progress*` is a request-local hash table. A new candidate is rejected if its radius is less than or equal to the previously accepted maximum for its stage and angular bucket. The maximum is updated from land-valid survivors at the end of the step and persists when a bucket is empty in later steps.

This suppresses inward-moving frontiers and apparent reflections near land. It is a heuristic: being farther from the stage centre is not a mathematical proof of better downstream arrival, and a useful detour can require temporary inward movement.

### Within-step selection: replace or discard

For each key, `retain-gate-state` stores a single `gate-state`, rather than a list of candidates. After computing the key and radius, it makes two checks:

```text
if radius <= historical maximum:
    discard candidate
else if the bucket is empty or radius >= current winner's radius:
    replace the winner with the candidate
else:
    discard candidate
```

The new candidate wins an exact radius tie. Selection uses one bucket lookup, comparisons, and possibly a replacement. There is no candidate-pool scan, insertion, sort, or pairwise spacing test. The winner's radius is cached for subsequent comparisons. The former four-candidate pool and its 100-metre separation rule have been removed.

### Land filtering after winner selection

After expansion, `select-gate-survivors` checks each bucket's sole winner. It walks the predecessor chain back to `source`. In vector mode it checks every sailed segment, including the approach to each touch and the turned remainder. Bitmap mode checks each segment endpoint. A bent step cannot be checked using only the straight chord from source to endpoint. Each occupied non-arrival bucket is filtered once, but can require several segment checks.

A blocked winner leaves its bucket empty even if an earlier discarded candidate was valid. This matches the ordinary single-leg router's select-then-filter policy. It deliberately gives up the former pool's fallback candidates. Testing every prospective winner for land before replacement could preserve a valid incumbent, but would increase land checks; that is not the current implementation.

Only land-valid survivors update progress history and gate coverage. Final-arrival candidates receive an immediate land check and bypass ordinary bucket retention. Traversing the final bucket set to build the next frontier is still required, but there is no traversal of candidates within a bucket.

## 6. Gate coverage and retirement

After selecting land-valid survivors, the router updates radial history and records their new crossings in gate coverage vectors. Each gate is divided into twelve intervals by its projected along-gate fraction. Hitting an interval sets its bit permanently; repeated hits do not increase coverage. Endpoints are clamped into the first and last intervals.

Only crossings carried by retained states count. A geometrically valid crossing discarded during pruning does not count. Coverage represents sampled intervals, not continuous proof that every point between two crossings has been reached.

A gate stage retires when:

```text
number of marked intervals >= ceiling(interval count × retirement fraction)
```

The defaults are twelve intervals and `*gate-retirement-coverage* = 1/2` (50%), requiring six distinct intervals. Binding the fraction to 1 requires all twelve. The threshold imposes neither endpoint coverage nor a maximum size for a contiguous uncovered gap.

Retirement is applied at a step boundary, after all that step's surviving crossings have entered their next stages. States still waiting to cross a retired gate are removed; subsequent candidates in that stage are rejected. This closes that particular stage, not automatically all earlier stages. The final destination stage never retires. Previously saved isochrones remain in the response.

The relaxed threshold avoids indefinite expansion caused by sampling holes. It can also discard a useful later crossing in an uncovered interval. Retirement is therefore a pruning heuristic, not an optimality result.

## 7. Selecting an arrival or a partial result

In the final stage, `gate-state-reached-p` accepts an endpoint if it is within one metre of the point destination, or if both conditions hold relative to its actual last crossing origin:

- Its bearing is within 1.5 degrees of the bearing to the destination.
- Its distance from that origin is at least the origin-to-destination distance.

Consequently, a successful endpoint may be beyond or to one side of the exact destination. The final destination is not automatically refined to an exact point.

The search stops at the first shared step containing a land-valid arrival. Among that step's arrivals, it chooses the endpoint nearest the destination. Endpoints have the same timestamp; this choice does not compute fractional destination-arrival times or compare total future travel costs.

Without an arrival, the partial result prefers the highest stage, then the state nearest that stage's target. For a gate, the distance uses a clamped local projection onto the gate followed by geographic distance to the projected position. This selects the fallback track; it does not reorder or prune the frontier.

Both choices use `best-gate-state`, a single-pass reduction. Exact ties retain the first state encountered. Frontier order ultimately depends on hash-table traversal and is not a cross-platform deterministic tie-breaker. The former arrival `sort` did not promise stable tie ordering either.

Termination produces `reached`, `max_duration`, or `no_route`. If no next frontier remains, the last selected partial result is returned. The track is reconstructed through predecessor links, including fractional gate-crossing points.

## 8. Exact approaches to intermediate point waypoints

When another group follows, the outer router refines the current group's point arrival using `best-approach`. It tries the selected endpoint and up to twelve predecessors, stopping before the last gate-crossing time when one exists. Among successful attempts it chooses the earliest arrival.

`approach-target` tries a direct approach when the target bearing lies within the VMG angles. Otherwise it computes candidate VMG tack/gybe first legs using a local planar construction. It returns the first feasible tack order rather than comparing every tack order by arrival time.

`sail-approach` integrates the approach in steps of at most 300 seconds, samples wind again at each position/time, and checks land. It currently omits currents and wave effects. It stops on low speed, a blocked segment, or its 1000-iteration limit. A successful final step ends exactly at the intermediate point.

This refinement supplies the next group's start time. It is applied only to intermediate point waypoints; the last group returns the search engine's selected final endpoint. The remaining duration is checked before starting each group, rather than enforced separately inside approach integration.

The sampled-gate mode of `best-approach` and `*gate-samples*` are legacy helpers. The active shared-clock gate algorithm does not restart a route from those sampled gate points.

## 9. Display isochrones

At full-hour step endpoints, `gate-frontier-isochrones` constructs display paths separately for each stage. It keeps the outermost frontier point in each angular bucket, stores these in a bucket-indexed array, and traverses the array in increasing index order. No sort is required.

It splits paths when the gap in transformed bucket-bearing coordinates exceeds `max(1 degree, 3 × bucket width)` or the position gap exceeds four times the largest emitted displacement seen in the step (with a 100-metre minimum displacement scale). This avoids drawing long chords and radial connections across missing portions of a frontier.

Each fragment contains a centre, stage, forecast cycle, whole-hour offset and position path. Several fragments can represent one stage at the same time. Different stages can also have frontiers at the same time. Neither necessarily indicates duplicate search states. Retirement prevents future expansion of a stage; it does not erase its earlier fragments.

## 10. What sorting was used for, and its replacements

The active gate search previously used four general-purpose sorts:

| Location | Former purpose | Current replacement |
| --- | --- | --- |
| `retain-gate-state` | Order a new candidate plus up to four existing candidates by radius before spatial thinning | Direct comparison with the current winner; replace or discard |
| `gate-frontier-isochrones` | Put occupied hash-table bucket indices into angular order | Bucket-indexed array traversed in index order |
| Arrival selection in `get-route-through-gates` | Sort arrivals by destination distance and take the first | Single-pass minimum selection |
| Partial-result selection in `get-route-through-gates` | Sort a copy of the frontier by stage, then target distance, and take the first | Single-pass stage/distance selection |

The bucket sort was the frequently executed one. It was first replaced by insertion into an ordered pool. That pool has now been removed entirely: only the current winner is stored. Radius ties still favour the new candidate, but spatial thinning and blocked-winner fallback no longer apply.

Bucket winner selection now costs O(1) after computing its geographic key and radius, with expected constant-time hash-table access. Selecting a single result costs O(F) for frontier size F, with no O(F log F) frontier sort and no repeated target-distance comparisons during sorting. Display array traversal costs O(B) per stage for B angular buckets; it happens when producing hourly output, not when comparing candidates within a bucket.

`best-gate-crossing` in [filter.cl](filter.cl) still contains a sort for the legacy single-leg `routing-gate` path. The active multi-leg dispatch clears `routing-gate` and handles ordered gates through `advance-gate-state`, so that helper is not used by the algorithm described here.

## 11. Why gates do not yet imply faster routing

The intended objective is earliest feasible arrival through ordered gates. The implemented frontier criterion is outward radial progress per stage/bearing bucket. It has no downstream ETA estimate, global incumbent ETA bound, or test proving that a candidate cannot contribute to a better final route.

The single-leg router also avoids exponential growth: it retains an outward candidate per angular bucket. Adding a gate therefore does not reduce an otherwise unpruned exponential tree. It introduces another bounded stage frontier, and earlier stages remain active until retirement. With A active stages and B buckets, the survivor count can approach A × B. Expansion then multiplies that count by the valid TWA choices and two tacks.

Additional costs include routepoint/state allocation before pruning, repeated geographic calculations, crossing tests, coverage bookkeeping, per-state routing copies and boxes, hash-table keys, and land checks on bucket winners. Eliminating sorting removes unnecessary ordering work, but does not reduce the number of generated candidates or overlapping stages.

The earlier cached-forecast comparison for the reported one-gate request generated about 6.62 million candidates with the gate versus 6.05 million without it. That explains why a large search reduction cannot be assumed. Those counts describe that request and configuration; they are not a general performance bound.

## 12. Shared-clock loop in pseudocode

```text
initialize one state at the group start
initialize coverage and historical radial maxima

while duration remains:
    choose common step end and weather parameters
    create empty per-stage/bearing bucket winners and arrival list

    for each frontier state:
        expand both tacks within the actual-crossing fan and fixed-stage box
        for each emitted endpoint:
            insert ordered gate intersections into its track
            determine its resulting stage
            if newly crossed and outside the resulting actual-crossing fan: discard
            if it qualifies as a final arrival:
                land-check the complete step and collect if valid
            else if its stage is not retired:
                reject historical radial regression
                replace the bucket winner if the new radius is greater or equal

    if there are valid arrivals:
        return the nearest endpoint among this step's arrivals

    land-check each bucket winner; discard blocked winners
    update radial maxima and gate coverage from survivors
    remove survivors belonging to newly retired stages
    if no survivors remain: return the last partial result
    select a partial result by stage and target distance
    retain survivors as the next common-time frontier
    save per-stage display fragments at full hours

return the last partial result with duration-limit status
```

## 13. Verification

[tests/gate-search-tests.cl](tests/gate-search-tests.cl) covers shared-clock alternatives, real expansion, bucket width and blocked-winner filtering, radius ties and direct replacement, display fragmentation, historical progress and retirement thresholds. The removal of sorting should also be checked against a real route by comparing its full selected track and saved isochrones with the preceding implementation. Timing comparisons should use cached forecasts for both routes; loading GRIB data can dominate an initial request.

The previous sort-removal comparison produced identical full tracks and isochrone fragments. The subsequent removal of the candidate pool changes behaviour near land, so that equivalence claim does not apply to the single-winner policy. Validation must check that routes still cross gates in order and avoid land, in addition to comparing ETA and calculation time.

For that same request, a paired cached-forecast comparison of the insertion-pool and single-winner implementations measured 7.30 versus 5.24 seconds. Complete-step land checks (including final-arrival checks) fell from 155,792 to 153,298. The selected full track and 15h 43m duration were identical; saved isochrones differed, as expected from losing fallback survivors. All five regression functions passed with the single-winner policy. These timings are individual runs, not a general speed guarantee.
