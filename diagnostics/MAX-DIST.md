# `max-dist` in `get-route-leg`

`max-dist` is a per-leg memory of the furthest radius admitted in each angular
bucket. It prevents later isochrones from expanding back into radial territory
already explored. It does not store distances to the destination, arrival times,
routepoints or a frontier for a particular time.

The relevant code is `get-route-leg` and `expand-routepoint` in
[simulation.cl](../simulation.cl), and `bucket`, `add-routepoint` and
`filter-isochrone` in [filter.cl](../filter.cl).

## Lifetime and access

`get-route-leg` allocates an array with `*max-iso-points*` entries, initially
`0d0`. Its `do*` binding has no step expression, so the same array survives every
simulation step of that leg. In contrast, `next-isochrone` is allocated afresh
each step.

The declarations make `max-dist`, `min-angle` and `next-isochrone` dynamically
bound special variables. `expand-routepoint` calls `add-routepoint`, which
accesses the active leg's bindings without explicit array arguments. Separate
calls to `get-route-leg`, including serial point-waypoint legs, get fresh arrays;
this is not a global cache shared between routing requests.

## What the index and value mean

Expansion computes a candidate's great-circle distance `r` and normalized
bearing `a` from the fixed leg start. Distances are in metres, bearings in
degrees. This bearing describes the endpoint's position relative to the start;
it is not the boat's heading or its TWA.

With `N = *max-iso-points*`:

```text
min-angle = 360 / N
history-index = truncate(a / min-angle)
max-dist[history-index] = greatest radius admitted in that bearing interval
```

At the default `N = 1500`, each interval is 0.24 degrees wide. Each interval
covers a range of bearings, rather than identifying a unique path or even a
unique exact bearing.

There are two related index systems:

| Array | Index | Lifetime |
| --- | --- | --- |
| `max-dist` | `truncate(a / min-angle)` | Entire leg |
| `next-isochrone` | `(truncate(a / delta-angle) - truncate(left / delta-angle)) mod N` | One step |

The frontier index rotates the full-circle buckets so the fan's left boundary
comes first in array order. The history index remains an absolute bearing
index, without that rotation. `max-points` starts at its configured cap and
stays there, so `delta-angle` and `min-angle` currently have the same value.
The two arrays therefore partition bearings at the same resolution, with
different slot numbering. The configured fan limits accepted bearings, but
does not change the array capacity to the fan's angular width.

For example, with 10-degree buckets and `left = 60°`, a candidate at bearing
70° uses history entry 7 and frontier slot 1. Reading the same numeric slot
from both arrays would be incorrect; the implementation computes both indices.

## Admission and update order

After the box and fan tests, ordinary single-leg `add-routepoint` does this:

```text
if the current-step bucket is empty or r > current-step winner's radius:
    if r > max-dist[history-index]:
        max-dist[history-index] = r
        current-step bucket = newly created routepoint
```

Both comparisons are strict: equal radius does not replace an existing winner
and does not beat a historical mark. Candidates rejected by these comparisons
do not allocate a new routepoint in this branch. Admission uses direct array
lookups and comparisons, with no sorting or scan of other buckets.

The first comparison selects the outward winner for this step. The second
adds memory across steps. With today's matching bucket widths, an existing
current-step winner has already raised the corresponding historical mark, so
the historical test also excludes candidates behind that winner. The separate
tests allow the frontier and history resolutions to be expressed independently.

For example, if a bucket previously admitted a point at 100 km, an empty bucket
in a later step still rejects candidates at 95 km and 100 km. A candidate at
101 km can enter and raises the mark to 101 km. There is no stored timestamp or
expiry: the older mark remains effective until exceeded or the leg ends.

## Land filtering does not undo history

Only after all candidates have competed does `filter-isochrone` check the
selected endpoints or segments for land. It returns a compact vector of
survivors. It neither reads nor updates `max-dist`.

Consequently, the array records **pre-filter admissions**, not the furthest
land-valid states. If a 100 km winner crosses land, the filter removes it but
leaves the 100 km mark. A later land-valid candidate at 99 km is still rejected.
A smaller alternative discarded during the same expansion is not recovered
when its winner fails the land check. An empty frontier can therefore coexist
with nonzero historical marks; the search then returns `no_route`.

This order was checked in an isolated SBCL process using the real admission
and filter functions, with a substituted land predicate. The checks confirmed
the absolute/rotated index distinction, persistence after a blocked winner,
rejection of smaller and equal radii in a fresh frontier, and acceptance of a
larger radius. No routing behavior was changed for this analysis.

## Purpose and limits of the pruning rule

The rule enforces an outward envelope over time in each bearing interval. It
suppresses repeated or inward-moving fronts, reducing work and helping prevent
frontiers from folding back behind earlier expansion near obstructions. Since
it remembers only scalar distances, storage is bounded by `N`; it does not
retain route ancestry itself.

This is an approximation to useful progress, not a proof that discarded states
cannot yield an earlier arrival. It assumes the outward state adequately
represents the alternatives in its angular interval. That can fail when:

- a useful detour requires temporarily reducing radius or revisiting an interval;
- land separates alternatives in the same interval;
- different positions within the interval encounter different future weather;
- a blocked pre-filter admission establishes an unreachable radial mark.

Nor does greater radius necessarily mean being closer to the destination: the
metric is distance from the leg start, with no downstream travel-time estimate.
The array can suppress inward artifacts without making the search globally
optimal.

## Relationship to gate routing

The shared-clock gate router binds `*routepoint-sink*`. In that branch,
`add-routepoint` forwards the candidate and returns **before** accessing
`max-dist` or the ordinary frontier array. That router uses its own
`*gate-progress*` hash table, keyed by stage and bearing bucket.

Both mechanisms reject radial regression, but their update order differs:
the gate router records progress from selected land-valid survivors, whereas
single-leg `max-dist` updates before land filtering. Gate routing also pools
different crossing origins and times under a stage's midpoint reference. That
extra assumption is the cross-history pruning defect described in
[GATE-LENGTH.md](GATE-LENGTH.md). Ordinary point-waypoint legs restart from one
fixed origin and a fresh history array, so they do not pool those gate crossing
histories. They still retain the general radial-pruning limitations above.
