# Client-side current display

The current overlay fetches the existing static GRIB files from `/weather/`,
using the shared `ServerSettings.js` metadata and JSGrib decoder. No sampled
viewport data is requested from `router.getCurrentField`. Routing still samples
currents on the server, independently of the wind-model selection.

`WindLayer.js` and `CurrentLayer.js` share one `GribCache.js` instance.
It finds the forecast fields before and after the requested time
and interpolates between them. IBI and Agulhas store multiple times in one file;
IFS stores each forecast step separately. Metadata-only decoding indexes the
file; only the required U/V messages are unpacked. The latest fully downloaded
cycle advertised by the server is used, and the first/last frame is held outside
its time range. Normal server-settings refreshes also refresh the available
current cycles. Unavailable transfers report an error and can be retried.

Spatial interpolation uses the GRIB's actual regional origin, spacing and scan
direction, supports a periodic global longitude grid, and returns zero outside
regional coverage. A missing U or V component makes that entire node contribute
zero. Bilinear weights are not renormalized; measured zero vectors are retained.
Time interpolation follows spatial interpolation of the two fields.

This intentionally replaces the old endpoint's exact coastline mask with
zero-filled GRIB interpolation. It can draw weak arrows just inland across
coarse coastal cells; it also preserves nearshore currents that a conservative
“discard if any neighbour is missing” rule would hide. There is no additional
mask dataset, mask generation or server sampling work for display.

The cache holds four compressed files and eight decoded U/V pairs shared by wind
and currents. In-flight
requests are shared and limited to two simultaneous transfers. View cancellation
prevents stale drawing but lets a shared transfer finish for reuse. A small
number of queued requests can occur during rapid time scrubbing. Map pans and
redraws at cached times require no additional HTTP requests or GRIB decoding.

Validation:

```sh
node tests/grib-cache-tests.cjs
node tests/grib-modules-tests.mjs
node tests/grib-sampling-tests.cjs
node tests/current-display-tests.cjs
```

The decoder integration test uses the synthetic CCSDS/bitmap fixture from the
JSGrib repository. Optionally pass a local `cl-weather/weather` directory to
`grib-sampling-tests.cjs` to exercise real IBI, Agulhas and IFS files
from 18 November 2026, plus GFS, IFS and AROME wind, without network requests. JSGrib's separate ecCodes tests
verify every decoded value in those forecast files.
