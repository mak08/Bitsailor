# Channel LAT depth overlay

The router map has an optional **Depth clearance · LAT** control at the bottom
left. Draft defaults to 2 m, clearance to 0.5 m; both accept 0–30 m. The overlay
is initially off and independent of wind, currents and forecast time. Red means
the recorded cell minimum is below draft plus clearance. Grey hatching means
missing or extrapolated data, including uncertainty propagated at coarse zoom.
Unshaded cells **inside the coverage boundary** meet the selected threshold.
Pointer inspection reports the native cell minimum, even when the displayed
pixel combines several cells. Routing does not yet consult this dataset.

## Data and limitations

Source: [EMODnet Bathymetry Consortium (2024), DTM 2024](https://doi.org/10.12770/cf51df64-56f9-4a99-b1aa-36b8d7b743a1),
tile E4, CC BY 4.0. [Product description](https://emodnet.ec.europa.eu/en/bathymetry).
Coverage: 48–52°N, 6.375°W–3.5°E. The build also includes an unknown strip
between 6.5°W and the E4 boundary. The native grid is 1/16 arc-minute
(~116 m north–south, ~75 m east–west at 50°N). No tide correction is applied.

The NetCDF uses **negative elevations for depths**. The shallowest depth is
therefore **`-elevation_max`**, not `-elevation_min` and not mean elevation.
Positive elevations are retained as zero-or-less depth. NaNs remain unknown;
nonzero/missing `interpolation_flag` marks uncertain values. Native source
quality varies; the grid does not resolve every rock, harbour or narrow channel.
The extrapolation flag is not a complete survey-quality assessment.

The downloaded file explicitly states: **“The data in the EMODnet Grid should
not be used for navigation or any purpose relating to safety at sea.”** This
restriction is displayed in the controls and Usage section. This is a regional
planning display, not a navigational clearance guarantee.

## Build and deploy

Run from the Bitsailor repository on **Fedora 44 or later**. Required packages:
`make`, `curl`, `unzip`, `python3`, `python3-numpy`, and `netcdf` (system
libnetcdf). The Makefile checks prerequisites and **does not install software**.
The browser needs `DecompressionStream` (current Firefox and Chromium support it).

```sh
make check
make                   # Same as make bathymetry for now
```

`make bathymetry-download` downloads and extracts the source without building
tiles. Downloads and the extracted NetCDF are cached under
`local/data-build/emodnet-2024/`, outside the served web directory. Downloads are
ZIP-validated and both archive and NetCDF are published by rename after success.
Subsequent builds reuse them; there is no network request when the source is
already cached. Tiles rebuild when the source, generator or Makefile changes,
or any individual tile/manifest is missing. GNU make grouped targets ensure
one generation pass even with `make -j`.

`make help` lists targets. Override `DATA_BUILD_DIR` for the source cache,
`BATHYMETRY_DIR` for output, or `PYTHON`, `CURL`, `UNZIP` for command paths.
For example: `make bathymetry DATA_BUILD_DIR=/mnt/data/bitsailor-build`.
The default `all` target can be extended with tidal-data preparation later.

The download is ~285 MB. The generated `web/bathymetry/channel-2024/` directory
is ~22 MB: 160 half-degree chunks plus a provenance manifest with source SHA-256.
It is deliberately ignored by Git. **Copy this directory with the web assets
when deploying**, or run the build there. The existing static web handler serves
it at `/bathymetry/channel-2024/`; no Lisp configuration or restart is required.
For updates, use `BATHYMETRY_DIR` to build a fresh directory, then publish it atomically
and change the versioned data URL in `BathymetryLayer.js` if replacing a dataset.
No upstream requests are made during map interaction. Missing local files show
hatched tiles and an unavailable message. Source archives are not served.

## Tile contract and rendering

Each gzip-compressed `.bin.gz` chunk contains 480×480 little-endian uint16 cells,
north-first row order. Filenames are `x_y.bin.gz`, with southwest corner
`(longitude=x/2, latitude=y/2)`. Low 15 bits contain depth in centimetres,
rounded **down**, capped at 32766 (327.66 m). High bit indicates uncertain data;
`65535` means unknown. The cap exceeds the UI's maximum combined requirement of
60 m and pointer inspection labels capped depths with ≥. Zero is labelled ≤0 m.

Leaflet grid tiles use the map projection, below wind/current and route layers.
At low zoom each pixel conservatively combines all intersecting native cells,
retaining minimum depth and any uncertainty. An on-demand minimum pyramid bounds
the work; no averaging or bilinear interpolation can hide a shoal. Coarse pixels
can consequently overstate shallow/uncertain areas: zoom in for detail. Display
starts at zoom 7, decoded chunks use a bounded 40-entry cache, and changing draft
recolours existing data without downloading the source again. Failed data remains
hatched. A dashed geographic coverage rectangle distinguishes unassessed water.

Tests:

```sh
python3 tests/bathymetry-tests.py
node tests/bathymetry-display-tests.cjs
node tests/router-startup-tests.cjs
```
