# Direct access to official IBI currents

Checked on 15 November 2026. The working English Channel source still downloads
the Fairwinds GRIB. This investigation does not change its provider.

## Official product

[IBI_ANALYSISFORECAST_PHY_005_001](https://data.marine.copernicus.eu/product/IBI_ANALYSISFORECAST_PHY_005_001/services)
provides Atlantic Iberia–Biscay–Ireland forecast currents on a regular 1/36°
grid. The full service domain includes the English Channel. Suitable surface
datasets are:

- `cmems_mod_ibi_phy_anfc_0.027deg-2D_PT1H-m`: hourly means.
- `cmems_mod_ibi_phy_anfc_0.027deg-2D_PT15M-i`: 15-minute instantaneous values.

Select eastward and northward sea-water velocity at the surface. Preserve tides;
the separately published daily/monthly **de-tided** products are unsuitable for
Channel tidal routing. Exact variable names and forecast coverage should be read
from current dataset metadata rather than inferred from the Fairwinds file.
See the [product manual](https://documentation.marine.copernicus.eu/PUM/CMEMS-IBI-PUM-005-001.pdf).

## Access and formats

The [official download services](https://help.marine.copernicus.eu/en/articles/16733110-how-to-access-and-download-copernicus-marine-data)
provide original NetCDF files and subset access in NetCDF/Zarr, rather than a
daily GRIB bundle. The supported programmatic route is the Copernicus Marine
Toolbox (`get`, `subset`, or Python access). Its
[installation documentation](https://toolbox-docs.marine.copernicus.eu/en/stable/installation.html)
requires a free Copernicus Marine account;
[credentials configuration](https://help.marine.copernicus.eu/en/articles/8185007-copernicus-marine-toolbox-credentials-configuration)
supports external credential files or environment variables. Credentials should
not be stored in the repository.

No official, ready-made GRIB mirror covering the English Channel was verified.
The [EUSKOOS ERDDAP IBI subset](https://www.euskoos.eus/erddap/griddap/IBI_ANALYSISFORECAST_PHY_005_001.html)
is an institutional redistribution, but its published latitude range is about
43.22–46.50°N, which excludes the Channel. It is not a replacement for this source.

## Integration needed

A provider replacement is feasible, but needs more than changing `data-uri`:

1. Add an authenticated, unattended Toolbox download job for the two current
   components, selecting the desired region and surface level. The present
   Fairwinds crop is approximately 47–56°N, 12°W–3°E.
2. Either convert the subset to the existing paired U/V GRIB format, or add a
   NetCDF reader that constructs the existing `cl-weather` UV/interpolation fields.
   Conversion would preserve the current Lisp and browser interfaces.
3. Preserve actual timestamps, grid origin, scan direction, units, masks and
   forecast-reference metadata. Do not round hourly mean timestamps to an assumed
   instantaneous forecast time. Preserve the native grid rather than regridding
   to IFS spacing.
4. Validate coastal and tidal samples, update schedules and cycle fallback, then
   publish downloads atomically after completeness checks. Only switch providers
   after comparison with a known-good Channel route and displayed current field.

The current environment has no `copernicusmarine` executable. No authenticated
download or conversion was attempted, so a direct-source ingestion pipeline has
not been validated.
