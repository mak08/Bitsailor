# VirtualHelm

### Sailing route optimization

VirtualHelm uses the well-known Isochrones method for sailing route optimization. This is basically a brute-force algorithm that computes all positions reachable from a given position by moving with constant speed and direction for a fixed amount of time (eg. 10min). The resulting positions are filtered to prevent exponential growth and constitute the next isochrone.

#### Map
'Land check' is discrete, i.e. only the computed positions are checked but not the line connecting a position to the predecessor position. It therefore possible that the route skips over small stretches of land.

VirtualHelm uses libgdal for detecting interior points. The 'Land Polygons' file with split polygons from openstreetmapdata is used. 
A geospatial index is computed off-line on the set of polygons. At runtime the index is used to quickly find and intersect polygons with the current position. This is why the non-split polygons are much slower.

#### Wind
#### Polars
Boat polars are provided as CSV files or in JSON format containing separate array for each sail. Data is interpolated to 0.1 degrees and 0.1 m/s. The resulting arrays are compressed into a single array returning both the best sail and the corresponding speed. 

#### Filtering


### Installation

#### Prerequisites


*	Install libgdal with GEOS support via apt (package name may vary)
	
	```bash
	$ sudo apt-get install libgdal20
	```

	or following [these instructions](https://trac.osgeo.org/gdal/wiki/BuildingOnUnix). 
	In the latter case first install libgeos-3.4.2 (or newer) and dev files
	
	```bash
	$ sudo apt-get install libgeos-3.5.0
	$ sudo apt-get install libgeos-dev
	```
	Then do

	```bash
	$ ./configure --with-geos=yes
	```

	Check that GEOS support is available.

*	Download the Land polygons shapefile, WGS84 projection, split polygons from [openstreetmapdata](http://openstreetmapdata.com/data/land-polygons)

	```bash
	$ wget https://osmdata.openstreetmap.de/download/land-polygons-complete-4326.zip
	```

*	Install package mapserver-bin

	```bash
	$ sudo apt-get install mapserver-bin
	```

*	Unpack the shapefile archive. Generate a geospatial index.

	```bash 
	$ unzip land-polygons-split-4326.zip
	$ cd land-polygons-split-4326.zip
	$ shptree land_polygons.shp
	```

In addition, VirtualHelm requires 

*	[PolarCL](https://github.com/mak08/PolarCL)

*	[cl-map](https://github.com/mak08/cl-map)

*	[cl-grib2](https://github.com/mak08/cl-grib2)

*	[log2](https://github.com/mak08/log2)
	
and	the [quickloadable](https://www.quicklisp.org/beta/) systems

*	cl-utilities

*	cffi

*	local-time

*	babel
	

#### Links
[DWD opendata](https://opendata.dwd.de/) DWD publicly available weather data (new)

[DWD GDS](http://www.dwd.de/DE/leistungen/gds/gds.html) DWD basic public services (old)

[GEOS](https://trac.osgeo.org/geos/) Geometry Engine

[GDAL](http://www.gdal.org/) Geospatial Data Abstraction Layer

[GDAL Wiki](https://trac.osgeo.org/gdal/) GDAL bug tracker and Wiki

