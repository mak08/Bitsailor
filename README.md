# VirtualHelm
Automated Weather Routing using Isochrones - sail routing optimizer


#### Prerequisites

VirtualHelm uses libgdal for determining if a point is on shore or at sea.


*	Install libgeos-3.4.2 (to automatically enable GEOS support in libgdal)
	
	```bash
	$ sudo apt-get install libgeos-3.4.2
	```

*	Install libgdal following [these instructions](https://trac.osgeo.org/gdal/wiki/BuildingOnUnix). Check that GEOS support is available.

*	Download the Land polygons shapefile, WGS84 projection, split polygons from [openstreetmapdata](http://openstreetmapdata.com/data/land-polygons)

	```bash
	$ wget http://data.openstreetmapdata.com/land-polygons-split-4326.zip
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

*	[PolarCL](PolarCL)

*	[cl-grib2](cl-grib2)

*	[log2](log2)
	
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

