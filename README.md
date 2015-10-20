zonalDaymet
====================

`zonalDaymet` provides tools to establish a relationship between 
[Daymet](http://daymet.ornl.gov/) climate records and user-provided 
spatial polygons within the R environment. Records are processed from
netCDF files into a more user-friendly format.

## Daymet Data Description

The Daymet daily surface weather and climatalogical summaries are provided in
netCDF format. Each netCDF mosaic file contains the record of a single year 
(1980 - present) for a single variable across the 1 km<sup>2</sup> resolution 
grid over North America. Each point location in the grid, specified by 
latitude/longitude coordinates, has a climatological time series associated with
it for the specified variable in the file. For example, the `prcp_1980.nc4` mosaic 
file contains the daily gridded precipitation record across North America for 1980.

## Package Usage

The package provides tools to accomplish the following:

 - Download netCDF climate mosaics from the [Daymet THREDDS Server](http://daymet.ornl.gov/dataaccess.html#THREDDS)
 - Iterate through netCDF files combining time series records and variables 
into tables
 - Spatially average climate records within user-specified spatial polygons
 - Process spatial objects into manageable pieces within the R environment 
to avoid memory issues during spatial averaging
 - Return subsets from the climate mosaics given latitude/longitude 
coordinate bounds

Functionality is provided to return spatial averages as either a dataframe in 
the current R environment or write directly to a SQLite database. If a polygon is
too small to warrant a spatial average of points, the nearest point is assigned
to the polygon. A function is also provided to simplify record selection from the 
database. 


## Software Requirements


## Sample Workflow


## Contact Info

Kyle O'Neil  
koneil@usgs.gov  
(413)863-3829  

