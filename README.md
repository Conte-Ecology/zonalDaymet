spatialAverageDaymet
====================

`spatialAverageDaymet` provides tools to establish a relationship between Daymet 
climate records and user-defined spatial polygons within the R environment.

## Description

Daymet climate records are provided in a gridded format (1 km^2 resolution) as 
netCDF files. Each point location in the grid, specified by latitude/longitude 
coordinates, has one time series associated with each climate variable. Each 
netCDF file contains 1 variable record over 1 year for the entire North American 
range. The spatialAverageDaymet package provides tools to accomplish the following:

 - Download netCDF climate mosaics from the Daymet THREDDS Server
 - Iterate through netCDF files combining time series records and variables 
into tables
 - Spatially average climate records within user-specified spatial polygons
 - Process spatial objects into manageable pieces within the R environment 
to avoid memory issues during spatial averaging
 - Return subsets from the climate mosaics given latitude/longitude 
coordinate bounds

Functionality is provided to return spatial averages as either a dataframe in 
the current R environment or write directly to a SQLite database. A function 
is provided to simplify record selection from the database. If a polygon is
too small to warrant a spatial average of points, the nearest point is assigned
to the polygon.


## Software Requirements


## Sample Workflow


## Contact Info

Kyle O'Neil  
koneil@usgs.gov  
(413)863-3829  

