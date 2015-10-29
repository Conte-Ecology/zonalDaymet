zonalDaymet
====================

`zonalDaymet` provides tools to establish a relationship between 
[Daymet](http://daymet.ornl.gov/) daily climate records and user-provided 
spatial polygons within the R environment. The package gives the user
flexibility to index, concatenate, and average climate records in specified 
locations.

## Daymet Data Description

Daily surface weather and climatalogical summaries are provided in
netCDF format. Each netCDF mosaic file contains the record for a single variable 
over 1 year (1980 - present) across a 1 km<sup>2</sup> resolution grid over 
North America. Each point location in the grid, specified by latitude/longitude 
coordinates, has a climatological time series associated with it for the 
specified variable in the file. For example, the `prcp_1980.nc4` mosaic file 
contains the daily gridded precipitation record across North America for 1980. In 
the case of a leap year, the December 31st record is omitted.

## Package Usage

The package provides tools to accomplish the following:

 - Download netCDF climate mosaics from the 
 [Daymet THREDDS Server](http://daymet.ornl.gov/dataaccess.html#THREDDS)
 - Iterate through netCDF files combining time series records and variables 
into tables
 - Spatially average climate records within user-specified spatial polygons
 - Divide spatial objects into manageable pieces within the R environment 
to avoid memory issues during spatial averaging
 - Return subsets from the climate mosaics given latitude/longitude 
coordinate bounds

Functionality is provided to return spatial averages as either a dataframe in 
the current R environment or write directly to a SQLite database. If a polygon is
too small to warrant a spatial average of points, the nearest point is assigned
to the polygon. A function is also provided to simplify record selection from the 
database. 

## Sample Workflow
The [Sample-Workflow Vignette](https://github.com/Conte-Ecology/zonalDaymet/blob/master/vignettes/Sample-Workflow.md) provides a walk-through of the package functions and how they
can be used together to process the Daymet netCDF mosaic files into a more
useful format in R.

## Contact Info

Kyle O'Neil  
koneil@usgs.gov 

