---
title: "Sample Workflow"
author: "Kyle O'Neil"
date: "2015-10-21"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Sample Workflow}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---

Vignettes are long form documentation commonly included in packages. Because they are part of the distribution of the package, they need to be as compact as possible. The `html_vignette` output type provides a custom style sheet (and tweaks some options) to ensure that the resulting html is as small as possible. The `html_vignette` format:

- Never uses retina figures
- Has a smaller default figure size
- Uses a custom CSS stylesheet instead of the default Twitter Bootstrap style

#collapse = TRUE}

## Load Libraries

```r
library(maptools) # For reading spatial objects
library(devtools) # For package installation

install_github("Conte-Ecology/zonalDaymet")
library(zonalDaymet)
```


## Enter Common Inputs

Inputs that will be called multiple times are defined as variables.

```r
# Temporal range
YEARS <- 2008:2009

# Variables
VARIABLES <- c("tmin", "prcp")

# Directory containing all of the raw Daymet data
DAYMET_DIRECTORY <- "C:/CLIMATE/daymet/raw"

# Name of the database with Daymet data paired to NHDPlus catchments
DATABASE_PATH <- "C:/CLIMATE/daymet/databases/exampleDatabase"
TABLE_NAME <- "climateRecord"

ZONE_FIELD <- "FEATUREID"
```


## Download Daymet Mosaic Files

The `downloadMosaic` function connects to the THREDDS server used by Daymet to
provide the mosaic netCDF files. Individual files are downloaded by specifying
the years and variables to download for, as well as the directory to download to.
There is also an option to retry failed downloads that become corrupted. This
is a possibility if the connection is lost during the download. A progress bar 
will appear during the download as well as messages describing the existence of
downloaded netCDF files and if any are corrupt.

```r
downloadMosaic(years = YEARS,
                variables = VARIABLES,
                destinationFolder = file.path(DAYMET_DIRECTORY),
                retryFailedDownloads = TRUE)
```


## Spatial Data Processing

This example uses hydrologic catchment polygons as the zones over which to 
spatially average the climate records.
The `maptools` and `sp` packages are used to read and process the spatial  
layer, which should already be projected into the same coordinate system 
as the [Daymet Projection](http://daymet.ornl.gov/datasupport.html). The 
polygon layer is transformed into the geographic coordinate system before
being split into 2 x 2 degree tiles to save memory in processing.

The spatial references first need to be 

```r
# Daymet spatial reference CRS definitions (Lambert Conformal Conic).
proj4.Lambert <- "+proj=lcc +ellps=WGS84 +datum=WGS84 +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0"  # Projected Coordinate System
proj4.WGS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"                                    # Geographic Coordinate System
```

The `maptools` package is used to read in the spatial polygons object.

```r
# Shapefile of zone polygons
zonesShapefile <- maptools::readShapePoly("C:/CLIMATE/daymet/spatial/Catchments01_Daymet.shp", 
                                            proj4string = CRS(proj4.Lambert))
```



```r
# Transform the shapefile into the coordinate system so the units are in lat/lon. 
#   This makes the shapefile comparable to the coordinates provided by Daymet NetCDFs in WGS.
transformShapefile <- sp::spTransform(zonesShapefile,
                                        CRS(proj4.WGS),
                                    class = "SpatialPolygonsDataFrame")
  
# Break up the spatial object to avoid memory errors during spatial averaging
tiledShapefile <- tileShapefile(shapefile = transformShapefile,
                                tileDegree = 2)

zonesPolygon <- tiledShapefile[[2]]
```

```r
plot(zonesPolygon)
```

```
## Error in plot(zonesPolygon): error in evaluating the argument 'x' in selecting a method for function 'plot': Error: object 'zonesPolygon' not found
```


## Climate Record Processing: Spatial Polygons

With the Daymet mosaics downloaded and the spatial objects loaded into R, the zonalDaymet
functions can be used to process the climate records. 



```r
assignZonalRecordsToDatabase(zonesShapefile = zonesPolygon,
                              zoneField = ZONE_FIELD,
                              zoneFieldType = "integer",
                              mosaicDirectory = DAYMET_DIRECTORY,
                              variables = VARIABLES, 
                              years = YEARS,
                              databaseFilePath = DATABASE_PATH, 
                              databaseTableName = TABLE_NAME)
```


```r
zones <- unique(zonesPolygon@data$FEATUREID)[1:2]

exampleZoneDB <- returnZonalRecordsFromDatabase(databaseFilePath = DATABASE_PATH, 
                                                databaseTableName = TABLE_NAME,
                                                zoneField = ZONE_FIELD,
                                                startDate = "2008-10-01",
                                                endDate = "2008-12-31",
                                                zoneIDs = zones,
                                                variables = VARIABLES)

str(exampleZoneDB)
```




```r
exampleZoneDF <- assignZonalRecordsToDataframe(zonesShapefile = zonesPolygon,
                                                zoneField = "FEATUREID",
                                                mosaicDirectory = DAYMET_DIRECTORY,
                                                variables = c("tmin", "prcp"), 
                                                years = 2008:2009)

str(exampleZoneDF)
```



## Climate Record Processing: Coordinates

```r
exampleCoordsLong <- returnRecordsByCoordinates(areaExtent = c(-71.0, -70.0, 42.0, 43.0),
                                              mosaicFile = file.path(DAYMET_DIRECTORY, "prcp_2010.nc4"),
                                              outputFormat = "long",
                                              startDate = "2010-01-01",
                                              endDate = "2010-01-31")
str(exampleCoordsLong)

exampleCoordsWide <- returnRecordsByCoordinates(areaExtent = c(-71.0, -70.0, 42.0, 43.0),
                                              mosaicFile = file.path(DAYMET_DIRECTORY, "prcp_2010.nc4"),
                                              outputFormat = "wide",
                                              startDate = "2010-01-01",
                                              endDate = "2010-01-31")

head(exampleCoordsWide)[1:10]
```



