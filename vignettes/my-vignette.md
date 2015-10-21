---
title: "Vignette Title"
author: "Vignette Author"
date: "2015-10-20"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{Vignette Title}
  %\VignetteEngine{knitr::rmarkdown}
  \usepackage[utf8]{inputenc}
---

Vignettes are long form documentation commonly included in packages. Because they are part of the distribution of the package, they need to be as compact as possible. The `html_vignette` output type provides a custom style sheet (and tweaks some options) to ensure that the resulting html is as small as possible. The `html_vignette` format:

- Never uses retina figures
- Has a smaller default figure size
- Uses a custom CSS stylesheet instead of the default Twitter Bootstrap style

## Vignette Info

Note the various macros within the `vignette` setion of the metadata block above. These are required in order to instruct R how to build the vignette. Note that you should change the `title` field and the `\VignetteIndexEntry` to match the title of your vignette.

## Styles

The `html_vignette` template includes a basic CSS theme. To override this theme you can specify your own CSS in the document metadata as follows:

    output: 
      rmarkdown::html_vignette:
        css: mystyles.css

## Figures

The figure sizes have been customised so that you can easily put two images side-by-side. 


```r
plot(1:10)
plot(10:1)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) ![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-2.png) 

You can enable figure captions by `fig_caption: yes` in YAML:

    output:
      rmarkdown::html_vignette:
        fig_caption: yes

Then you can use the chunk option `fig.cap = "Your figure caption."` in **knitr**.

## More Examples

You can write math expressions, e.g. $Y = X\beta + \epsilon$, footnotes^[A footnote here.], and tables, e.g. using `knitr::kable()`.


|                  |  mpg| cyl|  disp|  hp| drat|    wt|  qsec| vs| am| gear| carb|
|:-----------------|----:|---:|-----:|---:|----:|-----:|-----:|--:|--:|----:|----:|
|Mazda RX4         | 21.0|   6| 160.0| 110| 3.90| 2.620| 16.46|  0|  1|    4|    4|
|Mazda RX4 Wag     | 21.0|   6| 160.0| 110| 3.90| 2.875| 17.02|  0|  1|    4|    4|
|Datsun 710        | 22.8|   4| 108.0|  93| 3.85| 2.320| 18.61|  1|  1|    4|    1|
|Hornet 4 Drive    | 21.4|   6| 258.0| 110| 3.08| 3.215| 19.44|  1|  0|    3|    1|
|Hornet Sportabout | 18.7|   8| 360.0| 175| 3.15| 3.440| 17.02|  0|  0|    3|    2|
|Valiant           | 18.1|   6| 225.0| 105| 2.76| 3.460| 20.22|  1|  0|    3|    1|
|Duster 360        | 14.3|   8| 360.0| 245| 3.21| 3.570| 15.84|  0|  0|    3|    4|
|Merc 240D         | 24.4|   4| 146.7|  62| 3.69| 3.190| 20.00|  1|  0|    4|    2|
|Merc 230          | 22.8|   4| 140.8|  95| 3.92| 3.150| 22.90|  1|  0|    4|    2|
|Merc 280          | 19.2|   6| 167.6| 123| 3.92| 3.440| 18.30|  1|  0|    4|    4|

Also a quote using `>`:

> "He who gives up [code] safety for [code] speed deserves neither."
([via](https://twitter.com/hadleywickham/status/504368538874703872))



## Load Libraries
#```{r, results = "hide"}


```r
library(maptools) # For reading spatial objects
## Warning: package 'maptools' was built under R version 3.1.3
## Loading required package: sp
## Warning: package 'sp' was built under R version 3.1.3
## Checking rgeos availability: TRUE
library(devtools) # For package installation
## Warning: package 'devtools' was built under R version 3.1.3

install_github("Conte-Ecology/zonalDaymet")
## Downloading GitHub repo Conte-Ecology/zonalDaymet@master
## Installing zonalDaymet
## Skipping 1 packages not available: ncdf4
## "C:/PROGRA~1/R/R-31~1.2/bin/x64/R" --no-site-file --no-environ --no-save  \
##   --no-restore CMD INSTALL  \
##   "C:/Users/koneil/AppData/Local/Temp/1/RtmpMB6p5k/devtools1cb0786a3d11/Conte-Ecology-zonalDaymet-edce79f"  \
##   --library="C:/Users/koneil/Documents/R/win-library/3.1"  \
##   --install-tests
library(zonalDaymet)
## Loading required package: dplyr
## Warning: package 'dplyr' was built under R version 3.1.3
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```


## Enter Inputs

```r
# Temporal range
YEARS <- 1980:2014

# Variables
VARIABLES <- c("tmax", "tmin", "prcp", "dayl", "srad", "vp", "swe")

# Directory containing all of the raw Daymet data
DAYMET_DIRECTORY <- "C:/CLIMATE/daymet/raw"

# Name of the database with Daymet data paired to NHDPlus catchments
DATABASE_PATH <- "C:/CLIMATE/daymet/databases/"
TABLE_NAME <- "climateRecord"

ZONE_FIELD <- "FEATUREID"



# Define the projections of the shapefiles and Daymet data (Lambert Conformal Conic). This gets transformed to the coordinate system for processing.
proj4.Lambert <- "+proj=lcc +ellps=WGS84 +datum=WGS84 +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0"  # Projected Coordinate System
proj4.WGS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"                                    # Geographic Coordinate System


# Do not change:
allYears <- seq(from = START_YEAR, to = END_YEAR, by = 1)
```

```
## Error in seq(from = START_YEAR, to = END_YEAR, by = 1): object 'START_YEAR' not found
```


---
## Download the Daymet Mosaics

```r
downloadMosaic(years = YEARS,
                variables = VARIABLES,
                destinationFolder = file.path(DAYMET_DIRECTORY),
                retryFailedDownloads = TRUE)
```
