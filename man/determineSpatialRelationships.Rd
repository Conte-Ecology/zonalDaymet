% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/determineSpatialRelationships.R
\name{determineSpatialRelationships}
\alias{determineSpatialRelationships}
\title{Determine spatial relationships between climate records and zones}
\usage{
determineSpatialRelationships(zonesShapefile, zoneField, exampleMosaicFile)
}
\arguments{
\item{zonesShapefile}{SpatialPolygonsDataFrame A SpatialPolygonsDataFrame of the zones which will be
assigned climate records. The object should have a unique ID column and be in the WGS geographic
coordinate system (same as the Daymet NetCDF files, proj4string = "+proj=longlat +ellps=WGS84
+datum=WGS84 +no_defs +towgs84=0,0,0").}

\item{zoneField}{Character string of the field name describing the unique ID values that define the
zones.}

\item{exampleMosaicFile}{Character string of the file path to the netCDF mosaic file used to define
the spatial relationships between the shapefile and the climate record cells.}
}
\description{
\code{determineSpatialRelationships} is an internal function that defines the spatial relationships
between the netCDF files and the zones SpatialPolygonsDataFrame.
}

