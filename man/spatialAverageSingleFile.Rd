% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/spatialAverageSingleFile.R
\name{spatialAverageSingleFile}
\alias{spatialAverageSingleFile}
\title{Spatially average multiple climate records by zone}
\usage{
spatialAverageSingleFile(mosaicFilePath, spatialIndeces, zoneField)
}
\arguments{
\item{mosaicFilePath}{Character string of the file path to the netCDF mosaic file containing the
climate records.}

\item{spatialIndeces}{List of 2 dataframes describing the spatial relationships between netCDFs
and SpatialPolygonsDataFrame provided. This is the direct output from the internal
\code{determineSpatialRelationships} function.}

\item{zoneField}{Character string of the field name describing the unique ID values that define
the zones.}
}
\description{
\code{spatialAverageSingleFile} is an internal function that returns the spatial average of climate
records that fall inside the zones specified in the "spatialIndeces" input object (created by the
\code{determineSpatialRelationships} function).
}

