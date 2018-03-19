#' @title Spatially average multiple climate records by zone
#'
#' @description 
#' \code{spatialAverageSingleFile} is an internal function that returns the spatial average of climate 
#' records that fall inside the zones specified in the "spatialIndeces" input object (created by the
#' \code{determineSpatialRelationships} function).
#' 
#' @param mosaicFilePath Character string of the file path to the netCDF mosaic file containing the 
#' climate records.
#' @param spatialIndeces List of 2 dataframes describing the spatial relationships between netCDFs 
#' and SpatialPolygonsDataFrame provided. This is the direct output from the internal 
#' \code{determineSpatialRelationships} function.
#' @param zoneField Character string of the field name describing the unique ID values that define 
#' the zones.

spatialAverageSingleFile <- function(mosaicFilePath, spatialIndeces, zoneField){
  
  # Access pre-existing indeces
  # --------------------------- 
  mosaicIndeces <- spatialIndeces$mosaicIndeces
  shapefileIndeces <- spatialIndeces$shapefileIndeces
  
  
  # Access variables from netCDF
  # ----------------------------
  netCDF <- ncdf4::nc_open(file.path(mosaicFilePath))
  
  # Get the year and variable directly from netcdf file
  YR <- ncdf4::ncatt_get(netCDF, varid = 0, attname="start_year")$value
  variable <- names(netCDF$var)[names(netCDF$var) %in% c("tmax", "tmin", "prcp", "dayl", "srad", "vp", "swe")]
    
  #Read "Day of Year" and adjust it to start with 1 instead of 0
  dOY <- ncdf4::ncvar_get(nc    = netCDF,
                          varid = "yearday",
                          start = 1,
                          count = netCDF$var$yearday$varsize)
  dOY <- dOY + 1
  
  # Read variable
  shapeVar <- ncdf4::ncvar_get(nc    = netCDF,
                               varid = variable,
                               start = c(mosaicIndeces$minRow, mosaicIndeces$minCol, 1),
                               count = c(mosaicIndeces$countx, mosaicIndeces$county, length(dOY)))


  # Index and replace missing value with NA
  # ---------------------------------------
  # Determine the currently assigned missing value
  for (h in 1:length(netCDF$var)) {
    if (netCDF$var[[h]]$name == variable) {
      varIndex <- h
    }
  }
  missingValue <- netCDF$var[[varIndex]]$missval
    
  # Replace
  shapeVar <- replace(shapeVar, shapeVar == missingValue, NA)
    
  # Close the netCDF connection
  ncdf4::nc_close(netCDF)
    
  # Spatially average Daymet records for zones
  # ------------------------------------------
  # Setup storage dataframe (one year-long row for each point assigned to a zone)
  varPoints <- data.frame(matrix(nrow = nrow(shapefileIndeces), ncol = 366))
  names(varPoints) <- c(zoneField, dOY)
  varPoints[,zoneField] <- shapefileIndeces[,zoneField]
    
  # Loop through days, pulling records from variable array
  for (sel in dOY){
    selection <- as.matrix(data.frame(shapefileIndeces[c('subRow', 'subCol')], sel) )
    varPoints[,sel+1] <- shapeVar[selection]
  }
  
  # Take the mean of the points inside a catchment
  varMeans <-  group_by_(varPoints, zoneField) %>% 
                  summarise_all(funs(mean(.,na.rm = T))) %>%
                  ungroup()
  
  # Melt means into output format
  varMelt <- reshape2::melt(varMeans, id=c(zoneField))
  names(varMelt) <- c(zoneField, 'DayOfYear', variable)
  varMelt$DayOfYear <- as.numeric(varMelt$DayOfYear)
  varMelt$Year <- YR
  varMelt$Date <- paste(lubridate::parse_date_time(paste0(varMelt$Year,"-", varMelt$DayOfYear), "y-j", tz = "EST") )
  
  output <- varMelt[,c(zoneField, "Date", variable)]
   
  # Output result
  return(output)

}# End function