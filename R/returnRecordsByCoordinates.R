#' @title Return climate records based on coordinate bounds
#' 
#' @description 
#' The \code{returnRecordsByCoordinates} function returns the climate records for a section of the 
#' mosaic file specified by coordinate bounds. 
#'
#' @param areaExtent Vector string of numeric values specifying lat/lon coordinates of the bounding 
#' box. The format is: c(Minimum Longitude, Maximum Longitude, Minimum Latitude, Maximum Latitude).
#' @param mosaicFile Character string of the file path to the netCDF mosiac file to access.
#' @param outputFormat A character string of the format of the dataframe output. Options are "wide" 
#' or "long" defaulting to "long".
#' @param startDate A character string of the first date to return. This date must match the year of 
#' the mosaic file. The format is: "yyyy-mm-dd"
#' @param endDate A character string of the last date to return. This date must match the year of the 
#' mosaic file. The format is: "yyyy-mm-dd"
#'
#' @examples
#' returnRecordsByCoordinates(areaExtent   = c(-71.0, -70.0, 42.0, 43.0),
#'                            mosaicFile   = "C:/USER/Data/Daymet/prcp_2010.nc4",
#'                            outputFormat = "long",
#'                            startDate    = "2010-01-01",
#'                            endDate      = "2010-01-31")
#'                            
#' @details 
#' Climate records for all points within the bounding box will be retrieved and identified by their
#' source latitude and longitude coordinates.
#'
#' @export
returnRecordsByCoordinates <- function(areaExtent, mosaicFile, outputFormat = "long", startDate, endDate){
  
  # Check the extent values make sense
  if (areaExtent[4] < areaExtent[3] | areaExtent[2] < areaExtent[1]) {
    stop("The latitude and longitude coordinates of the bounding area do not comply with the required format. 
         Check 'areaExtent' input values.")
  } else {
    
    
    # Format dates
    startDOY <- lubridate::yday(lubridate::ymd(startDate))
    endDOY   <- lubridate::yday(lubridate::ymd(endDate))

    startYr <- lubridate::year(lubridate::ymd(startDate))
    endYr <-   lubridate::year(lubridate::ymd(endDate))
    
    
    # Read Daymet mosaic spatial information
    # --------------------------------------
    # Connect with the sample netCDF to 
    netCDF <- ncdf4::nc_open(mosaicFile)  #netcdf     
    
    
    fileYr <- ncdf4::ncatt_get(netCDF, varid = 0, attname="start_year")$value
    
    
    if (!(startYr == fileYr & endYr == fileYr)) {
      stop("Specified dates do not match the year of the mosaic file.")
    } else {
    
      # Latitude coordinates in full mosaic
      mosaicLat = ncdf4::ncvar_get (nc    = netCDF, 
                                    varid = "lat", 
                                    start = c(1,1), 
                                    count = c(netCDF$var$lat$varsize[1], 
                                              netCDF$var$lat$varsize[2]) )
            
      # Longitude coordinates in full mosaic
      mosaicLon = ncdf4::ncvar_get (nc    = netCDF, 
                                    varid = "lon", 
                                    start = c(1,1), 
                                    count = c(netCDF$var$lon$varsize[1], 
                                              netCDF$var$lon$varsize[2]) )
            
      
      # Calculate the indeces of the mosaic netcdf for the tile
      # -------------------------------------------------------
      # Determine the array positions of coordinates within the shapefile extent for spatial referencing
      arrayIndeces <- which(mosaicLat >= areaExtent[3] & #ymin
                            mosaicLat <= areaExtent[4] & #ymax
                            mosaicLon >= areaExtent[1] & #xmin
                            mosaicLon <= areaExtent[2],  #xmax
                            arr.ind    = T)
    
      # Corners of the box in the array
      minRow <- min(arrayIndeces[,1])
      maxRow <- max(arrayIndeces[,1])
      minCol <- min(arrayIndeces[,2])
      maxCol <- max(arrayIndeces[,2])
      
      # Number of rows and columns
      countx = maxRow - minRow + 1 
      county = maxCol - minCol + 1 
      
      # Save the spatial references for use in other netCDF files
      mosaicIndeces <- data.frame(minRow = minRow, 
                                  maxRow = maxRow, 
                                  minCol = minCol, 
                                  maxCol = maxCol, 
                                  countx = countx, 
                                  county = county)
      
      rm(mosaicLon, mosaicLat, arrayIndeces)
      
    
      # Get the year and variable 
      # -------------------------
      year <- ncdf4::ncatt_get(netCDF, varid = 0, attname="start_year")$value
      variable <- names(netCDF$var)[names(netCDF$var) %in% c("tmax", "tmin", "prcp", "dayl", "srad", "vp", "swe")]
      
      
      # Read variable from NetCDF
      # -------------------------
      # Latitude coordinates in the current tile
      shapeLat = ncdf4::ncvar_get(nc    = netCDF, 
                                  varid = "lat",     
                                  start = c(mosaicIndeces$minRow, mosaicIndeces$minCol),    
                                  count = c(mosaicIndeces$countx, mosaicIndeces$county) )
            
      # Longitude coordinates in the current tile
      shapeLon = ncdf4::ncvar_get(nc    = netCDF, 
                                  varid = "lon",     
                                  start = c(mosaicIndeces$minRow, mosaicIndeces$minCol),    
                                  count = c(mosaicIndeces$countx, mosaicIndeces$county) )
      
      # Read variable
      shapeVar <- ncdf4::ncvar_get(nc    = netCDF,
                                   varid = variable,
                                   start = c(mosaicIndeces$minRow, mosaicIndeces$minCol, startDOY),
                                   count = c(mosaicIndeces$countx, mosaicIndeces$county, (endDOY - startDOY + 1) ) )
      
     
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
      
      # Close the NetCDF connection
      ncdf4::nc_close(netCDF)
      
      
      # Restructure data for output
      # ---------------------------
      # Shift matrix to wide-format dataframe
      varWide <- data.frame(matrix(nrow = nrow(shapeLat)*ncol(shapeLat), ncol = endDOY - startDOY + 3))
      names(varWide) <- c("Latitude", "Longitude", startDOY:endDOY)
      
      varWide$Latitude  <- reshape2::melt(shapeLat)$value
      varWide$Longitude <- reshape2::melt(shapeLon)$value
        
      # Loop through days, pulling records from variable array
      for (sel in 1:(endDOY - startDOY + 1)){
        varWide[2+sel] <- reshape2::melt(shapeVar[,,sel])$value
      }
  
      # Rename column names as dates
      names(varWide)[3:ncol(varWide)] <- paste(lubridate::parse_date_time(paste0(year,"-", c(startDOY:endDOY)), 
                                                                          "y-j", 
                                                                          tz = "EST"))
      
      # Return output
      if (outputFormat == "wide") {
        
        return(varWide)
        
      } else if (outputFormat == "long") {
      
        # Convert wide format to long format
        varLong <- reshape2::melt(varWide, id=c("Latitude", "Longitude"))
        names(varLong)[3:4] <- c('Date', variable)
        varLong$Date <- paste(varLong$Date)
              
        # Return records
        return(varLong)
        
      } else {
        stop("Specify output format as either 'wide' or 'long'.") 
      }# End output formating
      
    }# End dates check
  }# End lat/lon error check
}# End funciton



