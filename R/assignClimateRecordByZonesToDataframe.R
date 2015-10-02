#' @title Assign climate record to zones
#'
#' @description 
#' \code{assignClimateRecordByZonesToDataframe} Spatially averages climate records from the Daymet NetCDFs over zones defined by the shapefile and returns a dataframe. This function is intended for use on smaller 
#'
#' @param zonesShapefile A SpatialPolygonsDataFrame of the zones which will be assigned climate records. The object should be in the WGS geographic coordinate system (same as the Daymet NetCDF files, proj4string = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0").
#' @param zoneField Character string of the field name describing the values that define the zones. For now this field gets converted to integer format.
#' @param daymetDirectory Character string of the file path to the folder containing the Daymet netCDF files.
#' @param variables Vector of character strings, the variables to process.
#' @param years Vector of numeric values indicating the years to process.
#' 
#' @export 
assignClimateRecordByZonesToDataframe <- function(zonesShapefile, zoneField, daymetDirectory, variables, years){
  
  # Libraries
  require(dplyr)
  
  
  # Reference NetCDF indeces
  # ------------------------  
  # Subsets the Daymet mosaic netcdf file based on the provided shapefile
  spatialIndeces <- determineSpatialRelationships(zonesShapefile = zonesShapefile,
                                                    zoneField = zoneField,
                                                    exampleDaymetFile = file.path(daymetDirectory, paste0(variables[1], '_', years[1], '.nc4') ) )
  
  
  # Process NetCDF data
  # -------------------
  fullRecord <- NULL
  
  for( yr in years ){
    
    S <- proc.time()[3]
    
    annualRecord <- NULL
    
    
    for( variable in variables ){
      
      print(paste0("Spatially averaging '", variable, "' records in ", yr, "." ))
      
      ncdfPath <- file.path(daymetDirectory, paste0(variable, '_', yr, '.nc4') )
      
      # Average the daymet points that fall inside each polygon      
      record <- spatialAverageSingleFile(daymetMosaicFilePath = ncdfPath, 
                                         spatialIndeces = spatialIndeces, 
                                         zoneField = zoneField)       
      
      print("Averaging complete")
      
      # Store the variables records for the current year
      if(is.null(annualRecord)){
        annualRecord <- record
      } else( annualRecord <- left_join(annualRecord, record, by = c(zoneField, 'Date')))

    }# End variables loop
    
    # Join years together
    if( is.null(fullRecord) ){
        fullRecord <- annualRecord
      } else( fullRecord <- rbind(fullRecord, annualRecord) )
    
    rm(annualRecord)
    
  }# End years loop

  rm(spatialIndeces, zonesShapefile, zoneField, daymetDirectory, variables, years)
    
  return(fullRecord)   
}# End function
