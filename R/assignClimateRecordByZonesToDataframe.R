#' @title Assign climate record to zones
#'
#' @description 
#' \code{assignClimateRecordByZonesToDataframe} Spatially averages climate records from the Daymet netCDF mosaic files over zones defined by the shapefile and returns a dataframe. If the shapefile is large enough to cause memory problems, the function can be iterated over the results of the "tileShapefile" function.
#'
#' @param zonesShapefile A SpatialPolygonsDataFrame of the zones which will be assigned climate records. The object should have a unique ID column and be in the WGS geographic coordinate system (same as the Daymet NetCDF files, proj4string = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0").
#' @param zoneField Character string of the field name describing the unique ID values that define the zones.
#' @param mosaicDirectory Character string of the file path to the folder containing the netCDF mosaic files.
#' @param variables Vector of character strings indicating the variables to process.
#' @param years Vector of numeric values indicating the years to process.
#' 
#' @examples
#' assignClimateRecordByZonesToDataframe(spatialPolygonsDataFrame, 
#'                                        "UNIQUE_ID",
#'                                        "C:/USER/Data/Daymet",
#'                                        c("tmin", "tmax", "prcp"),
#'                                        1980:1990)
#'                                        
#' @export 
assignClimateRecordByZonesToDataframe <- function(zonesShapefile, zoneField, mosaicDirectory, variables, years){

  # Reference NetCDF indeces
  # ------------------------  
  # Subsets the Daymet mosaic netcdf file based on the provided shapefile
  spatialIndeces <- determineSpatialRelationships(zonesShapefile      = zonesShapefile,
                                                  zoneField           = zoneField,
                                                  exampleMosaicFile   = file.path(mosaicDirectory, paste0(variables[1], '_', years[1], '.nc4') ) )
  
  
  # Process NetCDF data
  # -------------------
  fullRecord <- NULL
  
  for (yr in years) {
    
    S <- proc.time()[3]
    
    annualRecord <- NULL
    
    for (variable in variables) {
      
      # Status update
      print(paste0("Spatially averaging '", variable, "' records in ", yr, "." ))
      
      # Average the daymet points that fall inside each polygon      
      record <- spatialAverageSingleFile(mosaicFilePath = file.path(mosaicDirectory, paste0(variable, '_', yr, '.nc4')), 
                                         spatialIndeces = spatialIndeces, 
                                         zoneField      = zoneField)       
      
      # Status update
      print("Averaging complete")
      
      # Store the variables records for the current year
      if (is.null(annualRecord)) {
        annualRecord <- record
      } else {
        annualRecord <- left_join(annualRecord, record, by = c(zoneField, 'Date'))
      }
    }# End variables loop
    
    # Join years together
    if (is.null(fullRecord)) {
      fullRecord <- annualRecord
    } else {
      fullRecord <- rbind(fullRecord, annualRecord)
    }
    
  }# End years loop
    
  return(fullRecord)  
  
}# End function
