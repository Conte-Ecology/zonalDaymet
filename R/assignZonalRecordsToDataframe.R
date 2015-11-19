#' @title Assign climate record to zones
#'
#' @description 
#' The \code{assignZonalRecordsToDataframe} function patially averages climate records from the 
#' netCDF mosaic files over zones defined by spatial polygons and returns a dataframe. 
#'
#' @param zonesShapefile A SpatialPolygonsDataFrame of the zones which will be assigned climate records. 
#' The object should have a unique ID column and be in the WGS geographic coordinate system (same as the 
#' Daymet NetCDF files, proj4string = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0").
#' @param zoneField Character string of the field name describing the unique ID values that define the zones.
#' @param mosaicDirectory Character string of the file path to the folder containing the netCDF mosaic files.
#' @param variables Vector of character strings indicating the variables to process.
#' @param years Vector of numeric values indicating the years to process.
#' 
#' @details
#' The netCDF files are read and spatially indexed using custom internal functions. The climate time series 
#' area assigned to the zones represented by the "zonesShapefile" object. If only one record falls into a polygon
#' it is assigned. If multuple records fall into a single polygon, they are averaged. If no records
#' fall into a polygon, the record nearest to the polygon centroid is assigned. The ouput is written to a 
#' dataframe in long format with columns for the "zoneField", date, and each of the climate variables.
#'
#' The function relies on the default naming scheme for the netCDF mosaics (e.g. "prcp_2008.nc4").
#'
#' If the original shapefile is large enough to cause memory problems, the function can be iterated over the 
#' results of the \code{tileShapefile} function which splits the spatial polygon into manageable chunks.
#' 
#' @examples
#' assignZonalRecordsToDataframe(zonesShapefile  = spatialPolygonsDataFrame,
#'                               zoneField       = "UNIQUE_ID",
#'                               mosaicDirectory = "C:/USER/Data/Daymet",
#'                               variables       = c("tmin", "tmax", "prcp"),
#'                               years           = 1980:1990)
#'
#' @export 
assignZonalRecordsToDataframe <- function(zonesShapefile, zoneField, mosaicDirectory, variables, years){

  # Reference NetCDF indeces
  # ------------------------  
  # Subsets the Daymet mosaic netcdf file based on the provided shapefile
  spatialIndeces <- determineSpatialRelationships(zonesShapefile    = zonesShapefile,
                                                  zoneField         = zoneField,
                                                  exampleMosaicFile = file.path(mosaicDirectory, 
                                                                                paste0(variables[1], 
                                                                                       '_', years[1], 
                                                                                       '.nc4') ) )
  
  
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
      record <- spatialAverageSingleFile(mosaicFilePath = file.path(mosaicDirectory, 
                                                                    paste0(variable, 
                                                                           '_', yr, 
                                                                           '.nc4')), 
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
