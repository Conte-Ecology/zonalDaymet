#' @title Assign climate record to zones
#'
#' @description 
#' \code{assignZonalRecordsToDatabase} Spatially averages climate records from the Daymet netCDF mosaic files over zones defined by spatial polygons and exports directly to a SQLite database. If the shapefile is large enough to cause memory problems, the function can be iterated over the results of the "tileShapefile" function.
#'
#' @param zonesShapefile A SpatialPolygonsDataFrame of the zones which will be assigned climate records. The object should have a unique ID column and be in the WGS geographic coordinate system (same as the Daymet NetCDF files, proj4string = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0").
#' @param zoneField Character string of the field name describing the unique ID values that define the zones.
#' @param zoneFieldType Character string defining the column type for the unique ID field written into the database. Options are "character", "integer", "numeric", or the default: NULL. A NULL value will base the type on the shapefile field type as interpreted by R.
#' @param mosaicDirectory Character string of the file path to the folder containing the netCDF mosaic files.
#' @param variables Vector of character strings indicating the variables to process.
#' @param years Vector of numeric values indicating the years to process.
#' @param databaseFilePath Character string of the file path to the database to write to. If it does not exist, one will be created.
#' @param databaseTableName Character string of the name of the table to create/write to in the database.
#' 
#' @examples
#' assignZonalRecordsToDatabase(spatialPolygonsDataFrame, 
#'                                        "UNIQUE_ID",
#'                                        "integer",
#'                                        "C:/USER/Data/Daymet",
#'                                        c("tmin", "tmax", "prcp"),
#'                                        1980:1990, 
#'                                        "C:/USER/Data/Databases/DaymetDB",
#'                                        "climate_record")
#' 
#' @export 
assignZonalRecordsToDatabase <- function(zonesShapefile, zoneField, zoneFieldType = NULL, mosaicDirectory, variables, years, databaseFilePath = NULL, databaseTableName = NULL){
    
  if (!requireNamespace("RSQLite", quietly = TRUE)) {
    stop("The RSQLite is required for this function to work. Please install it.",
         call. = FALSE)
  }
  
  
  # Create and/or connect to the database
  # -------------------------------------
  # If the database does not exist then create it, otherwise establish connection with existing database for uploading.
  if (!file.exists(databaseFilePath)) { 
    src_sqlite(databaseFilePath, create = T) 
  } else {
    print("Database already exists. Processed records will be added to existing database.")
  }
  
  database <- RSQLite::dbConnect(RSQLite::SQLite(), databaseFilePath)
  
  
  # Reference NetCDF indeces
  # ------------------------
  # Subsets the Daymet mosaic netcdf file based on the provided shapefile
  spatialIndeces <- determineSpatialRelationships(zonesShapefile    = zonesShapefile,
                                                  zoneField         = zoneField,
                                                  exampleMosaicFile = file.path(mosaicDirectory, paste0(variables[1], '_', years[1], '.nc4') ) )

  
  # Process NetCDF data
  # -------------------
  for (yr in years) {
    
    S <- proc.time()[3]
    
    annualRecord <- NULL
    
    # Process each variable listed
    for (variable in variables) {
      
      # Status update
      print(paste0("Spatially averaging '", variable, "' records in ", yr, "." ))
            
      # Average the daymet points that fall inside each polygon
      record <- spatialAverageSingleFile(mosaicFilePath = file.path(mosaicDirectory, paste0(variable, '_', yr, '.nc4')), 
                                         spatialIndeces = spatialIndeces, 
                                         zoneField      = zoneField)       
      
      # Ensure the unique ID field is in the desired format for the database.
      if (!is.null(zoneFieldType)){
        if (zoneFieldType == "character") {
          record[ ,zoneField] <- as.character(record[ ,zoneField])
          
        } else if (zoneFieldType == "integer") {
          record[ ,zoneField] <- as.integer(record[ ,zoneField])
        
        } else if (zoneFieldType == "numeric") {
          record[ ,zoneField] <- numeric(record[ ,zoneField])
        }
      }
      
      # Status update
      print("Averaging complete")
      
      # Store the variables records for the current year
      if (is.null(annualRecord)) {
        annualRecord <- record
      } else {
        annualRecord <- left_join(annualRecord, record, by = c(zoneField, 'Date'))
      }
    }# End variables loop

    
    # Upload to database
    # ------------------
    # If the table doesn't exist in the database, create it. If if does, append to it.
    if (!databaseTableName %in% RSQLite::dbListTables(database)) {
      RSQLite::dbWriteTable(conn      = database, 
                            name      = databaseTableName, 
                            value     = annualRecord,
                            append    = FALSE, 
                            row.names = FALSE)
    } else {
      RSQLite::dbWriteTable(conn      = database, 
                            name      = databaseTableName, 
                            value     = annualRecord,
                            append    = TRUE,  
                            row.names = FALSE) 
    }
    
    # Status update
    print( paste0("Done writing results into database for ", yr, ". Elapsed time: ", (proc.time()[3] - S)/60, " minutes.") )
   
    rm(annualRecord)
    
  }# End years loop
 
  # Disconnect from the database
  RSQLite::dbDisconnect(conn = database)
      
}# End function