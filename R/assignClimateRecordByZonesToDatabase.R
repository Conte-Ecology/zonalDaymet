#' @title Assign climate record to zones
#'
#' @description 
#' \code{assignClimateRecordByZonesToDatabase} Spatially averages climate records from the Daymet NetCDFs over zones defined by the shapefile and exports directly to a SQLite database.
#'
#' @param zonesShapefile A SpatialPolygonsDataFrame of the zones which will be assigned climate records. The object should be in the WGS geographic coordinate system (same as the Daymet NetCDF files, proj4string = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0").
#' @param zoneField Character string of the field name describing the unique ID values that define the zones.
#' @param zoneFieldType Character string defining the column type for the unique ID field written into the database. Options are "character", "integer", "numeric", or the default: NULL. A NULL value will base the type on the shapefile field type as interpreted by R.
#' @param daymetDirectory Character string of the file path to the folder containing the Daymet netCDF files.
#' @param variables Vector of character strings indicating the variables to process.
#' @param years Vector of numeric values indicating the years to process.
#' @param databaseFilePath Character string of the file path to the database to write to. If it does not exist, one will be created.
#' @param databaseTableName Character string of the name of the table to create/write to in the database.
#' 
#' @export 
assignClimateRecordByZonesToDatabase <- function(zonesShapefile, zoneField, zoneFieldType = NULL, daymetDirectory, variables, years, databaseFilePath = NULL, databaseTableName = NULL){
  
  # Libraries
  require(dplyr)
  require(RSQLite)
  require(RSQLite.extfuns)
  
  
  # Create and/or connect to the database
  # -------------------------------------
  # If the database does not exist then create it, otherwise establish connection with existing database for uploading.
  if ( !file.exists( databaseFilePath ) ) { 
    src_sqlite(databaseFilePath, create = T) 
  } else ( print("Database already exists. Processed records will be added to existing database."))
    
  database <- dbConnect(SQLite(), databaseFilePath)
  
  
  # Reference NetCDF indeces
  # ------------------------
  # Subsets the Daymet mosaic netcdf file based on the provided shapefile
  spatialIndeces <- determineSpatialRelationships(zonesShapefile = zonesShapefile,
                                                    zoneField = zoneField,
                                                    exampleDaymetFile = file.path(daymetDirectory, paste0(variables[1], '_', years[1], '.nc4') ) )

  
  # Process NetCDF data
  # -------------------
  for( yr in years ){
    
    S <- proc.time()[3]
    
    annualRecord <- NULL
    
    # Process each variable listed
    for( variable in variables ){
      
      print(paste0("Spatially averaging '", variable, "' records in ", yr, "." ))
      
      ncdfPath <- file.path(daymetDirectory, paste0(variable, '_', yr, '.nc4') )
      
      # Average the daymet points that fall inside each polygon
      record <- spatialAverageSingleFile(daymetMosaicFilePath = ncdfPath, 
                                         spatialIndeces = spatialIndeces, 
                                         zoneField = zoneField)       
      
      # Ensure the unique ID field is in the desired format for the database.
      if (!is.null(zoneFieldType){
      
        if (zoneFieldType == "character"){
          record[ ,zoneField] <- as.character(record[ ,zoneField])
          
        }else if(zoneFieldType == "integer"){
          record[ ,zoneField] <- as.integer(record[ ,zoneField])
        
        }else if(zoneFieldType == "numeric"){
          record[ ,zoneField] <- numeric(record[ ,zoneField])
        }
      }
            
      print("Averaging complete")
      
      # Store the variables records for the current year
      if(is.null(annualRecord)){
        annualRecord <- record
      } else( annualRecord <- left_join(annualRecord, record, by = c(zoneField, 'Date')))   

    }# End variables loop

    
    # Upload to database
    # ------------------
    # If the table doesn't exist in the database, create it. If if does, append to it.
    if(!databaseTableName %in% dbListTables(database) ){
      dbWriteTable(conn = database, 
                    name = databaseTableName, 
                    value = annualRecord,
                    append = FALSE, 
                    row.names = FALSE)
    } else( dbWriteTable(conn = database, 
                          name = databaseTableName, 
                          value = annualRecord,
                          append = TRUE,  
                          row.names = FALSE) )
    
    # Provide an update on elapsed time
    print( paste0("Done writing results into database for ", yr, ". Elapsed time: ", (proc.time()[3] - S)/60, " minutes.") )
   
    rm(annualRecord)
    
  }# End years loop

  # Delete objects
  rm(spatialIndeces, zonesShapefile, zoneField, daymetDirectory, variables, years)
  
  # Disconnect from the database
  dbDisconnect(conn = database)
      
}# End function