#' @title Assign climate record to zones
#'
#' @description 
#' \code{assignClimateRecordToZones} Spatially averages climate records from the Daymet NetCDFs over zones defined by the shapefile.
#'
#' @param zonesShapefile A SpatialPolygonsDataFrame of the zones which will be assigned climate records. The object should be in the WGS geographic coordinate system (same as the Daymet NetCDF files, proj4string = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0").
#' @param zoneField Character string of the field name describing the values that define the zones. For now this field gets converted to integer format.
#' @param daymetDirectory Character string of the file path to the folder containing the Daymet netCDF files.
#' @param variables vector of character strings, the variables to process.
#' @param years numeric, a vector of years to process.
#' @param outputToSQLite logical; if TRUE the spatially averaged climate records for the zones are output into a SQLite database. In this case, "databaseDirectory" and "databaseTableName" must be defined. If FALSE, the output is returned as a dataframe.
#' @param databaseFilePath Character string of the file path to the database to write to. If it does not exist, one will be created.
#' @param databaseTableName Character string of the name of the table to create/write to in the database.
#' 
#' @export 
assignClimateRecordToZones <- function(zonesShapefile, zoneField, daymetDirectory, variables, years, outputToSQLite = FALSE, databaseFilePath = NULL, databaseTableName = NULL){
  
  require(dplyr)
  require(RSQLite)
  require(RSQLite.extfuns)
  
  # Create and/or connect to the database
  # -------------------------------------
  #if ( outputToSQLite == TRUE ){
    
    # If the database does not exist then create it, otherwise establish connection with existing database for uploading.
    if ( !file.exists( databaseFilePath ) ) { 
      src_sqlite(databaseFilePath, create = T) 
    } else ( print("Database already exists. Processed records will be added to existing database."))
    
    database <- dbConnect(SQLite(), databaseFilePath)
  #} else (fullRecord <- NULL)
  
  # Subsets the Daymet mosaic netcdf file based on the provided shapefile
  spatialIndeces <- determineSpatialRelationships(zonesShapefile = zonesShapefile,
                                                    zoneField = zoneField,
                                                    exampleDaymetFile = file.path(daymetDirectory, paste0(variables[1], '_', years[1], '.nc4') ) )

  for( yr in years ){
    
    S <- proc.time()[3]
    
    annualRecord <- NULL
    
    for( variable in variables ){
      
      print(paste0("Spatially averaging '", variable, "' records in ", yr, "." ))
      
      ncdfPath <- file.path(daymetDirectory, paste0(variable, '_', yr, '.nc4') )
      
      record <- spatialAverageSingleFile(daymetMosaicFilePath = ncdfPath, 
                                         spatialIndeces = spatialIndeces, 
                                         zoneField = zoneField)       
      
      # Converts unique ID to integer for database purposes
      record[ ,zoneField] <- as.integer(record[ ,zoneField])
      
      print("Averaging complete")
      
      # Store the variables records for the current year
      if(is.null(annualRecord)){
        annualRecord <- record
      } else( annualRecord <- left_join(annualRecord, record, by = c(zoneField, 'Date')))   

    }# End variables loop
    

    
    #if ( outputToSQLite == TRUE ) {
    
      # Upload to database
      # ------------------
      # If the table doesn't exist in the database, create it. If if does, append to it.
      if(!databaseTableName %in% dbListTables(database) ){
        dbWriteTable(conn = database, 
                     name = databaseTableName, 
                     #value = as.data.frame(annualRecord),
                     value = annualRecord,
                     append = FALSE, 
                     row.names = FALSE)
      } else( dbWriteTable(conn = database, 
                           name = databaseTableName, 
                           #value = as.data.frame(annualRecord), 
                           value = annualRecord,
                           append = TRUE,  
                           row.names = FALSE) )
    
      # Status update with time elapsed
      print( paste0("Done writing results into database for ", yr, ". Elapsed time: ", (proc.time()[3] - S)/60, " minutes.") )
    #} else{
    #  if( is.null(fullRecord) ){
    #    fullRecord <- annualRecord
    #  } else( fullRecord <- rbind(fullRecord, annualRecord) )
    #}
    
    rm(annualRecord)
    
  }# End years loop

  rm(spatialIndeces, zonesShapefile, zoneField, daymetDirectory, variables, years)
  
  dbDisconnect(conn = database)
  
  ## Close the database connection
  #if ( outputToSQLite == TRUE ) { dbDisconnect(conn = database)}
  #  else ( return(fullRecord) )
  
  ## If the results are not being written to a database, then they get returned as a dataframe.
  #if ( outputToSQLite == FALSE ) { return(fullRecord); rm(fullRecord)}
      
}# End function
