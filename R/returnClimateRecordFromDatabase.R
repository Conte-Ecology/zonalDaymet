#' @title Return climate records from SQLite database.
#'
#' @description 
#' \code{returnClimateRecordFromDatabase} Returns the climate records for specified zones as a dataframe from a SQLite database.
#'
#' @param databaseFilePath Character string of the file path to the SQLite database to access.
#' @param databaseTableName Character string of the name of the table to access in the database.
#' @param zoneField Character string of the field name describing the unique ID values that define the zones.
#' @param startDate Character string of the first date (format: "yyyy-mm-dd") to acces from the database
#' @param endDate Character string of the last date (format: "yyyy-mm-dd") to acces from the database
#' @param zoneIDs Vector of unique IDs specifying the zones to return
#' @param variables Vector of character strings indicating the variables to return
#' 
#' @examples
#' returnClimateRecordFromDatabase("C:/USER/Data/Databases/DaymetDB", 
#'                                 "climate_record",
#'                                 "UNIQUE_ID",
#'                                 "1980-01-01",
#'                                 "1980-12-31",
#'                                 c(1, 2, 3, 4, 5),
#'                                 c("tmin", "tmax", "prcp"))
#' @export 
returnClimateRecordFromDatabase <- function(databaseFilePath, databaseTableName, zoneField, startDate, endDate, zoneIDs, variables){
 
  # Connect to the database
  dBase <- src_sqlite(databaseFilePath, create = F)
  
  # Set the range to pull from the database
  dateRange <- paste(seq(from = lubridate::ymd(startDate, tz = "EST"),
                           to = lubridate::ymd(endDate, tz = "EST"),
                           by = "day"))
          
  # Create iteration storage
  tempRecord <- list()
  
  # Loop through variable tables in the database
  for ( i in seq_along(zoneIDs) ){
    
    # Select the current variable table
    sourceTable <- tbl(dBase, sql(paste0("SELECT * FROM ", databaseTableName, " WHERE ", zoneField, " = ", zoneIDs[i])))
      
    # Pull the FeatureIDs and dates
    tempDF <- collect(filter(sourceTable, 
                              Date %in% dateRange))
    
    tempRecord[[i]] <- tempDF[,c(zoneField, 'Date', variables)]
  }
    
  # Join all catchment records
  outRecord <- rbind_all(tempRecord)

  # Return the record
  return(outRecord)
}