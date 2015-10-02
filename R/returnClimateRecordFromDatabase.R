#' @title Return climate records from SQLite database.
#'
#' @description 
#' \code{returnClimateRecordFromDatabase} Returns the climate records for specified zones as a dataframe from a SQLite database.
#'
#' @param databaseFilePath Character string of the file path to the database to access.
#' @param databaseTableName Character string of the name of the table to access in the database.
#' @param startDate Character string of the first date (format: "yyyy-mm-dd") to acces from the database
#' @param endDate Character string of the last date (format: "yyyy-mm-dd") to acces from the database
#' @param zoneIDs Vector of unique IDs specifying the zones to return
#' @param variables Vector of character strings indicating the variables to return
#' 
#' @export 
returnClimateRecordFromDatabase <- function(databaseFilePath, databaseTableName, startDate, endDate, zoneIDs, variables){
  
  # Libraries
  require(dplyr)
  require(lubridate)
  
  # Connect to the database
  dBase <- src_sqlite(databaseFilePath, create = F)
  
  # Set the range to pull from the database
  dateRange <- paste(seq(from = ymd(startDate, tz = "EST"),
                           to = ymd(endDate, tz = "EST"),
                           by = "day"))
          
  # Create iteration storage
  tempRecord <- list()
  
  # Loop through variable tables in the database
  for ( i in seq_along(zoneIDs) ){
    
    # Select the current variable table
    sourceTable <- tbl(dBase, sql(paste0("SELECT * FROM ", databaseTableName)))
      
    # Pull the FeatureIDs and dates
    tempDF <- collect(filter(sourceTable, 
                                      FEATUREID == zoneIDs[i], 
                                      Date %in% dateRange))
    
    tempRecord[[i]] <- tempDF[,c('FEATUREID', 'Date', variables)]
  }
    
  # Join all catchment records
  outRecord <- rbind_all(tempRecord)

  # Return the record
  return(outRecord)
}