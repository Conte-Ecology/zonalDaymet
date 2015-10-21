#' @title Download netCDF mosaic files.
#'
#' @description
#' The \code{downloadMosaic} function downloads mosaic NetCDF files from the Daymet THREDDS server for the variables and years specified.
#'
#' @param years A numeric vector specifying the years to download
#' @param variables A character vector specifying the variables to download
#' @param destinationDirectory Character string of the file path to the target folder for downloaded files
#' @param retryFailedDownloads logical; If TRUE the function will check (by opening) to see if the netCDF files are corrupt.
#' 
#' @examples
#' downloadMosaic(1980:1990, 
#'                c("tmin", "tmax", "prcp"), 
#'                "C:/USER/Data/Daymet", 
#'                TRUE)
#' 
#' @export 
downloadMosaic <- function(years, variables, destinationDirectory, retryFailedDownloads = TRUE){
  
  for (year in years) {
    for (var in variables) {
    
      # Name the output file
      outFile <- file.path(destinationDirectory, paste0(var, '_', year, '.nc4'))
            
      # Check file existence and validity
      # ---------------------------------
      
      if(file.exists(outFile)){
        
        # Alerts user of existing file
        print(paste0("File '", outFile ,"' already exists in download directory."))#;

        if (retryFailedDownloads == TRUE) {
          
          if (is(try(ncdf4::nc_open(outFile), silent=T),"try-error")) {
           
            # Alerts the user if the existing file is corrupt
            print(paste0("Existing file: '", outFile, "' is corrupt. Deleting this file and retry download."))
            
          }# end corruption check         
        }# end if retryFailedDownloads
      }# end file.exists check
      
      # If the file doesn't exist, download it
      while (!file.exists(outFile)) {
        
        # Start time
        beg <- proc.time()[3]
        
        # Download the file from the THREDDS server
        download.file(url      = paste0('http://thredds.daac.ornl.gov/thredds/fileServer/ornldaac/1219/', year, '/', var, '_', year, '.nc4'), 
                      destfile = outFile, 
                      quiet    = FALSE, 
                      mode     = 'wb')
            
        # Print download time
        print(paste0("Download took ", (proc.time()[3] - beg)/3600, " hours.") )
        
        
        if (retryFailedDownloads == TRUE) {
          
          # Test to see if the file downloaded correctly. If not, retry download.
          fileTest <- try(ncdf4::nc_open(outFile), silent=T)
          
          if (is(fileTest,"try-error")) {
            
            file.remove(outFile)
            print("File corrupt. Removing and redownloading....")
            
          } else {
            ncdf4::nc_close(fileTest)
            rm(fileTest)
          }
        }# end if retryFailedDownloads
      }# end while 
    }# end variable loop
  }# end year loop
}# end function
