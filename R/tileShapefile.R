#' @title Divide SpatialPolygonsDataFrame into smaller tiles
#'
#' @description
#' The \code{tileShapefile} function splits large shapefiles into up into 1 or 2 degree tiles while
#' preserving the zonal polygons. The purpose of this is to prevent memory issues when reading Daymet 
#' records.
#'
#' @param shapefile A SpatialPolygonsDataFrame of the zones which will be assigned climate records. 
#' The object should be in the WGS geographic coordinate system to match the netCDF files 
#' (proj4string = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0").
#' @param tileDegree A numeric value of either 1 or 2 indicating the length of the tile edge in 
#' lat/lon degrees.
#'
#' @details
#' Polygons are assigned to tiles based on where their centroids are located.
#' The spatial object should be in the Daymet coordinate system before executing the function.
#' This function only works for SpatialPolygonsDataFrame with units that are lat/lon.  
#' The \code{spTransform} function can be used to transform the object from a projected to a 
#' geographic coordinate system.
#'
#' @examples
#' tileShapefile(shapefile  = spatialPolygonsDataFrame, 
#'               tileDegree = 2)
#'
#' @export 
tileShapefile <- function(shapefile, tileDegree){
  
  # Create storage for shapefiles
  shapefileTiles <- list()
  
  # Get the bounds of the shapefile for tiling
  EXT <- raster::extent(shapefile)

  xmin <- floor(EXT@xmin)
  xmax <- ceiling(EXT@xmax)
  ymin <- floor(EXT@ymin)
  ymax <- ceiling(EXT@ymax)
  
  # Get the centroids of the polygons for tiling
  centroids <- data.frame(sp::coordinates(shapefile))
  names(centroids) <- c('X', 'Y')
    
  # Create tile bounds for dividing catchments
  if (tileDegree == 1) {
    Xs <- seq(from = xmin, to = xmax, by = 1)
    Ys <- seq(from = ymin, to = ymax, by = 1)     
  } else if (tileDegree == 2) { 
    Xs <- seq(from = xmin, to = xmax+1, by = 2)
    Ys <- seq(from = ymin, to = ymax+1, by = 2)
  } else {
    print("Specify tile size in degrees (1 or 2).")
  }
    
  # Get number of X and Y tiles for looping
  XL <- length(Xs) - 1
  YL <- length(Ys) - 1
    
  tileNum <- 0
  # Loop through latitude (Y) bounds of tiles
  for (i in 1:YL) {
      
    # Y bounds
    YBmin <- Ys[i]
    YBmax <- Ys[i+1]
      
    # Loop through longitude (X) bounds of tiles
    for (j in 1:XL) {
        
      # X bounds
      XBmin <- Xs[j]
      XBmax <- Xs[j+1]
    
      # Select the catchments that fall within the current tile boundaries
      rows <- which(centroids$X >= XBmin & centroids$X < XBmax & centroids$Y >= YBmin & centroids$Y < YBmax)
        
      # If catchments fall into the tile then store them as a separate shapefile
      if (length(rows) > 0) {
        tileNum <- tileNum +1
          
        shapefileTiles[[tileNum]] <- shapefile[rows,]
      }
        
      ## Status update
      #print(paste0("Tiling shapefile: ", j +(XL*(i-1)), " of ", XL*YL, " iterations complete."))
      
    }# end lon loop
  }# end lat loop

  return(shapefileTiles)
}