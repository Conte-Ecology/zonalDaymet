#' @title Determine spatial relationships between NetCDF and zone shapefile
#' 
#' @description Defines the spatial relationships between the Daymet NetCDF files and the zones shapefile
#'
#' @param zonesShapefile SpatialPolygonsDataFrame A shapefile defining the zones that are to be assigned Daymet records. This needs to be in the same coordinate system(WGS) as the Daymet points.
#' @param zoneField Character string of the field name describing the values that define the zones
#' @param exampleDaymetFile Character string of the file path to the daymet file used to define the spatial relationships between the shapefile and the Daymet cells

determineSpatialRelationships <- function(zonesShapefile, zoneField, exampleDaymetFile){
  
  require(dplyr)
  require(ncdf4)
  require(rgdal)
  require(raster)
  
  print("Determining spatial relationships between Daymet NetCDF grid and shapefile.")
  
  if( proj4string(zonesShapefile) != "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"){
    stop("Error in 'determineSpatialRelationships' function: The spatial relationship between the shapefile and Daymet grid cannot be determined. The shapefile must be in WGS.")
  }
  
  # Read Daymet moasaic spatial information
  # ---------------------------------------
  # Connect with the sample netCDF to 
  spatialNCDF <- nc_open(exampleDaymetFile)  #netcdf     
  
  # Latitude coordinates in full mosaic
  mosaicLat = ncvar_get ( nc    = spatialNCDF, 
                          varid = "lat", 
                          start = c(1,1), 
                          count = c(spatialNCDF$var$lat$varsize[1], 
                                    spatialNCDF$var$lat$varsize[2]) )
  
  # Longitude coordinates in full mosaic
  mosaicLon = ncvar_get ( nc    = spatialNCDF, 
                          varid = "lon", 
                          start = c(1,1), 
                          count = c(spatialNCDF$var$lon$varsize[1], 
                                    spatialNCDF$var$lon$varsize[2]) )

   
  # Calculate the indeces of the mosaic netcdf for the tile
  # -------------------------------------------------------
  # Get the extent of the shapefile
  shapeExtent <- extent(zonesShapefile)
    
  # Determine the array positions of coordinates within the shapefile extent for spatial referencing
  arrayIndeces <- which( mosaicLat >= shapeExtent@ymin & 
                         mosaicLat <= shapeExtent@ymax & 
                         mosaicLon >= shapeExtent@xmin & 
                         mosaicLon <= shapeExtent@xmax, 
                         arr.ind    = T)
    
  # Corners of the box in the array
  minRow <- min(arrayIndeces[,1])
  maxRow <- max(arrayIndeces[,1])
  minCol <- min(arrayIndeces[,2])
  maxCol <- max(arrayIndeces[,2])
    
  # Number of rows and columns
  countx = maxRow - minRow + 1 
  county = maxCol - minCol + 1 
    
  # Save the spatial references for use in other netCDF files
  mosaicIndeces <- data.frame(minRow = minRow, 
                              maxRow = maxRow, 
                              minCol = minCol, 
                              maxCol = maxCol, 
                              countx = countx, 
                              county = county)
    
  # Create a spatial object of the tile Daymet points
  # -------------------------------------------------
  # Latitude coordinates in the current tile
  shapeLat = ncvar_get( nc    = spatialNCDF, 
                        varid = "lat",     
                        start = c(minRow, minCol),    
                        count = c(countx,county) )
    
  # Longitude coordinates in the current tile
  shapeLon = ncvar_get( nc    = spatialNCDF, 
                        varid = "lon",     
                        start = c(minRow, minCol),    
                        count = c(countx,county) )
    
  # Close connection
  nc_close(spatialNCDF)
    
  # Convert coordinate to a spatial object
  shapePoints <- as.data.frame( cbind( as.vector(shapeLon), as.vector(shapeLat) ) )
  colnames(shapePoints) <- c("Longitude", "Latitude")
  shapePointsSpPts   <- SpatialPoints(shapePoints, proj4string = CRS(proj4string(zonesShapefile)))
    
  # Daymet points get assigned to polygons using 2 methods:
  # 1. A spatial join is used to determine all points fall inside of each polygon. These will eventually get spatially averaged
  # 2. Polygons that are too small to overlap any points get assigned the point that is nearest to their centroid
    
  # Spatial Join
  # ------------
  # Overlay points onto shapefile. Creates dataframe that has the zone that each point falls inside of.
  overPoints <- over(shapePointsSpPts, zonesShapefile)
    
  # Add Daymet coordinates back in to joined dataframe
  spatialJoin <- cbind(overPoints, shapePoints)
    
  # Keep only the points that fall within the polygons
  polygonPoints <- spatialJoin[which(!is.na(spatialJoin[,zoneField])),]
    
  # Nearest Point
  # -------------
  if (any(zonesShapefile@data[,zoneField] %in% polygonPoints[,zoneField] == FALSE)){
      
    # Determine the polygons that do not contain any Daymet points
    missingPolygons <- zonesShapefile[!zonesShapefile@data[,zoneField] %in% polygonPoints[,zoneField],]
      
    # Associate the missing polygon IDs with their centroids
    missingCentroids <- data.frame(missingPolygons@data[,zoneField], coordinates(missingPolygons) )
    names(missingCentroids) <- c(zoneField, 'LON', 'LAT')
      
    # List of missing polygon IDs
    missingIDs <- missingPolygons@data[,zoneField]
      
    # Create storage for Daymet point assignment
    nearPoints <- as.data.frame(matrix(nrow = length(missingIDs), ncol = 3))
    names(nearPoints) <- c(zoneField, 'Longitude', 'Latitude')
      
    # Iterate through all polygons without Daymet points and assign the nearest point
    for ( i in seq_along(missingIDs) ) {
        
      # Polygon centroid coordinates
      tempLat <- missingCentroids$LAT[missingCentroids[,zoneField] == missingIDs[i]]
      tempLon <- missingCentroids$LON[missingCentroids[,zoneField] == missingIDs[i]]
        
      # Determine the nearest Daymet point
      distances <- spDistsN1(as.matrix(shapePoints), c(tempLon, tempLat), longlat = TRUE)
      minDist <- min(distances)
      distPos <- which(distances == minDist)[1]
        
      # Enter values into dataframe
      nearPoints[i ,zoneField] <- missingIDs[i] 
      nearPoints$Longitude[i]  <- shapePoints[distPos, 1]
      nearPoints$Latitude[i]   <- shapePoints[distPos, 2]
    }
      
    # Join the two versions of point assignments
    finalPoints <- rbind(polygonPoints[,c(zoneField, 'Longitude', 'Latitude')], nearPoints)
    
    # Delete objects
    rm(missingPolygons, missingCentroids, missingIDs, nearPoints)
    
  } else{finalPoints <- polygonPoints}   
  
  # Determine the position in the sub-array
  # ----------------------------------------
  finalPoints$subRow <- NA; finalPoints$subCol <- NA
    
  for ( m in 1:nrow(finalPoints) ){
      
    # Find th position in the array of the variable by matching assigned Daymet coordinates
    position <- which(shapeLon == finalPoints$Longitude[m] & shapeLat == finalPoints$Latitude[m], arr.in = TRUE)
      
    finalPoints$subRow[m] <- as.numeric(position[,1])
    finalPoints$subCol[m] <- as.numeric(position[,2])
  }
    
  # Output the polygon ID, the assigned Daymet coordinates, and their position in the subset array of Daymet points
  shapefileIndeces <- finalPoints

  output <- list(mosaicIndeces, shapefileIndeces)
  names(output) <- c('mosaicIndeces', 'shapefileIndeces')
  
  print("Spatial relationships complete.")
  
  # Delete objects
  rm(mosaicLat, mosaicLon, shapeExtent, arrayIndeces, minRow, maxRow, minCol, maxCol, countx, county, shapePointsSpPts, overPoints, spatialJoin, polygonPoints, finalPoints, shapefileIndeces, mosaicIndeces)
  
  # Output result
  return(output)
}