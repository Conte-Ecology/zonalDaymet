#' @title Determine spatial relationships between climate records and zones
#' 
#' @description 
#' \code{determineSpatialRelationships} is an internal function that defines the spatial relationships between the netCDF files and the zones SpatialPolygonsDataFrame.
#'
#' @param zonesShapefile SpatialPolygonsDataFrame A SpatialPolygonsDataFrame of the zones which will be assigned climate records. The object should have a unique ID column and be in the WGS geographic coordinate system (same as the Daymet NetCDF files, proj4string = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0").
#' @param zoneField Character string of the field name describing the unique ID values that define the zones.
#' @param exampleMosaicFile Character string of the file path to the netCDF mosaic file used to define the spatial relationships between the shapefile and the climate record cells.

determineSpatialRelationships <- function(zonesShapefile, zoneField, exampleMosaicFile){
  
  # Check spatial reference
  if(sp::proj4string(zonesShapefile) != "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"){
    stop("Error in 'determineSpatialRelationships' function: The spatial relationship between the shapefile and Daymet grid cannot be determined. The shapefile must be in WGS.")
  } else{
  
    # Status update
    print("Determining spatial relationships between Daymet NetCDF grid and shapefile.")
    
    
    # Read Daymet moasaic spatial information
    # ---------------------------------------
    # Connect with the sample netCDF to 
    spatialNCDF <- ncdf4::nc_open(exampleMosaicFile)  #netcdf     
    
    # Latitude coordinates in full mosaic
    mosaicLat = ncdf4::ncvar_get(nc    = spatialNCDF, 
                                 varid = "lat", 
                                 start = c(1,1), 
                                 count = c(spatialNCDF$var$lat$varsize[1], 
                                           spatialNCDF$var$lat$varsize[2]))
    
    # Longitude coordinates in full mosaic
    mosaicLon = ncdf4::ncvar_get(nc    = spatialNCDF, 
                                 varid = "lon", 
                                 start = c(1,1), 
                                 count = c(spatialNCDF$var$lon$varsize[1], 
                                           spatialNCDF$var$lon$varsize[2]))
        
     
    # Calculate the indeces of the mosaic netcdf for the tile
    # -------------------------------------------------------
    # Get the extent of the shapefile
    shapeExtent <- raster::extent(zonesShapefile)
      
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
    shapeLat = ncdf4::ncvar_get(nc    = spatialNCDF, 
                                varid = "lat",     
                                start = c(minRow, minCol),    
                                count = c(countx, county))
      
    # Longitude coordinates in the current tile
    shapeLon = ncdf4::ncvar_get(nc    = spatialNCDF, 
                                varid = "lon",     
                                start = c(minRow, minCol),    
                                count = c(countx, county))
      
    # Close connection
    ncdf4::nc_close(spatialNCDF)
      
    # Convert coordinate to a spatial object
    shapePoints <- as.data.frame(cbind(as.vector(shapeLon), as.vector(shapeLat)))
    colnames(shapePoints) <- c("Longitude", "Latitude")
    shapePointsSpPts <- sp::SpatialPoints(shapePoints, 
                                          proj4string = CRS(proj4string(zonesShapefile)))
      
    
    # Assigning Records
    # =================
    # Daymet points get assigned to polygons using 2 methods:
    # 1. A spatial join is used to determine all points fall inside of each polygon. These will eventually get spatially averaged
    # 2. Polygons that are too small to overlap any points get assigned the point that is nearest to their centroid
    
    
    # Spatial Join
    # ------------
    # Overlay points onto shapefile. Creates dataframe that has the zone that each point falls inside of.
    overPoints <- sp::over(shapePointsSpPts, zonesShapefile)
      
    # Add Daymet coordinates back in to joined dataframe
    spatialJoin <- cbind(overPoints, shapePoints)
      
    # Keep only the points that fall within the polygons
    polygonPoints <- spatialJoin[which(!is.na(spatialJoin[,zoneField])),]
      
    
    # Nearest Point
    # -------------
    if (any(zonesShapefile@data[,zoneField] %in% polygonPoints[,zoneField] == FALSE)) {
        
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
        distances <- sp::spDistsN1(as.matrix(shapePoints), 
                                   c(tempLon, tempLat), 
                                   longlat = TRUE)
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
      
    } else {
      finalPoints <- polygonPoints
    }
    
    
    # Determine the position in the sub-array
    # ----------------------------------------
    finalPoints$subRow <- NA
    finalPoints$subCol <- NA
      
    for (m in 1:nrow(finalPoints)) {

      # Find th position in the array of the variable by matching assigned Daymet coordinates
      position <- which(shapeLon == finalPoints$Longitude[m] & shapeLat == finalPoints$Latitude[m], arr.in = TRUE)
        
      finalPoints$subRow[m] <- as.numeric(position[,1])
      finalPoints$subCol[m] <- as.numeric(position[,2])
    }
      
    # Output the polygon ID, the assigned Daymet coordinates, and their position in the subset array of Daymet points
    shapefileIndeces <- finalPoints
  
    output <- list(mosaicIndeces, shapefileIndeces)
    names(output) <- c('mosaicIndeces', 'shapefileIndeces')
    
    # Status update
    print("Spatial relationships complete.")
        
    # Output result
    return(output)
    
  }# End spatial reference check
}# End function