prepGriddedData <- function(data, bl_data, cellsize_km){
    library(raster)

    bl_data$latitude <- bl_data$decimalLatitude
    data$latitude <- data$decimalLatitude
    bl_data$longitude <- bl_data$decimalLongitude
    data$longitude <- data$decimalLongitude

    # turn data into SpatialPoints
    bl_data$latitude <- as.numeric(bl_data$latitude)
    bl_data$longitude <- as.numeric(bl_data$longitude)
    coordinates(bl_data) <- cbind(bl_data$latitude,bl_data$longitude)
    data$latitude <- as.numeric(data$latitude)
    data$longitude <- as.numeric(data$longitude)
    coordinates(data) <- cbind(data$latitude,data$longitude)

    # create SpatialGridDataFrame from data
    grid <- makegrid(data, cellsize = cellsize_km/111) #cellsize_miles/69
    grid$id <- 1:length(grid$x1)
    coordinates(grid) <- cbind(grid$x1,grid$x2)
    gridded(grid) <- TRUE
    #plot(grid,add=TRUE)
    grid <- as(grid, "SpatialGridDataFrame")

    # spatial join and add cell info to points
    x <- sp::over(data,grid)
    data <- as.data.frame(data)
    data$grid_id <- x$id
    data$cell_lat <- x$x1
    data$cell_long <- x$x2

    # also add cell info to all obs points
    # turn sampled_bl_data data into SpatialPoints
    x <- sp::over(bl_data,grid)
    bl_data <- as.data.frame(bl_data)
    bl_data$grid_id <- x$id
    bl_data$cell_lat <- x$x1
    bl_data$cell_long <- x$x2

    return(list(data,bl_data))
}
