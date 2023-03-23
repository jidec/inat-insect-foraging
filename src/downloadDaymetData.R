
# could potentially add capacity to download multiple years and label accordingly
downloadSaveDaymetData <- function(cellsize_km){
    # prep inputs, download daymet data and merge the results into daymet.csv
    library(daymetr)

    # get only contiguous US tiles
    usa_tiles <- tile_outlines # from daymetr
    usa_tiles <- cbind(usa_tiles$TileID,usa_tiles$XMin,usa_tiles$XMax,usa_tiles$YMin,usa_tiles$Ymax)
    colnames(usa_tiles) <- c("id", "XMin","XMax", "YMin","YMax")
    usa_tiles <- as.data.frame(usa_tiles)
    library(dplyr)
    usa_tiles <- filter(usa_tiles, YMin > 24.396 & YMin < 49.38) #& YMax < -49.384 & XMin > -124.848 &)
    usa_tiles <- filter(usa_tiles, XMin > -124.84 & XMin < -66.88)
    tile_ids <- usa_tiles$id

    # set lat and long to the center of each tile
    usa_tiles$X <- (usa_tiles$XMax + usa_tiles$XMin) / 2
    usa_tiles$Y <- (usa_tiles$YMax + usa_tiles$YMin) / 2
    library(sp)
    coordinates(usa_tiles) <- cbind(usa_tiles$X,usa_tiles$Y)

    # create SpatialGridDataFrame from the usa_tiles
    grid <- makegrid(usa_tiles, cellsize = cellsize_km / 110.574) #cellsize_miles / 69
    grid$id <- 1:nrow(grid)
    coordinates(grid) <- cbind(grid$x1,grid$x2)
    gridded(grid) <- TRUE
    grid <- as(grid, "SpatialGridDataFrame")

    # make daymet_raw folder
    dir.create(file.path("data", "daymet_raw"), showWarnings = FALSE)
    dir.create(file.path("data/daymet_raw", "points"), showWarnings = FALSE)

    # write to .csv to feed into daymet func
    points <- cbind(grid$id,grid$x2,grid$x1)
    colnames(points) <- c("site","latitude","longitude")
    points <- as.matrix(points)
    write.csv(points,"data/daymet_raw/points/points_for_daymet.csv",row.names = FALSE)

    # download 16500 (max) tiles from these points
    download_daymet_batch(
        file_location = "data/daymet_raw/points/points_for_daymet.csv",
        start = 2020,
        end = 2021,
        internal = FALSE,
        path = "data/daymet_raw",
        simplify = FALSE
    )

    print("Finished downloading Daymet")
}

