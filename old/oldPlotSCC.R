#old plot SCC
# plot a species-season-cell distribution given raw iNat data with a sp, se, and grid_id
plotSeasonCellDistribution <- function(data, cellsize_miles=200, sp=NULL, se=NULL,g_id = NULL)
{
    library(dplyr)
    library(spatialrisk)
    library(plyr)

    temp <- data
    #cellsize_miles <- 200
    coordinates(temp) <- cbind(data$latitude,data$longitude)

    # create SpatialGridDataFrame from data

    grid <- makegrid(temp, cellsize = cellsize_miles/69)
    grid$id <- 1:length(grid$x1)
    coordinates(grid) <- cbind(grid$x1,grid$x2)
    gridded(grid) <- TRUE
    grid <- as(grid, "SpatialGridDataFrame")

    #View(grid@data)
    #grid$id == 1
    #View(grid@data)
    # with baseline data
    # read in and prep sampled_usa_insects to use as observer baseline
    #temp2 <- read.csv("data/all_usa_insects/occurrence.txt",sep = '\t',nrows=1000000)
    #temp2$t
    #temp2 <- rbind(temp2$decimalLatitude,temp2$decimalLongitude,temp2$e)
    temp2 <- read.csv("data/sampled_usa_insects1.csv")
    temp2 <- rbind(temp2,read.csv("data/sampled_usa_insects2.csv"))
    temp2 <- rbind(temp2,read.csv("data/sampled_usa_insects3.csv"))
    source("src/AddHourSeasonBaseline.R")
    temp2 <- addHourSeasonBaseline(temp2)
    t <-  read.table(text=temp2$location,sep=',',strip.white = TRUE)
    temp2$latitude <- as.numeric(unlist(t[1]))
    temp2$longitude <- as.numeric(unlist(t[2]))

    temp2 <- temp2[complete.cases(temp2),]
    coordinates(temp2) <- cbind(temp2$latitude,temp2$longitude)
    x <- sp::over(temp2,grid)

    temp2@data$grid_id <- x$id
    temp2 <- as.data.frame(temp2)

    #v <- 3330
    #temp2@data[v,c(13,22)]
    #temp2[v,c(13,22)]
    #x[v,]
    #temp2$grid_id <- x$id
    #View(temp2)

    temp2$grid_id <- as.numeric(temp2$grid_id)
    temp2 <- dplyr::filter(temp2,grid_id == g_id)
    temp2 <- dplyr::filter(temp2,season == se)

    # spatial join temp and add cell info to points
    x <- sp::over(temp,grid)
    temp@data$grid_id <- x$id
    temp <- as.data.frame(temp)

    #v <- 3330
    #temp@data[v,c(13,29)]
    #temp[v,c(13,14,29)]
    #x[v,]

    if(!is.null(sp)){
        temp <- dplyr::filter(temp, scientific_name == sp)
    }
    if(!is.null(se)){
        temp <- dplyr::filter(temp, season == se)
    }

    g_id <- 435
    temp <- dplyr::filter(temp, grid_id == g_id)

    # if the oconfidence interval for hte onset is overlapping, they are different

    # plot temp (ants)
    hist(temp$local_hour,xlim = c(0,24))

    #temp2$local_hour
    # plot temp 2 (baseline)
    hist(temp2$local_hour,xlim = c(0,24))

    temp$category = "ants"
    temp2$category = "baseline"
    stacked <- rbind.fill(temp,temp2)
    model <- aov(local_hour ~ category, data = stacked)
    return(model)
}
