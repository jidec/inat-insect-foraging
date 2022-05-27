
#data <- sp_season_cell
#field <- "median_hour"
#sp <- NULL #"Camponotus pennsylvanicus"
#se <- NULL #"3"
#nthresh <- 5
#sigthresh <- 0.1

# plot a foraging time field across space  for a given species and season
plotSpeciesSeasonCells <- function(data, field, sp=NULL, se=NULL, ge=NULL, nthresh=0,sigthresh=0.1)
{
    library(rgdal)
    library(sf)
    temp <- data
    if(!is.null(sp)){
        temp <- dplyr::filter(temp, species == sp)}
    if(!is.null(se)){
        temp <- dplyr::filter(temp, season == se)}
    if(!is.null(ge)){
        temp <- dplyr::filter(temp, genus == ge)}

    temp <- dplyr::filter(temp, n >= nthresh)
    temp <- dplyr::filter(temp, anova_p <= sigthresh)
    temp <- st_as_sf(temp, coords = c("cell_lon", "cell_lat"))
    plot(temp[field],pch=15,cex =3,ylim=c(20,55),xlim=c(-130,-60),axes=TRUE)
}

# plot a foraging histogram
plotSpeciesSeasonCellHists <- function(data, sp=NULL, se=NULL,nthresh=0,sigthresh=0.1)
{
    temp <- data
    if(!is.null(sp)){
        temp <- dplyr::filter(temp, species == sp)}
    if(!is.null(se)){
        temp <- dplyr::filter(temp, season == se)}

    temp <- dplyr::filter(temp, n >= nthresh)
    temp <- dplyr::filter(temp, p <= sigthresh)

    hist(temp$median_hour)
}

#data <- usa_ants
#cellsize_miles <- 200
#sp <- "Linepithema humile"
#sp <- NULL
#se <- "3"
#g_id <-"1"

# plot a species-season-cell distribution given raw iNat data with a sp, se, and grid_id
plotSeasonCellDistribution <- function(data, cellsize_miles=200, sp=NULL, se=NULL,g_id = NULL)
{
    library(dplyr)
    library(spatialrisk)
    library(plyr)
    library(sf)
    library(sp)
    library(phenesse)

    # read in and prep usa_insects to use as observer baseline

    source("src/addHourSeasonBaseline.R")
    usa_insects <- addHourSeasonBaseline(usa_insects)
    usa_insects$latitude <- usa_insects$decimalLatitude
    usa_insects$longitude <- usa_insects$decimalLongitude
    #t <-  read.table(text=usa_insects$location,sep=',',strip.white = TRUE)
    #usa_insects$latitude <- as.numeric(unlist(t[1]))
    #usa_insects$longitude <- as.numeric(unlist(t[2]))
    usa_insects <- usa_insects[complete.cases(usa_insects$decimalLatitude),]
    coordinates(usa_insects) <- cbind(usa_insects$latitude,usa_insects$longitude)
    #usa_insects <- as.data.frame(usa_insects)

    # turn input data (probably all usa ants) into SpatialPoints
    coordinates(data) <- cbind(data$latitude,data$longitude)

    # create SpatialGridDataFrame from data
    grid <- makegrid(data, cellsize = cellsize_miles/69)
    grid$id <- 1:length(grid$x1)
    coordinates(grid) <- cbind(grid$x1,grid$x2)
    gridded(grid) <- TRUE
    plot(grid,add=TRUE)
    grid <- as(grid, "SpatialGridDataFrame")

    # spatial join and add cell info to points
    x <- sp::over(data,grid)
    data <- as.data.frame(data)
    data$grid_id <- x$id
    data$cell_lat <- x$x1
    data$cell_long <- x$x2

    # also add cell info to all obs points
    # turn sampled_usa_insects data into SpatialPoints
    #coordinates(usa_insects) <- cbind(usa_insects$latitude,usa_insects$longitude)
    x <- sp::over(usa_insects,grid)
    usa_insects <- as.data.frame(usa_insects)
    usa_insects$grid_id <- x$id
    usa_insects$cell_lat <- x$x1
    usa_insects$cell_long <- x$x2

    if(!is.null(sp)){
        data <- dplyr::filter(data, scientific_name == sp)
    }
    if(!is.null(se)){
        data <- dplyr::filter(data, season == se)
        usa_insects <- dplyr::filter(usa_insects, season == se)
    }

    if(!is.null(se)){
        data <- dplyr::filter(data, grid_id == g_id)
        usa_insects <- dplyr::filter(usa_insects, grid_id == g_id)
    }

    hist(data$local_hour,axes=TRUE)
    hist(usa_insects$local_hour,axes=TRUE)
}
