# descriptive analyses and visualizations of the shape and scale of foraging data

# create foraging time plots using plotForagingTimeHists(data, species_name, yaxismax)
source("src/plotForagingTimeHists.R")
plotForagingTimeHists(usa_ants)
plotForagingTimeHists(usa_ants, "Camponotus pennsylvanicus", 1000)
plotForagingTimeHists(usa_ants, "Camponotus castaneus", 200)
plotForagingTimeHists(usa_ants, "Solenopsis invicta", 800)

#assess research grade vs non (must not filter for research in earlier lines)
nrow(filter(usa_ants, quality_grade == "research")) #55036 samples
nrow(usa_ants)#139919 samples
nrow(filter(campo_p, quality_grade == "research")) #8866 samples
nrow(campo_p) #like 10k samples

# is n correlated with nbase for species season cells?
sp_season_cell$nbase <- as.numeric(sp_season_cell$nbase)
sp_season_cell$n <- as.numeric(sp_season_cell$n)
# rats, its not! gridding is messed up somehow
cor.test(sp_season_cell$nbase,sp_season_cell$n)

g <- dplyr::filter(grid,id == 1)

# look at species season cell histograms
# warning space inefficent

# setup baseline
source("src/addHourSeasonBaseline.R")
baseline <- read.csv("data/usa_insects.csv",sep="\t")
baseline <- addHourSeasonBaseline(baseline)
baseline$latitude <- baseline$decimalLatitude
baseline$longitude <- baseline$decimalLongitude
baseline <- baseline[complete.cases(baseline$decimalLatitude),]
coordinates(baseline) <- cbind(baseline$latitude,baseline$longitude)

# setup ants
data <- usa_ants
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
x <- sp::over(baseline,grid)
baseline <- as.data.frame(baseline)
baseline$grid_id <- x$id
baseline$cell_lat <- x$x1
baseline$cell_long <- x$x2

plotSCCHist <- function(sp,se,g_id)
{
    tempd <- data
    tempb <- baseline

    if(!is.null(sp)){
    tempd <- dplyr::filter(tempd, scientific_name == sp)
    }

    if(!is.null(se)){
    tempd <- dplyr::filter(tempd, season == se)
    tempb <- dplyr::filter(tempb, season == se)
    }

    if(!is.null(g_id)){
    tempd <- dplyr::filter(tempd, grid_id == g_id)
    tempb <- dplyr::filter(tempb, grid_id == g_id)
    }

    hist(tempb$local_hour,axes=TRUE)
    #return(list(tempb$local_hour,tempd$local_hour))
    #return(hist(tempb$local_hour,axes=TRUE),hist(tempd$local_hour,axes=TRUE))
    hist(tempd$local_hour,axes=TRUE)
}

sp <- "Camponotus pennsylvanicus"
temp <- dplyr::filter(sp_season_cell, species == sp)

par(mfrow=c(1,2))
plotSCCHist("Camponotus pennsylvanicus",3,436)
