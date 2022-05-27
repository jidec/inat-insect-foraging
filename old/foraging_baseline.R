library(dplyr)
library(spatialrisk)
library(plyr)

saveRDS(usa_ants,"usa_ants.Rd")
# read in and prep sampled_usa_insects to use as observer baseline
sampled_usa_insects <- read.csv("data/sampled_usa_insects1.csv")
sampled_usa_insects <- rbind(sampled_usa_insects,read.csv("data/sampled_usa_insects2.csv"))
sampled_usa_insects <- rbind(sampled_usa_insects,read.csv("data/sampled_usa_insects3.csv"))
source("src/AddHourSeasonBaseline.R")
sampled_usa_insects <- addHourSeasonBaseline(sampled_usa_insects)
t <-  read.table(text=sampled_usa_insects$location,sep=',',strip.white = TRUE)
sampled_usa_insects$latitude <- as.numeric(unlist(t[1]))
sampled_usa_insects$longitude <- as.numeric(unlist(t[2]))
sampled_usa_insects <- sampled_usa_insects[complete.cases(sampled_usa_insects),]

# start with miles range and convert to meters
range <- 150 #miles
range <- range * 1609.34 #meters

# pick data to use (usa_ants or usa_ants filtered for a species probably)
adj_ants <- usa_ants
length(adj_ants$id)

# Loop over every individual ant
# Get all insects within 150 miles collected in the same season
# Save as local_hour_adj the difference between individual ant observation hour
#   and the mean observation hour of all insects in range
#
# NOTE - takes around 3 hours for all ants
for(i in 1:length(adj_ants$id))
{
    lat <- adj_ants[i,]$latitude
    lon <- adj_ants[i,]$longitude

    # create df to use for in_range
    df <- data.frame(cbind(sampled_usa_insects$latitude,sampled_usa_insects$longitude,
                           sampled_usa_insects$local_hour,sampled_usa_insects$season))
    # filter for only those with same season as observation
    df <- dplyr::filter(df, season == data[i,]$season)
    colnames(df) <- c('lon','lat','local_hour','season')

    # get all sampled usa insects in range
    in_range <- spatialrisk::points_in_circle(df, lat, lon, radius = 2.41e5)

    # calculate adjusted hour
    adj_ants$local_hour_adj[i] <- data$local_hour[i] - mean(in_range$local_hour)
    # drop point presenmces and background

    # add other datasample data poimnts proportional to backgorund ocrurance
    #adj_usa_ants$id[i] <- data$id[i]
    #adj_usa_ants$species[i] <- data$scientific_name[i]
    #adj_usa_ants$season[i] <- data$season[i]
    #adj_usa_ants$latitude[i] <- data$latitude[i]
    #adj_usa_ants$longitude[i] <- data$longitude[i]

    # print progress every 100 observations
    if(i%%100 == 0)
    {
        print(i)
    }
}
View(adj_ants)

# adjusted foraging distribution = distribution of observations (vector) - mean observation hour of all insects in area around coord (vector)
#   + mean observation hour of all insects in all locations (scalar, use to approximately get back to time of day measurement)
#   OR should this be + mean observation hour of all ants in all locations
# adjusted foraging distribution for a species is then the difference between the observed distribution and spatial patterns of observations
#   this accounts for the potential error of spatial patterns in observations driving the results

# add adjusted_bm (adjusted baseline mean) and adjusted_am (ant mean)
adj_usa_ants$local_hour_adj_bm <- mean(sampled_usa_insects$local_hour,na.rm = TRUE) + adj_usa_ants$local_hour_adj
adj_usa_ants$local_hour_adj_am <- mean(usa_ants$local_hour) + adj_usa_ants$local_hour_adj

adj_usa_ants$midday_dist <- abs(adj_usa_ants$local_hour_adj_bf - 15)

# plot unadjusted local hour for a species
f <- dplyr::filter(usa_ants, scientific_name == "Camponotus pennsylvanicus")
hist(f$local_hour, breaks = 25, xlim = c(0,24))

# plot adjusted local hour to compare
f <- dplyr::filter(adj_usa_ants, species == "Camponotus pennsylvanicus")
hist(f$local_hour_adj_bf, breaks = 25, xlim = c(0,24))

# bin strictly

# could try 70-80-90-100

# ------------------------------------------------------------------------------------
#

# OR instead of aggregating is every individual weighted by the species-cell-season combination
library(plyr)
# read in and prep sampled_usa_insects to use as observer baseline
sampled_usa_insects <- read.csv("data/sampled_usa_insects1.csv")
sampled_usa_insects <- rbind(sampled_usa_insects,read.csv("data/sampled_usa_insects2.csv"))
sampled_usa_insects <- rbind(sampled_usa_insects,read.csv("data/sampled_usa_insects3.csv"))
source("src/AddHourSeasonBaseline.R")
sampled_usa_insects <- addHourSeasonBaseline(sampled_usa_insects)
t <-  read.table(text=sampled_usa_insects$location,sep=',',strip.white = TRUE)
sampled_usa_insects$latitude <- as.numeric(unlist(t[1]))
sampled_usa_insects$longitude <- as.numeric(unlist(t[2]))
sampled_usa_insects <- sampled_usa_insects[complete.cases(sampled_usa_insects),]

# turn ants data into SpatialPoints
data <- usa_ants #dplyr::filter(usa_ants, scientific_name == "Camponotus castaneus")
coordinates(data) <- cbind(data$latitude,data$longitude)

# create SpatialGridDataFrame from data
cellsize_miles <- 150
grid <- makegrid(data, cellsize = cellsize_miles/69)
grid$id <- 1:length(grid$x1)
coordinates(grid) <- cbind(grid$x1,grid$x2)
gridded(grid) <- TRUE
plot(grid,add=TRUE)
grid <- as(grid, "SpatialGridDataFrame")
#plot(grid$x1,grid$x2) #,proj4string ="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# spatial join and add cell info to points
x <- sp::over(data,grid)
data <- as.data.frame(data)
data$grid_id <- x$id
data$cell_lat <- x$x1
data$cell_long <- x$x2
#data$grid_id <- as.factor(data$grid_id)
#data$grid_id <- as.numeric(data$grid_id)

#also add cell info to all obs points
# turn sampled_usa_insects data into SpatialPoints
coordinates(sampled_usa_insects) <- cbind(sampled_usa_insects$latitude,sampled_usa_insects$longitude)
x <- sp::over(sampled_usa_insects,grid)
sampled_usa_insects <- as.data.frame(sampled_usa_insects)
sampled_usa_insects$grid_id <- x$id
sampled_usa_insects$cell_lat <- x$x1
sampled_usa_insects$cell_long <- x$x2
#sampled_usa_insects$grid_id <- as.factor(sampled_usa_insects$grid_id)
#sampled_usa_insects$grid_id <- as.numeric(sampled_usa_insects$grid_id)

unique_cells <- unique(data$grid_id)
length(unique_cells)
length(unique(usa_ants$scientific_name))
out <- data.frame(matrix(ncol = 9, nrow = 220))
colnames(out) <- c("species","cell_id","season","p","f","n","median_hour","cell_lat","cell_long")
index <- 1
for(i in 1:length(unique_cells)) # length(data$id)
{
    #i = 17
    # filter for obs in cell
    cell_obs <- dplyr::filter(data,grid_id == unique_cells[i])
    baseline_cell_obs <- dplyr::filter(sampled_usa_insects,grid_id == unique_cells[i])
    species_in_cell <- unique(cell_obs$scientific_name)

    for(k in 1:length(species_in_cell))
    {
        #k = 1
        # filter for obs of species
        species_obs <- dplyr::filter(cell_obs,scientific_name == species_in_cell[k])
        sp <- species_in_cell[k]
        for(j in 1:4)
        {
            #j = 3
            season_obs <- dplyr::filter(species_obs, season == seasons[j])
            baseline_season_obs <- dplyr::filter(baseline_cell_obs, season == seasons[j])

            # compare histograms
            #hist(season_obs$local_hour, xlim = c(0,24))
            #hist(baseline_season_obs$local_hour, xlim = c(0,24))
            #print(length(season_obs$id) > 0 && length(baseline_season_obs$id) > 0)
            if(length(season_obs$id) > 1 && length(baseline_season_obs$id) > 1)
            {
                season_obs$category = "ants"
                baseline_season_obs$category = "baseline"
                stacked <- rbind.fill(season_obs,baseline_season_obs)
                model <- aov(local_hour ~ category, data = stacked)

                # get ant onset, gtet insect onset
                #sp <- season_obs$scientific_name[3] #species name
                p <- summary(model)[[1]][1,5] # p value
                f <- summary(model)[[1]][1,4] # f statistic

                # add new row to output
                print(index)
                #print(c(sp,i,j,p,f,length(season_obs$id)))
                out[index,] <- c(sp,i,j,p,f,length(season_obs$id),median(season_obs$local_hour),season_obs$cell_lat,season_obs$cell_long)
                index <- index + 1
            }
        }
    }
}
out$n <- as.numeric(out$n)
out$p <- as.numeric(out$p)
out$cell_lat <- as.numeric(out$cell_lat)
out$cell_lon <- as.numeric(out$cell_lon)
out$median_hour <- as.numeric(out$median_hour)
out$midday_dist <- abs(out$median_hour - 15)
library(stringr)
out$genus <- str_split_fixed(out$species, " ", n=2)[,1]

# question - whats a good number of obs to establish a decent species estimate for a season cell?
#

nthresh <- 0
sigthresh <- 0.1

out$sig <- out$p < sigthresh
out$highf <- out$f > 9

sp_season_cell <- out

908/3600
629/3600
#sp_season_cell <- dplyr::filter(scientific_, n >= nthresh)
sp_season_cell <- dplyr::filter(sp_season_cell, n >= nthresh)
sp_season_cell <- dplyr::filter(sp_season_cell, sig == TRUE)

sp_season_cell <- dplyr::filter(sp_season_cell, genus == "Camponotus")
sp_season_cell <- dplyr::filter(sp_season_cell, species == "Camponotus pennsylvanicus")

sp_season_cell <- dplyr::filter(sp_season_cell, species == "Prenolepis imparis")
sp_season_cell <- dplyr::filter(sp_season_cell, species == "Prenolepis imparis")

sp_season_cell

#sp_season_cell <- dplyr::filter(sp_season_cell, highf == TRUE)

# onset and offset
# 1% quantile

# secure shell login to bear.flmnh.ufl.edu
# R studio server -
# secure shell login to moose.flmnh.ufl.edu

# lat significance varies, seasonal sig pretty much disappears

# n 5 sig 0.05 - insig


model <- lm(midday_dist ~ season + cell_lat, data = sp_season_cell)
summary(model)

model <- lm(midday_dist ~ season + latitude, data = usa_ants)
summary(model)
# n 5 sig 0,05 - yep
# n 5 sig 0.1 - sorta

model <- lm(median_hour ~ season + cell_lat, data = sp_season_cell)
summary(model)

model <- lm(midday_dist ~ season + cell_lat, data = sp_season_cell)
summary(model)

# what does it mean if sig species season cells are insig?
# observational effects are probably driving the latitude midday dist signal

# very sensitive to sample size and sig threshold

# mixed effects for this model - nested taxonomy,

View(sp_season_cell)
hist(sp_season_cell$median_hour)
plot(sp_season_cell$cell_lat,sp_season_cell$cell_lon)

library(rgdal)

sp <- "Camponotus castaneus"
se <- 3
# add back to grid
plotSpeciesSeason <- function(sp, se)
{
    temp <- sp_season_cell
    temp <- dplyr::filter(temp, species == sp)
    temp <- dplyr::filter(temp, season == se)
    temp$median_hour <- as.numeric(temp$median_hour)
    temp <- temp %>%
        group_by(cell_id) %>%
        summarise(value = mean(midday_dist))
    g <- grid
    View(g)
    # get value of sp_season_cell for each grid cell
    # get values of median hour where grid_id matches cell_id
    g$value <- as.numeric(temp$value[match(grid$id,temp$cell_id)])
    View(g)
    return(plot(g["value"], axes=TRUE,xlim = c(15,70),ylim= c(-160,-140)))
    shape <- readOGR(dsn = "data/cb_2018_us_state_5m")
    plot(shape, bg="transparent", add=TRUE)
}

plot(g["value"], axes=TRUE,xlim = c(15,70),ylim= c(-160,-140))

plot(meuse.grid["dist"], zlim = c(0,1))
plot(g['median_hour'],zlim = c(0,1))
g <- as("SpatialGrid",g)
plotSpeciesSeason(sp = "Camponotus pennsylvanicus", se = 2)

sp_season_cell <- sp_season_cell[complete.cases(sp_season_cell),]
m <- SpatialPixelsDataFrame(points = sp_season_cell[c("cell_lat", "cell_lon")], data = sp_season_cell)
plot(m)
View(sp_season_cell)
sp_season_cell$median_hour[match(grid$id,sp_season_cell$cell_id)]

    # get all sampled usa insects in range
    df <- data.frame(cbind(sampled_usa_insects$latitude,sampled_usa_insects$longitude,
                           sampled_usa_insects$local_hour,sampled_usa_insects$season))
    colnames(df) <- c('lon','lat','local_hour','season')

    in_range <- spatialrisk::points_in_circle(df, lat, lon, radius = 2.41e5)

    # for each season
    seasons <- c("1","2","3","4")

    for(s in 1:length(seasons))
    {
        # filter for season
        in_range_season <- dplyr::filter(in_range, season == seasons[s])
        # set local_hour_adj to be difference between local hour and the mean of local hours in range
        #print(i*4 + s)
        adj_usa_ants$local_hour_adj[i*4 + s] <- data$local_hour[i] - mean(in_range_season$local_hour)
        adj_usa_ants$id[i*4 + s] <- data$id[i]
        adj_usa_ants$species[i*4 + s] <- data$scientific_name[i]
        adj_usa_ants$species[i*4 + s] <- data$scientific_name[i]
        adj_usa_ants$species[i*4 + s] <- data$scientific_name[i]
        adj_usa_ants$season[i*4 + s] <- s
        adj_usa_ants$latitude[i*4 + s] <- data$latitude[i]
        adj_usa_ants$longitude[i*4 + s] <- data$longitude[i]
    }

    if(i%%100 == 0)
    {
        print(i)
    }
}
View(adj_usa_ants)

# within radius curve for species X
# how to get onset offset?
#
# if observers out late in season,

# feed in 4 points per species, 1 per season
# insights from species distribtuion modeling - bias layer -
# use bias map to select bcakkground - sampling by density - accounting for that bias
# if looking at acitivty pattern and see species active from 10 to 4 in that cell and look at background data whihc is all insects and all insects are sampled 10-4
# if 6-4

# OLD RASTER ATTEMPT
# foraging baseline
# create raster containing mean and variance in ant foraging times
# create raster containing mean and variance in insect foraging times
# rasterize ant foraging time mean
data <- usa_ants
raster_res <- 10
r <- raster(data, resolution = raster_res) #2
r <- rasterize(data,r, field = data$local_hour, fun = mean)

# rasterize insect foraging mean
data <- sampled_insects
raster_res <- 10
r2 <- raster(data, resolution = raster_res) #2
r2 <- rasterize(data,r, field = data$local_hour, fun = mean)

# get difference in foraging time means
r3 <- r-r2

# rasterize ant foraging variances
data <- usa_ants
raster_res <- 10
r <- raster(data, resolution = raster_res) #2
r <- rasterize(data,r, field = data$local_hour, fun = var)

# rasterize insect foraging variances
data <- sampled_insects
raster_res <- 10
r2 <- raster(data, resolution = raster_res) #2
r2 <- rasterize(data,r, field = data$local_hour, fun = var)

# get difference in foraging times variances
r3 <- r-r2

# unsummarize raster to data frame?
# each data point is a single observation adjusted using the insect baseline in its cell
# could also assign cell ids to each observation
# for each data point, grab all insects within a lat long range, and set the bl_adj_local_hour
library(sp)
test <- SpatialG
x <- GridTopology(cellcentre.offset = c(0,0), cellsize = c(2.17,2.17), cells.dim = c(25,25))
data(meuse.grid)
summary(meuse.grid)
coords <- cbind(usa_ants$latitude,usa_ants$longitude)
y <- getGridIndex(coords, x, all.inside = TRUE)
class(x)
x
summary(x)
coordinates(x)

gtopo <- GridTopology(c(0,0), c(1,1), c(5,5)) # create the grid
datafr <- data.frame(runif(25)) # make up some data
SpGdf <- SpatialGridDataFrame(gtopo, c(1:1000)) #, datafr) # create the grid data frame
summary(SpGdf)

data(meuse.grid) # only the non-missing valued cells
View(meuse.grid)
coordinates(meuse.grid) = c("x", "y") # promote to SpatialPointsDataFrame

gridded(meuse.grid) <- TRUE # promote to SpatialPixelsDataFrame
meuse.grid
x = as(meuse.grid, "SpatialGridDataFrame") # creates the full grid
View(x)
library(sf)

meuse_sf <- st_as_sf(meuse, coords = c("x", "y"))
meuse_sf
scc_sf
scc_sf <- st_as_sf(sp_season_cell, coords = c("cell_lat", "cell_lon"))
plot(meuse_sf)
plot(scc_sf)
View(my_sf_object)
plot(my_sf_object["media"]
