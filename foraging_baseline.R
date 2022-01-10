library(dplyr)
library(spatialrisk)

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

    # add other data
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







# ------------------------------------------------------------------------------------
# WIP loop over species, another potential way of doing this
unique_species <- unique(usa_ants$scientific_name)
for(i in 1:length(unique_species)) # length(data$id)
{
    lat <- data[i,]$latitude
    lon <- data[i,]$longitude

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

