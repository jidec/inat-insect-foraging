# Script to fetch all World Clim data to .csv file using coordinates
# Neeka Sewnath
# nsewnath@ufl.edu

# Import packages
library(raster)
library(dplyr)
library(sp)

# Read CSV file
data <- read.csv("data/usa_ants.csv")

# Import WorldClim data
world_clim <- getData("worldclim", var = "bio", res = 10)

# Rename all 19 variable names 
names(world_clim) <- c("mean_temp", "mean_diurnal_range", "isothermality",
                       "temp_seasonality", "max_temp_warmest_month", 
                       "min_temp_coldest_month", "temp_annual_range", 
                       "mean_temp_wettest_qtr", "mean_temp_driest_qtr", 
                       "mean_temp_warmest_qtr", "mean_temp_coldest_qtr", 
                       "annual_precip", "precip_wettest_month", 
                       "precip_driest_month", "precip_seasonality", 
                       "precip_wettest_qtr", "precip_driest_qtr", 
                       "precip_warmest_qtr", "precip_coldest_qtr")

# Use coordinate data to make WorldClim extracts
lat <- c(data$latitude)
long <- c(data$longitude)

coords <- data.frame(x = long, y = lat)
points <- SpatialPoints(coords, proj4string = r@crs)

extract <- extract(world_clim, points)

# Input WorldClim data into df
data_with_wc <- cbind.data.frame(coordinates(points),extract)

data_with_wc <- data_with_wc %>%
  rename(longitude = x, latitude = y)

new_df <- right_join(data,data_with_wc,by=c("latitude" = "latitude", 
                                            "longitude" = "longitude"))

# Write new df 
write.csv(new_df, "data/usa_ants_with_wc.csv")
