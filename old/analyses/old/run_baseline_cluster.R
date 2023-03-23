# script to run createBaselineAdjData on the cluster
# first:
# 1. upload prepped usa_ants as serialized usa_ants.Rd in data folder
# 2. upload raw GBIF csv as sampled_usa_insects.csv in data folder
# 3. upload createBaselineAdjData.R and addHourSeasonBaseline.R scripts

install.packages("stringr")
install.packages("dplyr")
install.packages("plyr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("sp")
install.packages("sf")

source("createBaselineAdjData.R")
source("addHourSeasonBaseline.R")

usa_ants <- readRDS("data/usa_ants.Rd")
sp_season_cell <- createBaselineAdjData(data=usa_ants,cellsize_miles = 200)
write.csv(sp_season_cell, "sp_season_cell.csv")

