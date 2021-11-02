
#import all usa ant observations
usa_ants <- read.csv("data/usa_ants.csv")

#add hour and season data to dataframe
source("src/addHourSeason.R")
usa_ants <- addHourSeason(usa_ants)
View(usa_ants)

#remove observations with no time observed
usa_ants <- usa_ants[!(usa_ants$time_observed_at == ""),]

#remove observations with 0 for hour
#usa_ants <- usa_ants[usa_ants$hour != 0,]

#filter for one common species
library(dplyr)
campo_p <- filter(usa_ants, scientific_name == "Camponotus pennsylvanicus")

#create foraging time plots
source("src/plotForagingTimeHists.R")
plotForagingTimeHists(usa_ants)
plotForagingTimeHists(usa_ants, "Camponotus pennsylvanicus", 1000)
plotForagingTimeHists(usa_ants, "Solenopsis invicta", 800)


# getmode function for raster to get hours of highest foraging activity in grid cells
getmode <- function(v, ...) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

#create foraging time rasters
source("src/plotForagingTimeRasters.R")
plotForagingTimeRasters(usa_ants, scientificName = "Solenopsis invicta",
                        season_num = "4", raster_res = 2, raster_function = getmode)

#quick cor test between hour and lat
cor.test(usa_ants$hour, usa_ants$latitude)

# ideas that have come up
# use covariance matrix to adjust foraging times given baseline iNat activity
# given all ants, see if other ants are found in location at time
# download baseline iNat activity and randomly sample it to create baseline
