# install packages
install.packages("dplyr")
install.packages("lubridate")
install.packages("rgdal")
install.packages("maps")
install.packages("raster")
install.packages("RColorBrewer")
install.packages("lme4")
install.packages("ape")

#-----------------------
# PREP MAIN DATAFRAMES
#import all usa ant observations
usa_ants <- read.csv("data/usa_ants.csv")
View(usa_ants)

#add hour and season data to dataframe
source("src/addHourSeason.R")
usa_ants <- addHourSeason(usa_ants)
View(usa_ants)

#remove observations with no time observed
usa_ants <- usa_ants[!(usa_ants$time_observed_at == ""),]

#remove non research grade observations
usa_ants <- filter(usa_ants, quality_grade == "research")

#remove ants identified only to genus, not to species
usa_ants <- usa_ants[grepl(" ", usa_ants$scientific_name),]

# remove local hours at 0, 1 or 24
usa_ants <- filter(usa_ants, local_hour != 0)
usa_ants <- filter(usa_ants, local_hour != 1)
usa_ants <- filter(usa_ants, local_hour != 24)

# add distance of foraging hour from 3 pm around the hottest time of the day
usa_ants$midday_dist <- abs(usa_ants$local_hour - 15)

# add coding for day/night
usa_ants$is_night <- usa_ants$local_hour <= 6 | usa_ants$local_hour >= 21

# add local hour diff column
usa_ants$local_hour_diff <- usa_ants$local_hour - mean(usa_ants$local_hour)
# create day df with chopped off nighttimes from both sides
library(dplyr)
usa_ants_day <- filter(usa_ants, local_hour > 6)
usa_ants_day <- filter(usa_ants_day, local_hour < 20)

#filter for one common species
library(dplyr)
campo_p <- filter(usa_ants, scientific_name == "Camponotus pennsylvanicus")
campo_c <- filter(usa_ants, scientific_name == "Camponotus castaneus")
campo_a <- filter(usa_ants, scientific_name == "Camponotus americanus")
l_humile <- filter(usa_ants, scientific_name == "Linepithema humile")
p_imparis <- filter(usa_ants, scientific_name == "Prenolepis imparis")
t_immigrans <- filter(usa_ants, scientific_name == "Tetramorium immigrans")

#-----------------------
# SUMMARIZE TO SPECIES & GENUS
#summarize (condense) to species
# quick func to get mode
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
# summarize to species, with meanHour, modeHour, sdHour, n (sample size), meanMD (midday_dist), propNight,
# latCor (be careful with this, dont want to just cherry pick species)
usa_ant_species <- usa_ants %>%
    group_by(scientific_name) %>%
    summarise(meanHour = mean(local_hour), modeHour = Mode(local_hour), sdHour = sd(local_hour), n = n(), meanMD = mean(midday_dist),
              propNight = sum(is_night, na.rm = TRUE) / n(), latCor = cor(midday_dist,latitude))
usa_ant_species$latCor <- format(usa_ant_species$latCor, scientific = FALSE)
View(usa_ant_species)
usa_ant_species_n100 <- filter(usa_ant_species, n >= 100)
#plot species histograms
hist(usa_ant_species_n100$meanHour)
hist(usa_ant_species_n100$sdHour)
hist(usa_ant_species_n100$meanMD)

# similarly summarize to genus
usa_ant_genera <- usa_ants %>%
    group_by(taxon_genus_name) %>%
    summarise(meanHour = mean(local_hour), modeHour = Mode(local_hour), sdHour = sd(local_hour), n = n())

usa_ant_genera_n100 <- filter(usa_ant_genera, n >= 100)

# after start.R:
# descriptive_foraging.R
# lat_thermal_foraging.R
# color_foraging.R
# species_overlap_foraging.R


# ideas that have come up
# use covariance matrix to adjust foraging times given baseline iNat activity
# given all ants, see if other ants are found in location at time
# download baseline iNat activity and randomly sample it to create baseline

# notes
# get folks on species_overlap_foraging
#
