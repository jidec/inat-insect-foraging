
# read in iNat data downloaded from GBIF, fix time and add some other cols
# use with both target taxon and baseline
prepGBIFiNatData <- function(data_location)
{
  #import all usa ant observations
  data <- read.csv(data_location,sep="\t")

  #add hour and season data to dataframe
  source("src/addHourSeason.R")
  data <- addHourSeasonGBIF(data)

  #remove observations with no time observed
  data <- data[!(data$eventDate == ""),]

  #remove only identified only to genus, not to species
  data <- data[grepl(" ", data$species),]

  # remove local hours at 0, 1 or 24
  data <- filter(data, local_hour != 0)
  data <- filter(data, local_hour != 1)
  data <- filter(data, local_hour != 24)

  # add distance of foraging hour from 3 pm around the hottest time of the day
  data$midday_dist <- abs(data$local_hour - 15)

  # add coding for day/night
  data$is_night <- data$local_hour <= 6 | data$local_hour >= 21

  # add local hour diff column
  data$local_hour_diff <- data$local_hour - mean(data$local_hour)

  # add lat long cols
  data$latitude <- data$decimalLatitude
  data$longitude <- data$decimalLongitude

  data$scientific_name <- data$scientificName

  return(data)
}
