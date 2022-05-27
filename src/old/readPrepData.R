
readPrepBaselineInsects <- function(sample=-1)
{
    usa_insects <- read.csv("data/usa_insects.csv",sep="\t")
    source("src/addHourSeason.R")
    #source("src/addDaymetCellData.R")

    usa_insects$latitude <- usa_insects$decimalLatitude
    usa_insects$longitude <- usa_insects$decimalLongitude
    usa_insects <- usa_insects[complete.cases(usa_insects$decimalLatitude),]

    usa_insects <- addHourSeasonBaseline(usa_insects)

    if(sample != -1){
        usa_insects <- usa_insects[sample(nrow(usa_insects), sample), ]
    }

    return(usa_insects)
    #usa_insects <- addDaymetTileIDs(usa_insects,daymet_150m)
}

readPrepDaymetData <- function()
{
    library(tidyr)
    daymet_data <- read.csv("data/daymet_150m.csv",sep=",")

    daymet_data <- daymet_data[,2:10]
    daymet_data <- daymet_data %>%
        nest(cols=c(measurement,value))

    daymet_data$season <- rep(0,nrow(daymet_data))
    daymet_data[daymet_data$yday > 0 & daymet_data$yday < 91,9] <- 1
    daymet_data[daymet_data$yday > 91 & daymet_data$yday < 91*2,9] <- 2
    daymet_data[daymet_data$yday > 91*2 & daymet_data$yday < 91*3,9] <- 3
    daymet_data[daymet_data$yday > 91*3 & daymet_data$yday < 91*4,9] <- 4

    return(daymet_data)
}

readPrepAntData <- function()
{
    #import all usa ant observations
    usa_ants <- read.csv("data/usa_ants.csv")

    #add hour and season data to dataframe
    source("src/addHourSeason.R")
    usa_ants <- addHourSeason(usa_ants)

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

    return(usa_ants)
}

# must be a better way to do this stupid operation, this takes 3 hours
createYearSeasonCellDaymetData <- function(data)
{
    len <- nrow(data)

    data$daylength <- rep(NA,len)
    data$precip <- rep(NA,len)
    data$solar_rad <- rep(NA,len)
    data$temp_max <- rep(NA,len)
    data$temp_min <- rep(NA,len)
    data$vapor_pressure <- rep(NA,len)

    for(i in 1:len)
    {
        data[[8]][[i]][2]$value
        data$daylength[i] <- data[[8]][[i]][2]$value[1]
        data$precip[i] <- data[[8]][[i]][2]$value[2]
        data$solar_rad[i] <- data[[8]][[i]][2]$value[3]
        data$temp_max[i] <- data[[8]][[i]][2]$value[5]
        data$temp_min[i] <- data[[8]][[i]][2]$value[6]
        data$vapor_pressure[i] <- data[[8]][[i]][2]$value[7]
        if(i %% 1000 == 0){ print(i)}
    }

    year_season_cell <- group_by(data, year, season, tile)
    df <- summarise(year_season_cell, avg = mean(daylength))
}

readPrepGBIFData <- function(data_location)
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
