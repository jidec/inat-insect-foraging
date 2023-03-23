datetimeToSolarAndSeason <- function(df,season_intervals_per_year){
    library(solartime)
    library(lubridate)
    library(dplyr)

    # add solar time
    df$solar_time <- getSolarTimeHour(as.POSIXct(df$eventDate),df$decimalLongitude)
    df <- df %>% mutate(solar_interval = cut(solar_time, breaks=24))
    levels(df$solar_interval) <- 1:24
    df$local_hour <- as.numeric(df$solar_interval)

    # add yday
    df$yday <- yday(df$eventDate)

    # 4 season intervals
    if(season_intervals_per_year == 4){
        df$season <- "1"
        df$season[df$yday >= 61 & df$yday < 153] <- "2"
        df$season[df$yday >= 153 & df$yday < 245] <- "3"
        df$season[df$yday >= 245 & df$yday < 336] <- "4"
    }

    # 8 season intervals
    if(season_intervals_per_year == 8){
        df$season <- "1"
        df$season[df$yday >= 13 & df$yday < 59] <- "2"
        df$season[df$yday >= 59 & df$yday < 106] <- "3"
        df$season[df$yday >= 106 & df$yday < 153] <- "4"
        df$season[df$yday >= 153 & df$yday < 200] <- "5"
        df$season[df$yday >= 200 & df$yday < 247] <- "6"
        df$season[df$yday >= 247 & df$yday < 294] <- "7"
        df$season[df$yday >= 294 & df$yday < 340] <- "8"
    }

    df$latitude <- df$decimalLatitude
    df$longitude <- df$decimalLongitude

    return(df)
}
