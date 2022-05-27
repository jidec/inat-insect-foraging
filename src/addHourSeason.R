
# add hours and seasons fields to iNat data

addHourSeason <- function(data){

    library(lubridate)
    library(dplyr)

    #get times
    times <- data$time_observed_at
    times <- as.character(times)
    times <- as_datetime(times)
    data$datetimes <- round_date(times, "hour")

    # add col for utc hour
    data$utc_hour <- hour(data$datetimes)

    # add col for local hour using time zone
    out <- filter(data, time_zone == "Eastern Time (US & Canada)")
    out$local_hour <- hour(with_tz(out$datetimes,"America/New_York"))

    temp <- filter(data, time_zone == "Central Time (US & Canada)")
    temp$local_hour <- hour(with_tz(temp$datetimes,"America/Knox_IN"))
    out <- rbind(out, temp)

    temp <- filter(data, time_zone == "Mountain Time (US & Canada)")
    temp$local_hour <- hour(with_tz(temp$datetimes,"America/Denver"))
    out <- rbind(out, temp)

    temp <- filter(data, time_zone == "Pacific Time (US & Canada)")
    temp$local_hour <- hour(with_tz(temp$datetimes,"America/Los_Angeles"))
    out <- rbind(out, temp)

    # add col for season
    # Winter is December - February, Spring is March - May,
    # Summer is June - August, Fall is September - November
    out$season <- lubridate::quarter(out$datetimes, with_year = FALSE, fiscal_start = 12)
    out$season <- as.factor(out$season)
    #usa_ants$season <- as.factor(usa_ants$season)

    return(out)
}

# add hour season to GBIF formatted research-grade iNaturalist observations
addHourSeasonGBIF <- function(data)
{
    library(lubridate)
    library(dplyr)

    #get times
    times <- data$eventDate
    times <- as.character(times)
    times <- as_datetime(times)
    data$datetimes <- round_date(times, "hour")

    # add col for local_hour
    data$local_hour <- hour(data$datetimes)

    # add col for season
    # Winter is December - February, Spring is March - May,
    # Summer is June - August, Fall is September - November
    data$season <- lubridate::quarter(data$datetimes, with_year = FALSE, fiscal_start = 12)
    data$season <- as.factor(data$season)

    return(data)
}


# Add hours and seasons fields to iNat data

addHourSeasonBaseline <- function(data){

    library(lubridate)
    library(dplyr)

    out <- data
    #get times
    times <- out$eventDate
    times <- as_datetime(times)
    out$datetimes <- round_date(times, "hour")

    # add col for local hour
    out$local_hour <- hour(out$datetimes)

    # add col for season
    # Winter is December - February, Spring is March - May,
    # Summer is June - August, Fall is September - November
    out$season <- lubridate::quarter(out$datetimes, with_year = FALSE, fiscal_start = 12)
    out$season <- as.factor(out$season)

    out$year <- year(out$datetimes)
    out$yday <- yday(out$datetimes)

    out <- dplyr::filter(out, local_hour != 0)
    #usa_ants$season <- as.factor(usa_ants$season)

    return(out)
}
