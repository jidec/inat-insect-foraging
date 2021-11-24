
# Add hours and seasons fields to iNat data

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
    out$season <- quarter(out$datetimes, with_year = FALSE, fiscal_start = 12)
    out$season <- as.factor(out$season)

    return(out)
}
