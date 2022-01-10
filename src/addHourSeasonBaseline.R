
# Add hours and seasons fields to iNat data

addHourSeasonBaseline <- function(data){

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
    out <- data
    out$local_hour <- hour(with_tz(out$datetimes,out$observed_time_zone))

    # add col for season
    # Winter is December - February, Spring is March - May,
    # Summer is June - August, Fall is September - November
    out$season <- quarter(out$datetimes, with_year = FALSE, fiscal_start = 12)
    out$season <- as.factor(out$season)
    #usa_ants$season <- as.factor(usa_ants$season)

    return(out)
}
