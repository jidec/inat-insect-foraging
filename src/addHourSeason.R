
# Add hours and seasons fields to iNat data

addHourSeason <- function(data){

    library(lubridate)

    #get times
    times <- data$time_observed_at
    times <- as.character(times)
    times <- as_datetime(times)

    #get hours and seasons from times
    data$utc_hour <- hour(times)
    data$est_hour <- hour(with_tz(times,"EST"))

    # Winter is December - February, Spring is March - May,
    # Summer is June - August, Fall is September - November
    data$season <- quarter(times, with_year = FALSE, fiscal_start = 12)

    return(data)
}
