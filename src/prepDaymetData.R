
# intervals per year can be 4 or 8
# prep Daymet data downloaded by downloadDaymet and return as a dataframe
prepDaymetData <- function(season_intervals_per_year=4)
{
    # load downloaded data and bind it together
    library(dplyr)
    library(plyr)
    library(daymetr)
    library(tidyr)
    csv_files <- dir(path= "data/daymet_raw", pattern='*.csv$', recursive = F)
    csv_files <- paste0("data/daymet_raw/", csv_files)
    daymet_data <- data.frame()
    for(i in 1:length(csv_files)) {
        daymet_data <- rbind(daymet_data, read_daymet(csv_files[i],site="1"))
    }

  # nest climatic variables
  daymet_data <- daymet_data[,2:9]
  daymet_data <- daymet_data %>%
    nest(cols=c(measurement,value))

  # below - attempt at arbitrary year interval - scrapped for now
  # and hardcoded because difficult
  #days_per_interval <- as.integer(365 / intervals_per_year)

  #winter_solstice_yday <- 355
  # yday starts at 0 on Jan 1 and ends at 365 on Dec 31st
  # we want to shift it so that yday starts at 0 at the first interval
  #     centered on the winter solstice (Dec 21st) and ends at the day before that
  #first_interval_start <- winter_solstice_yday - days_per_interval/2
  #daymet_data$yday_adj <- daymet_data$yday - 12

  #for(i in 1:intervals_per_year){
  #}

  # start of 2nd interval
  #355 + ((365/8) / 2) - 365
  # days per interval
  #365 / 8

  # 4 season intervals
  if(season_intervals_per_year == 4){
      daymet_data$season <- "1"
      daymet_data$season[daymet_data$yday >= 61 & daymet_data$yday < 153] <- "2"
      daymet_data$season[daymet_data$yday >= 153 & daymet_data$yday < 245] <- "3"
      daymet_data$season[daymet_data$yday >= 245 & daymet_data$yday < 336] <- "4"
  }

  # 8 season intervals
  if(season_intervals_per_year == 8){
      daymet_data$season <- "1"
      daymet_data$season[daymet_data$yday >= 13 & daymet_data$yday < 59] <- "2"
      daymet_data$season[daymet_data$yday >= 59 & daymet_data$yday < 106] <- "3"
      daymet_data$season[daymet_data$yday >= 106 & daymet_data$yday < 153] <- "4"
      daymet_data$season[daymet_data$yday >= 153 & daymet_data$yday < 200] <- "5"
      daymet_data$season[daymet_data$yday >= 200 & daymet_data$yday < 247] <- "6"
      daymet_data$season[daymet_data$yday >= 247 & daymet_data$yday < 294] <- "7"
      daymet_data$season[daymet_data$yday >= 294 & daymet_data$yday < 340] <- "8"
  }

  # extract nested climatic variables
  extractListedData <- function(row)
  {
      return(row[7]$cols[[2]])
  }

  extracted <- apply(daymet_data,MARGIN=1,FUN=extractListedData)
  extracted <- data.frame(t(extracted))
  colnames(extracted) <- c("daylength","precip","srad","swe","tmax","tmin","vp")
  daymet_data <- cbind(daymet_data,extracted)
  daymet_data$daymet_tile <- daymet_data$tile

  return(daymet_data)
}
