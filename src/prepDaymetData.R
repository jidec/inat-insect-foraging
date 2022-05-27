
prepDaymetData <- function(data_location)
{
  library(tidyr)
  daymet_data <- read.csv(data_location,sep=",")
  
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