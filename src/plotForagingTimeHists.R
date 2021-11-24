
# Given a dataframe of iNat data and a scientific name
# (or NULL if using all individuals from data), plot histograms of foraging
# hours across seasons

# yMax is the y axis limit

plotForagingTimeHists <- function(data, scientificName = NULL, yMax = 10000){

    library(dplyr)
    o <- data
    if(!is.null(scientificName)){
        o <- filter(data, scientific_name == scientificName)
    }

    #winter hist of foraging times
    winter <- filter(o, season == "1")
    hist(winter$local_hour, ylim = c(0,yMax), main = paste("winter foraging hours of ", scientificName))

    #winter hist of foraging times
    spring <- filter(o, season == "2")
    hist(spring$local_hour, ylim = c(0,yMax), main = paste("spring foraging hours of ", scientificName))


    summer <- filter(o, season == "3")
    hist(summer$local_hour, ylim = c(0,yMax), main = paste("summer foraging hours of ", scientificName))

    fall <- filter(o, season == "4")
    hist(fall$local_hour, ylim = c(0,yMax), main = paste("fall foraging hours of ", scientificName))
}
