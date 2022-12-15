# code for Jamm

#saveRDS(bf_hours,file="bf_hours")
#saveRDS(bf_bl_hours,file="bf_bl_hours")

# bf_hours is a list of vectors,
#   each vector containing all observation hours for one butterfly species-season-cell combination
# bf_bl_hours is a list of vectors,
#   each vector containing all observation hours for one
#   season-cell combination (all insect species, thus the baseline)
# these match up, so bf_bl_hours[[1]] is the baseline of the season cell in which bf_hours[[1]] was recorded

# load shared data
bf_bl_hours <- readRDS("bf_bl_hours")
bf_hours <- readRDS("bf_hours")

# plot raw hours
hist(bf_bl_hours[[2]],xlim=c(0,24))
hist(bf_hours[[2]],xlim=c(0,24))

length(bf_hours[[1]])
bf_hours[[1]]
length(bf_bl_hours[[1]])

hist(usa_insects$local_hour)

length(bf_hours[[1]]) / length(usa_insects$local_hour)
