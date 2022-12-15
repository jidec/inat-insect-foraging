
# get observation hours
source("src/estimateSpeciesSeasonCellForaging.R")
obs1 <- getObsOnly(usa_bfs, usa_insects, cellsize_km=200, min_per_cell_n=100,sample_cutoff_n=100000000000, floor_hours=F, first_n=1000)
obs2 <- getObsOnly(usa_bfs, usa_bfs, cellsize_km=200, min_per_cell_n=100,sample_cutoff_n=100000000000, floor_hours=F, first_n=100)
#obs1 <- readRDS("data/saved_rds/obs1")

source("src/getHrsTupleFromObs.R")
hrs <- getHrsTupleFromObs(obs1)
sc_hrs <- hrs[[2]]
ssc_hrs <- hrs[[1]]

source("src/padBinFreqHours.R")
source("src/getSpExpecDiff.R")
ssc_freqs <- padBinFreqHours(ssc_hrs[[1]])
sc_freqs <- padBinFreqHours(sc_hrs[[1]])

# fun to apply the previous
getSpExpecDiffFromObs <- function(i,nbins=-1,obs){
    hrs <- getHrsTupleFromObs(obs)
    sc_hrs <- hrs[[2]]
    ssc_hrs <- hrs[[1]]
    getSpExpecDiff(padBinFreqHours(ssc_hrs[[i]],nbins),padBinFreqHours(sc_hrs[[i]],nbins),TRUE)
}

# doing it all in one line
#getSpExpecDiff(padBinFreqHours(ssc_hrs[[i]],nbins=8),padBinFreqHours(sc_hrs[[i]],nbins=8),plot=T)
# what is different about observations of the species from observations in the season cell
getSpExpecDiffFromObs(1,obs=obs1)

source("src/addNewForagingMetricsFromObs.R")
obs1 <- addNewForagingMetricsFromObs(obs1,nbins=6,start_trim=1,end_trim=1)
View(obs1[[2]])
#findpeaks(diff, npeaks=2, threshold=4, sortstr=TRUE)

# trying predicting a diffs metric
obs1[[2]]$species <- as.character(obs1[[2]]$species)
model <- lm(d_half1_sum ~ cell_lat, data=obs1[[2]])
summary(model)

# plot pc1
pc1_rot <- c(0.43,-0.62,-0.44,0.39,0.24)
plot(pc1_rot)

pc2_rot <- c(0.02,-0.21,0.37,0.53,-0.72)
plot(pc2_rot)

# interesting thing - sum diffs is sig greater at lower lats
