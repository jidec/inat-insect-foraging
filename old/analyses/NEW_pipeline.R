# OPTIONS
# 1. whether to split season by 4 or 8
# 2. whether or not to filter out butterflies (what to use as baseline)
# 3. size of grid cells for SSCs
# 4. threshold of n obs to keep an SSC
# 5. how to bin and trim for difference metrics

# prepare Daymet data by adding seasons and formatting correctly
# OPTION - whether to split season by 4 or 8
source("src/prepDaymetData.R")
daymet_data <- prepDaymetData("data/daymet.csv",season_intervals_per_year=8)
# read in prepped USA butterflies and all_insects
usa_bfs <- readRDS("usa_bfs.rds")
usa_insects <- readRDS("usa_insects.rds")

# OPTION - whether or not to filter out butterflies, to not filter out bflies, or to use all organisms
# remove butterflies from all_insects
# rationale here is that leaving butterflies in the baseline
#   will pollute it because butterflies in the baseline have the most REAL signal within them
# recall that the goal of the baseline is to capture observer behavior with as little
#   REAL foraging signal as possible
usa_insects <- filter(usa_insects, family != "Hesperiidae" &
                   family != "Hedylidae" &
                   family != "Lycaenidae" &
                   family != "Nymphalidae" &
                   family != "Papilionidaee" &
                   family != "Pieridae" &
                   family != "Riodinidae")

# convert local time to solar time
source("src/localTimeToSolar.R")
usa_bfs <- localTimeToSolar(usa_bfs)
usa_insects <- localTimeToSolar(usa_insects)

# OPTION - size of grid cells for SSCs
# grid up both clade and baseline observations and return them as a tuple
# note that obs must have closest daymet tile attached previously
source("src/prepGriddedData.R")
gridded_tuple <- prepGriddedData(usa_bfs,usa_insects,cellsize_km=250)
rm(usa_bfs)
rm(usa_insects)

# OPTION - threshold to keep an SSC
# create species-season-cells with a given min num obs required to keep an SSC
source("src/createSSCs.R")
sscs <- createSSCs(gridded_tuple,daymet_data,min_per_cell_n=30)

rm(daymet_data)
rm(gridded_tuple)

# add metrics generated using the Weibull fitting & onset/offset approach
source("src/addWeibullMetricsToSSCs.R")
sscs <- addWeibullMetricsToSSCs(sscs)

# add metrics generated using the differences between the clade and the baseline
# OPTION - how to bin and trim for difference metrics
source("src/addDiffMetricsToSSCs.R")
sscs <- addDiffMetricsToSSCs(sscs,nbins=8,start_trim = -1,end_trim=-1)

# merge leptraits data (wing size, habitat preference)
source("src/mergeLeptraitsToSCCs.R")
sscs <- mergeLeptraitsToSSCs(sscs)

saveRDS(sscs, "data/saved_rds/last_sscs.rds")

# plot PCs taken from print statement
pc1 <- c(0.0749,0.273,-0.40949,-0.661,0.1964,0.52)
pc2 <- c(-0.006,0.1294,0.77,-0.5566,-0.2739,-0.06398)
plot(pc1,ylim = c(-1,1))
plot(pc2,ylim = c(-1,1))

hist(sscs$diff_total,breaks=100)
hist(sscs$diff_abs_total,breaks=100)
hist(sscs$diff_abs_total / sscs$obs_n,breaks=100)
sscs$diff_abs_total_norm <- sscs$diff_abs_total / sscs$obs_n
hist(sscs$diff_abs_total_norm)

prepSSCModel(sscs,title="Morning thresh 0")
prepSSCModel(sscs,diff_threshold=25,title="Morning thresh 25")
prepSSCModel(sscs,diff_threshold=50,title="Morning thresh 50")
prepSSCModel(sscs,diff_threshold=75,title="Morning thresh 75")

prepSSCModel(sscs,response="diff_third2_mean",title="Noon thresh 0")
prepSSCModel(sscs,response="diff_third2_mean",diff_threshold=25,title="Noon thresh 25")
prepSSCModel(sscs,response="diff_third2_mean",diff_threshold=50,title="Noon thresh 50")
prepSSCModel(sscs,response="diff_third2_mean",diff_threshold=75,title="Noon thresh 75")

prepSSCModel(sscs,response="diff_third3_mean",title="Afternoon thresh 0")
prepSSCModel(sscs,response="diff_third3_mean",diff_threshold=25,title="Afternoon thresh 25")
prepSSCModel(sscs,response="diff_third3_mean",diff_threshold=50,title="Afternoon thresh 50")
prepSSCModel(sscs,response="diff_third3_mean",diff_threshold=75,title="Afternoon thresh 75")

# butterfly to butterfly models are probably terrible because
# if all butterfly foraging goes down in the summer, then no difference!!

for(i in 1:nrow(sscs)){
    sscs$q5 <- sscs$tmax
    sscs$q95 <- sscs$tmax
    sscs$q10 <- sscs$tmax
    sscs$dur <- sscs$tmax
    row <- sscs[i,]
    sscs$q5[i] <- quantile(as.numeric(as.vector(row$obs[[1]])),0.05)
    sscs$q10[i] <- quantile(as.numeric(as.vector(row$obs[[1]])),0.10)
    sscs$q95[i] <- quantile(as.numeric(as.vector(row$obs[[1]])),0.95)
}
sscs$dur <- sscs$q95 - sscs$q5
sscs$quantile
qfun <- quantile(as.numeric(as.vector(sscs$obs[5][[1]])),0.05)

quantile(as.numeric(as.vector(sscs$obs[5][[1]])),0.95)

sscs <- sscs_backup
sscs$dur <- sscs$q95 - sscs$q5

# sensitivity to grid id and season
prepSSCModel(sscs,response='dur',title='weib dur with grid id and season',
             formula="tmax + wingspan + daylength + season + (1 | species__) + (1 | grid_id)")
prepSSCModel(sscs,response='dur',title='weib dur with season',
             formula="tmax + wingspan + daylength + season + (1 | species__)")
prepSSCModel(sscs,response='dur',title='weib dur with grid id',
             formula="tmax + wingspan + daylength + (1 | species__) + (1 | grid_id)")
prepSSCModel(sscs,response='dur',title='weib dur',
             formula="tmax + wingspan + daylength + (1 | species__)")

# model selection - try starting with LMMs first -
prepSSCModel(sscs,response='q5',title='weib q5 with grid id and season',
             formula="tmax + wingspan + daylength + season + (1 | species__) + (1 | grid_id)")
prepSSCModel(sscs,response='q5',title='weib q5 with season',
             formula="tmax + wingspan + daylength + season + (1 | species__)")
prepSSCModel(sscs,response='q5',title='weib q5 with grid id',
             formula="tmax + wingspan + daylength + (1 | species__) + (1 | grid_id)")
prepSSCModel(sscs,response='q5',title='weib q5',
             formula="tmax + wingspan + daylength + (1 | species__)")

model <- prepSSCModel(sscs,response='diff_third1_mean',title='weib diff_third1_mean with grid id and season',
             formula="tmax + wingspan + daylength + (1 | species) + (1 | grid_id)")
vif(model)
model <- prepSSCModel(sscs,response='diff_third1_mean',title='weib diff_third1_mean with grid id and season',
                      formula="vp + daylength + season + (1 | species__)") #+ (1 | grid_id)")
plot(allEffects(model))
plot_bayes(model)
prepSSCModel(sscs,response='diff_third1_mean',title='weib diff_third1_mean with season',
             formula="tmax + wingspan + daylength + season + (1 | species__)")
prepSSCModel(sscs,response='diff_third1_mean',title='weib diff_third1_mean with grid id',
             formula="tmax + wingspan + daylength + (1 | species__) + (1 | grid_id)")
prepSSCModel(sscs,response='diff_third1_mean',title='weib diff_third1_mean',
             formula="tmax + wingspan + daylength + (1 | species__)")


prepSSCModel(sscs,response='q95',title='weib q95',
             formula="tmax + wingspan + daylength + (1 | species__)")

prepSSCModel(sscs,response='dur',title='weib dur',
             formula="tmax + wingspan + daylength + season + (1 | species__) + (1 | grid_id)")


prepSSCModel(sscs,response='q5',title='weib q5 without grid id',
             formula="tmax + wingspan + daylength + season + (1 | species__)")

prepSSCModel(sscs,response='diff_third1_mean',title='diff third1  mean with grid id',
             formula="tmax + wingspan + daylength + season + (1 | species__) + (1 | grid_id)")
prepSSCModel(sscs,response='diff_third1_mean',title='diff third1  mean without grid id',
             formula="tmax + wingspan + daylength + season + (1 | species__)")

prepSSCModel(sscs,response='q50',title='weib q50 with grid id',
             formula="tmax + wingspan + daylength + season + (1 | species__) + (1 | grid_id)")
prepSSCModel(sscs,response='q50',title='weib q50 without grid id',
             formula="tmax + wingspan + daylength + season + (1 | species__)")

prepSSCModel(sscs,response='diff_third2_mean',title='diff third2  mean with grid id',
             formula="tmax + wingspan + daylength + season + (1 | species__) + (1 | grid_id)")
prepSSCModel(sscs,response='diff_third2_mean',title='diff third2  mean without grid id',
             formula="tmax + wingspan + daylength + season + (1 | species__)")

# night
prepSSCModel(sscs,response='q95',title='weib q95 with grid id',
             formula="tmax + wingspan + daylength + season + (1 | species__) + (1 | grid_id)")
prepSSCModel(sscs,response='q95',title='weib q95 without grid id',
             formula="tmax + wingspan + daylength + season + (1 | species__)")

prepSSCModel(sscs,response='diff_third3_mean',title='diff third3  mean with grid id',
             formula="tmax + wingspan + daylength + season + (1 | species__) + (1 | grid_id)")
prepSSCModel(sscs,response='diff_third3_mean',title='diff third3  mean without grid id',
             formula="tmax + wingspan + daylength + season + (1 | species__)")

prepSSCModel(sscs,response="q5",title="Afternoon thresh 0")
prepSSCModel(sscs,response="diff_third3_mean",diff_threshold=25,title="Afternoon thresh 25")
prepSSCModel(sscs,response="diff_third3_mean",diff_threshold=50,title="Afternoon thresh 50")
prepSSCModel(sscs,response="diff_third3_mean",diff_threshold=75,title="Afternoon thresh 75")

hist(table(sscs$srad))
hist(table(sscs$tmax))
hist(as.numeric(sscs$season))
hist(table(sscs$species),breaks=100)
sscs$season
#making sure that a species being observed earlier isnt just because there are more humans out earlier
unique(sscs$tmax)

sscs <- sscs_backup
hist(as.numeric(sscs$tmax))

plot(sscs$srad)
mean(sscs$tmax)
sd(sscs$srad)
plot(sscs$precip)

# double check that climatic inst broken
