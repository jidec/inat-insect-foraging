
source("src/estimateSpeciesSeasonCellForaging.R")

# get obs first to assess spatiotemporal size of dataset
scc_obs_150_100 <- getObsOnly(usa_bfs,usa_insects,cellsize_km = 150, min_per_cell_n=100,sample_cutoff_n = 150,floor_hours = F)

# divide data into species season cells with difference metrics from baseline
scc_estimates_bfs <- estimateSpeciesSeasonCellForaging(usa_bfs,usa_insects,cellsize_km=150,
                                                       weib_iters=35,weib_ci_iters = 10,weib_ci_bootstraps = 10, ncpus=3,
                                                       min_per_cell_n = 100, sample_cutoff_n = 150, floor_hours=FALSE,
                                                       first_n=2000,kl_bootstraps=35, skip_n=0,simple =TRUE,daymet_manova = FALSE)
# save
saveRDS(scc_estimates_bfs,file="scc_estimates_bfs")
