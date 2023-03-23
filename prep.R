
# download Daymet data
# downloadDaymetData(cellsize_km=250)

# prep Daymet using the specified season intervals per year
# convert local time to solar time
# grid obs data and create SSCs while merging in Daymet data
# add Weibull and different metrics
# merge in traits
source("src/createSSCsFromRawData.R")
sscs_8s_rmbf_250km_30nobs <- createSSCsFromRawData(taxon_obs_loc="data/inat_obs_raw/usa_butterflies_inat_gbif.csv",
                                                   baseline_obs_loc="data/inat_obs_raw/usa_insects_inat_gbif.csv",
                                                   season_intervals_per_year=8,rm_bfs_from_bl=TRUE,
                                                   ssc_cellsize_km=250, ssc_n_obs_threshold=30,
                                                   diff_metric_nbins=8,diff_metric_trim=1,
                                                   merge_leptraits=TRUE, add_weib_metrics = FALSE)

# weibull metrics take the longest to add, so do that seperately and track progress
source("src/addWeibullMetricsToSSCs.R")
sscs_8s_rmbf_250km_30nobs <- addWeibullMetricsToSSCs(sscs_8s_rmbf_250km_30nobs)

#saveRDS(sscs_8s_rmbf_250km_30nobs,"saved/processed/sscs_8s_rmbf_250km_30nobs.rds")
#sscs_8s_rmbf_250km_30nobs <- readRDS("saved/processed/sscs_8s_rmbf_250km_30nobs.rds")

source("src/makeSSCModel.R")
model <- makeSSCModel(sscs_4si_rmbf_250km_30nobs,response="diff_third1_mean",diff_threshold=75,title="Afternoon thresh 75",model_type = "lmm",
                      formula="vp + wingspan + daylength + (1 | species) + (1 | grid_id)")
