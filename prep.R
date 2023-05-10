
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

# choose a parameter set to use in analysis
sscs <- sscs_8s_rmbf_250km_30nobs
source("src/addHrFreqsToSSCs.R")
sscs <- addHrFreqsToSSCs(sscs,start_trim = 6, end_trim=6)

# summarize to species for phylogenetic analysis
library(dplyr)
sscs_sp <- sscs %>%
    group_by(species) %>%
    dplyr::summarise(q5=mean(q5,na.rm=T),q50=mean(q50,na.rm=T),q95=mean(q95,na.rm=T),
                     diff1=mean(diff_third1_mean,na.rm=T),
                     diff2=mean(diff_third2_mean,na.rm=T),
                     diff3=mean(diff_third3_mean,na.rm=T),
                     pc1=mean(pc1,na.rm=T),pc2=mean(pc2,na.rm=T),
                     freq_pc1=mean(freq_pc1,na.rm=T),freq_pc2=mean(freq_pc2,na.rm=T),
                     freq_pc3=mean(freq_pc3,na.rm=T),freq_pc4=mean(freq_pc4,na.rm=T),
                     freq_pc5=mean(freq_pc5,na.rm=T),
                     shp_pc1 = mean(shp_pc1,na.rm=T),shp_pc2 = mean(shp_pc2,na.rm=T),
                     wingspan=mean(wingspan,na.rm=T), mean_n_obs=mean(obs_n,na.rm=T), n_cells=n())

sscs_sp$clade <- sscs_sp$species

# get sp thresholds
sscs_sp_10 <- dplyr::filter(sscs_sp,n_cells > 10)
sscs_sp_20 <- dplyr::filter(sscs_sp,n_cells > 20)
