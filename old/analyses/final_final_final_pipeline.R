# download Daymet data
downloadDaymetData(cellsize_km=250)

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

source("src/addWeibullMetricsToSSCs.R")
sscs_8s_rmbf_250km_30nobs <- addWeibullMetricsToSSCs(sscs_8s_rmbf_250km_30nobs)
sscs_8s_rmbf_250km_30nobs <- readRDS("data/saved_rds/sscs_8s_rmbf_250km_30nobs.rds")
source("src/makeSSCModel.R")
model <- makeSSCModel(sscs_4si_rmbf_250km_30nobs,response="diff_third1_mean",title="Afternoon thresh 75",model_type = "lmm",
                      formula="vp + wingspan + daylength + (1 | species) + (1 | grid_id)")

plotSaveLM(sscs_8s_rmbf_250km_30nobs,response="q5",formula="tmax + wingspan + daylength + srad + precip + (1 | species) + (1 | grid_id)")
plotSaveLM(sscs_8s_rmbf_250km_30nobs,response="q50",formula="tmax + wingspan + daylength + srad + precip + (1 | species) + (1 | grid_id)")
plotSaveLM(sscs_8s_rmbf_250km_30nobs,response="q95",formula="tmax + wingspan + daylength + srad + precip + (1 | species) + (1 | grid_id)")


sscs_8s_rmbf_250km_30nobs$clade <- sscs_8s_rmbf_250km_30nobs$species
phytrim_data_tuple <- trimDfToTree(sscs_8s_rmbf_250km_30nobs,tree_location = "data/misc/bf_species_tree.txt",
                                   replace_underscores = T, remove_commas = T, remove_first_split = T)


sscs_phytrim <- phytrim_data_tuple[[1]]
phy <- phytrim_data_tuple[[2]]

plotSaveLM(sscs_phytrim,model_type="pglmm",response="q95",formula="tmax + wingspan + daylength + srad + precip + (1 | species__)")


library(ape)
tree <- read.tree("data/misc/bf_species_tree.txt")

sscs_phytrim <- phytrim_data_tuple[[1]]
sscs_phy <- phytrim_data_tuple[[2]]

table(sscs_8s_rmbf_250km_30nobs$season)

tree$tip.label <-  str_replace(tree$tip.label,"_"," ")

tree$tip.label <- str_replace(tree$tip.label,"'","")

tree$tip.label <- str_split_fixed(tree$tip.label," ",2)[,2] #
tree$tip.label

