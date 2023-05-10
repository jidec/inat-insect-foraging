
# season_intervals_per_year - whether to split season by 4 or 8
# rm_bfs_from_bl - whether or not to filter out butterflies (what to use as baseline)
# ssc_cellsize_km - size of grid cells for SSCs
# ssc_n_obs_threshold -threshold of n obs to keep an SSC
# diff_metric_nbins/diff_metric_trim - how to bin and trim for difference metrics

# season_intervals_per_year=8
# rm_bfs_from_bl=TRUE
# ssc_cellsize_km=250
# ssc_n_obs_threshold=30
# diff_metric_nbins=8
# diff_metric_trim=1
# merge_leptraits=TRUE
# add_weib_metrics = FALSE
# taxon_obs_loc="data/inat_obs_raw/usa_butterflies_inat_gbif.csv"
# baseline_obs_loc="data/inat_obs_raw/usa_insects_inat_gbif.csv"

createSSCsFromRawData <- function(taxon_obs_loc, baseline_obs_loc,
                                  season_intervals_per_year=4,rm_bfs_from_bl=TRUE,
                                  ssc_cellsize_km=250, ssc_n_obs_threshold=30,
                                  start_hr_trim =1,end_hr_trim=1,
                                  diff_metric_nbins=8,
                                  merge_leptraits=TRUE, add_weib_metrics=FALSE,write_rds_name=NULL){
    # save start time for elapsed
    start_time <- Sys.time()

    # read in raw iNat data
    library(readr)
    print("Loading iNat data")
    taxon_obs <- read_delim(taxon_obs_loc,delim = "\t", escape_double = FALSE, trim_ws = TRUE)
    baseline_obs <- read_delim(baseline_obs_loc,delim = "\t", escape_double = FALSE, trim_ws = TRUE)

    # prepare Daymet data by adding seasons and formatting correctly
    # OPTION - whether to split season by 4 or 8
    source("src/prepDaymetData.R")
    print("Prepping Daymet data with prepDaymetData()")
    daymet_data <- prepDaymetData(season_intervals_per_year=8)
    saveRDS(daymet_data,file="data/saved_rds/dfs/daymet.rds")

    # OPTION - whether or not to filter out butterflies, to not filter out bflies, or to use all organisms
    # remove butterflies from all_insects
    # rationale here is that leaving butterflies in the baseline
    #   will pollute it because butterflies in the baseline have the most REAL signal within them
    # recall that the goal of the baseline is to capture observer behavior with as little
    #   REAL foraging signal as possible
    if(rm_bfs_from_bl){
        print("Removing butterflies from baseline")
        baseline_obs <- dplyr::filter(baseline_obs, family != "Hesperiidae" &
                                  family != "Hedylidae" &
                                  family != "Lycaenidae" &
                                  family != "Nymphalidae" &
                                  family != "Papilionidaee" &
                                  family != "Pieridae" &
                                  family != "Riodinidae")
    }

    # convert times
    source("src/datetimeToSolarAndSeason.R")
    print("Converting local time to solar time")
    taxon_obs <- datetimeToSolarAndSeason(taxon_obs,season_intervals_per_year = season_intervals_per_year)
    baseline_obs <- datetimeToSolarAndSeason(baseline_obs,season_intervals_per_year = season_intervals_per_year)

    # add Daymet tile IDS
    source("src/addDaymetTileIDs.R")
    print("Matching observations to closest Daymet tiles")
    taxon_obs <- addDaymetTileIDs(taxon_obs,daymet_data)
    baseline_obs <- addDaymetTileIDs(baseline_obs,daymet_data)

    # OPTION - size of grid cells for SSCs
    # grid up both clade and baseline observations and return them as a tuple
    # note that obs must have closest daymet tile attached previously
    source("src/prepGriddedData.R")
    print("Gridding up data")
    gridded_tuple <- prepGriddedData(taxon_obs,baseline_obs,cellsize_km=ssc_cellsize_km)

    # OPTION - threshold to keep an SSC
    # create species-season-cells with a given min num obs required to keep an SSC
    source("src/createSSCs.R")
    print("Creating SSCs")
    sscs <- createSSCs(gridded_tuple,daymet_data,min_per_cell_n=ssc_n_obs_threshold)

    # add metrics generated using the differences between the clade and the baseline
    # OPTION - how to bin and trim for difference metrics
    source("src/addDiffMetricsToSSCs.R")
    print("Adding difference metrics")
    sscs <- addDiffMetricsToSSCs(sscs,nbins=8,start_trim = start_hr_trim,end_trim=end_hr_trim)

    source("src/addHrFreqsToSSCs.R")
    sscs <- addHrFreqsToSSCs(sscs,start_trim=start_hr_trim,end_trim=end_hr_trim)

    # merge leptraits data (wing size, habitat preference)
    if(merge_leptraits){
        source("src/mergeLeptraitsToSCCs.R")
        print("Merging Leptraits data into SSCs")
        sscs <- mergeLeptraitsToSSCs(sscs)
    }

    if(add_weib_metrics){
        # add metrics generated using the Weibull fitting & onset/offset approach
        source("src/addWeibullMetricsToSSCs.R")
        print("Adding Weibull metrics (will take awhile)")
        sscs <- addWeibullMetricsToSSCs(sscs)
    }

    if(!is.null(write_rds_name)){
        saveRDS(sscs,file=paste0("data/saved_rds/",write_rds_name))
    }

    # print elapsed
    end_time <- Sys.time()
    print(end_time - start_time)

    return(sscs)
}
