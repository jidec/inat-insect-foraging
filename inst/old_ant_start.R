# install packages
install.packages("dplyr")
install.packages("lubridate")
install.packages("rgdal")
install.packages("maps")
install.packages("raster")
install.packages("RColorBrewer")
install.packages("lme4")
install.packages("ape")
install.packages("phytools")
install.packages("tidyr")
install.packages("data.table")
library("dplyr")
library("lubridate")
library("rgdal")
library("maps")
library("raster")
library("RColorBrewer")
library("lme4")
library("ape")
library("phytools")
library("tidyr")
library("data.table")

source("src/prepGBIFiNatData.R")

# prep usa_ants
usa_ants <- prepGBIFiNatData("data/usa_ants_inat_gbif.csv")
usa_bfs <- prepGBIFiNatData("data/usa_butterflies_inat_gbif.csv")

# prep usa_insects
usa_insects <- prepGBIFiNatData("data/usa_insects_inat_gbif.csv")

# download daymet data if haven't done so already
source("src/downloadDaymetData.R")
#daymet_data <- downloadDaymetData(93)

# prep daymet data
# must be called "daymet_data" for adDaymetTileIDs and addDaymetCellData to use
source("src/prepDaymetData.R")
daymet_data <- prepDaymetData("data/daymet_150m.csv")


# add daymet tile ids and cell data
source("src/addDaymetTileIDs.R")
source("src/addDaymetCellData.R")
usa_ants <- addDaymetTileIDs(usa_ants, nsubset=100000)
usa_ants <- addDaymetCellData(usa_ants)

usa_bfs <- addDaymetTileIDs(usa_bfs, nsubset=50000)
usa_bfs <- addDaymetCellData(usa_bfs)

usa_insects <- addDaymetTileIDs(usa_insects, nsubset=50000)
usa_insects <- addDaymetCellData(usa_insects)


source("src/estimateSpeciesSeasonCellForaging.R")

# get obs first to assess spatiotemporal size of dataset
scc_obs <- getObsOnly(usa_bfs,usa_insects,cellsize_km = 200, min_per_cell_n=50,sample_cutoff_n = 150,floor_hours = F)

# divide data into species season cells with difference metrics from baseline
scc_estimates <- createBaselineDiffData3(usa_ants,usa_insects,cellsize_km=200,
                                                weib_iters=35,weib_ci_iters = 10,weib_ci_bootstraps = 10, ncpus=1,
                                                min_ants_n = 20, sample_cutoff_n = 150, floor_hours=FALSE,
                                                first_n=1000,kl_bootstraps=35, skip_n=0,simple = TRUE,daymet_manova = FALSE)
# or just import from a cluster analysis
species_season_cells <- read.csv("data/cluster_exports/sp_season_cell_all_iter25_2.csv")
species_season_cells <- finalizeCols(species_season_cells)
species_season_cells <- dplyr::filter(species_season_cells, is_sig_onset_offset == TRUE)

# below - slightly older stuff
# quick func to get mode
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
# summarize to species, with meanHour, modeHour, sdHour, n (sample size), meanMD (midday_dist), propNight,
# latCor (be careful with this, dont want to just cherry pick species)
usa_ant_species <- usa_ants %>%
    group_by(scientific_name) %>%
    summarise(meanHour = mean(local_hour), modeHour = Mode(local_hour), sdHour = sd(local_hour), n = n(), meanMD = mean(midday_dist),
              propNight = sum(is_night, na.rm = TRUE) / n(), latCor = cor(midday_dist,latitude))
usa_ant_species$latCor <- format(usa_ant_species$latCor, scientific = FALSE)

usa_ant_species_n100 <- filter(usa_ant_species, n >= 100)
#plot species histograms
hist(usa_ant_species_n100$meanHour)
hist(usa_ant_species_n100$sdHour)
hist(usa_ant_species_n100$meanMD)

# similarly summarize to genus
usa_ant_genera <- usa_ants %>%
    group_by(taxon_genus_name) %>%
    summarise(meanHour = mean(local_hour), modeHour = Mode(local_hour), sdHour = sd(local_hour), n = n(), meanMD = mean(midday_dist),
              propNight = sum(is_night, na.rm = TRUE) / n(), latCor = cor(midday_dist,latitude))
View(usa_ant_genera)
usa_ant_genera_n100 <- filter(usa_ant_genera, n >= 100)

# after start.R:
# descriptive_foraging.R
# lat_thermal_foraging.R
# color_foraging.R
# species_overlap_foraging.R
