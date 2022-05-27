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

source("src/readPrepData.R")

# prep usa butterflies
usa_bfs <- readPrepGBIFData("data/usa_butterflies_gbif.csv")

# prep usa_insects baseline
usa_insects <- readPrepBaselineInsects(sample=500000)

# prep daymet dat
daymet_data <- readPrepDaymetData()

source("src/addDaymetCellData.R")

# add daymet tile ids and cell data
usa_bfs <- addDaymetTileIDs(usa_bfs, nsubset=100000)
usa_bfs <- addDaymetCellData(usa_bfs)


usa_insects <- addDaymetTileIDs(usa_insects, nsubset=50000)
usa_insects <- addDaymetCellData(usa_insects)

source("src/createBaselineDiffData3.R")
# divide data into species season cells with difference metrics from baseline
species_season_cells <- createBaselineDiffData3(usa_bfs,usa_insects,cellsize_miles=155,
                                                weib_iters=10,weib_ci_iters = 10,weib_ci_bootstraps = 10, ncpus=2,
                                                min_ants_n = 8, sample_cutoff_n = 150, floor_hours=TRUE,
                                                first_n=1000,skip_n=0)
# or just import from a cluster analysis
species_season_cells <- read.csv("data/cluster_exports/sp_season_cell_all_iter25_2.csv")

