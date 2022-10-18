
source("src/prepGBIFiNatData.R")

# prep clade
# usa_ants <- prepGBIFiNatData("../../../srv/mybook/jacobi/data/usa_ants_inat_gbif.csv")
usa_bfs <- prepGBIFiNatData("data/usa_butterflies_inat_gbif.csv")

# prep usa_insects
usa_insects <- prepGBIFiNatData("data/usa_insects_inat_gbif.csv")

# download daymet data ONLY if haven't done so already
source("src/downloadDaymetData.R")
daymet_data <- downloadDaymetData(100)

# prep daymet data
# must be called "daymet_data" for adDaymetTileIDs and addDaymetCellData to use
source("src/prepDaymetData.R")
daymet_data <- prepDaymetData("data/daymet.csv")


# add daymet tile ids and cell data
source("src/addDaymetTileIDs.R")
source("src/addDaymetCellData.R")
#usa_ants <- addDaymetTileIDs(usa_ants, nsubset=100000)
#usa_ants <- addDaymetCellData(usa_ants)

usa_bfs <- addDaymetTileIDs(usa_bfs, nsubset=50000)
usa_bfs <- addDaymetCellData(usa_bfs)

usa_insects <- addDaymetTileIDs(usa_insects, nsubset=50000)
usa_insects <- addDaymetCellData(usa_insects)
