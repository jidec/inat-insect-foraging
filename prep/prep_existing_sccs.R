scc_estimates_bfs <- readRDS("data/scc_estimates_bfs")

# extract sccs
sccs <- scc_estimates_bfs[[1]]
# extract hours
hours <- scc_estimates_bfs[[2]]

# add daymet data to sccs
# daymet_data is created in prep required data
source("src/addClosestDaymetIDToSCCs.R")
sccs <- addClosestDaymetIDToSCCs(sccs)

# merge seasonal daymet_data onto sccs
sccs <- merge(sccs, daymet_data, by = c("daymet_tile", "season"),how='left')

# remove duplicates
sccs <- sccs[!duplicated(sccs[ , c("season", "species","cell")]), ]

# add duration a new response variable (temp, new method does this already)
sccs$duration <- sccs$q95 - sccs$q5
