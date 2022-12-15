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

# merge leptraits trait data into sccs
library(dplyr)
leptraits <- read.csv("D:/GitProjects/inat-insect-foraging/data/leptraits_consensus.csv")
leptraits$species <- leptraits$Species
leptraits$CanopyAffinity[#leptraits$CanopyAffinity == "Mixed canopy (closed affinity)" |
                        #leptraits$CanopyAffinity == "Mixed canopy" |
                        leptraits$CanopyAffinity == "Canopy generalist" |
                        leptraits$CanopyAffinity == "Closed canopy (+edge)" |
                        leptraits$CanopyAffinity == "Edge associated"
                        #leptraits$CanopyAffinity == "Mixed canopy (open affinity)"
                        ] <- NA
leptraits$CanopyAffinity[leptraits$CanopyAffinity == "Mixed canopy (open affinity)" |
                             leptraits$CanopyAffinity == "Mixed canopy (closed affinity)" |
                             leptraits$CanopyAffinity == "Mixed canopy" |
                             leptraits$CanopyAffinity == "Canopy generalist"] <- "Mixed canopy"
leptraits$open_closed <- leptraits$CanopyAffinity
leptraits$wingspan <- leptraits$WS_U
leptraits <- select(leptraits, wingspan,open_closed,species)
library(stringr)
sccs$species <- paste(str_split_fixed(sccs$species," ",n=3)[,1],str_split_fixed(sccs$species," ",n=3)[,2])
sccs <- merge(sccs,leptraits,by="species")
table(sccs$open_closed)
#sccs <- sccs[,1:40]
