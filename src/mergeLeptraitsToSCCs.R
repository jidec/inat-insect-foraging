
mergeLeptraitsToSSCs <- function(sscs){
    # merge leptraits trait data into sscs
    library(dplyr)
    leptraits <- read.csv("D:/GitProjects/inat-insect-foraging/data/misc/leptraits_consensus.csv")
    leptraits$species <- leptraits$Species
    table(leptraits$CanopyAffinity)
    leptraits$CanopyAffinity[#leptraits$CanopyAffinity == "Mixed canopy (closed affinity)" |
        #leptraits$CanopyAffinity == "Mixed canopy" |
        #leptraits$CanopyAffinity == "Canopy generalist" |
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
    leptraits <- dplyr::select(leptraits, wingspan,open_closed,species)
    library(stringr)
    sscs$species <- paste(str_split_fixed(sscs$species," ",n=3)[,1],str_split_fixed(sscs$species," ",n=3)[,2])
    sscs <- merge(sscs,leptraits,by="species")
    return(sscs)
}
