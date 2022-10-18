removeDataPhyloMissing <- function(data,phylo)
{
    library(ape)
    # drop traits
    missing_traits <- getMissing(data$species, phylo$tip.label)
    print(length(missing_traits))
    if(length(missing_traits != 0))
    {
        for(i in 1:length(missing_traits))
        {
            data <- data[data[,"species"] != missing_traits[i],]
        }
    }

    missing_tips <- getMissing(phylo$tip.label,data$species)
    missing_tips <- as.character(missing_tips)
    phylo <- drop.tip(phylo,missing_tips)

    return(list(data,phylo))
}
