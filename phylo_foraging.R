# example phylo signal of foraging times using genus phylogeny

# pasted from previous work, fairly messy
library(ape)
genus_tree <- read.tree("data/Dryad_Supplementary_File_7_ML_TREE_treepl_185.txt")
tips <- genus_tree$tip.label
genera <-unique(sapply(strsplit(tips,"_"),function(x) x[1]))

# drop all but one of each
ii<-sapply(genera,function(x,y) grep(x,y)[1],y=tips)
genus_tree<-drop.tip(genus_tree,setdiff(genus_tree$tip.label,tips[ii]))
#View tree
plot(genus_tree, show.tip.label=TRUE)

# function fix string format to match tree tips
modTips <- function(string) {
    string <- string[1]
    string <- as.character(string)
    length <- nchar(string)
    pos <- regexpr("_",string)
    string <- substring(string,1,pos-1)
    return(string)
}

# apply to fix string format
genus_tree$tip.label <- lapply(genus_tree$tip.label, modTips)

genus_traits <- filter(usa_ant_genera, n >= 25)

#get list of tip/trait labels that DONT have matching trait/tip data
getMissing <- function(vector2drop, reference)
{
    missing <- vector(mode="character")
    z <- 1

    for(i in 1:length(vector2drop))
    {
        tip <- as.character(vector2drop[i])
        match <- FALSE
        for(k in 1:length(reference))
        {
            if(reference[k] == tip)
            {
                match <- TRUE
            }
        }
        if(match == FALSE) {
            missing[z] <- tip
            z <- z + 1
        }
    }
    return(missing)
}

# drop tips
missingtips <- getmissing(genus_tree$tip.label,genus_traits$taxon_genus_name)
missingtips <- as.character(missingtips)
genus_tree <- drop.tip(genus_tree,missingtips)

# drop traits
missingtraits <- getmissing(genus_traits$taxon_genus_name, genus_tree$tip.label)


if(length(missingtraits != 0))
{
    for(i in 1:length(missingtraits))
    {
        genus_traits <- genus_traits[genus_traits[,"taxon_genus_name"] != missingtraits[i],]
    }
}

# these should now be the same
nrow(genus_traits)
length(genus_tree$tip.label)

# prep traits for asr
f_trait <- genus_traits$propNight #change this to meanMD, meanHour, propNight etc

names(f_trait) <- genus_traits$taxon_genus_name

# unlist tree tips
genus_tree$tip.label <- unlist(genus_tree$tip.label)

#makes trait indices match order of tree tips
matchorder <- function(tree, traitvect)
{
    ordered <- traitvect
    for(i in 1:length(tree$tip.label))
    {
        tipstr <- tree$tip.label[i]
        for(ii in 1:length(traitvect))
        {
            if(tipstr == names(traitvect[ii]))
            {
                ordered[i] <- traitvect[ii]
                names(ordered)[i] <- tipstr
            }
        }
    }
    return(ordered)
}

f_trait <- matchorder(genus_tree, f_trait)

# create contMap for all genera
obj <- contMap(genus_tree,f_trait,plot=FALSE)
plot(obj,type="fan",legend=0.7*max(nodeHeights(genus_tree)), fsize=c(0.6,0.9))

# compute phylogenetic signal
physig <- phylosig(genus_tree, genusMDs, method="K", test=FALSE, nsim=1000, se=NULL, start=NULL,
         control=list())

# run ASR
MLasr <- ace(genusMDs, genus_tree, type="continuous", method = "ML")

