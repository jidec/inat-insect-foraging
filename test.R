layout(matrix(1:3,1,3),widths=c(0.39,0.22,0.39))
plot(obj,lwd=6,ftype="off",legend=60,outline=TRUE,fsize=c(0,1.2))
plot.new()
plot.window(xlim=c(-0.1,0.1),
            ylim=get("last_plot.phylo",envir=.PlotPhyloEnv)$y.lim)
par(cex=0.6)
text(rep(0,length(obj$tree$tip.label)),1:Ntip(obj$tree),
     gsub("_"," ",obj$tree$tip.label),font=3)
plot(setMap(contMap(eel.tree,bsize,plot=FALSE),invert=TRUE),lwd=6,outline=TRUE,
     direction="leftwards",ftype="off",legend=60,fsize=c(0,1.2),
     leg.txt="body size (cm)")
library(phytools)
eel.tree<-read.tree("data/test_phylo.tre")
eel.data<-read.csv("data/elopomorph.csv",row.names=1)
fmode<-as.factor(setNames(eel.data[,1],rownames(eel.data)))
dotTree(eel.tree,fmode,colors=setNames(c("blue","red"),
                                       c("suction","bite")),ftype="i",fsize=0.7)
eel.trees<-make.simmap(eel.tree,fmode,nsim=100)
obj<-densityMap(eel.trees,states=c("suction","bite"),plot=FALSE)
plot(obj,lwd=4,outline=TRUE,fsize=c(0.7,0.9),legend=50)

bf.tree <- read.tree("data/misc/bf_species_tree.txt")
bf.data <- traits
bf.data <- bf.data %>% distinct(species, .keep_all = TRUE)
rownames(bf.data) <- bf.data$species
bf.data <- bf.data[,c(2,3,4)]

leptraits <- read.csv("data/consensus.csv")
leptraits <- leptraits[,c(3,33,34,35)]
colnames(leptraits) <- c("clade","trait","1","2")
rownames(leptraits) <- leptraits$species
leptraits <- leptraits[,33]
obj<-contMap(bf.tree,bf.data,plot=FALSE)

match(leptraits,foraging_traits)
source("src/prepTraitsTree.R")
bf_tree$tip.label
traits_tree <- prepTraitsTree(leptraits,bf_tree)
tree <- traits_tree[1][[1]]
traits <- traits_tree[2][[1]]
traits <- as.data.frame(traits)
fmode<-as.factor(setNames(leptraits$trait,rownames(eel.data)))
dotTree(tree,type="fan",traits,colors=setNames(c("blue","red"),
                                       c("Open canopy","Closed canopy")))
canopy <- leptraits[,c(1,2)]
canopy <- na.omit(canopy[match(canopy$clade,foraging_traits$clade),]))

plot(obj)
eel.trees<-make.simmap(eel.tree,fmode,nsim=100)
obj<-densityMap(eel.trees,states=c("suction","bite"),plot=FALSE)
plot(obj,lwd=4,outline=TRUE,fsize=c(0.7,0.9),legend=50)

