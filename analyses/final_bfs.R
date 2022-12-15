library(lme4)
library(sjPlot)
library(car)
library(lmerTest)
library(performance)
library(ggplot2)
library(lattice)
library(ggbiplot)
library(phyr)
library(prediction)

# predictions from obvious to non
# daylength will increase duration
# species will have different regimes and be partially phylogenetically conserved
# tmax will increase duration, reduce q5, increase q95, however extremely high temperatures may be constraining
# vp represents non-aridity and should increase foraging by increasing plant productivity
# precip is interesting, at the day level should decrease foraging (can't forage in the rain), but at the season level should
#   increase productivity and thus foraging
# srad represents UV pressure and should decrease foraging

# scc_estimates is the output of estimateSpeciesSeasonCellForaging, probably run on the cluster
# it is a list of two lists - the first is the season cell info, the second contains corresponding raw hours

# scale sccs or skip
sccs <- transform(sccs,
                  precip=scale(precip),
                  srad=scale(srad),
                  tmax=scale(tmax),
                  tmin=scale(tmin),
                  daylength=scale(daylength),
                  vp = scale(vp),
                  cell_lat = scale(cell_lat),
                  cell_lon = scale(cell_lon),
                  wingspan = scale(wingspan),
                  nsamples = scale(n))

# fix species names
t <- str_split_fixed(sccs$species," ",3)
sccs$species <- paste(t[,1],t[,2])

testSCCModel <- function(response)
{
    model <- eval(parse(text=paste0("lmer(",var,"~ (precip + tmax + daylength)^2 + vp + srad + (1 | species) + (1 | cell),
              data = sccs,weights=sccs$kl_div)")))
    print(vif(model))
    print(summary(model))
    print(step(model)) #note to understand how that works backwards regression fit global model
    # not sig mle fits are removed and refit iteratively - reduces selection by removing noncontributing stuff

    print("1. Make sure vifs are lower than 5, adjust the formula if it is")
    print("2. Use suggested model formula")
}

# test SCC models
testSCCModel(response='duration')

summarizePlotSCCModel <- function(model)
{
    print(vif(model))
    print(summary(model))
    print(r2_nakagawa(model))
    plot(plot_model(model,type="pred",terms="daylength"))
    plot(plot_model(model,type="pred",terms="precip"))
    plot(plot_model(model,type="pred",terms="tmax"))
    plot(plot_model(model,type="pred",terms="vp"))
    plot(plot_model(model,type="pred",terms="srad"))
    plot(plot_model(model,type="pred",terms=c("daylength","precip")))
    dotplot(ranef(model,condVar=T))
}

dur_model <- lmer(duration ~ precip + tmax + daylength + vp + srad + (1 | species) + (1 | cell) + precip:daylength,
                            data = sccs, weights=sccs$kl_div)
q5_model <- lmer(q5 ~ precip + tmax + daylength + vp + srad + (1 | species) + (1 | cell) + precip:daylength,
                  data = sccs, weights=sccs$kl_div)
q50_model <- lmer(q50 ~ precip + tmax + daylength + vp + srad + (1 | species) + (1 | cell) + precip:daylength,
                  data = sccs, weights=sccs$kl_div)
q95_model <- lmer(q95 ~ precip + tmax + daylength + vp + srad + (1 | species) + (1 | cell) + precip:daylength,
                 data = sccs, weights=sccs$kl_div)

summarizePlotSCCModel(dur_model)
summarizePlotSCCModel(q5_model)
summarizePlotSCCModel(q50_model)
summarizePlotSCCModel(q95_model)

getPhySccs <- function(sccs,tree)
{
    sccs_and_phylo <- removeDataPhyloMissing(sccs,tree)
    phy_sccs <- sccs_and_phylo[[1]]
    phy_sccs <- subset(phy_sccs, select=-c(32))
    phy_sccs$season <- as.factor(phy_sccs$season)
    phy_sccs$duration <- phy_sccs$q95 - phy_sccs$q5
    phy <- sccs_and_phylo[[2]]
    phy$edge.length[which(is.na(phy$edge.length))] <- 0
    return(list(sccs,phy))
}

bf_tree <- ape::read.tree("data/misc/bf_species_tree.txt")
sccs_and_phy <- getPhySccs(sccs,bf_tree)
phy_sccs <- sccs_and_phy[[1]]
phy <- sccs_and_phy[[2]]

phy_dur_model <- pglmm(duration ~ 0 + precip + tmax + daylength + vp + srad + (1 | species__) + (1 | cell) + precip:daylength,
                       data = phy_sccs, cov_ranef = list(species=phy),bayes=T)

summarizePlotPhySCCModel <- function(model)
{
    library(phyr)
    print(summary(model))
    plot_bayes(model_duration,sort=T)
    #plot_model(model,type="pred",terms="daylength")
}

summarizePlotPhySCCModel(phy_dur_model)


# new traits lmer
dur_model_traits <- lmer(duration ~ precip + tmax + daylength + vp + srad + (1 | species) + (1 | cell) + precip:daylength + wingspan + open_closed,
                  data = sccs, weights=sccs$kl_div)
summary(dur_model_traits)
# these are ALL open or mixed, therefore


# get 5th quantile of tmax
high_tmax_sccs <- sccs[sccs$tmax > quantile(sccs$tmax,0.1),]
high_tmax_dur <- lm(duration ~ wingspan,
                         data = high_tmax_sccs, weights=high_tmax_sccs$kl_div)
summary(high_tmax_dur)
