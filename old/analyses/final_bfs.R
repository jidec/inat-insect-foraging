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

# scale sscs or skip
sscs <- transform(sscs,
                  precip=scale(precip),
                  srad=scale(srad),
                  tmax=scale(tmax),
                  #tmin=scale(tmin),
                  daylength=scale(daylength),
                  vp = scale(vp),
                  #cell_lat = scale(cell_lat),
                  #cell_lon = scale(cell_lon),
                  wingspan = scale(wingspan),
                  #nsamples = scale(n),
                  pc1 = scale(pc1),
                  d_half1_sum = scale(d_half1_sum),
                  d_half2_sum = scale(d_half2_sum),
                  d_mean_v = scale(d_mean_v))

# fix species names
t <- str_split_fixed(sscs$species," ",3)
sscs$species <- paste(t[,1],t[,2])

testSCCModel <- function(response)
{
    model <- eval(parse(text=paste0("lmer(",var,"~ (precip + tmax + daylength)^2 + vp + srad + (1 | species) + (1 | cell),
              data = sscs,weights=sscs$kl_div)")))
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

class(model) == "lmerModLmerTest"
dur_model <- lmer(duration ~ precip + tmax + daylength + vp + srad + (1 | species) + (1 | cell) + precip:daylength,
                            data = sscs, weights=sscs$kl_div)
q5_model <- lmer(q5 ~ precip + tmax + daylength + vp + srad + (1 | species) + (1 | cell) + precip:daylength,
                  data = sscs, weights=sscs$kl_div)
q50_model <- lmer(q50 ~ precip + tmax + daylength + vp + srad + (1 | species) + (1 | cell) + precip:daylength,
                  data = sscs, weights=sscs$kl_div)
q95_model <- lmer(q95 ~ precip + tmax + daylength + vp + srad + (1 | species) + (1 | cell) + precip:daylength,
                 data = sscs, weights=sscs$kl_div)

summarizePlotSCCModel(dur_model)
summarizePlotSCCModel(q5_model)
summarizePlotSCCModel(q50_model)
summarizePlotSCCModel(q95_model)


getPhySSCs <- function(sscs,tree)
{
    source("src/removeDataPhyloMissing.R")
    source("src/plotPhyloEffects.R")
    library(ape)
    sscs_and_phy <- removeDataPhyloMissing(sscs,tree)
    phy_sscs <- sscs_and_phylo[[1]]
    phy_sscs <- subset(phy_sscs, select=-c(32))
    #phy_sscs$season <- as.factor(phy_sscs$season)
    #phy_sscs$duration <- phy_sscs$q95 - phy_sscs$q5
    phy <- sscs_and_phy[[2]]
    sscs <- sscs_and_phy[[1]]
    phy$edge.length[which(is.na(phy$edge.length))] <- 0
    return(list(sscs,phy))
}

bf_tree <- ape::read.tree("data/misc/bf_species_tree.txt")
library(stringr)
bf_tree$tip.label <- str_split_fixed(bf_tree$tip.label," ",2)[,2]
bf_tree$tip.label <- str_replace(bf_tree$tip.label,"'","")

sscs_and_phy <- getPhySSCs(sscs,bf_tree)
phy_sscs <- sscs_and_phy[[1]]
phy <- sscs_and_phy[[2]]
#length(tree$tip.label)
#tree <- drop.tip(tree,tree$tip.label[!unique(sscs$species) %in% tree$tip.label])
#length(tree$tip.label)
#sscs <- sscs[sscs$species %in% tree$tip.label,]
#tree$edge.length[which(is.na(tree$edge.length))] <- 0
phy_sscs$obs <- NULL
phy_sscs$bl_obs <- NULL
phy_sscs$wingspan <- as.numeric(phy_sscs$wingspan)
phy_sscs$half <- phy_sscs$d_half1_sum + phy_sscs$d_half2_sum
phy_sscs$half_mean <- (phy_sscs$d_half1_sum + phy_sscs$d_half2_sum) / 2
model <- pglmm(half ~ 0 + tmax + daylength + srad + wingspan + (1 | grid_id) + (1 | species__),
                       data = phy_sscs, cov_ranef = list(species=phy),bayes=T)
plot_bayes(model) + ggtitle("half1")

model <- pglmm(half_mean ~ 0 + tmax + daylength + srad + wingspan + season + (1 | grid_id) + (1 | species__),
               data = phy_sscs, cov_ranef = list(species=phy),bayes=T)
plot_bayes(model) + ggtitle("half_mean")

summarizePlotPhySCCModel <- function(model)
{
    library(phyr)
    print(summary(model))
    plot_bayes(model_duration,sort=T)
    #plot_model(model,type="pred",terms="daylength")
}

summarizePlotPhySCCModel(phy_dur_model)


# new traits lmer
# tmax increases duration
# daylength increases duration
# srad decreases duration
# open canopy decreases duration
# precip:daylength
dur_model_traits <- lmer(pc1 ~ precip + tmax + daylength + srad + (1 | species) + precip:daylength + wingspan, #+ open_closed,
                  data = sscs)
hist(sscs$pc1)
summary(dur_model_traits)
table(sscs$open_closed)
# these are ALL open or mixed, therefore
library(sjPlot)
plot_model(dur_model_traits)

# get 5th quantile of tmax
high_tmax_sscs <- sscs[sscs$tmax > quantile(sscs$tmax,0.1),]
high_tmax_dur <- lm(duration ~ wingspan,
                         data = high_tmax_sscs, weights=high_tmax_sscs$kl_div)
summary(high_tmax_dur)

# look at bayesian prob theory
# conditional - random effects are important
# marginal

print(r2_nakagawa(dur_model_traits))

# talk about how to scope paper
