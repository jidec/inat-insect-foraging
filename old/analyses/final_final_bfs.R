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
library(dplyr)
library(stringr)
library(rr2)
library(ape)

# predictions from obvious to non
# daylength will increase duration
# species will have different regimes and be partially phylogenetically conserved
# tmax will increase duration, reduce q5, increase q95, however extremely high temperatures may be constraining
# vp represents non-aridity and should increase foraging by increasing plant productivity
# precip is interesting, at the day level should decrease foraging (can't forage in the rain), but at the season level should
#   increase productivity and thus foraging
# srad represents UV pressure and should decrease foraging
# -6 gold pieces

# scc_estimates is the output of estimateSpeciesSeasonCellForaging, probably run on the cluster
# it is a list of two lists - the first is the season cell info, the second contains corresponding raw hours

# scale sscs or skip
sscs_sc <- transform(sscs,
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
                  #diff_half1_sum = scale(d_half1_sum),
                  #d_half2_sum = scale(d_half2_sum),
                  diff_third1_sum = scale(diff_third1_sum),
                  diff_third2_sum = scale(diff_third2_sum),
                  diff_third3_sum = scale(diff_third3_sum),
                  diff_third1_mean = scale(diff_third1_mean),
                  diff_third2_mean = scale(diff_third2_mean),
                  diff_third3_mean = scale(diff_third3_mean))
                  #d_mean_v = scale(d_mean_v))

# prep tree and trim sscs_sc
tree <- ape::read.tree("data/misc/bf_species_tree.txt")
tree$tip.label <- str_split_fixed(tree$tip.label," ",2)[,2]
tree$tip.label <- str_replace(tree$tip.label,"'","")
tree <- drop.tip(tree,tree$tip.label[!unique(sscs_sc$species) %in% tree$tip.label])
length(tree$tip.label)
sscs_sc <- sscs_sc[sscs_sc$species %in% tree$tip.label,]
tree$edge.length[which(is.na(tree$edge.length))] <- 0
sscs_sc$obs <- NULL
sscs_sc$bl_obs <- NULL
sscs_sc$season <- as.factor(sscs_sc$season)
#sscs$diff_third13_mean <- (sscs$diff_third1_sum + sscs$diff_third3_sum) / 2

# d_mean_v works, pc1 crashes on plot_bayes for some reason
model <- pglmm(diff_third1_mean ~ 0 + season + wingspan + vp + (1 | species__),
               data = sscs_sc, cov_ranef = list(species=tree),bayes=T)
plot_bayes(model) + ggtitle("Morning activity ")

rr2::R2(model)
model <- pglmm(diff_third2_mean ~ 0 + season + wingspan + (1 | species__),
               data = sscs_sc, cov_ranef = list(species=tree),bayes=T)
plot_bayes(model) + ggtitle("Noon activity")
rr2::R2(model)
model <- pglmm(diff_third3_mean ~ 0 + season + wingspan + (1 | species__),
               data = sscs_sc, cov_ranef = list(species=tree),bayes=T)
plot_bayes(model) + ggtitle("Afternoon/evening activity")
rr2::R2(model)

testSCCModel <- function(response,formula)
{
    model <- eval(parse(text=paste0("lmer(",response,"~ ",formula,
              ",data = sscs_sc)"))) #sscs
    print("VIFS:")
    print(vif(model))
    print(summary(model))
    print(step(model)) #note to understand how that works backwards regression fit global model
    # not sig mle fits are removed and refit iteratively - reduces selection by removing noncontributing stuff

    print("1. Make sure vifs are lower than 5, adjust the formula if it is")
    print("2. Use suggested model formula")
    return(model)
}
testSCCModel <- function(response,formula) # revise for pglmm
{
    model <- eval(parse(text=paste0("pglmm(",response,"~ ",formula,
                                    ",data = sscs_sc, cov_ranef = list(species=tree),bayes=T)"))) #sscs
    #print("VIFS:")
    #print(vif(model))
    print(summary(model))
    #print(step(model)) #note to understand how that works backwards regression fit global model
    # not sig mle fits are removed and refit iteratively - reduces selection by removing noncontributing stuff
    print(rr2::R2(model))
    print("1. Make sure vifs are lower than 5, adjust the formula if it is")
    print("2. Use suggested model formula")
    return(model)
}

# test SCC models
testSCCModel(response='diff_third1_mean',formula="(precip + tmax + daylength)^2 + vp + srad + wingspan + (1 | species) + (1 | grid_id)")
testSCCModel(response='diff_third1_mean',formula="(tmax + daylength)^2 + vp + srad + wingspan + season + (1 | species) + (1 | grid_id)") # rm precip for VIFS
testSCCModel(response='diff_third1_mean',formula="(tmax + daylength)^2 + vp + wingspan + season + (1 | species) + (1 | grid_id)") # rm srad for vifs
testSCCModel(response='diff_third1_mean',formula="(tmax + daylength)^2 + vp + srad + wingspan + season + (1 | species) + (1 | grid_id)") # rm precip for VIFS
testSCCModel(response='diff_third1_mean',formula="tmax + vp + wingspan + season + (1 | species) + (1 | grid_id)") # rm daylength for vifs
morning_model <- testSCCModel(response='diff_third1_mean',formula="tmax + vp + season + (1 | species) + (1 | grid_id)") # vifs are fine? (vp is 7..) rm wingspan as suggested
plot_model(morning_model)
testSCCModel(response='diff_third13_mean',formula="(tmax + daylength)^2 + vp + srad + wingspan + (1 | species) + (1 | grid_id)")
#model <- lmer(diff_third3_sum ~ 0 + tmax + daylength + wingspan + (1 | species),data=sscs)

# MORNING pglmms
# include vp and tmax - vp very positive tmax very negative
plot_bayes(testSCCModel(response='diff_third1_mean',formula="vp + tmax + wingspan + daylength + (1 | species__) + (1 | grid_id)")) + ggtitle("Morning Tmax VP")
# tmax alone - not significant
plot_bayes(testSCCModel(response='diff_third1_mean',formula="tmax + wingspan + daylength + (1 | species__)")) + ggtitle("Morning Tmax")
# vp alone - sig
plot_bayes(testSCCModel(response='diff_third1_mean',formula="vp + wingspan + daylength + season + (1 | species__) + (1 | grid_id)")) + ggtitle("Morning VP")
# whats this mean?

# NOON pglmms
# include vp and tmax - vp very positive tmax very negative
plot_bayes(testSCCModel(response='diff_third2_mean',formula="vp + tmax + wingspan + daylength + (1 | species__) + (1 | grid_id)"))
# tmax alone - not significant
plot_bayes(testSCCModel(response='diff_third2_mean',formula="tmax + wingspan + daylength + (1 | species__) + (1 | grid_id)"))
# vp alone - sig
plot_bayes(testSCCModel(response='diff_third2_mean',formula="vp + wingspan + daylength + (1 | species__)")) + ggtitle("Noon")
# whats this mean?

# EVENING pglmms
# include vp and tmax - vp very positive tmax very negative
plot_bayes(testSCCModel(response='diff_third3_mean',formula="vp + tmax + wingspan + daylength + (1 | species__) + (1 | grid_id)"))
# tmax alone - not significant
plot_bayes(testSCCModel(response='diff_third3_mean',formula="tmax + wingspan + daylength + (1 | species__) + (1 | grid_id)"))
# vp alone - sig
plot_bayes(testSCCModel(response='diff_third3_mean',formula="vp + wingspan + daylength + season + (1 | species__)")) + ggtitle("Afternoon/evening")
# whats this mean?

source("src/prepSSCModel.R")

library(effects)
plot(allEffects(model))
