scc_estimates_bfs <- readRDS("data/scc_estimates_bfs")
scc_estimates_bfs[[1]]$season <- as.factor(scc_estimates_bfs[[1]]$season)
# scc_estimates is the output of estimateSpeciesSeasonCellForaging
# it is a list of two lists - the first is the season cell info, the second contains corresponding raw hours

# extract bf hours and baseline hours
bf_hours <- list()
bf_bl_hours <- list()
for(i in 1:length(scc_estimates_bfs[[2]]))
{
    hours <- scc_estimates_bfs[[2]][[i]]
    split_index <- which(hours == -1)
    bf <- hours[1:split_index-1]
    bl <- hours[split_index+1:length(hours)]
    bl <- na.omit(bl)
    bf_hours <- c(bf_hours,list(bf))
    bf_bl_hours <- c(bf_bl_hours,list(bl))
}

# plot the bf and baseline hour for scc 2
hist(bf_hours[[2]],xlim=c(5,24))
hist(bf_bl_hours[[2]],xlim=c(5,24))

# extract sccs
sccs <- scc_estimates_bfs[[1]]

# add daymet data to sccs
source("src/prepDaymetData.R")
source("src/addClosestDaymetIDToSCCs.R")
daymet_data <- prepDaymetData("data/daymet.csv")
sccs <- addClosestDaymetIDToSCCs(sccs)

#sccs <- join(sccs,daymet_data,by=c("daymet_tile", "season"),type='left')
sccs <- merge(sccs, daymet_data, by = c("daymet_tile", "season"),how='left')

# fix species names
t <- str_split_fixed(sccs$species," ",3)
sccs$species <- paste(t[,1],t[,2])

# summarize sccs
length(unique(sccs$cell)) # number of unique scc cells
length(unique(sccs$daymet_tile)) # num of unique daymet tiles

sccs <- sccs[!duplicated(sccs[ , c("season", "species","cell")]), ]

# scale sccs or skip
sccs <- transform(sccs,
                        precip=scale(precip),
                        srad=scale(srad),
                        tmax=scale(tmax),
                        tmin=scale(tmin),
                        daylength=scale(daylength),
                        vp = scale(vp),
                        cell_lat = scale(cell_lat),
                        cell_lon = scale(cell_lon))

sccs$duration <- sccs$q95 - sccs$q5
sccs_scaled$duration <- sccs_scaled$q95 - sccs_scaled$q5
library(lme4)

model <- lmer(duration ~ (precip + tmax + daylength)^2 + vp + srad + (1 | species) + (1 | cell),
              data = sccs,weights=sccs$kl_div)
model <- lmer(duration ~ precip + tmax + daylength + vp + srad + (1 | species) + (1 | cell) + precip:daylength,
              data = sccs,weights=sccs$kl_div)
vif(model)
step(model)
summary(model)
model <- lmer(duration ~ precip + tmax + daylength + (1 | species) + (1 | cell) + precip:daylength + tmax:daylength,
              data = sccs,weights=sccs$kl_div)
summary(model)
vif(model)

# there to look at interaction
plot_model(model,type="pred",terms=c("daylength","precip","tmax"))
plot_model(model,type="pred",terms=c("daylength","precip"))
# low dls, duration increases with more precip
# in drought conditions strong effect vs wet conditions
# if longer daylengths, negative relationship with precip
# temp low, dl precip high, short duration
#tmax:daylength is the biggest

library(sjPlot)
model <- lmer(duration ~ precip + tmax + season + cell_lat + daylength + (1 | species) + (1 | cell) + precip:season + tmax:vp + tmax:cell_lat + tmax:cell_lon + season:vp + season:cell_lat + vp:cell_lat + vp:daylength + cell_lat:daylength,
              data = sccs,weights=sccs$kl_div)
model_duration <- lmer(duration ~ precip + tmax + season + cell_lat + daylength + cell_lon + (1 | species) + (1 | cell) + precip:season + tmax:vp + tmax:cell_lat + tmax:cell_lon + season:vp + season:cell_lat + vp:cell_lat + vp:daylength + cell_lat:daylength,
                  data = sccs,weights=sccs$kl_div)
step(model_duration)

model_duration <- lmer(duration ~ (precip + tmax + season + vp + cell_lat + daylength + cell_lon)^2 + (1 | species) + (1 | cell),
                  data = sccs,weights=sccs$kl_div)

model_q95 <- lmer(q95 ~ precip + tmin + season + (1 | species) + (1 | cell) + precip:season + tmin:season + vp:season,
                  data = sccs,weights=sccs$kl_div)

model_q95 <- lmer(q95 ~ precip + tmin + cell_lat + cell_lon + season + (1 | species) + (1 | cell) + precip:tmin + precip:season,
                  data = sccs,weights=sccs$kl_div)

library(car)

vif(model_duration)
model_q5 <- lmer(q95 ~ (tmin + daylength + vp)^2
                  + (1|species) + (1|season) + (1|cell),
                  data = sccs,weights=sccs$kl_div)
sccs$
install.packages("lmerTest")
summary(model_duration)
library(lmerTest)
lmerTest::step(model_duration)

# stepwise model reduction, reduce overfitting
install.packages("performance")
library(performance)
r2_nakagawa(model_duration)
# performance package - r2 for lmms, nakagawa
# sjplot

# variance inflation
# carr package - vif function for model - if >5 there is multicollinearity
# pglmm
# 1|species__tree-calling-param
# ncf corlog

# look up variance inflation factors and condition numbers

# get vifs for non phylogenetic model

# look at residuals from
# residual - structure of deviation of points from best fit line

# slack jamm about meeting

# spatial autocor - are residuals structured around space?
# a phenomenna - we expect spatial structure in models
# residual plot against distances - corlog R function

install.packages("phyloseq")
install.packages("ggridges")
library(ape)
sccs_and_phylo <- removeDataPhyloMissing(sccs,bf_tree)
phy_sccs <- sccs_and_phylo[[1]]
phy_sccs <- subset(phy_sccs, select=-c(32))
phy_sccs$season <- as.factor(phy_sccs$season)
phy <- sccs_and_phylo[[2]]
phy$edge.length[which(is.na(phy$edge.length))] <- 0

library(phyr)

model_duration <- pglmm(duration ~ (tmax + precip)^2 + vp + daylength + (1 | species__) + (1 | cell), data = phy_sccs, cov_ranef = list(species=phy),bayes=T)
plot_bayes(model_duration,sort=T)
summary(model_duration)
model <- phyr::pglmm(pres ~ disturbance + (1 | sp__) + (1 | site) +
                       (disturbance | sp__) + (1 | sp__@site),
                   data = oldfield$data,
                   cov_ranef = list(sp = phy))
install.packages('INLA', repos='https://inla.r-inla-download.org/R/stable')

model_q95 <- phyr::pglmm(q95 ~ tmin + daylength + vp
                  + (1|species__) + (1|cell),
                  data = phy_sccs,
                  cov_ranef = list(species=phy),bayes=T)
plot(phy)
length(unique(phy_sccs$species))
length(phy$tip.label)
library(dplyr)
phy_sccs <- phy_sccs %>% group_by(species) %>% filter(n()>=10)
library(ape)
sccs_and_phylo <- removeDataPhyloMissing(phy_sccs,phy)
phy_sccs <- sccs_and_phylo[[1]]
phy <- sccs_and_phylo[[2]]
table(phy_sccs$species)
install.packages("car")
library(car)
# the leading minor of order 1 is not positive definite
# The error you are seeing occurs when some of the eigenvectors of the matrix
# you are trying to operate on are not positive
# (typically they'll be zero, or below some very small threshold);
# this means, essentially, that your data are too noisy/small to estimate a full covariance matrix.

library(sjPlot)
dotplot(ranef(model_q95,condVar=T))
plot_model(model_q95,type="pred",terms="season") + labs(x="season",y="foraging offset hour",title="") +
    theme_grey()

library(plyr)

merged <- merged[,-33]
merged <- merged[,-40]

q5_model <-
q5_model <- lm(q5 ~ season + tmax, data=sccs)
summary(q5_model)

q5_model_w <- lm(q5 ~ season + tmax + daylength, data=sccs,weights=sccs$kl_div)
summary(q5_model_w)

q5_model_w_sp <- lm(q5 ~ cell_lat + season + species, data=scc_estimates,weights=scc_estimates$kl_div)
summary(q5_model_w_sp)

q50_model <- lm(q50 ~ cell_lat + season, data=scc_estimates)
summary(q50_model)

q50_model_w <- lm(q50 ~ cell_lat + season, data=scc_estimates,weights=scc_estimates$kl_div)
summary(q50_model_w)

q95_model <- lm(q95 ~ cell_lat + season, data=scc_estimates)
summary(q95_model)

q95_model_w <- lm(q95 ~ cell_lat + season, data=scc_estimates,weights=scc_estimates$kl_div)
summary(q95_model_w)

# weighting makes all effects larger
cor(scc_estimates$q5,scc_estimates$q50)
cor(scc_estimates$q95,scc_estimates$q50)
cor(scc_estimates$q5,scc_estimates$q95)

plot(scc_estimates$q5,scc_estimates$q95)

# apply dimensionality reduction on the hours or quantiles to get axes of foraging distribution variation

# increased onset is correlated with increased center
# increased onset is correlated with decreased offset
# increased center is correlated with increased offset

library(dplyr)
species_means <- scc_estimates %>%
    group_by(species) %>%
    summarise(mean_q5 = mean(q5), mean_q50 = mean(q50),mean_q95 = mean(q95), n = n())
plot(species_means$mean_q5,species_means$mean_q95)
cor(species_means$mean_q5,species_means$mean_q50)
cor(species_means$mean_q95,species_means$mean_q50)
cor(species_means$mean_q5,species_means$mean_q95)

species_means_cutoff <- dplyr::filter(species_means,n >= 5)
plot(species_means_cutoff$mean_q5,species_means_cutoff$mean_q95)
hist(species_means_cutoff$mean_q5,xlim=c(1,24))
hist(species_means_cutoff$mean_q50,xlim=c(1,24))

all_qs <- c(species_means_cutoff$mean_q5,species_means_cutoff$mean_q50,species_means_cutoff$mean_q95)
hist(all_qs,xlim=c(1,24), breaks=24)
hist(species_means_cutoff$mean_q50)

library(lme4)
model_duration_dl <- lmer(duration ~ precip + srad + tmax + tmin + daylength + vp
                       + (0 + cell | species),
                       data = sccs,weights=sccs$kl_div)
model_duration_nondl <- lmer(duration ~ precip + srad + tmax + tmin + vp
                       + (0 + cell | species),
                       data = sccs,weights=sccs$kl_div)
AIC(model_du)

# check aic with and without daylength
step(model_duration)


model_q95 <- lmer(q95 ~ precip + srad + tmax + tmin + daylength + vp
                      + (0 + cell | species),
                  data = sccs_scaled,weights=sccs_scaled$kl_div)

step(model_q95)

model_q95 <- lmer(q95 ~ cell_lat + season +
                  (0 + cell_lat | species) +
                  (0 + season | species),
              data = sccs,weights=sccs$kl_div)

library(ggplot2)
library(sjPlot)
library(lattice)
dotplot(ranef(model,condVar=T))
plot_model(model_q95,type="pred",terms="season") + labs(x="season",y="foraging offset hour",title="") +
    theme_grey()

m_model_q5 <- lmer(q5 ~ cell_lat + season +
                        (1|cell) + (1|species) +
                        (0 + cell_lat | species) +
                        (0 + season | species),
                    data = scc_estimates,weights=scc_estimates$kl_div)

dotplot(ranef(m_model_q5,condVar=T))
plot_model(m_model_q5,type="pred",terms="season") + labs(x="season",y="foraging offset hour",title="") +
    theme_grey()

m_model_q50 <- lmer(q50 ~ cell_lat + season +
                       (1|cell) + (1|species) +
                       (0 + cell_lat | species) +
                       (0 + season | species),
                   data = scc_estimates,weights=scc_estimates$kl_div)

dotplot(ranef(m_model_q50,condVar=T))
plot_model(m_model_q50,type="pred",terms="season") + labs(x="season",y="foraging offset hour",title="") +
    theme_grey
summary(m_model_q5)
summary(m_model_q50)
summary(m_model_q95)

table(scc_estimates$species)
scc_estimates$season <- as.factor(scc_estimates$season)

scc_estimates <- merged

# east of the rockies
j_coenia <- dplyr::filter(scc_estimates,species=="Junonia coenia Hübner, 1822")
j_coenia_model <- lm(q95 ~ season + tmax + daylength, data=j_coenia,weights=j_coenia$kl_div)
summary(j_coenia_model)
# seasonal and lat effects

# most widespread of all bflies globally, migratory
v_cardui <- dplyr::filter(scc_estimates,species=="Vanessa cardui (Linnaeus, 1758)")
v_cardui_model <- lm(q5 ~ season + tmax, data=v_cardui,weights=v_cardui$kl_div)
summary(v_cardui_model)
# no seasonal effects, lat effect

# most widespread of all bflies globally, migratory
v_rapae <- dplyr::filter(scc_estimates,species=="Pieris rapae (Linnaeus, 1758)")
v_rapae_model <- lm(q5 ~ season + tmax, data=v_rapae,weights=v_rapae$kl_div)
summary(v_rapae_model)

# most widespread of all bflies globally, migratory
p_glaucus <- dplyr::filter(scc_estimates,species=="Papilio glaucus Linnaeus, 1758")
p_glaucus_model <- lm(q5 ~ season + cell_lat, data=p_glaucus,weights=p_glaucus$kl_div)
summary(p_glaucus_model)
# no effects

# most widespread of all bflies globally, migratory
p_polibetes <- dplyr::filter(scc_estimates,species=="Papilio polibetes Stoll, 1781")
p_polibetes_model <- lm(q5 ~ season + cell_lat, data=p_polibetes,weights=p_polibetes$kl_div)
summary(p_polibetes_model)
# no effects

# most widespread of all bflies globally, migratory
p_polibetes <- dplyr::filter(scc_estimates,species=="Papilio polibetes Stoll, 1781")
p_polibetes_model <- lm(q5 ~ season + cell_lat, data=p_polibetes,weights=p_polibetes$kl_div)
summary(p_polibetes_model)
# no effects

Battus philenor (Linnaues, 1771)

# most widespread of all bflies globally, migratory
b_philenor <- dplyr::filter(scc_estimates,species=="Battus philenor (Linnaues, 1771)")
b_philenor_model <- lm(q5 ~ season + cell_lat, data=b_philenor,weights=b_philenor$kl_div)
summary(b_philenor_model)

# most widespread of all bflies globally, migratory
v_atalanta <- dplyr::filter(scc_estimates,species=="Vanessa atalanta (Linnaeus, 1758)")
v_atalanta_model <- lm(q95 ~ season + cell_lat, data=v_atalanta,weights=v_atalanta$kl_div)
summary(v_atalanta_model)


# no effects
for_clustering <- cbind(scc_estimates$q5,scc_estimates$q50,scc_estimates$q95)

clusters <- hclust(dist(for_clustering[,1:3]))
plot(clusters)

clusterCut <- cutree(clusters, 3)
plot(clusterCut)
table(clusterCut,)

clusters = cutree(hclust(dist(for_clustering[,1:3])), k=3)# get 5 clusters

# function to find medoid in cluster i
clust.centroid = function(i, dat, clusters) {
    ind = (clusters == i)
    colMeans(dat[ind,])
}

sapply(unique(clusters), clust.centroid, for_clustering, clusters)

pca <- prcomp(for_clustering[,c(1:3)], center = TRUE,scale. = TRUE)

summary(pca)
pca$rotation

library(devtools)
install_github("vqv/ggbiplot")

library(ggbiplot)
ggbiplot(pca)
biplot(pca, scale = 0)

cor(merged$q95,merged$tmin)

model <- lm(q50 ~ season + tmax + daylength + precip + srad + vp, data=merged) #,weights=merged$kl_div)
summary(model)

library(lme4)
model <- lmer(q5 ~ season + tmax + daylength + precip + srad + vp +
                       (1|cell) + (1|species) +
                       (0 + cell_lat | species) +
                   data = merged,weights=merged$kl_div)

model<- lmer(q50 ~ tmax + daylength + precip + srad + vp +
                 (1|cell) + (1|species),
             data = merged,weights=merged$kl_div)
merged$season <- as.factor(merged$season)
summary(model)

model <- lm(q95 ~ season + tmax + tmin + daylength + precip + srad + vp, data=merged, weights=merged$kl_div)
model <- lm(q95 ~ season, data=merged, weights=merged$kl_div) ##,weights=merged$kl_div)
summary(model)
#q5 - reduced in high temps, increased slightly with day length, reduced slightly by precip
#q50 - decreased in high temps
#q95 - decreased in high temps



library(ape)
library(stringr)
library(magrittr)

sccs <- merged
# create traits df from sccs
traits <- data.frame(sccs$species,sccs$q50,sccs$q5,sccs$q95)
colnames(traits) <- c("species","q50","q5","q95")

library(dplyr)
# summarize traits to species
traits <- traits %>%
    group_by(species) %>%
    summarise(mean_q50 = mean(q50),mean_q5 = mean(q5), mean_q95 = mean(q95))

traits <- data.frame(traits)
genus <- strsplit(traits$species, " ") %>% sapply(extract2, 1)
spe <- strsplit(traits$species, " ") %>% sapply(extract2, 2)
traits$species <- paste(genus,spe)

colnames(traits) <- c("clade","trait","x","y")

# load bf tree
library(ape)
bf_tree <- read.tree("data/misc/bf_species_tree.txt")
# mod tree tips
library(dplyr)
genus_tips <- strsplit(bf_tree$tip.label, " ") %>% sapply(extract2, 2)
spe_tips <- strsplit(bf_tree$tip.label, " ") %>% sapply(extract2, 3)
tips <- paste(genus_tips,spe_tips)
tips <- str_replace(tips,"'","")

bf_tree$tip.label <- tips

ape_tree <- bf_tree

colnames(traits) <- c("clade","1","2","trait")

plotPhyloEffects(traits,ape_tree)

merged$season <- as.factor(merged$season)

library(lme4)
model5 <- lmer(q5 ~ season + tmax + daylength + precip + srad + vp +
                 (1|cell) + (1|species) +
                 (0 + tmax | species),
                 data = merged,weights=merged$kl_div)

model95 <- lmer(q95 ~ season + tmax + daylength + precip + srad + vp +
                   (1|cell) + (1|species) +
                   (0 + tmax | species),
               data = merged,weights=merged$kl_div)
