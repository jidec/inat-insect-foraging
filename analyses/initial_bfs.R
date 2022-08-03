scc_estimates_bfs <- readRDS("data/scc_estimates_bfs")
library(stringr)
scc_hours_split <- str_split(scc_estimates_bfs[[2]],-1)

merged
scc_estimates <- merged

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

scc_estimates_bfs[[2]][[1]]

hist(bf_bl_hours[[2]])
hist(bf_hours[[2]])
scc_estimates <- scc_estimates_bfs[[1]]
scc_estimates[1]
scc_estimates$season <- as.factor(scc_estimates$season)

q5_model <- lm(q5 ~ season + tmax, data=scc_estimates)
summary(q5_model)

q5_model_w <- lm(q5 ~ season + tmax + daylength, data=scc_estimates,weights=scc_estimates$kl_div)
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


m_model_q95 <- lmer(q95 ~ cell_lat + season +
                  (1|cell) + (1|species) +
                  (0 + cell_lat | species) +
                  (0 + season | species),
              data = scc_estimates,weights=scc_estimates$kl_div)

dotplot(ranef(m_model_q95,condVar=T))
plot_model(m_model_q95,type="pred",terms="season") + labs(x="season",y="foraging offset hour",title="") +
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
model<- lmer(q5 ~ season + tmax + daylength + precip + srad + vp +
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
bf_tree <- read.tree("data/misc/bf_species_tree.txt")

View(ape_tree$tip.label)

# mod tree tips
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
