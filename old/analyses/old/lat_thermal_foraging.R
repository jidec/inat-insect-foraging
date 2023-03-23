# test correlations and models based on latitude and thermal hypotheses

# quick cor tests between hour/midday dist and lat
cor.test(usa_ants$local_hour, usa_ants$latitude)
cor.test(usa_ants$midday_dist, usa_ants$latitude)

library(lme4)
# linear mixed model, lat and season are fixed effects while taxonomy is random effect
mixed <- lmer(midday_dist ~ latitude + season + (1|taxon_subfamily_name/taxon_genus_name/scientific_name), data = usa_ants)
summary(mixed)

source("src/plotForagingTimeRasters.R")
# midday dist is distance of the mean foraging time from 3 PM local time
plotForagingTimeRasters(usa_ants,
                        season_num = "0", raster_res = 2, raster_function = mean)
plotForagingTimeRasters(usa_ants, scientificName = "Camponotus pennsylvanicus",
                        season_num = "0", raster_res = 1, raster_function = mean, min_cell_sample = 30)
plotForagingTimeRasters(usa_ants, scientificName = "Camponotus castaneus",
                        season_num = "0", raster_res = 1, raster_function = mean, min_cell_sample = 30)

View(table(species_season_cells$species))

library(dplyr)
species_season_cells <- species_season_cells %>% group_by(species) %>% filter(n()>1)
species_season_cells <- dplyr::filter(species_season_cells, species != "Solenopsis invicta")

# nich breadth from distribution models

table(species_season_cells$species[table(species_season_cells$species) == 1])

hist(species_season_cells$cell_lat)
source("src/plotSpeciesSeasonCells.R")
# plot species-season-cells
table(species_season_cells$species)
plotSpeciesSeasonCells(data=species_season_cells, field = "q5", sp="Pogonomyrmex barbatus",sigthresh=0.1)
plotSpeciesSeasonCells(data=species_season_cells, field = "q5",nthresh=5,sigthresh=0.1)
plotSpeciesSeasonCells(data=sp_season_cell, field = "midday_dist", sp="Camponotus pennsylvanicus", nthresh=10,sigthresh=0.1)

plotSpeciesSeasonCells(data=sp_season_cell, field = "center_hour", sp=NULL, se="3",nthresh=5,sigthresh=0.1)

plotSpeciesSeasonCells(data=sp_season_cell, field = "onset_hour", sp="Camponotus castaneus", se = "3", nthresh=0,sigthresh=0.1)

# plot distributions from a cell


# old, unsure if this works
scc_sf <- st_as_sf(sp_season_cell, coords = c("cell_lon", "cell_lat"))
plot(scc_sf["midday_dist"],pch=15,cex = 3,axes = TRUE)
shape <- readOGR(dsn = "data/cb_2018_us_state_5m")
plot(shape, bg="transparent", add=TRUE)



usa_ants$midday_dist <- as.numeric(usa_ants$midday_dist)
usa_ants$temp_max <- as.numeric(usa_ants$temp_max)
usa_ants$temp_min <- as.numeric(usa_ants$temp_min)
usa_ants$precip <- as.numeric(usa_ants$precip)
usa_ants$vapor_pressure <- as.numeric(usa_ants$vapor_pressure)
usa_ants$solar_rad <- as.numeric(usa_ants$solar_rad)
usa_ants$local_hour <- as.numeric(usa_ants$local_hour)
usa_ants$season <- as.factor(usa_ants$season)
model <- lm(midday_dist ~ temp_min + temp_max + precip + vapor_pressure + solar_rad, data = usa_ants)
model <- lm(midday_dist ~ scientific_name, data=usa_ants)
summary(model)
usa_ants$season

source("src/createBaselineDiffData3.R")
species_season_cells <- finalizeCols(species_season_cells)
summary(model)
model <- lm(night_metric ~ cell_lat + season, data = species_season_cells)
summary(model)
quantile(model$effects,0.9)
View(model$effects[model$effects > 2.9])


species_season_cells <- dplyr::filter(species_season_cells, is_sig_onset_offset == TRUE)
temp <- dplyr::filter(species_season_cells,season=="4")
mean(temp$q50,na.rm=TRUE)

# bumping down means losing sparsely sampled species
sum(!species_season_cells$onset_ci_ol, na.rm=TRUE)
sum(species_season_cells$anova_p < 0.05, na.rm=TRUE)
sum(species_season_cells$offset_ci_ol < 0.05, na.rm=TRUE)

nrow(sp_season_cells_sig)

# we're getting
# concerns/things to highlight - like a third of species with sccs have just 1 scc
# most

View(model$effects)
model$effects[6:33]
summary(model)

# for all significant cells
model <- lm(q95 ~ cell_lat + season, data = species_season_cells)
summary(model)


for(s in unique(species_season_cells$species))
{
    temp <- dplyr::filter(species_season_cells, species == s)
    model <- lm(q5 ~ cell_lat + season, data = temp)
    print(s)
    print(summary(model))
}
sp_season_cells_sig$cell <- as.factor(sp_season_cells_sig$cell)

library(lme4)

install.packages("lme4")
library(lme4)
# mixed effects model
model <- lmer(q95 ~ cell_lat + season +
                  (1|cell) + (1|species) +
                  (0 + cell_lat | species) +
                  (0 + season | species),
              data = scc_estimates,weights=scc_estimates$kl_div)
summary(model)
help('isSingular')
# mixed effects model
model <- lmer(q95 ~ cell_lat + season +
                (1|species),
              data = species_season_cells)

model <- lmer()
summary(model)
# make 0 length branches for species

# fix a glmm with species as fixed effect, with no slope estimates

# fitting cell - what about that cell beside lat is indicative?
# replace 1|cell with temp and precip
# random effects - fitting leftover variation not by fixed -
# explaining half of leftover variation

# strategy for validating against what's known
# create list of taxon - using ant
# check species season_data

# super exploratory framing - my lab has done work at phenological patterns using iNat, point at Mikes and Daijong's papers
# can we drive reasonable estimates fof foraging patterns
# answer - glimmers of pattterns for now
sum(species_season_cells$manova_p < 0.05)
nrow(species_season_cells)
# motivation for temp and rpecip - differentially determining nonoverlap between groups
# manova - hour is repsonse
install.packages("sjPlot")
install.packages("glmmTMB")
install.packages("lattice")
library(sjPlot)
library(glmmTMB)
library(lattice)
dotplot(ranef(model,condVar=T))
sccs_saved <- species_season_cells
species_season_cells <- species_season_cells3[[1]]
library(ggplot2)
species_season_cells <- read.csv("data/cluster_exports/sp_season_cell_all_iter15.csv")
species_season_cells <- finalizeCols(species_season_cells)
species_season_cells <- dplyr::filter(species_season_cells, is_sig_onset_offset == TRUE)

species_season_cells3[[1]]$season <- as.factor(species_season_cells3[[1]]$season)
seasons_named <- species_season_cells$season
seasons_named <- as.numeric(seasons_named)
seasons_named <- replace(seasons_named,seasons_named==1,"winter")
seasons_named <- replace(seasons_named,seasons_named==2,"spring")
seasons_named <- replace(seasons_named,seasons_named==3,"summer")
seasons_named <- replace(seasons_named,seasons_named==4,"fall")
species_season_cells$season <- species_season_cells$season[order(species_season_cells$season)]
levels(species_season_cells$season) <- c("winter", "spring","summer","fall")
#species_season_cells$season <- as.factor(seasons_named)

# mixed effects model
model <- lmer(q95 ~ cell_lat + season +
                  (1|species),
              data = species_season_cells3[[1]],weights = species_season_cells3[[1]]$kl_div)
summary(model)

temp <-dplyr::filter(species_season_cells, season=="1")
plot_model(model,type="re",terms="species")
View(model)
library(ggplot2)
plot_model(model95,type="pred",terms="season") + labs(x="season",y="foraging offset hour",title="") +
    theme_grey()

View(usa_ants)

# mixed effects model
model <- lmer(q50 ~ cell_lat + season +
                  (1|species),
              data = species_season_cells,weights=species_season_cells$kl_div)
summary(model)
# try type="re"
# singular - situation where model cant fit data properly
# read about how fixed

# interepts for first variable are caculated, just others are caculated with respect to it
# species

model <- lm(q5 ~ cell_lat + season, data = sp_season_cell_campo)
model <- lm(q5 ~ cell_lat + season, data = sp_season_cell_preno)
model <- lm(q5 ~ cell_lat + season, data = sp_season_cell_pogo)
model <- lm(q95 ~ cell_lat + season, data = sp_season_cell_vero)

sp_season_cell_campo <- dplyr::filter(sp_season_cell_all_iter10_sig_most, species == "Camponotus castaneus")
sp_season_cell_preno <- dplyr::filter(sp_season_cell_all_iter10_sig_most, species == "Prenolepis imparis")
sp_season_cell_pogo <- dplyr::filter(sp_season_cell_all_iter10_sig, species == "Pogonomyrmex barbatus")
sp_season_cell_linep <- dplyr::filter(sp_season_cell_all_iter10_sig_most, species == "Linepithema humile")
sp_season_cell_vero <- dplyr::filter(sp_season_cell_all_iter10_sig_most, species == "Veromessor pergandei")

# mixed effects model
library(lme4)
model <- lmer(q50 ~ cell_lat + season +
                  (1|species),
              data = test)

test <- species_season_cells3[[1]]

summary(model)
species_season_cells3[[1]]$season <- as.factor(species_season_cells3[[1]]$season)
model <- lm(q95 ~ cell_lat + season, data=species_season_cells3[[1]],weights=species_season_cells3[[1]]$kl_div)
#plot(species_season_cells3[[1]]$cell_lat,species_season_cells3[[1]]$cell_lon)
summary(model)

model <- lm(q50 ~ cell_lat + season, data = out,weights = species_season_cells3[[1]]$kl_div)

saveRDS(species_season_cells,"sccs.rds")
write.csv(usa_bfs,"usa_bfs.csv")
write.csv(usa_insects,"usa_insects.csv")
