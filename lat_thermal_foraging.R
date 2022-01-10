# test correlations and models based on latitude and thermal hypotheses

# quick cor tests between hour/midday dist and lat
cor.test(usa_ants$local_hour, usa_ants$latitude)
cor.test(usa_ants$midday_dist, usa_ants$latitude)

cor.test(campo_p$midday_dist, campo_p$latitude)
cor.test(campo_c$midday_dist, campo_c$latitude)
cor.test(campo_a$midday_dist, campo_a$latitude)

#linear mixed model, lat and season are fixed effects while taxonomy is random effect
library(lme4)
mixed <- lmer(midday_dist ~ latitude + season + (1|taxon_subfamily_name/taxon_genus_name/scientific_name), data = usa_ants)
summary(mixed)
#incorporate day length bias as covariate
#drop winter seasons
#just bio1 from worldclim
#continuous spring to fall values
#consider interannual variation
#using annotations to vet species
#gen 240 watt hours
#leds at 60 watts - 4 hrs

#also do variance partitioning
library(ape)
#cant remember why this isnt working...
varpart <- varcomp(mixed, scale = FALSE, cum = FALSE)

#midday dist rasters
#midday dist is distance of the mean foraging time from 3 PM local time
source("src/plotForagingTimeRasters.R")
plotForagingTimeRasters(usa_ants,
                        season_num = "0", raster_res = 2, raster_function = mean)
plotForagingTimeRasters(usa_ants, scientificName = "Camponotus pennsylvanicus",
                        season_num = "0", raster_res = 1, raster_function = mean, min_cell_sample = 30)
plotForagingTimeRasters(usa_ants, scientificName = "Camponotus castaneus",
                        season_num = "0", raster_res = 1, raster_function = mean, min_cell_sample = 30)

# take a look at adj foraging times
adj_usa_ants$season <- as.factor(adj_usa_ants$season)
lin <- lm(local_hour_adj ~ latitude + season, data = adj_usa_ants)

# species
s <- "Camponotus americanus"
f <- dplyr::filter(adj_usa_ants, species == s)
lin <- lm(midday_dist ~ latitude + season_actual, data = f)
summary(lin)

f <- dplyr::filter(usa_ants, scientific_name == s)
lin <- lm(midday_dist ~ latitude + season, data = f)
summary(lin)

# all usa ants
lin <- lm(midday_dist ~ latitude + season_actual + species, data = adj_usa_ants)
summary(lin)
lin <- lm(midday_dist ~ latitude + season, data = usa_ants)
summary(lin)
