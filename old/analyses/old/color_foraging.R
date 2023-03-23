
#import ant species colors
ant_species_colors <- read.csv("data/ant_species_colors.csv")
ant_species_colors$species <- ant_species_colors$term
View(ant_species_colors)

# capitalize
library(stringr)

for_color <- species_season_cells_species

# make usa_ant_species names lowercase so they match to color data
for_color$species <- str_to_lower(for_color$species)

# merge color data into Usa_ant_species
for_color <- merge(for_color, ant_species_colors, by ="species")

# test correlation
cor.test(for_color$mean_nightm, for_color$meanlightness)
View(for_color)
model <- lm(night_metric ~ meanlightness, data=for_color)
View(for_color)
plot(for_color$mean_nightm,for_color$meanlightness)
summary(model)

ggplot(for_color, aes(x=mean_nightm, y=meanlightness)) + geom_point() + labs(x="Night foraging metric of species (offset time / onset time)",y="Color lightness of species")
# test with night coding
# get season adjusted night coding
# see if coded night foragers are actually night foragers

# are night foraging ants geographically structured?

# testing Miles' field observations:

# yellow Myrmecocystus are nocturnal
# our sampling resolution is pretty poor in this genus, but
# this seems to be the case where mexicanus and testaceus which have the highest prop night
# are lighter than mendax, for example
# but really our rez is too low to be sure - can look for more natural history data and vet

# yellow-orange Camponotus are primarily nocturnal with exceptions
test <- usa_ant_species %>% filter(str_detect(scientific_name, "campo")) %>% filter(n > 50)
cor.test(test$propNight, test$meanlightness)
# this seems to be the case, but sensitive to sample size filtering and high p-value
# maybe that is the "with exceptions" part

# yellow orange formica crepuscular, latitudinally limited and seasonally varied
test <- usa_ant_species %>% filter(str_detect(scientific_name, "formica")) %>% filter(n > 30)

#this does seem to be the case, no cor with lightness but a strong but insignificant positive cor
# with yellowness and orangeness
cor.test(test$meanMD, test$meanlightness)
cor.test(test$meanMD, test$meanyellowness)
cor.test(test$meanMD, test$meanorangeness)

# no cor with dorymyrmex
test <- usa_ant_species %>% filter(str_detect(scientific_name, "dory")) %>% filter(n > 5)
cor.test(test$propNight, test$meanlightness)
# there is a strong but insigificant corr, super tiny sample size 4 species
