
#import ant species colors
ant_species_colors <- read.csv("data/ant_species_colors.csv")
ant_species_colors$scientific_name <- ant_species_colors$term
View(ant_species_colors)

# capitalize
library(stringr)

# make usa_ant_species names lowercase so they match to color data
usa_ant_species$scientific_name <- str_to_lower(usa_ant_species$scientific_name)

# merge color data into Usa_ant_species
usa_ant_species <- merge(usa_ant_species, ant_species_colors, by ="scientific_name")

# test correlation
cor.test(usa_ant_species$propNight, usa_ant_species$meanlightness)

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
