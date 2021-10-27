
# summarize hours by species
usa_ant_species <- usa_ants %>%
    group_by(scientific_name) %>%
    summarise(hour_mode = getmode(hour), sample_size = n())

View(usa_ant_species)

# filter for species with a certain number of observations
usa_ant_species <- filter(usa_ant_species, sample_size >= 10)

#import ant species colors
ant_species_colors <- read.csv("data/ant_species_colors.csv")
View(ant_species_colors)

# capitalize
library(stringr)

# make usa_ant_species names lowercase so they match to color data
usa_ant_species$scientific_name <- str_to_lower(usa_ant_species$scientific_name)

ant_species_colors$scientific_name <- ant_species_colors$term

# merge color data into Usa_ant_species
usa_ant_species <- merge(usa_ant_species, ant_species_colors, by = "scientific_name")

# test correlation
cor.test(usa_ant_species$hour_mode, usa_ant_species$meanlightness)
