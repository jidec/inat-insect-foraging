install.packages("phenesse")
library(phenesse)

# Gather sightings of iNaturalist observations for four species:
# Danaus plexippus, Speyeria cybele, Rudbeckia hirta, and Asclepias syriaca
# Estimate when the first 50 percent of individuals of the milkweed species
# Asclepias syriaca have been observed.
data(inat_examples)
a_syriaca <- subset(inat_examples, scientific_name == "Asclepias syriaca")
weib_percentile(a_syriaca$doy, percentile = 0.5, iterations = 500)
View(a_syriaca)

# Estimate when 90 percent of individuals of the milkweed species A. syriaca
# have been observed, using only 100 iterations for quicker processing. To
# get a more stable result, more iterations should be used.
weib_percentile(a_syriaca$doy, percentile = 0.9, iterations = 100)

# on ants:
campo_p <- filter(usa_ants, scientific_name == "Camponotus pennsylvanicus")
wp <- weib_percentile(campo_p$local_hour, percentile = 0.5, iterations = 50)
