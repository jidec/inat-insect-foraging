# descriptive analyses and visualizations of the shape and scale of foraging data

# create foraging time plots using plotForagingTimeHists(data, species_name, yaxismax)
source("src/plotForagingTimeHists.R")
plotForagingTimeHists(usa_ants)
plotForagingTimeHists(usa_ants, "Camponotus pennsylvanicus", 1000)
plotForagingTimeHists(usa_ants, "Camponotus castaneus", 200)
plotForagingTimeHists(usa_ants, "Solenopsis invicta", 800)

#assess research grade vs non (must not filter for research in earlier lines)
nrow(filter(usa_ants, quality_grade == "research")) #55036 samples
nrow(usa_ants)#139919 samples
nrow(filter(campo_p, quality_grade == "research")) #8866 samples
nrow(campo_p) #like 10k samples
