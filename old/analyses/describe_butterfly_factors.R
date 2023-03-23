library(plotly)
library(ggplot2)

# plot species
hist(table(sscs$species), breaks=200,main="40% of species with sscs have 3 or less sscs")
sum(table(sscs$species) <= 3)
sum(table(sscs$species) > 3)
sum(table(sscs$species) <= 3) / (sum(table(sscs$species) > 3) + sum(table(sscs$species) <= 3))

# plot lat
hist(sscs$lat, main="SSCs are relatively evenly distributed across lat - TODO plot geo heatmap")

library(RColorBrewer)
getPalette = colorRampPalette(brewer.pal(9, "Set1"))
# create an interactive plot showing how families have similar amounts of pigments
fig <- plot_ly(x=wings$col_2_prop, y=wings$col_6_prop, z=wings$col_1_prop,
               type="scatter3d", mode="markers", color=wings$Family,colors=getPalette(50))
axx <- list(title = "% wing brown")
axy <- list(title = "% wing black")
axz <- list(title = "% wing yellow")
fig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))

# same with sexes
fig <- plot_ly(x=wings$col_2_prop, y=wings$col_6_prop, z=wings$col_1_prop,
               type="scatter3d", mode="markers", color=wings$Sex,colors=getPalette(50))
axx <- list(title = "% wing brown")
axy <- list(title = "% wing black")
axz <- list(title = "% wing yellow")
fig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))

# same with hind fore
fig <- plot_ly(x=wings$col_2_prop, y=wings$col_6_prop, z=wings$col_1_prop,
               type="scatter3d", mode="markers", color=wings$wing_type,colors=getPalette(50))
axx <- list(title = "% wing brown")
axy <- list(title = "% wing black")
axz <- list(title = "% wing yellow")
fig %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz))
