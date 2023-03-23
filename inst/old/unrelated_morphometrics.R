morpho <- read.csv("data/AntMorphoSheet.csv")
morpho$flatness <- as.numeric(morpho$flatness)
morpho$species.lightness.mean <- as.numeric(morpho$species.lightness.mean)
View(morpho)

forplot <- as.matrix(cbind(morpho$species,morpho$flatness))
forplot <- na.omit(forplot)
forplot <-t(forplot)
plot(forplot,xlim = c(0,0))
barplot(forplot)
View(forplot)

barplot(morpho$flatness, xlab="Species", ylab="Roundness")
barplot

forplot <-t(forplot)
View(forplot)
xlab <- "Gender", names = morpho$species

install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)
morpho %>%
    ggplot(aes(species,flatness))+
    geom_col() +
    labs(title="Barplot with geom_col()")
ggsave("simple_barplot_with_R_ggplot2.png")
install.packages("tidyr")
library(tidyr)

#START

morpho <- read.csv("data/AntMorphoSheet.csv")

morpho$flatness <- as.numeric(morpho$flatness)
morpho$species.lightness.mean <- as.numeric(morpho$species.lightness.mean)
morpho$species.orangeness.mean <- as.numeric(morpho$species.orangeness.mean)

View(morpho)


temp <- morpho %>% drop_na(species.orangeness.mean) %>% drop_na(flatness)
temp %>%
    ggplot(aes(species,roughness_factor))+
    geom_col() +
    coord_flip(ylim = c(1,1.5))+
    labs(title="Roughness of Ant Species",
         x="Species", y= "Roughness")

temp %>%
    ggplot(aes(species.orangeness.mean,flatness))+
    geom_point() +
    coord_flip(ylim = c(0.5,2.25))+
    labs(title="Orangeness vs Flatness of Ant Species",
         x="Orangeness", y= "Flatness")

    geom_text(aes(label = round(flatness, 1)), nudge_y= -3, color="white")

morpho$species.lightness.mean

cor.test(temp$flatness,temp$species.lightness.mean)
cor.test(temp$roughness_factor,temp$species.lightness.mean)

cor.test(temp$flatness,temp$species.orangeness.mean)
cor.test(temp$roughness_factor,temp$species.orangeness.mean)

cor.test(temp$flatness,temp$roughness_factor)
