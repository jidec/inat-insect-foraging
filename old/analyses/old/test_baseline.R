# read ant and baseline .csvs and make some basic changes, fixing and adding time data
source("src/readPrepData.R")
usa_ants <- readPrepAntData()
usa_ants <- addHourSeason(usa_ants)
usa_insects <- readPrepBaselineInsects(sample=1000000)

# read daymet data and add to ants and baseline
source("src/addDaymetCellData.R")
daymet_data <- read.csv("data/daymet_150m.csv",sep = ",")
usa_ants <- addDaymetTileIDs(usa_ants,nsubset = 100000)
usa_ants <- addDaymetCellData(usa_ants)

usa_insects <- addDaymetTileIDs(usa_insects,nsubset = 50000)
usa_insects <- addDaymetCellData(usa_insects)

source("src/createBaselineAdjustedData3.R")
species_season_cells <- createBaselineAdjData3(usa_ants,cellsize_miles=200,firstn=2,print=TRUE)


par(mfrow=c(2,1))

# look at obs histograms
row <- obs[[1]]
onset_low <- out$q5_low[1]
onset_high <- out$q5_high[1]
offset_low <- out$q95_low[1]
offset_high <- out$q95_high[1]

bl_onset_low <- out$bl_q5_low[1]
bl_onset_high <- out$bl_q5_high[1]
bl_offset_low <- out$bl_q95_low[1]
bl_offset_high <- out$bl_q95_high[1]

plotAntBaselineHists <- function(i)
{
    hours <- obs[[i]]
    split_index <- which(hours == -1)
    ant_hours <- hours[1:split_index-1]
    bl_hours <- hours[split_index+1:length(row)]
    bl_hours <- bl_hours[!is.na(bl_hours)]

    ant_hours <- as.data.frame(ant_hours)
    bl_hours <- as.data.frame(bl_hours)

    antplot <- ggplot(ant_hours, aes(x=ant_hours)) + geom_histogram(bins = 10) + xlim(0,24) +
        geom_point(aes(x=out$q5_low[i], y=0), colour="black") + geom_point(aes(x=out$q5_high[i], y=0), colour="black") +
        geom_point(aes(x=out$q95_low[i], y=0), colour="black") + geom_point(aes(x=out$q95_high[i], y=0), colour="black") +
        labs(title=paste("Foraging histogram of",out$species[i], "in season", out$season[i], "in cell", out$cell[i]), y="Occurrences", x="Local hour observed",
             caption=paste("manova p:",out$manova_p[i],", anova p:", out$anova_p[i], ", onset overlaps:", out$onset_ci_ol[i], ", offset overlaps", out$offset_ci_ol[i]))

    blplot <- ggplot(bl_hours, aes(x=bl_hours)) + geom_histogram(bins = 10) + xlim(0,24) +
        geom_point(aes(x=out$bl_q5_low[i], y=0), colour="black") + geom_point(aes(x=out$bl_q5_high[i], y=0), colour="black") +
        geom_point(aes(x=out$bl_q95_low[i], y=0), colour="black") + geom_point(aes(x=out$bl_q95_high[i], y=0), colour="black") +
        labs(caption=paste("manova p:",out$manova_p[i],", anova p:", out$anova_p[i], ", onset overlaps:", out$onset_ci_ol[i], ", offset overlaps", out$offset_ci_ol[i]))

    return(list(antplot,blplot))
}

obs_out_tuple <- getObsOnly(usa_ants,usa_insects,cellsize_miles = 250,first_n = 500)
test <- sample(usa_insects,5000)
plots <- plotAntBaselineHists(2)
plots[[1]]
plots[[2]]

View(out)
out_row
split_index <- which(row == -1)
ant_hours <- row[1:split_index-1]
bl_hours <- row[split_index+1:length(row)]
bl_hours <- bl_hours[!is.na(bl_hours)]

ant_hours <- as.data.frame(ant_hours)
bl_hours <- as.data.frame(bl_hours)

ggplot(ant_hours, aes(x=ant_hours)) + geom_histogram(bins = 10) + xlim(0,24) +
    geom_point(aes(x=out$q5_low[1], y=0), colour="black") + geom_point(aes(x=out$q5_high[1], y=0), colour="black") +
    geom_point(aes(x=out$q95_low[1], y=0), colour="black") + geom_point(aes(x=out$q95_high[1], y=0), colour="black")

ggplot(bl_hours, aes(x=bl_hours)) + geom_histogram(bins = 10) + xlim(0,24) +
    geom_point(aes(x=out$bl_q5_low[1], y=0), colour="black") + geom_point(aes(x=out$bl_q5_high[1], y=0), colour="black")
    geom_point(aes(x=out$bl_q95_low[1], y=0), colour="black") + geom_point(aes(x=out$bl_q95_high[1], y=0), colour="black")

hist(ant_hours,axes=TRUE,xlim=c(0,24))
hist(bl_hours,axes=TRUE,xlim=c(0,24))


library(ggplot2)
ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point()
points(0)
sum(bl_hours == 22)

hist(bl_hours)

mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}
