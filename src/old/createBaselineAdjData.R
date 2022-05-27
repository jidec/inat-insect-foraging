
# given iNat data, create a grid over its range, split data into species-season-cell combinations, and
# assign p and f statistics by ANOVA with baseline for that season-cell

#data = usa_ants
#cellsize_miles = 200
#weib_iters = 50
#weib_ci_iters=10
#weib_ci_bootstraps=10
#ncpus=4

createBaselineAdjData <- function(data, cellsize_miles,
                                  weib_iters=50, weib_ci_iters=10,weib_ci_bootstraps=10, ncpus = 4,
                                  use_genus=FALSE,simple=FALSE,firstn=10000,min_ants_n=10,cutoffn=250,print=FALSE){

library(dplyr)
library(spatialrisk)
library(plyr)
library(sf)
library(sp)
library(phenesse)

# turn data into SpatialPoints
coordinates(usa_insects) <- cbind(usa_insects$latitude,usa_insects$longitude)
coordinates(data) <- cbind(data$latitude,data$longitude)

# create SpatialGridDataFrame from data
grid <- makegrid(data, cellsize = cellsize_miles/69)
grid$id <- 1:length(grid$x1)
coordinates(grid) <- cbind(grid$x1,grid$x2)
gridded(grid) <- TRUE
plot(grid,add=TRUE)
grid <- as(grid, "SpatialGridDataFrame")

# spatial join and add cell info to points
x <- sp::over(data,grid)
data <- as.data.frame(data)
data$grid_id <- x$id
data$cell_lat <- x$x1
data$cell_long <- x$x2

# also add cell info to all obs points
# turn sampled_usa_insects data into SpatialPoints
#coordinates(usa_insects) <- cbind(usa_insects$latitude,usa_insects$longitude)
x <- sp::over(usa_insects,grid)
usa_insects <- as.data.frame(usa_insects)
usa_insects$grid_id <- x$id
usa_insects$cell_lat <- x$x1
usa_insects$cell_long <- x$x2

# get cells and prep output
unique_cells <- unique(data$grid_id)
out <- data.frame(matrix(ncol = 21, nrow = 220))

#percent of a given clade or species to all obs
#proportion sampled for that taxon across the year
# nests just noise

index <- 1
# for each cell...
for(i in 1:length(unique_cells))
{
    #i = 1
    # filter for obs in cell
    cell_obs <- dplyr::filter(data,grid_id == unique_cells[i])
    baseline_cell_obs <- dplyr::filter(usa_insects,grid_id == unique_cells[i])

    species_in_cell <- unique(cell_obs$scientific_name)

    if(use_genus){
        species_in_cell <- unique(cell_obs$taxon_genus_name)
    }

    # for each species found in the cell...
    for(k in 1:length(species_in_cell))
    {
        #k = 1
        # filter for obs of species
        species_obs <- dplyr::filter(cell_obs,scientific_name == species_in_cell[k])
        sp <- species_in_cell[k]

        if(use_genus){
            species_obs <- dplyr::filter(cell_obs,taxon_genus_name == species_in_cell[k])
            sp <- species_in_cell[k]
        }

        # for each season...
        for(j in 1:4)
        {
            #j = 3
            season_obs <- dplyr::filter(species_obs, season == j)
            baseline_season_obs <- dplyr::filter(baseline_cell_obs, season == j)

            if(nrow(baseline_season_obs) > cutoffn){
                baseline_season_obs <- sample_n(baseline_season_obs,cutoffn)
            }

            if(nrow(season_obs) > cutoffn){
                season_obs <- sample_n(season_obs,cutoffn)
            }

            if(length(season_obs$id) > min_ants_n && length(baseline_season_obs$id) > min_ants_n)
            {
                #------------------------
                ## compute anova and save p and f statistics
                #hist(baseline_season_obs$local_hour,axes=TRUE)
                #hist(season_obs$local_hour)
                season_obs$category = "ants"
                baseline_season_obs$category = "baseline"
                library(plyr)
                stacked <- rbind.fill(season_obs,baseline_season_obs)
                model <- aov(local_hour ~ category, data = stacked)

                p <- summary(model)[[1]][1,5] # p value
                f <- summary(model)[[1]][1,4] # f statistic

                #------------------------
                ## compute manova and save p and f statistics

                if(print){
                    print("adding tile ids")}

                # add tile IDs to baseline season obs
                baseline_season_obs <- addDaymetTileIDs(baseline_season_obs)
                season_obs <- addDaymetTileIDs(season_obs)
                stacked <- rbind.fill(season_obs,baseline_season_obs)

                if(print){
                    print("adding daymet data")}

                # eventually can delete this (and tileID adding), as usa_ants and usa_insects will already contain these values
                # gave up on vectorizing this
                for(d in 1:nrow(stacked)){
                    # get row (one observation, one point)
                    row <- stacked[d,]

                    # filter for year and yearday and tile
                    library(lubridate)

                    dm_cell <- filter(daymet_150m,year==year(row$datetimes) & yday==yday(row$datetimes) & tile == row$daymet_tile)#& yday==yday(row$datetimes) & tile==row$daymet_tile)

                    if(nrow(dm_cell) > 0)
                    {
                        # assign back to stacked
                        stacked$temp_min[d] <- filter(dm_cell,measurement =="tmax..deg.c.")$value
                        stacked$precip[d] <- filter(dm_cell,measurement =="prcp..mm.day.")$value
                    }
                    else{
                        stacked$temp_min[d] <- NA
                        stacked$precip[d] <- NA
                    }

                    if(d %% 10 == 0){
                        print(d)}
                }

                if(print){
                    print("manova started")}

                library(tidyr)
                stacked <- drop_na(stacked, c(temp_min,precip))
                model <- manova(cbind(local_hour,temp_min,precip) ~ category, data=stacked)

                m_p <- summary(model)[[4]][1,6] # p value
                m_f <- summary(model)[[4]][1,3] # f statistic

                if(!simple)
                {
                #------------------------
                ## calculate mid weib percentile for ants

                if(print){
                    print("calculating mid weib percentile")}

                mid <- weib_percentile(season_obs$local_hour, percentile = 0.5, iterations = weib_iters)

                #------------------------
                ## compute onset cis and check if overlapping

                if(print){
                    print("onset and offset ci")}

                # ant onset ci
                ant_onset_ci <- weib_percentile_ci(observations = season_obs$local_hour, iterations = weib_ci_iters,
                                   percentile = 0.1, bootstraps = weib_ci_bootstraps,
                                   parallelize = "multicore", ncpus = ncpus)

                # save value
                onset <- ant_onset_ci$estimate

                bl_ci_hours <- baseline_season_obs$local_hour

                # compute bl ci
                bl_onset_ci <- weib_percentile_ci(observations = bl_ci_hours, iterations = weib_ci_iters,
                                                  percentile = 0.1, bootstraps = weib_ci_bootstraps,
                                                  parallelize = "multicore", ncpus = ncpus)

                onset_ci_ol <- (ant_onset_ci$low_ci < bl_onset_ci$high_ci) & (ant_onset_ci$high_ci > bl_onset_ci$low_ci) |
                               (ant_onset_ci$high_ci < bl_onset_ci$low_ci) & (ant_onset_ci$low_ci > bl_onset_ci$high_ci)

                #------------------------
                ## compute OFFSET cis and check if overlapping

                # ant offset ci
                ant_offset_ci <- weib_percentile_ci(observations = season_obs$local_hour, iterations = weib_ci_iters,
                                                   percentile = 0.9, bootstraps = weib_ci_bootstraps,
                                                   parallelize = "multicore", ncpus = ncpus)

                # save value
                offset <- ant_offset_ci$estimate

                # compute bl ci
                bl_offset_ci <- weib_percentile_ci(observations = bl_ci_hours, iterations = weib_ci_iters,
                                                  percentile = 0.9, bootstraps = weib_ci_bootstraps,
                                                  parallelize = "multicore", ncpus = ncpus)

                offset_ci_ol <- (ant_offset_ci$low_ci < bl_offset_ci$high_ci) & (ant_offset_ci$high_ci > bl_offset_ci$low_ci) |
                    (ant_offset_ci$high_ci < bl_offset_ci$low_ci) & (ant_offset_ci$low_ci > bl_offset_ci$high_ci)

                bl_onset_hour <- bl_onset_ci$estimate
                bl_offset_hour <- bl_offset_ci$estimate
                bl_center_hour <- 0

                ncenter <- sum(season_obs$local_hour > mid - 1.5 & season_obs$local_hour < mid + 1.5)

                bl_center <- median(baseline_season_obs$local_hour)
                bl_ncenter <- sum(baseline_season_obs$local_hour > bl_center - 1.5 & baseline_season_obs$local_hour < bl_center + 1.5)

                }
                else #use simple
                {
                    onset <- unname(quantile(season_obs$local_hour,0.1))
                    offset <- unname(quantile(season_obs$local_hour,0.9))
                    mid <- unname(quantile(season_obs$local_hour,0.5))
                    onset_ci_ol <- 0
                    offset_ci_ol <- 0
                }
                #------------------------
                ## finish

                colnames(out) <- c("species","season","cell_id","onset_hour","center_hour","offset_hour",
                                   "anova_p","anova_f","manova_p","manova_f", "onset_ci_overlaps", "offset_ci_overlaps",
                                   "n","nbase","cell_lat","cell_lon","bl_onset_hour","bl_center_hour","bl_offset_hour","center_power","bl_center_power")

                #print(c(sp,i,j,p,f,length(season_obs$id)))
                out[index,] <- c(sp,j,unique_cells[i],onset,mid,offset,
                                 p,f,m_p,m_f,onset_ci_ol,offset_ci_ol,
                                 nrow(season_obs),nrow(baseline_season_obs),season_obs$cell_lat[1],season_obs$cell_long[1],
                                 bl_onset_hour, bl_center_hour, bl_offset_hour, ncenter, bl_ncenter)
                #index <- index + 1
            }
            print(index)
            index <- index + 1
            if(index >= firstn){
                break}
        }
    }
}
out$n <- as.numeric(out$n)
out$anova_p <- as.numeric(out$anova_p)
out$anova_f <- as.numeric(out$anova_f)
out$onset_hour <- as.numeric(out$onset_hour)
out$center_hour <- as.numeric(out$center_hour)
out$offset_hour <- as.numeric(out$offset_hour)

out$cell_lat <- as.numeric(out$cell_lat)
out$cell_lon <- as.numeric(out$cell_lon)
out$midday_dist <- abs(out$center_hour - 15)
library(stringr)
out$genus <- str_split_fixed(out$species, " ", n=2)[,1]

return(out)
}


pheno_fun <- function(data){
    #df_dup <- expandRows(data_list2[[x]], count = "local_hour")
    q5  <- tryCatch(phenesse::quantile_ci(observations = data, percentile = 0.05),
                    error = function(e) data.frame(estimate = NA, low_ci = NA, high_ci = NA))
    q50 <- tryCatch(phenesse::quantile_ci(observations = data, percentile = 0.5),
                    error = function(e) data.frame(estimate = NA, low_ci = NA, high_ci = NA))
    q95 <- tryCatch(phenesse::quantile_ci(observations = df$local_hour, percentile = 0.95),
                    error = function(e) data.frame(estimate = NA, low_ci = NA, high_ci = NA))
    rdf <- data.frame(q5 = q5$estimate, q5_low = q5$low_ci, q5_high = q5$high_ci,
                      q50 = q50$estimate, q50_low = q50$low_ci, q50_high = q50$high_ci,
                      q95 = q95$estimate, q95_low = q95$low_ci, q95_high = q95$high_ci) %>%
    return(rdf)
}
