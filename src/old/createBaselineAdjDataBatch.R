
# given iNat data and with baseline data loaded, create a grid over its range,
# split data into species-season-cell combinations, and assign values denoting how significantly they
# deviate from the baseline

#data = usa_ants
#cellsize_miles = 200
#weib_iters = 10
#weib_ci_iters=10
#weib_ci_bootstraps=10
#ncpus=4
#use_genus=FALSE
#simple=FALSE
#firstn=10
#min_ants_n=10
#cutoffn=250 # the max number of observations to use when computing weib percentiles

# createBaselineAdjData requires the input data and baseline data to have daymet tile ids and day data assigned
#source("src/readPrepData.R")
#usa_insects <- readPrepBaselineInsects(sample=100000)

#daymet_data <- readPrepDaymetData()
#source("src/addDaymetCellData.R")
#usa_ants <- addDaymetTileIDs(usa_ants,200000)
#temp <- addDaymetCellData(usa_ants)

#usa_ants <- temp
#usa_insects <- addDaymetTileIDs(usa_insects,50000)
#usa_insects <- addDaymetCellData(usa_insects)
#data <- usa_ants

createBaselineAdjDataBatch <- function(data, cellsize_miles=200,
                                       weib_iters=50, weib_ci_iters=10,weib_ci_bootstraps=10, ncpus = 4,
                                       use_genus=FALSE,simple=FALSE,firstn=10000,min_ants_n=10,cutoffn=250,print=FALSE){
    library(plyr)
    library(dplyr)
    library(sf)
    library(sp)
    library(phenesse)
    library(parallel)

    print("prepping gridded data")

    # turn data into SpatialPoints
    usa_insects$latitude <- as.numeric(usa_insects$latitude)
    usa_insects$longitude <- as.numeric(usa_insects$longitude)
    coordinates(usa_insects) <- cbind(usa_insects$latitude,usa_insects$longitude)
    data$latitude <- as.numeric(data$latitude)
    data$longitude <- as.numeric(data$longitude)
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
    x <- sp::over(usa_insects,grid)
    usa_insects <- as.data.frame(usa_insects)
    usa_insects$grid_id <- x$id
    usa_insects$cell_lat <- x$x1
    usa_insects$cell_long <- x$x2

    # get cells and prep output
    unique_cells <- unique(data$grid_id)

    # create a species-season-cells df to contain all species season cell info
    scc <- data.frame(matrix(ncol = 21, nrow = 220))

    # create a list of numeric vectors, each containing a set of observation hours from one species season cell combo
    # create a second list of numeric vectors, each containing a set of baseline hours to match
    obs <- list()
    bl_obs <- list()

    # after batch ci, data will be merged back into scc
    index = 1

    # set num of cells to do to first n
    # unique_cells <- unique_cells[1:firstn]

    # initialize out df
    out <- data.frame(matrix(ncol = 3, nrow = 10000))
    colnames(out) <- c("species", "season", "cell")


    do_break = FALSE
    print("preparing obs data for batching")

    # iterate through cells and add observation hours to obs and bl_obs for batching
    for(i in 1:length(unique_cells))
    {
        #i=1
        cell_obs <- dplyr::filter(data,grid_id == unique_cells[i])
        baseline_cell_obs <- dplyr::filter(usa_insects,grid_id == unique_cells[i])
        species_in_cell <- unique(cell_obs$scientific_name)

        if(use_genus){
            species_in_cell <- unique(cell_obs$taxon_genus_name)
        }

        # for each species found in the cell...
        for(k in 1:length(species_in_cell))
        {
            #k=1
            # filter for obs of species
            species_obs <- dplyr::filter(cell_obs,scientific_name == species_in_cell[[k]])
            sp <- species_in_cell[k]

            if(use_genus){
                species_obs <- dplyr::filter(cell_obs,taxon_genus_name == species_in_cell[[k]])
                sp <- species_in_cell[k]
            }

            # for each season...
            for(j in 1:4)
            {
                #j=3

                season_obs <- dplyr::filter(species_obs, season == j)
                baseline_season_obs <- dplyr::filter(baseline_cell_obs, season == j)

                # cutoff
                if(nrow(baseline_season_obs) > cutoffn){
                    baseline_season_obs <- sample_n(baseline_season_obs,cutoffn)
                }
                if(nrow(season_obs) > cutoffn){
                    season_obs <- sample_n(season_obs,cutoffn)
                }

                if(length(season_obs$id) > min_ants_n && length(baseline_season_obs$id) > min_ants_n)
                {
                    # add the cell to the list - try ants and bl separated by -1
                    obs_vect <- unname(unlist(c(season_obs$local_hour,-1,baseline_season_obs$local_hour)))
                    obs[[index]]<- as.numeric(obs_vect)

                    # add the matching baseline cell to the list
                    #bl_obs <- append(obs, baseline_season_obs$local_hour)

                    # add base scc info
                    out$species[index] <- species_in_cell[k]
                    out$season[index] <- j
                    out$cell[index] <- unique_cells[i]

                    # increment index
                    index <- index + 1
                }
                if(index > firstn){
                    do_break = TRUE
                    break
                }
            }
            if(do_break){
                break
            }
        }
        if(do_break){
            break
        }
    }

    print(out)
    #write.csv(out,"out.csv")

    batchCI <- function(row){

        # get ant and bl hours using split index
        split_index <- which(row == -1)
        ant_hours <- row[1:split_index-1]
        bl_hours <- row[split_index+1:length(row)]
        bl_hours <- bl_hours[!is.na(bl_hours)]
        #
        #print(split_index)
        #print(ant_hours)
        #print(bl_hours)

        # get onset
        q5 <- tryCatch(weib_percentile_ci(observations = ant_hours, percentile = 0.05,
                                          bootstraps = weib_ci_bootstraps, iterations = weib_ci_iters),
                       error = function(e) data.frame(estimate = NA, low_ci = NA, high_ci = NA))
        bl_q5  <- tryCatch(weib_percentile_ci(observations = bl_hours, percentile = 0.05,
                                              bootstraps = weib_ci_bootstraps, iterations = weib_ci_iters),
                           error = function(e) data.frame(estimate = NA, low_ci = NA, high_ci = NA))

        # get mid
        q50 <- tryCatch(weib_percentile(observations = ant_hours, percentile = 0.5, iterations = weib_iters),
                        error = function(e) data.frame(estimate = NA, low_ci = NA, high_ci = NA))

        # get offset
        q95  <- tryCatch(weib_percentile_ci(observations = ant_hours, percentile = 0.95,
                                            bootstraps = weib_ci_bootstraps, iterations = weib_ci_iters),
                         error = function(e) data.frame(estimate = NA, low_ci = NA, high_ci = NA))
        bl_q95  <- tryCatch(weib_percentile_ci(observations = bl_hours, percentile = 0.95,
                                               bootstraps = weib_ci_bootstraps, iterations = weib_ci_iters),
                            error = function(e) data.frame(estimate = NA, low_ci = NA, high_ci = NA))
        rdf <- data.frame(q5 = q5$estimate, q5_low = q5$low_ci, q5_high = q5$high_ci,
                          bl_q5 = bl_q5$estimate, bl_q5_low = bl_q5$low_ci, bl_q5_high = bl_q5$high_ci,
                          q50 = q50,
                          q95 = q95$estimate, q95_low = q95$low_ci, q95_high = q95$high_ci,
                          bl_q95 = bl_q95$estimate, bl_q95_low = bl_q95$low_ci, bl_q95_high = bl_q95$high_ci)
        return(rdf)
    }

    print(paste("nrow obs:", nrow(obs)))
    print("starting batch CI - estimated time...")
    print("current time -" + str(Sys.time()))

    batchCIout <- mclapply(X = obs, FUN = batchCI, mc.cores = ncpus)
    write.csv(batchCIout,"batchCIout.csv")
    View(batchCIout)
    View(out)

    # generate new cols
    test <- rbind(out, batchCIout)
    out <- test
    write.csv(out,"boundOut.csv")

    # generate some new cols
    out$onset_ci_ol <- (out$q5_low < out$bl_q5_high) & (out$q5_high > out$bl_q5_low) |
        (out$q5_high < out$bl_q5_low) & (out$q5_low > out$bl_q5_high)
    out$offset_ci_ol <- (out$q95_low < out$bl_q95_high) & (out$q95_high > out$bl_q95_low) |
        (out$q95_high < out$bl_q95_low) & (out$q95_low > out$bl_q95_high)

    # format columns
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
