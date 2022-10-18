
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
#min_per_cell_n=10
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
#data= usa_ants
#bl_data= usa_insects
#cellsize_miles=250
#weib_iters=10
#weib_ci_iters=10
#weib_ci_bootstraps=10
#ncpus = 5
#use_genus=FALSE
#simple=FALSE
#min_per_cell_n=8
#sample_cutoff_n=150
#floor_hours=TRUE
#first_n=100
#skip_n=0
#target_species=NULL
#print=FALSE

#source("src/createBaselineDiffData3.R")
estimateSpeciesSeasonCellForaging <- function(data, bl_data, cellsize_km=200,
                                   weib_iters=50, weib_ci_iters=10,weib_ci_bootstraps=10, kl_bootstraps=50,ncpus = 4,
                                   use_genus=FALSE,simple=FALSE,
                                   min_per_cell_n=8,sample_cutoff_n=250, floor_hours=TRUE, daymet_manova=FALSE,
                                   first_n=10000,skip_n=1000, target_species=NULL,
                                   print=FALSE){
    library(plyr)
    library(dplyr)
    library(sf)
    library(sp)
    library(phenesse)
    library(parallel)
    library(philentropy)

    print("Prepping gridded data")
    #data_tuple <- prepGriddedDataBL(usa_ants, usa_insects, 250)
    data_tuple <- prepGriddedDataBL(data, bl_data, cellsize_km)
    data <- data_tuple[[1]]
    bl_data <- data_tuple[[2]]

    print("Preparing obs data for batching")
    #obs_out_tuple <- prepareObsDataForBatch(data, bl_data, min_per_cell_n=8,sample_cutoff_n=150,first_n=20,use_genus=FALSE) #target_species="Camponotus pennsylvanicus")
    obs_out_tuple <- prepareObsDataForBatch(data, bl_data, min_per_cell_n,sample_cutoff_n,first_n,skip_n,floor_hours,use_genus,target_species,daymet_manova)

    obs <- obs_out_tuple[[1]]
    out <- obs_out_tuple[[2]]

    View(out)

    print(paste("Starting batch CI - estimated time", length(obs) * 7, "minutes"))
    print(paste("number of obs:", length(obs)))

    start_time <- Sys.time()
    print("start time:")
    print(start_time)

    #obs_ci <- mclapply(X = obs, FUN = batchCI, mc.cores = 1) #for testing on Windows
    if(simple){
        obs_ci <- mclapply(X = obs, FUN = batchCISimple, mc.cores = ncpus)
    }
    else{
        obs_ci <- mclapply(X = obs, FUN = batchCI, mc.cores = ncpus)
    }

    end_time <- Sys.time()
    print("end time:")
    print(end_time)

    print("Finalizing cols")
    obs_ci <- do.call(rbind.data.frame, obs_ci)
    out <- na.omit(out)
    out <- cbind(out, obs_ci)
    #plot(obs_ci$kl_div)
    #out <- finalizeCols(out)

    View(out)
    return(list(out,obs))
}

prepGriddedDataBL <- function(data, bl_data, cellsize_km){

    # turn data into SpatialPoints
    bl_data$latitude <- as.numeric(bl_data$latitude)
    bl_data$longitude <- as.numeric(bl_data$longitude)
    coordinates(bl_data) <- cbind(bl_data$latitude,bl_data$longitude)
    data$latitude <- as.numeric(data$latitude)
    data$longitude <- as.numeric(data$longitude)
    coordinates(data) <- cbind(data$latitude,data$longitude)

    # create SpatialGridDataFrame from data
    grid <- makegrid(data, cellsize = cellsize_km/111) #cellsize_miles/69
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
    # turn sampled_bl_data data into SpatialPoints
    x <- sp::over(bl_data,grid)
    bl_data <- as.data.frame(bl_data)
    bl_data$grid_id <- x$id
    bl_data$cell_lat <- x$x1
    bl_data$cell_long <- x$x2

    return(list(data,bl_data))
}

#min_per_cell_n = 8
#sample_cutoff_n = 250
#first_n = 3
#use_genus = FALSE
#bl_data <- usa_insects
#data <- usa_ants

prepareObsDataForBatch <- function(data, bl_data, min_per_cell_n,sample_cutoff_n,first_n=10000,skip_n=0,floor_hours=TRUE,use_genus=FALSE,target_species=NULL,daymet_manova=FALSE){

    library(dplyr)
    library(plyr)

    # create a list of numeric vectors, each containing a set of observation hours from one species season cell combo
    # create a second list of numeric vectors, each containing a set of baseline hours to match
    obs <- list()
    bl_obs <- list()

    # initialize out df
    out <- data.frame(matrix(ncol = 8, nrow = 10000))
    colnames(out) <- c("species", "season", "cell", "cell_lat", "cell_lon", "n","anova_p","manova_p")

    # filter for target species
    if(!is.null(target_species)){
        data <- dplyr::filter(data,species == target_species)
    }

    # get cells and prep output
    unique_cells <- unique(data$grid_id)
    ncells <- length(unique_cells)

    # prep loop
    do_break <- FALSE
    index <- 1
    # iterate through cells and add observation hours to obs and bl_obs for batching
    for(i in 1:ncells)
    {
        #i=1
        #k=1
        #j=3
        cell_obs <- dplyr::filter(data,grid_id == unique_cells[i])
        baseline_cell_obs <- dplyr::filter(bl_data,grid_id == unique_cells[i])
        #print(cell_obs)
        species_in_cell <- unique(cell_obs$scientific_name)

        if(use_genus){
            species_in_cell <- unique(cell_obs$taxon_genus_name)
        }

        #print(species_in_cell)
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
                if(nrow(baseline_season_obs) > sample_cutoff_n){
                    baseline_season_obs <- sample_n(baseline_season_obs,sample_cutoff_n)
                }
                if(nrow(season_obs) > sample_cutoff_n){
                    season_obs <- sample_n(season_obs,sample_cutoff_n)
                }

                season_obs_s <- season_obs

                if(length(season_obs$id) > min_per_cell_n && length(baseline_season_obs$id) > min_per_cell_n)
                {
                    if(index > skip_n){
                        ant_hours <- unlist(season_obs$local_hour)
                        ant_hours <- as.numeric(ant_hours)
                        if(floor_hours){
                            ant_hours <- floorVector(ant_hours)
                        }

                        # add the cell to the list - try ants and bl separated by -1
                        obs_vect <- unname(unlist(c(ant_hours,-1,baseline_season_obs$local_hour)))
                        obs[[index]]<- as.numeric(obs_vect)
                        p <- 0
                        m_p <- 0

                        # run manova
                        if(daymet_manova){
                            # run anova
                            season_obs$category <- "ants"
                            baseline_season_obs$category <- "baseline"

                            baseline_season_obs <- cbind(as.numeric(baseline_season_obs$local_hour),as.numeric(baseline_season_obs$temp_min),as.numeric(baseline_season_obs$precip),as.character(baseline_season_obs$category))
                            colnames(baseline_season_obs) <- c("local_hour","temp_min","precip","category")
                            season_obs <- cbind(as.numeric(season_obs$local_hour),as.numeric(season_obs$temp_min),as.numeric(season_obs$precip),as.character(season_obs$category))
                            colnames(season_obs) <- c("local_hour","temp_min","precip","category")

                            obs_stacked <- as.data.frame(rbind(season_obs,baseline_season_obs))

                            #anov <- aov(local_hour ~ category, data = obs_stacked)

                            #p <- summary(anov)[[1]][1,5] # p value

                            obs_stacked$temp_min <- as.numeric(obs_stacked$temp_min)
                            obs_stacked$local_hour <- as.numeric(obs_stacked$local_hour)
                            obs_stacked <- na.omit(obs_stacked)
                            manov <- manova(cbind(local_hour,temp_min) ~ category, data=obs_stacked)
                            m_p <- summary(manov)[[4]][1,6] # p value
                        }

                        # add base scc info
                        out$species[index] <- species_in_cell[k]
                        out$season[index] <- j

                        out$cell[index] <- season_obs_s$grid_id[1]
                        out$cell_lat[index] <- season_obs_s$cell_lat[1]
                        out$cell_lon[index] <- season_obs_s$cell_lon[1]
                        out$n[index] <- length(season_obs[,1])
                        out$anova_p[index] <- p
                        out$manova_p[index] <- m_p

                        # scc temp and precip
                    }
                    # increment index
                    index <- index + 1
                }

                if(index > first_n){
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

    return(list(obs,out))
}

batchCI <- function(row,kl_bootstraps){

    # get ant and bl hours using split index
    split_index <- which(row == -1)
    ant_hours <- row[1:split_index-1]
    bl_hours <- row[split_index+1:length(row)]
    bl_hours <- bl_hours[!is.na(bl_hours)]

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

    # set n bootstraps and n samples
    nbootstraps <- 50
    # sample entire set with replacement
    nsamples <- 100

    bootstrapped_divergences <- c()

    for(b in 1:nbootstraps)
    {
        #empirical - sampling from raw data
        ant_sampled <- sample(ant_hours,nsamples,replace=TRUE)
        baseline_sampled <- sample(bl_hours,nsamples,replace=TRUE)
        sampled <- cbind(ant_sampled,baseline_sampled)
        div <- KL(sampled, test.na = TRUE, unit = "log2", est.prob = NULL, epsilon = 1e-05)

        tryCatch(KL(sampled, test.na = TRUE, unit = "log2", est.prob = NULL, epsilon = 1e-05),
                 error = function(e) {return(0)})
        # append new divergence
        bootstrapped_divergences <- c(bootstrapped_divergences,div)
    }

    kl_div <- mean(bootstrapped_divergences)
    kl_div_var <- variance(bootstrapped_divergences)
    # save and visualize distributions

    # prob a histogram of the KL divergences
   # hist(kl_divergences)
    rdf <- data.frame(q5 = q5$estimate, q5_low = q5$low_ci, q5_high = q5$high_ci,
                      bl_q5 = bl_q5$estimate, bl_q5_low = bl_q5$low_ci, bl_q5_high = bl_q5$high_ci,
                      q50 = q50,
                      q95 = q95$estimate, q95_low = q95$low_ci, q95_high = q95$high_ci,
                      bl_q95 = bl_q95$estimate, bl_q95_low = bl_q95$low_ci, bl_q95_high = bl_q95$high_ci,kl_div=kl_div,kl_div_var=kl_div_var)
    return(rdf)
}

batchCISimple <- function(row){

    # get ant and bl hours using split index
    split_index <- which(row == -1)
    ant_hours <- row[1:split_index-1]
    bl_hours <- row[split_index+1:length(row)]
    bl_hours <- bl_hours[!is.na(bl_hours)]

    weib_iters <- 35

    # get mid
    q50 <- tryCatch(weib_percentile(observations = ant_hours, percentile = 0.5, iterations = weib_iters),
                    error = function(e) data.frame(estimate = NA, low_ci = NA, high_ci = NA))
    # get mid
    q95 <- tryCatch(weib_percentile(observations = ant_hours, percentile = 0.95, iterations = weib_iters),
                    error = function(e) data.frame(estimate = NA, low_ci = NA, high_ci = NA))
    # get mid
    q5 <- tryCatch(weib_percentile(observations = ant_hours, percentile = 0.05, iterations = weib_iters),
                    error = function(e) data.frame(estimate = NA, low_ci = NA, high_ci = NA))
    # set n bootstraps and n samples
    nsamples <- length(ant_hours)
    kl_bootstraps <- 50
    bootstrapped_divergences <- c()

    #library(fitdistrplus)

    #test <- rnorm(100,12,2)
    #test <- round(test, digits = 0)
    #test <- table(test)
    #test <- test / sum(test)

    #fw <- fitdist(test, "weibull")
    #denscomp(fw)
    #fw$fix.arg
    #?fitdist
    #t <- table(test)

    for(b in 1:kl_bootstraps)
    {
        ant_sampled <- sample(ant_hours,nsamples,replace=TRUE)
        ant_probs <- table(ant_sampled) / length(ant_sampled)
        baseline_sampled <- sample(bl_hours,length(ant_sampled),replace=TRUE)
        baseline_probs <- table(baseline_sampled) / length(baseline_sampled)
        probs <- cbind(ant_probs,baseline_probs)
        div <- KL(probs, test.na = TRUE, unit = "log2", est.prob = NULL, epsilon = 1e-05)

        # append new divergence
        bootstrapped_divergences <- c(bootstrapped_divergences,div)
    }

    # what summary statist
    kl_div <- mean(bootstrapped_divergences)
    kl_var <- var(bootstrapped_divergences)

    rdf <- data.frame(q5 = q5, q5_low = 0, q5_high = 0,
                      bl_q5 = 0, bl_q5_low = 0, bl_q5_high = 0,
                      q50 = q50,
                      q95 = q95, q95_low = 0, q95_high = 0,
                      bl_q95 = 0, bl_q95_low =0, bl_q95_high = 0,kl_div=kl_div,kl_var=kl_var)
    print("row finished")
    return(rdf)
}

finalizeCols <- function(out)
{
    # generate some new cols
    out$onset_ci_ol <- (out$q5_low < out$bl_q5_high) & (out$q5_high > out$bl_q5_low) |
        (out$q5_high < out$bl_q5_low) & (out$q5_low > out$bl_q5_high)
    out$offset_ci_ol <- (out$q95_low < out$bl_q95_high) & (out$q95_high > out$bl_q95_low) |
        (out$q95_high < out$bl_q95_low) & (out$q95_low > out$bl_q95_high)

    out$is_sig_any <- (out$manova_p < 0.05 | out$anova_p < 0.05 | !out$onset_ci_ol | !out$offset_ci_ol)
    out$is_sig_except_off <- (out$manova_p < 0.05 | out$anova_p < 0.05 | !out$onset_ci_ol)
    out$is_sig_except_man <- (out$anova_p < 0.05 | !out$onset_ci_ol | !out$offset_ci_ol)
    out$is_sig_anov_onset <- (out$anova_p < 0.05 | !out$onset_ci_ol)
    out$is_sig_onset_offset <- (!out$offset_ci_ol | !out$onset_ci_ol)

    out <- unlistDF(out)

    #out$night_metric <- out$q95/out$q5

    # format columns
    out$n <- as.numeric(out$n)
    out$anova_p <- as.numeric(out$anova_p)
    out$manova_p <- as.numeric(out$manova_p)
    out$midday_dist <- abs(out$q50 - 15)
    library(stringr)
    out$genus <- str_split_fixed(out$species, " ", n=2)[,1]

    out$season <- as.factor(out$season)
    out$genus <- as.factor(out$genus)
    out$duration <- out$q95 - out$q5
    return(out)
}

# used to floor values below the median
floorVector <- function(v){
    hour_occurences <- table(v)
    floored <- hour_occurences
    for(o in 1:length(hour_occurences)){
        if(floored[o] > median(hour_occurences)){
            floored[o] = median(hour_occurences)
        }
    }

    floored2 <- numeric()
    # can't find for the life of me a converter of counts
    for(f in 1:length(floored))
    {
        repped <- rep(as.numeric(names(floored[f])),floored[f])
        floored2 <- c(floored2,repped)
    }
    return(floored2)
}

# use to fix some weird listing bug
unlistDF <- function(df)
{
    for(i in 1:ncol(df))
    {
        df[,i] <- unlist(df[,i])
    }
    return(df)
}

getObsOnly <- function(data, bl_data, cellsize_km=200,
                                   min_per_cell_n=8,sample_cutoff_n=250, floor_hours=TRUE,
                                   first_n=10000){
    library(plyr)
    library(dplyr)
    library(sf)
    library(sp)
    library(phenesse)
    library(parallel)

    print("Prepping gridded data")
    #data_tuple <- prepGriddedDataBL(usa_ants, usa_insects, 250)
    data_tuple <- prepGriddedDataBL(data, bl_data, cellsize_km)
    data <- data_tuple[[1]]
    bl_data <- data_tuple[[2]]

    print("Preparing obs data for batching")
    #obs_out_tuple <- prepareObsDataForBatch(data, bl_data, min_per_cell_n=8,sample_cutoff_n=150,first_n=500,use_genus=FALSE) #target_species="Camponotus pennsylvanicus")
    obs_out_tuple <- prepareObsDataForBatch(data, bl_data, min_per_cell_n,sample_cutoff_n,first_n)
    return(obs_out_tuple)
}
