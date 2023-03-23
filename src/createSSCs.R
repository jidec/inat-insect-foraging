#min_per_cell_n = 30
# create SSCs simply
# in daymet_data, temp and more must be unlisted and set as columns
createSSCs <- function(gridded_tuple, daymet_data, min_per_cell_n){
    library(dplyr)
    Mode <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
    }
    # group daymet data into season_tile
    daymet_grouped <- daymet_data %>%
        group_by(season,tile) %>%
        dplyr::summarise(temp = mean(tmax),daylength=mean(daylength),vp=mean(vp),precip=mean(precip),srad=mean(srad),lat=mean(latitude),lon=mean(longitude))

    # Butterflies (or another clade)
    # group into SSCs
    sscs <- gridded_tuple[[1]] %>%
        group_by(species,season,grid_id) %>%
        dplyr::summarise(n = n(),obs=list(local_hour),tile=Mode(daymet_tile)) %>%
        dplyr::filter(n > min_per_cell_n)

    # Baseline
    # group into SSCs
    sscs_bl <- gridded_tuple[[2]] %>%
        group_by(season,grid_id) %>%
        dplyr::summarise(n = n(),obs=list(local_hour),tile=Mode(daymet_tile)) %>%
        dplyr::filter(n > min_per_cell_n)
    # merge clade and baseline and rename cols
    sscs <- merge(sscs,sscs_bl,by=c("season","grid_id"),all.x=TRUE)
    sscs$tile <- sscs$tile.x

    # merge daymet data in
    sscs <- merge(sscs,daymet_grouped,all.x=TRUE)
    colnames(sscs) <- c("season","grid_id","tile","species","obs_n","obs","daymet_tile","bl_obs_n","bl_obs","todrop","tmax","daylength","vp","precip","srad","lat","lon")
    return(sscs)
}
