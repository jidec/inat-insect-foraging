#nsubset = 100
#sccs <- sccs_list[[1]]

addClosestDaymetIDToSCCs <- function(sccs)
{
    library(dplyr)
    library(sf)

    df <- sccs

    # get unique daymet cells
    dm_cells <- distinct(daymet_data, tile, .keep_all = TRUE)
    dm_cells_sf <- st_as_sf(dm_cells,coords = c("latitude", "longitude")) # daymet cells

    # convert to sf
    sf <- st_as_sf(df,coords = c("cell_lat", "cell_lon")) #scc cells

    # get dist matrix of df to all dm_cells
    dist_matrix <- st_distance(sf, dm_cells_sf)

    # get vector of closest cells and then tilenames
    close_tiles <- max.col(-1 * dist_matrix)
    df$daymet_tile <- dm_cells[close_tiles,]$tile

    return(df)
}

library(plyr)

merged <- join(sccs,daymet_data,by=c("daymet_tile", "season"),type='left')
merged <- merge(sccs, daymet_data, by = c("daymet_tile", "season"),how='left')

merged <- sccs

match(sccs$daymet_tile, daymet_data$daymet_tile)

merged <- cbind(sccs, daymet_data[match(paste(sccs$daymet_tile, sccs$season), paste(daymet_data$daymet_tile, daymet_data$season)),])

merged <- merged[,-33]
merged <- merged[,-40]

