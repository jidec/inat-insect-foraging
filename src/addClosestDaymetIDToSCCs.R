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
