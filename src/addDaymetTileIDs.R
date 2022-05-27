
# add daymet_tile ids by computing a dist matrix of obs to all daymet cells
addDaymetTileIDs <- function(df, nsubset)
{
  library(lubridate)
  library(sf)
  library(dplyr)
  library(tidyr)
  
  t <- 0.003 * nrow(df)
  print(paste("estimated max time:", t, "seconds"))
  
  # add year and day if not already
  df$year <- year(df$datetimes)
  df$yday <- yday(df$datetimes)
  
  # create subsets so dist mat doesn't overflow
  n <- nsubset
  nr <- nrow(df)
  subsets <- split(df, rep(1:ceiling(nr/n), each=n, length.out=nr))
  
  # get unique cells to use for every loop
  dm_cells <- distinct(daymet_data, tile, .keep_all = TRUE)
  dm_cells_sf <- st_as_sf(dm_cells,coords = c("latitude", "longitude"))
  
  # loop through subsets, getting sf and dist matrix for each
  for(i in 1:length(subsets)){
    
    # get subset
    ss <- subsets[[i]]
    
    # convert to sf
    ss_sf <- st_as_sf(ss,coords = c("latitude", "longitude"))
    
    # get dist matrix of df to all dm_cells
    dist_matrix <- st_distance(ss_sf, dm_cells_sf)
    
    # get vector of closest cells and then tilenames
    close_tiles <- max.col(-1 * dist_matrix)
    ss$daymet_tile <- dm_cells[close_tiles,]$tile
    
    # reassign to subset
    subsets[[i]] <- ss
  }
  df <- as.data.frame(do.call(rbind, subsets))
  return(df)
}