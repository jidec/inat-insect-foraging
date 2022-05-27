# geoknife tests
install.packages("daymetr")

library(daymetr)
library(ggplot2)
df <- download_daymet(site = "Oak Ridge National Laboratories",
                      lat = 36.0133,
                      lon = -84.2625,
                      start = 2000,
                      end = 2010,
                      internal = TRUE,
                      simplify = TRUE) # return tidy data !!
View(df)

View(tile_outlines)
plot(tile_outlines)
library(dplyr)

# get only contiguous US tiles
usa_tiles <- tile_outlines
usa_tiles <- cbind(usa_tiles$TileID,usa_tiles$XMin,usa_tiles$XMax,usa_tiles$YMin,usa_tiles$Ymax)
colnames(usa_tiles) <- c("id", "XMin","XMax", "YMin","YMax")
usa_tiles <- as.data.frame(usa_tiles)
library(dplyr)
usa_tiles <- filter(usa_tiles, YMin > 24.396 & YMin < 49.38) #& YMax < -49.384 & XMin > -124.848 &)
usa_tiles <- filter(usa_tiles, XMin > -124.84 & XMin < -66.88)
tile_ids <- usa_tiles$id

# 24.396308, -124.848974: 49.384358, -66.885444
#Extent: (-124.848974, 24.396308) - (-66.885444, 49.384358)

p <- ggplot(tile_outlines)+
    geom_sf() +
    theme_void()
p

cellsize_miles = 300
usa_tiles$X <- (usa_tiles$XMax + usa_tiles$XMin) / 2
usa_tiles$Y <- (usa_tiles$YMax + usa_tiles$YMin) / 2
coordinates(usa_tiles) <- cbind(usa_tiles$X,usa_tiles$Y)

# create SpatialGridDataFrame from data
grid <- makegrid(usa_tiles, cellsize = cellsize_miles/69)
grid$id <- 1:nrow(grid)
coordinates(grid) <- cbind(grid$x1,grid$x2)
gridded(grid) <- TRUE
grid <- as(grid, "SpatialGridDataFrame")

points <- cbind(grid$id,grid$x2,grid$x1)
colnames(points) <- c("site","latitude","longitude")
points <- as.matrix(points)
write.csv(points,"data/points_for_daymet.csv",row.names = FALSE)

# spatial join and add cell info to points
x <- sp::over(data,grid)
data <- as.data.frame(data)
data$grid_id <- x$id
data$cell_lat <- x$x1
data$cell_long <- x$x2

# Download tiled data for multiple years (1980 - 2012)
# based upon a tile number (11207) restricted to the
# minimum temperature data only.
download_daymet_tiles(tiles = tile_ids,
                      start = 2010,
                      end = 2011,
                      param = "tmax",
                      path = "data/daymet")

# solution
# sample 200 from ants
# get all daymet tile ids overlapping these 200 lat long coords
# download daymet batch

tempdid
library(lubridate)

# get all ants lat longs from one year
usa_ants$year <- year(usa_ants$datetimes)
recent_year_ants <- filter(usa_ants, year == 2020)
points <- cbind(recent_year_ants$id, recent_year_ants$latitude, recent_year_ants$longitude)
colnames(points) <- c("site","latitude","longitude")
points <- as.matrix(points)
write.csv(points,"data/points_for_daymet.csv",row.names = FALSE)


# download 16500 (max) tiles from these points
download_daymet_batch(
    file_location = "data/points_for_daymet.csv",
    start = 2010,
    end = 2021,
    internal = FALSE,
    path = "data/daymet",
    simplify = FALSE
)

library(daymetr)
d <- read_daymet("data/daymet/37167660_2020_2021.csv")

mergeDaymetBatch <- function(){
    library(dplyr)
    csv_files <- dir(path= "data/daymet", pattern='*.csv$', recursive = T)
    csv_files <- paste0("data/daymet/", csv_files)
    df <- data.frame()
    for(i in 1:length(csv_files)) {
        df <- rbind(df, read_daymet(csv_files[i]))
    }

    #todo - process when multiple shared annotations
    write.csv(df,file="data/daymet_300m.csv")
    return(df)
}

library(lubridate)
yday(usa_ants$datetimes)

daymet_300m <- mergeDaymetBatch()

nc2tif(path = "data/daymet", overwrite = TRUE, silent = FALSE)
library(raster)
test <- raster("data/daymet/tmin_2010_01subset.tif")
View(test)
plot(test)
library(ncdf4)
nc_data <- nc_open('data/daymet/tmin_2010_01subset.nc')
nc_data
lon <- ncvar_get(nc_data, "t_max")
print(nc_data)
