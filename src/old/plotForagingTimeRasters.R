
# Given a dataframe of iNat data and a scientific name
# (or NULL if using all individuals from data), plot histograms of foraging
# hours across seasons

# yMax is the y axis limit

plotForagingTimeRasters <- function(data, scientificName = NULL, season_num = "1", raster_res = 2, raster_function = getmode,  min_cell_sample = 100){

  library(rgdal)
  library(raster)
  library(maps)
  library(dplyr)
  library(RColorBrewer)

  # data <- usa_ants
  # scientificName <- "Camponotus pennsylvanicus"
  # season_num <- 3
  # raster_res <- 2
  # raster_function <- mean

  if(!is.null(scientificName)){
  data <- filter(data, scientific_name == scientificName)
  }

  if(season_num != "0"){
  data <- filter(data, season == season_num)
  }

  # add coords & projection
  coordinates(data) <- cbind(data$longitude, data$latitude)
  projection(data) <- "+init=EPSG:4326"

  # create raster
  r <- raster(data, resolution = raster_res) #2

  # rasterize
  r <- rasterize(data,r, field = data$midday_dist, fun = raster_function)
  rcount <- rasterize(data,r,field = data$midday_dist, fun= 'count')

  # remove squares of low sample size using count raster
  for (i in 1:10){
      for(j in 1:18){
          if(!is.na(rcount[i,j]))
          {
          if(rcount[i,j] < min_cell_sample)
          {
              r[i,j] <- NA
          }
          }
      }
  }

  pal <- colorRampPalette(c("white","black"))
  #can use or not use breaks, which bucket foraging times when plotting
  plot(r, col = pal(7))# ylim <- c(20,60), xlim <- c(-130,-70)) #col = terrain.colors(4)) #breaks = c(7,11,18,24),axes = FALSE,
  #plot USA shapefile on top
  shape <- readOGR(dsn = "data/cb_2018_us_state_5m")
  plot(shape, bg="transparent", add=TRUE)
}


# code send by Rob to get started using rnaturalearth and sf in the future - apparently rgdal is getting retired in 2023!
# library(rgeos)
# library(rnaturalearth)
# library(sf)
# library(dplyr)
#
# # test2 <- rnaturalearth::ne_countries(country = c("United States of America"),returnclass = "sf")
# # test2 <- st_transform(test2, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# #
# # grids <- st_make_grid(test2, cellsize = c(250, 250))
# # grids <- mutate(st_sf(geometry = grids), id_cells = 1:n())
# # grids
# # plot(grids)
# # plot(st_make_grid(x = test2, cellsize = 5, square = FALSE))
#
# #install.packages("rgdal")
# #install.packages("raster")
