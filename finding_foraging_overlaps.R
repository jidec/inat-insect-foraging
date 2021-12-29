# R Script to map species distribution polygons and quantify overlaps
# Neeka Sewnath
# nsewnath@ufl.edu

# Import libraries
library(raster)
library(ENMTools)

# Read in data
data <- read.csv("data/usa_ants_with_wc.csv")

#Bioclim initial stacks
bioclim <- stack("bioclim/bio1.bil", "bioclim/bio2.bil", "bioclim/bio3.bil",
                 "bioclim/bio4.bil", "bioclim/bio5.bil","bioclim/bio6.bil", 
                 "bioclim/bio7.bil","bioclim/bio8.bil","bioclim/bio9.bil",
                 "bioclim/bio10.bil","bioclim/bio11.bil","bioclim/bio12.bil",
                 "bioclim/bio13.bil","bioclim/bio14.bil","bioclim/bio15.bil",
                 "bioclim/bio16.bil","bioclim/bio17.bil","bioclim/bio18.bil",
                 "bioclim/bio19.bil")

# TODO: Create species distribution polygons

# NS: I'm thinking maybe pull a unique species list, develop a shape file for 
# for each one, plot extent, crop, and mask bioclim layers, and then maybe
# create maxent models and quanitfy env and raster overlap between species?
# that might take a while if the shape files made by hand. 

# Example: 

# Shape file for region 
#shape <- readOGR("shape.shp")
#crop <- crop(bioclim, shape, snap = 'near')
#mask <- mask(crop, shape) 

# Read in accessible shape file (.shp made outside of this R script)
#species_1_extent<-readOGR(paste0("species_1_accessible_area.shp"))
#species_1_crop<-crop(bioclim, species_1_extent, snap = 'near')
#species_1_mask<-mask(species_1_crop, species_1_extent) 

# Determine background points
#species_1_bg <- randomPoints(species_1_mask[[1]], n=10000)
#species_1_bg <- as.data.frame(species_1_bg)
#plot(species_1_mask[[1]], legend=FALSE)
#points(species_1_bg, col='red')

# Create ENMTools object for each species
#species1_enmtools<-enmtools.species()
#species1_enmtools$species.name<-"some species"
#species1_enmtools$presence.points<-species_1_points
#species1_enmtools$range<-species_1_mask[[1]]
#species1_enmtools$background.points<- species_1_bg

# Make Maxent Model
#my.args =c("betamultiplier=2","threshold=FALSE")
#species1.mx <- enmtools.maxent(species1_enmtools, mask, bg_source="range", test.prop = 0.2, args = my.args)

# Then you would do the same 4 steps above for a second species and then compare. 

# Raster overlap
#species1_x_species2_raster_overlap<-raster.overlap(species1.mx, species2.mx)

# Env_overlap
#species1_x_species2_env_overlap<- env.overlap(species1.mx, species2.mx, mask, tolerance = .001)

# We could just set it up as a loop but it's probably going to take a while seeing
# how many unique species there are. Are all the species standardized?
