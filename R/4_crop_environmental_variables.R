
# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Date: 01 abr 2021


# Cropping variables from Worldclim ---------------------------------------


# Required packages 

library(raster)
library(dplyr)
library(rgeos)
library(reshape)


# Download climatic data from any database of your interest
# I chose worldclim - https://www.worldclim.org/


## Current variables ------------------------------------------------------


# Reading rasters
# use pattern = '.tif$' or something else if you have multiple files in this folder
raster_files <- list.files("./Environmental_data/Worldclim/wc2.1_2.5m_bio", 
                           full.names = T, 'tif$|bil$')
head(raster_files)

envi <- stack(raster_files)

# Cropping rasters

# Choose your extension
# All America
envi.cut<-crop(envi, c(-130, -25, -50, 50)) # my species occur in America, I cropped here only to reduce the size of the raster, reducing the analysis process later.. 
plot(envi.cut[[1]]) # check it


# Projections

# geographical, datum WGS84
crs.wgs84 <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84") 
# projected, South America Albers Equal Area Conic
crs.albers <- CRS("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +x_0=0 
                  +y_0=0 +ellps=aust_SA +units=m +no_defs") 


# Creating mask area for variables' correlation, which includes all coordinates
species_df <- read.csv("./outputs/03_clean_df_thin_5.csv")        
coords <- species_df[ ,2:3]
coordinates(coords) <- c("lon", "lat")
proj4string(coords) <- crs.wgs84  # define original projection - wgs84
coords <- spTransform(coords, crs.albers)  # project to Albers Equal Area
mcp <- gConvexHull(coords) # create minimum convex polygon
# If you want to add a buffer with a distance or an area around the mpc
# Attention: gBuffer and gArea are in meters, you have to convert in km if you want to
mcp_buffer <- gBuffer(mcp, width = gArea(mcp)*1e-07) # 20% bigger than mcp
mcp_buffer <- SpatialPolygonsDataFrame(mcp_buffer, data = data.frame("val" = 1, row.names = "buffer"))
mcp_buffer <- spTransform(mcp_buffer, crs.wgs84)

# OR if you prefer to you 'sf' package

library("sf")

mcp_buffer = sf::st_as_sf(mcp_buffer)
mcp_buffer = sf::st_transform(mcp_buffer, CRS("+proj=longlat +datum=WGS84 +no_defs"))

envi.mask <- crop(envi.cut,mcp_buffer)
envi.mask2 <- mask(envi.mask,mcp_buffer)

# Saving rasters
dir.create(paste0("./data/env_cropped/present/", "."))
writeRaster(envi.mask2, filename='./data/env_cropped/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)




## Future variables ------------------------------------------------------


# Reading a current raster already cropped to use as extent model
# to_crop <- raster('./data/env/_wc2.1_10m_bio_1.tif')
to_crop <- envi.mask2[[1]]

# If future biovariables come separately, build a list like you did before
raster_files <- list.files("./data/env", full.names = T, 'tif$|bil$')

# If future biovariables come as bands, follow these commands just below

# First option to read the raster stack from worldclim
# future_var <- raster("./data/env/wc2.1_2.5m_bioc_BCC-CSM2-MR_ssp585_2041-2060.tif")
# nbands(future_var)

# Second option to read the raster stack from worldclim
future_var_stk <- stack("./data/env/wc2.1_2.5m_bioc_BCC-CSM2-MR_ssp585_2041-2060.tif")



# Cropping future variables for each GCMs 

# A GCM as example: BCC

envi_fut.cut<- crop(future_var_stk, to_crop)
envi_fut.mask<- mask(envi_fut.cut, to_crop)

dir.create(paste0("./data/env_cropped/future/"))
writeRaster(envi_fut.mask, filename='./data/env_cropped/future/', format="GTiff", 
            bylayer=TRUE, suffix="names", overwrite=TRUE)


# If you are going to select variables from Pearson/Spearman correlation
# Before crop all rasters from future scenarios, 5_variable_correlation_spearman_and_pearson.R
# Then come back and crop the selected variables

