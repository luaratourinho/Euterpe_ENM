
# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Date: 08 may 2021


# Cropping variables from Worldclim ---------------------------------------


# Required packages 

library(raster)
library(dplyr)
library(rgeos)
library(reshape)
library(rgdal)


# Opening GCMs ------------------------------------------------------------

# Now, for other GCMs
# And selected variables
# By Spearman test (0.6): 
# "X_wc2.1_2.5m_bio_15" "X_wc2.1_2.5m_bio_18" 
# "X_wc2.1_2.5m_bio_3"   "X_wc2.1_2.5m_bio_8"

## Future variables ------------------------------------------------------

to_crop <- raster('./data/env_cropped/present/_wc2.1_2.5m_bio_1.tif')
#to_crop <- envi.mask2[[1]]

# If future biovariables come separately, build a list like you did before


# BCC ---------------------------------------------------------------------

# spp126 ------------------------------------------------------------------

# Ps. 2021-2040 was done in the step "5_crop_environmental_variables.R"

# Choosing another future scenario

# 2041-2060

future_var <- stack("D:/Luara Tourinho/OneDrive/Documentos/Environmental_data/Worldclim/wc2.1_2.5m_bioc_IPSL-CM6A-LR_ssp585_2041-2060/wc2.1_2.5m_bioc_IPSL-CM6A-LR_ssp585_2041-2060.tif")

# Chose the variables that you selected in "5_crop_environmental_variables.R"
future_var_stk <- future_var[[c(2,3,10,15,18)]]
envi_fut.cut<- crop(future_var_stk, to_crop)
envi_fut.mask<- mask(envi_fut.cut, to_crop)

# Save only that selected variables 5_crop_environmental_variables.R step
names(envi_fut.mask) <- c("bio_2", "bio_3", "bio_10","bio_15","bio_18")
dir.create(paste0("./env_sel/future/mic6_ssp585_41-60"))
writeRaster(envi_fut.mask, filename='./env_sel/future/ip_ssp585_41-60/', 
            format="GTiff", bylayer=TRUE, suffix="names", overwrite=TRUE)


# However, that write function above is adding a "_" before the raster names
# because that I am saving one by one

# Saving one by one
writeRaster(envi_fut.mask[[1]], filename='./env_sel/future/ip_ssp585_41-60/bio_2', 
            format="GTiff", overwrite=TRUE)
writeRaster(envi_fut.mask[[2]], filename='./env_sel/future/ip_ssp585_41-60/bio_3', 
            format="GTiff", overwrite=TRUE)
writeRaster(envi_fut.mask[[3]], filename='./env_sel/future/ip_ssp585_41-60/bio_10', 
            format="GTiff", overwrite=TRUE)
writeRaster(envi_fut.mask[[4]], filename='./env_sel/future/ip_ssp585_41-60/bio_15', 
            format="GTiff", overwrite=TRUE)
writeRaster(envi_fut.mask[[5]], filename='./env_sel/future/ip_ssp585_41-60/bio_18', 
            format="GTiff", overwrite=TRUE)



# Repeat this routine for other scenarios, GCMs, and years that interest you

