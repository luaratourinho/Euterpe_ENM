
# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 21 May 2021


# Required packages

library(tidyverse)
library(dplyr)
library(raster)
library(rgdal)
library(rgeos)

# Read your species list
sp.names <- read.csv("./data/04_clean_df_thin_5_hand.csv", head=T)
sp.names <- unique(sp.names$species)
sp.names <- sp.names[-18]
n <-length(sp.names)



# Building table by a loop ---------------------------------------------------

Results_diff_area <- matrix(nrow = n, ncol = 24)
colnames(Results_diff_area) <- c("Species", "unchanged_percent_bin","loss_percent_bin", 
                                 "gain_percent_bin", "gain_loss_perc_pixel", 
                                 "unchanged", "loss", "gain", "total_cells",
                                 "mean_diff_fut_cur_percent", "t", "p",
                                 "ocorrencia_area_cu", "ocorrencia_area_fu",
                                 "loss_area3", "gain_area3", "unchaged_area3",
                                 "perc_loss_rel_pres", "perc_gain_rel_pres",
                                 "net_value_area", "net_value_perc",
                                 "percentage_change",
                                 "ocorrencia_area_cu_Eut","ocorrencia_area_fu_Eut")


for(i in 1:n){
  
  # Read your current and future rasters
  # Binary and continuous projections
  cu <- raster(paste0("./outputs/", sp.names[i], "/results", "/CUR.bin_", sp.names[i], ".asc"))
  fu <- raster(paste0("./outputs/", sp.names[i], "/results","/Fut_all.bin_", sp.names[i], ".asc"))
  cu_cont <- raster(paste0("./outputs/", sp.names[i], "/results","/CUR.cont_", sp.names[i], ".asc"))
  fu_cont <- raster(paste0("./outputs/", sp.names[i], "/results","/Fut_all.cont_", sp.names[i], ".asc"))
  
 
  # Saving only presence area - for Euterpe
  # cu_pres = cu
  # cu_pres[cu_pres == 0] <- NA
  # #cu_pres[!is.na(cu_pres)] <- 1
  # fu_pres = fu
  # fu_pres[fu_pres == 0] <- NA
  # 
  # 
  # writeRaster(cu_pres, filename = paste0("./outputs/", sp.names[i], "/results", "/Diff_raster/",
  #                                          sp.names[i], "_cur_bin.tif"),
  #             format="GTiff", overwrite=T)
  # 
  # writeRaster(fu_pres, filename = paste0("./outputs/", sp.names[i], "/results", "/Diff_raster/",
  #                                          sp.names[i], "_fut_bin.tif"),
  #             format="GTiff", overwrite=T)
  # 
  
  
  #BINARY
  
  cu2 = cu
  cu2[cu2 == 1] <- 2
  # fut-pres
  # 1-2 = -1 -> Here should be 1-1 = 0, i.e., the two have not changed, STABLE
  # 1-0 = 1 -> GAIN
  # 0-2 = -2 ->  Here should be 0-1 = -1, i.e., LOSS
  # 0-0 = 0 -> inadequate area in both scenarios, so the next step we turn into NA
  
  diff_bin <- fu - cu2
  diff_bin[diff_bin == 0] <- NA
 
   
  
  # Values and percentage related to the number of pixels
  
  unchanged <- ncell(which(diff_bin[] == -1))
  loss <- ncell(which(diff_bin[] == -2))
  gain <- ncell(which(diff_bin[] == 1))
  total_cells <- unchanged + loss + gain
  
  unchanged_percent_bin <- (unchanged/total_cells)*100
  loss_percent_bin <- (loss/total_cells)*100
  gain_percent_bin <- (gain/total_cells)*100
  gain_loss_perc_pixel <- gain_percent_bin - loss_percent_bin
  
  
  # Reclassing to -1, zero and 1 to save the raster
  # This is a raster that you could present in your final work
  
  diff_bin2 = diff_bin
  diff_bin2[diff_bin2 == -1] <- 0
  diff_bin2[diff_bin2 == -2] <- -1
  
  target_dir = paste(paste0("./outputs/", sp.names[i], "/results", "/Diff_raster/", sep=""))
  #dir.create(target_dir)
  
  if (file.exists(target_dir)) {
    cat("The directory already exists")
  } else {
    dir.create(target_dir)
  }
  
  
  writeRaster(diff_bin2, filename = paste0("./outputs/", sp.names[i], "/results", "/Diff_raster/",
                                           sp.names[i], "_diff_bin.tif"),
              format="GTiff", overwrite=T)
  

  
  # CONTINUOUS
  
  # Evaluating the difference between future and current continuous projections
  # Mean of difference - Negative value means loss, positive value means gain
  
  testt <- t.test(fu_cont[], cu_cont[], paired=T)
  chars <- capture.output(print(testt))
  mean_diff_estimate <- as.data.frame(testt$estimate)
  mean_diff_fut_cur <- mean_diff_estimate[1,]
  mean_diff_fut_cur_percent <- mean_diff_fut_cur*100
  mean_diff_conf.int <- as.data.frame(testt$conf.int)
  t <- testt$statistic
  p <- testt$p.value
  
  diff_cont <- fu_cont - cu_cont
  writeRaster(diff_cont, filename = paste0("./outputs/", sp.names[i], "/results", "/Diff_raster/", 
                                           sp.names[i], "_diff_cont.tif"),
              format="GTiff", overwrite=T)
  
  
  
  # AREA --------------------------------------------------------------------
  
  # To calculate the areas, we normally use binary results
  
  # Values and percentage related to the area in km2
  
  #current
  cu_area = cu
  cu_area[cu_area == 0] <- NA
  r = raster(nrow = cu_area@nrows, ncol = cu_area@ncols, xmn = cu_area@extent@xmin, 
             xmx = cu_area@extent@xmax, ymn = cu_area@extent@ymin, ymx = cu_area@extent@ymax) 
  # calculate area of each cell (the area changes along the latitudes / longitudes)
  x = raster::area(r) 
  #plot(x)
  ocorrencia_area = x * cu_area
  ocorrencia_area_cu <- cellStats(ocorrencia_area, stat='sum', na.rm=TRUE, asSample=TRUE)
  
  #future
  fu_area = fu 
  fu_area[fu_area == 0] <- NA
  ocorrencia_area2 = x * fu_area
  ocorrencia_area_fu <- cellStats(ocorrencia_area2, stat='sum', na.rm=TRUE, asSample=TRUE)
  
  # Expected area to contract (loss)
  
  loss_area = diff_bin 
  loss_area[loss_area == -1] <- NA
  loss_area[loss_area == 1] <- NA
  loss_area[loss_area == -2] <- 1
  loss_area2 = x * loss_area
  loss_area3 <- cellStats(loss_area2, stat='sum', na.rm=TRUE, asSample=TRUE)
  # Percentage of loss related to the current area
  perc_loss_rel_pres <- (loss_area3*100)/ocorrencia_area_cu
  
  # Expected area to expand (gain)
  
  gain_area = diff_bin 
  gain_area[gain_area == -1] <- NA
  gain_area[gain_area == -2] <- NA
  gain_area2 = x * gain_area
  gain_area3 <- cellStats(gain_area2, stat='sum', na.rm=TRUE, asSample=TRUE)
  # Percentage of gain related to the current area
  perc_gain_rel_pres <- (gain_area3*100)/ocorrencia_area_cu
  
  
  # Expected area not to change (unchanged)
  
  unchaged_area = diff_bin 
  unchaged_area[unchaged_area == 1] <- NA
  unchaged_area[unchaged_area == -2] <- NA
  unchaged_area[unchaged_area == -1] <- 1
  unchaged_area2 = x * unchaged_area
  unchaged_area3 <- cellStats(unchaged_area2, stat='sum', na.rm=TRUE, asSample=TRUE)
  
  
  # How much did change?
  
  # Net value in area and in percentage
  net_value_area <- gain_area3 - loss_area3
  net_value_perc <- perc_gain_rel_pres - perc_loss_rel_pres
  
  # Percentage of Change = (FV/IV - 1) * 100
  # Where FV is the final value of the transaction, whereas IV refers to the initial value.
  # The increase was ((700/500) - 1) * 100 = 0,4 × 100 = 40%.
  # The loss was ((500/700) - 1)* 100 = -29%
  percentage_change <- ((ocorrencia_area_fu/ocorrencia_area_cu) - 1)*100
  
  
  # Area inside of Euterpe distribution
  
  cu_eut <- raster(paste0("./outputs/", "Euterpe_edulis", "/results", "/Diff_raster/", "Euterpe_edulis_cur_bin.tif"))
  fu_eut <- raster(paste0("./outputs/", "Euterpe_edulis", "/results","/Diff_raster/", "Euterpe_edulis_fut_bin.tif"))
  cu <- raster(paste0("./outputs/", sp.names[i], "/results", "/CUR.bin_", sp.names[i], ".asc"))
  fu <- raster(paste0("./outputs/", sp.names[i], "/results","/Fut_all.bin_", sp.names[i], ".asc"))
  
  
  # You should rebuid the r2 to match r1
  
  cu_crop <- resample(cu, cu_eut, method='bilinear')
  cu_crop <- mask(cu_crop, cu_eut)
  cu_crop[cu_crop == 0] <- NA
  
  fu_crop <- resample(fu, fu_eut, method='bilinear')
  fu_crop <- mask(fu_crop, fu_eut)
  fu_crop[fu_crop == 0] <- NA
  
  writeRaster(cu_crop, filename = paste0("./outputs/", sp.names[i], "/results", "/Diff_raster/", 
                                           sp.names[i], "_cu_by_Eut.tif"),
              format="GTiff", overwrite=T)
  
  writeRaster(fu_crop, filename = paste0("./outputs/", sp.names[i], "/results", "/Diff_raster/", 
                                           sp.names[i], "_fu_by_Eut.tif"),
              format="GTiff", overwrite=T)
  
  #current
  cu_crop_area = cu_crop
  cu_crop_area[cu_crop_area == 0] <- NA
  r = raster(nrow = cu_crop_area@nrows, ncol = cu_crop_area@ncols, xmn = cu_crop_area@extent@xmin, 
             xmx = cu_crop_area@extent@xmax, ymn = cu_crop_area@extent@ymin, ymx = cu_crop_area@extent@ymax) 
  # calculate area of each cell (the area changes along the latitudes / longitudes)
  x = raster::area(r) 
  #plot(x)
  ocorrencia_area = x * cu_crop_area
  ocorrencia_area_cu_Eut <- cellStats(ocorrencia_area, stat='sum', na.rm=TRUE, asSample=TRUE)
  
  #future
  fu_crop_area = fu_crop 
  fu_crop_area[fu_crop_area == 0] <- NA
  ocorrencia_area2 = x * fu_crop_area
  ocorrencia_area_fu_Eut <- cellStats(ocorrencia_area2, stat='sum', na.rm=TRUE, asSample=TRUE)

  
  Results_diff_area[i, ] <- c(sp.names[i], unchanged_percent_bin, loss_percent_bin,
                              gain_percent_bin, gain_loss_perc_pixel, 
                              unchanged, loss, gain, total_cells,
                              mean_diff_fut_cur_percent, t, p,
                              ocorrencia_area_cu, ocorrencia_area_fu,
                              loss_area3, gain_area3, unchaged_area3,
                              perc_loss_rel_pres, perc_gain_rel_pres,
                              net_value_area, net_value_perc,
                              percentage_change,ocorrencia_area_cu_Eut,ocorrencia_area_fu_Eut)
  
}

write.csv(Results_diff_area, paste0("./outputs/", "8_calculating_area.csv"), row.names = F)

