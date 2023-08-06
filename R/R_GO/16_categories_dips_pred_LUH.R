# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 22 Feb 2022


# Required packages

library(tidyverse)
library(dplyr)
library(raster)
library(ggplot2)


# Reading files

# raster_files <- list.files("./Richness/", full.names = T, 'tif$|bil$')
# head(raster_files)
# variable <- stack(raster_files[c(3,7,4,8)])
# raster_names <- c("cu_rich_disp", "fu_rich_disp", "cu_rich_pred", "fu_rich_pred")
variable <- stack(cu_rich_disp, fu_rich_disp, cu_rich_pred, fu_rich_pred)

# pred
# 0-4, 5-9, 10-13, 14-18
# disp
# 0-3, 4-7, 8-11, 12-15


# Richness categories --------------------------------------------------------


cu_rich_pred <- variable[[3]]
cu_rich_pred[cu_rich_pred <= 4] <- 1
cu_rich_pred[cu_rich_pred >=5 & cu_rich_pred <=9] <- 2
cu_rich_pred[cu_rich_pred >=10 & cu_rich_pred <=13] <- 3
cu_rich_pred[cu_rich_pred >=14 & cu_rich_pred <=18] <- 4

writeRaster(cu_rich_pred, "./Richness/Categories/LUH/cu_rich_pred_categ.tif",
            format="GTiff", overwrite=T)

fu_rich_pred <- variable[[4]]
fu_rich_pred[fu_rich_pred <= 4] <- 1
fu_rich_pred[fu_rich_pred >=5 & fu_rich_pred <=9] <- 2
fu_rich_pred[fu_rich_pred >=10 & fu_rich_pred <=13] <- 3
fu_rich_pred[fu_rich_pred >=14 & fu_rich_pred <=18] <- 4

writeRaster(fu_rich_pred, "./Richness/Categories/LUH/fu_rich_pred_categ.tif",
            format="GTiff", overwrite=T)

cu_rich_disp <- variable[[1]]
cu_rich_disp[cu_rich_disp <= 3] <- 1
cu_rich_disp[cu_rich_disp >=4 & cu_rich_disp <=7] <- 2
cu_rich_disp[cu_rich_disp >=8 & cu_rich_disp <=11] <- 3
cu_rich_disp[cu_rich_disp >=12 & cu_rich_disp <=15] <- 4

writeRaster(cu_rich_disp, "./Richness/Categories/LUH/cu_rich_disp_categ.tif",
            format="GTiff", overwrite=T)

fu_rich_disp <- variable[[2]]
fu_rich_disp[fu_rich_disp <= 3] <- 1
fu_rich_disp[fu_rich_disp >=4 & fu_rich_disp <=7] <- 2
fu_rich_disp[fu_rich_disp >=8 & fu_rich_disp <=11] <- 3
fu_rich_disp[fu_rich_disp >=12 & fu_rich_disp <=15] <- 4

writeRaster(fu_rich_disp, "./Richness/Categories/LUH/fu_rich_disp_categ.tif",
            format="GTiff", overwrite=T)


# Calculating areas -------------------------------------------------------

# Euterpe area ------------------------------------------------------------

cu_eut <- raster(paste0("./outputs/", "Euterpe_edulis", "/results", 
                        "/Diff_raster/", "Euterpe_edulis_cur_bin.tif"))
cu_eut[cu_eut != 1] <- NA
r = raster(nrow = cu_eut@nrows, ncol = cu_eut@ncols, xmn = cu_eut@extent@xmin, 
           xmx = cu_eut@extent@xmax, ymn = cu_eut@extent@ymin, ymx = cu_eut@extent@ymax) 
x = raster::area(r) 
ocorrencia_area_cu_eut = x * cu_eut
Eut_cu <- cellStats(ocorrencia_area_cu_eut, stat='sum', na.rm=TRUE, asSample=TRUE)

fu_eut <- raster(paste0("./outputs/", "Euterpe_edulis", "/results",
                        "/Diff_raster/", "Euterpe_edulis_fut_bin.tif"))
fu_eut[fu_eut != 1] <- NA
ocorrencia_area_fu_eut = x * fu_eut
Eut_fu <- cellStats(ocorrencia_area_fu_eut, stat='sum', na.rm=TRUE, asSample=TRUE)


# Predators

# Current

area_0_4 = cu_rich_pred
area_0_4[area_0_4 != 1] <- NA
ocorrencia_area_0_4 = x * area_0_4
ocorrencia_area_cu_0_4_p <- cellStats(ocorrencia_area_0_4, stat='sum', na.rm=TRUE, asSample=TRUE)

area_5_9 = cu_rich_pred
area_5_9[area_5_9 != 2] <- NA
area_5_9[area_5_9 == 2] <- 1
ocorrencia_area_5_9 = x * area_5_9
ocorrencia_area_cu_5_9_p <- cellStats(ocorrencia_area_5_9, stat='sum', na.rm=TRUE, asSample=TRUE)

area_10_13 = cu_rich_pred
area_10_13[area_10_13 != 3] <- NA
area_10_13[area_10_13 == 3] <- 1
ocorrencia_area_10_13 = x * area_10_13
ocorrencia_area_cu_10_13_p <- cellStats(ocorrencia_area_10_13, stat='sum', na.rm=TRUE, asSample=TRUE)

area_14_18 = cu_rich_pred
area_14_18[area_14_18 != 4] <- NA
area_14_18[area_14_18 == 4] <- 1
ocorrencia_area_14_18 = x * area_14_18
ocorrencia_area_cu_14_18_p <- cellStats(ocorrencia_area_14_18, stat='sum', na.rm=TRUE, asSample=TRUE)

# Future

area_0_4 = fu_rich_pred
area_0_4[area_0_4 != 1] <- NA
ocorrencia_area_0_4 = x * area_0_4
ocorrencia_area_fu_0_4_p <- cellStats(ocorrencia_area_0_4, stat='sum', na.rm=TRUE, asSample=TRUE)

area_5_9 = fu_rich_pred
area_5_9[area_5_9 != 2] <- NA
area_5_9[area_5_9 == 2] <- 1
ocorrencia_area_5_9 = x * area_5_9
ocorrencia_area_fu_5_9_p <- cellStats(ocorrencia_area_5_9, stat='sum', na.rm=TRUE, asSample=TRUE)

area_10_13 = fu_rich_pred
area_10_13[area_10_13 != 3] <- NA
area_10_13[area_10_13 == 3] <- 1
ocorrencia_area_10_13 = x * area_10_13
ocorrencia_area_fu_10_13_p <- cellStats(ocorrencia_area_10_13, stat='sum', na.rm=TRUE, asSample=TRUE)

area_14_18 = fu_rich_pred
area_14_18[area_14_18 != 4] <- NA
area_14_18[area_14_18 == 4] <- 1
ocorrencia_area_14_18 = x * area_14_18
ocorrencia_area_fu_14_18_p <- cellStats(ocorrencia_area_14_18, stat='sum', na.rm=TRUE, asSample=TRUE)

# Dispersers

# Current

area_0_3 = cu_rich_disp
area_0_3[area_0_3 != 1] <- NA
r = raster(nrow = area_0_3@nrows, ncol = area_0_3@ncols, xmn = area_0_3@extent@xmin, 
           xmx = area_0_3@extent@xmax, ymn = area_0_3@extent@ymin, ymx = area_0_3@extent@ymax) 
x = raster::area(r) 
ocorrencia_area_0_3 = x * area_0_3
ocorrencia_area_cu_0_3_d <- cellStats(ocorrencia_area_0_3, stat='sum', na.rm=TRUE, asSample=TRUE)

area_4_7 = cu_rich_disp
area_4_7[area_4_7 != 2] <- NA
area_4_7[area_4_7 == 2] <- 1
ocorrencia_area_4_7 = x * area_4_7
ocorrencia_area_cu_4_7_d <- cellStats(ocorrencia_area_4_7, stat='sum', na.rm=TRUE, asSample=TRUE)

area_8_11 = cu_rich_disp
area_8_11[area_8_11 != 3] <- NA
area_8_11[area_8_11 == 3] <- 1
ocorrencia_area_8_11 = x * area_8_11
ocorrencia_area_cu_8_11_d <- cellStats(ocorrencia_area_8_11, stat='sum', na.rm=TRUE, asSample=TRUE)

area_12_15 = cu_rich_disp
area_12_15[area_12_15 != 4] <- NA
area_12_15[area_12_15 == 4] <- 1
ocorrencia_area_12_15 = x * area_12_15
ocorrencia_area_cu_12_15_d <- cellStats(ocorrencia_area_12_15, stat='sum', na.rm=TRUE, asSample=TRUE)

# Future

area_0_3 = fu_rich_disp
area_0_3[area_0_3 != 1] <- NA
ocorrencia_area_0_3 = x * area_0_3
ocorrencia_area_fu_0_3_d <- cellStats(ocorrencia_area_0_3, stat='sum', na.rm=TRUE, asSample=TRUE)

area_4_7 = fu_rich_disp
area_4_7[area_4_7 != 2] <- NA
area_4_7[area_4_7 == 2] <- 1
ocorrencia_area_4_7 = x * area_4_7
ocorrencia_area_fu_4_7_d <- cellStats(ocorrencia_area_4_7, stat='sum', na.rm=TRUE, asSample=TRUE)

area_8_11 = fu_rich_disp
area_8_11[area_8_11 != 3] <- NA
area_8_11[area_8_11 == 3] <- 1
ocorrencia_area_8_11 = x * area_8_11
ocorrencia_area_fu_8_11_d <- cellStats(ocorrencia_area_8_11, stat='sum', na.rm=TRUE, asSample=TRUE)

area_12_15 = fu_rich_disp
area_12_15[area_12_15 != 4] <- NA
area_12_15[area_12_15 == 4] <- 1
ocorrencia_area_12_15 = x * area_12_15
ocorrencia_area_fu_12_15_d <- cellStats(ocorrencia_area_12_15, stat='sum', na.rm=TRUE, asSample=TRUE)


# Proportion related to Euterpe area --------------------------------------

cu_0_4_p <- (ocorrencia_area_cu_0_4_p*100)/Eut_cu
cu_5_9_p <- (ocorrencia_area_cu_5_9_p*100)/Eut_cu
cu_10_13_p <- (ocorrencia_area_cu_10_13_p*100)/Eut_cu
cu_14_18_p <- (ocorrencia_area_cu_14_18_p*100)/Eut_cu

fu_0_4_p <- (ocorrencia_area_fu_0_4_p*100)/Eut_fu
fu_5_9_p <- (ocorrencia_area_fu_5_9_p*100)/Eut_fu
fu_10_13_p <- (ocorrencia_area_fu_10_13_p*100)/Eut_fu
fu_14_18_p <- (ocorrencia_area_fu_14_18_p*100)/Eut_fu

cu_0_3_d <- (ocorrencia_area_cu_0_3_d*100)/Eut_cu
cu_4_7_d <- (ocorrencia_area_cu_4_7_d*100)/Eut_cu
cu_8_11_d <- (ocorrencia_area_cu_8_11_d*100)/Eut_cu
cu_12_15_d <- (ocorrencia_area_cu_12_15_d*100)/Eut_cu

fu_0_3_d <- (ocorrencia_area_fu_0_3_d*100)/Eut_fu
fu_4_7_d <- (ocorrencia_area_fu_4_7_d*100)/Eut_fu
fu_8_11_d <- (ocorrencia_area_fu_8_11_d*100)/Eut_fu
fu_12_15_d <- (ocorrencia_area_fu_12_15_d*100)/Eut_fu


# Results --------------------------------------------------------------------


Results_area_disp_pred <- c(ocorrencia_area_cu_0_4_p, ocorrencia_area_cu_5_9_p,
                            ocorrencia_area_cu_10_13_p, ocorrencia_area_cu_14_18_p,
                            ocorrencia_area_fu_0_4_p, ocorrencia_area_fu_5_9_p,
                            ocorrencia_area_fu_10_13_p, ocorrencia_area_fu_14_18_p,
                            ocorrencia_area_cu_0_3_d, ocorrencia_area_cu_4_7_d,
                            ocorrencia_area_cu_8_11_d, ocorrencia_area_cu_12_15_d,
                            ocorrencia_area_fu_0_3_d, ocorrencia_area_fu_4_7_d,
                            ocorrencia_area_fu_8_11_d, ocorrencia_area_fu_12_15_d,
                            cu_0_4_p, cu_5_9_p, cu_10_13_p, cu_14_18_p,
                            fu_0_4_p, fu_5_9_p, fu_10_13_p, fu_14_18_p,
                            cu_0_3_d, cu_4_7_d, cu_8_11_d, cu_12_15_d,
                            fu_0_3_d, fu_4_7_d, fu_8_11_d, fu_12_15_d)



# Plot --------------------------------------------------------------------

# Richness of dispersers within the E. edulis distribution

# Proportion of dispersers

Current <- c(cu_0_3_d, cu_4_7_d, cu_8_11_d, cu_12_15_d)
Future <- c(fu_0_3_d, fu_4_7_d, fu_8_11_d, fu_12_15_d)
categ <- c("0_03", "04_07", "08_11", "12_15")


df1 <- data.frame(Current, Future, categ)
df2 <- melt(df1, id.vars='categ')
head(df2)

p <- ggplot(df2, aes(x=categ, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge', width = 0.6) +
  # theme(axis.line = element_line(colour = "black"),
  #       panel.background = element_blank()) +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 11),
        #legend.text = element_text(size = 12),
        #axis.ticks = element_blank(),  
        strip.background = element_rect(color="white"),
        panel.spacing = unit(1.5, "lines")) +
  theme(strip.text.x = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none") +
  scale_x_discrete(limit = c("0_03", "04_07", "08_11", "12_15"),
                   labels = c("Very low \n(0-3)", "Low \n(4-7)", 
                              "Moderate \n(8-11)", "High \n(12-15)"))+
  scale_fill_manual(values = c("#8491B4B2", "#4A708B")) +
  scale_y_continuous(expand = c(0, 0))

p

# Proportion of predators

Current <- c(cu_0_4_p, cu_5_9_p, cu_10_13_p, cu_14_18_p)
Future <- c(fu_0_4_p, fu_5_9_p, fu_10_13_p, fu_14_18_p)
categ <- c("0_04", "05_09", "10_13", "14_18")

df1 <- data.frame(Current, Future, categ)
df2 <- melt(df1, id.vars='categ')
head(df2)

g <- ggplot(df2, aes(x=categ, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge', width = 0.6) +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 11),
        legend.text = element_text(size = 12),
        #axis.ticks = element_blank(),  
        strip.background = element_rect(color="white"),
        panel.spacing = unit(1.5, "lines")) +
  theme(strip.text.x = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_x_discrete(limit = c("0_04", "05_09", "10_13", "14_18"),
                   labels = c("Very low \n(0-4)", "Low \n(5-9)", 
                              "Moderate \n(10-13)", "High \n(14-18)"))+
  scale_fill_manual(values = c("#8491B4B2", "#4A708B")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = c(0.8, 0.8))

g


# Saving

g_arrange <-
  grid.arrange(p, g, nrow = 1)
ggsave(
  g_arrange,
  file = "./Figures/Categories_LUH.tiff",
  height = 10,
  width = 20,
  units = "cm"
)
