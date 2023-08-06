
# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 22 Feb 2022


# Required packages
library(raster)
library(dplyr)
library(rgeos)
library(reshape)
library(rgdal)
library(ggmap)
library(ggplot2)
library(scico)
library("gridExtra")


# Creating objects for the loop

number_ticks <- function(n) {
  function(limits)
    pretty(limits, n)
}


# Richness of dispersers and predators ------------------------------------


# Binary ------------------------------------------------------------------

# Current

# Reading files

#raster_files <- list.files("./Richness/", full.names = T, 'tif$|bil$')
#rasters_bin <- stack(raster_files)
rasters_bin <- stack(cu_rich_disp, cu_rich_pred, fu_rich_disp, fu_rich_pred)

n <- length(rasters_bin@layers)
p <- list()

for (i in 1:n) {
map.p <- rasterToPoints(rasters_bin[[i]])
df <- data.frame(map.p)
colnames(df) <- c("Longitude", "Latitude", "Adequabilidade ambiental")

p[[i]] <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
  geom_raster(aes(fill = `Adequabilidade ambiental`)) + theme_bw() +
  coord_equal() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16, angle = 90),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(0.5, 'cm')
  ) +
  # scico::scale_fill_scico(palette = "lajolla") +
  # scale_colour_brewer(palette="BuPu", direction=-1) +
   scale_fill_distiller(palette = "Spectral") +
  #RColorBrewer::display.brewer.all("Spectral") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank()) + 
  theme(legend.justification = c(0.5, 0),
        legend.position = c(0.85, 0.05),
        legend.text = element_text(size=13)) +
  # labs(title = "Acritopappus harleyi\n") +
  #labs(title = paste0(target_species[i], "\n")) +
  theme(plot.title = element_text(
    lineheight = .8,
    face = "italic",
    size = 20
  )) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),
                     breaks = number_ticks(3))
}

p[[1]]

p_arrange <-
  grid.arrange(p[[1]], p[[2]], p[[3]], p[[4]], nrow = 2)
ggsave(
  p_arrange,
  file = "./Richness/Figures/Disp_pred_bin.tiff",
  height = 20,
  width = 21,
  units = "cm"
)


# Continuous


rasters_cont <- stack(cu_cont_rich_disp, cu_cont_rich_pred, fu_cont_rich_disp, fu_cont_rich_pred)

n <- length(rasters_cont@layers)
g <- list()

for (i in 1:n) {
  map.p <- rasterToPoints(rasters_cont[[i]])
  df <- data.frame(map.p)
  colnames(df) <- c("Longitude", "Latitude", "Adequabilidade ambiental")
  
  g[[i]] <- ggplot(data = df, aes(y = Latitude, x = Longitude)) +
    geom_raster(aes(fill = `Adequabilidade ambiental`)) + theme_bw() +
    coord_equal() +
    theme(
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16, angle = 90),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "right",
      legend.title = element_blank(),
      legend.key = element_blank(),
      legend.key.size = unit(0.5, 'cm')
    ) +
    # scico::scale_fill_scico(palette = "lajolla") +
    scale_fill_distiller(palette = "Spectral") +
    theme(axis.title.y = element_blank(),
          axis.title.x = element_blank()) + 
    theme(legend.justification = c(0.5, 0),
          legend.position = c(0.85, 0.05),
          legend.text = element_text(size=13)) +
    # labs(title = "Acritopappus harleyi\n") +
    #labs(title = paste0(target_species[i], "\n")) +
    theme(plot.title = element_text(
      lineheight = .8,
      face = "italic",
      size = 20
    )) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1),
                       breaks = number_ticks(3))
}

g[[1]]

g_arrange <-
  grid.arrange(g[[1]], g[[2]], g[[3]], g[[4]], nrow = 2)
ggsave(
  g_arrange,
  file = "./Richness/Figures/Disp_pred_cont.tiff",
  height = 20,
  width = 21,
  units = "cm"
)


# Reverse order of scale, if needed
# https://stackoverflow.com/questions/8750871/ggplot2-reverse-order-of-scale-brewer