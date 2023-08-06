
# Credits ---------------------------

# Created by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 22 Feb 2022


# Required packages

library(tidyverse)
library(dplyr)
library(ggplot2)


# Reading file with species names
table_results <- read_csv("./outputs/13_calculating_area_LUH.csv")
sp.names <- read_csv("./data/eco_relationship.csv")

table_results_d_p <- left_join(table_results, sp.names, by="species")

disp <- table_results_d_p %>%
  group_by(eco_relationship) %>%
  filter(eco_relationship == 'dispersor') 


# Predators

pred <- table_results_d_p %>%
  group_by(eco_relationship) %>%
  filter(eco_relationship == 'predator') 

colnames(pred)
#"ocorrencia_area_cu" "ocorrencia_area_fu"

# Plots -------------------------------------------------------------------

# Dispersers

# Current <- disp$ocorrencia_area_cu
# Future <- disp$ocorrencia_area_fu
# disp$species <- gsub(x = disp$species, pattern = "_", replacement = " ")
# categ <- disp$species

# *1000
Current <- c(162, 83, 18, 108, 3883, 428, 519, 98, 260, 2290, 196, 1000, 325, 367, 11)
Future <- c(93, 44, 16, 47, 3881, 289, 324, 48, 140, 2222, 91, 833, 166, 218, 1)
categ <- c("C. cucullata","C. melanocephala","C. blumenbachii",
           "L. lanioides","L. vociferans","P. obscura","P. superciliaris",
           "P. jacutinga", "P. nudicollis","P. aracari","P. bailloni",
           "P. scutatus","R. dicolorus","S. maculirostris","T. atra")


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
        axis.text.x = element_text(size = 11, angle = 60, hjust=1, face = "italic"),
        #legend.text = element_text(size = 12),
        #axis.ticks = element_blank(),  
        strip.background = element_rect(color="white"),
        panel.spacing = unit(1.5, "lines")) +
  theme(strip.text.x = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_blank(),
        legend.position = "none") +
  scale_fill_manual(values = c("#8491B4B2", "#4A708B")) +
  #scale_y_continuous(expand = c(0, 0))
  scale_y_continuous(expand = c(0, 0), limits = c(0,550))

p

# Predators


# Current <- pred$ocorrencia_area_cu
# Future <- pred$ocorrencia_area_fu
# pred$species <- gsub(x = pred$species, pattern = "_", replacement = " ")
# categ <- pred$species

Current <- c(179, 3, 165, 523, 3256, 119, 3373, 100, 6301, 38, 4060, 24, 1307, 
             2225, 451, 4336, 312, 123, 65)
Future <- c(85, 1, 82, 362, 3236, 58, 3351, 39, 6177, 8, 4021, 17, 1154,
            2221, 297, 4306, 171, 69, 13)
categ <- c("A. montensis", "A. brasiliensis", "B. tirica", "C. tataupa",
           "C. paca", "D. azarae", "D. leporina", "E. russatus", "G. montana",
           "G. ingrami", "M. americana", "M. nana", "N. squamipes", "P. tajacu",
           "P. frontalis", "T. pecari", "T. solitarius", "T. malachitacea", 
           "T. iheringi")

df1 <- data.frame(Current, Future, categ)
df2 <- melt(df1, id.vars='categ')
head(df2)

g <- ggplot(df2, aes(x=categ, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge', width = 0.6) +
  theme_classic() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(size = 10, colour = "black"),
        axis.text.x = element_text(size = 11, angle = 60, hjust=1, face = "italic"),
        legend.text = element_text(size = 12),
        #axis.ticks = element_blank(),  
        strip.background = element_rect(color="white"),
        panel.spacing = unit(1.5, "lines")) +
  theme(strip.text.x = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12),
        legend.position="bottom") +
  scale_fill_manual(values = c("#8491B4B2", "#4A708B")) +
  #scale_y_continuous(expand = c(0, 0))
  scale_y_continuous(expand = c(0, 0), limits = c(0,550))

g


# Saving

g_arrange <-
  grid.arrange(p, g, nrow = 2)
ggsave(
  g_arrange,
  file = "./Figures/Species_area_dist_LUH_zoom.tiff",
  height = 20,
  width = 15,
  units = "cm"
)
