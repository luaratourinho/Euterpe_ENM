

sp.names <- read_csv("./data/eco_relationship.csv")
records <- read.csv("./outputs/03_clean_df_thin_5.csv", head=T)
family <- read.csv("./outputs/03_n_thinned_records.csv", head=T)

n_records <- records %>%
  group_by(species) %>%
  summarize(lon = n())


table_s1 <- sp.names %>%
  left_join(n_records, by = "species")

write.csv(table_s1, paste0("./", "Table_S1_pre.csv"), row.names = F)


# Histogram ---------------------------------------------------------------

results <- read.csv("./outputs/8_calculating_area.csv", head=T)
results_d_p <- left_join(results, sp.names, by="species")

disp <- table_results_d_p %>%
  group_by(eco_relationship) %>%
  filter(eco_relationship == 'dispersor') 


# Predators

pred <- table_results_d_p %>%
  group_by(eco_relationship) %>%
  filter(eco_relationship == 'predator') 

aa <- hist(disp$net_value_perc)
bb <- hist(pred$net_value_perc)

aa <- ggplot(disp, aes(x=net_value_perc)) + geom_histogram()

g_arrange <-
  grid.arrange(aa, bb, nrow = 2)
ggsave(
  g_arrange,
  file = "./Figures/histogram.tiff",
  height = 20,
  width = 30,
  units = "cm"
)
