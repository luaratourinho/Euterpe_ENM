# Credits ---------------------------

# Script created by
# Bruno M. Carvalho (https://github.com/brunomc-eco)

# Edited by
# Luara Tourinho (https://github.com/luaratourinho)

# Last update: 02 Feb 2022
# Odoyá!


# Getting species records -------------------------------------------------



# Required packages

library(tidyverse)
library(rgbif)
library(taxize) # for get_gbifid_
library(data.table)
library(dplyr)
library(purrr)
library(readr)
library(magrittr) # for %T>% pipe
library(rgbif) # for occ_download
#devtools::install_github("liibre/Rocc")
library(Rocc) # fpr spescieslink



# Creating/reading our species list


splist <- read.csv("./data/function.csv", stringsAsFactors = FALSE) %>%
  pull(species)



# GBIF --------------------------------------------------------------------


# getting records from gbif
# get this code from https://data-blog.gbif.org/post/downloading-long-species-lists-on-gbif/
# Manual of rgbif: https://cran.r-project.org/web/packages/rgbif/rgbif.pdf

gbif_taxon_keys <- splist %>%
  get_gbifid_(method="backbone") %>% # get taxonkeys for each species name
  imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back to data.frame
  bind_rows() # combine all results in a single data.frame

only_keys <- gbif_taxon_keys %>%
  filter(matchtype == "EXACT" & status == "ACCEPTED") %>% # get only accepted names
  pull(usagekey) #retain only the taxonkeys

# download data directly at GBIF
# (file needs to be manually fetched at the user's downloads page at gbif.org)

# enter GBIF credentials
user <- "xxx" # your gbif.org username
pwd <- "xxx" # your gbif.org password
email <- "xxx" # your email

occ_download(
  pred_in("taxonKey", only_keys),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "SIMPLE_CSV",
  user = user, pwd = pwd, email = email
)

# https://doi.org/10.15468/dl.6karx3
gbif_df <- fread("./data/0120643-210914110416597.csv", na.strings = c("", NA))

# Chose the colunms of your interest
gbif_df2 <- gbif_df[,c("family","species","decimalLongitude","decimalLatitude",
                       "year","countryCode")]
head(gbif_df2)
colnames(gbif_df2) <- c("family", "species", "lon", "lat", "year", "country")

gbif_table <- tibble(gbif_df2)

# Adding more data (in this case, I added data given by collaborators)
myoccs <- read_csv("./data/species_by_rita_metz.csv")

# Join gbif table to other data
searches <- full_join(gbif_table, myoccs)


# Saving outputs ----------------------------------------------------------

write_csv(searches, "./data/1_myoccs_gbif.csv") #all data together (gbif + other data)
write_csv(gbif_table, "./data/1_gbif.csv") #the original table from gbif, in case you want to check later


### END
