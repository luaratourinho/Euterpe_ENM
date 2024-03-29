
# Credits ---------------------------

# Created by
# Sara Mortara (https://github.com/saramortara/data_cleaning)
# Bruno M. Carvalho (https://github.com/brunomc-eco)

# Edited by
# Luara Tourinho (https://github.com/luaratourinho)

# Date: 04 May 2021


# Cleaning records --------------------------------------------------------



# Required packages

library(tidyverse)
library(CoordinateCleaner)
library(countrycode)



# reading data with species occurrences

searches_df  <- read_csv("./data/1_myoccs_gbif.csv")

splist <- read.csv("./data/1_myoccs_gbif.csv",
                stringsAsFactors = FALSE) %>%
  pull(species)

splist <- unique(splist)


# removing records with NA coordinates, keeping only species from our list
searches_occs <- searches_df %>%
  filter(!is.na(lon) & !is.na(lat)) %>%
  filter(species %in% splist)


# Viewing unclean records
ggplot()+ coord_fixed()+
  borders("world", colour="gray50", fill="gray50") +
  geom_point(data = searches_occs, aes(x = lon, y = lat),
             colour = "yellow", size = 0.5)+
  theme_bw()

searches_occs$lon <- ifelse(is.na(searches_occs$lon), 
                            0, searches_occs$lon)


# finding outliers data (flagged records - e.g., at the sea, city centroid, capital... if you know that the target species do not occur there)

flags_occs <- clean_coordinates(
  x = searches_occs,
  lon = "lon",
  lat = "lat",
  # countries = "countrycode",
  # centroids_rad = 2000,
  # had to increase this limit because was not flagging the centroid of Brazil
  species = "species",
  tests = c(
    #"capitals",
    # flags records at adm-0 capitals
    #"centroids",
    # flags records at country centroids
    #"equal",
    # flags records with equal lon and lat
    "gbif",
    # flags records at gbif headquarters
    #"institutions",
    # flags records at biodiversity institutions
    "seas",
    # flags records at sea
    "zeros"
  )
) # flags records with zero lon or lat

# Viewing flagged records
plot(flags_occs, lon = "lon", lat = "lat")

# Removing flagged records and duplicates
searches_occs_clean1 <- searches_occs[flags_occs$.summary, ] %>%
  distinct()



# Cleaning by worldclim files ------------------------------------------------
# use one of the raster that refer to the bioclimatic variable. It is just to make sure that any coordinate is out of the raster

library(raster)

searches_occs_clean2 = searches_occs_clean1

variable_world <- raster("./Environmental_data/Worldclim/wc2.1_2.5m_bio/wc2.1_2.5m_bio_1.tif")

# All Americas - if you want only the coordinates from Americas, for example
variable <- crop(variable_world, c(-130, -25, -50, 50))
coordinates(searches_occs_clean2) <- ~lon+lat
proj4string(searches_occs_clean2)=CRS("+proj=longlat +datum=WGS84")
searches_prj<-spTransform(searches_occs_clean2,
                          CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(variable, axes=T)
plot(searches_prj, add=T, col= "red")

varcrop = crop(variable, searches_prj)

searches_extract <- raster::extract(varcrop, searches_prj, method = "bilinear")
searches_prjext<- data.frame(searches_prj,searches_extract)

which(searches_prjext$searches_extract ==0)
which(is.na(searches_prjext$searches_extract))
search_occ_extracted <- searches_prjext[!is.na(searches_prjext$searches_extract),]
head(search_occ_extracted)

# keep only the columns of your interest
search_occ_extracted <- search_occ_extracted[,c("family", "species", "lon", 
                                                "lat", "year", "country")]

# check the data
head(search_occ_extracted)
dim(search_occ_extracted)



# Cleaning old date -------------------------------------------------------

search_occ_by_date <- search_occ_extracted %>%
  filter(year >= 1950)

# Number of records -------------------------------------------------------

n_records <- count(search_occ_by_date, species)
n_records_before <- count(search_occ_extracted, species)

# Writing outputs ---------------------------------------------------------
# I created the 'output' directory just to save the outputs from the whole ENM, but you can save in anywhere

write_csv(n_records, path = "./outputs/02_n_records.csv")
write_csv(search_occ_by_date, path = "./outputs/02_clean_occ.csv")
#write_csv(n_records_before, path = "./data/02_n_records.csv")
#write_csv(search_occ_extracted, path = "./data/02_clean_occs.csv")

# END
