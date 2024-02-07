
# Script to extract and summarize sightings from within St Anns Bank MPA


### Set up

# load packages
pacman::p_load(here, tidyverse, sf)

# load MPA shapefile
all_mpas <- read_sf(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_2\shapefiles\ProtectedAreas\DFO\OA_MPAs\EastCan_MPAS.shp)") %>% 
  st_transform(4326)

# extract SAB MPA
sab_mpa <- all_mpas %>% 
  filter(NAME_E == "St. Anns Bank Marine Protected Area")

# load sightings and prepare sightings data

all_sightings <- read_csv(here("Joy_cetaceans_Feb2024.csv")) %>% 
  mutate(species = as_factor(COMMONNAME))

species_groups <- read_csv("species_groups.csv")

all_sightings <- all_sightings %>% 
  left_join(species_groups, by = "species")

# create spatial feature of sighting coordinates

sightings_sf <- st_as_sf(all_sightings, 
                         coords = c("LONGITUDE", "LATITUDE"), 
                         crs = st_crs(4326))

# filter for sightings located within SAB MPA

sightings_sab_sf <- st_join(sightings_sf, sab_mpa) %>% 
  filter(NAME_E == "St. Anns Bank Marine Protected Area")

# summarize by species

sightings_sab <- sightings_sab_sf %>% 
  group_by(species_name) %>% 
  summarize(count = n()) %>% 
  st_drop_geometry()

# summarize by ID certainty

sightings_all_cert <- sightings_sf %>% 
  group_by(IDREL_CD) %>% 
  summarize(count = n())

sightings_sab_cert <- sightings_sab_sf %>% 
  group_by(IDREL_CD) %>% 
  summarize(count = n())



