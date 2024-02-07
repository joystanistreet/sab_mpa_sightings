
# Script to create maps of cetacean sightings from WSDB around SAB MPA


### Set up

# load packages
pacman::p_load(here, tidyverse, sf, RColorBrewer)

# read in saved bathymetry
bf <- readRDS("bathymetry.RDS") %>% 
  filter(z>-3500) %>% 
  mutate(z_adj = ifelse(z >= 1000, 0, z))

# load MPA shapefile
all_mpas <- read_sf(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_2\shapefiles\ProtectedAreas\DFO\OA_MPAs\EastCan_MPAS.shp)") %>% 
  st_transform(4326)

# extract SAB MPA
sab_mpa <- all_mpas %>% 
  filter(NAME_E == "St. Anns Bank Marine Protected Area")

# load hi res land data (sourced from Open Gov Atlas, saved as shapefile)
canada <- read_sf("R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/shapefiles/coastline/canada/clipped/canada_simple.shp") %>% 
  st_transform(4326)

# load coordinates for bounding box (used for WSDB data request) and create spatial feature

box <- read_csv("bounding_box_WSDB_sightings.csv") %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = st_crs(4326))

polygon <- box %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")


# load sightings data

all_sightings<-read_csv(here("Joy_cetaceans_Feb2024.csv")) 

all_sightings <- all_sightings %>% 
  mutate(species = as_factor(COMMONNAME))

## set up species groups

#species_list<-tibble(species = levels(all_sightings$species))

#write_csv(species_list, "species_groups.csv")

## MANUALLY ADDED 'group' COLUMN TO 'species_list' CSV

# add species group variable to sightings data

species_groups<-read_csv("species_groups.csv")

all_sightings <- all_sightings %>% 
  left_join(species_groups, by = "species")


# create spatial feature of sighting coordinates

sightings_sf <- st_as_sf(all_sightings, 
                         coords = c("LONGITUDE", "LATITUDE"), 
                         crs = st_crs(4326))


# filter by group for mapping

plot_sightings <- sightings_sf %>% 
  filter(group == "unknown")

### create map

map<-ggplot() +
  
  ## add bathymetry
  geom_raster(data = bf, aes(x = x, y = y, fill = z_adj)) +
  scale_fill_distiller(palette = "Blues", guide = 'none') +
  
  # add land region
  #geom_polygon(data = reg, aes(x = long, y = lat, group = group), 
   #            color=NA, fill="grey60") +
  
  # add land 
  geom_sf(data = canada, color=NA, fill="grey60") +
  
  # add mpa 
  geom_sf(data = sab_mpa, 
          col = "darkblue", 
          fill = "darkblue", 
          alpha = 0.2, 
          linewidth = 0.25) +
  
  # add box
  geom_sf(data = polygon, col = "black", fill = NA, linewidth = 0.5) +
  
  # add sightings
  geom_sf(data = plot_sightings, aes(colour = species_name),
             shape = 16, size = 1) +
  
  # set colors
  scale_colour_brewer("Species", palette = "Set1") +
  #scale_colour_brewer("Species", palette = "Dark2") +
  
  # set area
  coord_sf(xlim = c(-62, -56), ylim = c(44.4, 47.8), expand = FALSE) +
  
  # format axes
  ylab("") + 
  xlab("") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 9),
        legend.key = element_rect(fill = NA),
        plot.margin = margin(0.2,0.05,0.05,0.05,"in"))
  
#ggsave(here("output_maps", "baleen_whale_sightings_map.png"), map, width = 6.5, height = 3.5, units = "in", dpi = 300)

#ggsave(here("output_maps", "large_odontocete_sightings_map.png"), map, width = 6.5, height = 3.5, units = "in", dpi = 300)

#ggsave(here("output_maps", "small_odontocete_sightings_map.png"), map, width = 6.5, height = 3.4, units = "in", dpi = 300)

ggsave(here("output_maps", "unknown_sightings_map.png"), map, width = 6.5, height = 3.5, units = "in", dpi = 300)
