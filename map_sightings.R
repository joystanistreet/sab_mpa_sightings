
# Script to create maps of WSDB sightings for St. Anns Bank Technical Report

# Joy Stanistreet February 2023

### Set up

# load packages
pacman::p_load(here, tidyverse, sf, RColorBrewer)

# read in saved bathymetry
bf <- readRDS("bathymetry.RDS") %>% 
  filter(z>-3500) %>% 
  mutate(z_adj = ifelse(z >= 1000, 0, z))

# load MPA shapefile
all_mpas <- read_sf("R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/shapefiles/ProtectedAreas/DFO/OA_MPAs/EastCan_MPAS.shp") %>% 
  st_transform(4326)

# extract SAB MPA
sab_mpa <- all_mpas %>% 
  filter(NAME_E == "St. Anns Bank Marine Protected Area")

# load hi res land data (sourced from Open Gov Atlas, saved as shapefile)
canada <- read_sf("R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/shapefiles/coastline/canada/clipped/canada_simple.shp") %>% 
  st_transform(4326)

# load coordinates for bounding box (used for WSDB data request) and create sf polygon
box <- read_csv("bounding_box_WSDB_sightings.csv") %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = st_crs(4326))

polygon <- box %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")


# load sightings data from WSDB
all_sightings<-read_csv(here("Joy_cetaceans_Feb2024.csv")) %>% 
  mutate(species = as_factor(COMMONNAME))

### set up species groups (these lines used for INITIAL SET UP ONLY)

#species_list<-tibble(species = levels(all_sightings$species))
#write_csv(species_list, "species.csv")

### MANUALLY in Excel: added 'group' and 'species_name' columns to csv

# add group and species_name variables to sightings data
species_groups<-read_csv("species_groups.csv")

all_sightings <- all_sightings %>% 
  left_join(species_groups, by = "species") %>% 
  mutate(group = as_factor(group))

# create spatial feature of sighting positions
sightings_sf <- st_as_sf(all_sightings, 
                         coords = c("LONGITUDE", "LATITUDE"), 
                         crs = st_crs(4326))


### summarize for tables


# summarize by species
all_sightings_summary <- all_sightings %>% 
  group_by(species_name) %>% 
  summarize(count = n())

# spatially filter for sightings located within SAB MPA and summarize by species
sab_sightings_summary <- st_join(sightings_sf, sab_mpa) %>% 
  filter(NAME_E == "St. Anns Bank Marine Protected Area") %>% 
  group_by(species_name) %>% 
  summarize(count = n()) %>% 
  st_drop_geometry()


### create maps based on group

for (i in levels(sightings_sf$group)){

  # filter by group for mapping
  plot_sightings <- sightings_sf %>% 
    filter(group == i)
  
  # create map
  sightings_map<-ggplot() +
    
    # add bathymetry
    geom_raster(data = bf, aes(x = x, y = y, fill = z_adj)) +
    scale_fill_distiller(palette = "Blues", guide = 'none') +
    
    # add land 
    geom_sf(data = canada, color=NA, fill="grey60") +
    
    # add SAB MPA 
    geom_sf(data = sab_mpa, 
            col = "darkblue", 
            fill = "darkblue", 
            alpha = 0.2, 
            linewidth = 0.25) +
    
    # add bounding box
    geom_sf(data = polygon, col = "black", fill = NA, linewidth = 0.5) +
    
    # add sightings
    geom_sf(data = plot_sightings, aes(colour = species_name),
            shape = 16, size = 1) +
    
    # set colors
    scale_colour_brewer("Species", palette = "Set1") +
    
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
  
  # create output figure name
  output_file <- paste0("SAB_", i, "_sightings_map.png")
  
  # save output figure
  ggsave(here("output_maps", output_file), sightings_map, width = 6.5, height = 3.5, units = "in", dpi = 300)
  
}
