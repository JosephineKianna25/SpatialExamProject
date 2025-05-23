# Her laver vi bounding box

# Script 1: Simple Buffer 2000 km South of Aarhus

# Install packages
install.packages(c("sf", "ggplot", "rnaturalearth", "rnaturalearthdata", "osmdata", "dplyr", "ggrepel"))

# Load required libraries
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(osmdata)
library(dplyr)
library(ggrepel)
library(viridis)

# Set CRS

crs_wgs <- 4326
crs_proj <- 3857  # Web Mercator for distance calculation

# Aarhus coordinates

aarhus <- st_sfc(st_point(c(10.2039, 56.1629)), crs = crs_wgs)
aarhus_proj <- st_transform(aarhus, crs_proj)
aarhus_coords <- st_coordinates(aarhus_proj)

# Create 2000 km southward buffer

buffer_width <- 2000000  # 2000 km east-west
buffer_height <- 2000000  # 2000 km south

xmin <- aarhus_coords[1] - buffer_width / 2
xmax <- aarhus_coords[1] + buffer_width / 2
ymin <- aarhus_coords[2] - buffer_height
ymax <- aarhus_coords[2]

buffer_rect <- st_polygon(list(rbind(
  c(xmin, ymin),
  c(xmin, ymax),
  c(xmax, ymax),
  c(xmax, ymin),
  c(xmin, ymin)
))) %>%
  st_sfc(crs = crs_proj)

# Load large cities from OSM (within a wide bounding box)

bbox <- c(xmin = 0, ymin = 40, xmax = 25, ymax = 60)
cities_osm <- opq(bbox = bbox, timeout = 120) %>%
  add_osm_feature(key = "place", value = "city") %>%
  osmdata_sf()

cities <- cities_osm$osm_points %>%
  dplyr::select(name, population, geometry) %>%
  filter(!is.na(population)) %>%
  mutate(population = as.numeric(population)) %>%
  filter(population > 500000) %>%
  st_transform(crs_proj)

# Get cities within the buffer

cities_in_buffer <- cities[st_intersects(cities, buffer_rect, sparse = FALSE), ]

# Load countries and crop

countries <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs_proj)
countries_clip <- st_intersection(countries, buffer_rect)

# Plot

ggplot() +
  geom_sf(data = countries_clip, fill = "antiquewhite") +
  geom_sf(data = buffer_rect, fill = NA, color = "blue", linetype = "dashed") +
  geom_sf(data = cities_in_buffer, color = "darkgreen", size = 3) +
  geom_text_repel(
    data = cities_in_buffer,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    size = 3,
    max.overlaps = 15
  ) +
  geom_sf(data = aarhus_proj, color = "red", size = 4) +
  labs(
    title = "Cities Within 2000 km Buffer South of Aarhus",
  ) +
  theme_minimal()


# Choose cities
highlighted_cities <- c("Lyon", "Torino", "Zagreb", "Budapest")

# 2. Filter chosen cities
cities_highlight <- cities_in_buffer %>%
  filter(name %in% highlighted_cities)

# Create buffer circles og 50 km radius = 50000 meters
highlight_buffers <- st_buffer(cities_highlight, dist = 50000)

# Add to plot 
ggplot() +
  geom_sf(data = countries_clip, fill = "antiquewhite") +
  geom_sf(data = buffer_rect, fill = NA, color = "blue", linetype = "dashed") +
  geom_sf(data = highlight_buffers, fill = NA, color = "black", size = 5, linetype = "longdash") +
  geom_sf(data = cities_in_buffer, color = "darkgreen", size = 3) +
  geom_text_repel(
    data = cities_in_buffer,
    aes(label = name, geometry = geometry),
    stat = "sf_coordinates",
    size = 3,
    max.overlaps = 15
  ) +
  geom_sf(data = aarhus_proj, color = "red", size = 4) +
  labs(
    title = "Cities Within 2000 km Buffer South of Aarhus"
  ) +
  theme_minimal()
  
  
  
  
  
  
  
  
  
  




