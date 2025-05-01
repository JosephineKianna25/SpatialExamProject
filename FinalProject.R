# Install packages

install.packages(c("sf", "osmdata", "mapview", "sfnetworks", "tidygraph", "dplyr", "igraph", "units", "rnaturalearth", "rnaturalearthdata"))

# Load libraries

library(sf)
library(osmdata)
library(mapview)
library(sfnetworks)
library(tidygraph)
library(dplyr)
library(units)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# --------------------------------------------------
## Define a bounding box for the network area to go 1500 km south of Aarhus

# Set Aarhus coordinates

aarhus <- st_sfc(st_point(c(10.2039, 56.1629)), crs = 4326)

# Define bounding box (from Denmark down to near countries in Europe)

bbox <- c(xmin = 5, ymin = 48, xmax = 15, ymax = 56)

# Get road network (motorways and trunks)

query_motorway <- opq(bbox = bbox, timeout = 180) %>%
  add_osm_feature(key = "highway", value = "motorway")

query_trunk <- opq(bbox = bbox, timeout = 180) %>%
  add_osm_feature(key = "highway", value = "trunk")

roads_motorway <- osmdata_sf(query_motorway)$osm_lines
roads_trunk <- osmdata_sf(query_trunk)$osm_lines

# Find common columns
common_cols <- intersect(names(roads_motorway), names(roads_trunk))

# Keep only those columns
roads_motorway_common <- roads_motorway[, common_cols]
roads_trunk_common <- roads_trunk[, common_cols]

# Now you can safely bind them
roads <- rbind(roads_motorway_common, roads_trunk_common)

# Use projected CRS for accurate distance and convert to sf network

roads <- st_transform(roads, 3857) 

network <- as_sfnetwork(roads, directed = FALSE) %>%
  activate("edges") %>%
  mutate(weight = edge_length())

# Project Aarhus point to match network CRS

aarhus_proj <- st_transform(aarhus, 3857)

# Attach Aarhus to nearest network node

nodes <- st_as_sf(network, "nodes")

nearest_node_id <- st_nearest_feature(aarhus_proj, nodes)

# Set distance threshold to 1500 km 

max_dist <- set_units(1500000, "m")

# Strip units from weights for igraph

weights <- network %>%
  activate("edges") %>%
  pull(weight) %>%
  drop_units()  

# Filter nodes reachable within 1500 km

distances <- igraph::distances(network, v = nearest_node_id, weights = network %>% activate("edges") %>% pull(weight))
distances <- set_units(distances, "m")
reachable_nodes <- which(distances <= max_dist)

# Filter the network to only include edges between reachable nodes

sub_network <- network %>%
  activate("edges") %>%
  filter(from %in% reachable_nodes & to %in% reachable_nodes) %>%
  activate("nodes") %>%
  slice(reachable_nodes)

# --------------------------------------------------
## Intersect large European cities with the reachable network

# Download cities with a population > 100k from OSM 

cities_osm <- opq(bbox = bbox, timeout = 180) %>%
  add_osm_feature(key = "place", value = "city") %>%
  osmdata_sf()

# Filter for cities with population > 100k 

cities_sf <- cities_osm$osm_points
cities_sf_clean <- cities_sf %>%
  dplyr::select(name, population, geometry)

cities <- cities_sf_clean %>%
  filter(!is.na(population) & population > 100000)

# Transform CRS for consistent spatial analysis

cities <- st_transform(cities, 3857)

# Define reachable area

reachable_area <- st_as_sf(sub_network, "edges") %>%
  st_geometry() %>%
  st_buffer(10000) %>%
  st_union()

# Get the cities that intersect with the reachable area

reachable_cities <- cities[st_intersects(cities, reachable_area, sparse = FALSE), ]

# Convert previous bbox to an sf polygon in the same CRS

bbox_polygon <- st_as_sfc(st_bbox(c(xmin = 5, ymin = 48, xmax = 15, ymax = 56), crs = 4326)) %>%
  st_transform(3857)

# Load country outlines and clip to 1500 km polygon

countries <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(3857)

countries_clipped <- st_intersection(countries, bbox_polygon)

# Plot cities reachable within 1500 km road distance from Aarhus

print(
  ggplot() +
    geom_sf(data = countries_clipped, fill = "palegreen", color = "white", size = 0.5) +
    geom_sf(data = st_as_sf(sub_network, "edges"), color = "gray25") +
    geom_sf(data = reachable_cities, aes(color = name), size = 3) +
    geom_sf(data = aarhus_proj, color = "red", size = 4) +
    labs(
      title = "Cities Reachable Within 1500 km from Aarhus by Road",
      color = "City"
    ) +
    theme_minimal()
)


