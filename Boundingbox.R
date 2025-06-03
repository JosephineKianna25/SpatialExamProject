# Install packages (run only once)
install.packages(c("sf", "ggplot2", "rnaturalearth", "rnaturalearthdata", "osmdata", "dplyr", "ggrepel", "viridis"))

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

# Compute distance from Aarhus to every city in buffer, in kilometers
cities_in_buffer$distance_km <- as.numeric(st_distance(aarhus_proj, cities_in_buffer)) / 1000
cities_in_buffer$label <- paste0(
  cities_in_buffer$name,
  "\n",
  round(cities_in_buffer$distance_km, 0), " km"
)

# Load countries and crop
countries <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs_proj)
countries_clip <- st_intersection(countries, buffer_rect)

# Choose cities
highlighted_cities <- c("Lyon", "Torino", "Zagreb", "Budapest")

# 2. Filter chosen cities
cities_highlight <- cities_in_buffer %>%
  filter(name %in% highlighted_cities)

# Create buffer circles of 50 km radius
highlight_buffers <- st_buffer(cities_highlight, dist = 50000)

# Aarhus label
aarhus_label_df <- data.frame(
  label = "Aarhus\n0 km",
  X = aarhus_coords[1],
  Y = aarhus_coords[2]
)

# Labels for all cities
cities_label_df <- cities_in_buffer %>%
  st_drop_geometry() %>%
  mutate(
    X = st_coordinates(cities_in_buffer)[,1],
    Y = st_coordinates(cities_in_buffer)[,2]
  )

label_df <- bind_rows(cities_label_df, aarhus_label_df)

# Assign city types
cities_in_buffer$type <- "Other major cities"
cities_highlight$type <- "Selected cities (Aarhus + destinations)"
aarhus_sf <- st_sf(geometry = aarhus_proj, type = "Selected cities (Aarhus + destinations)")

# Combine all cities
all_cities <- rbind(
  cities_in_buffer %>% select(geometry, type),
  cities_highlight %>% select(geometry, type),
  aarhus_sf
)

# Final Plot
ggplot() +
  # Countries background
  geom_sf(data = countries_clip, fill = "antiquewhite") +
  # Buffer rectangle
  geom_sf(data = buffer_rect, fill = NA, color = "blue", linetype = "dashed") +
  # Highlighted city buffers
  geom_sf(data = highlight_buffers, fill = NA, color = "black", size = 2, linetype = "longdash") +
  # Plot all cities with colors based on type
  geom_sf(data = all_cities, aes(color = type), size = 3) +
  # Labels
  geom_text_repel(
    data = label_df,
    aes(x = X, y = Y, label = label),
    size = 3,
    family = "Times New Roman",
    fontface = "bold",
    max.overlaps = 15
  ) +
  # Color legend
  scale_color_manual(values = c(
    "Selected cities (Aarhus + destinations)" = "red",
    "Other major cities" = "darkgreen"
  )) +
  labs(
    title = "Major Cities Within 2000 km South of Aarhus (Population > 500,000)",
    color = "City Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5)
  )

ggsave("bbox_destinations.png", width = 10, height = 6, dpi = 300)
