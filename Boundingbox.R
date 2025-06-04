# Install packages (run only once)
install.packages(c("sf", "ggplot2", "rnaturalearth", "rnaturalearthdata", "osmdata", "dplyr", "ggrepel", "viridis"))

# Loading required libraries
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(osmdata)
library(dplyr)
library(ggrepel)
library(viridis)

# Setting up the coordinate reference systems
crs_wgs <- 4326
crs_proj <- 3857  # Web Mercator for distance calculation

# Aarhus coordinates
aarhus <- st_sfc(st_point(c(10.2039, 56.1629)), crs = crs_wgs)
aarhus_proj <- st_transform(aarhus, crs_proj)
aarhus_coords <- st_coordinates(aarhus_proj)

# Creating 2000 km southward buffer
buffer_width <- 2000000  # 2000 km east-west
buffer_height <- 2000000  # 2000 km south

# Creating a rectangle buffer around Aarhus
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

# Bounding box for OpenStreetMap query
bbox <- c(xmin = 0, ymin = 40, xmax = 25, ymax = 60)

# Query OpenStreetMap for cities
cities_osm <- opq(bbox = bbox, timeout = 120) %>%
  add_osm_feature(key = "place", value = "city") %>%
  osmdata_sf()

# Filtering and transforming cities data
cities <- cities_osm$osm_points %>%
  dplyr::select(name, population, geometry) %>%
  filter(!is.na(population)) %>%
  mutate(population = as.numeric(population)) %>%
  filter(population > 500000) %>%
  st_transform(crs_proj)

# Filtering cities within the buffer rectangle
cities_in_buffer <- cities[st_intersects(cities, buffer_rect, sparse = FALSE), ]

# Calculating distances and preparing labels
cities_in_buffer$distance_km <- as.numeric(st_distance(aarhus_proj, cities_in_buffer)) / 1000

# Creating labels for cities
cities_in_buffer$label <- paste0(
  cities_in_buffer$name,
  "\n",
  round(cities_in_buffer$distance_km, 0), " km"
)

# Preparing the countries layer
countries <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs_proj)

# Clipping countries to the buffer rectangle
countries_clip <- st_intersection(countries, buffer_rect)

# Choosing cities
highlighted_cities <- c("Lyon", "Torino", "Zagreb", "Budapest")

# Filtering chosen cities
cities_highlight <- cities_in_buffer %>%
  filter(name %in% highlighted_cities)

# Creating buffer circles of 50 km radius
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

# Assigning city types
cities_in_buffer$type <- "Other major cities"
cities_highlight$type <- "Selected cities (Aarhus + destinations)"
aarhus_sf <- st_sf(geometry = aarhus_proj, type = "Selected cities (Aarhus + destinations)")

# Combining all cities into one sf object
all_cities <- rbind(
  cities_in_buffer %>% select(geometry, type),
  cities_highlight %>% select(geometry, type),
  aarhus_sf
)

# Final plot
ggplot() +
  # Countries background
  geom_sf(data = countries_clip, fill = "antiquewhite") +
  # Buffer rectangle
  geom_sf(data = buffer_rect, fill = NA, color = "blue", linetype = "dashed") +
  # Highlighted city buffers
  geom_sf(data = highlight_buffers, fill = NA, color = "black", size = 2, linetype = "longdash") +
  # Ploting all cities with colors based on type
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

ggsave("bbox_destinations.png", width = 15, height = 6, dpi = 300)
