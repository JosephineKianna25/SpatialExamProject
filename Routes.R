
# Install packages
install.packages(c("sf", "osrm", "sfnetworks", "tidygraph", "dplyr", "ggplot2", "rnaturalearth", "rnaturalearthdata"))

# Install libraries
library(osrm)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)

# Define Aarhus as sf POINT
from <- st_sf(
  id = "Aarhus",
  geometry = st_sfc(st_point(c(10.2039, 56.1629))),
  crs = 4326
)

# Define destination cities as sf POINTs
destinations <- st_sf(
  id = c("Lyon", "Torino", "Zagreb", "Budapest"),
  geometry = st_sfc(
    st_point(c(4.8357, 45.7640)),
    st_point(c(7.6869, 45.0703)),
    st_point(c(15.9819, 45.8150)),
    st_point(c(19.0402, 47.4979))
  ),
  crs = 4326
)

routes <- list()

for (i in seq_len(nrow(destinations))) {
  to <- destinations[i, ]
  message("Routing to ", to$id)
  
  # osrmRoute expects src and dst as sf POINTs or lon/lat vectors
  route_sf <- osrmRoute(src = from, dst = to, overview = "full", returnclass = "sf")
  
  route_sf$destination <- to$id
  
  routes[[to$id]] <- route_sf
}

# Combine all routes
routes_all <- do.call(rbind, routes)

# Plot routes and points
points_all <- rbind(from, destinations)

# List of countries in the road network
country_names <- c("Denmark", "France", "Italy", "Croatia", "Hungary", "Austria", "Germany", "Switzerland", "Slovenia", "Czechia")

# Load all countries from Natural Earth
all_countries <- ne_countries(scale = "medium", returnclass = "sf")

# Filter only the relevant countries by name
countries_selected <- all_countries %>%
  filter(admin %in% country_names)

# Combine geometries from routes and points
combined_geom <- st_union(st_geometry(routes_all), st_geometry(points_all))

# Get bounding box
bbox <- st_bbox(combined_geom)

# Calculate buffer (5%)
x_range <- bbox$xmax - bbox$xmin
y_range <- bbox$ymax - bbox$ymin

xlim <- c(bbox$xmin - 0.05 * x_range, bbox$xmax + 0.05 * x_range)
ylim <- c(bbox$ymin - 0.05 * y_range, bbox$ymax + 0.05 * y_range)

# Create a named numeric vector for bbox with buffer
bbox_vec <- c(xmin = xlim[1], ymin = ylim[1], xmax = xlim[2], ymax = ylim[2])

# Create bbox object with CRS
bbox_obj <- structure(
  bbox_vec,
  class = "bbox",
  crs = st_crs(combined_geom)
)

# Convert bbox to sfc polygon
bbox_polygon <- st_as_sfc(bbox_obj)

# Clip countries by intersecting with buffered bbox polygon
countries_clipped <- st_intersection(countries_selected, bbox_polygon)

# Prepare points data for text labels
points_coords <- as.data.frame(st_coordinates(points_all))
points_coords$name <- points_all$id 

ggplot() +
  geom_sf(data = countries_clipped, fill = "lightgreen", color = "black", size = 0.2) +
  geom_sf(data = routes_all, color = "darkblue", size = 0.6) +
  geom_sf(data = points_all, color = "red", size = 2) +
  geom_text(data = points_coords, aes(X, Y, label = name), nudge_y = 0.2, size = 3) +
  theme_minimal() +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(title = "Routes Across Selected European Countries",
       subtitle = "Highlighted roads and desinations",
       caption = "Data: Natural Earth + Custom Route Data") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 8),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
