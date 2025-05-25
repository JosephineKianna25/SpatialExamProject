# Install packages
install.packages(c("httr", "jsonlite", "leaflet", "dplyr", "sf", "ggplot2", "readr", "rnaturalearth", "rnaturalearthdata"))

# Load libraries
library(httr)
library(jsonlite)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(readr)
library(rnaturalearth)
library(rnaturalearthdata)

# List of destinations
destination_ids <- destinations$id

# Initialize lists to store results
buffers_list <- list()
centroids_list <- list()
chargers_list <- list()

# OpenChargeMap API key
api_key <- "f512fd42-eb0d-40df-9b41-41bc8a1c27d5"  # Replace with actual key

# Helper function for charger query
`%||%` <- function(a, b) if (!is.null(a)) a else b

get_chargers_near <- function(lon, lat, distance_km = 5, api_key = NULL) {
  response <- httr::GET(
    url = "https://api.openchargemap.io/v3/poi/",
    query = list(
      output = "json",
      latitude = lat,
      longitude = lon,
      distance = distance_km,
      distanceunit = "KM",
      maxresults = 1000,
      compact = "true",
      verbose = "false",
      key = api_key
    )
  )
  
  if (response$status_code == 200) {
    chargers_data <- jsonlite::fromJSON(rawToChar(response$content))
    
    if (length(chargers_data) == 0) return(NULL)
    if (!is.list(chargers_data[[1]])) chargers_data <- list(chargers_data)
    
    chargers_df <- do.call(rbind, lapply(chargers_data, function(x) {
      data.frame(
        name = x$AddressInfo$Title %||% NA_character_,
        lon = x$AddressInfo$Longitude %||% NA_real_,
        lat = x$AddressInfo$Latitude %||% NA_real_,
        stringsAsFactors = FALSE
      )
    }))
    
    return(chargers_df)
  } else {
    warning("Failed request")
    return(NULL)
  }
}

# Loop over each destination
for (dest_id in destination_ids) {
  message("Processing ", dest_id)
  
  # Select route for this destination
  single_route <- routes_all %>% filter(destination == dest_id)
  
  # Create buffer
  route_buffer <- st_buffer(single_route, dist = 0.1)  # adjust buffer size as needed
  
  # Create grid of points
  bbox <- st_bbox(route_buffer)
  x_seq <- seq(bbox$xmin, bbox$xmax, by = 0.05)
  y_seq <- seq(bbox$ymin, bbox$ymax, by = 0.05)
  
  grid_points <- expand.grid(x = x_seq, y = y_seq) %>%
    st_as_sf(coords = c("x", "y"), crs = st_crs(route_buffer))
  
  buffer_centroids <- grid_points[st_within(grid_points, route_buffer, sparse = FALSE), ]
  
  # Query chargers for each centroid
  chargers_all <- list()
  for (i in seq_len(nrow(buffer_centroids))) {
    coords <- st_coordinates(buffer_centroids[i, ])
    lon <- coords[1]
    lat <- coords[2]
    
    chargers_df <- get_chargers_near(lon, lat, distance_km = 5, api_key = api_key)
    
    if (!is.null(chargers_df)) {
      chargers_df$route_id <- dest_id
      chargers_all[[length(chargers_all) + 1]] <- chargers_df
    }
    
    Sys.sleep(1)  # be kind to the API
  }
  
  # Combine chargers for this route
  if (length(chargers_all) > 0) {
    chargers_df_all <- bind_rows(chargers_all) %>%
      distinct(lon, lat, .keep_all = TRUE)
    
    chargers_list[[dest_id]] <- chargers_df_all
  }
  
  # Store buffers and centroids
  buffers_list[[dest_id]] <- route_buffer
  centroids_list[[dest_id]] <- buffer_centroids
}

# Combine buffers, centroids, chargers
all_buffers <- do.call(rbind, buffers_list)
all_centroids <- do.call(rbind, centroids_list)

# Combine and deduplicate chargers across all routes
all_chargers_df <- bind_rows(chargers_list) %>%
  distinct(lon, lat, .keep_all = TRUE)

# Convert chargers to sf points
chargers_sf <- st_as_sf(all_chargers_df, coords = c("lon", "lat"), crs = 4326)

# Final plot
ggplot() +
  geom_sf(data = countries_clipped, fill = "lightgreen", color = "black", size = 0.2) +
  
  # All routes
  geom_sf(data = routes_all, color = "darkblue", size = 0.6, alpha = 0.4) +
  
  # All start/end points
  geom_sf(data = points_all, color = "red", size = 2) +
  geom_text(data = points_coords, aes(X, Y, label = name), nudge_y = 0.2, size = 3) +
  
  # All buffers
  geom_sf(data = all_buffers, fill = "lightblue", alpha = 0.3, color = NA) +
  
  # All centroids
  geom_sf(data = all_centroids, color = "blue", size = 1, alpha = 0.5) +
  
  # All chargers
  geom_sf(data = chargers_sf, color = "orange", size = 2, alpha = 0.8) +
  
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  
  labs(
    title = "Routes and EV Chargers for Four Destinations",
    subtitle = "All buffers, centroids, and chargers plotted together",
    caption = "Data: Natural Earth + OpenChargeMap"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(size = 8),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Make the map interactive for clearer inspection

# Combine all buffers for leaflet
all_buffers <- do.call(rbind, buffers_list)

# Create the leaflet map
leaflet() %>%
  # Add base map
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  # Countries layer
  addPolygons(
    data = countries_clipped,
    fillColor = "lightgreen",
    fillOpacity = 0.4,
    color = "black",
    weight = 1,
    group = "Countries"
  ) %>%
  
  # Routes layer
  addPolylines(
    data = routes_all,
    color = "darkblue",
    weight = 3,
    opacity = 0.7,
    group = "Routes"
  ) %>%
  
  # Destination points
  addCircleMarkers(
    data = points_all,
    color = "red",
    radius = 5,
    label = ~id,
    group = "Destinations"
  ) %>%
  
  # Buffers
  addPolygons(
    data = all_buffers,
    fillColor = "lightblue",
    fillOpacity = 0.3,
    color = "blue",
    weight = 1,
    group = "Buffers"
  ) %>%
  
  # Charger points
  addCircleMarkers(
    data = chargers_sf,
    color = "orange",
    radius = 4,
    opacity = 0.8,
    label = ~name,
    group = "EV Chargers"
  ) %>%
  
  # Add layer controls
  addLayersControl(
    overlayGroups = c("Countries", "Routes", "Destinations", "Buffers", "EV Chargers"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Add scale bar
  addScaleBar(position = "bottomleft")

