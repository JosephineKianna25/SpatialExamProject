
# Install and load libraries
install.packages(c("httr", "jsonlite", "leaflet", "dplyr", "sf", "ggplot2", "osmdata", "osrm", "rnaturalearth", "rnaturalearthdata", "ggrepel", "kableExtra", "tidyr", "knitr", "RANN"))
library(httr)
library(jsonlite)
library(leaflet)
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
library(osmdata)
library(osrm)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(knitr)
library(kableExtra)
library(RANN)
library(htmlwidgets)

options(timeout = 600)  # Increase HTTP timeout (seconds)
set_overpass_url("https://overpass-api.de/api/interpreter")

# API key for OpenChargeMap
api_key <- "f512fd42-eb0d-40df-9b41-41bc8a1c27d5"

# Fetch reference data for connection types
ref_response <- GET(
  "https://api.openchargemap.io/v3/referencedata",
  query = list(key = api_key)
)
stop_for_status(ref_response)
ref_data <- fromJSON(rawToChar(ref_response$content))

# Extracting connection types and filtering for Tesla and CCS
connection_types <- ref_data$ConnectionTypes
target_types <- connection_types %>%
  filter(grepl("tesla|ccs", Title, ignore.case = TRUE)) %>%
  select(ID, Title)

# Extracting target IDs for filtering
target_ids <- target_types$ID

# Defining a helper function for NULL-safe operations
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Function to get chargers near a given location
get_chargers_near <- function(lon, lat, distance_km = 5, api_key = NULL, target_ids = NULL) {
  response <- GET(
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
  
  if (response$status_code != 200) {
    warning("Failed request: status ", response$status_code)
    return(NULL)
  }
  
  chargers_data <- fromJSON(rawToChar(response$content))
  
  # Converting to dataframe if necessary
  if (!is.data.frame(chargers_data)) chargers_data <- as.data.frame(chargers_data)
  
  chargers_df <- bind_rows(lapply(seq_len(nrow(chargers_data)), function(i) {
    connections <- chargers_data$Connections[[i]]
    
    suitable <- FALSE
    fast <- FALSE
    
    if (!is.null(connections) && nrow(connections) > 0) {
      suitable <- any(connections$ConnectionTypeID %in% target_ids)
      fast <- any(connections$LevelID == 3)
    }
    
    data.frame(
      name = chargers_data$AddressInfo$Title[i] %||% NA_character_,
      lon = chargers_data$AddressInfo$Longitude[i] %||% NA_real_,
      lat = chargers_data$AddressInfo$Latitude[i] %||% NA_real_,
      fast = as.integer(fast),
      suitable = as.integer(suitable),
      stringsAsFactors = FALSE
    )
  }))
  
  # Filtering out rows with NA coordinates
  if ("suitable" %in% colnames(chargers_df)) {
    chargers_df <- chargers_df %>% filter(suitable == 1)
  }
  
  return(chargers_df)
}

# Defining destinations
destinations <- data.frame(
  id = c("Lyon", "Torino", "Zagreb", "Budapest"),
  lon = c(4.847, 7.743, 15.967, 19.04),
  lat = c(45.748, 45.116, 45.815, 47.498)
)
dest_sf <- st_as_sf(destinations, coords = c("lon", "lat"), crs = 4326)

# Defining the start point (Aarhus)
start_point <- data.frame(lon = 10.211, lat = 56.157)
start_sf <- st_as_sf(start_point, coords = c("lon", "lat"), crs = 4326)

# Query routes from Aarhus to each destination
routes_list <- lapply(1:nrow(dest_sf), function(i) {
  osrmRoute(
    src = st_coordinates(start_sf),
    dst = st_coordinates(dest_sf[i,]),
    overview = "full",
    returnclass = "sf"
  )
})

# Assigning names to the routes list based on destination IDs
names(routes_list) <- destinations$id

# Combining all routes into one sf object with destination names
routes_all <- do.call(rbind, lapply(names(routes_list), function(name) {
  routes_list[[name]]$destination <- name
  routes_list[[name]]
}))

# Creating lists to store buffers, chargers, and POIs
buffers_list <- list()
chargers_list <- list()
poi_list <- list()

# Looping through each route to create buffers, grid points, and query chargers and POIs
for (i in seq_along(routes_list)) {
  route <- routes_list[[i]]
  dest_id <- names(routes_list)[i]
  
  route_proj <- st_transform(route, 32632) # Using UTM zone for Europe
  buffer_proj <- st_buffer(route_proj, dist = 5000) # 5 km buffer in meters
  buffer <- st_transform(buffer_proj, 4326)
  buffers_list[[dest_id]] <- buffer
  
  # --- Use projected CRS for grid, spacing in meters ---
  bbox_proj <- st_bbox(buffer_proj)
  x_seq <- seq(bbox_proj$xmin, bbox_proj$xmax, by = 20000)
  y_seq <- seq(bbox_proj$ymin, bbox_proj$ymax, by = 20000)
  grid_points_proj <- expand.grid(x = x_seq, y = y_seq) %>%
    st_as_sf(coords = c("x", "y"), crs = st_crs(buffer_proj))
  grid_points_proj <- grid_points_proj[st_within(grid_points_proj, buffer_proj, sparse = FALSE), ]
  grid_points <- st_transform(grid_points_proj, 4326) # back to WGS84 for API
  
  # Storing grid points
  chargers_all <- list()
  for (j in 1:nrow(grid_points)) {
    coords <- st_coordinates(grid_points[j, ])
    chargers_df <- get_chargers_near(coords[1], coords[2], distance_km = 5, api_key = api_key, target_ids = target_ids)
    
    if (!is.null(chargers_df)) {
      chargers_all[[length(chargers_all) + 1]] <- chargers_df
    }
    Sys.sleep(3)  # Pausing between requests
  }
  
  # Combining all chargers into one data frame
  if (length(chargers_all) > 0) {
    chargers_df_all <- bind_rows(chargers_all) %>%
      distinct(lon, lat, .keep_all = TRUE)
    
    # Keeping only relevant columns
    if ("suitable" %in% colnames(chargers_df_all)) {
      # Replacing NAs with 0 (or FALSE)
      chargers_df_all$suitable[is.na(chargers_df_all$suitable)] <- 0
      chargers_df_all <- chargers_df_all[chargers_df_all$suitable == 1, , drop = FALSE]
    }
    
  } else {
    chargers_df_all <- NULL
  }
  
  chargers_list[[dest_id]] <- chargers_df_all
  
  # Querying POIs within a small bounding box around each grid point
  pois_all <- list()
  for (j in 1:nrow(grid_points)) {
    coords <- st_coordinates(grid_points[j, ])
    lon <- coords[1]
    lat <- coords[2]
    small_bbox <- c(lon - 0.05, lat - 0.05, lon + 0.05, lat + 0.05)
    
    q <- tryCatch({
      opq(bbox = small_bbox, timeout = 600) %>%
        add_osm_features(features = list(
          "amenity" = c("cafe", "restaurant", "fast_food", "toilets"),
          "tourism" = c("attraction", "hotel", "hostel", "guest_house", "apartment")
        )) %>%
        osmdata_sf()
    }, error = function(e) {
      message(sprintf("Overpass query failed at lon %.4f, lat %.4f: %s", lon, lat, e$message))
      NULL
    })
    
    if (!is.null(q$osm_points) && nrow(q$osm_points) > 0) {
      pois_all[[length(pois_all) + 1]] <- q$osm_points
    }
    
    Sys.sleep(3)  # Pausing between requests
  }
  
  # Combining all POIs into one data frame
  if (length(pois_all) > 0) {
    # Converting list of sf objects to data frames
    pois_all_df <- lapply(pois_all, function(x) as.data.frame(x))
    
    # Combining them safely
    pois_df_all <- dplyr::bind_rows(pois_all_df)
    
    # Keeping only the desired columns
    cols_exist <- c("tourism", "amenity") %in% colnames(pois_df_all)
    if (any(cols_exist)) {
      pois_df_all <- pois_df_all %>%
        dplyr::filter(
          (if ("tourism" %in% colnames(.)) tourism %in% c("hotel", "hostel", "guest_house", "apartment") else FALSE) |
            (if ("amenity" %in% colnames(.)) amenity %in% c("cafe", "restaurant", "fast_food", "toilets") else FALSE)
        ) %>%
        dplyr::select(osm_id, any_of(c("tourism", "amenity")), geometry)
    } else {
      pois_df_all <- pois_df_all %>% dplyr::select(osm_id, geometry)
    }
    
    poi_list[[dest_id]] <- pois_df_all
  } else {
    poi_list[[dest_id]] <- NULL
  }
  
  message("Finished processing for ", dest_id)
}

# Calculating EDSI metrics
edsi_df <- data.frame(
  destination = character(),
  charger_density = numeric(),
  fast_charger_pct = numeric(),
  poi_density = numeric(),
  avg_interstation = numeric(),
  stringsAsFactors = FALSE
)

# Looping through each destination to calculate metrics
for (dest_id in names(chargers_list)) {
  chargers <- chargers_list[[dest_id]]
  pois <- poi_list[[dest_id]]
  route <- routes_list[[dest_id]]
  
  # Charger density (per 100 km of route)
  route_length <- sum(st_length(route)) / 1000  # in km
  if (!is.null(chargers) && as.numeric(route_length) > 0) {
    charger_density <- nrow(chargers) / (as.numeric(route_length) / 100)
  } else {
    charger_density <- 0
  }
  
  # Fast charger percentage
  fast_pct <- if (!is.null(chargers) && nrow(chargers) > 0) mean(chargers$fast, na.rm = TRUE) else 0
  
  # Converting chargers to sf if not already
  chargers_sf <- if (!is.null(chargers) && nrow(chargers) > 0)
    st_as_sf(chargers, coords = c("lon", "lat"), crs = 4326)
  else NULL
  
  # POI density (per 100 km of route)
  if (!inherits(pois, "sf") && !is.null(pois)) {
    if ("geometry" %in% names(pois)) {
      pois <- st_as_sf(pois)
    } else {
      pois <- NULL
    }
  }
  
  # POI density (per 100 km of route)
  if (!is.null(pois) && nrow(pois) > 0 && !is.null(chargers_sf) && nrow(chargers_sf) > 0) {
    # Ensuring both layers have the same CRS
    if (st_crs(pois) != st_crs(chargers_sf)) {
      pois <- st_transform(pois, st_crs(chargers_sf))
    }
    pois_near <- st_join(chargers_sf, pois, join = st_is_within_distance, dist = 2000)
    poi_density <- nrow(na.omit(pois_near)) / nrow(chargers)
  } else {
    poi_density <- 0
  }
  
  # Average interstation distance (in km)
  if (!is.null(chargers_sf) && nrow(chargers_sf) > 1) {
    dist_matrix <- st_distance(chargers_sf)
    avg_interstation <- mean(dist_matrix[lower.tri(dist_matrix)], na.rm = TRUE) / 1000  # km
  } else {
    avg_interstation <- NA
  }
  
  # Appending the metrics to the data frame
  edsi_df <- rbind(edsi_df, data.frame(
    destination = dest_id,
    charger_density = charger_density,
    fast_charger_pct = fast_pct,
    poi_density = poi_density,
    avg_interstation = as.numeric(avg_interstation)
  ))
}

# Combining all buffers into one sf object
all_chargers <- do.call(rbind, lapply(chargers_list, function(df) {
  if (!is.null(df)) {
    # Only keep rows with valid lon/lat
    df <- df %>% filter(!is.na(lon) & !is.na(lat))
    # Create geometry from lon/lat
    st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  } else {
    NULL
  }
}))

# Combining all buffers into one sf object
all_pois <- do.call(rbind, lapply(poi_list, function(df) {
  if (!is.null(df)) {
    # Only keeping rows with valid geometry
    st_as_sf(df, crs = 4326)
  } else {
    NULL
  }
}))

# --- RANN filtering: Only POIs within 5km (5000m) of any charger ---
utm_crs <- 32632 # UTM zone for central Europe, adjust if you work elsewhere

if (!is.null(all_chargers) && nrow(all_chargers) > 0 && !is.null(all_pois) && nrow(all_pois) > 0) {
  all_chargers_utm <- st_transform(all_chargers, crs = utm_crs)
  all_pois_utm <- st_transform(all_pois, crs = utm_crs)
  chargers_coords <- st_coordinates(all_chargers_utm)
  pois_coords <- st_coordinates(all_pois_utm)
  nn <- RANN::nn2(data = chargers_coords, query = pois_coords, k = 1)
  nearest_dist <- nn$nn.dists[, 1]
  pois_within_5km_utm <- all_pois_utm[nearest_dist <= 5000, ]
  pois_within_5km <- st_transform(pois_within_5km_utm, 4326)
} else {
  pois_within_5km <- all_pois[0, ] # empty
}

# --- After creating pois_within_5km (and before plotting/maps) ---

food_amenities <- pois_within_5km %>%
  filter(!is.na(amenity) & amenity %in% c("cafe", "restaurant", "fast_food"))

toilet_amenities <- pois_within_5km %>%
  filter(!is.na(amenity) & amenity == "toilets")

tourism_pois <- pois_within_5km %>%
  filter(!is.na(tourism))

# Visualizing the routes, buffers, grid points, chargers, and filtered POIs (within 5km of any charger)
# Adding topographic layer and grid points, with filtered POIs (within 5km of any charger)
interactive_map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron, group = "Positron") %>%
  addProviderTiles(providers$OpenTopoMap, group = "Topography", options = providerTileOptions(opacity = 0.5)) %>%  # Topographic/hillshade
  addPolylines(data = do.call(rbind, routes_list), color = "blue", weight = 3, opacity = 0.8, group = "Routes") %>%
  addPolygons(data = do.call(rbind, buffers_list), fillColor = "lightblue", fillOpacity = 0.3, color = NA, group = "Buffer") %>%
  addCircleMarkers(data = grid_points, color = "pink", radius = 3, group = "Grid Points") %>%
  addCircleMarkers(data = all_chargers, color = "purple", radius = 5, group = "EV Chargers") %>%
  addCircleMarkers(data = food_amenities, color = "orange", radius = 4, group = "Food Spots", label = ~amenity) %>% 
  addCircleMarkers(data = toilet_amenities, color = "green", radius = 4, group = "Toilets", label = ~amenity) %>%
  addCircleMarkers(data = tourism_pois, color = "red", radius = 4, group = "Overnight Stays", label = ~tourism) %>%
  addLayersControl(
    baseGroups = c("Positron", "Topography"),
    overlayGroups = c("Routes", "Buffer", "Grid Points", "EV Chargers", "Food Spots", "Toilets", "Overnight Stays"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(position = "bottomright",
            colors = c("purple", "orange", "green", "red"),
            labels = c("EV Chargers", "Food Spots", "Toilets", "Overnight stays"),
            title = "POI Types",
            opacity = 1) %>%
  addScaleBar(position = "bottomleft")

# Save to HTML
saveWidget(interactive_map, file = "chargers&pois.html", selfcontained = TRUE)

# Subplots
# Converting POI data to sf objects with type labels
ev_chargers_sf <- all_chargers %>%
  mutate(type = "EV Charger")

food_spots_sf <- food_amenities %>%
  mutate(type = "Food Spot")

toilets_sf <- toilet_amenities %>%
  mutate(type = "Toilet")

overnight_sf <- tourism_pois %>%
  mutate(type = "Overnight Stay")

# Combining them all
all_poi_long <- bind_rows(ev_chargers_sf, food_spots_sf, toilets_sf, overnight_sf)

# Start point
start_sf$city <- "Aarhus"

# Destination points already have IDs
dest_sf$city <- dest_sf$id

# Combining start and destinations
city_points <- bind_rows(start_sf, dest_sf)

# Preparing the combined geometries for bounding box
all_geom <- c(
  st_geometry(all_poi_long),
  st_geometry(do.call(rbind, routes_list)),
  st_geometry(city_points)
)
bbox <- st_bbox(do.call(c, all_geom))
pad <- 1
xlim <- c(bbox["xmin"] - pad, bbox["xmax"] + pad)
ylim <- c(bbox["ymin"] - pad, bbox["ymax"] + pad)

# Color mapping for POI types
poi_colors <- c(
  "EV Charger" = "purple",
  "Food Spot" = "orange",
  "Toilet" = "green",
  "Overnight Stay" = "red"
)

# The following two plots may trigger a warning about using st_point_on_surface on lon/lat data. This is safe to ignore, as we are labeling points and not polygons on the map.

# Plotting the routes and POIs with facets
ggplot() +
  geom_sf(data = do.call(rbind, routes_list), color = "blue", size = 1, alpha = 0.7) +
  geom_sf(data = all_poi_long, aes(color = type), size = 1.5, alpha = 0.85, show.legend = FALSE) +
  geom_sf(data = city_points, color = "black", size = 2, shape = 21, fill = "yellow") +
  ggrepel::geom_text_repel(
    data = city_points,
    aes(geometry = geometry, label = city),
    stat = "sf_coordinates",
    size = 3.5,
    fontface = "bold"
  ) +
  facet_wrap(~type, ncol = 2) +  # facet by POI type
  scale_color_manual(values = poi_colors, drop = FALSE) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(
    title = "Density and Distribution of POI Types Along EV Travel Routes",
    subtitle = "Each facet shows one POI category",
    caption = "Data: OpenStreetMap, OpenChargeMap"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    strip.text = element_text(face = "bold", size = 13),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Saving the plot as png
ggsave("poi_distribution_faceted.png", width = 15, height = 8, dpi = 300)

# Plotting the routes and POIs with hexbin for density visualization
ggplot() +
  geom_sf(data = do.call(rbind, routes_list), color = "blue", size = 1, alpha = 0.7) +
  geom_hex(
    data = all_poi_long,
    aes(x = st_coordinates(geometry)[,1], y = st_coordinates(geometry)[,2]),
    bins = 40,
    alpha = 0.8
  ) +
  scale_fill_viridis_c(
     option = "C",
    direction = -1,
    name = "POI Count",
    limits = c(0, 50),  
    oob = scales::squish # handling outliers
  ) +
  geom_sf(data = city_points, color = "black", size = 3, shape = 21, fill = "yellow") +
  ggrepel::geom_text_repel(
    data = city_points,
    aes(geometry = geometry, label = city),
    stat = "sf_coordinates",
    size = 5,
    fontface = "bold"
  ) +
  facet_wrap(~type, ncol = 2) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  labs(
    title = "Density and Distribution of POI Types Along EV Travel Routes",
    subtitle = "Each facet shows one POI category",
    caption = "Data: OpenStreetMap, OpenChargeMap",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    strip.text = element_text(face = "bold", size = 16),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Saving the plot as png
ggsave("poi_distribution_hexbin.png", width = 15, height = 8, dpi = 300)

# Linear min-max normalization
normalize <- function(x, epsilon = 0.01) {
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (rng == 0) rep(0.5, length(x))
  else {
    norm <- (x - min(x, na.rm = TRUE)) / rng
    norm * (1 - 2 * epsilon) + epsilon
  }
}

# Quantile normalization function
quantile_norm <- function(x) ecdf(x)(x)

# Normalizing the EDSI metrics
edsi_df$charger_density_norm <- quantile_norm(edsi_df$charger_density)
edsi_df$fast_charger_pct_norm <- quantile_norm(edsi_df$fast_charger_pct)
edsi_df$poi_density_norm <- quantile_norm(edsi_df$poi_density)
edsi_df$avg_interstation_norm <- quantile_norm(edsi_df$avg_interstation)
edsi_df$avg_interstation_norm <- 1 - edsi_df$avg_interstation_norm

# Infrastructure: mean of charger density, fast charger percentage, and average interstation distance
edsi_df$Infrastructure <- rowMeans(data.frame(
  edsi_df$charger_density_norm,
  edsi_df$fast_charger_pct_norm,
  edsi_df$avg_interstation_norm
), na.rm = TRUE)

# Comfort: poi density normalized
edsi_df$Comfort <- edsi_df$poi_density_norm

# Calculating the EDSI as a weighted average of Infrastructure and Comfort
edsi_df$EDSI <- rowMeans(data.frame(
  edsi_df$Infrastructure,
  edsi_df$Comfort
), na.rm = TRUE)

# Alternatively, using a weighted average
edsi_df$EDSI <- 0.6 * edsi_df$Infrastructure + 0.4 * edsi_df$Comfort

# Viewing the results
print(edsi_df)

# Displaying the EDSI metrics in a table
edsi_table <- kable(edsi_df, 
      caption = "Table: EDSI Metrics for Selected Destinations", 
      digits = 3, 
      format = "html") %>%
  kable_styling(full_width = FALSE, 
                bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = TRUE, background = "#f7f7f7") %>%
  column_spec(1, bold = TRUE)  # make 'destination' stand out

# Save to HTML file
save_kable(edsi_table, "edsi_metrics_table.html")

# Reshaping the EDSI data for plotting
edsi_long <- pivot_longer(edsi_df, cols = c(EDSI, Infrastructure, Comfort), names_to = "Score", values_to = "Value")

# Plotting the EDSI scores and subscores
ggplot(edsi_long %>% filter(Score != "EDSI"), aes(x = destination, y = Value, fill = Score)) +
  geom_col(position = "dodge", color = "gray30", width = 0.7) +
  geom_text(aes(label = round(Value, 2)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Subscores: Comfort & Infrastructure", y = "Score (0-1 scale)", x = "Destination") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20)
  )

# Saving the plot as png
ggsave("edsi_subscores.png", width = 15, height = 8, dpi = 300)

# Plotting the overall EDSI scores
ggplot(edsi_df, aes(x = destination, y = EDSI, fill = destination)) +
  geom_col(width = 0.7, color = "gray30") +
  geom_text(aes(label = round(EDSI, 2)), vjust = -0.5, size = 5, color = "black") +
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "EV-Driving-Suitability-Index (EDSI)",
    subtitle = "Overall Suitability by Destination",
    y = "EDSI (0-1 Scale)",
    x = "Destination",
    fill = "Destination"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_light(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 13, margin = margin(b = 10)),
    axis.text.x = element_text(angle = 15, hjust = 1, size = 12),
    legend.position = "none"
  )

# Saving the plot as png
ggsave("edsi_overall.png", width = 15, height = 8, dpi = 300)

# Function to get stops every 480 km along the route
get_stops_every_480km <- function(route_sf, dist_km = 490) {
  # Checking if the route is valid and finding the UTM zone
  centroid <- st_centroid(route_sf)
  lon <- st_coordinates(centroid)[1]
  utm_zone <- floor((lon + 180) / 6) + 1
  utm_crs <- paste0("EPSG:", 32600 + utm_zone) # UTM zone for northern hemisphere
  
  # Transforming the route to UTM CRS
  route_proj <- st_transform(route_sf, crs = utm_crs)
  
  # Calculating the length of the route in meters
  route_length_m <- as.numeric(st_length(route_proj))
  
  # Number of stops
  n_stops <- floor(route_length_m / (dist_km * 1000))
  if(n_stops == 0) return(NULL)  # Route shorter than 480 km
  
  # Generating distances for stops
  stop_dists_m <- seq(from = dist_km * 1000, to = n_stops * dist_km * 1000, by = dist_km * 1000)
  
  # Sampling points along the route at specified distances
  stop_points_proj <- st_line_sample(route_proj, sample = stop_dists_m / route_length_m)
  
  # Converting sampled points to sf object
  stops_proj_sf <- st_sf(
    geometry = st_cast(stop_points_proj, "POINT"),
    dist_km = stop_dists_m / 1000
  )
  
  # Transforming back to WGS84
  stops_wgs84 <- st_transform(stops_proj_sf, crs = 4326)
  
  return(stops_wgs84)
}

# Getting stops every 490 km for each route
stops_list <- lapply(routes_list, get_stops_every_480km, dist_km = 490)

# Combining all stops into one sf object with destination names
stops_all <- do.call(rbind, lapply(names(stops_list), function(dest) {
  sf_obj <- stops_list[[dest]]
  sf_obj$destination <- dest
  sf_obj
}))

# Visualizing the routes, chargers, stops, and topography
charger_stops <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addProviderTiles(providers$OpenTopoMap, group = "Topography", options = providerTileOptions(opacity = 0.5)) %>%  # Topographic/hillshade
  addPolylines(data = do.call(rbind, routes_list), color = "blue", weight = 3, opacity = 0.8, group = "Routes") %>%
  addCircleMarkers(data = all_chargers, color = "purple", radius = 4, group = "EV Chargers") %>%
  addCircleMarkers(data = stops_all, 
                   color = "gold", radius = 6, 
                   label = ~paste0(destination, ": ", round(dist_km, 1), " km"),
                   group = "Stops every 490 km") %>%
  addLayersControl(
    baseGroups = c("Positron", "Topography"),
    overlayGroups = c("Routes", "EV Chargers", "Stops every 490 km"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(position = "bottomright",
            colors = c("purple", "gold"),
            labels = c("EV Chargers", "Stops every 490 km"),
            title = "Range with necessary charging breaks",
            opacity = 1) %>%
  addScaleBar(position = "bottomleft")

# Saving the stops map as html
saveWidget(charger_stops, file = "stops_per_490km.html", selfcontained = TRUE)
