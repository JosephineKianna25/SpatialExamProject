
# Install and load libraries
install.packages(c("httr", "jsonlite", "leaflet", "dplyr", "sf", "ggplot2", "osmdata", "osrm", "rnaturalearth", "rnaturalearthdata"))
library(httr)
library(jsonlite)
library(leaflet)
library(dplyr)
library(sf)
library(ggplot2)
library(osmdata)
library(osrm)
library(rnaturalearth)
library(rnaturalearthdata)

# API key for OpenChargeMap
api_key <- "f512fd42-eb0d-40df-9b41-41bc8a1c27d5"

# Fetch reference data once (you already have this)
ref_response <- GET(
  "https://api.openchargemap.io/v3/referencedata",
  query = list(key = api_key)
)
stop_for_status(ref_response)
ref_data <- fromJSON(rawToChar(ref_response$content))

connection_types <- ref_data$ConnectionTypes
target_types <- connection_types %>%
  filter(grepl("tesla|ccs", Title, ignore.case = TRUE)) %>%
  select(ID, Title)

target_ids <- target_types$ID

# Helper function for charger query
`%||%` <- function(a, b) if (!is.null(a)) a else b

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
  
  # Convert to dataframe if necessary
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
  
  # --- Fix: Check for 'suitable' column existence before filtering ---
  if ("suitable" %in% colnames(chargers_df)) {
    chargers_df <- chargers_df %>% filter(suitable == 1)
  }
  
  return(chargers_df)
}

# Load or define your destinations (as sf POINTs) and routes 
destinations <- data.frame(
  id = c("Lyon", "Torino", "Zagreb", "Budapest"),
  lon = c(4.847, 7.743, 15.967, 19.04),
  lat = c(45.748, 45.116, 45.815, 47.498)
)
dest_sf <- st_as_sf(destinations, coords = c("lon", "lat"), crs = 4326)

# Start point: Aarhus
start_point <- data.frame(lon = 10.211, lat = 56.157)
start_sf <- st_as_sf(start_point, coords = c("lon", "lat"), crs = 4326)

# Compute fastest routes using osrmRoute
routes_list <- lapply(1:nrow(dest_sf), function(i) {
  osrmRoute(
    src = st_coordinates(start_sf),
    dst = st_coordinates(dest_sf[i,]),
    overview = "full",
    returnclass = "sf"
  )
})
names(routes_list) <- destinations$id
routes_all <- do.call(rbind, lapply(names(routes_list), function(name) {
  routes_list[[name]]$destination <- name
  routes_list[[name]]
}))

# Query chargers along the routes
buffers_list <- list()
chargers_list <- list()
poi_list <- list()

for (i in seq_along(routes_list)) {
  route <- routes_list[[i]]
  dest_id <- names(routes_list)[i]
  
  # Buffer around route
  buffer <- st_buffer(route, dist = 0.1)
  buffers_list[[dest_id]] <- buffer
  
  # Grid points within buffer
  bbox <- st_bbox(buffer)
  x_seq <- seq(bbox$xmin, bbox$xmax, by = 0.1)
  y_seq <- seq(bbox$ymin, bbox$ymax, by = 0.1)
  grid_points <- expand.grid(x = x_seq, y = y_seq) %>%
    st_as_sf(coords = c("x", "y"), crs = 4326)
  grid_points <- grid_points[st_within(grid_points, buffer, sparse = FALSE), ]
  
  # --- Query chargers ---
  chargers_all <- list()
  for (j in 1:nrow(grid_points)) {
    coords <- st_coordinates(grid_points[j, ])
    chargers_df <- get_chargers_near(coords[1], coords[2], distance_km = 5, api_key = api_key, target_ids = target_ids)
    
    if (!is.null(chargers_df)) {
      chargers_all[[length(chargers_all) + 1]] <- chargers_df
    }
    Sys.sleep(3)  # Pause between requests
  }
  
  # Combine and deduplicate chargers
  if (length(chargers_all) > 0) {
    chargers_df_all <- bind_rows(chargers_all) %>%
      distinct(lon, lat, .keep_all = TRUE)
    
    # --- Safe check: does 'suitable' column exist? ---
    if ("suitable" %in% colnames(chargers_df_all)) {
      # Replace NAs with 0 (or FALSE)
      chargers_df_all$suitable[is.na(chargers_df_all$suitable)] <- 0
      chargers_df_all <- chargers_df_all[chargers_df_all$suitable == 1, , drop = FALSE]
    }
    
  } else {
    chargers_df_all <- NULL
  }
  
  chargers_list[[dest_id]] <- chargers_df_all
  
  # --- Query hospitality POIs ---
  pois_all <- list()
  for (j in 1:nrow(grid_points)) {
    coords <- st_coordinates(grid_points[j, ])
    lon <- coords[1]
    lat <- coords[2]
    small_bbox <- c(lon - 0.05, lat - 0.05, lon + 0.05, lat + 0.05)
    
    q <- opq(bbox = small_bbox, timeout = 300) %>%
      add_osm_features(features = list(
        "amenity" = c("cafe", "restaurant", "fast_food", "toilets"),
        "tourism" = c("attraction", "hotel", "hostel", "guest_house", "apartment")
      )) %>%
      osmdata_sf()
    
    if (!is.null(q$osm_points) && nrow(q$osm_points) > 0) {
      pois_all[[length(pois_all) + 1]] <- q$osm_points
    }
    
    Sys.sleep(3)
  }
  
  # Combine POI results (if any)
  if (length(pois_all) > 0) {
    # Convert to data frames
    pois_all_df <- lapply(pois_all, function(x) as.data.frame(x))
    
    # Combine safely
    pois_df_all <- dplyr::bind_rows(pois_all_df)
    
    # Keep only the desired columns
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

# ---- EDSI RAW VARIABLE CALCULATION LOOP ----
edsi_df <- data.frame(
  destination = character(),
  charger_density = numeric(),
  fast_charger_pct = numeric(),
  poi_density = numeric(),
  avg_interstation = numeric(),
  stringsAsFactors = FALSE
)

for (dest_id in names(chargers_list)) {
  chargers <- chargers_list[[dest_id]]
  pois <- poi_list[[dest_id]]
  route <- routes_list[[dest_id]]
  
  # Charger density per 100km
  route_length <- sum(st_length(route)) / 1000  # in km
  if (!is.null(chargers) && as.numeric(route_length) > 0) {
    charger_density <- nrow(chargers) / (as.numeric(route_length) / 100)
  } else {
    charger_density <- 0
  }
  
  # % fast chargers
  fast_pct <- if (!is.null(chargers) && nrow(chargers) > 0) mean(chargers$fast, na.rm = TRUE) else 0
  
  # Convert chargers to sf
  chargers_sf <- if (!is.null(chargers) && nrow(chargers) > 0)
    st_as_sf(chargers, coords = c("lon", "lat"), crs = 4326)
  else NULL
  
  # Convert pois to sf if not already
  if (!inherits(pois, "sf") && !is.null(pois)) {
    if ("geometry" %in% names(pois)) {
      pois <- st_as_sf(pois)
    } else {
      pois <- NULL
    }
  }
  
  # POI density near chargers (within 1 km)
  if (!is.null(pois) && nrow(pois) > 0 && !is.null(chargers_sf) && nrow(chargers_sf) > 0) {
    pois_near <- st_join(chargers_sf, pois, join = st_is_within_distance, dist = 1000)
    poi_density <- nrow(na.omit(pois_near)) / nrow(chargers)
  } else {
    poi_density <- 0
  }
  
  # Average inter-station distance
  if (!is.null(chargers_sf) && nrow(chargers_sf) > 1) {
    dist_matrix <- st_distance(chargers_sf)
    avg_interstation <- mean(dist_matrix[lower.tri(dist_matrix)], na.rm = TRUE) / 1000  # km
  } else {
    avg_interstation <- NA
  }
  
  edsi_df <- rbind(edsi_df, data.frame(
    destination = dest_id,
    charger_density = charger_density,
    fast_charger_pct = fast_pct,
    poi_density = poi_density,
    avg_interstation = as.numeric(avg_interstation)
  ))
}

# --- Visualize buffer, grid points, chargers, and hospitality POIs ---
# Combine all chargers into one sf object
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

# Combine all POIs into one sf object
all_pois <- do.call(rbind, lapply(poi_list, function(df) {
  if (!is.null(df)) {
    # POIs from OSM already have geometry
    st_as_sf(df, crs = 4326)
  } else {
    NULL
  }
}))

# "Food spots" = amenities (cafe, restaurant, fast_food)
food_amenities <- all_pois %>%
  filter(!is.na(amenity) & amenity %in% c("cafe", "restaurant", "fast_food"))

# "Toilets" = amenities (toilets)
toilet_amenities <- all_pois %>%
  filter(!is.na(amenity) & amenity == "toilets")

# "Overnight stays" = tourism (hotel, hostel, apartment, guest house)
tourism_pois <- all_pois %>%
  filter(!is.na(tourism))
  
  
# Plot
ggplot() +
  geom_sf(data = do.call(rbind, buffers_list), fill = "lightblue", alpha = 0.3, color = NA) +
  geom_sf(data = do.call(rbind, routes_list), color = "blue", size = 1, alpha = 0.7) +
  geom_sf(data = grid_points, color = "pink", size = 1) +     # Grid sampling points with 5 km buffer
  geom_sf(data = all_chargers, color = "orange", size = 1, alpha = 0.9) +
  geom_sf(data = all_pois, color = "purple", size = 1, alpha = 0.9) +
  labs(
    title = "Visualization of Buffers, Routes, Chargers, and Hospitality POIs",
    subtitle = "Data coverage around each route buffer",
    caption = "Data: OpenStreetMap, OpenChargeMap, project queries"
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

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolylines(data = do.call(rbind, routes_list), color = "blue", weight = 3, opacity = 0.8, group = "Routes") %>%
  addPolygons(data = do.call(rbind, buffers_list), fillColor = "lightblue", fillOpacity = 0.3, color = NA, group = "Buffer") %>%
  addCircleMarkers(data = grid_points, color = "pink", radius = 3, group = "Grid Points") %>%
  addCircleMarkers(data = all_chargers, color = "orange", radius = 5, group = "EV Chargers") %>%
  addCircleMarkers(data = food_amenities, color = "purple", radius = 4, group = "Food Spots", label = ~amenity) %>% 
  addCircleMarkers(data = toilet_amenities, color = "green", radius = 4, group = "Toilets", label = ~amenity) %>%
  addCircleMarkers(data = tourism_pois, color = "red", radius = 4, group = "Overnight Stays", label = ~tourism) %>%
  addLayersControl(
    overlayGroups = c("Routes", "Buffer", "Grid Points", "EV Chargers", "Food Spots", "Toilets", "Overnight Stays"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(position = "bottomright",
            colors = c("purple", "green", "red"),
            labels = c("Food Spots",
                       "Toilets",
                       "Overnight stays"),
            title = "POI Types",
            opacity = 1) %>%
  addScaleBar(position = "bottomleft")
  
# --- Normalization Functions ---
# Linear min-max, quantile, and log-minmax provided for flexibility

normalize <- function(x, epsilon = 0.01) {
  rng <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  if (rng == 0) rep(0.5, length(x))
  else {
    norm <- (x - min(x, na.rm = TRUE)) / rng
    norm * (1 - 2 * epsilon) + epsilon
  }
}

quantile_norm <- function(x) ecdf(x)(x)

log_minmax <- function(x) {
  x_log <- log1p(x)
  rng <- range(x_log, na.rm = TRUE)
  (x_log - rng[1]) / diff(rng)
}

# --- Normalize Each Component (choose best per variable) ---
edsi_df$charger_density_norm <- log_minmax(edsi_df$charger_density)
edsi_df$fast_charger_pct_norm <- normalize(edsi_df$fast_charger_pct)
edsi_df$poi_density_norm <- quantile_norm(edsi_df$poi_density)
edsi_df$avg_interstation_norm <- normalize(edsi_df$avg_interstation)

# --- Calculate Subscores ---
# Infrastructure: charger density, fast charger %, avg interstation distance (inverse)
edsi_df$Infrastructure <- rowMeans(data.frame(
  edsi_df$charger_density_norm,
  edsi_df$fast_charger_pct_norm,
  1 - edsi_df$avg_interstation_norm
), na.rm = TRUE)

# Comfort: just POI density (or add more comfort features if available)
edsi_df$Comfort <- edsi_df$poi_density_norm

# Combined EDSI: mean of Infrastructure and Comfort
edsi_df$EDSI <- rowMeans(data.frame(
  edsi_df$Infrastructure,
  edsi_df$Comfort
), na.rm = TRUE)

# View the results
print(edsi_df)

library(tidyr)
edsi_long <- pivot_longer(edsi_df, cols = c(EDSI, Infrastructure, Comfort), names_to = "Score", values_to = "Value")

ggplot(edsi_long, aes(x = destination, y = Value, fill = Score)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = round(Value, 2)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "EV-Driving-Suitability & Subscores", y = "Score (0-1 scale)", x = "Destination") +
  theme_minimal()

ggplot(edsi_df, aes(x = destination, y = EDSI, fill = destination)) +
  geom_col() +
  geom_text(aes(label = round(EDSI, 2)), vjust = -0.5) +
  labs(title = "EV-Driving-Suitability-Index (EDSI)", y = "EDSI (0-1 Scale)", x = "Destination") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal()



