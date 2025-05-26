
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

# Helper function for charger query
`%||%` <- function(a, b) if (!is.null(a)) a else b
get_chargers_near <- function(lon, lat, distance_km = 5, api_key = NULL) {
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
  
  if (response$status_code == 200) {
    chargers_data <- fromJSON(rawToChar(response$content))
    if (length(chargers_data) == 0) return(NULL)
    if (!is.list(chargers_data[[1]])) chargers_data <- list(chargers_data)
    
    chargers_df <- do.call(rbind, lapply(chargers_data, function(x) {
      fast <- if (!is.null(x$Connections) && length(x$Connections) > 0) {
        any(sapply(x$Connections, function(c) {
          level <- c$LevelID
          if (is.list(level)) level <- unlist(level)
          isTRUE(length(level) == 1 && level == 3)
        }))
      } else {
        FALSE
      }
      
      data.frame(
        name = x$AddressInfo$Title %||% NA_character_,
        lon = x$AddressInfo$Longitude %||% NA_real_,
        lat = x$AddressInfo$Latitude %||% NA_real_,
        fast = as.integer(fast),
        stringsAsFactors = FALSE
      )
    }))
    
    return(chargers_df)
  } else {
    warning("Failed request")
    return(NULL)
  }
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
    chargers_df <- get_chargers_near(coords[1], coords[2], distance_km = 5, api_key = api_key)
    
    if (!is.null(chargers_df)) {
      chargers_all[[length(chargers_all) + 1]] <- chargers_df
    }
    Sys.sleep(3)  # Pause between requests
  }
  
  # Combine and deduplicate chargers
  chargers_df_all <- bind_rows(chargers_all) %>%
    distinct(lon, lat, .keep_all = TRUE)
  chargers_list[[dest_id]] <- chargers_df_all
  
  # --- Query hospitality POIs in smaller chunks ---
  pois_all <- list()
  for (j in 1:nrow(grid_points)) {
    coords <- st_coordinates(grid_points[j, ])
    lon <- coords[1]
    lat <- coords[2]
    small_bbox <- c(lon - 0.05, lat - 0.05, lon + 0.05, lat + 0.05)
    
    # Query OSM for restaurants and cafes
    q <- opq(bbox = small_bbox, timeout = 300) %>%
      add_osm_feature(key = "amenity", value = c("restaurant", "cafe")) %>%
      osmdata_sf()
    
    if (!is.null(q$osm_points) && nrow(q$osm_points) > 0) {
      pois_all[[length(pois_all) + 1]] <- q$osm_points
    }
    
    Sys.sleep(3)  # Pause to avoid rate limiting
  }
  
  # Combine POI results (if any)
  if (length(pois_all) > 0) {
    # Convert to data frames
    pois_all_df <- lapply(pois_all, function(x) as.data.frame(x))
    
    # Combine safely
    pois_df_all <- dplyr::bind_rows(pois_all_df)
    
    # Keep only the desired columns
    pois_df_all <- pois_df_all %>%
      dplyr::select(osm_id, name, amenity, geometry)
    
    poi_list[[dest_id]] <- pois_df_all
  } else {
    poi_list[[dest_id]] <- NULL
  }
  
  message("Finished processing for ", dest_id)
}

# --- Visualize buffer, grid points, chargers, and hospitality POIs ---
# Combine all chargers into one sf object
all_chargers <- do.call(rbind, lapply(chargers_list, function(df) {
  st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
}))

# Combine all POIs into one sf object
all_pois <- do.call(rbind, poi_list)
all_pois <- st_as_sf(all_pois, crs = 4326)

# Plot
ggplot() +
  geom_sf(data = do.call(rbind, buffers_list), fill = "lightblue", alpha = 0.3, color = NA) +
  geom_sf(data = do.call(rbind, routes_list), color = "blue", size = 1, alpha = 0.7) +
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
  addPolygons(data = do.call(rbind, buffers_list), fillColor = "lightblue", fillOpacity = 0.3, color = NA, group = "Buffers") %>%
  addCircleMarkers(data = all_chargers, color = "orange", radius = 4, label = ~name, group = "EV Chargers") %>%
  addCircleMarkers(data = all_pois, color = "purple", radius = 4, label = ~name, group = "Hospitality POIs") %>%
  addLayersControl(
    overlayGroups = c("Routes", "Buffers", " EV Chargers", "Hospitality POIs"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addScaleBar(position = "bottomleft")

# Calculate EV-Driving-Suitability-Index (EDSI)
edsi_df <- data.frame(destination = character(), charger_density = numeric(),
                      fast_charger_pct = numeric(), poi_density = numeric(),
                      avg_interstation = numeric(), EDSI = numeric())

normalize <- function(x) {
  (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
}

# Initialize empty dataframe to store raw values
edsi_raw_df <- data.frame(destination = character(),
                          charger_density = numeric(),
                          fast_charger_pct = numeric(),
                          poi_density = numeric(),
                          avg_interstation = numeric(),
                          stringsAsFactors = FALSE)

for (dest_id in names(chargers_list)) {
  chargers <- chargers_list[[dest_id]]
  pois <- poi_list[[dest_id]]
  route <- routes_list[[dest_id]]
  
  # Charger density per 100km
  route_length <- sum(st_length(route)) / 1000  # in km
  if (as.numeric(route_length) > 0) {
    charger_density <- nrow(chargers) / (as.numeric(route_length) / 100)
  } else {
    charger_density <- 0
  }
  
  # % fast chargers
  fast_pct <- ifelse(nrow(chargers) > 0, mean(chargers$fast, na.rm = TRUE), 0)
  
  # Convert chargers to sf
  chargers_sf <- st_as_sf(chargers, coords = c("lon", "lat"), crs = 4326)
  
  # Convert pois to sf if not already
  if (!inherits(pois, "sf") && !is.null(pois)) {
    if ("geometry" %in% names(pois)) {
      pois <- st_as_sf(pois)
    } else {
      pois <- NULL
    }
  }
  
  # POI density near chargers (within 1 km)
  if (!is.null(pois) && nrow(pois) > 0 && nrow(chargers_sf) > 0) {
    pois_near <- st_join(chargers_sf, pois, join = st_is_within_distance, dist = 1000)
    poi_density <- nrow(na.omit(pois_near)) / nrow(chargers)
  } else {
    poi_density <- 0
  }
  
  # Average inter-station distance
  if (nrow(chargers) > 1) {
    dist_matrix <- st_distance(chargers_sf)
    avg_interstation <- mean(dist_matrix[lower.tri(dist_matrix)], na.rm = TRUE) / 1000  # km
  } else {
    avg_interstation <- NA
  }
  
  # Normalize and combine
  edsi <- mean(
    c(
      normalize(charger_density),
      normalize(fast_pct),
      normalize(poi_density),
      1 - normalize(as.numeric(avg_interstation))
    ),
    na.rm = TRUE
  )
  
  # Append to dataframe
  edsi_raw_df <- rbind(edsi_raw_df, data.frame(
    destination = dest_id,
    charger_density = charger_density,
    fast_charger_pct = fast_pct,
    poi_density = poi_density,
    avg_interstation = as.numeric(avg_interstation)
  ))
}

edsi_raw_df$EDSI <- rowMeans(data.frame(
  normalize(edsi_raw_df$charger_density),
  normalize(edsi_raw_df$fast_charger_pct),
  normalize(edsi_raw_df$poi_density),
  1 - normalize(edsi_raw_df$avg_interstation)
), na.rm=TRUE)

# View final EDSI data frame
print(edsi_raw_df)

# Create EDSI bar plot
ggplot(edsi_raw_df, aes(x = destination, y = EDSI, fill = destination)) +
  geom_col() +
  labs(title = "EV-Driving-Suitability-Index (EDSI)", y = "EDSI (0-1 Scale)", x = "Destination") +
  theme_minimal()




