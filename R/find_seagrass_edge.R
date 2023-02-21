find_seagrass_edge <- function(data, habitat, distance){
  # extract just seagrass polygons from the habitat layer
  habitat <- habitat[habitat$HABITAT == "Seagrass",]
  
  # create a buffer around the seagrass
  seagrass_edge <- st_buffer(habitat, dist = distance)
  
  # add a dummy row ID column for joining purposes
  data <- data %>%
    mutate(row.id = row_number())
  
  # find which points are within that layer
  soft_sed <- data %>% filter(habitat == "Soft sediment")
  
  # from here down copied from habitat function
  # housekeeping with the spatial points
  utmcoord.1 <- SpatialPoints(as.data.frame(soft_sed %>% dplyr::select(x, y)),
                              proj4string = CRS("+proj=utm +zone=56 +datum=WGS84"))
  llcoord.1 <- spTransform(utmcoord.1, CRS("+proj=longlat +datum=WGS84"))
  
  # convert UTM to lat/lon in prepData so it can be intersected with shapefile
  soft_sed$lon <- attr(llcoord.1, "coords")[,1]
  soft_sed$lat <- attr(llcoord.1, "coords")[,2]
  
  # get the points as a spatial df
  points <- st_as_sf(as.data.frame(soft_sed %>% dplyr::select(lon, lat, row.id)), 
                     coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")
  
  seagrass_edge <- seagrass_edge %>% st_transform(crs = "+proj=longlat + datum=WGS84")
  points <- point.in.poly(points, seagrass_edge)
  
  points <- as.data.frame(points[, 1:2]) %>%
    rename(seagrass_edge = HABITAT) %>%
    mutate(seagrass_edge = if_else(is.na(seagrass_edge), NA_character_, "Seagrass edge"))
  
  data <- data %>%
    left_join(points, by = "row.id") %>%
    dplyr::select(-row.id, -coords.x1, -coords.x2)
  
  data <- data %>% mutate(habitat = if_else(is.na(seagrass_edge), habitat, seagrass_edge)) %>% dplyr::select(-seagrass_edge)
}
