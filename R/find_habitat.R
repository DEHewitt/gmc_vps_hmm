find_habitat <- function(data, habitat){
  
  # housekeeping with the spatial points
  utmcoord.1 <- SpatialPoints(as.data.frame(data %>% dplyr::select(x, y)),
                              proj4string = CRS("+proj=utm +zone=56 +datum=WGS84"))
  llcoord.1 <- spTransform(utmcoord.1, CRS("+proj=longlat +datum=WGS84"))
  
  # convert UTM to lat/lon in prepData so it can be intersected with shapefile
  data$lon <- attr(llcoord.1, "coords")[,1]
  data$lat <- attr(llcoord.1, "coords")[,2]
  
  # add a dummy row ID column
  data <- data %>%
    mutate(row.id = row_number())
  
  # get the points as a spatial df
  points <- st_as_sf(as.data.frame(data %>% dplyr::select(lon, lat, row.id)), 
                     coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")
  
  habitat <- habitat %>% st_transform(crs = "+proj=longlat + datum=WGS84")
  points <- point.in.poly(points, habitat)
  
  points <- as.data.frame(points[, 1:2]) %>%
    rename(habitat = HABITAT) %>%
    mutate(habitat = if_else(is.na(habitat), "Soft sediment", habitat))
  
  data <- data %>%
    left_join(points, by = "row.id") %>%
    dplyr::select(-row.id, -coords.x1, -coords.x2)
  
  #data <- data %>% mutate(habitat = factor(habitat))
}












