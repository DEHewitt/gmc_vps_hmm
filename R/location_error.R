location_error <- function(data, station.data, vps.error){
  # this function will calculate the error in x & y directions and correlation
  # to be fed into crawlWrap when estimating temporally regular locations
  # from irregularly recorded detections
  
  station.data <- station.data %>% 
    filter(FullId %in% ref.tag) %>%
    dplyr::select(FullId, Long, Lat) %>%
    mutate(Long = as.numeric(Long),
           Lat = as.numeric(Lat))
  
  coordinates(station.data) <- c("Long", "Lat") # assign coordinates
  proj4string(station.data) <- CRS("+proj=longlat") # assign CRS
  station.data <- station.data %>%
    spTransform("+proj=utm +zone=56 +south +datum=WGS84 +units=m +no_defs") # transform to UTM
  station.data <- as_tibble(station.data) %>%
    rename(x_receiver = Long, y_receiver = Lat)
  
  error <- vps.error %>%
    dplyr::select(FullId, Time, Longitude, Latitude)
  
  coordinates(error) <- c("Longitude", "Latitude") # assign coordinates
  proj4string(error) <- CRS("+proj=longlat") # assign CRS
  error <- error %>%
    spTransform("+proj=utm +zone=56 +south +datum=WGS84 +units=m +no_defs")
  error <- as_tibble(error) %>%
    rename(x_detection = Longitude, y_detection = Latitude)
  
  error <- error %>%
    left_join(station.data) %>%
    mutate(x_error = x_receiver - x_detection) %>%
    mutate(y_error = y_receiver - y_detection)
  
  data <- data %>%
    mutate(ln.sd.x = log(sd(error$x_error)),
           ln.sd.y = log(sd(error$y_error)),
           error.corr = cor(error$x_error, error$y_error))
  
}


