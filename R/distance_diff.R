distance_diff <- function(data, crs){
  
  # this function adds a column that is the euclidean distance between consecutive detections
  # the method taken from https://github.com/r-spatial/sf/issues/799
  # the crs for wgs84 is 4326
  # this function takes a fair while...
  
  # reduce to the trhow away df
  x <- data %>% dplyr::select(ID, time, lon, lat)
  
  # convert to an sf object
  x <- x %>% st_as_sf(coords = c("lon", "lat"))
  
  # set the coordinate reference system
  st_crs(x) <- crs
  
  # assign an empty geometry
  empty <- st_as_sfc("POINT(EMPTY)", crs = crs)
  
  # calculate the distance
  x <- x %>%
    group_by(ID) %>% # for each crab
    arrange(time) %>% # in the right time order
    mutate(distance = as.numeric(sf::st_distance(geometry, lag(geometry, default = empty), by_element = TRUE))) %>% # actual distance calculation
    ungroup()
  
  # join back to the main df
  x <- x %>% sfheaders::sf_to_df(fill = TRUE) %>% dplyr::select(ID, time, distance)
  
  data <- x %>% left_join(data)
  
  data
}
