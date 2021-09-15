create_tracks <- function(data, method, max.time, max.speed){
  # this function is modified from refined shortest paths
  
  # data is the df
  # method = "time" or "speed"
  # max.time is the max. time (in minutes) between detections before splitting into tracks
  # max.speed is the max. speed (in metres per second) we expect a crab to be able to swim
  
  # data must have:
  # a column named ID that identifies each animal
  # a column that is the time difference between detections (in minutes) for method = "time"
  # or a column that is the distance between detections (in metres) for method = "speed"
  # for method = "time" function will also recalculate the time differences as the previous ones will now be inaccurate
  # for method = "speed" the df needs to have a time between detections too so that speed can be caluclated
  
  # this function will split up tracks based on the differences in time/distance between consecutive detections (according to the value of max.time or max.distance)
  
  if (method == "time"){
    # split the data by the ID
    tracks <- data %>% split(.$ID)
    
    # make a list of the tags
    tags <- seq(1:length(tracks))
    
    # for every tag generate a new label if the time between detections is greater than max.time
    for (i in 1:length(tags)) {
      breaks <- which(tracks[[tags[i]]]$time.diff > max.time) 
      starts <- c(1, breaks)
      stops  <- c(breaks, nrow(tracks[[tags[i]]]) + 1)
      n <- (stops - starts)
      
      track.index <- paste0("Track_", unlist(lapply(1:length(n), function(i) {
        stringr::str_pad(string = rep(i, n[i]), width = nchar(length(n)), pad = "0")
      })))
      tracks[[tags[i]]]$track <- track.index
    }
    crab.data <- tracks %>% bind_rows()
    crab.data <- crab.data %>%
      mutate(ID = paste(ID, track, sep = "-")) %>%
      dplyr::select(-track)
    
    crab.data <- crab.data %>% time_diff()
    
  } else if (method == "speed"){
    
    # calculate the implied speed between detections
    data <- data %>%
      group_by(ID) %>%
      arrange(time) %>%
      mutate(speed = if_else(time.diff > 0, distance/time.diff/60, 0)) %>%
      ungroup()
    
    # split the data by the ID
    tracks <- data %>% split(.$ID)
    
    # make a list of the tags
    tags <- seq(1:length(tracks))
    
    # for every tag generate a new label if the time between detections is greater than max.time
    for (i in 1:length(tags)) {
      breaks <- which(tracks[[tags[i]]]$speed > max.speed) 
      starts <- c(1, breaks)
      stops  <- c(breaks, nrow(tracks[[tags[i]]]) + 1)
      n <- (stops - starts)
      
      track.index <- paste0("Track_", unlist(lapply(1:length(n), function(i) {
        stringr::str_pad(string = rep(i, n[i]), width = nchar(length(n)), pad = "0")
      })))
      tracks[[tags[i]]]$track <- track.index
    }
    crab.data <- tracks %>% bind_rows()
    crab.data <- crab.data %>%
      mutate(ID = paste(ID, track, sep = "-")) %>%
      dplyr::select(-track)
    
    crab.data <- crab.data %>% 
      group_by(ID) %>% 
      arrange(time) %>% 
      dplyr::select(-time.diff, -distance) %>% 
      time_diff() %>% 
      distance_diff(crs = 4326) %>%
      ungroup()
  }
}
