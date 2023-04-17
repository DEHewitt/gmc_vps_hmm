time_diff <- function(data, group){
  
  # this function returns the time difference (in mins) between detections for each tag
  
  data %>%
    group_by(group) %>%
    arrange(time) %>%
    mutate(time.diff = time - lag(time, default = first(time))) %>% # this is in seconds
    mutate(time.diff = as.numeric(time.diff)/60) %>% # converted to minutes
    ungroup()
}
