few_detections <- function(data, n, group){
  
  # n = the lower threshold for the number of detections in a track
  # the idea of this is to help with mle (i.e., more data, more numerical stability)
  
  few.detections <- data %>%
    group_by(group) %>%
    summarise(n.det = n()) %>%
    filter(n.det < n) %>% 
    ungroup()
  
  few.detections <- unique(few.detections$group)
  
  for (i in 1:length(few.detections)) {
    data <- data %>%
      filter(group != few.detections[i])
  }
  data
}
