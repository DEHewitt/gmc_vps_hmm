few_detections <- function(data, n){
  
  # n = the lower threshold for the number of detections in a track
  # the idea of this is to help with mle (i.e., more data, more numerical stability)
  
  few.detections <- data %>%
    group_by(ID) %>%
    summarise(n.det = n()) %>%
    filter(n.det < n) %>% 
    ungroup()
  
  few.detections <- unique(few.detections$ID)
  
  for (i in 1:length(few.detections)) {
    data <- data %>%
      filter(ID != few.detections[i])
  }
  data
}
