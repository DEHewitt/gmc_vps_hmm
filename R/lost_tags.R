lost_tags <- function(data, tags){
  
  # tags = the character string that identifies lost (or moulted) tags to be removed
  
  for (i in 1:length(tags)) {
    data <- data %>%
      filter(ID != tags[i])
  }
  data
}
