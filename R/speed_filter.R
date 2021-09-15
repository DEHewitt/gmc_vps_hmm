speed_filter <- function(data, interval, speed.limit){
  # function to filter step lengths that imply speeds greater than we expect for mud crabs
  # supply the interval in minutes
  # supply the speed.limit in metres per second
  
  # get the id of the crabs out
  cut.off <- speed.limit*interval*60
  fast.crabs <- data %>% filter(step > cut.off)
  
  fast.crabs <- as.character(unique(fast.crabs$ID))
  
  data <- data %>% filter(!(ID %in% fast.crabs))
  data
}
