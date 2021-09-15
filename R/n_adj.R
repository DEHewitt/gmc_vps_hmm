n_adj <- function(input_data, interpolated_data){
  obs.loc <- as.numeric(length(input_data$lat)) # number of observed locations
  int.loc <- as.numeric(length(interpolated_data$lat)) # number of interpolated locations
  n.groups <- as.numeric(length(unique(interpolated_data$ID)))
  
  # check 2: nadj
  nadj <- (int.loc - (2 * n.groups))/(obs.loc - 2)
  nadj
}
