n_prop <- function(input_data, interpolated_data){
  obs.loc <- as.numeric(length(input_data$lat)) # number of observed locations
  int.loc <- as.numeric(length(interpolated_data$lat))
  
  nprop <- int.loc/obs.loc
  nprop
}
