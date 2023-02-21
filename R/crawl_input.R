crawl_input <- function(data){
  n_tracks <- length(unique(data$ID))
  n_crabs <- length(unique(str_sub(data$ID, 1, 13)))
  
  print(paste0("The data processing applied here results in the CTCRW being fit to ",
               n_tracks, " tracks from ", n_crabs, " crabs"))
}
