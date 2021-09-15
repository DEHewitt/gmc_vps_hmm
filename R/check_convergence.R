check_convergence <- function(data){
  tracks <- seq(1:length(data$crwFits))
  crw.convergence <- data.frame()
  for (i in 1:length(tracks)){
    temp <- data.frame(ID = names(data$crwFits[i]),
                       loglik = data$crwFits[[i]]$loglik,
                       aic = data$crwFits[[i]]$aic,
                       convergence = data$crwFits[[i]]$convergence, 
                       message = data$crwFits[[i]]$message)
    crw.convergence <- bind_rows(crw.convergence, temp)
  }
  crw.convergence
}
