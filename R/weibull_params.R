weibull_params <- function(m){
  # m is a fitted momentuhmm model
  # this function will find the number of states and convert the shape/scale params of the weibull into the mean
  
  # find out how many states there are
  nbStates <- as.numeric(length(m$stateNames))
  
  # generate a sequence for getting the right parameters out
  if(nbStates == 2){
    sequence <- c(1, 4)
  } else if (nbStates == 3){
    sequence <- seq(1, nbStates*nbStates, nbStates)
  } else if (nbStates == 4){
    sequence <- seq(1, nbStates*3, 3)
  }
  
  #means <- NULL
  print(paste0("Mean (+/- SD) step length for the ", nbStates, " states are"))
  # iteratively calculate the means and variance
  for (i in sequence){
      shape <- m$mle$step[i]
      scale <- m$mle$step[i+1]
      
      mean <- scale*gamma(1+1/shape)
      variance <- (scale**2)*(gamma(1+2/shape)-gamma(1+1/shape)**2)
      
      print(paste0(round(mean, 3), " +/- ", round(variance, 3)))
  }
}
