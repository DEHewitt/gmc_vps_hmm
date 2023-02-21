stationary_probs <- function(m){
  # this function takes a fitted momentuhmm object
  # and returns a df with the stationary probabilities for each covariate
  
  covars <- seq(1, length(m$rawCovs), 1)
  
  nbStates <- seq(1, length(m$stateNames), 1)
  
  x <- plotStationary(m, plotCI = TRUE, return = TRUE)
  
  # empty object for overall results
  out <- NULL
  
  for(i in 1:length(covars)){
    
    # empty object for results of inner loop
    df <- NULL
    
    for(j in 1:length(nbStates)){
      temp <- x[[i]][[j]] %>% as.data.frame()
      
      cols <- c("est", "se", "lci", "uci", "cov") %>% paste0(names(x[i]), "_", .)
      
      names(temp) <- cols
      
      #temp <- temp %>% add_column(state = j)
      
      df <- df %>% bind_rows(temp)
    }
    
    out <- out %>% bind_cols(df)
  }
  
  if(length(nbStates) == 2){
    out <- out %>% add_column(state = c(rep(1, 101), rep(2, 101))) %>% dplyr::mutate(state = as.factor(state))
  } else if (length(nbStates) == 3){
    out <- out %>% add_column(state = c(rep(1, 101), rep(2, 101), rep(3, 101))) %>% dplyr::mutate(state = as.factor(state))
  }
  
}
