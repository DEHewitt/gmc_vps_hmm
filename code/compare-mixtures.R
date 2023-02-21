# load {momentuHMM}
library(momentuHMM)
library(tidyverse)

m <- NULL

mixtures <- c("1", "2", "3", "4")

timeStep <- seq(5, 15, 5) %>% paste("mins")

nbStates <- c(2, 3)

# load the models and compare them
for (n in 1:length(nbStates)) {
  for (t in 1:length(timeStep)){
    for (mix in 1:length(mixtures)){
      
      temp <- readRDS(paste0("output/crab-hmm-", nbStates[n], "-state-", timeStep[t], "-", mixtures[mix], "-mixtures-.rds"))
      
      m[[mix]] <- temp
      
      if(mix == length(mixtures)){
        
        print(paste0(nbStates[n], "-state model with a ", timeStep[t], " interval:"))
        
        data.frame(K = c(1, 2, 3, 4),
                   AIC = c(AIC(m[[1]]), AIC(m[[2]]), AIC(m[[3]]), AIC(m[[4]])),
                   nPar = c(length(m[[1]]$mod$wpar), length(m[[2]]$mod$wpar), length(m[[3]]$mod$wpar), length(m[[4]]$mod$wpar))) %>%
          mutate(delta.AIC = AIC-min(AIC)) %>%
          print()
        
        
        
      }
      
    }
    
  }
  
}
