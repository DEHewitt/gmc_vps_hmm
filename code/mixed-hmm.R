# load the packages
library(momentuHMM)
library(tidyverse)
library(gtools)
library(utile.tools)

# setwd() on katana
if (Sys.info()[6] != "Dan"){
  setwd("/srv/scratch/z5278054/VPS")
}

# get index for array jobs - for fitting multiple hmms
k <- as.integer(Sys.getenv('PBS_ARRAY_INDEX'))

# list all possible values for the model parameters
## mixtures
mixtures <- c(1, 2, 3, 4)
#mixtures <- c(3, 4) # took longer...

## states
nbStates <- c(2, 3)
#nbStates <- 3 # took longer...

# timesteps 
timeStep <- seq(5, 15, 5) %>% paste0(" mins")

# now make a df with all possible combinations
combinations <- crossing(mixtures, nbStates, timeStep)

# now assign a value to each parameter based on the array index
mixtures <- combinations[[k, 1]]
nbStates <- combinations[[k, 2]]
timeStep <- combinations[[k, 3]]

# print the timestep
print(timeStep)

# effects on mixture
if (mixtures > 1){
  formulaPi <- formula("~crab") # individual-level random effects
} else {
  formulaPi <- NULL
}

# number of random perturbations of initial parameters
retryFits <- 50

# probability distributions for data streams
dist <- list(step = "gamma", angle = "wrpcauchy", mean.accel = "gamma")

# estimate turning angle?
estAngleMean <- list(angle = FALSE)

# open the data
prepData <- readRDS(paste0("output/crab_prep_env_", timeStep, ".rds"))

if (nbStates == 2){
  
  # initial step parameters (mean and sd = log-link, zeromass = logit)
  if(min(prepData$step, na.rm = T) == 0){ # if any step length = 0 then need a zero-mass
    stepPar0 <- c(11, 0.8,  # mean
                  1.5, 0.9, # sd
                  0.00001, 0.01) # zero-mass
  } else {
    stepPar0 <- c(11, 0.8,  # mean
                  1.5, 0.9) # sd
  }
  
  # initial angle parameters (natural scale)
  anglePar0 <- c(0.48, 0.41) # concentration
  
  # inital accel parameters (mean and sd = log link, zeromass = logit)
  if(min(prepData$mean.accel, na.rm = TRUE) == 0){ # if any accel = 0 then need a zero-mass
    accelPar0 <- c(0.5, 0.04, # mean
                   0.5, 0.01, # sd
                   0.007, 0.002) # zero-mass
  } else {
    accelPar0 <- c(0.5, 0.04, # mean
                   0.5, 0.01) # sd
    
  }
  
} else if (nbStates == 3){
  
  # initial step parameters (working scale)
  if(min(prepData$step, na.rm = T) == 0){ # if any step length = 0 then need a zero-mass
    stepPar0 <- c(2.2, 12.8, 0.3,  # mean
                  2, 16, 0.2, # sd
                  0.03, 0.000001, 0.000001) # zero-mass
  } else {
    stepPar0 <- c(2.2, 12.8, 0.3,  # mean
                  2, 16, 0.2) # sd
  }
  
  # initial angle parameters (natural scale)
  anglePar0 <- c(0.16, 0.5, 0.5) # concentration
  
  # inital accel parameters (working scale)
  if(min(prepData$mean.accel, na.rm = TRUE) == 0){ # if any accel = 0 then need a zero-mass
    accelPar0 <- c(0.04, 0.5, 0.04, # mean
                   0.01, 0.5, 0.01, # sd
                   0.004, 0.007, 0.001) 
    
  } else {
    accelPar0 <- c(0.2, 1.5, 2, # mean
                   0.2, 1.5, 2) #sd
  }
}

# starting parameters
Par0 <- list(step = stepPar0,
             angle = anglePar0,
             mean.accel = accelPar0)

# parameter constraints to ensure estimates don't wander into the 'nether regions' during optimization
# from: https://rdrr.io/cran/momentuHMM/man/fitHMM.html
prior <- function(par){
  sum(dnorm(par, 0, 100, log = TRUE))
}

# fit the model
m <- fitHMM(data = prepData,
            nbStates = nbStates,
            retryFits = retryFits,
            dist = dist,
            Par0 = Par0,
            mixtures = mixtures,
            formulaPi = formulaPi,
            estAngleMean = estAngleMean,
            optMethod = "Nelder-Mead", # nlm having trouble so try this routine
            prior = prior) # used to keep parameters away from boundary
  
# name for the output
name <- paste0(getwd(),
               "/output/crab-hmm-", 
               nbStates, "-state-", 
               timeStep, "-", 
               #formula_label, "-", 
               mixtures, "-mixtures-", 
               ".rds")

# save the output
saveRDS(m, name)