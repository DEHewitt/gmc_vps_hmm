# load the packages
library(momentuHMM)

# setwd() on katana
if (Sys.info()[6] != "Dan"){
  setwd("/srv/scratch/z5278054/VPS")
}

# load custom functions
source("R/save_object.R")

# get index for array jobs - for fitting multiple hmms
index <- as.integer(Sys.getenv('PBS_ARRAY_INDEX'))

# read in the data
prepData <- readRDS("crab_prep.rds")

# number of random perturbations of initial parameters
retryFits <- 25

if (index == 1){ 
  # 3 data-streams, 2 states
  
  # probability distributions for data streams
  dist <- list(step = "gamma", angle = "vm", mean.accel = "gamma")
  
  # number of states
  nbStates <- 2
  
  # starting parameters
  Par0 <- list(step = c(0.8, 2,  # mean
                        0.8, 2), # sd
               angle = c(0.1, 10), # concentration
               mean.accel = c(0.2, 1.5, # mean
                              0.2, 1.5, # sd
                              0.5, 0.0005)) # zero-mass
  
  # fit the model
  m <- fitHMM(data = prepData, # the data
              nbStates = nbStates, # number of states
              retryFits = retryFits, # random iterations of initial values to assist MLE
              dist = dist, # data-stream distributions
              Par0 = Par0,
              formula = ~1,
              estAngleMean = list(angle = FALSE))
  
  # name for the output
  name <- paste0("crab_hmm_", nbStates, "state_", length(dist), "streams.rds")
  
  # save the output
  save_object(m, name)
  
} else if (index == 2){
  # 3 data-streams, 3 states
  
  # probability distributions for data streams
  dist <- list(step = "gamma", angle = "vm", mean.accel = "gamma")
  
  # number of states
  nbStates <- 3
  
  # starting parameters
  Par0 <- list(step = c(0.8, 2, 5,  # mean
                        0.8, 2, 5), # sd
               angle = c(0.1, 10, 5), # concentration
               mean.accel = c(0.2, 1.5, 2, # mean
                              0.2, 1.5, 2, # sd
                              0.5, 0.0005, 0.0005)) # zero-mass
  
  # fit the model
  m <- fitHMM(data = prepData, # the data
              nbStates = nbStates, # number of states
              retryFits = retryFits, # random iterations of initial values to assist MLE
              dist = dist, # data-stream distributions
              Par0 = Par0,
              formula = ~1,
              estAngleMean = list(angle = FALSE))
  
  # name for the output
  name <- paste0("crab_hmm_", nbStates, "state_", length(dist), "streams.rds")
  
  # save the output
  save_object(m, name)
  
} else if (index == 3){
  # 2 data-streams, 2 states
  
  # probability distributions for data streams
  dist <- list(step = "gamma", angle = "vm")
  
  # number of states
  nbStates <- 2
  
  # starting parameters
  Par0 <- list(step = c(0.8, 2,  # mean
                        0.8, 2), # sd
               angle = c(0.1, 10))
  
  # fit the model
  m <- fitHMM(data = prepData, # the data
              nbStates = nbStates, # number of states
              retryFits = retryFits, # random iterations of initial values to assist MLE
              dist = dist, # data-stream distributions
              Par0 = Par0,
              formula = ~1,
              estAngleMean = list(angle = FALSE))
  
  # name for the output
  name <- paste0("crab_hmm_", nbStates, "state_", length(dist), "streams.rds")
  
  # save the output
  save_object(m, name)
  
} else if(index == 4){
  # 2 data-streams, 3 states
  
  # probability distributions for data streams
  dist <- list(step = "gamma", angle = "vm")
  
  # number of states
  nbStates <- 3
  
  # starting parameters
  Par0 <- list(step = c(0.8, 2, 5,  # mean
                        0.8, 2, 5), # sd
               angle = c(0.1, 10, 5))
  
  # fit the model
  m <- fitHMM(data = prepData, # the data
              nbStates = nbStates, # number of states
              retryFits = retryFits, # random iterations of initial values to assist MLE
              dist = dist, # data-stream distributions
              Par0 = Par0,
              formula = ~1,
              estAngleMean = list(angle = FALSE))
  
  # name for the output
  name <- paste0("crab_hmm_", nbStates, "state_", length(dist), "streams.rds")
  
  # save the output
  save_object(m, name)
  
}