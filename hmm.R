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
prepData <- readRDS("output/crab_prep_env.rds")

# number of random perturbations of initial parameters
retryFits <- 25

formula <- ~height + cosinor(hour, period = 24) # tide height is also cyclic...

if (index == 1){ 
  # 3 data-streams, 2 states
  
  # probability distributions for data streams
  dist <- list(step = "weibull", angle = "wrpcauchy", mean.accel = "gamma")
  
  # number of states
  nbStates <- 2
  
  # starting parameters
  Par0 <- list(step = c(0.8, 2,  # mean
                        0.8, 2, # sd
                        0.8, 0.0001), # zero-mass
               angle = c(0, 0, 
                         0.07, 0.03), # concentration
               mean.accel = c(0.2, 1.5, # mean
                              0.2, 1.5, # sd
                              0.5, 0.0005)) # zero-mass
  
  # fit the model
  m <- fitHMM(data = prepData, # the data
              nbStates = nbStates, # number of states
              retryFits = retryFits, # random iterations of initial values to assist MLE
              dist = dist, # data-stream distributions
              Par0 = Par0,
              formula = formula,
              estAngleMean = list(angle = TRUE))
  
  # name for the output
  name <- paste0("output/5 mins_crab_hmm_", nbStates, "state_", length(dist), "streams.rds")
  
  # save the output
  save_object(m, name)
  
} else if (index == 2){
  # 3 data-streams, 3 states
  
  # probability distributions for data streams
  dist <- list(step = "weibull", angle = "wrpcauchy", mean.accel = "gamma")
  
  # number of states
  nbStates <- 3
  
  # starting parameters
  Par0 <- list(step = c(0.5, 2, 9,  # mean
                        0.5, 2, 9, # sd
                        0.8, 0.0001, 0.0001), # zero-mass
               angle = c(0, 0, 0, 
                         0.07, 0.9, 0.03), # concentration
               mean.accel = c(0.2, 1.5, 2, # mean
                              0.2, 1.5, 2, # sd
                              0.5, 0.0005, 0.0005)) # zero-mass
  
  # fit the model
  m <- fitHMM(data = prepData, # the data
              nbStates = nbStates, # number of states
              retryFits = retryFits, # random iterations of initial values to assist MLE
              dist = dist, # data-stream distributions
              Par0 = Par0,
              formula = formula,
              estAngleMean = list(angle = TRUE))
  
  # name for the output
  name <- paste0("output/5 mins_crab_hmm_", nbStates, "state_", length(dist), "streams.rds")
  
  # save the output
  save_object(m, name)
  
} else if (index == 3){
  # 2 data-streams, 2 states
  
  # probability distributions for data streams
  dist <- list(step = "weibull", angle = "wrpcauchy")
  
  # number of states
  nbStates <- 2
  
  # starting parameters
  Par0 <- list(step = c(0.5, 9,  # mean
                        0.5, 9, # sd
                        0.8, 0.00001), # zero-mass
               angle = c(0, 0,
                         0.07, 0.03))
  
  # fit the model
  m <- fitHMM(data = prepData, # the data
              nbStates = nbStates, # number of states
              retryFits = retryFits, # random iterations of initial values to assist MLE
              dist = dist, # data-stream distributions
              Par0 = Par0,
              formula = formula,
              estAngleMean = list(angle = TRUE))
  
  # name for the output
  name <- paste0("output/5 mins_crab_hmm_", nbStates, "state_", length(dist), "streams.rds")
  
  # save the output
  save_object(m, name)
  
} else if(index == 4){
  # 2 data-streams, 3 states
  
  # probability distributions for data streams
  dist <- list(step = "weibull", angle = "wrpcauchy")
  
  # number of states
  nbStates <- 3
  
  # starting parameters
  Par0 <- list(step = c(0.5, 2, 9,  # mean
                        0.5, 2, 9, # sd
                        0.8, 0.00001, 0.00001), # zero-mass 
               angle = c(0, 0, 0,
                         0.07, 0.9, 0.03))
  
  # fit the model
  m <- fitHMM(data = prepData, # the data
              nbStates = nbStates, # number of states
              retryFits = retryFits, # random iterations of initial values to assist MLE
              dist = dist, # data-stream distributions
              Par0 = Par0,
              formula = formula,
              estAngleMean = list(angle = TRUE))
  
  # name for the output
  name <- paste0("output/5 mins_crab_hmm_", nbStates, "state_", length(dist), "streams.rds")
  
  # save the output
  save_object(m, name)
} else if(index == 5){
  # 2 data-streams, 4 states
  
  # probability distributions for data streams
  dist <- list(step = "weibull", angle = "wrpcauchy")
  
  # number of states
  nbStates <- 4
  
  # starting parameters
  Par0 <- list(step = c(0.5, 2, 9, 20,  # mean
                        0.5, 2, 9, 20, # sd
                        0.8, 0.00001, 0.00001, 0.00001), # zero-mass 
               angle = c(0, 0, 0, 0, 
                         0.07, 0.9, 0.03, 0.5))
  
  # fit the model
  m <- fitHMM(data = prepData, # the data
              nbStates = nbStates, # number of states
              retryFits = retryFits, # random iterations of initial values to assist MLE
              dist = dist, # data-stream distributions
              Par0 = Par0,
              formula = formula,
              estAngleMean = list(angle = TRUE),
              nlmPar = list(print.level = 2))
  
  # name for the output
  name <- paste0("output/5 mins_crab_hmm_", nbStates, "state_", length(dist), "streams.rds")
  
  # save the output
  save_object(m, name)
} else if(index == 6){
  # 3 data-streams, 3 states
  
  # probability distributions for data streams
  dist <- list(step = "weibull", angle = "wrpcauchy", mean.accel = "gamma")
  
  # number of states
  nbStates <- 4
  
  # starting parameters
  Par0 <- list(step = c(0.5, 2, 9, 20,  # mean
                        0.5, 2, 9, 20, # sd
                        0.8, 0.0001, 0.0001, 0.0001), # zero-mass
               angle = c(0, 0, 0, 0, 
                         0.07, 0.9, 0.03, 0.5), # concentration
               mean.accel = c(0.2, 1.5, 2, 1, # mean
                              0.2, 1.5, 2, 1,# sd
                              0.5, 0.0005, 0.0005, 0.0005)) # zero-mass
  
  # fit the model
  m <- fitHMM(data = prepData, # the data
              nbStates = nbStates, # number of states
              retryFits = retryFits, # random iterations of initial values to assist MLE
              dist = dist, # data-stream distributions
              Par0 = Par0,
              formula = formula,
              estAngleMean = list(angle = TRUE),
              nlmPar = list(print.level = 2))
  
  # name for the output
  name <- paste0("output/5 mins_crab_hmm_", nbStates, "state_", length(dist), "streams.rds")
  
  # save the output
  save_object(m, name)
}