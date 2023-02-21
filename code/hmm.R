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
mixtures <- 1 # from model selection on mixed hmms

## states
nbStates <- c(2, 3)
nbStates <- 3

# timesteps 
timeStep <- seq(5, 15, 5) %>% paste0(" mins")
#timeStep <- "5 mins"

# formulas
variables <- c("cosinor(hour, period = 24)", "temp", "height * delta.height", "habitat")

# empty object for results
formulas <- NULL

# create a thing of all possible combinations of our covariates
for (v in 1:length(variables)) {
  
  temp <- combinations(n = length(variables),
                       r = v,
                       v = variables)
  
  formulas <- temp %>% 
    as.data.frame() %>%
    bind_rows(formulas)
  
  if (v == length(variables)){
    formulas <- formulas %>%
      unite("formula", 1:length(variables), sep = " + ", na.rm = TRUE) %>%
      mutate(formula = paste0("~", formula))
    
    formulas <- formulas[["formula"]]
    
    formulas <- c(formulas, "~1")
  }
}

# now make a df with all possible combinations
combinations <- crossing(nbStates, formulas, timeStep)

# for file name
#formula_label <- seq(1, length(formulas), 1) %>% paste0("formula", .) %>% rep(each = 3) %>% rep(2)
formula_label <- seq(1, length(formulas), 1) %>% paste0("formula", .) %>% rep(each = 3)

# add the formula label for saving
combinations <- combinations %>% bind_cols(label = formula_label)

combinations <- combinations %>% filter(formula_label %in% c("formula3", "formula4", "formula5",
                                                  "formula6", "formula7", "formula8",
                                                  "formula11", "formula12") &
                               timeStep == "5 mins" | 
                               formula_label %in% c("formula4", "formula5") & timeStep == "10 mins" |
                               formula_label == "formula5" & timeStep == "15 mins")

# now assign a value to each parameter based on the array index
nbStates <- combinations[[k, 1]]
formula <- combinations[[k, 2]]
timeStep <- combinations[[k, 3]]
formula_label <- combinations[[k, 4]]

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
            #DM = DM,
            formula = formula(formula),
            estAngleMean = estAngleMean,
            #nlmPar = list(print.level = 2),  # for diagnosis of troublesome parameters
            optMethod = "Nelder-Mead", # nlm having trouble so try this routine
            prior = prior) # used to keep parameters away from boundary
  
# name for the output
name <- paste0(getwd(),
               "/output/crab-hmm-", 
               nbStates, "-state-", 
               timeStep, "-", 
               formula_label, 
              # mixtures, "-mixtures-", 
               ".rds")

# save the output
saveRDS(m, name)












  
# covariates for mean step and mean.accel
#DM <- list(step = list(mean = ~1, sd = ~1, zeromass = ~1),
 #          mean.accel = list(mean = ~1, sd = ~1, zeromass = ~1))




  
  
  
  
  
  # DM for lag1 model
  #DM1 <- list(step = list(mean = ~1, sd = ~1, zeromass = ~1),
   #           mean.accel = list(mean = ~prev.accel, sd = ~1, zeromass = ~1))
  
  # get initial values for lag1 model
  #Par01 <- getPar0(m, DM = DM1)
  
  # fit the model
  #m1 <- fitHMM(data = prepData, 
   #            nbStates = nbStates, 
    #           retryFits = retryFits, 
     #          dist = dist, 
      #         Par0 = Par01$Par,
       #        beta0 = Par01$beta,
        #       formula = formula,
         #      estAngleMean = estAngleMean,
          #     DM = DM1)
  
  # name for the output
  #name <- paste0("output/crab_hmm_lag", nbStates, "state_", length(dist), "streams_", timeStep[index], ".rds")
  
  # save the output
  #save_object(m1, name)
  
#} else { # make this only odd numbers
  # 3 data-streams, 3 states
  
  # probability distributions for data streams
  #dist <- list(step = "gamma", angle = "wrpcauchy", mean.accel = "gamma")
  
  
  
  # starting parameters
  #Par0 <- list(step = stepPar0,
   #            angle = anglePar0,
    #           mean.accel = accelPar0)
  
  # fit the model
  #m <- fitHMM(data = prepData, # the data
   #           nbStates = nbStates, # number of states
    #          retryFits = retryFits, # random iterations of initial values to assist MLE
     #         dist = dist, # data-stream distributions
      #        Par0 = Par0,
       #       formula = formula[[index]],
              #DM = DM,
        #      estAngleMean = estAngleMean)
  
  # name for the output
  #name <- paste0("output/crab_hmm_", length(dist), "_streams_", "_", nbStates, "_state_", timeStep[index], ".rds")
  
  # save the output
  #save_object(m, name)
  
  # DM for lag1 model
  #DM1 <- list(step = list(mean = ~1, sd = ~1, zeromass = ~1),
   #           mean.accel = list(mean = ~prev.accel, sd = ~1, zeromass = ~1))
  
  # get initial values for lag1 model
  #Par01 <- getPar0(m, DM = DM1)
  
  # fit the model
  #m1 <- fitHMM(data = prepData, 
   #            nbStates = nbStates, 
    #           retryFits = retryFits, 
     #          dist = dist, 
      #         Par0 = Par01$Par,
       #        beta0 = Par01$beta,
        #       formula = formula,
         #      estAngleMean = estAngleMean,
          #     DM = DM1)
  
  # name for the output
  #name <- paste0("output/crab_hmm_lag", nbStates, "state_", length(dist), "streams_", timeStep[index], ".rds")
  
  # save the output
  #save_object(m1, name)
#}
