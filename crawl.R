# load in packages
library(tidyverse)
library(momentuHMM)
library(rgdal)
library(ggplot2)
library(lubridate)
library(parallel)
library(sf)
library(sfheaders)
library(lwgeom)
library(spatialEco)

# import some environment variables
error_correction <- Sys.getenv("error_correction") # logical indicating whether or not to correct positions in crawlWrap()
speed_limit <- Sys.getenv("speed_limit") # logical indicating whether or not to split tracks based on some maximum implied swimming speed

# names for output
if(error_correction == TRUE){
  error_name <- "corrected"
} else {
  error_name <- "uncorrected"
}

if(speed_limit == TRUE){
  speed_name <- "speed_limited"
} else {
  speed_name <- "no_speed_limit"
}

if (Sys.info()[6] != "Dan"){
  setwd("/srv/scratch/z5278054/VPS")
}

# load custom functions
source("R/load_data.R") # custom function that loads in relevant data
source("R/time_diff.R") # calculates time difference between detections (in minutes)
source("R/distance_diff.R") # calculates the distance between detections (in metres)
source("R/lost_tags.R") # function to remove lost tags
source("R/create_tracks.R") # split paths up into tracks based on some time cutoff
source("R/few_detections.R") # remove tracks with too few detections
source("R/vps_error.R") # function to calculate error in the VPS
source("R/vps_error_plot.R") # function to plot daily error boxplots of the VPS
source("R/n_prop.R") # function for checking interpolation choices based on Lawler et al. (2019)
source("R/n_adj.R") # function for checking interpolation choices based on Lawler et al. (2019)
source("R/check_convergence.R") # check which tracks converged from crawlWrap()
source("R/location_error.R") # gets the estimated of location error (in x and y) to feed into crawlWrap()
source("R/save_object.R")

# load in the data
load_data()

# select the columns and rename them
crab.data <- raw.data %>% dplyr::select(ID = FullId,
                                        time = Time,
                                        lon = Longitude,
                                        lat = Latitude,
                                        accel = AccelData)

# remove the lost tags
crab.data <- crab.data %>% lost_tags(tags = c("A69-9006-7799", "A69-9006-7806"))

# calculate time difference between detections
crab.data <- crab.data %>% time_diff()

# split the paths up into tracks based on time
crab.data <- crab.data %>% create_tracks(max.time = 20, method = "time")

if (speed_limit == TRUE){
  # calculate distance between consecutive detections (in each track)
  crab.data <- crab.data %>% distance_diff(crs = 4326) # this takes a little while - make yourself a cuppa...
  
  # split the tracks up further based on the distance/time elapsed
  # split where distance imply swim speeds > max.speed
  # max.speed (in m/h) taken from Hill 1978 (https://doi.org/10.1007/BF00395634)
  max.speed <- 188
  
  # convert to metres per second
  max.speed <- max.speed/60/60
  
  # do the actual splitting
  crab.data <- crab.data %>% create_tracks(max.speed = max.speed, method = "speed") # this recalculates distances after splitting tracks so takes a little while
}

# set the minimum number of detections permissible in a track
# we will remove any tracks with less than n detections
# this helps with numerical stability while doing mle
# Bacheler et al., 2019 go with n = 100
n <- 50

# remove any tracks with too few detections
crab.data <- crab.data %>% few_detections(n = n)

# how much error is their in the sync/ref tag data
#vps.error <- syncref.data %>% vps_error(type = "both", which = "ref")

if (error_correction == TRUE){
  # get the error to use when predicting locations
  # first need to have the location of the ref tag(s)
  ref.tag <- vps.error$FullId %>% unique()
  
  crab.data <- crab.data %>% location_error(station.data = station.data, vps.error = vps.error)
  
  # plot the error
  #syncref.data %>% vps_error_plot(which = "ref")
  
  # matt is interested in the error data up to the third quantile (most accurate 75 % of data)
  #error.upperq <- vps.error %>% filter(HPEm < quantile(vps.error$HPEm)[4])
}

# Convert time to POSIXct
crab.data <- crab.data %>% mutate(time = as.POSIXct(time, tz = "UTC"))

if (error_correction == TRUE){
  # project utm coordinates
  llcoord <- SpatialPoints(crab.data[,4:5], proj4string = CRS("+proj=longlat +datum=WGS84"))
  utmcoord <- spTransform(llcoord, CRS("+proj=utm +zone=56 +datum=WGS84"))
} else {
  llcoord <- SpatialPoints(crab.data[,3:4], proj4string = CRS("+proj=longlat +datum=WGS84"))
  utmcoord <- spTransform(llcoord, CRS("+proj=utm +zone=56 +datum=WGS84"))
}

# add UTM locations to data frame
crab.data$x <- attr(utmcoord, "coords")[,1]
crab.data$y <- attr(utmcoord, "coords")[,2]

# randomly change starting values to help mle
retryFits <- 25

# reduce the data to what momentuhmm expects
if (error_correction == TRUE){
  obsData <- crab.data %>% dplyr::select(ID, time, lon, lat, x, y, ln.sd.x, ln.sd.y, error.corr)
} else {
  obsData <- crab.data %>% dplyr::select(ID, time, lon, lat, x, y)
}

# name for the output
name <- paste(error_name, speed_name, "crab_data.rds", sep = "_")

# save the output
save_object(crab.data, name)

# Log-scale beta parameter prior recommended in https://github.com/bmcclintock/momentuHMM/issues/24
# this should help with paths where the parameter covariance matrix was NaN
#prior <- function(theta) dnorm(theta[2], -4, 2, log = T)

if (error_correction == TRUE){
  # use crawl to predict temporally regular locations
  crwOut <- crawlWrap(obsData = obsData,
                      timeStep = "4 mins",
                      theta = c(3, 15), 
                      retryFits = retryFits,
                      fixPar = c(1, 1, NA, NA),
                      err.model = list(x = ~ln.sd.x-1, y = ~ln.sd.y-1, rho = ~error.corr))
} else {
  crwOut <- crawlWrap(obsData = obsData,
                      timeStep = "4 mins",
                      theta = c(3, 15), 
                      retryFits = retryFits,
                      fixPar = c(NA, NA))
}

# check what the warnings are
# this is just so it gets printed from katana
warnings()

# check convergence
convergence <- crwOut %>% check_convergence()

# name for the output
name <- paste(error_name, speed_name, "crab_ctcrw_convergence.rds", sep = "_")

# save the convergence file
save_object(convergence, name)

# name for the output
name <- paste(error_name, speed_name, "crab_ctcrw.rds", sep = "_")

# save the output
save_object(crwOut, name)

# prepare the data for a HMM
prepData <- prepData(crwOut)

# check the metrics described in Lawler et al., 2019
# proportional sample size (n_prop) and adjusted proportional sample size (n_adj)
# both should be ~1 to avoid over-smoothing (<1) or data replication (>1)

# n_prop tries to preserve number of locations in the track
print(n_prop(input_data = crab.data, interpolated_data = prepData))

# n_adj preserves the number of data points that enter the likelihood
print(n_adj(input_data = crab.data, interpolated_data = prepData))

# name for the output
name <- paste(error_name, speed_name, "crab_prep.rds", sep = "_")

# save the output
save_object(prepData, name)