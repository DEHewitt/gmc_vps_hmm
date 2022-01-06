# load in packages
library(tidyverse)
library(momentuHMM)
library(rgdal)
library(ggplot2)
library(lubridate)
library(parallel)
library(lunar)
library(sf)
library(sfheaders)
library(lwgeom)
library(spatialEco)

# load functions
source("R/load_crawl.R") # loads output from crawl
source("R/load_data.R")
source("R/find_interval.R") # function to assign detections to the interpolated interval for downstream summarising of accel - takes a couple of minutes to run
source("R/save_object.R") # wrapper for savRDS()
source("R/find_habitat.R") # intersects crab location with habitat layers
source("R/tide_height.R") # gets the tide height at the time of each detection
source("R/find_wq.R") # gets temp. and cond. at the time of each detection
source("R/salinity_converter.R") # converts conductivity to salinity

# load crawl output
load_crawl() # need to edit this to open the right objects

# get auxilliary data (e.g., habitat, wq)
load_data()

# get sex and sizes of each crab
bio.data <- bio.data %>% select(crab = Tag_model, sex = Sex, cl_mm = CL_mm)

# get the hour of day variable
prepData <- prepData %>% mutate(hour = hour(time))

# intersect points with habitat layer
prepData <- prepData %>% find_habitat(habitat)

# get the nearest (in time) tide measurement
prepData <- prepData %>% tide_height(tide.data)

# get the nearest (in time) temperature and conductivity
prepData <- prepData %>% find_wq(wq.data)

# convert conductivity to salinity
prepData <- prepData %>% salinity_converter()

# get the lunar data
prepData <- prepData %>% mutate(lunar = lunar.phase(time, shift = 10))

# find the intervals
intervals <- find_interval(irregular.times = crab.data, interpolated.times = prepData, time.step = 900) # this takes a decent bit of time

# summarise the accel data
intervals <- intervals %>% group_by(ID, time) %>% summarise(mean.accel = mean(accel), sd.accel = sd(accel), n = n()) %>% ungroup() %>% as.data.frame()

# merge with the prepData
prepData <- prepData %>% merge(intervals, by = c("ID", "time"), all.x = TRUE) # cannot get any of the dplyr::*_join() functions to work

# get the individual id of each crab
prepData <- prepData %>% mutate(crab = str_sub(ID, 1, 13))

# get the sex of each crab
prepData <- prepData %>% left_join(bio.data)

# ensure prepData is a momentuHMM object
class(prepData) <- append("momentuHMMData", class(prepData))

#ggplot() + geom_path(data = prepData, aes(x = x, y = y, colour = ID, group = ID), size = 1) + 
 # facet_wrap(vars(sex, crab), scales = "free") + 
  #scale_color_viridis_d() +
  #theme(legend.position = "none")

# previous step covariate
#prepData <- prepData %>%
# group_by(ID) %>%
#arrange(time) %>%
#mutate(prev.step = lag(step, 1)) %>%
#ungroup()

# remove first location in each track so that every prev.step row has a value
#prepData <- prepData %>% filter(!is.na(prev.step))

name <- paste0("data_processed/crab_prep_env.rds")

# save the output
save_object(prepData, name)
