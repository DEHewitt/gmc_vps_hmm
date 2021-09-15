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

# load data
crab.data <- readRDS("data_processed/crab_data.rds")
crwOut <- readRDS("data_processed/crabctcrw.rds")
prepData <- readRDS("data_processed/crab_prep.rds")

# load functions
source("R/find_interval.R") # function to assign detections to the interpolated interval for downstream summarising of accel - takes a couple of minutes to run
source("R/save_object.R") # wrapper for savRDS()
source("R/find_habitat.R") # intersects crab location with habitat layers
source("R/tide_height.R") # gets the tide height at the time of each detection
source("R/find_wq.R") # gets temp. and cond. at the time of each detection
source("R/salinity_converter.R") # converts conductivity to salinity

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

# find the intervals
intervals <- find_interval(irregular.times = crab.data, interpolated.times = prepData, time.step = 240) # this takes a decent bit of time

# summarise the accel data
intervals <- intervals %>%
  group_by(ID, time) %>%
  summarise(mean.accel = mean(accel),
            sd.accel = sd(accel),
            n = n()) %>%
  ungroup() %>%
  group_by(ID) %>% 
  mutate(stand.accel = mean.accel/max(mean.accel)) %>%
  ungroup()

# just get stand.accel for now
intervals.join <- intervals %>% dplyr::select(ID, time, mean.accel)

# merge with the prepData
prepData <- prepData %>% merge(intervals.join, all.x = TRUE)

# fix up duplication issue
prepData <- prepData %>%
  mutate(duplication = paste(ID, time, lat, lon)) %>%
  distinct(duplication, .keep_all = TRUE) %>%
  select(-duplication)

# previous step covariate
#prepData <- prepData %>%
# group_by(ID) %>%
#arrange(time) %>%
#mutate(prev.step = lag(step, 1)) %>%
#ungroup()

# remove first location in each track so that every prev.step row has a value
#prepData <- prepData %>% filter(!is.na(prev.step))

# ensure prepData is a momentuHMM object
#prepData <- prepData %>% as.data.frame()
class(prepData) <- append("momentuHMMData", class(prepData))