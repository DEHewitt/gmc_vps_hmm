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
library(zoo)
library(spatialEco)
library(imputeTS)

# load functions
source("R/load_crawl.R") # loads output from crawl
source("R/load_data.R")
source("R/find_interval.R") # function to assign detections to the interpolated interval for downstream summarising of accel - takes a couple of minutes to run
source("R/save_object.R") # wrapper for savRDS()
source("R/find_habitat.R") # intersects crab location with habitat layers
source("R/tide_height.R") # gets the tide height at the time of each detection
source("R/find_wq.R") # gets temp. and cond. at the time of each detection
source("R/salinity_converter.R") # converts conductivity to salinity
source("R/find_seagrass_edge.R")
source("R/find_oyster_leases.R")

# set the timeStep to dictate which output you're working with
timeStep <- c(10, 15) %>% paste0(" mins")

for (i in 1:length(timeStep)){
  # load crawl output
  load_crawl(timeStep = timeStep[i]) # need to edit this to open the right objects
  
  # get auxilliary data (e.g., habitat, wq)
  load_data()
  
  # get sex and sizes of each crab
  bio.data <- bio.data %>% dplyr::select(crab = Tag_model, sex = Sex, cl_mm = CL_mm, release_date = Release_date) %>% dplyr::mutate(release_date = dmy(release_date))
  
  # get the hour of day variable
  prepData <- prepData %>% mutate(hour = hour(time))
  
  # intersect points with habitat layer
  prepData <- prepData %>% find_habitat(habitat = habitat)
  
  # find the points that are along the seagrass edge (within 1m; distance is in decimal degrees)
  prepData <- prepData %>% find_seagrass_edge(habitat = habitat, distance = 0.0000126)
  
  prepData <- prepData %>% 
    mutate(habitat = if_else(habitat == "Seagrass edge", "Seagrass", habitat)) %>%
    mutate(habitat = factor(habitat, levels = c("Soft sediment", "Seagrass", "Mangrove", "Saltmarsh")))
  
  # find the points that are under oyster leases
  prepData <- prepData %>% find_oyster_leases(oyster.leases = oyster.leases, distance = 0.0000126)
  
  # get the nearest (in time) tide measurement
  prepData <- prepData %>% tide_height(tide.data)
  
  # get the nearest (in time) temperature and conductivity
  prepData <- prepData %>% find_wq(wq.data)
  
  # set time.step argument so you're calculating the right intervals
  time.step <- timeStep %>% str_sub(1, 2) %>% as.numeric()*60
  
  # find the intervals
  intervals <- find_interval(irregular.times = crab.data, interpolated.times = prepData, time.step = time.step) # this takes a decent bit of time
  
  # summarise the accel data
  intervals <- intervals %>% 
    group_by(ID, time) %>% 
    summarise(mean.accel = mean(accel), sd.accel = sd(accel), n = n()) %>% 
    ungroup()
  
  # merge with the prepData
  prepData <- prepData %>% merge(intervals, by = c("ID", "time"), all.x = TRUE) # cannot get any of the dplyr::*_join() functions to work
  
  # print percentage missing values
  length(which(is.na(prepData$mean.accel)))/nrow(prepData)
  
  # get the individual id of each crab
  prepData <- prepData %>% mutate(crab = str_sub(ID, 1, 13))
  
  # get the sex of each crab
  prepData <- prepData %>% left_join(bio.data)
  
  # release dates
  releases <- prepData %>% 
    group_by(crab, release_date) %>% 
    summarise(first_detection = min(time)) %>%
    filter(date(first_detection) == release_date)
  
  # how many crabs detected on first day of tracking?
  print(paste0("For timeStep = ", timeStep[i], ", ",
        length(which(releases$release_date == date(releases$first_detection))),
        " crabs out of ",
        length(unique(prepData$crab)),
        " were detected on their first day of tracking"))
  
  a <- prepData %>% 
    filter(crab %in% unique(releases$crab)) %>%
    group_by(ID) %>%
    arrange(time) %>%
    mutate(index = row_number()) %>%
    mutate(track = cur_group_id()) %>%
    ungroup() %>%
    group_by(crab) %>%
    mutate(track1 = if_else(track == min(track), "first", "not first")) %>% 
    ungroup() %>%
    mutate(track = factor(track, levels = unique(track))) %>%
    mutate(track = factor(track, levels = rev(levels(track)))) %>%
  ggplot(data = .,
         aes(x = index,
             y = mean.accel)) +
    geom_path(aes(colour = track1, group = track)) +
    facet_wrap(vars(crab), scales = "free_x") +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 12, colour = "black"),
          axis.text = element_text(size = 12, colour = "black"),
          strip.background = element_blank(),
          strip.text = element_text(size = 12, colour = "black"),
          legend.position = "none") +
    xlab("Observation index") +
    scale_colour_manual(values = c("black", "light grey")) +
    ylab(expression(paste("Acceleration (m s"^-2, ")")))# +
    #scale_x_datetime(date_breaks = "1 month", date_labels = "%b")
  
  ggsave(paste0("figures/", timeStep[i], "-first-day-activity.png"),
         plot = a,
         width = 16,
         height = 12,
         units = "cm")
  
  # remove data from winter
  # seasonal fishery and only two crabs (1 track from 1, and 11 from the other) detected during those months
  seasonal <- prepData %>%
    group_by(crab, lubridate::month(time)) %>%
    summarise(n = n(), n_tracks = n_distinct(ID))
  
  write_csv(x = seasonal,
            file = paste0("output/", timeStep[i], "-seasonal-tracks.csv"))
  
  # ensure prepData is a momentuHMM object
  class(prepData) <- append("momentuHMMData", class(prepData))
  
  # name for prepData
  name <- paste0("data_processed/crab_prep_env_", timeStep[i], ".rds")
  
  # save prepData
  save_object(prepData, name)
}
