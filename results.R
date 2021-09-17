# load the libraries
library(momentuHMM)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(patchwork)

# models are named alphanumerically
# prefix 'm' for model, then two 
# numbers the first of which is the
# number of states and the second
# is the number of data-streams

# load the data
prepData <- readRDS("data_processed/crab_prep_env.rds")
crab.data <- readRDS("data_processed/crab_data.rds")

# summary of tracks
# summarise the "raw" input data
input.summary <- crab.data %>%
  group_by(crab) %>%
  summarise(start = min(time), # first detection
            end = max(time), # last detection
            duration.days = as.numeric(difftime(end, start, units = "days")), # total time at liberty - not total tracked/modelled time
            n.raw = n(), # number of detections
            n.tracks = length(unique(ID)), # number of tracks
            mean.interval = mean(time.diff), # average time between detections (within a track)
            sd.interval = sd(time.diff),
            mean.accel = mean(accel, na.rm = TRUE), # average acceleration value
            sd.accel = sd(accel, na.rm = TRUE)) %>% 
  ungroup()

# summarise the interpolated data (output from crawlWrap()).
interp.summary <- prepData %>%
  group_by(crab, sex, cl_mm) %>%
  summarise(n.interp = n(),
            missing.accel = length(which(is.na(mean.accel))),
            distance.km = sum(step, na.rm = TRUE)/1000) %>%
  ungroup()

# summarise the tracks
track.summary <- prepData %>%
  group_by(crab, ID) %>%
  summarise(start = min(time), # first detection
            end = max(time), # last detection
            duration.days = as.numeric(difftime(end, start, units = "days")),
            n = n()) %>%
  ungroup() %>% 
  group_by(crab) %>%
  summarise(total.duration.track = sum(duration.days),
            mean.duration.track = mean(duration.days),
            sd.duration.track = sd(duration.days),
            mean.n.track = mean(n),
            sd.n.track = sd(n)) %>%
  ungroup()

# add it all together
summary <- input.summary %>% left_join(interp.summary) %>% left_join(track.summary)

# save the table
# some shit here about how to save tables in a nifty manner

# plot an example track (like Fig. 2 in doi.org/10.3389/fmars.2021.660122)
# find two long tracks
ggplot() + geom_path(data = prepData, aes(x = x, y = y, group = ID, colour = mean.accel), size = 1) + facet_wrap(vars(sex, crab)) + scale_color_viridis_c()

# for help picking longer ones
x <- prepData %>% group_by(crab, ID) %>% summarise(distance = sum(step, na.rm = TRUE))

# selected tracks
#example.tracks <- (c("A69-9006-7802-Track_02", "A69-9006-7802-Track_10"))

# filter the example tracks out
example.raw <- crab.data %>% filter(crab == "A69-9006-7798")
example.interp <- prepData %>% filter(crab == "A69-9006-7798")

# plot the raw points
ggplot() + geom_point(data = example.raw, aes(x = lon, y = lat, shape = ID)) # could add error bars (or ellipses) to this to indicate there is error here

# plot the interpolated points
ggplot() + # and then this plot has no error representation as in this modelling step that is what we assume
  geom_point(data = example.interp, aes(x = x, y = y, shape = ID)) + 
  geom_path(data = example.interp, aes(x = x, y = y, group = ID)) +
  geom_errorbar(data = example.interp,
                aes(x = x, y = y,
                    xmin = x - se.mu.x, xmax = x + se.mu.x,
                    ymin = y - se.mu.y, ymax = y + se.mu.y))

# plot the behaviour-estimated points
# plotting code

# put it all together with some patchwork code

# save the figure
ggsave()

# load the models
m22 <- readRDS("data_processed/crab_hmm_2state_2streams.rds")
m23 <- readRDS("data_processed/crab_hmm_2state_3streams.rds")
m32 <- readRDS("data_processed/crab_hmm_3state_2streams.rds")
m33 <- readRDS("data_processed/crab_hmm_3state_3streams.rds")

# check model assumptions
plotPR(m22)
plotPR(m23)
plotPR(m32)
plotPR(m33)

# check out the model
m22
m23
m32
m33

# look at some plots
plot(m22)
plot(m23)
plot(m32)
plot(m33)