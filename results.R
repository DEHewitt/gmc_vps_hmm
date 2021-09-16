# load the libraries
library(momentuHMM)
library(tidyverse)
library(ggplot2)

# models are named alphanumerically
# prefix 'm' for model, then two 
# numbers the first of which is the
# number of states and the second
# is the number of data-streams

# load the data
m22 <- readRDS("data_processed/crab_hmm_2state_2streams.rds")
m23 <- readRDS("data_processed/crab_hmm_2state_3streams.rds")
m32 <- readRDS("data_processed/crab_hmm_3state_2streams.rds")
m33 <- readRDS("data_processed/crab_hmm_3state_3streams.rds")

# check model assumptions
plotPR(m22)
plotPR(m23)
plotPR(m32)
plotPR(m33)