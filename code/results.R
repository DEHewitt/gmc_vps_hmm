# load the libraries
library(momentuHMM)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(patchwork)
library(ggrepel)
library(grid)
library(ggpubr)
library(sf)
library(ggspatial)
library(rgdal)
library(metR)
library(CircStats)
library(gganimate)
library(magick)

# load the data
prepData <- readRDS("data_processed/crab_prep_env_15 mins.rds")
crab.data <- readRDS("data_processed/crab_data_15 mins.rds")
nsw_coast <- st_as_sf(readOGR(dsn = "data_raw/clipped_port_stephens.shp"))
habitat <- st_as_sf(readOGR(dsn = "data_raw/taylors_beach_habitat.shp"))
station.data <- read_csv("data_raw/gmc_vps_stations.csv")
syncref.data <- read_csv("data_raw/Daniel Hewitt/VPS-TaylorsBeach-MudCrab-01-Results-20201130/results/syncref/all.csv")
oyster.leases <- st_as_sf(readOGR("data_raw/Aquaculture_Leases.shp"))

#---------- for plotting ----------#
nsw_coast <- nsw_coast %>% mutate(water = if_else(code_coast == 1000, "Soft sediment", "Land"))
#----------------------------------#

#---------- custom functions ----------#
source("R/hmm_residual_plot.R")
source("R/stationary_probs.R")
source("R/plot_state_distributions.R")
#--------------------------------------#

#---------- empty objects for loops ----------#
AIC2 <- NULL
AIC3 <- NULL
#---------------------------------------------#

#---------- list timesteps ----------#
timeStep <- seq(5, 15, 5) %>% paste("mins")

#---------- list formulas ----------#
formulas <- seq(1, 16, 1) %>% paste("formula", ., sep = "")
#-----------------------------------#

#---------- model selection ----------#
for (f in 1:length(formulas)){
  for(t in 1:length(timeStep)){
    
    # open the model
    temp2 <- readRDS(paste0("output/crab-hmm-2-state-", timeStep[t], "-", formulas[f], ".rds"))
    
    # extract fromula, AIC and nPar
    tempAIC2 <- data.frame(timeStep = timeStep[t],
                           formula = as.character(temp2$conditions$formula), 
                           AIC = AIC(temp2), 
                           nPar = length(temp2$mod$estimate),
                           formula_label = formulas[f],
                           logLik = temp2$mod$minimum)
    
    # bind it to master
    AIC2 <- AIC2 %>% bind_rows(tempAIC2)
    
  }
}

# calculate delta.aic
AIC2 <- AIC2 %>%
  group_by(timeStep) %>%
  mutate(delta.AIC = AIC-min(AIC)) %>%
  filter(formula != "~") %>% 
  ungroup()
#-------------------------------------#

#---------- load the models ----------#
## 2 state
### 5 min
m2_5 <- readRDS(file = "output/crab-hmm-2-state-5 mins-formula1.rds")

# check pseudo-residuals
plotPR(m2_5) # very bad

# prettier plots
hmm_residual_plot(m2_5)

# save the plot
ggsave(filename = paste0("figures/crab-hmm-2-state-5-min-residuals.png"),
       device = "png",  
       width = 14, 
       height = 21, 
       units = "cm", 
       dpi = 600)

### 10 min
m2_10 <- readRDS(file = "output/crab-hmm-2-state-10 mins-formula1.rds")

# check pseudo-residuals
plotPR(m2_10) # still not great

# prettier plots
hmm_residual_plot(m2_10)

# save the plot
ggsave(filename = paste0("figures/crab-hmm-2-state-10-min-residuals.png"),
       device = "png",  
       width = 14, 
       height = 21, 
       units = "cm", 
       dpi = 600)

### 15 min
m2_15 <- readRDS(file = "output/crab-hmm-2-state-15 mins-formula14.rds")

# check pseudo-residuals
plotPR(m2_15) # best looking yet

# prettier plots
hmm_residual_plot(m2_15)

# save the plot
ggsave(filename = paste0("figures/crab-hmm-2-state-15-min-residuals.png"),
       device = "png",  
       width = 14, 
       height = 21, 
       units = "cm", 
       dpi = 600)
#-------------------------------------#

#---------- activity budgets ----------#
viterbi(m2_5) %>% table()/nrow(m2_5$data) # 16%, 84%
viterbi(m2_10) %>% table()/nrow(m2_10$data) # 18%, 82%
viterbi(m2_15) %>% table()/nrow(m2_15$data) # 21%, 79%
#--------------------------------------#

#---------- model estimates ----------#
step <- m2_15$CIreal$step$est %>% 
  as_tibble() %>% 
  pivot_longer(cols = 1:2, names_to = "state") %>%
  mutate(value = round(value, 3),
         param = rep(c("mean_step", "sd_step", "zeromass_step"), each = 2))

angle <- m2_15$CIreal$angle$est %>% 
  as_tibble() %>%
  pivot_longer(cols = 1:2, names_to = "state") %>%
  mutate(value = round(value, 3),
         param = rep("conc_angle", each = 2))

accel <- m2_15$CIreal$mean.accel$est %>%
  as_tibble() %>%
  pivot_longer(cols = 1:2, names_to = "state") %>%
  mutate(value = round(value, 3),
         param = rep(c("mean_accel", "sd_accel", "zeromass_accel"), each = 2))

state_params <- bind_rows(step, angle, accel) %>%
  pivot_wider(names_from = param, values_from = value, values_fill = NA_real_)

write_csv(state_params, file = "output/crab-hmm-2-state-15-mins-state-params.csv")

# t.p.m
tpm <- m2_15$CIreal$gamma$est %>% 
  as_tibble() %>%
  mutate(across(everything(), round, 2)) %>%
  mutate(current_state = c(1, 2)) %>%
  relocate(current_state, .before = `state 1`)

# save it
write_csv(tpm, file = "output/crab-hmm-2-state-15-mins-tpm.csv")
#-------------------------------------#

#---------- state distributions ----------#
state_dist <- plot_state_distributions(m = m2_15, prepData = prepData, outliers = TRUE)

# save the plot
ggsave(filename = "figures/crab-hmm-2-state-15-min-distributions.png",
       device = "png",  
       width = 27, 
       height = 9, 
       units = "cm", 
       dpi = 600)
#-----------------------------------------#

#---------- example track + timeseries ----------#
# get out the state probabilities
stateProbs <- stateProbs(m2_15) %>% as.data.frame()

# get the decoded states
viterbi <- viterbi(m2_15)

# add it together
states <- stateProbs %>% bind_cols(state = viterbi) %>% rename(`pr(1)` = `state 1`, `pr(2)` = `state 2`)

# need to put this together with ID, time too
crabs <- prepData %>% dplyr::select(ID, crab, time, lon, lat) %>% bind_cols(states) %>% mutate(ID = as.character(ID))

# theme plotting
theme_set(theme_bw())
theme_update(panel.grid = element_blank(),
             axis.text = element_text(size = 12, colour = "black"),
             axis.title = element_text(size = 12, colour = "black"))

IDs <- unique(crabs$ID) %>% as.character()

# plot it
for(i in 1:length(IDs)){
 
  # one per crab track
  df <- crabs %>% dplyr::filter(ID == IDs[i])
  
  # set the extent for the inset
  xlim1 <- c(min(prepData$lon), max(prepData$lon))
  ylim1 <- c(min(prepData$lat), max(prepData$lat))
  
  # set the extent for the track
  #xlim2 <- c(min(df$lon), max(df$lon))
  #ylim2 <- c(min(df$lat), max(df$lat))
  
  # make an inset 
  #inset <- ggplot() +
   # geom_sf(data = nsw_coast, aes(fill = water)) +
    #geom_sf(data = habitat, aes(fill = HABITAT)) +
    #geom_rect(aes(xmin = xlim2[1], xmax = xlim2[2],
     #             ymin = ylim2[1], ymax = ylim2[2]),
      #        fill = NA,
       #       colour = "black",
        #      linetype = "dashed",
         #     size = 1) +
    #coord_sf(xlim = xlim1, ylim = ylim1) +
    #theme(axis.text = element_blank(),
     #     axis.title = element_blank(),
      #    legend.position = "none")  +
    #scale_fill_manual(breaks = c("Land", "Mangrove", "Saltmarsh", "Seagrass", "Soft sediment"),
     #                 values = c("light grey", "seagreen", "tan2", "seagreen1", "light yellow"))
  
  # plot the track
  a <- ggplot() + 
    geom_sf(data = nsw_coast, aes(fill = water)) +
    geom_sf(data = habitat, aes(fill = HABITAT)) +
    geom_point(data = df,
               aes(x = lon,
                   y = lat,
                   colour = as.factor(state)),
               alpha = 0.75) +
    geom_path(data = df,
              aes(x = lon,
                  y = lat,
                  colour = as.factor(state)),
              alpha = 0.75) +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom") +
    scale_colour_viridis_d(direction = -1) +
    guides(colour = "none", fill = guide_legend(nrow = 3, byrow = TRUE)) +
    scale_fill_manual(breaks = c("Land", "Mangrove", "Saltmarsh", "Seagrass", "Soft sediment"),
                      values = c("light grey", "seagreen", "tan2", "seagreen1", "light yellow")) +
    #coord_sf(xlim = c(xlim2[1], xlim2[2]+0.0009), ylim = ylim2) +
    coord_sf(xlim = xlim1, ylim = ylim1) +
    annotation_scale() +
    labs(fill = "Habitat")# +
    #annotation_custom(grob = ggplotGrob(inset), xmin = xlim2[2]+0.0002, xmax = xlim2[2]+0.0011, ymax = ylim2[1]+0.0009, ymin = ylim2[1])
  
  # plot the sequence of states
  b <- ggplot() +
    geom_point(data = df,
               aes(x = time,
                   y = factor(state),
               colour = as.factor(state))) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(hjust = 1),
          legend.position = "none") +
    ylab("Estimated state") +
  scale_colour_viridis_d(direction = -1)
  
  # plot probability of state 1
  c <-  ggplot() +
    geom_path(data = df,
              aes(x = time,
                  y = `pr(1)`)) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(hjust = 1)) +
    ylab("Pr(State 1)") +
    scale_y_continuous(limits = c(0, 1))
  
  # plot probability of state 2
  d <- ggplot() +
    geom_path(data = df,
              aes(x = time,
                  y = `pr(2)`)) +
    ylab("Pr(State 2)") +
    xlab("Time") +
    theme(axis.text.x = element_text(hjust = 1)) +
    scale_y_continuous(limits = c(0, 1))
  
  x <- (a|(b/c/d)) + plot_annotation(tag_levels = "a")
  
  ggsave(plot = x,
         filename = paste0("figures/", IDs[i], "-crab-hmm-2-state-15-min-example-track.jpeg"),
         height = 21,
         width = 21,
         units = "cm",
         dpi = 600)
}
#------------------------------------------------#

#---------- plot environmental effects ----------#
# get out the probabilities
stationary <- stationary_probs(m = m2_15)

stationary <- stationary %>% mutate(state = case_when(state == 1 ~ "State 1 (foraging)", state == 2 ~ "State 2 (inactive)"))

# plot delta.height
delta.height <- ggplot(data = stationary) +
  geom_ribbon(aes(x = delta.height_cov, 
                  ymin = delta.height_lci, 
                  ymax = delta.height_uci,
                  fill = state),
              alpha = 0.25) +
  geom_line(aes(x = delta.height_cov,
                y = delta.height_est,
                colour = state),
            size = 1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 12, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        legend.position = c(0.75, 0.15),
        #legend.background = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, colour = "black"),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(min(stationary$delta.height_cov), max(stationary$delta.height_cov))) +
  scale_fill_viridis_d(direction = -1) +
  scale_color_viridis_d(direction = -1) +
  xlab(bquote(paste(Delta, "-tide height (m 15-min"^-1, ")"))) +
  ylab("Stationary state probabilities")

# plot height
height <- ggplot(data = stationary)  +
  geom_ribbon(aes(x = height_cov, 
                  ymin = height_lci, 
                  ymax = height_uci,
                  fill = state),
              alpha = 0.25) +
  geom_line(aes(x = height_cov,
                y = height_est,
                colour = state),
            size = 1)  +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 12, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        legend.position = "none",
        legend.background = element_blank(),
        legend.title = element_blank()) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(min(stationary$height_cov), max(stationary$height_cov))) +
  scale_fill_viridis_d(direction = -1) +
  scale_color_viridis_d(direction = -1) +
  xlab("Tide height (m)") +
  ylab("Stationary state probabilities")

# add them together
main.effects <- (height | delta.height) + plot_annotation(tag_levels = "a")

# save the plot
ggsave(plot = main.effects,
       filename = paste0("figures/crab-hmm-2-state-15-mins-main-effects.jpeg"),
       height = 10,
       width = 20,
       units = "cm",
       dpi = 600)

# plot their interaction
cov1seq <- seq(min(m2_15$data$height), max(m2_15$data$height), 0.1)
cov2seq <- seq(min(m2_15$data$delta.height), max(m2_15$data$delta.height), 0.01)
interaction <- stationary(model = m2_15, covs = expand.grid(height = cov1seq, delta.height = cov2seq), plotCI = TRUE) %>% 
  as.data.frame() %>%
  bind_cols(expand.grid(height = cov1seq, delta.height = cov2seq)) %>%
  pivot_longer(cols = c("state.1", "state.2"), names_to = "state", values_to = "probability")

# labels for facets
interaction <- interaction %>% mutate(state = case_when(state == "state.1" ~ "State 1 (foraging)", state == "state.2" ~ "State 2 (inactive)"))

# make the plot
interaction.plot <- ggplot() + 
  geom_raster(data = interaction, 
              aes(x = height, y = delta.height, fill = probability)) + 
  facet_wrap(vars(state)) +
  scale_fill_viridis_c(limits = c(0, 1)) + 
  geom_contour(data = interaction, 
               aes(x = height, 
                   y = delta.height, 
                   z = probability),
               colour = "white",
               size = 1) +
  geom_text_contour(data = interaction, 
                    aes(x = height, 
                        y = delta.height, 
                        z = probability),
                    size = 5,
                    stroke = 0.15) +
  labs(fill = "Probability") +
  xlab("Tide height (m)") +
  ylab(bquote(paste(Delta, "-tide height (m 15-min"^-1, ")"))) +
  theme_bw() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12, colour = "black"),
        panel.grid = element_blank(),
        axis.title = element_text(size = 12, colour = "black"),
        axis.text = element_text(size = 12, colour = "black"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

# save it
ggsave(plot = interaction.plot,
       filename = paste0("figures/crab-hmm-2-state-15-mins-interaction.jpeg"),
       height = 10,
       width = 20,
       units = "cm",
       dpi = 600)

##########
##########
##########

#---------- 3 state model ----------#
# model selection
for (f in 1:length(formulas)){
  for(t in 1:length(timeStep)){
    
    # open the model
    temp3 <- readRDS(paste0("output/crab-hmm-3-state-", timeStep[t], "-", formulas[f], ".rds"))
    
    # add it to the list
    #m3[[f]] <- temp3
    
    # extract formula, AIC, and nPar
    tempAIC3 <- data.frame(timeStep = timeStep[t],
                           formula = as.character(temp3$conditions$formula), 
                           AIC = AIC(temp3), 
                           nPar = length(temp3$mod$estimate),
                           formula_label = formulas[f],
                           logLik = temp2$mod$minimum)
    
    # add it to master
    AIC3 <- AIC3 %>% bind_rows(tempAIC3)
    
  }
}

AIC3 <- AIC3 %>%
  group_by(timeStep) %>%
  mutate(delta.AIC = AIC-min(AIC)) %>%
  filter(formula != "~")

## 3 state
### 5 min
m3_5 <- readRDS(file = "output/crab-hmm-3-state-5 mins-formula1.rds")
plotPR(m3_5)
m3_5
plot(m3_5)
plotStationary(m3_5)

### 10 min
m3_10 <- readRDS(file = "output/crab-hmm-3-state-10 mins-formula1.rds")
plotPR(m3_10)
m3_10
plot(m3_10)
plotStationary(m3_10)

### 15 min
m3_15 <- readRDS(file = "output/crab-hmm-3-state-15 mins-formula1.rds")
plotPR(m3_15)
m3_15
plot(m3_15)
plotStationary(m3_15)

#---------- diagnostics ----------#
hmm_residual_plot(m3_15)

# save the plot
ggsave(filename = paste0("figures/crab-hmm-3-state-15-min-residuals.png"),
       device = "png",  
       width = 14, 
       height = 21, 
       units = "cm", 
       dpi = 600)
#---------------------------------#

#---------- activity budgets ----------#
viterbi(m3_5) %>% table()/nrow(m3_5$data) # 1 = 17%, 2 = 14%, 3 = 69%
viterbi(m3_10) %>% table()/nrow(m3_10$data) # 25%, 14%, 61%
viterbi(m3_15) %>% table()/nrow(m3_15$data) # 35%, 19%, 46%
#--------------------------------------#

#---------- state distributions ----------#
#-----------------------------------------#
state_dist <- plot_state_distributions(m = m3_15, prepData = prepData, outliers = TRUE)

# save the plot
ggsave(filename = "figures/crab-hmm-3-state-15-min-distributions.png",
       device = "png",  
       width = 20, 
       height = 20, 
       units = "cm", 
       dpi = 600)
#-----------------------------------------#

#---------- model estimates ----------#
step <- m3_15$CIreal$step$est %>% 
  as_tibble() %>% 
  pivot_longer(cols = 1:3, names_to = "state") %>%
  mutate(value = round(value, 3),
         param = rep(c("mean_step", "sd_step", "zeromass_step"), each = 3))

angle <- m3_15$CIreal$angle$est %>% 
  as_tibble() %>%
  pivot_longer(cols = 1:3, names_to = "state") %>%
  mutate(value = round(value, 3),
         param = rep("conc_angle", 3))

accel <- m3_15$CIreal$mean.accel$est %>%
  as_tibble() %>%
  pivot_longer(cols = 1:3, names_to = "state") %>%
  mutate(value = round(value, 3),
         param = rep(c("mean_accel", "sd_accel", "zeromass_accel"), each = 3))
  
state_params <- bind_rows(step, angle, accel) %>%
  pivot_wider(names_from = param, values_from = value, values_fill = NA_real_)

write_csv(state_params, file = "output/crab-hmm-3-state-15-min-model-params.csv")
#-------------------------------------#

#---------- t.p.m ----------#
tpm <- m3_15$CIreal$gamma$est %>% 
  as_tibble() %>%
  mutate(across(everything(), round, 2)) %>%
  mutate(current_state = c(1, 2, 3)) %>%
  relocate(current_state, .before = `state 1`)

write_csv(tpm, file = "output/crab-hmm-3-state-15-min-tpm.csv")
#---------------------------#

#---------- example track + timeseries ----------#
# get out the state probabilities
stateProbs <- stateProbs(m3_15) %>% as.data.frame()

# get the decoded states
viterbi <- viterbi(m3_15)

# add it together
states <- stateProbs %>% bind_cols(state = viterbi) %>% rename(`pr(1)` = `state 1`, `pr(2)` = `state 2`, `pr(3)` = `state 3`)

# need to put this together with ID, time too
crabs <- prepData %>% dplyr::select(ID, crab, time, lon, lat) %>% bind_cols(states) %>% mutate(ID = as.character(ID))

# theme plotting
theme_set(theme_bw())
theme_update(panel.grid = element_blank(),
             axis.text = element_text(size = 12, colour = "black"),
             axis.title = element_text(size = 12, colour = "black"))

IDs <- unique(crabs$ID) %>% as.character()

# plot it
for(i in 1:length(IDs)){
  
  # one per crab track
  df <- crabs %>% dplyr::filter(ID == IDs[i])
  
  # set the extent for the inset
  xlim1 <- c(min(prepData$lon), max(prepData$lon))
  ylim1 <- c(min(prepData$lat), max(prepData$lat))
  
  # set the extent for the track
  #xlim2 <- c(min(df$lon), max(df$lon))
  #ylim2 <- c(min(df$lat), max(df$lat))
  
  # make an inset 
  #inset <- ggplot() +
   # geom_sf(data = nsw_coast, aes(fill = water)) +
    #geom_sf(data = habitat, aes(fill = HABITAT)) +
    #geom_rect(aes(xmin = xlim2[1], xmax = xlim2[2],
     #             ymin = ylim2[1], ymax = ylim2[2]),
      #        fill = NA,
       #       colour = "black",
        #      linetype = "dashed",
         #     size = 1) +
    #coord_sf(xlim = xlim1, ylim = ylim1) +
    #theme(axis.text = element_blank(),
     #     axis.title = element_blank(),
      #    legend.position = "none")  +
    #scale_fill_manual(breaks = c("Land", "Mangrove", "Saltmarsh", "Seagrass", "Soft sediment"),
     #                 values = c("light grey", "seagreen", "tan2", "seagreen1", "light yellow"))
  
  # plot the track
  a <- ggplot() + 
    geom_sf(data = nsw_coast, aes(fill = water)) +
    geom_sf(data = habitat, aes(fill = HABITAT)) +
    geom_point(data = df,
               aes(x = lon,
                   y = lat,
                   colour = as.factor(state)),
               alpha = 0.75) +
    geom_path(data = df,
              aes(x = lon,
                  y = lat,
                  colour = as.factor(state)),
              alpha = 0.75) +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = "bottom") +
    scale_colour_viridis_d(direction = -1) +
    guides(colour = "none", fill = guide_legend(nrow = 3, byrow = TRUE)) +
    scale_fill_manual(breaks = c("Land", "Mangrove", "Saltmarsh", "Seagrass", "Soft sediment"),
                      values = c("light grey", "seagreen", "tan2", "seagreen1", "light yellow")) +
    #coord_sf(xlim = c(xlim2[1], xlim2[2]+0.0009), ylim = ylim2) +
    coord_sf(xlim = xlim1, ylim = ylim1) +
    labs(fill = "Habitat")# +
    #annotation_custom(grob = ggplotGrob(inset), xmin = xlim2[2]+0.0002, xmax = xlim2[2]+0.0011, ymax = ylim2[1]+0.0009, ymin = ylim2[1])
  
  # plot the sequence of states
  b <- ggplot() +
    geom_point(data = df,
               aes(x = time,
                   y = factor(state),
                   colour = as.factor(state))) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position = "none") +
    ylab("Estimated state") +
    scale_colour_viridis_d()
  
  # plot probability of state 1
  c <-  ggplot() +
    geom_path(data = df,
              aes(x = time,
                  y = `pr(1)`)) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank()) +
    ylab("Pr(State 1)") +
    scale_y_continuous(limits = c(0, 1))
  
  # plot probability of state 2
  d <- ggplot() +
    geom_path(data = df,
              aes(x = time,
                  y = `pr(2)`)) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank()) +
    ylab("Pr(State 2)") +
    xlab("Time") +
    scale_y_continuous(limits = c(0, 1))
  
  e <- ggplot() +
    geom_path(data = df,
              aes(x = time,
                  y = `pr(3)`)) +
    ylab("Pr(State 3)") +
    xlab("Time") +
    scale_y_continuous(limits = c(0, 1))
  
  x <- (a|(b/c/d/e)) + plot_annotation(tag_levels = "a")
  
  ggsave(plot = x,
         filename = paste0("figures/", IDs[i], "-crab-hmm-3-state-15-min-example-track.jpeg"),
         height = 21,
         width = 21,
         units = "cm",
         dpi = 600)
}
#------------------------------------------------#

# create a crab variable
crab.data <- crab.data %>%
  mutate(crab = str_sub(ID, 1, 13))

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
  group_by(crab, sex, cl_mm, release_date) %>%
  summarise(n.interp = n(),
            missing.accel = length(which(is.na(mean.accel))),
            distance.km = sum(step, na.rm = TRUE)/1000,
            min_temp = min(temp),
            max_temp = max(temp)) %>%
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
write_csv(summary, file = "output/summary-table.csv")


###################################################################
###################################################################
###################################################################

# habitat use table
habitat.use <- as.data.frame(table(prepData$state, prepData$habitat, prepData$oyster_edge)) %>%
  pivot_wider(names_from = Var1, values_from = Freq) %>%
  rename(habitat = Var2,
         oyster = Var3) %>% 
  #rename(`3` = `2`, `2` = `3`) %>%
  #group_by(habitat) %>% 
  mutate(total = sum(`1`, `2`, `3`)) %>%
  ungroup() %>%
  mutate(state.1.perc = round(`1`/total*100, 2),
         state.2.perc = round(`2`/total*100, 2),
         state.3.perc = round(`3`/total*100, 2))

# save table
write_csv(habitat.use, file = "output/3_state_habitat_use.csv")

# make a map
xmin <- prepData$lon %>% min()
xmax <- prepData$lon %>% max()
ymin <- prepData$lat %>% min()
ymax <- prepData$lat %>% max()

# reorder levels of state so 1 is on bottom
prepData <- prepData %>% 
  arrange(desc(state)) %>% 
  mutate(state = paste0("State ", as.character(state))) %>%
  mutate(state = case_when(state == "State 1" ~ "State 1 (inactive)",
                           state == "State 2" ~ "State 2 (inactive + error)",
                           state == "State 3" ~ "State 3 (foraging)"))

map <- ggplot() + # fix up legend position and get the purple points on top
  geom_sf(data = nsw_coast, aes(fill = ID_NAME)) +
  geom_sf(data = habitat, aes(fill = HABITAT)) +
  geom_point(data = prepData,
             aes(x = lon, y = lat,
                 colour = state)) +
  geom_sf(data = oyster.leases, fill = NA, lwd = 1, colour = "black") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, colour = "black"),
        #legend.position = c(0.8, 0.9),
        axis.title = element_blank(),
        axis.text = element_blank()) +
  scale_colour_viridis_d() +
  #scale_colour_manual(values = c("#FDE725FF", "#2A788EFF", "#440154FF")) +
  guides(colour = "legend", fill = "none") +
  #labs(colour = "State") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax)) +
  annotation_scale() +
  scale_fill_manual(values = c("dark green", "light grey", "light grey", "orange", "light green", "white"))

  
# stacked bar chart about habitats
stacked <- prepData %>%
  dplyr::select(state, habitat, oyster_edge) %>%
  dplyr::mutate(habitat = if_else(habitat == "Seagrass edge", "Seagrass", habitat)) %>%
  group_by(habitat, state) %>%
  summarise(prop = n()) %>%
  ungroup() %>% 
  mutate(prop = round(prop/sum(prop), 3))
  
bar <- ggplot() + 
  geom_col(data = stacked,
           aes(x = habitat,
               y = prop,
               fill = state),
           position = "fill",
           stat = "identity") +
  scale_fill_viridis_d() + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, colour = "black"),
        legend.position = "none",
        axis.text = element_text(size = 12, colour = "black"),
        axis.title = element_text(size = 12, colour = "black")) +
  xlab("Habitat") + 
  ylab("Proportion")

bar + map + plot_annotation(tag_levels = "a") + plot_layout()

# save plot
ggsave(filename = paste0("figures/", 3, "_state_map.png"),
       device = "png",  width = 30, height = 15, units = "cm", dpi = 600)

# animations
decoded_tracks(m = m, prepData = prepData)