plot_state_distributions <- function(m, prepData, outliers){
  # this function takes a hmm fitted with momentuhmm (m)
  # and the data it was fit to (prepData)
  # and outputs plots of the state-dependent distributions
  # with histograms of the observations
  # if outliers == TRUE this will zoom in the step length plot to 95% of the data
  
  # how many states?
  nbStates <- length(m$stateNames) %>% as.numeric()
  
  if(outliers == TRUE){
    step.limits <- c(0, 20)
    accel.limits <- c(0, 1)
  } else {
    step.limits <- c(0, 200)
    accel.limits <- c(0, 3.1)
  }
  
  # calculate frequencies of states
  v <- viterbi(m)
  stateFreq <- table(v) / length(v)
  
  # calculate shape and scale of the gamma distributions from mean and sd
  sh <- function(mean, sd) { return(mean^2 / sd^2)}
  sc <- function(mean, sd) { return(sd^2 / mean)}
  
  # generate sequence for x axis of density functions
  x <- seq(0, max(m$data$step, na.rm = TRUE), length = 1000)
  
  ##############
  # step plots #
  ##############
  if(nbStates == 2){
    # get converged mean and sd for each state 
    step1 <- m$CIreal$step$est[1]
    sd1 <- m$CIreal$step$est[2]
    
    step2 <- m$CIreal$step$est[4] 
    sd2 <- m$CIreal$step$est[5]
    
    # get density functions of the distributions
    y_1 <- dgamma(x, shape = sh(step1, sd1),  scale = sc(step1, sd1)) 
    y_2 <- dgamma(x, shape = sh(step2, sd2),  scale = sc(step2, sd2)) 
    
    # sum densities to get total
    #y_tot <- y_1 + y_2
    
    # combine densities in a single dataframe for more convenient plotting
    df.y_1 <- data.frame(dens = y_1, state = "State 1 (foraging)", x = x)
    df.y_2 <- data.frame(dens = y_2,  state = "State 2 (inactive)", x = x)
    #df.y_tot <- data.frame(dens = y_tot,  state = "Total", x = x)
    
    cmb <- rbind(df.y_1, df.y_2) #, df.y_tot
    
    # reorder factor levels so "total" appears bottom of the legend
    cmb$state <- factor(cmb$state, levels = c("State 1 (foraging)", "State 2 (inactive)")) #, "Total"
    
    # set up some custom stuff for the plot
    linetype <- c("solid", "solid") #, "dashed"
    
    colours <- c("#FDE725FF", "#440154FF") #, "black"
  } else {
    mean1 <- m$CIreal$step$est[1]
    sd1 <- m$CIreal$step$est[2]
    
    mean2 <- m$CIreal$step$est[4] 
    sd2 <- m$CIreal$step$est[5]
    
    mean3 <- m$CIreal$step$est[7]
    sd3 <- m$CIreal$step$est[8]
    
    # get density functions of the distributions
    y_1 <- dgamma(x, shape = sh(mean1, sd1),  scale = sc(mean1, sd1)) * stateFreq[[1]] 
    y_2 <- dgamma(x, shape = sh(mean2, sd2),  scale = sc(mean2, sd2)) * stateFreq[[3]] 
    y_3 <- dgamma(x, shape = sh(mean3, sd3),  scale = sc(mean3, sd3)) * stateFreq[[2]] 
    # sum densities to get total
    #y_tot <- y_1 + y_2 + y_3
    
    # combine densities in a single dataframe for more convenient plotting
    df.y_1 <- data.frame(dens = y_1, state = "State 1 (foraging)", x = x)
    df.y_2 <- data.frame(dens = y_2, state = "State 2 (searching)", x = x)
    df.y_3 <- data.frame(dens = y_3, state = "State 3 (inactive)", x = x)
    #df.y_tot <- data.frame(dens = y_tot,  state = "Total", x = x)
    
    cmb <- rbind(df.y_1, df.y_2, df.y_3) #, df.y_tot
    
    # reorder factor levels so "total" appears bottom of the legend
    cmb$state <- factor(cmb$state, levels=c("State 1 (foraging)", "State 2 (searching)", "State 3 (inactive)")) #, "Total"
    
    # set up some custom stuff for the plot
    linetype <- c("solid", "solid", "solid") #, "dashed"
    
    colours <- c("#440154FF", "#2A788EFF", "#FDE725FF") #, "black"
  }
  
  hist <- prepData %>%
    dplyr::select(step) %>%
    mutate(step = round(step)) %>%
    group_by(step) %>% 
    summarise(n = n()) %>%
    filter(!is.na(step))
  
  # get a value to transform things by
  temp <- cmb %>% dplyr::mutate(dens = if_else(dens == Inf, 0, dens)) # remove infinite values so transformation works
  
  trans.1 <- max(hist$n, na.rm = TRUE)/max(temp$dens, na.rm = TRUE)

  # plot step distributions
  a <- ggplot() +
    geom_col(data = hist,
             aes(x = step + 1/2,
                 y = n/trans.1),
             width = 1,
             fill = "light grey") +
    geom_line(data = cmb,
              aes(x = x,
                  y = dens,
                  colour = state,
                  linetype = state), 
              size = 1) +
    scale_colour_manual(values = colours) +
    scale_linetype_manual(values = linetype) +
    #coord_cartesian(ylim = c(0, 0.15)) +
    scale_x_continuous(limits = step.limits) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "none",
          axis.title = element_text(size = 12, colour = "black"),
          axis.text = element_text(size = 12, colour = "black")) +
    ylab("Density") +
    xlab("Step length (m)") 
  
  ###############
  # accel plots #
  ###############
  
  # generate sequence for x axis of density functions
  x <- seq(0, max(m$data$mean.accel, na.rm = TRUE), length=1000)
  
  if(nbStates == 2){
    # get converged mean and sd for each state 
    mean1 <- m$CIreal$mean.accel$est[1]
    sd1 <- m$CIreal$mean.accel$est[2]
    
    mean2 <- m$CIreal$mean.accel$est[4] 
    sd2 <- m$CIreal$mean.accel$est[5]
    
    # get density functions of the distributions
    y_1 <- dgamma(x, shape = sh(mean1, sd1),  scale = sc(mean1, sd1)) 
    y_2 <- dgamma(x, shape = sh(mean2, sd2),  scale = sc(mean2, sd2)) 
    
    # sum densities to get total
    #y_tot <- y_1 + y_2
    
    # combine densities in a single dataframe for more convenient plotting
    df.y_1 <- data.frame(dens = y_1, state = "State 1 (foraging)", x = x)
    df.y_2 <- data.frame(dens = y_2,  state = "State 2 (inactive)", x = x)
    #df.y_tot <- data.frame(dens = y_tot,  state = "Total", x = x)
    
    cmb <- rbind(df.y_1, df.y_2) #, df.y_tot
    
    # reorder factor levels so "total" appears bottom of the legend
    cmb$state <- factor(cmb$state, levels = c("State 1 (foraging)", "State 2 (inactive)")) #, "Total"
    
  } else {
    mean1 <- m$CIreal$mean.accel$est[1]
    sd1 <- m$CIreal$mean.accel$est[2]
    
    mean2 <- m$CIreal$mean.accel$est[4] 
    sd2 <- m$CIreal$mean.accel$est[5]
    
    mean3 <- m$CIreal$mean.accel$est[7]
    sd3 <- m$CIreal$mean.accel$est[8]
    
    # get density functions of the distributions
    y_1 <- dgamma(x, shape = sh(mean1,sd1),  scale = sc(mean1,sd1)) * stateFreq[[1]] 
    y_2 <- dgamma(x, shape = sh(mean2,sd2),  scale = sc(mean2,sd2)) * stateFreq[[3]] 
    y_3 <- dgamma(x, shape = sh(mean3,sd3),  scale = sc(mean3,sd3)) * stateFreq[[2]] 
    # sum densities to get total
    #y_tot <- y_1 + y_2 + y_3
    
    # combine densities in a single dataframe for more convenient plotting
    df.y_1 <- data.frame(dens = y_1, state = "State 1 (foraging)", x = x)
    df.y_2 <- data.frame(dens = y_2, state = "State 2 (searching)", x = x)
    df.y_3 <- data.frame(dens = y_3, state = "State 3 (inactive)", x = x)
    #df.y_tot <- data.frame(dens = y_tot,  state = "Total", x = x)
    
    cmb <- rbind(df.y_1, df.y_2, df.y_3) #, df.y_tot
    
    # reorder factor levels so "total" appears bottom of the legend
    cmb$state <- factor(cmb$state, levels=c("State 1 (foraging)", "State 2 (searching)", "State 3 (inactive)")) #, "Total"
  }
  
  hist <- prepData %>%
    dplyr::select(mean.accel) %>%
    mutate(mean.accel = round(mean.accel, 1)) %>%
    group_by(mean.accel) %>% 
    summarise(n = n()) %>%
    filter(!is.na(mean.accel))
  
  # get rid of infinite values
  cmb <- cmb %>% dplyr::mutate(dens = if_else(dens == Inf, 0, dens))
  
  trans <- max(hist$n, na.rm = TRUE)/max(cmb$dens, na.rm = TRUE)
  
  # plot step distributions
  b <- ggplot() +
    geom_col(data = hist,
             aes(x = mean.accel + 0.1/2,
                 y = n/trans),
             width = 0.1,
             fill = "light grey") +
    geom_line(data = cmb,
              aes(x = x,
                  y = dens,
                  colour = state,
                  linetype = state), 
              size = 1) +
    scale_colour_manual(values = colours) +
    scale_linetype_manual(values = linetype) +
    scale_x_continuous(limits = accel.limits) +
    #scale_y_continuous(limits = c(0, 60)) +
    #scale_x_continuous(limits = c(0, 100)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 10, colour = "black"),
          legend.position = c(0.75, 0.9),
          legend.background = element_blank(),
          axis.title = element_text(size = 12, colour = "black"),
          axis.text = element_text(size = 12, colour = "black")) +
    ylab("Density") +
    xlab(expression(paste("Acceleration (m s"^-2, ")")))
  
  ###############
  # angle plots #
  ###############
  # generate sequence for x axis of density functions
  x <- seq(min(m$data$angle, na.rm = TRUE), max(m$data$angle, na.rm = TRUE), length=1000)
  
  if(nbStates == 2){
    # get converged mean and sd for each state 
    conc1 <- m$CIreal$angle$est[1]
    conc2 <- m$CIreal$angle$est[2]
    
    # get density functions of the distributions
    y_1 <- dwrpcauchy(x, mu = 0, rho = conc1) 
    y_2 <- dwrpcauchy(x, mu = 0, rho = conc2) 
    
    # sum densities to get total
    #y_tot <- y_1 + y_2
    
    # combine densities in a single dataframe for more convenient plotting
    df.y_1 <- data.frame(dens = y_1, state = "State 1 (foraging)", x = x)
    df.y_2 <- data.frame(dens = y_2,  state = "State 2 (inactive)", x = x)
    #df.y_tot <- data.frame(dens = y_tot,  state = "Total", x = x)
    
    cmb <- rbind(df.y_1, df.y_2) #, df.y_tot
    
    # reorder factor levels so "total" appears bottom of the legend
    cmb$state <- factor(cmb$state, levels = c("State 1 (foraging)", "State 2 (inactive)")) #, "Total"
  } else {
    conc1 <- m$CIreal$angle$est[1]
    
    conc2 <- m$CIreal$angle$est[2]
    
    conc3 <- m$CIreal$angle$est[3]
    
    # get density functions of the distributions
    y_1 <- dwrpcauchy(x, mu = 0, rho = conc1) * stateFreq[[1]]
    y_2 <- dwrpcauchy(x, mu = 0, rho = conc2) * stateFreq[[3]]
    y_3 <- dwrpcauchy(x, mu = 0, rho = conc3) * stateFreq[[2]]
    # sum densities to get total
    #y_tot <- y_1 + y_2 + y_3
    
    # combine densities in a single dataframe for more convenient plotting
    df.y_1 <- data.frame(dens = y_1, state = "State 1 (foraging)", x = x)
    df.y_2 <- data.frame(dens = y_2, state = "State 2 (searching)", x = x)
    df.y_3 <- data.frame(dens = y_3, state = "State 3 (inactive)", x = x)
    #df.y_tot <- data.frame(dens = y_tot,  state = "Total", x = x)
    
    cmb <- rbind(df.y_1, df.y_2, df.y_3) #, df.y_tot
    
    # reorder factor levels so "total" appears bottom of the legend
    cmb$state <- factor(cmb$state, levels=c("State 1 (foraging)", "State 2 (searching)", "State 3 (inactive)")) #, "Total"
  }
  
  hist <- prepData %>%
    dplyr::select(angle) %>%
    mutate(angle = round(angle, 1)) %>%
    group_by(angle) %>% 
    summarise(n = n()) %>%
    filter(!is.na(angle))
  
  # get rid of infinite values
  cmb <- cmb %>% dplyr::mutate(dens = if_else(dens == Inf, 0, dens))
  
  # get a value to transform things by
  trans.3 <- max(hist$n, na.rm = TRUE)/max(cmb$dens, na.rm = TRUE)
  
  # plot
  c <- ggplot() +
    geom_col(data = hist,
             aes(x = angle,
                 y = n/trans.3),
             width = 0.1,
             fill = "light grey") +
    geom_line(data = cmb,
              aes(x = x,
                  y = dens,
                  colour = state,
                  linetype = state), 
              size = 1) +
    scale_colour_manual(values = colours) +
    scale_linetype_manual(values = linetype) +
    #scale_y_continuous(limits = c(0, 0.021)) +
    #scale_x_continuous(limits = c(0, 100)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          legend.position = "none",
          axis.title = element_text(size = 12, colour = "black"),
          axis.text = element_text(size = 12, colour = "black"),
          axis.title.y = element_blank()) +
    ylab("Density") +
    xlab("Turning angle (radians)") +
    scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi), 
                       labels = c(expression(paste("-", pi)), 
                                  expression(paste("-", pi,"/2")), 
                                  0,
                                  expression(paste(pi, "/2")),
                                  expression(paste(pi))))
  
  plot <- a + c + b + plot_spacer() + plot_annotation(tag_levels = "a") + plot_layout(nrow = 2)
  print(plot)
  return(plot)
}
