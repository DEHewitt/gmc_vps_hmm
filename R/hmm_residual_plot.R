hmm_residual_plot <- function(m){
  # this function takes a fitted momentuhmm model
  # and outputs a qq-plot and acf plot of the residuals using ggplot
  
  # extract the residuals
  res <- pseudoRes(m) %>% as.data.frame()
  
  # blank string of letters so all panels align
  blank <- "blank"
  
  # set up themes for plots row-wise
  top_theme <- theme(axis.title = element_text(colour = "white", size = 12),
                     axis.text = element_text(colour = "black", size = 12),
                     panel.grid = element_blank(),
                     legend.position = "none")
  
  middle_theme <- theme(axis.title.y = element_text(colour = "black", size = 12),
                        axis.text = element_text(colour = "black", size = 12),
                        panel.grid = element_blank(),
                        axis.title.x = element_text(size = 12, colour = "white"),
                        legend.position = "none")
  
  bottom_theme <- theme(axis.title.y = element_text(colour = "white", size = 12),
                        axis.text = element_text(colour = "black", size = 12),
                        panel.grid = element_blank(),
                        axis.title.x = element_text(size = 12, colour = "black"),
                        legend.position = "none")
  
  # plot with zero-step length residuals
  step <- ggplot(data = res, aes(sample = stepRes)) + 
    geom_qq_line(aes(colour = "red"))  +
    geom_qq() +
    theme_bw() +
    top_theme +
    xlab(blank) +
    ylab(blank)
  
  # remove zeroes
  #non.zero <- which(prepData$step > 0)
  #res1 <- res[c(non.zero),]
  
  # plot without zero steps - not sure this one is going to be needed
  #step1 <- ggplot(data = res1,
   #               aes(sample = stepRes)) + 
    #geom_qq_line(aes(colour = "red"))  +
    #geom_qq() +
    #theme_bw() +
    #theme(axis.title = element_text(colour = "black", size = 12),
     #     axis.text = element_text(colour = "black", size = 12),
      #    panel.grid = element_blank(),
       #   legend.position = "none") +
    #xlab("Theoretical quantiles") +
    #ylab("Sample quantiles")
  
  # acf plot
  step_acf <- res$stepRes
  step_acf <- step_acf[complete.cases(step_acf)]
  step_acf <- acf(step_acf, lag.max = 100, plot = FALSE)
  step_acf <- data.frame(acf = step_acf$acf, lag = step_acf$lag)
  
  step_acf_plot <- ggplot(data = step_acf, aes(x = lag, y = acf)) +
    geom_bar(stat = "identity", position = "identity") +
    theme_bw() +
    top_theme +
    xlab(blank) +
    ylab(blank) +
    scale_x_continuous(breaks = c(0, 24, 48, 72, 96),
                       labels = c(0, 6, 12, 18, 24))
  
  # angle qq plot
  angle <- ggplot(data = res,
                  aes(sample = angleRes)) + 
    geom_qq_line(aes(colour = "red"))  +
    geom_qq() +
    theme_bw() +
    middle_theme +
    xlab(blank) +
    ylab("Sample quantiles")
  
  # angle acf plot
  angle_acf <- res$angleRes
  angle_acf <- angle_acf[complete.cases(angle_acf)]
  angle_acf <- acf(angle_acf, lag.max = 100, plot = FALSE)
  angle_acf <- data.frame(acf = angle_acf$acf, lag = angle_acf$lag)
  
  angle_acf_plot <- ggplot(data = angle_acf, aes(x = lag, y = acf)) +
    geom_bar(stat = "identity",
             position = "identity") +
    theme_bw() +
    middle_theme +
    xlab(blank) +
    ylab("ACF") +
    scale_x_continuous(breaks = c(0, 24, 48, 72, 96),
                       labels = c(0, 6, 12, 18, 24))
  
  # plot with zero-accel length residuals
  accel <- ggplot(data = res, aes(sample = mean.accelRes)) + 
    geom_qq_line(aes(colour = "red"))  +
    geom_qq() +
    theme_bw() +
    bottom_theme +
    xlab("Theoretical quantiles") +
    ylab(blank)
  
  # remove zeroes
  #non.zero <- which(prepData$mean.accel > 0)
  #res1 <- res[c(non.zero),]
  
  # plot without zero accels - not sure this one is going to be needed
  #accel1 <- ggplot(data = res1,
   #                aes(sample = mean.accelRes)) + 
    #geom_qq_line(aes(colour = "red"))  +
    #geom_qq() +
    #theme_bw() +
    #theme(axis.title = element_text(colour = "black", size = 12),
     #     axis.text = element_text(colour = "black", size = 12),
      #    panel.grid = element_blank(),
       #   legend.position = "none") +
    #xlab("Theoretical quantiles") +
    #ylab(" ")
  
  # acf plot
  accel_acf <- res$mean.accelRes
  accel_acf <- accel_acf[complete.cases(accel_acf)]
  accel_acf <- acf(accel_acf, lag.max = 100, plot = FALSE)
  accel_acf <- data.frame(acf = accel_acf$acf, lag = accel_acf$lag)
  
  accel_acf_plot <- ggplot(data = accel_acf, aes(x = lag, y = acf)) +
    geom_bar(stat = "identity", position = "identity") +
    theme_bw() +
    bottom_theme +
    xlab("Lag (hours)") +
    ylab(blank) +
    scale_x_continuous(breaks = c(0, 24, 48, 72, 96),
                       labels = c(0, 6, 12, 18, 24))
  
  plot <- (step|step_acf_plot)/(angle|angle_acf_plot)/(accel|accel_acf_plot) + plot_annotation(tag_levels = "a")
  
  print(plot)
  return(plot)
}
