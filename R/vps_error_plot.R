vps_error_plot <- function(data, which){
  if (which == "all"){
    data <- data %>%
      filter(Time > min(crab.data$time) & Time < max(crab.data$time))
  } else if (which == "ref"){
    data <- data %>%
      filter(Time > min(crab.data$time) & Time < max(crab.data$time)) %>%
      filter(FullId == "A69-1602-65344")
  }
  trunc <- data %>%
    mutate(date = date(Time)) %>%
    ggplot(data = .,
           aes(x = date,
               y = HPEm,
               group = date)) +
    geom_boxplot() +
    coord_cartesian(ylim = c(0, 12)) +
    scale_y_continuous(breaks = seq(0, 12, 2)) +
    scale_x_date(breaks = "month",
                 date_labels = "%e %b") +
    theme_bw() +
    theme(axis.text = element_text(colour = "black", size = 12),
          axis.title = element_text(colour = "black", size = 12),
          panel.grid = element_blank()) +
    ylab("Horizontal position error (m)") +
    xlab("Date")
  
  full <- data %>%
    mutate(date = date(Time)) %>%
    ggplot(data = .,
           aes(x = date,
               y = HPEm,
               group = date)) +
    geom_boxplot() +
    #coord_cartesian(ylim = c(0, 15)) +
    scale_x_date(breaks = "month",
                 date_labels = "%e %b") +
    theme_bw() +
    theme(axis.text = element_text(colour = "black", size = 12),
          axis.title = element_text(colour = "black", size = 12),
          panel.grid = element_blank(),
          axis.title.y = element_blank()) +
    ylab("Horizontal position error (m)") +
    xlab("Date")
  
  x <- (trunc|full) + plot_annotation(tag_levels = "a")
  return(x)
}
