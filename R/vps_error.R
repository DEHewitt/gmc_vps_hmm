vps_error <- function(data, type, which, time){
  # in this function type refers to the type of 
  # summary statistic we want: mean (+/- sd) or 
  # median or both
  # which refers to which tags with know locations 
  # that we want to use and can be either "all" 
  # which includes sync tags and ref tags or "ref" which only includes ref tags
  # time is the df that has the time limits (i.e. beginning and end of study)
  if (which == "both"){
    data <- data %>%
      filter(Time > min(time$time) & Time < max(time$time)) %>%
      filter(FullId == "A69-1602-65344" | FullId == "A69-1602-65345")
  } else if (which == "longest"){
    data <- data %>%
      filter(Time > min(time$time) & Time < max(time$time)) %>%
      filter(FullId == "A69-1602-65344")
  }
  
  if (type == "mean"){
    x <- data$HPEm %>% mean()  %>% round(digits = 2)
    y <- data$HPEm %>% sd()  %>% round(digits = 2)
    xy <- paste0("mean = ", x, " m (+/- ", y, ")")
          
  } else if (type == "median"){
    x <- data$HPEm %>% median()  %>% round(digits = 2)
    y <- data$HPEm %>% IQR() %>% round(digits = 2)
    xy <- paste0("median = ", x, " m, IQR = ", y, " m")
    
  } else if (type == "both"){
    x <- data$HPEm %>% mean() %>% round(digits = 2)
    y <- data$HPEm %>% sd()  %>% round(digits = 2)
    z <- data$HPEm %>% median()  %>% round(digits = 2)
    a <- data$HPEm %>% IQR() %>% round(digits = 2)
    xy <- paste0("mean = ", x, " m (+/- ", y, "); median = ", z, " m, IQR = ", a, " m")
  }
  print(xy)
  data
}
