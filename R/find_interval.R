find_interval <- function(irregular.times, interpolated.times, time.step){
  # irregular.times is a df with ID, time and (in this case) accel. values
  # interpolated.times is a df that includes the ID (that corresponds to the ID in irregular.times) and the interpolated times
  # time.step is numeric and correspond to the interpolation interval (in seconds)
  
  # this function takes these two data frames and returns one with each time point allocated to an interval
  # this is for downstream summary of acceleration values into means for each interval
  
  # get each unique ID
  IDs <- unique(irregular.times$ID)
  
  # an empty df for results of each iteration
  output.1 <- data.frame()
  
  # loop through each crab
  for(i in 1:length(IDs)){
    
    # just give me the ID, times and any additional info I want to know about
    x <- irregular.times %>% 
      dplyr::select(ID, time, accel) %>%
      filter(ID == IDs[i])
    
    # just get the IDs and times to set and define a set of intervals for each crab
    y <- interpolated.times %>% 
      dplyr::select(ID, time) %>%
      group_by(ID) %>%
      arrange(time) %>%
      mutate(interval = if_else(time == min(time), interval(time-time.step, time), interval(lag(time), time))) %>%
      ungroup() %>% 
      select(ID, interval) %>%
      filter(ID == IDs[i])
    
    # get every unique time - we need to run this process for every time point
    times <- unique(x$time)
    
    # an empty data frame for this loop
    output.2 <- data.frame()
    
    
    for (i in 1:length(times)){
      # find the index of the interval that the time point is in
      j <- which(x$time[i] %within% y$interval)
      
      # extract the upper boundary of the interval and convert it to a date object
      k <- str_sub(y$interval[j[1]], 26, 44)
      
      # store the results of this iteration
      temp <- data.frame(time = x$time[i], 
                         interval = ymd_hms(k), 
                         accel = x$accel[i], 
                         ID = x$ID[i])
      
      # add them to the previous results
      output.2 <- bind_rows(output.2, temp)
      
      # for the max times
      #end <- length(intervals$interval)
      #max.int <- str_sub(intervals$interval[end], 26, 44)
      
      # get rid of the last point which will always be outside an interval
      output.2 <- output.2 %>% filter(!is.na(interval))
    }
    output.1 <- bind_rows(output.1, output.2)
  }
  output.1 <- output.1 %>%
    dplyr::select(ID, interval, accel) %>%
    rename(time = interval)
}