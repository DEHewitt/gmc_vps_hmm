find_wq <- function(data, wq.data){
  
  wq.data <- wq.data %>%
    rename(time = date_time, temp = temp_degC, cond = calibrated_conductivity_ms_cm) %>%
    mutate(time.join = round_date(dmy_hms(time), "hour")) %>%
    dplyr::select(-latitude, -longitude) %>%
    filter(time.join > min(data$time)-3600) %>%
    filter(time.join < max(data$time)+3600) %>%
    dplyr::select(cond, temp, time.join)
  
  data <- data %>%
    mutate(time.join = round_date(time, "hour"))
  
  data <- data %>% left_join(wq.data) %>% dplyr::select(-time.join)
  
  # missing time point between 13:30-14:30 on 6/8/2020
  # just fill in the last obs - they're pretty much identical
  data <- data %>%
    fill(cond) %>%
    fill(temp)
}
