tide_height <- function(data, tide.data){
  # tidying and adding the tide data
  tide.data <- tide.data %>%
    rename(date = Date, time = Time, height = `Value[m]`) %>%
    mutate(delta.height = height-lag(height, n = 1)) %>%
    mutate(date = dmy(date)) %>%
    mutate(time = hms::as_hms(time)) %>%
    mutate(time.join = ymd_hms(paste(date, time))) %>%
    dplyr::select(-`State of value`, -date, -time) %>%
    filter(time.join > min(data$time)-900) %>%
    filter(time.join < max(data$time)+300)
  
  data <- data %>%
    mutate(time.join = round_date(time, "15 mins"))
  
  data <- data %>% left_join(tide.data) %>% dplyr::select(-time.join)
  
  data <- data[complete.cases(data$height),]
}
