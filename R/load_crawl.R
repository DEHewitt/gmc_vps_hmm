load_crawl <- function(){
  data <- list(crab.data = readRDS(paste0("data_processed/crab_data.rds")),
               crwOut = readRDS(paste0("data_processed/crab_ctcrw.rds")),
               prepData = readRDS(paste0("data_processed/crab_prep.rds")))
  
  list2env(data, .GlobalEnv)
}

