load_crawl <- function(timeStep){
  data <- list(crab.data = readRDS(paste0("data_processed/crab_data_", timeStep, ".rds")),
               crwOut = readRDS(paste0("data_processed/crab_ctcrw_", timeStep, ".rds")),
               prepData = readRDS(paste0("data_processed/crab_prep_", timeStep, ".rds")))
  
  list2env(data, .GlobalEnv)
}

