load_data <- function(){
  if (Sys.info()[6] == "Dan"){
    data <- list(raw.data = read_csv("data_raw/Daniel Hewitt/VPS-TaylorsBeach-MudCrab-01-Results-20201130/results/animal/all.csv"),
                 syncref.data = read_csv("data_raw/Daniel Hewitt/VPS-TaylorsBeach-MudCrab-01-Results-20201130/results/syncref/all.csv"),
                 station.data = read_csv("data_raw/gmc_vps_stations.csv"),
                 #nsw.coast = st_as_sf(readOGR(dsn = "data_raw/Coastaline_WGS84_MGA56_NSWOEH.shp")),
                 bio.data = read_csv("data_raw/gmc_vps_biodata.csv"),
                 wq.data = read_csv("data_raw/gmc_vps_cond_salinity_201019.csv"),
                 habitat = st_as_sf(readOGR("data_raw/taylors_beach_habitat.shp"), crs = "proj=longlat +datum=GDA94"),
                 tide.data = read_csv("data_raw/MallabulaPoint.Level1.csv", skip = 30),
                 oyster.leases = st_as_sf(readOGR("data_raw/Aquaculture_Leases.shp")))
  } else {
    data <- list(raw.data = read_csv("data/animal/all.csv"),
                 syncref.data = read_csv("data/syncref/all.csv"),
                 station.data = read_csv("data/gmc_vps_stations.csv"),
                 #nsw.coast = st_as_sf(readOGR(dsn = "data_raw/Coastaline_WGS84_MGA56_NSWOEH.shp")),
                 bio.data = read_csv("data/gmc_vps_biodata.csv"),
                 wq.data = read_csv("data/gmc_vps_cond_salinity_201019.csv"),
                 habitat = st_as_sf(readOGR("data/taylors_beach_habitat.shp"), crs = "proj=longlat +datum=GDA94"),
                 tide.data = read_csv("data/MallabulaPoint.Level1.csv", skip = 30))
  }
 
  list2env(data, .GlobalEnv)
}
