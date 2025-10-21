shp_districts <- read_rds("data/Shapefiles/apmis_districts.Rds")  %>%
  st_as_sf()
shp_provinces <- read_rds("data/Shapefiles/apmis_provinces.Rds")  %>%
  st_as_sf()
shp_regions <- read_rds("data/Shapefiles/apmis_regions.Rds") %>%
  st_as_sf()

shp_clusters <- read_rds("data/Shapefiles/apmis_clusters.Rds") %>%
  st_as_sf()

df_apmis_list <- read_rds("data/clean_apmis_data.Rds")
