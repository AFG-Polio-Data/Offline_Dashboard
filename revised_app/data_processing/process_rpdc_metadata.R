district_centroids <- sf::st_point_on_surface(shp_districts %>% ungroup()) %>%
  sf::st_as_sf() %>%
  mutate(CENTER_LO = st_coordinates(geometry)[,1],
         CENTER_LA = st_coordinates(geometry)[,2]) %>%
  sf::st_drop_geometry()

province_centroids <- sf::st_point_on_surface(shp_provinces %>% ungroup()) %>%
  sf::st_as_sf() %>%
  mutate(CENTER_LO = st_coordinates(geometry)[,1],
         CENTER_LA = st_coordinates(geometry)[,2]) %>%
  sf::st_drop_geometry()

region_centroids <- sf::st_point_on_surface(shp_regions %>% ungroup()) %>%
  sf::st_as_sf() %>%
  mutate(CENTER_LO = st_coordinates(geometry)[,1],
         CENTER_LA = st_coordinates(geometry)[,2]) %>%
  sf::st_drop_geometry()

cluster_centroids <- sf::st_point_on_surface(shp_clusters %>% ungroup()) %>%
  sf::st_as_sf() %>%
  mutate(CENTER_LO = st_coordinates(geometry)[,1],
         CENTER_LA = st_coordinates(geometry)[,2]) %>%
  sf::st_drop_geometry()

rpd_list <- shp_districts %>%
  sf::st_drop_geometry() %>%
  select(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE, APMIS_Region, APMIS_Province, APMIS_District) %>%
  unique()

campaign_rpdc <- df_apmis_list$campaign_rpdc %>%
  mutate(ccode = as.numeric(str_remove(as.character(ccode), as.character(dcode)))) %>%
  # mutate(ccode = ifelse(dcode == 2306, as.numeric(str_remove(as.character(ccode), as.character("2205"))), ccode)) %>%
  left_join(shp_districts %>% sf::st_drop_geometry() %>%
              select(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE, APMIS_Region, APMIS_Province, APMIS_District) %>%
              unique(),
            by=c("rcode" = "APMIS_RCODE", 
                 "pcode" = "APMIS_PCODE",
                 "dcode" = "APMIS_DCODE")) %>%
  mutate(region_name = ifelse(is.na(APMIS_Region), region_name, APMIS_Region),
         province_name = ifelse(is.na(APMIS_Province), province_name, APMIS_Province),
         district_name = ifelse(is.na(APMIS_District), district_name, APMIS_District)) %>%
  select(-c("APMIS_Region", "APMIS_Province", "APMIS_District"))

campaign_rpd <- purrr::map(df_apmis_list, function(x){
  if(all(c("campaigns", "region", "province", "district", "rcode", "pcode", "dcode") %in% colnames(x))){
    out <- x %>%
      select(campaigns, region, province, district, rcode, pcode, dcode) %>%
      unique() 
  }else{
    out <- NULL
  }
  return(out)
}) %>%
  bind_rows() %>%
  left_join(df_campaigns %>%
              select(campaign_name, campaign_startdate),
            by=c("campaigns" = "campaign_name")) %>%
  filter(campaign_startdate <= "2024-08-10" | is.na(campaign_startdate)) %>%
  bind_rows(df_apmis_list$campaign_district_pop %>%
              filter(campaign_startdate > "2024-08-10") %>%
              rename(region = region_name,
                     province = province_name,
                     district=district_name) %>%
              select(campaign_name, region, province, district, rcode, pcode, dcode, campaign_startdate) %>%
              rename(campaigns = campaign_name)) %>%
  unique() %>%
  rename(campaign_name = campaigns) %>%
  left_join(shp_districts %>% sf::st_drop_geometry() %>%
              select(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE, APMIS_Region, APMIS_Province, APMIS_District) %>%
              unique(),
            by=c("rcode" = "APMIS_RCODE", 
                 "pcode" = "APMIS_PCODE",
                 "dcode" = "APMIS_DCODE")) %>%
  mutate(region = ifelse(is.na(APMIS_Region), region, APMIS_Region),
         province = ifelse(is.na(APMIS_Province), province, APMIS_Province),
         district = ifelse(is.na(APMIS_District), district, APMIS_District)) %>%
  select(-c("APMIS_Region", "APMIS_Province", "APMIS_District"))


#Join rpd to post-campaign validation
# Function to join rpd_list and clean naming
add_admin_names <- function(df, rpd_list) {
  if (is.null(df)) return(NULL)
  
  join_cols <- c("rcode", "pcode", "dcode")
  
  # Only join if all join columns are present
  if (all(join_cols %in% colnames(df))) {
    df <- df %>%
      left_join(
        rpd_list %>%
          select(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE, APMIS_Region, APMIS_Province, APMIS_District) %>%
          distinct(),
        by = c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")
      ) %>%
      mutate(
        region   = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
        province = ifelse(!is.na(APMIS_Province), APMIS_Province, province),
        district = ifelse(!is.na(APMIS_District), APMIS_District, district)
      ) %>%
      select(-APMIS_Region, -APMIS_Province, -APMIS_District)
  }
  
  # Dynamically construct arrange columns if they exist
  arrange_cols <- c("campaigns", "region", "province", "district")
  if ("ccode" %in% colnames(df)) arrange_cols <- c(arrange_cols, "ccode")
  arrange_cols <- intersect(arrange_cols, colnames(df))  # Keep only those that exist
  
  df <- df %>% arrange(across(all_of(arrange_cols)))
  
  return(df)
}

post_campaign_data_validation <- purrr::imap(post_campaign_data_validation, function(dataset_list, dataset_name) {
  message("Processing dataset: ", dataset_name)
  purrr::imap(dataset_list, function(df, level) {
    message("  Level: ", level)
    add_admin_names(df, rpd_list)
  })
})
