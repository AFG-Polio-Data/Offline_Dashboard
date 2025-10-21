
icm_indicators <- list(
  "icm_pcts_cluster" = icm_pcts_cluster,
  "icm_pcts_district" = icm_pcts_district,
  "icm_pcts_province" = icm_pcts_province,
  "icm_pcts_region" = icm_pcts_region,
  "icm_pcts_campaign" = icm_pcts_campaign,
  "icm_pcts_day_district" = icm_pcts_day_district,
  "icm_pcts_day_province" = icm_pcts_day_province,
  "icm_pcts_day_region" = icm_pcts_day_region,
  "icm_pcts_day_campaign" = icm_pcts_day_campaign,
  "icm_household_cluster_coverage" = icm_household_cluster_coverage,
  "icm_household_district_coverage" = icm_household_district_coverage,
  "icm_household_province_coverage" = icm_household_province_coverage,
  "icm_household_region_coverage" = icm_household_region_coverage,
  "icm_household_campaign_coverage" = icm_household_campaign_coverage,
  "icm_household_cluster_day_coverage" = icm_household_cluster_day_coverage,
  "icm_household_district_day_coverage" = icm_household_district_day_coverage,
  "icm_household_province_day_coverage" = icm_household_province_day_coverage,
  "icm_household_region_day_coverage" = icm_household_region_day_coverage,
  "icm_household_campaign_day_coverage" = icm_household_campaign_day_coverage,
  "icm_cat_dist_cluster" = icm_cat_dist_cluster,
  "icm_cat_dist_district" = icm_cat_dist_district,
  "icm_cat_dist_province" = icm_cat_dist_province,
  "icm_cat_dist_region" = icm_cat_dist_region,
  "icm_cat_dist_campaign" = icm_cat_dist_campaign
)

icm_indicators <- purrr::map(icm_indicators, function(x){
  x <- x %>% as.data.frame()
  if("indicator" %in% colnames(x)){
    x <- x %>%
      mutate(indicator = as.character(indicator))
  }
  if("dcode" %in% colnames(x)){
    out <- x %>%
      left_join(rpd_list %>%
                  select(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE, APMIS_Region, APMIS_Province, APMIS_District) %>%
                  unique(),
                by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
      mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
             province = ifelse(!is.na(APMIS_Province), APMIS_Province, province),
             district = ifelse(!is.na(APMIS_District), APMIS_District, district)) %>%
      select(-c("APMIS_Region", "APMIS_Province", "APMIS_District"))
  } else{
    if("pcode" %in% colnames(x)){
      out <- x %>%
        left_join(rpd_list %>%
                    select(APMIS_RCODE, APMIS_PCODE, APMIS_Region, APMIS_Province) %>%
                    unique(),
                  by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE")) %>%
        mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
               province = ifelse(!is.na(APMIS_Province), APMIS_Province, province)) %>%
        select(-c("APMIS_Region", "APMIS_Province"))
    } else{
      if("rcode" %in% colnames(x)){
        out <- x %>%
          left_join(rpd_list %>%
                      select(APMIS_RCODE, APMIS_Region) %>%
                      unique(),
                    by=c("rcode" = "APMIS_RCODE")) %>%
          mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region)) %>%
          select(-c("APMIS_Region"))
      } else{
        out <- x
      }}}
  return(out)
})


icm_indicators <- purrr::map(icm_indicators, function(x){
  out <- x %>%
    rename(campaign_name = campaigns)
  if("age_group" %in% colnames(out)){
    out <- out %>%
      mutate(age_group = ifelse(is.na(age_group), "0-59 Months", age_group))
  }
  if("campaign_day" %in% colnames(out)){
    out <- out %>%
      mutate(campaign_day = ifelse(is.na(campaign_day), "Missing", campaign_day))
  }
  return(out)
})
