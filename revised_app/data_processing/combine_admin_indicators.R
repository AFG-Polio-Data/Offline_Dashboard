apmis_admin_data <- list("cluster" = admin_cluster,
                         "district" = admin_district,
                         "province" = admin_province,
                         "region" = admin_region,
                         "national" = admin_campaign,
                         "cluster_completeness" = admin_cluster_completeness,
                         "district_completeness" = admin_district_completeness,
                         "province_completeness" = admin_province_completeness,
                         "region_completeness" = admin_region_completeness,
                         "national_completeness" = admin_national_completeness)

apmis_admin_data <- purrr::map(apmis_admin_data, function(x){
  x %>%
    rename(campaign_name = campaigns)
})
