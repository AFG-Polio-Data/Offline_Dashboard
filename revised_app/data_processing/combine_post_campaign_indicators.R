options(scipen = 999)
indicators_cluster <- bind_rows(
  ind_pca_fm_cov_0_11m_cluster,
  ind_pca_fm_cov_12_59m_cluster,
  ind_pca_fm_cov_0_59m_cluster,
  ind_pca_fm_cov_female_cluster,
  ind_pca_fm_cov_male_cluster,
  ind_pca_recall_cov_0_59m_cluster,
  ind_pca_fm_cov_fipv_cluster %>%
    mutate(label = as.character(label)),
  ind_pca_awareness_yn_cluster,
  ind_pca_awarenss_source_cluster,
  ind_pca_door_marking_cluster,
  ind_pca_reasons_missed_cluster,
  ind_pca_completeness_cluster,
  ind_pca_hrmp_cov_cluster,
  ind_pca_hrmp_pct_houses_cluster,
  ind_pca_reasons_missed_rates_cluster) %>%
  mutate(value = as.character(value)) %>%
  bind_rows(ind_modality_cluster) %>%
  mutate(value = as.character(value)) %>%
  # select(-c("region", "province", "district")) %>%
  left_join(rpd_list, by=c("rcode" = "APMIS_RCODE",
                           "pcode" = "APMIS_PCODE",
                           "dcode" = "APMIS_DCODE")) %>%
  mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
         province = ifelse(!is.na(APMIS_Province), APMIS_Province, province),
         district = ifelse(!is.na(APMIS_District), APMIS_District, district))

indicators_district <- bind_rows(
  ind_pca_fm_cov_0_11m_district,
  ind_pca_fm_cov_12_59m_district,
  ind_pca_fm_cov_0_59m_district,
  ind_pca_recall_cov_0_59m_district,
  ind_pca_fm_cov_fipv_district %>%
    mutate(label = as.character(label)),
  ind_pca_fm_cov_female_district,
  ind_pca_fm_cov_male_district,
  ind_pca_awareness_yn_district,
  ind_pca_awarenss_source_district,
  ind_pca_door_marking_district,
  ind_pca_reasons_missed_district,
  ind_pca_completeness_district,
  ind_pca_hrmp_cov_district,
  ind_pca_hrmp_pct_houses_district,
  ind_lqas_reasons_missed_district,
  ind_ooh_fm_coverage_district,
  ind_ooh_reasons_missed_district,
  ind_ooh_completeness_district,
  ind_pca_pct_clusters_lt95_fm_cov_district,
  ind_pca_reasons_missed_rates_district
) %>%
  mutate(value = as.character(value)) %>%
  bind_rows(ind_lqas_result_district,
            ind_lqas_result_fipv_district,
            ind_modality_district) %>%
  mutate(value = as.character(value)) %>%
  # select(-c("region", "province", "district")) %>%
  left_join(rpd_list, by=c("rcode" = "APMIS_RCODE",
                           "pcode" = "APMIS_PCODE",
                           "dcode" = "APMIS_DCODE")) %>%
  mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
         province = ifelse(!is.na(APMIS_Province), APMIS_Province, province),
         district = ifelse(!is.na(APMIS_District), APMIS_District, district))

indicators_province <- bind_rows(
  ind_pca_fm_cov_0_11m_province,
  ind_pca_fm_cov_12_59m_province,
  ind_pca_fm_cov_0_59m_province,
  ind_pca_recall_cov_0_59m_province,
  ind_pca_fm_cov_female_province,
  ind_pca_fm_cov_male_province,
  ind_pca_fm_cov_fipv_province %>%
    mutate(label=as.character(label)),
  ind_pca_awareness_yn_province,
  ind_pca_awarenss_source_province,
  ind_pca_door_marking_province,
  ind_pca_reasons_missed_province,
  ind_pca_completeness_province,
  ind_pca_hrmp_cov_province,
  ind_pca_hrmp_pct_houses_province,
  ind_lqas_reasons_missed_province,
  ind_ooh_fm_coverage_province,
  ind_ooh_reasons_missed_province,
  ind_ooh_completeness_province,
  ind_lqas_result_province %>%
    mutate(label = as.character(label)),
  ind_lqas_result_fipv_province %>%
    mutate(label = as.character(label)),
  ind_pca_pct_clusters_lt95_fm_cov_province,
  ind_pca_reasons_missed_rates_province)  %>%
  mutate(value = as.character(value)) %>%
  bind_rows(ind_modality_province) %>%
  # select(-c("region", "province")) %>%
  left_join(rpd_list %>%
              select(APMIS_Region, APMIS_Province, APMIS_RCODE, APMIS_PCODE) %>%
              unique(), 
            by=c("rcode" = "APMIS_RCODE",
                 "pcode" = "APMIS_PCODE")) %>%
  mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
         province = ifelse(!is.na(APMIS_Province), APMIS_Province, province))

indicators_region <- bind_rows(
  ind_pca_fm_cov_0_11m_region,
  ind_pca_fm_cov_12_59m_region,
  ind_pca_fm_cov_0_59m_region,
  ind_pca_fm_cov_female_region,
  ind_pca_fm_cov_male_region,
  ind_pca_recall_cov_0_59m_region,
  ind_pca_fm_cov_fipv_region %>%
    mutate(label = as.character(label)),
  ind_pca_awareness_yn_region,
  ind_pca_awarenss_source_region,
  ind_pca_door_marking_region,
  ind_pca_reasons_missed_region,
  ind_pca_completeness_region,
  ind_pca_hrmp_cov_region,
  ind_pca_hrmp_pct_houses_region,
  ind_lqas_reasons_missed_region,
  ind_ooh_fm_coverage_region,
  ind_ooh_reasons_missed_region,
  ind_ooh_completeness_region,
  ind_lqas_result_region,
  ind_lqas_result_fipv_region %>%
    mutate(label = as.character(label)),
  ind_pca_pct_clusters_lt95_fm_cov_region,
  ind_pca_reasons_missed_rates_region
) %>%
  mutate(value = as.character(value)) %>%
  bind_rows(ind_modality_region) %>%
  # select(-c("region")) %>%
  left_join(rpd_list %>%
              select(APMIS_Region, APMIS_RCODE) %>%
              unique(), 
            by=c("rcode" = "APMIS_RCODE")) %>%
  mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region))

indicators_national <- bind_rows(
  ind_pca_fm_cov_0_11m_national,
  ind_pca_fm_cov_12_59m_national,
  ind_pca_fm_cov_0_59m_national,
  ind_pca_fm_cov_female_national,
  ind_pca_fm_cov_male_national,
  ind_pca_recall_cov_0_59m_national,
  ind_pca_fm_cov_fipv_national %>%
    mutate(label = as.character(label)),
  ind_pca_awareness_yn_national,
  ind_pca_awarenss_source_national,
  ind_pca_door_marking_national,
  ind_pca_reasons_missed_national,
  ind_pca_completeness_national,
  ind_pca_hrmp_cov_national,
  ind_pca_hrmp_pct_houses_national,
  ind_lqas_reasons_missed_national,
  ind_ooh_fm_coverage_national,
  ind_ooh_reasons_missed_national,
  ind_ooh_completeness_national,
  ind_lqas_result_national,
  ind_lqas_result_fipv_national %>%
    mutate(label = as.character(label)),
  ind_pca_pct_clusters_lt95_fm_cov_national,
  ind_pca_reasons_missed_rates_national) %>%
  mutate(value = as.character(value)) %>%
  bind_rows(ind_modality_national)


apmis_indicators <- list("cluster_indicators" = indicators_cluster,
                         "district_indicators" = indicators_district,
                         "province_indicators" = indicators_province,
                         "region_indicators" = indicators_region,
                         "national_indicators" = indicators_national)


indicator_types <- purrr::map(apmis_indicators, function(x){
  out <- x %>%
    select(indicator) %>%
    unique()
  return(out)
}) %>%
  bind_rows() %>%
  unique() %>%
  mutate(indicator_type = case_when(grepl("coverage", indicator) ~ "pct",
                                    grepl("hrmp_pct_of_houses", indicator) ~ "pct",
                                    indicator %in% c("awareness_yn", "pct_districts_pass", "pct_clusters_lt95_fm_cov", "completeness") ~ "pct",
                                    indicator %in% c("door_marking", "awareness_source") ~ "cat_dist",
                                    grepl("reasons_missed_rates", indicator) ~ "rate",
                                    grepl("reasons", indicator) ~ "cat_dist",
                                    indicator %in% c("result", "modality", "fipv_result") ~ "cat",
                                    TRUE ~ NA_character_))

apmis_indicators <- purrr::map(apmis_indicators, function(x){
  x %>%
    left_join(indicator_types, by=c("indicator"))
})

apmis_indicators <- purrr::map(apmis_indicators, function(x){
  x %>%
    mutate(indicator = paste0(data_source, "_", indicator))
})

