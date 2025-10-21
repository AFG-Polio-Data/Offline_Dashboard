icm_household_summary_cluster <- all_data_list$icm_indicators$icm_pcts_cluster %>%
  filter(form_type == "Household Monitoring" & age_group == "0-59 Months") %>%
  filter(!is.na(region) & !is.na(province) & !is.na(district) & !is.na(clustername)) %>%
  mutate(label = paste0(round(pct, 2)*100,"% (", numerator,"/",denominator,")")) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, indicator, label) %>%
  unique() %>%
  pivot_wider(names_from = indicator,
              values_from = label) %>%
  full_join(all_data_list$icm_indicators$icm_household_cluster_coverage %>%
              filter(form_type == "Household Monitoring" & age_group == "0-59 Months") %>%
              filter(!is.na(region) & !is.na(province) & !is.na(district) & !is.na(clustername)) %>%
              mutate(label = paste0(round(pct, 2)*100,"% (", numerator,"/",denominator,")")) %>%
              select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, indicator, label) %>%
              pivot_wider(names_from = indicator,
                          values_from = label),
            by=c("campaign_name", "region", "rcode", "province", "pcode", "district", "dcode", "clustername", "ccode")) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, `H2H: Coverage, recall`, `H2H: Coverage, finger-marked`, `H2H: Missed Area`, `H2H: Poorly Covered Area`) %>%
  arrange(campaign_name, region, province, district, ccode)

colnames(icm_household_summary_cluster) <- gsub("^H2H: ", "", colnames(icm_household_summary_cluster))

icm_household_summary_district <- all_data_list$icm_indicators$icm_pcts_district %>%
  filter(form_type == "Household Monitoring" & age_group == "0-59 Months") %>%
  filter(!is.na(region) & !is.na(province) & !is.na(district) ) %>%
  mutate(label = paste0(round(pct, 2)*100,"% (", numerator,"/",denominator,")")) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, indicator, label) %>%
  unique() %>%
  pivot_wider(names_from = indicator,
              values_from = label) %>%
  full_join(all_data_list$icm_indicators$icm_household_district_coverage %>%
              filter(form_type == "Household Monitoring" & age_group == "0-59 Months") %>%
              filter(!is.na(region) & !is.na(province) & !is.na(district)) %>%
              mutate(label = paste0(round(pct, 2)*100,"% (", numerator,"/",denominator,")")) %>%
              select(campaign_name, region, rcode, province, pcode, district, dcode, indicator, label) %>%
              pivot_wider(names_from = indicator,
                          values_from = label),
            by=c("campaign_name", "region", "rcode", "province", "pcode", "district", "dcode")) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, `H2H: Coverage, recall`, `H2H: Coverage, finger-marked`, `H2H: Missed Area`, `H2H: Poorly Covered Area`) %>%
  arrange(campaign_name, region, province, district)

colnames(icm_household_summary_district) <- gsub("^H2H: ", "", colnames(icm_household_summary_district))

icm_supervisor_monitoring_h2h_cluster <- all_data_list$icm_indicators$icm_pcts_cluster %>%
  filter(form_type == "Supervisor Monitoring" & age_group == "0-59 Months") %>%
  filter(!is.na(region) & !is.na(province) & !is.na(district) & !is.na(clustername)) %>%
  mutate(
    label = paste0(round(pct, 2) * 100, "% (", numerator, "/", denominator, ")"),
    indicator_num = as.numeric(str_extract(indicator, "^[0-9]+"))
  ) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, indicator, label, indicator_num) %>%
  arrange(indicator_num, indicator) %>%
  select(-indicator_num) %>%
  unique() %>%
  pivot_wider(names_from = indicator,
              values_from = label) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything()) %>%
  arrange(campaign_name, region, province, district)

colnames(icm_supervisor_monitoring_h2h_cluster) <- gsub("^H2H: ", "", colnames(icm_supervisor_monitoring_h2h_cluster))

icm_supervisor_monitoring_h2h_district <- all_data_list$icm_indicators$icm_pcts_district %>%
  filter(form_type == "Supervisor Monitoring" & age_group == "0-59 Months") %>%
  # filter(grepl("H2H:", indicator)) %>%
  filter(!is.na(region) & !is.na(province) & !is.na(district)) %>%
  mutate(label = paste0(round(pct, 2)*100,"% (", numerator,"/",denominator,")"),
         indicator_num = as.numeric(str_extract(indicator, "^[0-9]+"))) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, indicator, label, indicator_num) %>%
  arrange(indicator_num, indicator) %>%
  select(-indicator_num) %>%
  unique() %>%
  pivot_wider(names_from = indicator,
              values_from = label) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, everything()) %>%
  arrange(campaign_name, region, province, district)

colnames(icm_supervisor_monitoring_h2h_cluster) <- gsub("^H2H: ", "", colnames(icm_supervisor_monitoring_h2h_cluster))

icm_team_monitoring_s2s_cluster <- all_data_list$icm_indicators$icm_pcts_cluster %>%
  filter(form_type == "Team Monitoring" & age_group == "0-59 Months") %>%
  filter(grepl("M2M/S2S:", indicator) | grepl("^[0-9]+\\)", indicator)) %>%
  filter(!is.na(region) & !is.na(province) & !is.na(district) & !is.na(clustername)) %>%
  mutate(label = paste0(round(pct, 2)*100,"% (", numerator,"/",denominator,")"),
         indicator_num = as.numeric(str_extract(indicator, "^[0-9]+"))) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, indicator, label, indicator_num) %>%
  arrange(indicator_num, indicator) %>%
  select(-indicator_num) %>%
  unique() %>%
  pivot_wider(names_from = indicator,
              values_from = label) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything()) %>%
  arrange(campaign_name, region, province, district, ccode)

colnames(icm_team_monitoring_s2s_cluster) <- gsub("^M2M/S2S: ", "", colnames(icm_team_monitoring_s2s_cluster))

icm_team_monitoring_h2h_cluster <- all_data_list$icm_indicators$icm_pcts_cluster %>%
  filter(form_type == "Team Monitoring" & age_group == "0-59 Months") %>%
  filter(grepl("H2H:", indicator)) %>%
  filter(!is.na(region) & !is.na(province) & !is.na(district) & !is.na(clustername)) %>%
  mutate(label = paste0(round(pct, 2)*100,"% (", numerator,"/",denominator,")")) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, indicator, label) %>%
  unique() %>%
  pivot_wider(names_from = indicator,
              values_from = label) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything()) %>%
  arrange(campaign_name, region, province, district, ccode)

colnames(icm_team_monitoring_h2h_cluster) <- gsub("^H2H: ", "", colnames(icm_team_monitoring_h2h_cluster))

icm_team_monitoring_s2s_district <- all_data_list$icm_indicators$icm_pcts_district %>%
  filter(form_type == "Team Monitoring" & age_group == "0-59 Months") %>%
  filter(grepl("M2M/S2S:", indicator) | grepl("^[0-9]+\\)", indicator)) %>%
  filter(!is.na(region) & !is.na(province) & !is.na(district)) %>%
  mutate(label = paste0(round(pct, 2)*100,"% (", numerator,"/",denominator,")"),
         indicator_num = as.numeric(str_extract(indicator, "^[0-9]+"))) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, indicator, label, indicator_num) %>%
  arrange(indicator_num, indicator) %>%
  select(-indicator_num) %>%
  unique() %>%
  pivot_wider(names_from = indicator,
              values_from = label) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, everything()) %>%
  arrange(campaign_name, region, province, district)

colnames(icm_team_monitoring_s2s_district) <- gsub("^M2M/S2S: ", "", colnames(icm_team_monitoring_s2s_district))

icm_team_monitoring_h2h_district <- all_data_list$icm_indicators$icm_pcts_district %>%
  filter(form_type == "Team Monitoring" & age_group == "0-59 Months") %>%
  filter(grepl("H2H:", indicator)) %>%
  filter(!is.na(region) & !is.na(province) & !is.na(district)) %>%
  mutate(label = paste0(round(pct, 2)*100,"% (", numerator,"/",denominator,")")) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, indicator, label) %>%
  unique() %>%
  pivot_wider(names_from = indicator,
              values_from = label) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, everything()) %>%
  arrange(campaign_name, region, province, district)

colnames(icm_team_monitoring_h2h_district) <- gsub("^H2H: ", "", colnames(icm_team_monitoring_h2h_district))

icm_revisit_monitoring_h2h_cluster <- all_data_list$icm_indicators$icm_pcts_cluster %>%
  filter(form_type == "Monitoring for Revisit Strategy" & age_group == "0-59 Months") %>%
  filter(grepl("H2H:", indicator)) %>%
  filter(!is.na(region) & !is.na(province) & !is.na(district) & !is.na(clustername)) %>%
  mutate(label = paste0(round(pct, 2)*100,"% (", numerator,"/",denominator,")")) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, indicator, label) %>%
  unique() %>%
  pivot_wider(names_from = indicator,
              values_from = label) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything()) %>%
  arrange(campaign_name, region, province, district, ccode)

colnames(icm_revisit_monitoring_h2h_cluster) <- gsub("^H2H: ", "", colnames(icm_revisit_monitoring_h2h_cluster))

icm_revisit_monitoring_h2h_district <- all_data_list$icm_indicators$icm_pcts_district %>%
  filter(form_type == "Monitoring for Revisit Strategy" & age_group == "0-59 Months") %>%
  filter(grepl("H2H:", indicator)) %>%
  filter(!is.na(region) & !is.na(province) & !is.na(district) ) %>%
  mutate(label = paste0(round(pct, 2)*100,"% (", numerator,"/",denominator,")")) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, indicator, label) %>%
  unique() %>%
  pivot_wider(names_from = indicator,
              values_from = label) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, everything()) %>%
  arrange(campaign_name, region, province, district)

colnames(icm_revisit_monitoring_h2h_district) <- gsub("^H2H: ", "", colnames(icm_revisit_monitoring_h2h_district))

icm_site_monitoring_s2s_cluster <- all_data_list$icm_indicators$icm_pcts_cluster %>%
  filter(form_type == "Site Monitoring" & age_group == "0-59 Months") %>%
  filter(!is.na(region) & !is.na(province) & !is.na(district) & !is.na(clustername)) %>%
  mutate(label = paste0(round(pct, 2)*100,"% (", numerator,"/",denominator,")"),
         indicator_num = as.numeric(str_extract(indicator, "^[0-9]+"))) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, indicator, label, indicator_num) %>%
  arrange(indicator_num) %>%
  select(-indicator_num) %>%
  unique() %>%
  pivot_wider(names_from = indicator,
              values_from = label) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything()) %>%
  arrange(campaign_name, region, province, district, ccode)

icm_site_monitoring_s2s_district <- all_data_list$icm_indicators$icm_pcts_district %>%
  filter(form_type == "Site Monitoring" & age_group == "0-59 Months") %>%
  filter(!is.na(region) & !is.na(province) & !is.na(district)) %>%
  mutate(label = paste0(round(pct, 2)*100,"% (", numerator,"/",denominator,")"),
         indicator_num = as.numeric(str_extract(indicator, "^[0-9]+"))) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, indicator, label, indicator_num) %>%
  arrange(indicator_num) %>%
  select(-indicator_num) %>%
  unique() %>%
  pivot_wider(names_from = indicator,
              values_from = label) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, everything()) %>%
  arrange(campaign_name, region, province, district)

icm_session_monitoring_s2s_cluster <- all_data_list$icm_indicators$icm_pcts_cluster %>%
  filter(form_type == "IPV Session Monitoring" & age_group == "0-59 Months") %>%
  filter(!is.na(region) & !is.na(province) & !is.na(district) & !is.na(clustername)) %>%
  mutate(label = paste0(round(pct, 2)*100,"% (", numerator,"/",denominator,")"),
         indicator_num = as.numeric(str_extract(indicator, "^[0-9]+"))) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, indicator, label, indicator_num) %>%
  arrange(indicator_num) %>%
  select(-indicator_num) %>%
  unique() %>%
  pivot_wider(names_from = indicator,
              values_from = label) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything()) %>%
  arrange(campaign_name, region, province, district, ccode)

icm_session_monitoring_s2s_district <- all_data_list$icm_indicators$icm_pcts_district %>%
  filter(form_type == "IPV Session Monitoring" & age_group == "0-59 Months") %>%
  filter(!is.na(region) & !is.na(province) & !is.na(district)) %>%
  mutate(label = paste0(round(pct, 2)*100,"% (", numerator,"/",denominator,")"),
         indicator_num = as.numeric(str_extract(indicator, "^[0-9]+"))) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, indicator, label, indicator_num) %>%
  arrange(indicator_num) %>%
  select(-indicator_num) %>%
  unique() %>%
  pivot_wider(names_from = indicator,
              values_from = label) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, everything()) %>%
  arrange(campaign_name, region, province, district)

all_data_list$export_icm$icm_household_summary_cluster <- icm_household_summary_cluster
all_data_list$export_icm$icm_household_summary_district <- icm_household_summary_district
all_data_list$export_icm$icm_supervisor_monitoring_h2h_cluster <- icm_supervisor_monitoring_h2h_cluster
all_data_list$export_icm$icm_supervisor_monitoring_h2h_district <- icm_supervisor_monitoring_h2h_district
all_data_list$export_icm$icm_team_monitoring_s2s_cluster <- icm_team_monitoring_s2s_cluster
all_data_list$export_icm$icm_team_monitoring_s2s_district <- icm_team_monitoring_s2s_district
all_data_list$export_icm$icm_team_monitoring_h2h_district <- icm_team_monitoring_h2h_district
all_data_list$export_icm$icm_team_monitoring_h2h_cluster <- icm_team_monitoring_h2h_cluster
all_data_list$export_icm$icm_revisit_monitoring_h2h_cluster <- icm_revisit_monitoring_h2h_cluster
all_data_list$export_icm$icm_revisit_monitoring_h2h_district <- icm_revisit_monitoring_h2h_district
all_data_list$export_icm$icm_site_monitoring_s2s_cluster <- icm_site_monitoring_s2s_cluster
all_data_list$export_icm$icm_site_monitoring_s2s_district <- icm_site_monitoring_s2s_district
all_data_list$export_icm$icm_session_monitoring_s2s_cluster <- icm_session_monitoring_s2s_cluster
all_data_list$export_icm$icm_session_monitoring_s2s_district <- icm_session_monitoring_s2s_district
