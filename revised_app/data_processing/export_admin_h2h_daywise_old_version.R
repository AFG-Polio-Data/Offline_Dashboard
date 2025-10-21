export_admin_h2h_0_4_gt <- all_data_list$df_apmis_list$`DC Daily Compilation Day 1-4 H2H` %>%
  select(campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode,
         no_f_ull_vials_received_day1_4, no_f_ull_vials_returned_day1_4, no_f_ull_vials_used_day1_4,
         houses_visited_day1_3, children_vaccinated_in_houses_day1_3, children_vaccinated_outside_house_day1_3, nomad_children_vaccinated_day1_3,
         recorded_absent_day1_3gt, found_vaccinated_absent_return_during_campaign_day1_3gt, revaccinated_during_c_day1_3gt, remaining_absent_day1_3gt,
         found_vaccinated_absent_return_during_campaign_day4gt, revaccinated_during_c_day4gt, remaining_absent_day4gt,
         number_recorded_absent_day1_3gt, found_vaccinated_absent_day1_3gt, revaccinated_after_c_day1_3gt, remaining_absent_after_c_day1_3gt, found_vaccinated_absent_day4gt, revaccinated_after_c_day4gt, remaining_absent_after_c_day4gt,
         recorded_nss_day1_3gt, found_vacc_nss_day1_3gt, re_vacc_nss_day1_3gt, remaining_nss_day1_3gt, 
         found_vacc_nss_day4gt, re_vacc_nss_day4gt, remaining_nss_day4gt,
         recorded_refusal_day1_3gt, found_vacc_refusal_day1_3gt, re_vacc_refusal_day1_3gt, remaining_refusal_day1_3gt,
         found_vacc_refusal_day4gt, re_vacc_refusal_day4gt, remaining_refusal_day4gt,
         children_vaccinated_outside_house_day4gt,
         total_children_vaccinated_day1_3gt,
         total_children_vaccinated_day4gt,
         transit_teams_vaccinated_day1_4gt,
         total_missed_day1_4,
         afp_case_day1_4gt) %>%
  group_by(campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode) %>%
  summarise_all(~sum(as.numeric(str_remove_all(., "null")), na.rm=T)) %>%
  ungroup() %>%
  #NOTE: Once the district targets configuration is fixed, and all districts with targets are expecte to report Admin data, this should be changed to a right-join
  # left_join(all_data_list$df_apmis_list$campaign_district_pop %>%
  #              filter(ageGroup == "AGE_0_4") %>%
  #              select(campaign_name, dcode, district_population) %>%
  #              group_by(campaign_name, dcode) %>%
  #              summarise(target_population = sum(district_population, na.rm=T)) %>%
  #              ungroup() %>%
  #              unique() %>%
  #              filter(target_population != 0),
  #            by=c("campaigns"="campaign_name", "dcode")) %>%
  rename(campaign_name = campaigns) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything()) %>%
  # rowwise() %>%
  # mutate(total_children_vaccinated = sum(total_children_vaccinated_day1_3gt, total_children_vaccinated_day4gt, na.rm=T),
  #        admin_coverage = paste0(round(total_children_vaccinated / target_population,3)*100,"%")) %>%
  # ungroup() %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode,
         no_f_ull_vials_received_day1_4, no_f_ull_vials_returned_day1_4, no_f_ull_vials_used_day1_4,
         houses_visited_day1_3, children_vaccinated_in_houses_day1_3, children_vaccinated_outside_house_day1_3, nomad_children_vaccinated_day1_3,
         recorded_absent_day1_3gt, found_vaccinated_absent_return_during_campaign_day1_3gt, revaccinated_during_c_day1_3gt, remaining_absent_day1_3gt,
         found_vaccinated_absent_return_during_campaign_day4gt, revaccinated_during_c_day4gt, remaining_absent_day4gt,
         number_recorded_absent_day1_3gt, found_vaccinated_absent_day1_3gt, revaccinated_after_c_day1_3gt, remaining_absent_after_c_day1_3gt, found_vaccinated_absent_day4gt, revaccinated_after_c_day4gt, remaining_absent_after_c_day4gt,
         recorded_nss_day1_3gt, found_vacc_nss_day1_3gt, re_vacc_nss_day1_3gt, remaining_nss_day1_3gt, 
         found_vacc_nss_day4gt, re_vacc_nss_day4gt, remaining_nss_day4gt,
         recorded_refusal_day1_3gt, found_vacc_refusal_day1_3gt, re_vacc_refusal_day1_3gt, remaining_refusal_day1_3gt,
         found_vacc_refusal_day4gt, re_vacc_refusal_day4gt, remaining_refusal_day4gt,
         children_vaccinated_outside_house_day4gt,
         total_children_vaccinated_day1_3gt,
         total_children_vaccinated_day4gt,
         transit_teams_vaccinated_day1_4gt,
         total_missed_day1_4,
         afp_case_day1_4gt
         # target_population,
         # total_children_vaccinated,
         # admin_coverage
  ) %>%
  full_join(campaign_rpdc %>%
              select(campaign_name, rcode, pcode, dcode, ccode) %>%
              distinct(),
            by=c("campaign_name",
                 "rcode",
                 "pcode",
                 "dcode",
                 "ccode")) %>%
  left_join(all_data_list$df_apmis_list$campaigns %>%
              select(campaign_name, campaign_startdate)) %>%
  arrange(campaign_startdate, region, province, district, ccode) %>%
  select(-c("campaign_startdate")) %>%
  left_join(rpd_list %>%
              select(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE, APMIS_Region, APMIS_Province, APMIS_District) %>%
              unique(),
            by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
  mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
         province = ifelse(!is.na(APMIS_Province), APMIS_Province, province),
         district = ifelse(!is.na(APMIS_District), APMIS_District, district)) %>%
  select(-c("APMIS_Region", "APMIS_Province", "APMIS_District", "clustername")) %>%
  left_join(campaign_rpdc %>%
              select(cluster_name, rcode, pcode, dcode, ccode) %>%
              group_by(rcode, pcode, dcode, ccode) %>%
              slice(1) %>%
              ungroup() %>%
              distinct(),
            by=c("rcode", "pcode", "dcode", "ccode")) %>%
  rename(clustername = cluster_name) %>%
  arrange(campaign_name, region, province, district, ccode) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything()) 


export_admin_h2h_0_4_day1 <- all_data_list$df_apmis_list$`DC Daily Compilation Day 1-4 H2H` %>%
  select(campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode,
         no_f_ull_vials_received_day1, no_f_ull_vials_returned_day1, no_f_ull_vials_used_day1,
         transit_teams_vaccinated_day1,
         houses_visited_day1, children_vaccinated_in_houses_day1, children_vaccinated_outside_house_day1, nomad_children_vaccinated_day1,
         reasons_absent_return_during_campaign_day1, found_vaccinated_absent_return_during_campaign_day1, absent_vaccinated_by_team_during_c_day1, remaining_absent_day1,
         recorded_vaccinated_absent_day1, found_vaccinated_absent_day1, absent_vaccinated_by_team_after_c_day1, number_remaining_absent_day1,
         recorded_nss_day1, found_vacc_nss_day1, re_vacc_nss_day1, remaining_nss_day1,
         reason_refusal_day1, f_ound_vacc_refusal_day1, re_vacc_refusal_day1, remaining_refusal_day1,
         total_children_vaccinated_day1, afp_case_day1) %>%
  group_by(campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode) %>%
  summarise_all(~sum(as.numeric(str_remove_all(., "null")), na.rm=T)) %>%
  ungroup() %>%
  rename(campaign_name = campaigns) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything()) %>%
  ungroup() %>%
  full_join(campaign_rpdc %>%
              select(campaign_name, rcode, pcode, dcode, ccode) %>%
              distinct(),
            by=c("campaign_name",
                 "rcode",
                 "pcode",
                 "dcode",
                 "ccode")) %>%
  left_join(all_data_list$df_apmis_list$campaigns %>%
              select(campaign_name, campaign_startdate)) %>%
  arrange(campaign_startdate, region, province, district, ccode) %>%
  select(-c("campaign_startdate")) %>%
  left_join(rpd_list %>%
              select(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE, APMIS_Region, APMIS_Province, APMIS_District) %>%
              unique(),
            by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
  mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
         province = ifelse(!is.na(APMIS_Province), APMIS_Province, province),
         district = ifelse(!is.na(APMIS_District), APMIS_District, district)) %>%
  select(-c("APMIS_Region", "APMIS_Province", "APMIS_District", "clustername")) %>%
  left_join(campaign_rpdc %>%
              select(cluster_name, rcode, pcode, dcode, ccode) %>%
              group_by(rcode, pcode, dcode, ccode) %>%
              slice(1) %>%
              ungroup() %>%
              distinct(),
            by=c("rcode", "pcode", "dcode", "ccode")) %>%
  rename(clustername = cluster_name) %>%
  arrange(campaign_name, region, province, district, ccode) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything()) 

export_admin_h2h_0_4_day2 <- all_data_list$df_apmis_list$`DC Daily Compilation Day 1-4 H2H` %>%
  select(campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode, 
         no_f_ull_vials_received_day2, no_f_ull_vials_returned_day2, no_f_ull_vials_used_day2,
         transit_teams_vaccinated_day2,
         houses_visited_day2, children_vaccinated_in_houses_day2, children_vaccinated_outside_house_day2, nomad_children_vaccinated_day2,
         reasons_absent_return_during_campaign_day2, found_vaccinated_absent_return_during_campaign_day2, absent_vaccinated_by_team_during_c_day2, remaining_absent_day2,
         recorded_vaccinated_absent_day2, found_vaccinated_absent_day2, absent_vaccinated_by_team_after_c_day2, number_remaining_absent_day2,
         recorded_nss_day2, found_vacc_nss_day2, re_vacc_nss_day2, remaining_nss_day2,
         reason_refusal_day2, f_ound_vacc_refusal_day2, re_vacc_refusal_day2, remaining_refusal_day2,
         total_children_vaccinated_day2, afp_case_day2) %>%
  group_by(campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode) %>%
  summarise_all(~sum(as.numeric(str_remove_all(., "null")), na.rm=T)) %>%
  ungroup() %>%
  rename(campaign_name = campaigns) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, everything()) %>%
  ungroup() %>%
  full_join(campaign_rpdc %>%
              select(campaign_name, rcode, pcode, dcode, ccode) %>%
              distinct(),
            by=c("campaign_name",
                 "rcode",
                 "pcode",
                 "dcode",
                 "ccode")) %>%
  left_join(all_data_list$df_apmis_list$campaigns %>%
              select(campaign_name, campaign_startdate)) %>%
  arrange(campaign_startdate, region, province, district, ccode) %>%
  select(-c("campaign_startdate")) %>%
  left_join(rpd_list %>%
              select(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE, APMIS_Region, APMIS_Province, APMIS_District) %>%
              unique(),
            by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
  mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
         province = ifelse(!is.na(APMIS_Province), APMIS_Province, province),
         district = ifelse(!is.na(APMIS_District), APMIS_District, district)) %>%
  select(-c("APMIS_Region", "APMIS_Province", "APMIS_District", "clustername")) %>%
  left_join(campaign_rpdc %>%
              select(cluster_name, rcode, pcode, dcode, ccode) %>%
              group_by(rcode, pcode, dcode, ccode) %>%
              slice(1) %>%
              ungroup() %>%
              distinct(),
            by=c("rcode", "pcode", "dcode", "ccode")) %>%
  rename(clustername = cluster_name) %>%
  arrange(campaign_name, region, province, district, ccode) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything()) 

export_admin_h2h_0_4_day3 <- all_data_list$df_apmis_list$`DC Daily Compilation Day 1-4 H2H` %>%
  select(campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode,
         no_f_ull_vials_received_day3, no_f_ull_vials_returned_day3, no_f_ull_vials_used_day3,
         transit_teams_vaccinated_day3,
         houses_visited_day3, children_vaccinated_in_houses_day3, children_vaccinated_outside_house_day3, nomad_children_vaccinated_day3,
         reasons_absent_return_during_campaign_day3, found_vaccinated_absent_return_during_campaign_day3, absent_vaccinated_by_team_during_c_day3, remaining_absent_day3,
         recorded_vaccinated_absent_day3, found_vaccinated_absent_day3, absent_vaccinated_by_team_after_c_day3, number_remaining_absent_day3,
         recorded_nss_day3, found_vacc_nss_day3, re_vacc_nss_day3, remaining_nss_day3,
         reason_refusal_day3, f_ound_vacc_refusal_day3, re_vacc_refusal_day3, remaining_refusal_day3,
         total_children_vaccinated_day3, afp_case_day3) %>%
  group_by(campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode) %>%
  summarise_all(~sum(as.numeric(str_remove_all(., "null")), na.rm=T)) %>%
  ungroup() %>%
  rename(campaign_name = campaigns) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, everything()) %>%
  ungroup() %>%
  full_join(campaign_rpdc %>%
              select(campaign_name, rcode, pcode, dcode, ccode) %>%
              distinct(),
            by=c("campaign_name",
                 "rcode",
                 "pcode",
                 "dcode",
                 "ccode")) %>%
  left_join(all_data_list$df_apmis_list$campaigns %>%
              select(campaign_name, campaign_startdate)) %>%
  arrange(campaign_startdate, region, province, district, ccode) %>%
  select(-c("campaign_startdate")) %>%
  left_join(rpd_list %>%
              select(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE, APMIS_Region, APMIS_Province, APMIS_District) %>%
              unique(),
            by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
  mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
         province = ifelse(!is.na(APMIS_Province), APMIS_Province, province),
         district = ifelse(!is.na(APMIS_District), APMIS_District, district)) %>%
  select(-c("APMIS_Region", "APMIS_Province", "APMIS_District", "clustername")) %>%
  left_join(campaign_rpdc %>%
              select(cluster_name, rcode, pcode, dcode, ccode) %>%
              group_by(rcode, pcode, dcode, ccode) %>%
              slice(1) %>%
              ungroup() %>%
              distinct(),
            by=c("rcode", "pcode", "dcode", "ccode")) %>%
  rename(clustername = cluster_name) %>%
  arrange(campaign_name, region, province, district, ccode) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything()) 

export_admin_h2h_0_4_day4 <- all_data_list$df_apmis_list$`DC Daily Compilation Day 1-4 H2H` %>%
  select(campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode,
         no_f_ull_vials_received_day4, no_f_ull_vials_returned_day4, no_f_ull_vials_used_day4,
         transit_teams_vaccinated_day4,
         remaining_absent_day1_3_day4, found_vaccinated_absent_return_during_campaign_day4, absent_vaccinated_by_team_during_c_day4, remaining_absent_day4,
         number_remaining_absent_day1_3_day4, found_vaccinated_absent_day4, absent_vaccinated_by_team_after_c_day4, number_remaining_absent_day4,
         remaining_nss_day1_3_day4, found_vacc_nss_day4, re_vacc_nss_day4, remaining_nss_day4,
         remaining_refusal_day1_3_day4, f_ound_vacc_refusal_day4, re_vacc_refusal_day4, remaining_refusal_day4,
         children_vaccinated_outside_house_day4, total_children_vaccinated_day4,
         afp_case_day4) %>%
  group_by(campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode) %>%
  summarise_all(~sum(as.numeric(str_remove_all(., "null")), na.rm=T)) %>%
  ungroup() %>%
  rename(campaign_name = campaigns) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything()) %>%
  ungroup() %>%
  full_join(campaign_rpdc %>%
              select(campaign_name, rcode, pcode, dcode, ccode) %>%
              distinct(),
            by=c("campaign_name",
                 "rcode",
                 "pcode",
                 "dcode",
                 "ccode")) %>%
  left_join(all_data_list$df_apmis_list$campaigns %>%
              select(campaign_name, campaign_startdate)) %>%
  arrange(campaign_startdate, region, province, district, ccode) %>%
  select(-c("campaign_startdate")) %>%
  left_join(rpd_list %>%
              select(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE, APMIS_Region, APMIS_Province, APMIS_District) %>%
              unique(),
            by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
  mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
         province = ifelse(!is.na(APMIS_Province), APMIS_Province, province),
         district = ifelse(!is.na(APMIS_District), APMIS_District, district)) %>%
  select(-c("APMIS_Region", "APMIS_Province", "APMIS_District", "clustername")) %>%
  left_join(campaign_rpdc %>%
              select(cluster_name, rcode, pcode, dcode, ccode) %>%
              group_by(rcode, pcode, dcode, ccode) %>%
              slice(1) %>%
              ungroup() %>%
              distinct(),
            by=c("rcode", "pcode", "dcode", "ccode")) %>%
  rename(clustername = cluster_name) %>%
  arrange(campaign_name, region, province, district, ccode) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything()) 
