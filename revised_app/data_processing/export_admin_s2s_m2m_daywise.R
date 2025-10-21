# export_s2s_east <- all_data_list$df_apmis_list$`East SIA DC Daily Compilation Day 1-3` 
# for(i in c("total_children_vaccinated_day1", "total_children_vaccinated_day2", "total_children_vaccinated_day3",
#            "no_vials_used_day1", "no_vials_used_day2", "no_vials_used_day3",
#            "no_vials_distributed_day1", "no_vials_distributed_day2", "no_vials_distributed_day3",
#            "no_vials_remained_day1", "no_vials_remained_day2", "no_vials_remained_day3",
#            "children_vaccinated011m_day1", "children_vaccinated1259m_day1",
#            "children_vaccinated011m_day2", "children_vaccinated1259m_day2",
#            "children_vaccinated011m_day3", "children_vaccinated1259m_day3",
#            "missed_childrenfound011m_day1", "missed_childrenfound1259m_day1", "revisit_children_vaccinated011m_day1", "revisit_children_vaccinated1259m_day1", "transit_team_day1",
#            "missed_childrenfound011m_day2", "missed_childrenfound1259m_day2", "revisit_children_vaccinated011m_day2", "revisit_children_vaccinated1259m_day2", "transit_team_day2",
#            "missed_childrenfound011m_day3", "missed_childrenfound1259m_day3", "revisit_children_vaccinated011m_day3", "revisit_children_vaccinated1259m_day3", "transit_team_day3",
#            "no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", "total_sites_visited",
#            "afp_case_day1", "afp_case_day2", "afp_case_day3"
#            )){
#   if(!(i %in% colnames(export_s2s_east))){
#     export_s2s_east <- export_s2s_east %>%
#       mutate({{i}} := NA_integer_)
#   }
# }

export_s2s_east_4day <- all_data_list$df_apmis_list$`East SIA DC Daily Compilation Day 1-4` 
for(i in c("total_children_vaccinated_day1", "total_children_vaccinated_day2", "total_children_vaccinated_day3", "total_children_vaccinated_day4",
           "no_vials_used_day1", "no_vials_used_day2", "no_vials_used_day3", "no_vials_used_day4",
           "no_vials_distributed_day1", "no_vials_distributed_day2", "no_vials_distributed_day3", "no_vials_distributed_day4",
           "no_vials_remained_day1", "no_vials_remained_day2", "no_vials_remained_day3", "no_vials_remained_day4",
           "no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", "no_site_visited_day4",
           "children_vaccinated011m_day1", "children_vaccinated1259m_day1",
           "children_vaccinated011m_day2", "children_vaccinated1259m_day2",
           "children_vaccinated011m_day3", "children_vaccinated1259m_day3",
           "children_vaccinated011m_day4", "children_vaccinated1259m_day4",
           "transit_vaccinated011m_day1", "transit_vaccinated1259m_day1",
           "transit_vaccinated011m_day2", "transit_vaccinated1259m_day2",
           "transit_vaccinated011m_day3", "transit_vaccinated1259m_day3",
           "transit_vaccinated011m_day4", "transit_vaccinated1259m_day4",
           "hrmp_covered011m_day1", "hrmp_covered1259m_day1",
           "hrmp_covered011m_day2", "hrmp_covered1259m_day2",
           "hrmp_covered011m_day3", "hrmp_covered1259m_day3",
           "hrmp_covered011m_day4", "hrmp_covered1259m_day4",
           
           "reasons_absent_return_during_campaign_day1", "found_vaccinated_absent_return_during_campaign_day1", "absent_vaccinated_by_team_during_c_day1", "remaining_absent_day1",
           "reasons_absent_return_during_campaign_day2", "found_vaccinated_absent_return_during_campaign_day2", "absent_vaccinated_by_team_during_c_day2", "remaining_absent_day2",
           "reasons_absent_return_during_campaign_day3", "found_vaccinated_absent_return_during_campaign_day3", "absent_vaccinated_by_team_during_c_day3", "remaining_absent_day3",
           "reasons_absent_return_during_campaign_day4", "found_vaccinated_absent_return_during_campaign_day4", "absent_vaccinated_by_team_during_c_day4", "remaining_absent_day4",
           
           "recorded_vaccinated_absent_day1", "found_vaccinated_absent_day1", "absent_vaccinated_by_team_after_c_day1", "number_remaining_absent_day1",
           "recorded_vaccinated_absent_day2", "found_vaccinated_absent_day2", "absent_vaccinated_by_team_after_c_day2", "number_remaining_absent_day2",
           "recorded_vaccinated_absent_day3", "found_vaccinated_absent_day3", "absent_vaccinated_by_team_after_c_day3", "number_remaining_absent_day3",
           "recorded_vaccinated_absent_day4", "found_vaccinated_absent_day4", "absent_vaccinated_by_team_after_c_day4", "number_remaining_absent_day4",
           
           "recorded_nss_day1", "found_vacc_nss_day1", "re_vacc_nss_day1", "remaining_nss_day1",
           "recorded_nss_day2", "found_vacc_nss_day2", "re_vacc_nss_day2", "remaining_nss_day2",
           "recorded_nss_day3", "found_vacc_nss_day3", "re_vacc_nss_day3", "remaining_nss_day3",
           "recorded_nss_day4", "found_vacc_nss_day4", "re_vacc_nss_day4", "remaining_nss_day4",
           
           "reason_refusal_day1", "f_ound_vacc_refusal_day1", "re_vacc_refusal_day1", "remaining_refusal_day1",
           "reason_refusal_day2", "f_ound_vacc_refusal_day2", "re_vacc_refusal_day2", "remaining_refusal_day2",
           "reason_refusal_day3", "f_ound_vacc_refusal_day3", "re_vacc_refusal_day3", "remaining_refusal_day3",
           "reason_refusal_day4", "f_ound_vacc_refusal_day4", "re_vacc_refusal_day4", "remaining_refusal_day4",
           "afp_case_day1", "afp_case_day2", "afp_case_day3", "afp_case_day4", 
           "total_sites_visited", "additional_children_vaccinated_day4")){
  if(!(i %in% colnames(export_s2s_east_4day))){
    export_s2s_east_4day <- export_s2s_east_4day %>%
      mutate({{i}} := NA_integer_)
  }
}


export_admin_s2s_m2m <- all_data_list$df_apmis_list$`DC Daily Compilation Day 1-3` %>%
  mutate(across(
    where(~ is.character(.) && all(grepl("^\\s*$|^-?\\d+(\\.\\d+)?$", ., perl = TRUE) | is.na(.))),
    ~ suppressWarnings(as.numeric(.))
  )) %>%
  rowwise() %>%
  mutate(total_sites_visited = sum(no_site_visited_day1, no_site_visited_day2, no_site_visited_day3, na.rm=T),
         no_vials_distributed_days13 =  sum(no_vials_distributed_day1, no_vials_distributed_day2, no_vials_distributed_day3, na.rm=T), 
         no_vials_used_days13 = sum(no_vials_used_day1, no_vials_used_day2, no_vials_used_day3, na.rm=T), 
         no_vials_remained_days13 = sum(no_vials_remained_day1, no_vials_remained_day2, no_vials_remained_day3, na.rm=T),
         children_vaccinated011m_days13 = sum(children_vaccinated011m_day1, children_vaccinated011m_day2, children_vaccinated011m_day3,
                                              transit_vaccinated011m_day1, transit_vaccinated011m_day2, transit_vaccinated011m_day3, na.rm=T), 
         children_vaccinated1259m_days13 = sum(children_vaccinated1259m_day1, children_vaccinated1259m_day2, children_vaccinated1259m_day3,
                                               transit_vaccinated1259m_day1, transit_vaccinated1259m_day2, transit_vaccinated1259m_day3, na.rm=T), 
         total_children_vaccinated_days13 = sum(total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, na.rm=T),
         afp_case_days13 = sum(afp_case_day1, afp_case_day2, afp_case_day3, na.rm=T)) %>%
  ungroup() %>%
  bind_rows(all_data_list$df_apmis_list$`South SIA DC Daily Compilation Day 1-4` %>%
              mutate(across(
                where(~ is.character(.) && all(grepl("^\\s*$|^-?\\d+(\\.\\d+)?$", ., perl = TRUE) | is.na(.))),
                ~ suppressWarnings(as.numeric(.))
              )) %>%
              rowwise() %>%
              mutate(total_sites_visited = sum(no_site_visited_day1, no_site_visited_day2, no_site_visited_day3, no_site_visited_day4, na.rm=T),
                     no_vials_distributed_days13 =  sum(no_vials_distributed_day1, no_vials_distributed_day2, no_vials_distributed_day3, no_vials_distributed_day4, na.rm=T), 
                     no_vials_used_days13 = sum(no_vials_used_day1, no_vials_used_day2, no_vials_used_day3, no_vials_used_day4, na.rm=T), 
                     no_vials_remained_days13 = sum(no_vials_remained_day1, no_vials_remained_day2, no_vials_remained_day3, no_vials_remained_day4, na.rm=T),
                     children_vaccinated011m_days13 = sum(children_vaccinated011m_day1, children_vaccinated011m_day2, children_vaccinated011m_day3, children_vaccinated011m_day4,
                                                          transit_vaccinated011m_day1, transit_vaccinated011m_day2, transit_vaccinated011m_day3, transit_vaccinated011m_day4, 
                                                          hrmp_covered011m_day1, hrmp_covered011m_day2, hrmp_covered011m_day3, hrmp_covered011m_day4, na.rm=T), 
                     children_vaccinated1259m_days13 = sum(children_vaccinated1259m_day1, children_vaccinated1259m_day2, children_vaccinated1259m_day3, children_vaccinated1259m_day4,
                                                           transit_vaccinated1259m_day1, transit_vaccinated1259m_day2, transit_vaccinated1259m_day3, transit_vaccinated1259m_day4,
                                                           hrmp_covered1259m_day1, hrmp_covered1259m_day2, hrmp_covered1259m_day3, hrmp_covered1259m_day4, na.rm=T), 
                     total_children_vaccinated_days13 = sum(total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, na.rm=T),
                     afp_case_days13 = sum(afp_case_day1, afp_case_day2, afp_case_day3, afp_case_day4, na.rm=T)) %>%
              ungroup()) %>%
  bind_rows(all_data_list$df_apmis_list$`DC Daily Compilation Day 1-3 M2M` %>%
              mutate(across(
                where(~ is.character(.) && all(grepl("^\\s*$|^-?\\d+(\\.\\d+)?$", ., perl = TRUE) | is.na(.))),
                ~ suppressWarnings(as.numeric(.))
              )) %>%
              rowwise() %>%
              mutate(total_sites_visited = NA,
                     no_vials_distributed_days13 =  sum(no_vials_distributed_day1, no_vials_distributed_day2, no_vials_distributed_day3, na.rm=T), 
                     no_vials_used_days13 = sum(no_vials_used_day1, no_vials_used_day2, no_vials_used_day3, na.rm=T), 
                     no_vials_remained_days13 = sum(no_vials_remained_day1, no_vials_remained_day2, no_vials_remained_day3, na.rm=T),
                     children_vaccinated011m_days13 = sum(children_vaccinated011m_day1, children_vaccinated011m_day2, children_vaccinated011m_day3, na.rm=T), 
                     children_vaccinated1259m_days13 = sum(children_vaccinated1259m_day1, children_vaccinated1259m_day2, children_vaccinated1259m_day3, na.rm=T), 
                     total_children_vaccinated_days13 = sum(total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, na.rm=T),
                     afp_case_days13 = sum(afp_case_day1, afp_case_day2, afp_case_day3, na.rm=T)) %>%
              ungroup()
  ) %>%
  # bind_rows( export_s2s_east %>%
  #              rowwise() %>%
  #              mutate(total_sites_visited = sum(no_site_visited_day1, no_site_visited_day2, no_site_visited_day3, na.rm=T),
  #                     children_vaccinated011m_day1 = sum(children_vaccinated011m_day1, revisit_children_vaccinated011m_day1, na.rm=T),
  #                     children_vaccinated1259m_day1 = sum(children_vaccinated1259m_day1, revisit_children_vaccinated1259m_day1, na.rm=T),
  #                     children_vaccinated011m_day2 = sum(children_vaccinated011m_day2, revisit_children_vaccinated011m_day2, na.rm=T),
  #                     children_vaccinated1259m_day2 = sum(children_vaccinated1259m_day2, revisit_children_vaccinated1259m_day2, na.rm=T),
  #                     children_vaccinated011m_day3 = sum(children_vaccinated011m_day3, revisit_children_vaccinated011m_day3, na.rm=T),
  #                     children_vaccinated1259m_day3 = sum(children_vaccinated1259m_day3, revisit_children_vaccinated1259m_day3, na.rm=T),
  #                     total_children_vaccinated_day1 = sum(children_vaccinated011m_day1, children_vaccinated1259m_day1, na.rm=T),
  #                     total_children_vaccinated_day2 = sum(children_vaccinated011m_day2, children_vaccinated1259m_day2, na.rm=T),
  #                     total_children_vaccinated_day3 = sum(children_vaccinated011m_day3, children_vaccinated1259m_day3, na.rm=T),
  #                     no_vials_distributed_days13 =  sum(no_vials_distributed_day1, no_vials_distributed_day2, no_vials_distributed_day3, na.rm=T), 
  #                     no_vials_used_days13 = sum(no_vials_used_day1, no_vials_used_day2, no_vials_used_day3, na.rm=T), 
  #                     no_vials_remained_days13 = sum(no_vials_remained_day1, no_vials_remained_day2, no_vials_remained_day3, na.rm=T),
  #                     children_vaccinated011m_days13 = sum(children_vaccinated011m_day1, children_vaccinated011m_day2, children_vaccinated011m_day3, na.rm=T), 
  #                     children_vaccinated1259m_days13 = sum(children_vaccinated1259m_day1, children_vaccinated1259m_day2, children_vaccinated1259m_day3, na.rm=T), 
  #                     total_children_vaccinated_days13 = sum(total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, na.rm=T),
  #                     afp_case_days13 = sum(afp_case_day1, afp_case_day2, afp_case_day3, na.rm=T)) %>%
  #              ungroup()) %>%
  bind_rows( export_s2s_east_4day %>%
               # Convert all relevant variables to numeric before rowwise
               mutate(across(
                 c(
                   no_site_visited_day1, no_site_visited_day2, no_site_visited_day3, no_site_visited_day4,
                   children_vaccinated011m_day1, children_vaccinated1259m_day1,
                   children_vaccinated011m_day2, children_vaccinated1259m_day2,
                   children_vaccinated011m_day3, children_vaccinated1259m_day3,
                   children_vaccinated011m_day4, children_vaccinated1259m_day4,
                   hrmp_covered011m_day1, hrmp_covered1259m_day1,
                   hrmp_covered011m_day2, hrmp_covered1259m_day2,
                   hrmp_covered011m_day3, hrmp_covered1259m_day3,
                   hrmp_covered011m_day4, hrmp_covered1259m_day4,
                   transit_vaccinated011m_day1, transit_vaccinated1259m_day1,
                   transit_vaccinated011m_day2, transit_vaccinated1259m_day2,
                   transit_vaccinated011m_day3, transit_vaccinated1259m_day3,
                   transit_vaccinated011m_day4, transit_vaccinated1259m_day4,
                   absent_vaccinated_by_team_during_c_day1, absent_vaccinated_by_team_after_c_day1,
                   absent_vaccinated_by_team_during_c_day2, absent_vaccinated_by_team_after_c_day2,
                   absent_vaccinated_by_team_during_c_day3, absent_vaccinated_by_team_after_c_day3,
                   absent_vaccinated_by_team_during_c_day4, absent_vaccinated_by_team_after_c_day4,
                   re_vacc_nss_day1, re_vacc_refusal_day1,
                   re_vacc_nss_day2, re_vacc_refusal_day2,
                   re_vacc_nss_day3, re_vacc_refusal_day3,
                   re_vacc_nss_day4, re_vacc_refusal_day4,
                   additional_children_vaccinated_day4,
                   no_vials_distributed_day1, no_vials_distributed_day2, no_vials_distributed_day3, no_vials_distributed_day4,
                   no_vials_used_day1, no_vials_used_day2, no_vials_used_day3, no_vials_used_day4,
                   no_vials_remained_day1, no_vials_remained_day2, no_vials_remained_day3, no_vials_remained_day4,
                   total_children_vaccinated_day1, total_children_vaccinated_day2,
                   total_children_vaccinated_day3, total_children_vaccinated_day4,
                   afp_case_day1, afp_case_day2, afp_case_day3, afp_case_day4
                 ),
                 ~ suppressWarnings(as.numeric(.))
               )) %>%
               rowwise() %>%
               mutate(
                 total_sites_visited = sum(no_site_visited_day1, no_site_visited_day2, no_site_visited_day3, no_site_visited_day4, na.rm = TRUE),
                 total_children_vaccinated_day1 = sum(children_vaccinated011m_day1, children_vaccinated1259m_day1, hrmp_covered011m_day1, hrmp_covered1259m_day1,
                                                      transit_vaccinated011m_day1, transit_vaccinated1259m_day1,
                                                      absent_vaccinated_by_team_during_c_day1, absent_vaccinated_by_team_after_c_day1,
                                                      re_vacc_nss_day1, re_vacc_refusal_day1, na.rm = TRUE),
                 total_children_vaccinated_day2 = sum(children_vaccinated011m_day2, children_vaccinated1259m_day2, hrmp_covered011m_day2, hrmp_covered1259m_day2,
                                                      transit_vaccinated011m_day2, transit_vaccinated1259m_day2,
                                                      absent_vaccinated_by_team_during_c_day2, absent_vaccinated_by_team_after_c_day2,
                                                      re_vacc_nss_day2, re_vacc_refusal_day2, na.rm = TRUE),
                 total_children_vaccinated_day3 = sum(children_vaccinated011m_day3, children_vaccinated1259m_day3, hrmp_covered011m_day3, hrmp_covered1259m_day3,
                                                      transit_vaccinated011m_day3, transit_vaccinated1259m_day3,
                                                      absent_vaccinated_by_team_during_c_day3, absent_vaccinated_by_team_after_c_day3,
                                                      re_vacc_nss_day3, re_vacc_refusal_day3, na.rm = TRUE),
                 total_children_vaccinated_day4 = sum(children_vaccinated011m_day4, children_vaccinated1259m_day4, hrmp_covered011m_day4, hrmp_covered1259m_day4,
                                                      transit_vaccinated011m_day4, transit_vaccinated1259m_day4,
                                                      absent_vaccinated_by_team_during_c_day4, absent_vaccinated_by_team_after_c_day4,
                                                      re_vacc_nss_day4, re_vacc_refusal_day4, additional_children_vaccinated_day4, na.rm = TRUE),
                 no_vials_distributed_days13 = sum(no_vials_distributed_day1, no_vials_distributed_day2, no_vials_distributed_day3, no_vials_distributed_day4, na.rm = TRUE),
                 no_vials_used_days13 = sum(no_vials_used_day1, no_vials_used_day2, no_vials_used_day3, no_vials_used_day4, na.rm = TRUE),
                 no_vials_remained_days13 = sum(no_vials_remained_day1, no_vials_remained_day2, no_vials_remained_day3, no_vials_remained_day4, na.rm = TRUE),
                 total_children_vaccinated_days13 = sum(total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, na.rm = TRUE),
                 afp_case_days13 = sum(afp_case_day1, afp_case_day2, afp_case_day3, afp_case_day4, na.rm = TRUE)
               ) %>%
               ungroup() %>%
               select(-c("admin_day1_readonly", "admin_day2_readonly", "admin_day3_readonly", "admin_day4_readonly",
                         "day_target_day1", "day_target_day2", "day_target_day3", "day_target_day4"))) %>%
  select(campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode,
         no_site_visited_day1,
         no_vials_distributed_day1, no_vials_used_day1, no_vials_remained_day1,
         children_vaccinated011m_day1, children_vaccinated1259m_day1, total_children_vaccinated_day1,
         no_site_visited_day2,
         no_vials_distributed_day2, no_vials_used_day2, no_vials_remained_day2,
         children_vaccinated011m_day2, children_vaccinated1259m_day2, total_children_vaccinated_day2,
         no_site_visited_day3,
         no_vials_distributed_day3, no_vials_used_day3, no_vials_remained_day3,
         children_vaccinated011m_day3, children_vaccinated1259m_day3, total_children_vaccinated_day3,
         no_site_visited_day4,
         no_vials_distributed_day4, no_vials_used_day4, no_vials_remained_day4,
         total_children_vaccinated_day4,
         total_sites_visited,
         no_vials_distributed_days13, no_vials_used_days13, no_vials_remained_days13,
         children_vaccinated011m_days13, children_vaccinated1259m_days13, total_children_vaccinated_days13,
         afp_case_days13) %>%
  # Step 1: Clean up numeric-like text once
  mutate(across(
    where(~ !is.numeric(.) && !is.list(.) &&
            # only convert if column looks numeric overall
            all(grepl("^\\s*$|^-?\\d+(\\.\\d+)?$|^null$", ., perl = TRUE) | is.na(.))),
    ~ suppressWarnings(as.numeric(str_remove_all(., "null")))
  )) %>%
  # Step 2: group and summarise numeric cols only
  group_by(campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE), .names = "{.col}"), .groups = "drop_last") %>%
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
  mutate_at(c("no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", "total_sites_visited"), ~ifelse(.==0, NA_integer_, .)) %>%
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


export_admin_ipv_7day <- all_data_list$df_apmis_list$`fIPV OPV DC Daily Compilation Day 1-7` %>%
  mutate(across(c(afp_case_day1, afp_case_day2, afp_case_day3, afp_case_day4,
                  afp_case_day5, afp_case_day6, afp_case_day7,
                  no_site_visited_day1, no_site_visited_day2, no_site_visited_day3, no_site_visited_day4,
                  no_site_visited_day5, no_site_visited_day6, no_site_visited_day7,
                  opv_total_children_vaccinated_day1, opv_total_children_vaccinated_day2, opv_total_children_vaccinated_day3, opv_total_children_vaccinated_day4,
                  opv_total_children_vaccinated_day5, opv_total_children_vaccinated_day6, opv_total_children_vaccinated_day7,
                  ipv_total_children_vaccinated_day1, ipv_total_children_vaccinated_day2, ipv_total_children_vaccinated_day3, ipv_total_children_vaccinated_day4,
                  ipv_total_children_vaccinated_day5, ipv_total_children_vaccinated_day6, ipv_total_children_vaccinated_day7), ~as.numeric(.))) %>%
  mutate(
    total_afp_cases = rowSums(
      across(c(
        afp_case_day1, afp_case_day2, afp_case_day3, afp_case_day4,
        afp_case_day5, afp_case_day6, afp_case_day7
      )),
      na.rm = TRUE
    )
  ) %>%
  mutate(
    total_sites_visited = rowSums(
      across(c(
        no_site_visited_day1, no_site_visited_day2, no_site_visited_day3, no_site_visited_day4,
        no_site_visited_day5, no_site_visited_day6, no_site_visited_day7
      )),
      na.rm = TRUE
    )
  ) %>%
  mutate(
    total_opv_vaccinated = rowSums(
      across(c(
        opv_total_children_vaccinated_day1, opv_total_children_vaccinated_day2, opv_total_children_vaccinated_day3, opv_total_children_vaccinated_day4,
        opv_total_children_vaccinated_day5, opv_total_children_vaccinated_day6, opv_total_children_vaccinated_day7
      )),
      na.rm = TRUE
    )
  ) %>%
  mutate(
    total_ipv_vaccinated = rowSums(
      across(c(
        ipv_total_children_vaccinated_day1, ipv_total_children_vaccinated_day2, ipv_total_children_vaccinated_day3, ipv_total_children_vaccinated_day4,
        ipv_total_children_vaccinated_day5, ipv_total_children_vaccinated_day6, ipv_total_children_vaccinated_day7
      )),
      na.rm = TRUE
    )
  ) %>%
  select(campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode,
         
         no_site_visited_day1,
         opv_no_vials_distributed_day1, opv_no_vials_used_day1, opv_no_vials_remained_day1,
         ipv_no_vials_distributed_day1, ipv_no_vials_used_day1, ipv_no_vials_remained_day1,
         opv_total_children_vaccinated_day1, ipv_total_children_vaccinated_day1,
         
         no_site_visited_day2,
         opv_no_vials_distributed_day2, opv_no_vials_used_day2, opv_no_vials_remained_day2,
         ipv_no_vials_distributed_day2, ipv_no_vials_used_day2, ipv_no_vials_remained_day2,
         opv_total_children_vaccinated_day2, ipv_total_children_vaccinated_day2,
         
         no_site_visited_day3,
         opv_no_vials_distributed_day3, opv_no_vials_used_day3, opv_no_vials_remained_day3,
         ipv_no_vials_distributed_day3, ipv_no_vials_used_day3, ipv_no_vials_remained_day3,
         opv_total_children_vaccinated_day3, ipv_total_children_vaccinated_day3,
         
         no_site_visited_day4,
         opv_no_vials_distributed_day4, opv_no_vials_used_day4, opv_no_vials_remained_day4,
         ipv_no_vials_distributed_day4, ipv_no_vials_used_day4, ipv_no_vials_remained_day4,
         opv_total_children_vaccinated_day4, ipv_total_children_vaccinated_day4,
         
         no_site_visited_day5,
         opv_no_vials_distributed_day5, opv_no_vials_used_day5, opv_no_vials_remained_day5,
         ipv_no_vials_distributed_day5, ipv_no_vials_used_day5, ipv_no_vials_remained_day5,
         opv_total_children_vaccinated_day5, ipv_total_children_vaccinated_day5,
         
         no_site_visited_day6,
         opv_no_vials_distributed_day6, opv_no_vials_used_day6, opv_no_vials_remained_day6,
         ipv_no_vials_distributed_day6, ipv_no_vials_used_day6, ipv_no_vials_remained_day6,
         opv_total_children_vaccinated_day6, ipv_total_children_vaccinated_day6,
         
         no_site_visited_day7,
         opv_no_vials_distributed_day7, opv_no_vials_used_day7, opv_no_vials_remained_day7,
         ipv_no_vials_distributed_day7, ipv_no_vials_used_day7, ipv_no_vials_remained_day7,
         opv_total_children_vaccinated_day7, ipv_total_children_vaccinated_day7,
         
         total_sites_visited, total_opv_vaccinated, total_ipv_vaccinated, total_afp_cases
         
) %>%
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
  mutate_at(c("no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", "no_site_visited_day4", "no_site_visited_day5", "no_site_visited_day6", "no_site_visited_day7",  "total_sites_visited"), ~ifelse(.==0, NA_integer_, .)) %>%
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

