export_admin_conversion_cluster <- all_data_list$df_apmis_list$`DC Daily Compilation Day 1-4 H2H` %>%
  select(campaigns, rcode, pcode, dcode, ccode, clustername,
         
         reasons_absent_return_during_campaign_day1, reasons_absent_return_during_campaign_day2, reasons_absent_return_during_campaign_day3,
         found_vaccinated_absent_return_during_campaign_day1, found_vaccinated_absent_return_during_campaign_day2, found_vaccinated_absent_return_during_campaign_day3, found_vaccinated_absent_return_during_campaign_day4, 
         absent_vaccinated_by_team_during_c_day1, absent_vaccinated_by_team_during_c_day2, absent_vaccinated_by_team_during_c_day3, absent_vaccinated_by_team_during_c_day4,
         
         recorded_vaccinated_absent_day1, recorded_vaccinated_absent_day2, recorded_vaccinated_absent_day3,
         found_vaccinated_absent_day1, found_vaccinated_absent_day2, found_vaccinated_absent_day3, found_vaccinated_absent_day4, 
         absent_vaccinated_by_team_after_c_day1, absent_vaccinated_by_team_after_c_day2, absent_vaccinated_by_team_after_c_day3, absent_vaccinated_by_team_after_c_day4,
         
         recorded_nss_day1, recorded_nss_day2, recorded_nss_day3,
         found_vacc_nss_day1, found_vacc_nss_day2, found_vacc_nss_day3,  found_vacc_nss_day4, 
         re_vacc_nss_day1, re_vacc_nss_day2, re_vacc_nss_day3,  re_vacc_nss_day4,
         
         reason_refusal_day1, reason_refusal_day2, reason_refusal_day3,
         f_ound_vacc_refusal_day1, f_ound_vacc_refusal_day2, f_ound_vacc_refusal_day3, f_ound_vacc_refusal_day4, 
         re_vacc_refusal_day1, re_vacc_refusal_day2, re_vacc_refusal_day3,  re_vacc_refusal_day4) %>%
  mutate(across(-c(campaigns, rcode, pcode, dcode, ccode, clustername), as.numeric)) %>%
  rowwise() %>%
  mutate(recorded_missed_reason_absent1 = sum(reasons_absent_return_during_campaign_day1, reasons_absent_return_during_campaign_day2, reasons_absent_return_during_campaign_day3, na.rm=T),
         recorded_missed_reason_absent2 = sum(recorded_vaccinated_absent_day1, recorded_vaccinated_absent_day2, recorded_vaccinated_absent_day3, na.rm=T),
         recorded_missed_reason_nss = sum(recorded_nss_day1, recorded_nss_day2, recorded_nss_day3, na.rm=T),
         recorded_missed_reason_refusal = sum(reason_refusal_day1, reason_refusal_day2, reason_refusal_day3, na.rm=T),
         recorded_missed_day1 = sum(reasons_absent_return_during_campaign_day1, recorded_vaccinated_absent_day1, recorded_nss_day1, reason_refusal_day1, na.rm=T),
         recorded_missed_day2 = sum(reasons_absent_return_during_campaign_day2, recorded_vaccinated_absent_day2, recorded_nss_day2, reason_refusal_day2, na.rm=T),
         recorded_missed_day3 = sum(reasons_absent_return_during_campaign_day3, recorded_vaccinated_absent_day3, recorded_nss_day3, reason_refusal_day3, na.rm=T),
         recorded_missed_reason_absent1_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day1, found_vaccinated_absent_return_during_campaign_day2, found_vaccinated_absent_return_during_campaign_day3, found_vaccinated_absent_return_during_campaign_day4, 
                                                         absent_vaccinated_by_team_during_c_day1, absent_vaccinated_by_team_during_c_day2, absent_vaccinated_by_team_during_c_day3, absent_vaccinated_by_team_during_c_day4, na.rm=T),
         recorded_missed_reason_absent2_vaccinated = sum(found_vaccinated_absent_day1, found_vaccinated_absent_day2, found_vaccinated_absent_day3, found_vaccinated_absent_day4, 
                                                         absent_vaccinated_by_team_after_c_day1, absent_vaccinated_by_team_after_c_day2, absent_vaccinated_by_team_after_c_day3, absent_vaccinated_by_team_after_c_day4, na.rm=T),
         recorded_missed_reason_nss_vaccinated = sum(found_vacc_nss_day1, found_vacc_nss_day2, found_vacc_nss_day3,  found_vacc_nss_day4, 
                                                     re_vacc_nss_day1, re_vacc_nss_day2, re_vacc_nss_day3,  re_vacc_nss_day4, na.rm=T),
         recorded_missed_reason_refusal_vaccinated = sum(f_ound_vacc_refusal_day1, f_ound_vacc_refusal_day2, f_ound_vacc_refusal_day3, f_ound_vacc_refusal_day4, 
                                                         re_vacc_refusal_day1, re_vacc_refusal_day2, re_vacc_refusal_day3,  re_vacc_refusal_day4, na.rm=T),
         recorded_missed_day1_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day1, absent_vaccinated_by_team_during_c_day1,
                                               found_vaccinated_absent_day1, absent_vaccinated_by_team_after_c_day1,
                                               found_vacc_nss_day1, re_vacc_nss_day1,
                                               f_ound_vacc_refusal_day1, re_vacc_refusal_day1, na.rm=T),
         recorded_missed_day2_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day2, absent_vaccinated_by_team_during_c_day2,
                                               found_vaccinated_absent_day2, absent_vaccinated_by_team_after_c_day2,
                                               found_vacc_nss_day2, re_vacc_nss_day2,
                                               f_ound_vacc_refusal_day2, re_vacc_refusal_day2, na.rm=T),
         recorded_missed_day3_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day3, absent_vaccinated_by_team_during_c_day3,
                                               found_vaccinated_absent_day3, absent_vaccinated_by_team_after_c_day3,
                                               found_vacc_nss_day3, re_vacc_nss_day3,
                                               f_ound_vacc_refusal_day3, re_vacc_refusal_day3, na.rm=T),
         recorded_missed_revisit_day_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day4, absent_vaccinated_by_team_during_c_day4,
                                                      found_vaccinated_absent_day4, absent_vaccinated_by_team_after_c_day4,
                                                      found_vacc_nss_day4, re_vacc_nss_day4,
                                                      f_ound_vacc_refusal_day4, re_vacc_refusal_day4, na.rm=T),
         recorded_missed_total = sum(recorded_missed_day1, recorded_missed_day2, recorded_missed_day3, na.rm=T),
         recorded_missed_total_vaccinated = sum(recorded_missed_day1_vaccinated, recorded_missed_day2_vaccinated, recorded_missed_day3_vaccinated, recorded_missed_revisit_day_vaccinated, na.rm=T)) %>%
  ungroup() %>%
  mutate(age_group = "0-59 Months",
         campaign_days = 4) %>%
  select(campaigns, rcode, pcode, dcode, ccode, clustername, age_group, campaign_days,
         recorded_missed_reason_absent1, recorded_missed_reason_absent1_vaccinated,
         recorded_missed_reason_absent2, recorded_missed_reason_absent2_vaccinated,
         recorded_missed_reason_nss, recorded_missed_reason_nss_vaccinated,
         recorded_missed_reason_refusal, recorded_missed_reason_refusal_vaccinated,
         recorded_missed_day1, recorded_missed_day1_vaccinated,
         recorded_missed_day2, recorded_missed_day2_vaccinated,
         recorded_missed_day3, recorded_missed_day3_vaccinated,
         recorded_missed_revisit_day_vaccinated,
         recorded_missed_total, recorded_missed_total_vaccinated) %>%
  bind_rows(all_data_list$df_apmis_list$`DC Daily Compilation Day 1-7 (0-5)` %>%
              select(campaigns, rcode, pcode, dcode, ccode, clustername,
                     
                     reasons_absent_return_during_campaign_day1, reasons_absent_return_during_campaign_day2, reasons_absent_return_during_campaign_day3, reasons_absent_return_during_campaign_day4, reasons_absent_return_during_campaign_day5, reasons_absent_return_during_campaign_day6,
                     found_vaccinated_absent_return_during_campaign_day1, found_vaccinated_absent_return_during_campaign_day2, found_vaccinated_absent_return_during_campaign_day3, found_vaccinated_absent_return_during_campaign_day4, found_vaccinated_absent_return_during_campaign_day5, found_vaccinated_absent_return_during_campaign_day6, found_vaccinated_absent_return_during_campaign_day7,
                     absent_vaccinated_by_team_during_c_day1, absent_vaccinated_by_team_during_c_day2, absent_vaccinated_by_team_during_c_day3, absent_vaccinated_by_team_during_c_day4, absent_vaccinated_by_team_during_c_day5, absent_vaccinated_by_team_during_c_day6, absent_vaccinated_by_team_during_c_day7, 
                     
                     recorded_vaccinated_absent_day1, recorded_vaccinated_absent_day2, recorded_vaccinated_absent_day3, recorded_vaccinated_absent_day4, recorded_vaccinated_absent_day5, recorded_vaccinated_absent_day6,
                     found_vaccinated_absent_day1, found_vaccinated_absent_day2, found_vaccinated_absent_day3, found_vaccinated_absent_day4, found_vaccinated_absent_day5, found_vaccinated_absent_day6, found_vaccinated_absent_day7,
                     absent_vaccinated_by_team_after_c_day1, absent_vaccinated_by_team_after_c_day2, absent_vaccinated_by_team_after_c_day3, absent_vaccinated_by_team_after_c_day4, absent_vaccinated_by_team_after_c_day5, absent_vaccinated_by_team_after_c_day6, absent_vaccinated_by_team_after_c_day7,
                     
                     recorded_nss_day1, recorded_nss_day2, recorded_nss_day3, recorded_nss_day4, recorded_nss_day5, recorded_nss_day6, 
                     found_vacc_nss_day1, found_vacc_nss_day2, found_vacc_nss_day3,  found_vacc_nss_day4, found_vacc_nss_day5, found_vacc_nss_day6, found_vacc_nss_day7,
                     re_vacc_nss_day1, re_vacc_nss_day2, re_vacc_nss_day3,  re_vacc_nss_day4, re_vacc_nss_day5, re_vacc_nss_day6, re_vacc_nss_day7,
                     
                     reason_refusal_day1, reason_refusal_day2, reason_refusal_day3, reason_refusal_day4, reason_refusal_day5, reason_refusal_day6,
                     f_ound_vacc_refusal_day1, f_ound_vacc_refusal_day2, f_ound_vacc_refusal_day3, f_ound_vacc_refusal_day4, f_ound_vacc_refusal_day5, f_ound_vacc_refusal_day6, f_ound_vacc_refusal_day7,
                     re_vacc_refusal_day1, re_vacc_refusal_day2, re_vacc_refusal_day3,  re_vacc_refusal_day4, re_vacc_refusal_day5, re_vacc_refusal_day6, re_vacc_refusal_day7) %>%
              mutate(across(-c(campaigns, rcode, pcode, dcode, ccode, clustername), as.numeric)) %>%
              rowwise() %>%
              mutate(recorded_missed_reason_absent1 = sum(reasons_absent_return_during_campaign_day1, reasons_absent_return_during_campaign_day2, reasons_absent_return_during_campaign_day3, reasons_absent_return_during_campaign_day4, reasons_absent_return_during_campaign_day5, reasons_absent_return_during_campaign_day6, na.rm=T),
                     recorded_missed_reason_absent2 = sum(recorded_vaccinated_absent_day1, recorded_vaccinated_absent_day2, recorded_vaccinated_absent_day3, recorded_vaccinated_absent_day4, recorded_vaccinated_absent_day5, recorded_vaccinated_absent_day6, na.rm=T),
                     recorded_missed_reason_nss = sum(recorded_nss_day1, recorded_nss_day2, recorded_nss_day3, recorded_nss_day4, recorded_nss_day5, recorded_nss_day6, na.rm=T),
                     recorded_missed_reason_refusal = sum(reason_refusal_day1, reason_refusal_day2, reason_refusal_day3, reason_refusal_day4, reason_refusal_day5, reason_refusal_day6, na.rm=T),
                     recorded_missed_day1 = sum(reasons_absent_return_during_campaign_day1, recorded_vaccinated_absent_day1, recorded_nss_day1, reason_refusal_day1, na.rm=T),
                     recorded_missed_day2 = sum(reasons_absent_return_during_campaign_day2, recorded_vaccinated_absent_day2, recorded_nss_day2, reason_refusal_day2, na.rm=T),
                     recorded_missed_day3 = sum(reasons_absent_return_during_campaign_day3, recorded_vaccinated_absent_day3, recorded_nss_day3, reason_refusal_day3, na.rm=T),
                     recorded_missed_day4 = sum(reasons_absent_return_during_campaign_day4, recorded_vaccinated_absent_day4, recorded_nss_day4, reason_refusal_day4, na.rm=T),
                     recorded_missed_day5 = sum(reasons_absent_return_during_campaign_day5, recorded_vaccinated_absent_day5, recorded_nss_day5, reason_refusal_day5, na.rm=T),
                     recorded_missed_day6 = sum(reasons_absent_return_during_campaign_day6, recorded_vaccinated_absent_day6, recorded_nss_day6, reason_refusal_day6, na.rm=T),
                     recorded_missed_reason_absent1_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day1, found_vaccinated_absent_return_during_campaign_day2, found_vaccinated_absent_return_during_campaign_day3, found_vaccinated_absent_return_during_campaign_day4, found_vaccinated_absent_return_during_campaign_day5, found_vaccinated_absent_return_during_campaign_day6, found_vaccinated_absent_return_during_campaign_day7,
                                                                     absent_vaccinated_by_team_during_c_day1, absent_vaccinated_by_team_during_c_day2, absent_vaccinated_by_team_during_c_day3, absent_vaccinated_by_team_during_c_day4, absent_vaccinated_by_team_during_c_day5, absent_vaccinated_by_team_during_c_day6, absent_vaccinated_by_team_during_c_day7, na.rm=T),
                     recorded_missed_reason_absent2_vaccinated = sum(found_vaccinated_absent_day1, found_vaccinated_absent_day2, found_vaccinated_absent_day3, found_vaccinated_absent_day4, found_vaccinated_absent_day5, found_vaccinated_absent_day6, found_vaccinated_absent_day7,
                                                                     absent_vaccinated_by_team_after_c_day1, absent_vaccinated_by_team_after_c_day2, absent_vaccinated_by_team_after_c_day3, absent_vaccinated_by_team_after_c_day4, absent_vaccinated_by_team_after_c_day5, absent_vaccinated_by_team_after_c_day6, absent_vaccinated_by_team_after_c_day7, na.rm=T),
                     recorded_missed_reason_nss_vaccinated = sum(found_vacc_nss_day1, found_vacc_nss_day2, found_vacc_nss_day3,  found_vacc_nss_day4, found_vacc_nss_day5, found_vacc_nss_day6, found_vacc_nss_day7,
                                                                 re_vacc_nss_day1, re_vacc_nss_day2, re_vacc_nss_day3,  re_vacc_nss_day4, re_vacc_nss_day5, re_vacc_nss_day6, re_vacc_nss_day7, na.rm=T),
                     recorded_missed_reason_refusal_vaccinated = sum(f_ound_vacc_refusal_day1, f_ound_vacc_refusal_day2, f_ound_vacc_refusal_day3, f_ound_vacc_refusal_day4, f_ound_vacc_refusal_day5, f_ound_vacc_refusal_day6, f_ound_vacc_refusal_day7,
                                                                     re_vacc_refusal_day1, re_vacc_refusal_day2, re_vacc_refusal_day3,  re_vacc_refusal_day4, re_vacc_refusal_day5, re_vacc_refusal_day6, re_vacc_refusal_day7, na.rm=T),
                     recorded_missed_day1_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day1, absent_vaccinated_by_team_during_c_day1,
                                                           found_vaccinated_absent_day1, absent_vaccinated_by_team_after_c_day1,
                                                           found_vacc_nss_day1, re_vacc_nss_day1,
                                                           f_ound_vacc_refusal_day1, re_vacc_refusal_day1, na.rm=T),
                     recorded_missed_day2_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day2, absent_vaccinated_by_team_during_c_day2,
                                                           found_vaccinated_absent_day2, absent_vaccinated_by_team_after_c_day2,
                                                           found_vacc_nss_day2, re_vacc_nss_day2,
                                                           f_ound_vacc_refusal_day2, re_vacc_refusal_day2, na.rm=T),
                     recorded_missed_day3_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day3, absent_vaccinated_by_team_during_c_day3,
                                                           found_vaccinated_absent_day3, absent_vaccinated_by_team_after_c_day3,
                                                           found_vacc_nss_day3, re_vacc_nss_day3,
                                                           f_ound_vacc_refusal_day3, re_vacc_refusal_day3, na.rm=T),
                     recorded_missed_day4_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day4, absent_vaccinated_by_team_during_c_day4,
                                                           found_vaccinated_absent_day4, absent_vaccinated_by_team_after_c_day4,
                                                           found_vacc_nss_day4, re_vacc_nss_day4,
                                                           f_ound_vacc_refusal_day4, re_vacc_refusal_day4, na.rm=T),
                     recorded_missed_day5_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day5, absent_vaccinated_by_team_during_c_day5,
                                                           found_vaccinated_absent_day5, absent_vaccinated_by_team_after_c_day5,
                                                           found_vacc_nss_day5, re_vacc_nss_day5,
                                                           f_ound_vacc_refusal_day5, re_vacc_refusal_day5, na.rm=T),
                     recorded_missed_day6_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day6, absent_vaccinated_by_team_during_c_day6,
                                                           found_vaccinated_absent_day6, absent_vaccinated_by_team_after_c_day6,
                                                           found_vacc_nss_day6, re_vacc_nss_day6,
                                                           f_ound_vacc_refusal_day6, re_vacc_refusal_day6, na.rm=T),
                     recorded_missed_revisit_day_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day7, absent_vaccinated_by_team_during_c_day7,
                                                                  found_vaccinated_absent_day7, absent_vaccinated_by_team_after_c_day7,
                                                                  found_vacc_nss_day7, re_vacc_nss_day7,
                                                                  f_ound_vacc_refusal_day7, re_vacc_refusal_day7, na.rm=T),
                     recorded_missed_total = sum(recorded_missed_day1, recorded_missed_day2, recorded_missed_day3, recorded_missed_day4, recorded_missed_day5,recorded_missed_day6, na.rm=T),
                     recorded_missed_total_vaccinated = sum(recorded_missed_day1_vaccinated, recorded_missed_day2_vaccinated, recorded_missed_day3_vaccinated, recorded_missed_day4_vaccinated, recorded_missed_day5_vaccinated, recorded_missed_day6_vaccinated, recorded_missed_revisit_day_vaccinated, na.rm=T)) %>%
              ungroup() %>%
              mutate(age_group = "0-59 Months",
                     campaign_days = 7) %>%
              select(campaigns, rcode, pcode, dcode, ccode, clustername, age_group, campaign_days,
                     recorded_missed_reason_absent1, recorded_missed_reason_absent1_vaccinated,
                     recorded_missed_reason_absent2, recorded_missed_reason_absent2_vaccinated,
                     recorded_missed_reason_nss, recorded_missed_reason_nss_vaccinated,
                     recorded_missed_reason_refusal, recorded_missed_reason_refusal_vaccinated,
                     recorded_missed_day1, recorded_missed_day1_vaccinated,
                     recorded_missed_day2, recorded_missed_day2_vaccinated,
                     recorded_missed_day3, recorded_missed_day3_vaccinated,
                     recorded_missed_day4, recorded_missed_day4_vaccinated,
                     recorded_missed_day5, recorded_missed_day5_vaccinated,
                     recorded_missed_day6, recorded_missed_day6_vaccinated,
                     recorded_missed_revisit_day_vaccinated,
                     recorded_missed_total, recorded_missed_total_vaccinated)
            
  ) %>%
  bind_rows(all_data_list$df_apmis_list$`DC Daily Compilation Day 1-7 (5-10)` %>%
              select(campaigns, rcode, pcode, dcode, ccode, clustername,
                     
                     reasons_absent_return_during_campaign_day1, reasons_absent_return_during_campaign_day2, reasons_absent_return_during_campaign_day3, reasons_absent_return_during_campaign_day4, reasons_absent_return_during_campaign_day5, reasons_absent_return_during_campaign_day6,
                     found_vaccinated_absent_return_during_campaign_day1, found_vaccinated_absent_return_during_campaign_day2, found_vaccinated_absent_return_during_campaign_day3, found_vaccinated_absent_return_during_campaign_day4, found_vaccinated_absent_return_during_campaign_day5, found_vaccinated_absent_return_during_campaign_day6, found_vaccinated_absent_return_during_campaign_day7,
                     absent_vaccinated_by_team_during_c_day1, absent_vaccinated_by_team_during_c_day2, absent_vaccinated_by_team_during_c_day3, absent_vaccinated_by_team_during_c_day4, absent_vaccinated_by_team_during_c_day5, absent_vaccinated_by_team_during_c_day6, absent_vaccinated_by_team_during_c_day7, 
                     
                     recorded_vaccinated_absent_day1, recorded_vaccinated_absent_day2, recorded_vaccinated_absent_day3, recorded_vaccinated_absent_day4, recorded_vaccinated_absent_day5, recorded_vaccinated_absent_day6,
                     found_vaccinated_absent_day1, found_vaccinated_absent_day2, found_vaccinated_absent_day3, found_vaccinated_absent_day4, found_vaccinated_absent_day5, found_vaccinated_absent_day6, found_vaccinated_absent_day7,
                     absent_vaccinated_by_team_after_c_day1, absent_vaccinated_by_team_after_c_day2, absent_vaccinated_by_team_after_c_day3, absent_vaccinated_by_team_after_c_day4, absent_vaccinated_by_team_after_c_day5, absent_vaccinated_by_team_after_c_day6, absent_vaccinated_by_team_after_c_day7,
                     
                     recorded_nss_day1, recorded_nss_day2, recorded_nss_day3, recorded_nss_day4, recorded_nss_day5, recorded_nss_day6, 
                     found_vacc_nss_day1, found_vacc_nss_day2, found_vacc_nss_day3,  found_vacc_nss_day4, found_vacc_nss_day5, found_vacc_nss_day6, found_vacc_nss_day7,
                     re_vacc_nss_day1, re_vacc_nss_day2, re_vacc_nss_day3,  re_vacc_nss_day4, re_vacc_nss_day5, re_vacc_nss_day6, re_vacc_nss_day7,
                     
                     reason_refusal_day1, reason_refusal_day2, reason_refusal_day3, reason_refusal_day4, reason_refusal_day5, reason_refusal_day6,
                     f_ound_vacc_refusal_day1, f_ound_vacc_refusal_day2, f_ound_vacc_refusal_day3, f_ound_vacc_refusal_day4, f_ound_vacc_refusal_day5, f_ound_vacc_refusal_day6, f_ound_vacc_refusal_day7,
                     re_vacc_refusal_day1, re_vacc_refusal_day2, re_vacc_refusal_day3,  re_vacc_refusal_day4, re_vacc_refusal_day5, re_vacc_refusal_day6, re_vacc_refusal_day7) %>%
              mutate(across(-c(campaigns, rcode, pcode, dcode, ccode, clustername), as.numeric)) %>%
              rowwise() %>%
              mutate(recorded_missed_reason_absent1 = sum(reasons_absent_return_during_campaign_day1, reasons_absent_return_during_campaign_day2, reasons_absent_return_during_campaign_day3, reasons_absent_return_during_campaign_day4, reasons_absent_return_during_campaign_day5, reasons_absent_return_during_campaign_day6, na.rm=T),
                     recorded_missed_reason_absent2 = sum(recorded_vaccinated_absent_day1, recorded_vaccinated_absent_day2, recorded_vaccinated_absent_day3, recorded_vaccinated_absent_day4, recorded_vaccinated_absent_day5, recorded_vaccinated_absent_day6, na.rm=T),
                     recorded_missed_reason_nss = sum(recorded_nss_day1, recorded_nss_day2, recorded_nss_day3, recorded_nss_day4, recorded_nss_day5, recorded_nss_day6, na.rm=T),
                     recorded_missed_reason_refusal = sum(reason_refusal_day1, reason_refusal_day2, reason_refusal_day3, reason_refusal_day4, reason_refusal_day5, reason_refusal_day6, na.rm=T),
                     recorded_missed_day1 = sum(reasons_absent_return_during_campaign_day1, recorded_vaccinated_absent_day1, recorded_nss_day1, reason_refusal_day1, na.rm=T),
                     recorded_missed_day2 = sum(reasons_absent_return_during_campaign_day2, recorded_vaccinated_absent_day2, recorded_nss_day2, reason_refusal_day2, na.rm=T),
                     recorded_missed_day3 = sum(reasons_absent_return_during_campaign_day3, recorded_vaccinated_absent_day3, recorded_nss_day3, reason_refusal_day3, na.rm=T),
                     recorded_missed_day4 = sum(reasons_absent_return_during_campaign_day4, recorded_vaccinated_absent_day4, recorded_nss_day4, reason_refusal_day4, na.rm=T),
                     recorded_missed_day5 = sum(reasons_absent_return_during_campaign_day5, recorded_vaccinated_absent_day5, recorded_nss_day5, reason_refusal_day5, na.rm=T),
                     recorded_missed_day6 = sum(reasons_absent_return_during_campaign_day6, recorded_vaccinated_absent_day6, recorded_nss_day6, reason_refusal_day6, na.rm=T),
                     recorded_missed_reason_absent1_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day1, found_vaccinated_absent_return_during_campaign_day2, found_vaccinated_absent_return_during_campaign_day3, found_vaccinated_absent_return_during_campaign_day4, found_vaccinated_absent_return_during_campaign_day5, found_vaccinated_absent_return_during_campaign_day6, found_vaccinated_absent_return_during_campaign_day7,
                                                                     absent_vaccinated_by_team_during_c_day1, absent_vaccinated_by_team_during_c_day2, absent_vaccinated_by_team_during_c_day3, absent_vaccinated_by_team_during_c_day4, absent_vaccinated_by_team_during_c_day5, absent_vaccinated_by_team_during_c_day6, absent_vaccinated_by_team_during_c_day7, na.rm=T),
                     recorded_missed_reason_absent2_vaccinated = sum(found_vaccinated_absent_day1, found_vaccinated_absent_day2, found_vaccinated_absent_day3, found_vaccinated_absent_day4, found_vaccinated_absent_day5, found_vaccinated_absent_day6, found_vaccinated_absent_day7,
                                                                     absent_vaccinated_by_team_after_c_day1, absent_vaccinated_by_team_after_c_day2, absent_vaccinated_by_team_after_c_day3, absent_vaccinated_by_team_after_c_day4, absent_vaccinated_by_team_after_c_day5, absent_vaccinated_by_team_after_c_day6, absent_vaccinated_by_team_after_c_day7, na.rm=T),
                     recorded_missed_reason_nss_vaccinated = sum(found_vacc_nss_day1, found_vacc_nss_day2, found_vacc_nss_day3,  found_vacc_nss_day4, found_vacc_nss_day5, found_vacc_nss_day6, found_vacc_nss_day7,
                                                                 re_vacc_nss_day1, re_vacc_nss_day2, re_vacc_nss_day3,  re_vacc_nss_day4, re_vacc_nss_day5, re_vacc_nss_day6, re_vacc_nss_day7, na.rm=T),
                     recorded_missed_reason_refusal_vaccinated = sum(f_ound_vacc_refusal_day1, f_ound_vacc_refusal_day2, f_ound_vacc_refusal_day3, f_ound_vacc_refusal_day4, f_ound_vacc_refusal_day5, f_ound_vacc_refusal_day6, f_ound_vacc_refusal_day7,
                                                                     re_vacc_refusal_day1, re_vacc_refusal_day2, re_vacc_refusal_day3,  re_vacc_refusal_day4, re_vacc_refusal_day5, re_vacc_refusal_day6, re_vacc_refusal_day7, na.rm=T),
                     recorded_missed_day1_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day1, absent_vaccinated_by_team_during_c_day1,
                                                           found_vaccinated_absent_day1, absent_vaccinated_by_team_after_c_day1,
                                                           found_vacc_nss_day1, re_vacc_nss_day1,
                                                           f_ound_vacc_refusal_day1, re_vacc_refusal_day1, na.rm=T),
                     recorded_missed_day2_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day2, absent_vaccinated_by_team_during_c_day2,
                                                           found_vaccinated_absent_day2, absent_vaccinated_by_team_after_c_day2,
                                                           found_vacc_nss_day2, re_vacc_nss_day2,
                                                           f_ound_vacc_refusal_day2, re_vacc_refusal_day2, na.rm=T),
                     recorded_missed_day3_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day3, absent_vaccinated_by_team_during_c_day3,
                                                           found_vaccinated_absent_day3, absent_vaccinated_by_team_after_c_day3,
                                                           found_vacc_nss_day3, re_vacc_nss_day3,
                                                           f_ound_vacc_refusal_day3, re_vacc_refusal_day3, na.rm=T),
                     recorded_missed_day4_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day4, absent_vaccinated_by_team_during_c_day4,
                                                           found_vaccinated_absent_day4, absent_vaccinated_by_team_after_c_day4,
                                                           found_vacc_nss_day4, re_vacc_nss_day4,
                                                           f_ound_vacc_refusal_day4, re_vacc_refusal_day4, na.rm=T),
                     recorded_missed_day5_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day5, absent_vaccinated_by_team_during_c_day5,
                                                           found_vaccinated_absent_day5, absent_vaccinated_by_team_after_c_day5,
                                                           found_vacc_nss_day5, re_vacc_nss_day5,
                                                           f_ound_vacc_refusal_day5, re_vacc_refusal_day5, na.rm=T),
                     recorded_missed_day6_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day6, absent_vaccinated_by_team_during_c_day6,
                                                           found_vaccinated_absent_day6, absent_vaccinated_by_team_after_c_day6,
                                                           found_vacc_nss_day6, re_vacc_nss_day6,
                                                           f_ound_vacc_refusal_day6, re_vacc_refusal_day6, na.rm=T),
                     recorded_missed_revisit_day_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day7, absent_vaccinated_by_team_during_c_day7,
                                                                  found_vaccinated_absent_day7, absent_vaccinated_by_team_after_c_day7,
                                                                  found_vacc_nss_day7, re_vacc_nss_day7,
                                                                  f_ound_vacc_refusal_day7, re_vacc_refusal_day7, na.rm=T),
                     recorded_missed_total = sum(recorded_missed_day1, recorded_missed_day2, recorded_missed_day3, recorded_missed_day4, recorded_missed_day5,recorded_missed_day6, na.rm=T),
                     recorded_missed_total_vaccinated = sum(recorded_missed_day1_vaccinated, recorded_missed_day2_vaccinated, recorded_missed_day3_vaccinated, recorded_missed_day4_vaccinated, recorded_missed_day5_vaccinated, recorded_missed_day6_vaccinated, recorded_missed_revisit_day_vaccinated, na.rm=T)) %>%
              ungroup() %>%
              mutate(age_group = "5-10 Years",
                     campaign_days = 7) %>%
              select(campaigns, rcode, pcode, dcode, ccode, clustername, age_group, campaign_days,
                     recorded_missed_reason_absent1, recorded_missed_reason_absent1_vaccinated,
                     recorded_missed_reason_absent2, recorded_missed_reason_absent2_vaccinated,
                     recorded_missed_reason_nss, recorded_missed_reason_nss_vaccinated,
                     recorded_missed_reason_refusal, recorded_missed_reason_refusal_vaccinated,
                     recorded_missed_day1, recorded_missed_day1_vaccinated,
                     recorded_missed_day2, recorded_missed_day2_vaccinated,
                     recorded_missed_day3, recorded_missed_day3_vaccinated,
                     recorded_missed_day4, recorded_missed_day4_vaccinated,
                     recorded_missed_day5, recorded_missed_day5_vaccinated,
                     recorded_missed_day6, recorded_missed_day6_vaccinated,
                     recorded_missed_revisit_day_vaccinated,
                     recorded_missed_total, recorded_missed_total_vaccinated)
  ) %>%
  # bind_rows(all_data_list$df_apmis_list$`East SIA DC Daily Compilation Day 1-3` %>%
  #             select(campaigns, rcode, pcode, dcode, ccode, clustername,
  #                    
  #                    missed_childrenfound011m_day1, missed_childrenfound1259m_day1,
  #                    missed_childrenfound011m_day2, missed_childrenfound1259m_day2,
  #                    missed_childrenfound011m_day3, missed_childrenfound1259m_day3,
  #                    revisit_children_vaccinated011m_day1,revisit_children_vaccinated1259m_day1,
  #                    revisit_children_vaccinated011m_day2,revisit_children_vaccinated1259m_day2,
  #                    revisit_children_vaccinated011m_day3,revisit_children_vaccinated1259m_day3) %>%
  #             mutate(across(-c(campaigns, rcode, pcode, dcode, ccode, clustername), as.numeric)) %>%
  #             rowwise() %>%
  #             mutate(recorded_missed_day1 = sum(missed_childrenfound011m_day1, missed_childrenfound1259m_day1, na.rm=T),
  #                    recorded_missed_day2 = sum(missed_childrenfound011m_day2, missed_childrenfound1259m_day2, na.rm=T),
  #                    recorded_missed_day3 = sum(missed_childrenfound011m_day3, missed_childrenfound1259m_day3, na.rm=T),
  #                    
  #                    recorded_missed_day1_vaccinated = sum(revisit_children_vaccinated011m_day1,revisit_children_vaccinated1259m_day1, na.rm=T),
  #                    recorded_missed_day2_vaccinated = sum(revisit_children_vaccinated011m_day2,revisit_children_vaccinated1259m_day2, na.rm=T),
  #                    recorded_missed_day3_vaccinated = sum(revisit_children_vaccinated011m_day3,revisit_children_vaccinated1259m_day3, na.rm=T),
  #                    
  #                    recorded_missed_total = sum(recorded_missed_day1, recorded_missed_day2, recorded_missed_day3, na.rm=T),
  #                    recorded_missed_total_vaccinated = sum(recorded_missed_day1_vaccinated, recorded_missed_day2_vaccinated, recorded_missed_day3_vaccinated, na.rm=T)) %>%
  #             ungroup() %>%
  #             mutate(age_group = "0-59 Months",
  #                    campaign_days = 3) %>%
  #             select(campaigns, rcode, pcode, dcode, ccode, clustername, age_group, campaign_days,
  #                    recorded_missed_day1, recorded_missed_day1_vaccinated,
  #                    recorded_missed_day2, recorded_missed_day2_vaccinated,
  #                    recorded_missed_day3, recorded_missed_day3_vaccinated,
  #                    recorded_missed_total, recorded_missed_total_vaccinated)
  # ) %>%
  bind_rows(all_data_list$df_apmis_list$`East SIA DC Daily Compilation Day 1-4` %>%
              select(campaigns, rcode, pcode, dcode, ccode, clustername,
                     
                     reasons_absent_return_during_campaign_day1, reasons_absent_return_during_campaign_day2, reasons_absent_return_during_campaign_day3,
                     found_vaccinated_absent_return_during_campaign_day1, found_vaccinated_absent_return_during_campaign_day2, found_vaccinated_absent_return_during_campaign_day3, found_vaccinated_absent_return_during_campaign_day4, 
                     absent_vaccinated_by_team_during_c_day1, absent_vaccinated_by_team_during_c_day2, absent_vaccinated_by_team_during_c_day3, absent_vaccinated_by_team_during_c_day4,
                     
                     recorded_vaccinated_absent_day1, recorded_vaccinated_absent_day2, recorded_vaccinated_absent_day3,
                     found_vaccinated_absent_day1, found_vaccinated_absent_day2, found_vaccinated_absent_day3, found_vaccinated_absent_day4, 
                     absent_vaccinated_by_team_after_c_day1, absent_vaccinated_by_team_after_c_day2, absent_vaccinated_by_team_after_c_day3, absent_vaccinated_by_team_after_c_day4,
                     
                     recorded_nss_day1, recorded_nss_day2, recorded_nss_day3,
                     found_vacc_nss_day1, found_vacc_nss_day2, found_vacc_nss_day3,  found_vacc_nss_day4, 
                     re_vacc_nss_day1, re_vacc_nss_day2, re_vacc_nss_day3,  re_vacc_nss_day4,
                     
                     reason_refusal_day1, reason_refusal_day2, reason_refusal_day3,
                     f_ound_vacc_refusal_day1, f_ound_vacc_refusal_day2, f_ound_vacc_refusal_day3, f_ound_vacc_refusal_day4, 
                     re_vacc_refusal_day1, re_vacc_refusal_day2, re_vacc_refusal_day3,  re_vacc_refusal_day4) %>%
              mutate(across(-c(campaigns, rcode, pcode, dcode, ccode, clustername), as.numeric)) %>%
              rowwise() %>%
              mutate(recorded_missed_reason_absent1 = sum(reasons_absent_return_during_campaign_day1, reasons_absent_return_during_campaign_day2, reasons_absent_return_during_campaign_day3, na.rm=T),
                     recorded_missed_reason_absent2 = sum(recorded_vaccinated_absent_day1, recorded_vaccinated_absent_day2, recorded_vaccinated_absent_day3, na.rm=T),
                     recorded_missed_reason_nss = sum(recorded_nss_day1, recorded_nss_day2, recorded_nss_day3, na.rm=T),
                     recorded_missed_reason_refusal = sum(reason_refusal_day1, reason_refusal_day2, reason_refusal_day3, na.rm=T),
                     recorded_missed_day1 = sum(reasons_absent_return_during_campaign_day1, recorded_vaccinated_absent_day1, recorded_nss_day1, reason_refusal_day1, na.rm=T),
                     recorded_missed_day2 = sum(reasons_absent_return_during_campaign_day2, recorded_vaccinated_absent_day2, recorded_nss_day2, reason_refusal_day2, na.rm=T),
                     recorded_missed_day3 = sum(reasons_absent_return_during_campaign_day3, recorded_vaccinated_absent_day3, recorded_nss_day3, reason_refusal_day3, na.rm=T),
                     recorded_missed_reason_absent1_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day1, found_vaccinated_absent_return_during_campaign_day2, found_vaccinated_absent_return_during_campaign_day3, found_vaccinated_absent_return_during_campaign_day4, 
                                                                     absent_vaccinated_by_team_during_c_day1, absent_vaccinated_by_team_during_c_day2, absent_vaccinated_by_team_during_c_day3, absent_vaccinated_by_team_during_c_day4, na.rm=T),
                     recorded_missed_reason_absent2_vaccinated = sum(found_vaccinated_absent_day1, found_vaccinated_absent_day2, found_vaccinated_absent_day3, found_vaccinated_absent_day4, 
                                                                     absent_vaccinated_by_team_after_c_day1, absent_vaccinated_by_team_after_c_day2, absent_vaccinated_by_team_after_c_day3, absent_vaccinated_by_team_after_c_day4, na.rm=T),
                     recorded_missed_reason_nss_vaccinated = sum(found_vacc_nss_day1, found_vacc_nss_day2, found_vacc_nss_day3,  found_vacc_nss_day4, 
                                                                 re_vacc_nss_day1, re_vacc_nss_day2, re_vacc_nss_day3,  re_vacc_nss_day4, na.rm=T),
                     recorded_missed_reason_refusal_vaccinated = sum(f_ound_vacc_refusal_day1, f_ound_vacc_refusal_day2, f_ound_vacc_refusal_day3, f_ound_vacc_refusal_day4, 
                                                                     re_vacc_refusal_day1, re_vacc_refusal_day2, re_vacc_refusal_day3,  re_vacc_refusal_day4, na.rm=T),
                     recorded_missed_day1_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day1, absent_vaccinated_by_team_during_c_day1,
                                                           found_vaccinated_absent_day1, absent_vaccinated_by_team_after_c_day1,
                                                           found_vacc_nss_day1, re_vacc_nss_day1,
                                                           f_ound_vacc_refusal_day1, re_vacc_refusal_day1, na.rm=T),
                     recorded_missed_day2_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day2, absent_vaccinated_by_team_during_c_day2,
                                                           found_vaccinated_absent_day2, absent_vaccinated_by_team_after_c_day2,
                                                           found_vacc_nss_day2, re_vacc_nss_day2,
                                                           f_ound_vacc_refusal_day2, re_vacc_refusal_day2, na.rm=T),
                     recorded_missed_day3_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day3, absent_vaccinated_by_team_during_c_day3,
                                                           found_vaccinated_absent_day3, absent_vaccinated_by_team_after_c_day3,
                                                           found_vacc_nss_day3, re_vacc_nss_day3,
                                                           f_ound_vacc_refusal_day3, re_vacc_refusal_day3, na.rm=T),
                     recorded_missed_revisit_day_vaccinated = sum(found_vaccinated_absent_return_during_campaign_day4, absent_vaccinated_by_team_during_c_day4,
                                                                  found_vaccinated_absent_day4, absent_vaccinated_by_team_after_c_day4,
                                                                  found_vacc_nss_day4, re_vacc_nss_day4,
                                                                  f_ound_vacc_refusal_day4, re_vacc_refusal_day4, na.rm=T),
                     recorded_missed_total = sum(recorded_missed_day1, recorded_missed_day2, recorded_missed_day3, na.rm=T),
                     recorded_missed_total_vaccinated = sum(recorded_missed_day1_vaccinated, recorded_missed_day2_vaccinated, recorded_missed_day3_vaccinated, recorded_missed_revisit_day_vaccinated, na.rm=T)) %>%
              ungroup() %>%
              mutate(age_group = "0-59 Months",
                     campaign_days = 4) %>%
              select(campaigns, rcode, pcode, dcode, ccode, clustername, age_group, campaign_days,
                     recorded_missed_reason_absent1, recorded_missed_reason_absent1_vaccinated,
                     recorded_missed_reason_absent2, recorded_missed_reason_absent2_vaccinated,
                     recorded_missed_reason_nss, recorded_missed_reason_nss_vaccinated,
                     recorded_missed_reason_refusal, recorded_missed_reason_refusal_vaccinated,
                     recorded_missed_day1, recorded_missed_day1_vaccinated,
                     recorded_missed_day2, recorded_missed_day2_vaccinated,
                     recorded_missed_day3, recorded_missed_day3_vaccinated,
                     recorded_missed_revisit_day_vaccinated,
                     recorded_missed_total, recorded_missed_total_vaccinated)
  ) %>%
  bind_rows(all_data_list$df_apmis_list$`fIPV OPV DC Daily Compilation Day 1-7` %>%
              select(campaigns, rcode, pcode, dcode, ccode, clustername,
                     
                     opv_reasons_absent_return_during_campaign_day1, opv_reasons_absent_return_during_campaign_day2, opv_reasons_absent_return_during_campaign_day3, opv_reasons_absent_return_during_campaign_day4, opv_reasons_absent_return_during_campaign_day5, opv_reasons_absent_return_during_campaign_day6, opv_reasons_absent_return_during_campaign_day7,
                     opv_found_vaccinated_absent_return_during_campaign_day1, opv_found_vaccinated_absent_return_during_campaign_day2, opv_found_vaccinated_absent_return_during_campaign_day3, opv_found_vaccinated_absent_return_during_campaign_day4, opv_found_vaccinated_absent_return_during_campaign_day5, opv_found_vaccinated_absent_return_during_campaign_day6, opv_found_vaccinated_absent_return_during_campaign_day7,
                     opv_absent_vaccinated_by_team_during_c_day1, opv_absent_vaccinated_by_team_during_c_day2, opv_absent_vaccinated_by_team_during_c_day3, opv_absent_vaccinated_by_team_during_c_day4, opv_absent_vaccinated_by_team_during_c_day5, opv_absent_vaccinated_by_team_during_c_day6, opv_absent_vaccinated_by_team_during_c_day7,
                     
                     opv_recorded_vaccinated_absent_day1, opv_recorded_vaccinated_absent_day2, opv_recorded_vaccinated_absent_day3, opv_recorded_vaccinated_absent_day4, opv_recorded_vaccinated_absent_day5, opv_recorded_vaccinated_absent_day6, opv_recorded_vaccinated_absent_day7,
                     opv_found_vaccinated_absent_day1, opv_found_vaccinated_absent_day2, opv_found_vaccinated_absent_day3, opv_found_vaccinated_absent_day4, opv_found_vaccinated_absent_day5, opv_found_vaccinated_absent_day6, opv_found_vaccinated_absent_day7, 
                     opv_absent_vaccinated_by_team_after_c_day1, opv_absent_vaccinated_by_team_after_c_day2, opv_absent_vaccinated_by_team_after_c_day3, opv_absent_vaccinated_by_team_after_c_day4, opv_absent_vaccinated_by_team_after_c_day5, opv_absent_vaccinated_by_team_after_c_day6, opv_absent_vaccinated_by_team_after_c_day7,
                     
                     opv_recorded_nss_day1, opv_recorded_nss_day2, opv_recorded_nss_day3, opv_recorded_nss_day4, opv_recorded_nss_day5, opv_recorded_nss_day6, opv_recorded_nss_day7,
                     opv_found_vacc_nss_day1, opv_found_vacc_nss_day2, opv_found_vacc_nss_day3,  opv_found_vacc_nss_day4, opv_found_vacc_nss_day5, opv_found_vacc_nss_day6, opv_found_vacc_nss_day7,
                     opv_re_vacc_nss_day1, opv_re_vacc_nss_day2, opv_re_vacc_nss_day3,  opv_re_vacc_nss_day4, opv_re_vacc_nss_day5, opv_re_vacc_nss_day6, opv_re_vacc_nss_day7,
                     
                     opv_reason_refusal_day1, opv_reason_refusal_day2, opv_reason_refusal_day3, opv_reason_refusal_day4, opv_reason_refusal_day5, opv_reason_refusal_day6, opv_reason_refusal_day7,
                     opvf_ound_vacc_refusal_day1, opvf_ound_vacc_refusal_day2, opvf_ound_vacc_refusal_day3, opvf_ound_vacc_refusal_day4, opvf_ound_vacc_refusal_day4, opvf_ound_vacc_refusal_day5, opvf_ound_vacc_refusal_day6, opvf_ound_vacc_refusal_day7,
                     opv_re_vacc_refusal_day1, opv_re_vacc_refusal_day2, opv_re_vacc_refusal_day3,  opv_re_vacc_refusal_day4, opv_re_vacc_refusal_day5, opv_re_vacc_refusal_day6, opv_re_vacc_refusal_day7) %>%
              mutate(across(-c(campaigns, rcode, pcode, dcode, ccode, clustername), as.numeric)) %>%
              rowwise() %>%
              mutate(recorded_missed_reason_absent1 = sum(opv_reasons_absent_return_during_campaign_day1, opv_reasons_absent_return_during_campaign_day2, opv_reasons_absent_return_during_campaign_day3, opv_reasons_absent_return_during_campaign_day4, opv_reasons_absent_return_during_campaign_day5, opv_reasons_absent_return_during_campaign_day6, na.rm=T),
                     recorded_missed_reason_absent2 = sum(opv_recorded_vaccinated_absent_day1, opv_recorded_vaccinated_absent_day2, opv_recorded_vaccinated_absent_day3, opv_recorded_vaccinated_absent_day4, opv_recorded_vaccinated_absent_day5, opv_recorded_vaccinated_absent_day6, na.rm=T),
                     recorded_missed_reason_nss = sum(opv_recorded_nss_day1, opv_recorded_nss_day2, opv_recorded_nss_day3, opv_recorded_nss_day4, opv_recorded_nss_day5, opv_recorded_nss_day6, na.rm=T),
                     recorded_missed_reason_refusal = sum(opv_reason_refusal_day1, opv_reason_refusal_day2, opv_reason_refusal_day3, opv_reason_refusal_day4, opv_reason_refusal_day5, opv_reason_refusal_day6, na.rm=T),
                     recorded_missed_day1 = sum(opv_reasons_absent_return_during_campaign_day1, opv_recorded_vaccinated_absent_day1, opv_recorded_nss_day1, opv_reason_refusal_day1, na.rm=T),
                     recorded_missed_day2 = sum(opv_reasons_absent_return_during_campaign_day2, opv_recorded_vaccinated_absent_day2, opv_recorded_nss_day2, opv_reason_refusal_day2, na.rm=T),
                     recorded_missed_day3 = sum(opv_reasons_absent_return_during_campaign_day3, opv_recorded_vaccinated_absent_day3, opv_recorded_nss_day3, opv_reason_refusal_day3, na.rm=T),
                     recorded_missed_day4 = sum(opv_reasons_absent_return_during_campaign_day4, opv_recorded_vaccinated_absent_day4, opv_recorded_nss_day4, opv_reason_refusal_day4, na.rm=T),
                     recorded_missed_day5 = sum(opv_reasons_absent_return_during_campaign_day5, opv_recorded_vaccinated_absent_day5, opv_recorded_nss_day5, opv_reason_refusal_day5, na.rm=T),
                     recorded_missed_day6 = sum(opv_reasons_absent_return_during_campaign_day6, opv_recorded_vaccinated_absent_day6, opv_recorded_nss_day6, opv_reason_refusal_day6, na.rm=T),
                     # recorded_missed_day7 = sum(opv_reasons_absent_return_during_campaign_day7, opv_recorded_vaccinated_absent_day7, opv_recorded_nss_day7, opv_reason_refusal_day7, na.rm=T),
                     recorded_missed_reason_absent1_vaccinated = sum(opv_found_vaccinated_absent_return_during_campaign_day1, opv_found_vaccinated_absent_return_during_campaign_day2, opv_found_vaccinated_absent_return_during_campaign_day3, opv_found_vaccinated_absent_return_during_campaign_day4, opv_found_vaccinated_absent_return_during_campaign_day5, opv_found_vaccinated_absent_return_during_campaign_day6, opv_found_vaccinated_absent_return_during_campaign_day7, 
                                                                     opv_absent_vaccinated_by_team_during_c_day1, opv_absent_vaccinated_by_team_during_c_day2, opv_absent_vaccinated_by_team_during_c_day3, opv_absent_vaccinated_by_team_during_c_day4, opv_absent_vaccinated_by_team_during_c_day5, opv_absent_vaccinated_by_team_during_c_day6, opv_absent_vaccinated_by_team_during_c_day7, na.rm=T),
                     recorded_missed_reason_absent2_vaccinated = sum(opv_found_vaccinated_absent_day1, opv_found_vaccinated_absent_day2, opv_found_vaccinated_absent_day3, opv_found_vaccinated_absent_day4, opv_found_vaccinated_absent_day5, opv_found_vaccinated_absent_day6, opv_found_vaccinated_absent_day7, 
                                                                     opv_absent_vaccinated_by_team_after_c_day1, opv_absent_vaccinated_by_team_after_c_day2, opv_absent_vaccinated_by_team_after_c_day3, opv_absent_vaccinated_by_team_after_c_day4, opv_absent_vaccinated_by_team_after_c_day5, opv_absent_vaccinated_by_team_after_c_day6, opv_absent_vaccinated_by_team_after_c_day7, na.rm=T),
                     recorded_missed_reason_nss_vaccinated = sum(opv_found_vacc_nss_day1, opv_found_vacc_nss_day2, opv_found_vacc_nss_day3,  opv_found_vacc_nss_day4,  opv_found_vacc_nss_day5, opv_found_vacc_nss_day6, opv_found_vacc_nss_day7,
                                                                 opv_re_vacc_nss_day1, opv_re_vacc_nss_day2, opv_re_vacc_nss_day3,  opv_re_vacc_nss_day4, opv_re_vacc_nss_day5, opv_re_vacc_nss_day6, opv_re_vacc_nss_day7, na.rm=T),
                     recorded_missed_reason_refusal_vaccinated = sum(opvf_ound_vacc_refusal_day1, opvf_ound_vacc_refusal_day2, opvf_ound_vacc_refusal_day3, opvf_ound_vacc_refusal_day4, opvf_ound_vacc_refusal_day5, opvf_ound_vacc_refusal_day6, opvf_ound_vacc_refusal_day7,
                                                                     opv_re_vacc_refusal_day1, opv_re_vacc_refusal_day2, opv_re_vacc_refusal_day3,  opv_re_vacc_refusal_day4, opv_re_vacc_refusal_day5, opv_re_vacc_refusal_day6, opv_re_vacc_refusal_day7, na.rm=T),
                     recorded_missed_day1_vaccinated = sum(opv_found_vaccinated_absent_return_during_campaign_day1, 
                                                           opv_absent_vaccinated_by_team_during_c_day1,
                                                           opv_found_vaccinated_absent_day1, 
                                                           opv_absent_vaccinated_by_team_after_c_day1,
                                                           opv_found_vacc_nss_day1, 
                                                           opv_re_vacc_nss_day1,
                                                           opvf_ound_vacc_refusal_day1, 
                                                           opv_re_vacc_refusal_day1, na.rm=T),
                     recorded_missed_day2_vaccinated = sum(opv_found_vaccinated_absent_return_during_campaign_day2, 
                                                           opv_absent_vaccinated_by_team_during_c_day2,
                                                           opv_found_vaccinated_absent_day2, 
                                                           opv_absent_vaccinated_by_team_after_c_day2,
                                                           opv_found_vacc_nss_day2, 
                                                           opv_re_vacc_nss_day2,
                                                           opvf_ound_vacc_refusal_day2, 
                                                           opv_re_vacc_refusal_day2, na.rm=T),
                     recorded_missed_day3_vaccinated = sum(opv_found_vaccinated_absent_return_during_campaign_day3, 
                                                           opv_absent_vaccinated_by_team_during_c_day3,
                                                           opv_found_vaccinated_absent_day3, 
                                                           opv_absent_vaccinated_by_team_after_c_day3,
                                                           opv_found_vacc_nss_day3, 
                                                           opv_re_vacc_nss_day3,
                                                           opvf_ound_vacc_refusal_day3, 
                                                           opv_re_vacc_refusal_day3, na.rm=T),
                     recorded_missed_day4_vaccinated = sum(opv_found_vaccinated_absent_return_during_campaign_day4, 
                                                           opv_absent_vaccinated_by_team_during_c_day4,
                                                           opv_found_vaccinated_absent_day4, 
                                                           opv_absent_vaccinated_by_team_after_c_day4,
                                                           opv_found_vacc_nss_day4, 
                                                           opv_re_vacc_nss_day4,
                                                           opvf_ound_vacc_refusal_day4, 
                                                           opv_re_vacc_refusal_day4, na.rm=T),
                     recorded_missed_day5_vaccinated = sum(opv_found_vaccinated_absent_return_during_campaign_day5, 
                                                           opv_absent_vaccinated_by_team_during_c_day5,
                                                           opv_found_vaccinated_absent_day5, 
                                                           opv_absent_vaccinated_by_team_after_c_day5,
                                                           opv_found_vacc_nss_day5, 
                                                           opv_re_vacc_nss_day5,
                                                           opvf_ound_vacc_refusal_day5, 
                                                           opv_re_vacc_refusal_day5, na.rm=T),
                     recorded_missed_day6_vaccinated = sum(opv_found_vaccinated_absent_return_during_campaign_day6, 
                                                           opv_absent_vaccinated_by_team_during_c_day6,
                                                           opv_found_vaccinated_absent_day6, 
                                                           opv_absent_vaccinated_by_team_after_c_day6,
                                                           opv_found_vacc_nss_day6, 
                                                           opv_re_vacc_nss_day6,
                                                           opvf_ound_vacc_refusal_day6, 
                                                           opv_re_vacc_refusal_day6, na.rm=T),
                     recorded_missed_revisit_day_vaccinated = sum(opv_found_vaccinated_absent_return_during_campaign_day7, 
                                                           opv_absent_vaccinated_by_team_during_c_day7,
                                                           opv_found_vaccinated_absent_day7, 
                                                           opv_absent_vaccinated_by_team_after_c_day7,
                                                           opv_found_vacc_nss_day7, 
                                                           opv_re_vacc_nss_day7,
                                                           opvf_ound_vacc_refusal_day7, 
                                                           opv_re_vacc_refusal_day7, na.rm=T),
                     recorded_missed_total = sum(recorded_missed_day1, recorded_missed_day2, recorded_missed_day3, recorded_missed_day4, recorded_missed_day5, recorded_missed_day6, na.rm=T),
                     recorded_missed_total_vaccinated = sum(recorded_missed_day1_vaccinated, recorded_missed_day2_vaccinated, recorded_missed_day3_vaccinated, recorded_missed_day4_vaccinated, recorded_missed_day5_vaccinated, recorded_missed_day6_vaccinated, recorded_missed_revisit_day_vaccinated, na.rm=T)) %>%
              ungroup() %>%
              mutate(age_group = "0-59 Months",
                     campaign_days = 7) %>%
              select(campaigns, rcode, pcode, dcode, ccode, clustername, age_group, campaign_days,
                     recorded_missed_reason_absent1, recorded_missed_reason_absent1_vaccinated,
                     recorded_missed_reason_absent2, recorded_missed_reason_absent2_vaccinated,
                     recorded_missed_reason_nss, recorded_missed_reason_nss_vaccinated,
                     recorded_missed_reason_refusal, recorded_missed_reason_refusal_vaccinated,
                     recorded_missed_day1, recorded_missed_day1_vaccinated,
                     recorded_missed_day2, recorded_missed_day2_vaccinated,
                     recorded_missed_day3, recorded_missed_day3_vaccinated,
                     recorded_missed_day4, recorded_missed_day4_vaccinated,
                     recorded_missed_day5, recorded_missed_day5_vaccinated,
                     recorded_missed_day6, recorded_missed_day6_vaccinated,
                     recorded_missed_revisit_day_vaccinated,
                     recorded_missed_total, recorded_missed_total_vaccinated)
  ) %>%
  mutate(vaccine_type = "OPV") %>%
  bind_rows(
    all_data_list$df_apmis_list$`fIPV OPV DC Daily Compilation Day 1-7` %>%
      select(campaigns, rcode, pcode, dcode, ccode, clustername,
             
             ipv_reasons_absent_return_during_campaign_day1, ipv_reasons_absent_return_during_campaign_day2, ipv_reasons_absent_return_during_campaign_day3, ipv_reasons_absent_return_during_campaign_day4, ipv_reasons_absent_return_during_campaign_day5, ipv_reasons_absent_return_during_campaign_day6, ipv_reasons_absent_return_during_campaign_day7,
             ipv_found_vaccinated_absent_return_during_campaign_day1, ipv_found_vaccinated_absent_return_during_campaign_day2, ipv_found_vaccinated_absent_return_during_campaign_day3, ipv_found_vaccinated_absent_return_during_campaign_day4, ipv_found_vaccinated_absent_return_during_campaign_day5, ipv_found_vaccinated_absent_return_during_campaign_day6, ipv_found_vaccinated_absent_return_during_campaign_day7,
             ipv_absent_vaccinated_by_team_during_c_day1, ipv_absent_vaccinated_by_team_during_c_day2, ipv_absent_vaccinated_by_team_during_c_day3, ipv_absent_vaccinated_by_team_during_c_day4, ipv_absent_vaccinated_by_team_during_c_day5, ipv_absent_vaccinated_by_team_during_c_day6, ipv_absent_vaccinated_by_team_during_c_day7,
             
             ipv_recorded_vaccinated_absent_day1, ipv_recorded_vaccinated_absent_day2, ipv_recorded_vaccinated_absent_day3, ipv_recorded_vaccinated_absent_day4, ipv_recorded_vaccinated_absent_day5, ipv_recorded_vaccinated_absent_day6, ipv_recorded_vaccinated_absent_day7,
             ipv_found_vaccinated_absent_day1, ipv_found_vaccinated_absent_day2, ipv_found_vaccinated_absent_day3, ipv_found_vaccinated_absent_day4, ipv_found_vaccinated_absent_day5, ipv_found_vaccinated_absent_day6, ipv_found_vaccinated_absent_day7, 
             ipv_absent_vaccinated_by_team_after_c_day1, ipv_absent_vaccinated_by_team_after_c_day2, ipv_absent_vaccinated_by_team_after_c_day3, ipv_absent_vaccinated_by_team_after_c_day4, ipv_absent_vaccinated_by_team_after_c_day5, ipv_absent_vaccinated_by_team_after_c_day6, ipv_absent_vaccinated_by_team_after_c_day7,
             
             ipv_recorded_nss_day1, ipv_recorded_nss_day2, ipv_recorded_nss_day3, ipv_recorded_nss_day4, ipv_recorded_nss_day5, ipv_recorded_nss_day6, ipv_recorded_nss_day7,
             ipv_found_vacc_nss_day1, ipv_found_vacc_nss_day2, ipv_found_vacc_nss_day3,  ipv_found_vacc_nss_day4, ipv_found_vacc_nss_day5, ipv_found_vacc_nss_day6, ipv_found_vacc_nss_day7,
             ipv_re_vacc_nss_day1, ipv_re_vacc_nss_day2, ipv_re_vacc_nss_day3,  ipv_re_vacc_nss_day4, ipv_re_vacc_nss_day5, ipv_re_vacc_nss_day6, ipv_re_vacc_nss_day7,
             
             ipv_reason_refusal_day1, ipv_reason_refusal_day2, ipv_reason_refusal_day3, ipv_reason_refusal_day4, ipv_reason_refusal_day5, ipv_reason_refusal_day6, ipv_reason_refusal_day7,
             ipvf_ound_vacc_refusal_day1, ipvf_ound_vacc_refusal_day2, ipvf_ound_vacc_refusal_day3, ipvf_ound_vacc_refusal_day4, ipvf_ound_vacc_refusal_day4, ipvf_ound_vacc_refusal_day5, ipvf_ound_vacc_refusal_day6, ipvf_ound_vacc_refusal_day7,
             ipv_re_vacc_refusal_day1, ipv_re_vacc_refusal_day2, ipv_re_vacc_refusal_day3,  ipv_re_vacc_refusal_day4, ipv_re_vacc_refusal_day5, ipv_re_vacc_refusal_day6, ipv_re_vacc_refusal_day7) %>%
      mutate(across(-c(campaigns, rcode, pcode, dcode, ccode, clustername), as.numeric)) %>%
      rowwise() %>%
      mutate(recorded_missed_reason_absent1 = sum(ipv_reasons_absent_return_during_campaign_day1, ipv_reasons_absent_return_during_campaign_day2, ipv_reasons_absent_return_during_campaign_day3, ipv_reasons_absent_return_during_campaign_day4, ipv_reasons_absent_return_during_campaign_day5, ipv_reasons_absent_return_during_campaign_day6, na.rm=T),
             recorded_missed_reason_absent2 = sum(ipv_recorded_vaccinated_absent_day1, ipv_recorded_vaccinated_absent_day2, ipv_recorded_vaccinated_absent_day3, ipv_recorded_vaccinated_absent_day4, ipv_recorded_vaccinated_absent_day5, ipv_recorded_vaccinated_absent_day6, na.rm=T),
             recorded_missed_reason_nss = sum(ipv_recorded_nss_day1, ipv_recorded_nss_day2, ipv_recorded_nss_day3, ipv_recorded_nss_day4, ipv_recorded_nss_day5, ipv_recorded_nss_day6, na.rm=T),
             recorded_missed_reason_refusal = sum(ipv_reason_refusal_day1, ipv_reason_refusal_day2, ipv_reason_refusal_day3, ipv_reason_refusal_day4, ipv_reason_refusal_day5, ipv_reason_refusal_day6, na.rm=T),
             recorded_missed_day1 = sum(ipv_reasons_absent_return_during_campaign_day1, ipv_recorded_vaccinated_absent_day1, ipv_recorded_nss_day1, ipv_reason_refusal_day1, na.rm=T),
             recorded_missed_day2 = sum(ipv_reasons_absent_return_during_campaign_day2, ipv_recorded_vaccinated_absent_day2, ipv_recorded_nss_day2, ipv_reason_refusal_day2, na.rm=T),
             recorded_missed_day3 = sum(ipv_reasons_absent_return_during_campaign_day3, ipv_recorded_vaccinated_absent_day3, ipv_recorded_nss_day3, ipv_reason_refusal_day3, na.rm=T),
             recorded_missed_day4 = sum(ipv_reasons_absent_return_during_campaign_day4, ipv_recorded_vaccinated_absent_day4, ipv_recorded_nss_day4, ipv_reason_refusal_day4, na.rm=T),
             recorded_missed_day5 = sum(ipv_reasons_absent_return_during_campaign_day5, ipv_recorded_vaccinated_absent_day5, ipv_recorded_nss_day5, ipv_reason_refusal_day5, na.rm=T),
             recorded_missed_day6 = sum(ipv_reasons_absent_return_during_campaign_day6, ipv_recorded_vaccinated_absent_day6, ipv_recorded_nss_day6, ipv_reason_refusal_day6, na.rm=T),
             # recorded_missed_day7 = sum(ipv_reasons_absent_return_during_campaign_day7, ipv_recorded_vaccinated_absent_day7, ipv_recorded_nss_day7, ipv_reason_refusal_day7, na.rm=T),
             recorded_missed_reason_absent1_vaccinated = sum(ipv_found_vaccinated_absent_return_during_campaign_day1, ipv_found_vaccinated_absent_return_during_campaign_day2, ipv_found_vaccinated_absent_return_during_campaign_day3, ipv_found_vaccinated_absent_return_during_campaign_day4, ipv_found_vaccinated_absent_return_during_campaign_day5, ipv_found_vaccinated_absent_return_during_campaign_day6, ipv_found_vaccinated_absent_return_during_campaign_day7, 
                                                             ipv_absent_vaccinated_by_team_during_c_day1, ipv_absent_vaccinated_by_team_during_c_day2, ipv_absent_vaccinated_by_team_during_c_day3, ipv_absent_vaccinated_by_team_during_c_day4, ipv_absent_vaccinated_by_team_during_c_day5, ipv_absent_vaccinated_by_team_during_c_day6, ipv_absent_vaccinated_by_team_during_c_day7, na.rm=T),
             recorded_missed_reason_absent2_vaccinated = sum(ipv_found_vaccinated_absent_day1, ipv_found_vaccinated_absent_day2, ipv_found_vaccinated_absent_day3, ipv_found_vaccinated_absent_day4, ipv_found_vaccinated_absent_day5, ipv_found_vaccinated_absent_day6, ipv_found_vaccinated_absent_day7, 
                                                             ipv_absent_vaccinated_by_team_after_c_day1, ipv_absent_vaccinated_by_team_after_c_day2, ipv_absent_vaccinated_by_team_after_c_day3, ipv_absent_vaccinated_by_team_after_c_day4, ipv_absent_vaccinated_by_team_after_c_day5, ipv_absent_vaccinated_by_team_after_c_day6, ipv_absent_vaccinated_by_team_after_c_day7, na.rm=T),
             recorded_missed_reason_nss_vaccinated = sum(ipv_found_vacc_nss_day1, ipv_found_vacc_nss_day2, ipv_found_vacc_nss_day3,  ipv_found_vacc_nss_day4,  ipv_found_vacc_nss_day5, ipv_found_vacc_nss_day6, ipv_found_vacc_nss_day7,
                                                         ipv_re_vacc_nss_day1, ipv_re_vacc_nss_day2, ipv_re_vacc_nss_day3,  ipv_re_vacc_nss_day4, ipv_re_vacc_nss_day5, ipv_re_vacc_nss_day6, ipv_re_vacc_nss_day7, na.rm=T),
             recorded_missed_reason_refusal_vaccinated = sum(ipvf_ound_vacc_refusal_day1, ipvf_ound_vacc_refusal_day2, ipvf_ound_vacc_refusal_day3, ipvf_ound_vacc_refusal_day4, ipvf_ound_vacc_refusal_day5, ipvf_ound_vacc_refusal_day6, ipvf_ound_vacc_refusal_day7,
                                                             ipv_re_vacc_refusal_day1, ipv_re_vacc_refusal_day2, ipv_re_vacc_refusal_day3,  ipv_re_vacc_refusal_day4, ipv_re_vacc_refusal_day5, ipv_re_vacc_refusal_day6, ipv_re_vacc_refusal_day7, na.rm=T),
             recorded_missed_day1_vaccinated = sum(ipv_found_vaccinated_absent_return_during_campaign_day1, 
                                                   ipv_absent_vaccinated_by_team_during_c_day1,
                                                   ipv_found_vaccinated_absent_day1, 
                                                   ipv_absent_vaccinated_by_team_after_c_day1,
                                                   ipv_found_vacc_nss_day1, 
                                                   ipv_re_vacc_nss_day1,
                                                   ipvf_ound_vacc_refusal_day1, 
                                                   ipv_re_vacc_refusal_day1, na.rm=T),
             recorded_missed_day2_vaccinated = sum(ipv_found_vaccinated_absent_return_during_campaign_day2, 
                                                   ipv_absent_vaccinated_by_team_during_c_day2,
                                                   ipv_found_vaccinated_absent_day2, 
                                                   ipv_absent_vaccinated_by_team_after_c_day2,
                                                   ipv_found_vacc_nss_day2, 
                                                   ipv_re_vacc_nss_day2,
                                                   ipvf_ound_vacc_refusal_day2, 
                                                   ipv_re_vacc_refusal_day2, na.rm=T),
             recorded_missed_day3_vaccinated = sum(ipv_found_vaccinated_absent_return_during_campaign_day3, 
                                                   ipv_absent_vaccinated_by_team_during_c_day3,
                                                   ipv_found_vaccinated_absent_day3, 
                                                   ipv_absent_vaccinated_by_team_after_c_day3,
                                                   ipv_found_vacc_nss_day3, 
                                                   ipv_re_vacc_nss_day3,
                                                   ipvf_ound_vacc_refusal_day3, 
                                                   ipv_re_vacc_refusal_day3, na.rm=T),
             recorded_missed_day4_vaccinated = sum(ipv_found_vaccinated_absent_return_during_campaign_day4, 
                                                   ipv_absent_vaccinated_by_team_during_c_day4,
                                                   ipv_found_vaccinated_absent_day4, 
                                                   ipv_absent_vaccinated_by_team_after_c_day4,
                                                   ipv_found_vacc_nss_day4, 
                                                   ipv_re_vacc_nss_day4,
                                                   ipvf_ound_vacc_refusal_day4, 
                                                   ipv_re_vacc_refusal_day4, na.rm=T),
             recorded_missed_day5_vaccinated = sum(ipv_found_vaccinated_absent_return_during_campaign_day5, 
                                                   ipv_absent_vaccinated_by_team_during_c_day5,
                                                   ipv_found_vaccinated_absent_day5, 
                                                   ipv_absent_vaccinated_by_team_after_c_day5,
                                                   ipv_found_vacc_nss_day5, 
                                                   ipv_re_vacc_nss_day5,
                                                   ipvf_ound_vacc_refusal_day5, 
                                                   ipv_re_vacc_refusal_day5, na.rm=T),
             recorded_missed_day6_vaccinated = sum(ipv_found_vaccinated_absent_return_during_campaign_day6, 
                                                   ipv_absent_vaccinated_by_team_during_c_day6,
                                                   ipv_found_vaccinated_absent_day6, 
                                                   ipv_absent_vaccinated_by_team_after_c_day6,
                                                   ipv_found_vacc_nss_day6, 
                                                   ipv_re_vacc_nss_day6,
                                                   ipvf_ound_vacc_refusal_day6, 
                                                   ipv_re_vacc_refusal_day6, na.rm=T),
             recorded_missed_revisit_day_vaccinated = sum(ipv_found_vaccinated_absent_return_during_campaign_day7, 
                                                          ipv_absent_vaccinated_by_team_during_c_day7,
                                                          ipv_found_vaccinated_absent_day7, 
                                                          ipv_absent_vaccinated_by_team_after_c_day7,
                                                          ipv_found_vacc_nss_day7, 
                                                          ipv_re_vacc_nss_day7,
                                                          ipvf_ound_vacc_refusal_day7, 
                                                          ipv_re_vacc_refusal_day7, na.rm=T),
             recorded_missed_total = sum(recorded_missed_day1, recorded_missed_day2, recorded_missed_day3, recorded_missed_day4, recorded_missed_day5, recorded_missed_day6, na.rm=T),
             recorded_missed_total_vaccinated = sum(recorded_missed_day1_vaccinated, recorded_missed_day2_vaccinated, recorded_missed_day3_vaccinated, recorded_missed_day4_vaccinated, recorded_missed_day5_vaccinated, recorded_missed_day6_vaccinated, recorded_missed_revisit_day_vaccinated, na.rm=T)) %>%
      ungroup() %>%
      mutate(age_group = "0-59 Months",
             campaign_days = 7,
             vaccine_type = "IPV") %>%
      select(campaigns, rcode, pcode, dcode, ccode, clustername, age_group, campaign_days, vaccine_type,
             recorded_missed_reason_absent1, recorded_missed_reason_absent1_vaccinated,
             recorded_missed_reason_absent2, recorded_missed_reason_absent2_vaccinated,
             recorded_missed_reason_nss, recorded_missed_reason_nss_vaccinated,
             recorded_missed_reason_refusal, recorded_missed_reason_refusal_vaccinated,
             recorded_missed_day1, recorded_missed_day1_vaccinated,
             recorded_missed_day2, recorded_missed_day2_vaccinated,
             recorded_missed_day3, recorded_missed_day3_vaccinated,
             recorded_missed_day4, recorded_missed_day4_vaccinated,
             recorded_missed_day5, recorded_missed_day5_vaccinated,
             recorded_missed_day6, recorded_missed_day6_vaccinated,
             recorded_missed_revisit_day_vaccinated,
             recorded_missed_total, recorded_missed_total_vaccinated)
  ) %>%
  select(-c("campaign_days")) %>%
  group_by(campaigns, rcode, pcode, dcode, ccode, clustername, age_group, vaccine_type) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  ungroup() %>%
  # select(-c("region", "province", "district")) %>%
  left_join(rpd_list, by=c("rcode" = "APMIS_RCODE",
                           "pcode" = "APMIS_PCODE",
                           "dcode" = "APMIS_DCODE")) %>%
  mutate(region = APMIS_Region,
         province = APMIS_Province,
         district = APMIS_District) %>%
  # mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
  #        province = ifelse(!is.na(APMIS_Province), APMIS_Province, province),
  #        district = ifelse(!is.na(APMIS_District), APMIS_District, district)) %>%
  rowwise() %>%
  mutate(
    recorded_missed_reason_absent1_conversion_pct =  ifelse(!is.na(recorded_missed_reason_absent1) & recorded_missed_reason_absent1 != 0, recorded_missed_reason_absent1_vaccinated / recorded_missed_reason_absent1, NA_integer_),
    recorded_missed_reason_absent2_conversion_pct = ifelse(!is.na(recorded_missed_reason_absent2) & recorded_missed_reason_absent2 != 0, recorded_missed_reason_absent2_vaccinated / recorded_missed_reason_absent2, NA_integer_),
    recorded_missed_reason_nss_conversion_pct = ifelse(!is.na(recorded_missed_reason_nss) & recorded_missed_reason_nss != 0, recorded_missed_reason_nss_vaccinated / recorded_missed_reason_nss, NA_integer_),
    recorded_missed_reason_refusal_conversion_pct = ifelse(!is.na(recorded_missed_reason_refusal) & recorded_missed_reason_refusal != 0, recorded_missed_reason_refusal_vaccinated / recorded_missed_reason_refusal, NA_integer_),
    recorded_missed_day1_conversion_pct = ifelse(!is.na(recorded_missed_day1) & recorded_missed_day1 != 0, recorded_missed_day1_vaccinated / recorded_missed_day1, NA_integer_),
    recorded_missed_day2_conversion_pct = ifelse(!is.na(recorded_missed_day2) & recorded_missed_day2 != 0, recorded_missed_day2_vaccinated / recorded_missed_day2, NA_integer_),
    recorded_missed_day3_conversion_pct = ifelse(!is.na(recorded_missed_day3) & recorded_missed_day3 != 0, recorded_missed_day3_vaccinated / recorded_missed_day3, NA_integer_),
    recorded_missed_day4_conversion_pct = ifelse(!is.na(recorded_missed_day4) & recorded_missed_day4 != 0, recorded_missed_day4_vaccinated / recorded_missed_day4, NA_integer_),
    recorded_missed_day5_conversion_pct = ifelse(!is.na(recorded_missed_day5) & recorded_missed_day5 != 0, recorded_missed_day5_vaccinated / recorded_missed_day5, NA_integer_),
    recorded_missed_day6_conversion_pct = ifelse(!is.na(recorded_missed_day6) & recorded_missed_day6 != 0, recorded_missed_day6_vaccinated / recorded_missed_day6, NA_integer_),
    recorded_missed_total_conversion_pct = ifelse(!is.na(recorded_missed_total) & recorded_missed_total != 0, recorded_missed_total_vaccinated / recorded_missed_total, NA_integer_)
  ) %>%
  mutate(across(
    -c(campaigns, region, province, district, clustername, rcode, pcode, dcode, ccode, age_group, vaccine_type),
    ~ ifelse(. < 0, NA_integer_, .)
  )) %>%
  ungroup() %>%
  select(campaigns, region, province, district, clustername, rcode, pcode, dcode, ccode, age_group, vaccine_type,
         recorded_missed_reason_absent1, recorded_missed_reason_absent1_vaccinated, recorded_missed_reason_absent1_conversion_pct,
         recorded_missed_reason_absent2, recorded_missed_reason_absent2_vaccinated, recorded_missed_reason_absent2_conversion_pct,
         recorded_missed_reason_nss, recorded_missed_reason_nss_vaccinated, recorded_missed_reason_nss_conversion_pct,
         recorded_missed_reason_refusal, recorded_missed_reason_refusal_vaccinated, recorded_missed_reason_refusal_conversion_pct,
         recorded_missed_day1, recorded_missed_day1_vaccinated, recorded_missed_day1_conversion_pct,
         recorded_missed_day2, recorded_missed_day2_vaccinated, recorded_missed_day2_conversion_pct,
         recorded_missed_day3, recorded_missed_day3_vaccinated, recorded_missed_day3_conversion_pct,
         recorded_missed_day4, recorded_missed_day4_vaccinated, recorded_missed_day4_conversion_pct,
         recorded_missed_day5, recorded_missed_day5_vaccinated, recorded_missed_day5_conversion_pct,
         recorded_missed_day6, recorded_missed_day6_vaccinated, recorded_missed_day6_conversion_pct,
         recorded_missed_revisit_day_vaccinated,
         recorded_missed_total, recorded_missed_total_vaccinated, recorded_missed_total_conversion_pct) %>%
  mutate_at(c("recorded_missed_reason_absent1_conversion_pct", "recorded_missed_reason_absent2_conversion_pct", "recorded_missed_reason_nss_conversion_pct", "recorded_missed_reason_refusal_conversion_pct", 
              "recorded_missed_day1_conversion_pct", "recorded_missed_day2_conversion_pct", "recorded_missed_day3_conversion_pct", "recorded_missed_day4_conversion_pct", "recorded_missed_day5_conversion_pct", "recorded_missed_day6_conversion_pct",
              "recorded_missed_total_conversion_pct"), ~ifelse(is.na(.) | is.infinite(.) | . > 1, NA_integer_, .)) %>%
  rename(campaign_name = campaigns) %>%
  mutate_at(c("rcode", "pcode", "dcode", "ccode"), ~as.numeric(.)) %>%
  arrange(campaign_name, region, province, district, ccode, age_group)  %>%
  filter(!is.na(district))

export_admin_conversion_district <- export_admin_conversion_cluster %>%
  # Group by the necessary columns
  group_by(campaign_name, region, province, district, rcode, pcode, dcode, age_group, vaccine_type) %>%
  
  # Summarize the necessary columns
  summarise(across(where(is.numeric), ~sum(. , na.rm = TRUE))) %>%
  
  # Mutate conversion percentage columns
  mutate(
    recorded_missed_reason_absent1_conversion_pct = if_else(
      !is.na(recorded_missed_reason_absent1) & recorded_missed_reason_absent1 != 0, 
      recorded_missed_reason_absent1_vaccinated / recorded_missed_reason_absent1, 
      NA_real_
    ),
    recorded_missed_reason_absent2_conversion_pct = if_else(
      !is.na(recorded_missed_reason_absent2) & recorded_missed_reason_absent2 != 0, 
      recorded_missed_reason_absent2_vaccinated / recorded_missed_reason_absent2, 
      NA_real_
    ),
    recorded_missed_reason_nss_conversion_pct = if_else(
      !is.na(recorded_missed_reason_nss) & recorded_missed_reason_nss != 0, 
      recorded_missed_reason_nss_vaccinated / recorded_missed_reason_nss, 
      NA_real_
    ),
    recorded_missed_reason_refusal_conversion_pct = if_else(
      !is.na(recorded_missed_reason_refusal) & recorded_missed_reason_refusal != 0, 
      recorded_missed_reason_refusal_vaccinated / recorded_missed_reason_refusal, 
      NA_real_
    ),
    recorded_missed_day1_conversion_pct = if_else(
      !is.na(recorded_missed_day1) & recorded_missed_day1 != 0, 
      recorded_missed_day1_vaccinated / recorded_missed_day1, 
      NA_real_
    ),
    recorded_missed_day2_conversion_pct = if_else(
      !is.na(recorded_missed_day2) & recorded_missed_day2 != 0, 
      recorded_missed_day2_vaccinated / recorded_missed_day2, 
      NA_real_
    ),
    recorded_missed_day3_conversion_pct = if_else(
      !is.na(recorded_missed_day3) & recorded_missed_day3 != 0, 
      recorded_missed_day3_vaccinated / recorded_missed_day3, 
      NA_real_
    ),
    recorded_missed_day4_conversion_pct = if_else(
      !is.na(recorded_missed_day4) & recorded_missed_day4 != 0, 
      recorded_missed_day4_vaccinated / recorded_missed_day4, 
      NA_real_
    ),
    recorded_missed_day5_conversion_pct = if_else(
      !is.na(recorded_missed_day5) & recorded_missed_day5 != 0, 
      recorded_missed_day5_vaccinated / recorded_missed_day5, 
      NA_real_
    ),
    recorded_missed_day6_conversion_pct = if_else(
      !is.na(recorded_missed_day6) & recorded_missed_day6 != 0, 
      recorded_missed_day6_vaccinated / recorded_missed_day6, 
      NA_real_
    ),
    recorded_missed_total_conversion_pct = if_else(
      !is.na(recorded_missed_total) & recorded_missed_total != 0, 
      recorded_missed_total_vaccinated / recorded_missed_total, 
      NA_real_
    )
  ) %>%
  
  # Handle NA, negative, and infinite values efficiently
  mutate(across(
    where(is.numeric),  # Apply the transformation only to numeric columns
    ~ if_else(. < 0 | is.infinite(.), NA_real_, .)
  )) %>%
  
  # Ensure proper column types
  mutate_at(c("rcode", "pcode", "dcode"), ~as.numeric(.)) %>%
  
  # Select the relevant columns
  select(campaign_name, region, province, district, rcode, pcode, dcode, age_group, vaccine_type,
         recorded_missed_reason_absent1, recorded_missed_reason_absent1_vaccinated, recorded_missed_reason_absent1_conversion_pct,
         recorded_missed_reason_absent2, recorded_missed_reason_absent2_vaccinated, recorded_missed_reason_absent2_conversion_pct,
         recorded_missed_reason_nss, recorded_missed_reason_nss_vaccinated, recorded_missed_reason_nss_conversion_pct,
         recorded_missed_reason_refusal, recorded_missed_reason_refusal_vaccinated, recorded_missed_reason_refusal_conversion_pct,
         recorded_missed_day1, recorded_missed_day1_vaccinated, recorded_missed_day1_conversion_pct,
         recorded_missed_day2, recorded_missed_day2_vaccinated, recorded_missed_day2_conversion_pct,
         recorded_missed_day3, recorded_missed_day3_vaccinated, recorded_missed_day3_conversion_pct,
         recorded_missed_day4, recorded_missed_day4_vaccinated, recorded_missed_day4_conversion_pct,
         recorded_missed_day5, recorded_missed_day5_vaccinated, recorded_missed_day5_conversion_pct,
         recorded_missed_day6, recorded_missed_day6_vaccinated, recorded_missed_day6_conversion_pct,
         recorded_missed_revisit_day_vaccinated,
         recorded_missed_total, recorded_missed_total_vaccinated, recorded_missed_total_conversion_pct) %>%
  arrange(campaign_name, region, province, district, age_group, vaccine_type)

export_admin_conversion_province <- export_admin_conversion_district %>%
  group_by(campaign_name, region, province, rcode, pcode, age_group, vaccine_type) %>%
  summarise(across(where(is.numeric), ~sum(. , na.rm = TRUE))) %>%
  ungroup() %>%
  
  # Mutate conversion percentage columns
  mutate(
    recorded_missed_reason_absent1_conversion_pct = if_else(
      !is.na(recorded_missed_reason_absent1) & recorded_missed_reason_absent1 != 0, 
      recorded_missed_reason_absent1_vaccinated / recorded_missed_reason_absent1, 
      NA_real_
    ),
    recorded_missed_reason_absent2_conversion_pct = if_else(
      !is.na(recorded_missed_reason_absent2) & recorded_missed_reason_absent2 != 0, 
      recorded_missed_reason_absent2_vaccinated / recorded_missed_reason_absent2, 
      NA_real_
    ),
    recorded_missed_reason_nss_conversion_pct = if_else(
      !is.na(recorded_missed_reason_nss) & recorded_missed_reason_nss != 0, 
      recorded_missed_reason_nss_vaccinated / recorded_missed_reason_nss, 
      NA_real_
    ),
    recorded_missed_reason_refusal_conversion_pct = if_else(
      !is.na(recorded_missed_reason_refusal) & recorded_missed_reason_refusal != 0, 
      recorded_missed_reason_refusal_vaccinated / recorded_missed_reason_refusal, 
      NA_real_
    ),
    recorded_missed_day1_conversion_pct = if_else(
      !is.na(recorded_missed_day1) & recorded_missed_day1 != 0, 
      recorded_missed_day1_vaccinated / recorded_missed_day1, 
      NA_real_
    ),
    recorded_missed_day2_conversion_pct = if_else(
      !is.na(recorded_missed_day2) & recorded_missed_day2 != 0, 
      recorded_missed_day2_vaccinated / recorded_missed_day2, 
      NA_real_
    ),
    recorded_missed_day3_conversion_pct = if_else(
      !is.na(recorded_missed_day3) & recorded_missed_day3 != 0, 
      recorded_missed_day3_vaccinated / recorded_missed_day3, 
      NA_real_
    ),
    recorded_missed_day4_conversion_pct = if_else(
      !is.na(recorded_missed_day4) & recorded_missed_day4 != 0, 
      recorded_missed_day4_vaccinated / recorded_missed_day4, 
      NA_real_
    ),
    recorded_missed_day5_conversion_pct = if_else(
      !is.na(recorded_missed_day5) & recorded_missed_day5 != 0, 
      recorded_missed_day5_vaccinated / recorded_missed_day5, 
      NA_real_
    ),
    recorded_missed_day6_conversion_pct = if_else(
      !is.na(recorded_missed_day6) & recorded_missed_day6 != 0, 
      recorded_missed_day6_vaccinated / recorded_missed_day6, 
      NA_real_
    ),
    recorded_missed_total_conversion_pct = if_else(
      !is.na(recorded_missed_total) & recorded_missed_total != 0, 
      recorded_missed_total_vaccinated / recorded_missed_total, 
      NA_real_
    )
  ) %>%
  
  # Efficiently handle negative and infinite values in numeric columns
  mutate(across(
    where(is.numeric),  # Apply transformation only to numeric columns
    ~ if_else(. < 0 | is.infinite(.), NA_real_, .)  # Replace negative or infinite values with NA
  )) %>%
  
  # Ensure proper column types
  mutate_at(c("rcode", "pcode"), ~as.numeric(.)) %>%
  
  # Select the relevant columns
  select(campaign_name, region, province, rcode, pcode, age_group, vaccine_type,
         recorded_missed_reason_absent1, recorded_missed_reason_absent1_vaccinated, recorded_missed_reason_absent1_conversion_pct,
         recorded_missed_reason_absent2, recorded_missed_reason_absent2_vaccinated, recorded_missed_reason_absent2_conversion_pct,
         recorded_missed_reason_nss, recorded_missed_reason_nss_vaccinated, recorded_missed_reason_nss_conversion_pct,
         recorded_missed_reason_refusal, recorded_missed_reason_refusal_vaccinated, recorded_missed_reason_refusal_conversion_pct,
         recorded_missed_day1, recorded_missed_day1_vaccinated, recorded_missed_day1_conversion_pct,
         recorded_missed_day2, recorded_missed_day2_vaccinated, recorded_missed_day2_conversion_pct,
         recorded_missed_day3, recorded_missed_day3_vaccinated, recorded_missed_day3_conversion_pct,
         recorded_missed_day4, recorded_missed_day4_vaccinated, recorded_missed_day4_conversion_pct,
         recorded_missed_day5, recorded_missed_day5_vaccinated, recorded_missed_day5_conversion_pct,
         recorded_missed_day6, recorded_missed_day6_vaccinated, recorded_missed_day6_conversion_pct,
         recorded_missed_revisit_day_vaccinated,
         recorded_missed_total, recorded_missed_total_vaccinated, recorded_missed_total_conversion_pct) %>%
  arrange(campaign_name, region, province, age_group,vaccine_type)

export_admin_conversion_region <- export_admin_conversion_province %>%
  group_by(campaign_name, region, rcode, age_group, vaccine_type) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  
  # Mutate conversion percentage columns
  mutate(
    recorded_missed_reason_absent1_conversion_pct = if_else(
      !is.na(recorded_missed_reason_absent1) & recorded_missed_reason_absent1 != 0, 
      recorded_missed_reason_absent1_vaccinated / recorded_missed_reason_absent1, 
      NA_real_
    ),
    recorded_missed_reason_absent2_conversion_pct = if_else(
      !is.na(recorded_missed_reason_absent2) & recorded_missed_reason_absent2 != 0, 
      recorded_missed_reason_absent2_vaccinated / recorded_missed_reason_absent2, 
      NA_real_
    ),
    recorded_missed_reason_nss_conversion_pct = if_else(
      !is.na(recorded_missed_reason_nss) & recorded_missed_reason_nss != 0, 
      recorded_missed_reason_nss_vaccinated / recorded_missed_reason_nss, 
      NA_real_
    ),
    recorded_missed_reason_refusal_conversion_pct = if_else(
      !is.na(recorded_missed_reason_refusal) & recorded_missed_reason_refusal != 0, 
      recorded_missed_reason_refusal_vaccinated / recorded_missed_reason_refusal, 
      NA_real_
    ),
    recorded_missed_day1_conversion_pct = if_else(
      !is.na(recorded_missed_day1) & recorded_missed_day1 != 0, 
      recorded_missed_day1_vaccinated / recorded_missed_day1, 
      NA_real_
    ),
    recorded_missed_day2_conversion_pct = if_else(
      !is.na(recorded_missed_day2) & recorded_missed_day2 != 0, 
      recorded_missed_day2_vaccinated / recorded_missed_day2, 
      NA_real_
    ),
    recorded_missed_day3_conversion_pct = if_else(
      !is.na(recorded_missed_day3) & recorded_missed_day3 != 0, 
      recorded_missed_day3_vaccinated / recorded_missed_day3, 
      NA_real_
    ),
    recorded_missed_day4_conversion_pct = if_else(
      !is.na(recorded_missed_day4) & recorded_missed_day4 != 0, 
      recorded_missed_day4_vaccinated / recorded_missed_day4, 
      NA_real_
    ),
    recorded_missed_day5_conversion_pct = if_else(
      !is.na(recorded_missed_day5) & recorded_missed_day5 != 0, 
      recorded_missed_day5_vaccinated / recorded_missed_day5, 
      NA_real_
    ),
    recorded_missed_day6_conversion_pct = if_else(
      !is.na(recorded_missed_day6) & recorded_missed_day6 != 0, 
      recorded_missed_day6_vaccinated / recorded_missed_day6, 
      NA_real_
    ),
    recorded_missed_total_conversion_pct = if_else(
      !is.na(recorded_missed_total) & recorded_missed_total != 0, 
      recorded_missed_total_vaccinated / recorded_missed_total, 
      NA_real_
    )
  ) %>%
  
  # Efficiently handle negative and infinite values in numeric columns
  mutate(across(
    where(is.numeric), 
    ~ if_else(. < 0 | is.infinite(.), NA_real_, .)  # Replace negative or infinite values with NA
  )) %>%
  
  # Ensure proper column types
  mutate(rcode = as.numeric(rcode)) %>%
  
  # Select the relevant columns
  select(campaign_name, region, rcode, age_group, vaccine_type,
         recorded_missed_reason_absent1, recorded_missed_reason_absent1_vaccinated, recorded_missed_reason_absent1_conversion_pct,
         recorded_missed_reason_absent2, recorded_missed_reason_absent2_vaccinated, recorded_missed_reason_absent2_conversion_pct,
         recorded_missed_reason_nss, recorded_missed_reason_nss_vaccinated, recorded_missed_reason_nss_conversion_pct,
         recorded_missed_reason_refusal, recorded_missed_reason_refusal_vaccinated, recorded_missed_reason_refusal_conversion_pct,
         recorded_missed_day1, recorded_missed_day1_vaccinated, recorded_missed_day1_conversion_pct,
         recorded_missed_day2, recorded_missed_day2_vaccinated, recorded_missed_day2_conversion_pct,
         recorded_missed_day3, recorded_missed_day3_vaccinated, recorded_missed_day3_conversion_pct,
         recorded_missed_day4, recorded_missed_day4_vaccinated, recorded_missed_day4_conversion_pct,
         recorded_missed_day5, recorded_missed_day5_vaccinated, recorded_missed_day5_conversion_pct,
         recorded_missed_day6, recorded_missed_day6_vaccinated, recorded_missed_day6_conversion_pct,
         recorded_missed_revisit_day_vaccinated,
         recorded_missed_total, recorded_missed_total_vaccinated, recorded_missed_total_conversion_pct) %>%
  arrange(campaign_name, region, age_group, vaccine_type)


export_admin_conversion_national <- export_admin_conversion_region %>%
  group_by(campaign_name, age_group, vaccine_type) %>%
  summarise(across(where(is.numeric), ~sum(., na.rm = TRUE))) %>%
  ungroup() %>%
  
  # Mutate conversion percentage columns
  mutate(
    recorded_missed_reason_absent1_conversion_pct = if_else(
      !is.na(recorded_missed_reason_absent1) & recorded_missed_reason_absent1 != 0, 
      recorded_missed_reason_absent1_vaccinated / recorded_missed_reason_absent1, 
      NA_real_
    ),
    recorded_missed_reason_absent2_conversion_pct = if_else(
      !is.na(recorded_missed_reason_absent2) & recorded_missed_reason_absent2 != 0, 
      recorded_missed_reason_absent2_vaccinated / recorded_missed_reason_absent2, 
      NA_real_
    ),
    recorded_missed_reason_nss_conversion_pct = if_else(
      !is.na(recorded_missed_reason_nss) & recorded_missed_reason_nss != 0, 
      recorded_missed_reason_nss_vaccinated / recorded_missed_reason_nss, 
      NA_real_
    ),
    recorded_missed_reason_refusal_conversion_pct = if_else(
      !is.na(recorded_missed_reason_refusal) & recorded_missed_reason_refusal != 0, 
      recorded_missed_reason_refusal_vaccinated / recorded_missed_reason_refusal, 
      NA_real_
    ),
    recorded_missed_day1_conversion_pct = if_else(
      !is.na(recorded_missed_day1) & recorded_missed_day1 != 0, 
      recorded_missed_day1_vaccinated / recorded_missed_day1, 
      NA_real_
    ),
    recorded_missed_day2_conversion_pct = if_else(
      !is.na(recorded_missed_day2) & recorded_missed_day2 != 0, 
      recorded_missed_day2_vaccinated / recorded_missed_day2, 
      NA_real_
    ),
    recorded_missed_day3_conversion_pct = if_else(
      !is.na(recorded_missed_day3) & recorded_missed_day3 != 0, 
      recorded_missed_day3_vaccinated / recorded_missed_day3, 
      NA_real_
    ),
    recorded_missed_day4_conversion_pct = if_else(
      !is.na(recorded_missed_day4) & recorded_missed_day4 != 0, 
      recorded_missed_day4_vaccinated / recorded_missed_day4, 
      NA_real_
    ),
    recorded_missed_day5_conversion_pct = if_else(
      !is.na(recorded_missed_day5) & recorded_missed_day5 != 0, 
      recorded_missed_day5_vaccinated / recorded_missed_day5, 
      NA_real_
    ),
    recorded_missed_day6_conversion_pct = if_else(
      !is.na(recorded_missed_day6) & recorded_missed_day6 != 0, 
      recorded_missed_day6_vaccinated / recorded_missed_day6, 
      NA_real_
    ),
    recorded_missed_total_conversion_pct = if_else(
      !is.na(recorded_missed_total) & recorded_missed_total != 0, 
      recorded_missed_total_vaccinated / recorded_missed_total, 
      NA_real_
    )
  ) %>%
  
  # Handle negative and infinite values across all numeric columns
  mutate(across(
    where(is.numeric), 
    ~ if_else(. < 0 | is.infinite(.), NA_real_, .)  # Replace negative or infinite values with NA
  )) %>%
  
  # Select the relevant columns for the final output
  select(campaign_name, age_group, vaccine_type,
         recorded_missed_reason_absent1, recorded_missed_reason_absent1_vaccinated, recorded_missed_reason_absent1_conversion_pct,
         recorded_missed_reason_absent2, recorded_missed_reason_absent2_vaccinated, recorded_missed_reason_absent2_conversion_pct,
         recorded_missed_reason_nss, recorded_missed_reason_nss_vaccinated, recorded_missed_reason_nss_conversion_pct,
         recorded_missed_reason_refusal, recorded_missed_reason_refusal_vaccinated, recorded_missed_reason_refusal_conversion_pct,
         recorded_missed_day1, recorded_missed_day1_vaccinated, recorded_missed_day1_conversion_pct,
         recorded_missed_day2, recorded_missed_day2_vaccinated, recorded_missed_day2_conversion_pct,
         recorded_missed_day3, recorded_missed_day3_vaccinated, recorded_missed_day3_conversion_pct,
         recorded_missed_day4, recorded_missed_day4_vaccinated, recorded_missed_day4_conversion_pct,
         recorded_missed_day5, recorded_missed_day5_vaccinated, recorded_missed_day5_conversion_pct,
         recorded_missed_day6, recorded_missed_day6_vaccinated, recorded_missed_day6_conversion_pct,
         recorded_missed_revisit_day_vaccinated,
         recorded_missed_total, recorded_missed_total_vaccinated, recorded_missed_total_conversion_pct) %>%
  arrange(campaign_name, age_group, vaccine_type)


all_data_list$apmis_admin_data$conversion_cluster <- export_admin_conversion_cluster %>%
  filter(!is.na(region) & !is.na(rcode) & !is.na(province) & !is.na(pcode) &
           !is.na(district) & !is.na(dcode) & !is.na(campaign_name))

all_data_list$apmis_admin_data$conversion_district <- export_admin_conversion_district %>%
  filter(!is.na(region) & !is.na(rcode) & !is.na(province) & !is.na(pcode) &
           !is.na(district) & !is.na(dcode) & !is.na(campaign_name))

all_data_list$apmis_admin_data$conversion_province <- export_admin_conversion_province %>%
  filter(!is.na(region) & !is.na(rcode) & !is.na(province) & !is.na(pcode) &
           !is.na(campaign_name))

all_data_list$apmis_admin_data$conversion_region <- export_admin_conversion_region %>%
  filter(!is.na(region) & !is.na(rcode) & !is.na(campaign_name))

all_data_list$apmis_admin_data$conversion_national <- export_admin_conversion_national %>%
  filter(!is.na(campaign_name))

