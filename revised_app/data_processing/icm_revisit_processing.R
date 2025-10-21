icm_revisit_h2h <- df_apmis_list$`ICM Revisit Strategy H2H` %>%
  mutate(age_group = "0-59 Months") %>%
  bind_rows(df_apmis_list$`ICM for Revisit Strategy (5-10)` %>%
              mutate(age_group = "5-10 Years")) %>%
  select(rcode, pcode, dcode, ccode,  
         region, province, district, clustername,
         campaigns, visit_date_fmt, campaign_day, age_group,
         supervisorvisitingj, teamhavinglistj, teamgoing_h2hj, teamgoing_h2hj2, housesdoormarked, absent_day3, refusal_day3, h_revisitedj, md_revisitj, cov_absent, cov_refusal, still_absent, still_refusal, nsm_still_absentj, nsm_refusalsj, nsm_teamnegligencej) %>%
  mutate(across(where(~ all(. %in% c("yes", "no", "na", "TRUE", "FALSE", NA))), ~ case_when(. == "yes" ~ 1,
                                                                                            . == "no" ~ 0,
                                                                                            . == "TRUE" ~ 1,
                                                                                            . == "FALSE" ~ 0,
                                                                                            . == "na" ~ NA,
                                                                                            . == NA ~ NA))) %>%
  mutate(form_type = "Monitoring for Revisit Strategy")

icm_revisit_h2h_numeric <- icm_revisit_h2h %>%
  select(rcode, pcode, dcode, ccode,  
         region, province, district, clustername,
         campaigns, visit_date_fmt, campaign_day, age_group, form_type,
         housesdoormarked, absent_day3, refusal_day3, h_revisitedj, md_revisitj, cov_absent, cov_refusal, still_absent, still_refusal, nsm_still_absentj, nsm_refusalsj, nsm_teamnegligencej)

icm_revisit_h2h_pct <- icm_revisit_h2h %>%
  select(rcode, pcode, dcode, ccode,  
         region, province, district, clustername,
         campaigns, visit_date_fmt, campaign_day, age_group, form_type,
         supervisorvisitingj, teamhavinglistj, teamgoing_h2hj, teamgoing_h2hj2)

# List of variables to keep unchanged
keep_vars <- c("rcode", "pcode", "dcode", "ccode",
               "region", "province", "district", "clustername",
               "campaigns", "visit_date_fmt", "campaign_day", "age_group", "form_type")

# Rename others with "site_" prefix
icm_revisit_h2h_pct <- icm_revisit_h2h_pct %>%
  select(all_of(keep_vars), everything()) %>%
  rename_with(.fn = ~paste0("revisit_", .), .cols = -all_of(keep_vars))