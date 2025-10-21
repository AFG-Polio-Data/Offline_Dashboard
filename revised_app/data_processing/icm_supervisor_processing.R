
icm_supervisor <- df_apmis_list$`ICM Supervisor Monitoring H2H` %>%
  bind_rows(df_apmis_list$`ICM Supervisor S2S`) %>%
  bind_rows(df_apmis_list$`ICM Supervisor Monitoring M2M`) %>%
  bind_rows(df_apmis_list$`SIA ICM Supervisor Monitoring`) %>%
  bind_rows(df_apmis_list$`SIA ICM Supervisor Monitoring_new`) %>%
  mutate(transport_type_supervision = case_when(transport_type_supervision %in% c("Animal", "Animal/A" ) ~ "animal",
                                                transport_type_supervision %in% c("motorbike", "Motorbike") ~ "motorbike",
                                                transport_type_supervision %in% c("Car") ~ "car",
                                                transport_type_supervision %in% c("No_Transport", "notransport", "NoTransport") ~ "no_transport",
                                                TRUE ~ transport_type_supervision)) %>%
  select(rcode, pcode, dcode, ccode, 
         region, province, district, clustername,
         campaigns, visit_date_fmt, campaign_day,
         resident_of_cluster,
         trained_before_campaign,
         carrying_opv,
         revised_maps,
         filling_households_check_list,
         transport_supervision,
         transport_type_supervision,
         supervising_teams_plan,
         daily_supervision_plan,
         literate,
         carrying_maps, 
         updated_hrmp,
         supervisor_tallying_recordkeeping_corrective_actions,
         carrying_logistics,
         conducted_coordination_meetings,
         field_issuesto_higher_levels) %>%
  mutate(across(where(~ all(. %in% c("yes", "no", "na", "TRUE", "FALSE", NA))), ~ case_when(. == "yes" ~ 1,
                                                                                            . == "no" ~ 0,
                                                                                            . == "TRUE" ~ 1,
                                                                                            . == "FALSE" ~ 0,
                                                                                            . == "na" ~ NA,
                                                                                            . == NA ~ NA))) %>%
  mutate(form_type = "Supervisor Monitoring")

icm_supervisor_categorical <- icm_supervisor %>%
  select(rcode, pcode, dcode, ccode, 
         region, province, district, clustername, form_type,
         campaigns, visit_date_fmt, campaign_day,
         transport_type_supervision)

icm_supervisor_pct <- icm_supervisor %>%
  select(rcode, pcode, dcode, ccode,  
         region, province, district, clustername, form_type,
         campaigns, visit_date_fmt, campaign_day,
         resident_of_cluster,
         trained_before_campaign,
         carrying_opv,
         revised_maps,
         filling_households_check_list,
         transport_supervision,
         supervising_teams_plan,
         daily_supervision_plan,
         literate,
         carrying_maps, 
         updated_hrmp,
         supervisor_tallying_recordkeeping_corrective_actions,
         carrying_logistics,
         conducted_coordination_meetings,
         field_issuesto_higher_levels)

# List of variables to keep unchanged
keep_vars <- c("rcode", "pcode", "dcode", "ccode",
               "region", "province", "district", "clustername",
               "campaigns", "visit_date_fmt", "campaign_day", "form_type")

# Rename others with "site_" prefix
icm_supervisor_pct <- icm_supervisor_pct %>%
  select(all_of(keep_vars), everything()) %>%
  rename_with(.fn = ~paste0("supervisor_", .), .cols = -all_of(keep_vars))