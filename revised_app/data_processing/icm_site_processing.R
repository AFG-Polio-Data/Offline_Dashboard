
icm_site_s2s <- df_apmis_list$`SIA ICM Vaccine Site Monitoring` %>%
  mutate(visit_date_fmt = as.Date(visit_date_fmt)) %>%
  select(rcode, pcode, dcode, ccode,  
         region, province, district, clustername,
         campaigns, visit_date_fmt, campaign_day,
         site_setupas_microplan,
         number_houses_appropriate,                          
         site_suitablefor_community,
         site_well_organized,
         adequate_time_spentfor_target,
         team_present,
         s_mwith_team,
         sm_resident_locality,
         sm_coordinated_with_local_elders,
         sm_assisting_during_campaign,
         sm_using_child_registration_book,
         enablers_present,
         enablers_assisting,
         refusal_familiesin_visited_site,
         team_asking_about_number_families_eligible_children,
         recording_missed_childrenby_reason,
         completing_vaccination_sitesper_microplan,
         logistical_issues_observed,
         team_revisiting_all_sites,
         revisit_timings) %>%
  mutate(sm_assisting_megaphones = case_when(grepl("Announcements_via_handheld_megaphones", sm_assisting_during_campaign, ignore.case=T) ~ 1,
                                             !grepl("Mobilization_of_community_elders", sm_assisting_during_campaign, ignore.case=T) &
                                             !grepl("Seeking_out_absent_missed_children", sm_assisting_during_campaign, ignore.case=T) &
                                             !grepl("Community_mobilization", sm_assisting_during_campaign, ignore.case=T) &
                                             !grepl("No_assistance", sm_assisting_during_campaign, ignore.case=T) ~ NA,
                                             is.na(sm_assisting_during_campaign) ~ NA,
                                             TRUE ~ 0),
         sm_assisting_elders = case_when(grepl("Mobilization_of_community_elders", sm_assisting_during_campaign, ignore.case=T) ~ 1,
                                             !grepl("Announcements_via_handheld_megaphones", sm_assisting_during_campaign, ignore.case=T) &
                                               !grepl("Seeking_out_absent_missed_children", sm_assisting_during_campaign, ignore.case=T) &
                                               !grepl("Community_mobilization", sm_assisting_during_campaign, ignore.case=T) &
                                               !grepl("No_assistance", sm_assisting_during_campaign, ignore.case=T) ~ NA,
                                         is.na(sm_assisting_during_campaign) ~ NA,
                                             TRUE ~ 0),
         sm_assisting_seeking_missed = case_when(grepl("Seeking_out_absent_missed_children", sm_assisting_during_campaign, ignore.case=T) ~ 1,
                                             !grepl("Mobilization_of_community_elders", sm_assisting_during_campaign, ignore.case=T) &
                                               !grepl("Announcements_via_handheld_megaphones", sm_assisting_during_campaign, ignore.case=T) &
                                               !grepl("Community_mobilization", sm_assisting_during_campaign, ignore.case=T) &
                                               !grepl("No_assistance", sm_assisting_during_campaign, ignore.case=T) ~ NA,
                                             is.na(sm_assisting_during_campaign) ~ NA,
                                             TRUE ~ 0),
         sm_assisting_community = case_when(grepl("Community_mobilization", sm_assisting_during_campaign, ignore.case=T) ~ 1,
                                             !grepl("Mobilization_of_community_elders", sm_assisting_during_campaign, ignore.case=T) &
                                               !grepl("Seeking_out_absent_missed_children", sm_assisting_during_campaign, ignore.case=T) &
                                               !grepl("Announcements_via_handheld_megaphones", sm_assisting_during_campaign, ignore.case=T) &
                                               !grepl("No_assistance", sm_assisting_during_campaign, ignore.case=T) ~ NA,
                                            is.na(sm_assisting_during_campaign) ~ NA,
                                             TRUE ~ 0)) %>%
  select(-sm_assisting_during_campaign) 

exclude_cols <- c(
  "visit_date_fmt", "rcode", "pcode", "dcode", "ccode",
  "region", "province", "district", "clustername",
  "campaigns", "campaign_day", "form_type"
)

cols_to_convert <- icm_site_s2s %>%
  select(where(~ all(.x %in% c("yes", "no", "TRUE", "FALSE", "na", NA)))) %>%
  select(all_of(setdiff(names(.), exclude_cols))) %>%
  names()

icm_site_s2s <- icm_site_s2s %>%
  mutate(
    across(
      all_of(cols_to_convert),
      ~ case_when(
        . == "yes" ~ 1,
        . == "no" ~ 0,
        . == "TRUE" ~ 1,
        . == "FALSE" ~ 0,
        . == "na" ~ NA_real_,
        is.na(.) ~ NA_real_
      )
    )
  ) %>%
  mutate(form_type = "Site Monitoring") 

icm_site_pct <- icm_site_s2s %>%
  select(rcode, pcode, dcode, ccode,  
         region, province, district, clustername,
         campaigns, visit_date_fmt, campaign_day,
         form_type, everything())

# List of variables to keep unchanged
keep_vars <- c("rcode", "pcode", "dcode", "ccode",
               "region", "province", "district", "clustername",
               "campaigns", "visit_date_fmt", "campaign_day", "form_type")

# Rename others with "site_" prefix
icm_site_pct <- icm_site_pct %>%
  select(all_of(keep_vars), everything()) %>%
  rename_with(.fn = ~paste0("site_", .), .cols = -all_of(keep_vars))
