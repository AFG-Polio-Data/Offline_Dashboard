
icm_ipv_site_s2s <- df_apmis_list$`fIPV ICM Session Monitoring` %>%
  mutate(visit_date_fmt = as.Date(visit_date_fmt)) %>%
  select(rcode, pcode, dcode, ccode,  
         region, province, district, clustername,
         campaigns, visit_date_fmt, campaign_day,
         vaccinators_trained,
         administer_fipv_confidently,
         aware_open_vial_policy,
         aware_correct_age_group,
         recording_in_tally_sheet,
         marking_two_fingers,
         check_tropis_charge,
         syringe_filled_correctly,
         injector_upward_insertion,
         invert_before_needle_detach,
         correct_pressure_and_angle,
         site_dry_after_injection,
         visible_bleeding_after_injection,
         vvm_stage_not_optimal,
         icepacks_conditioned,
         carrier_properly_closed,
         adequate_devices_and_syringes,
         follow_microplan_itinerary,
         start_on_time_per_microplan,
         sufficient_supplies_available,
         screen_age_eligibility,
         administer_both_vaccines,
         s_mwith_team,
         caregivers_aware_before_visit,
         community_supporting_mobilization,
         sm_assisting_with_community_announcements,
         supervisor_visit_teams,
         microplan_include_daily_target) %>%
  mutate(form_type = "IPV Session Monitoring") 

exclude_cols <- c(
  "visit_date_fmt", "rcode", "pcode", "dcode", "ccode",
  "region", "province", "district", "clustername",
  "campaigns", "campaign_day", "form_type"
)

cols_to_convert <- icm_ipv_site_s2s %>%
  select(where(~ all(.x %in% c("yes", "no", "TRUE", "FALSE", "na", NA)))) %>%
  select(all_of(setdiff(names(.), exclude_cols))) %>%
  names()

icm_ipv_site_s2s <- icm_ipv_site_s2s %>%
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
  mutate(form_type = "IPV Session Monitoring") 

icm_ipv_site_s2s_pct <- icm_ipv_site_s2s %>%
  select(rcode, pcode, dcode, ccode,  
         region, province, district, clustername,
         campaigns, visit_date_fmt, campaign_day,
         form_type, everything())

# List of variables to keep unchanged
keep_vars <- c("rcode", "pcode", "dcode", "ccode",
               "region", "province", "district", "clustername",
               "campaigns", "visit_date_fmt", "campaign_day", "form_type")

# Rename others with "site_" prefix
icm_ipv_site_s2s_pct <- icm_ipv_site_s2s_pct %>%
  select(all_of(keep_vars), everything()) %>%
  rename_with(.fn = ~paste0("ipv_session_", .), .cols = -all_of(keep_vars))
