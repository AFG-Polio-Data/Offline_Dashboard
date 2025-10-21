icm_team_h2h <- df_apmis_list$`ICM Team Monitoring H2H` %>%
  select(rcode, pcode, dcode, ccode,  
         region, province, district, clustername,
         campaigns, visit_date_fmt, campaign_day,
         team_members_resident_same_area, vaccinators_trained, carrying_copy_of_microplan1, describe_areas_map, going_house_to_house, opv_vials_kept_dry, vaccine_usable, team_asking_children_under1, team_screening_guest_new_born_children_under1, team_recording_absent_refusal_children_under1, team_asking_af_pcases_children_under1, supervisor_visit_teams, one_member_of_team_chw, female_team_members, social_mobilizer_accompany_team) %>%
  mutate(across(where(~ all(. %in% c("yes", "no", "TRUE", "FALSE", "na", NA))), ~ case_when(. == "yes" ~ 1,
                                                                                            . == "no" ~ 0,
                                                                                            . == "TRUE" ~ 1,
                                                                                            . == "FALSE" ~ 0,
                                                                                            . == "na" ~ NA,
                                                                                            . == NA ~ NA))) %>%
  mutate(form_type = "Team Monitoring")


icm_team_s2s <- df_apmis_list$`ICM Team Monitoring S2S` %>%
  bind_rows(df_apmis_list$`SIA ICM Team Monitoring` %>%
              mutate(team_no = as.numeric(team_no))) %>%
  mutate(visit_date_fmt = as.Date(visit_date_fmt)) %>%
  bind_rows(df_apmis_list$`ICM Team Monitoring M2M`) %>%
  select(rcode, pcode, dcode, ccode,  
         region, province, district, clustername,
         campaigns, visit_date_fmt, campaign_day,
         vaccination_session, 
         iec_material_displayed, 
         team_present, 
         team_members_resident_same_area, 
         vaccinators_trained, 
         team_work_is_clear, 
         team_have_session_list, 
         suficient_op_vvials, 
         carier_with_frozen_ice_packs, 
         team_have_tallysheet, 
         opv_vials_kept_dry, 
         correct_vv_mknowledge, 
         vvm_not_optimal_for_use, 
         finger_marking, 
         marking_tally_sheets, 
         sm_announcing_and_mobilizing_children, 
         seeking_children_walking_past_session, 
         used_vaccin_match_with_children_recorded, 
         team_asking_af_pcases_children, 
         supervisor_visit_teams,
         vaccination_held_plan,
         s_mwith_team,
         team_engaging_lcv_emobilizing,
         local_community_supporting,
         masjid_announcement,
         sm_updating_microcensus_for_mobilize_chidren,
         sm_updating_microcensus_for_record_missed,
         pluses_used_children_mobilization,
         pluses_distributor,
         
         one_member_of_team_chw,
         female_team_members,
         carrying_copy_of_microplan1,
         updated_hrmp,
         microplan_list_village_elders,
         daily_revisit_plan,
         have_daily_target,
         vaccine_usable,
         team_screening_guest_new_born_children_under1,
         team_asking_children_under1,
         missed_children_registration,    
         cross_verifying_information,
         giving_key_vaccination_messages,
         sm_updating_child_booklet,
         sm_has_megaphones,
         sm_assisting_with_community_announcements) %>%
  mutate(across(where(~ all(. %in% c("yes", "no", "TRUE", "FALSE", "na", NA))), ~ case_when(. == "yes" ~ 1,
                                                                                            . == "no" ~ 0,
                                                                                            . == "TRUE" ~ 1,
                                                                                            . == "FALSE" ~ 0,
                                                                                            . == "na" ~ NA,
                                                                                            . == NA ~ NA))) %>%
  mutate(form_type = "Team Monitoring") 

# List of variables to keep unchanged
keep_vars <- c("rcode", "pcode", "dcode", "ccode",
               "region", "province", "district", "clustername",
               "campaigns", "visit_date_fmt", "campaign_day", "form_type")

# Rename others with "site_" prefix
icm_team_s2s <- icm_team_s2s %>%
  select(all_of(keep_vars), everything()) %>%
  rename_with(.fn = ~paste0("s2s_", .), .cols = -all_of(keep_vars))

# Rename others with "site_" prefix
icm_team_h2h <- icm_team_h2h %>%
  select(all_of(keep_vars), everything()) %>%
  rename_with(.fn = ~paste0("h2h_", .), .cols = -all_of(keep_vars))

icm_team <- icm_team_h2h %>%
  bind_rows(icm_team_s2s)

icm_team_pct <- icm_team %>%
  select(rcode, pcode, dcode, ccode,  
         region, province, district, clustername,
         campaigns, visit_date_fmt, campaign_day,
         form_type, everything())

# List of variables to keep unchanged
keep_vars <- c("rcode", "pcode", "dcode", "ccode",
               "region", "province", "district", "clustername",
               "campaigns", "visit_date_fmt", "campaign_day", "form_type")

# Rename others with "site_" prefix
icm_team_pct <- icm_team_pct %>%
  select(all_of(keep_vars), everything()) %>%
  rename_with(.fn = ~paste0("team_", .), .cols = -all_of(keep_vars))