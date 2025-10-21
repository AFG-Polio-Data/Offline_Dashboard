
# List of input dataframes
dfs <- list(icm_supervisor_pct,
            icm_revisit_h2h_pct,
            icm_team_pct,
            icm_site_pct,
            icm_ipv_site_s2s_pct,
            icm_household_h2h_pct)

# Step 1: Build a reference class lookup from non-empty dataframes
class_lookup <- dfs %>%
  keep(~ nrow(.) > 0) %>%
  map(~ map(., ~ class(.)[1])) %>%  # keep as list
  reduce(function(x, y) modifyList(x, y))

# Step 2: Harmonize empty dfs to match column classes from the lookup
dfs_harmonized <- map(dfs, function(df) {
  if (nrow(df) > 0) return(df)
  
  # For each column, coerce to the matching class in the reference
  df[] <- map2(df, names(df), function(col, name) {
    target_class <- class_lookup[[name]]
    
    if (is.null(target_class)) return(col)  # No info → leave as-is
    
    # Coerce to target class
    switch(target_class,
           character = as.character(col),
           numeric   = as.numeric(col),
           integer   = as.integer(col),
           logical   = as.logical(col),
           Date      = as.Date(col),
           factor    = factor(col),
           col)  # fallback: return as-is
  })
  
  return(df)
})

icm_pcts <- bind_rows(dfs_harmonized)

icm_pcts_pre_switch <- icm_pcts %>%
  filter(campaigns %in% (df_campaigns %>%
                           select(campaign_name, campaign_startdate) %>%
                           unique() %>%
                           filter(campaign_startdate <= as.Date("2024-10-01")))$campaign_name) %>%
  rename('H2H: Poorly Covered Area' = household_poorly_covered_area,
         'H2H: Missed Area' = household_missed_area,
         'H2H: Supervisor is moving in the field to supervise the team' = revisit_supervisorvisitingj,
         'H2H: Supervisor has compiled list of absent/refusal children' = revisit_teamhavinglistj,
         'H2H: Team has list/record of remaining missed (absent/refusal) children' = revisit_teamgoing_h2hj,
         'H2H: Team going house-to-house to vaccinate the remaining missed (absent/refusal) children' = revisit_teamgoing_h2hj2,
         'Is supervisor resident of his assigned cluster?' = supervisor_resident_of_cluster,
         'Has supervisor been trained before campaign?' = supervisor_trained_before_campaign,
         'Is supervisor carrying OPV?' = supervisor_carrying_opv,
         'Is supervisor carrying maps with clearly marked team areas and boundaries?' = supervisor_revised_maps,
         'Is supervisor carrying and filling household check list?' = supervisor_filling_households_check_list,
         'Is transport provided for supervision based on microplan?' = supervisor_transport_supervision,
         'Supervisor is carrying itinerary / plan and session site list' = supervisor_supervising_teams_plan,
         'Supervisor has daily supervision plan' = supervisor_daily_supervision_plan,
         'H2H: At least one team member is resident of same area' = team_h2h_team_members_resident_same_area,
         'H2H: Both vaccinators trained before this campaign' = team_h2h_vaccinators_trained,
         'H2H: Team has a clear itinerary/map' = team_h2h_carrying_copy_of_microplan1,
         'H2H: Team is able to clearly describe their area and boundaires' = team_h2h_describe_areas_map,
         'H2H: Vaccination team going house-to-house' = team_h2h_going_house_to_house,
         'H2H: OPV vials are kept dry' = team_h2h_opv_vials_kept_dry,
         'H2H: Based on observation of VVM, vaccine is usable' = team_h2h_vaccine_usable,
         'H2H: Team is specifcally asking about 0-11 months children' = team_h2h_team_asking_children_under1,
         'H2H: Team is screening for guest/new-born/sleeping/sick children' = team_h2h_team_screening_guest_new_born_children_under1,
         'H2H: Team is recording names of absent/refusal children on back of tally sheet' = team_h2h_team_recording_absent_refusal_children_under1,
         'H2H: Team is asking about AFP cases' = team_h2h_team_asking_af_pcases_children_under1,
         'H2H: Team visited by supervisor in the field and tally-sheet signed' = team_h2h_supervisor_visit_teams,
         'H2H: At least one team member is CHW' = team_h2h_one_member_of_team_chw,
         'H2H: At least one team member is female' = team_h2h_female_team_members,
         'H2H: Social mobilizer accompanies vaccination team' = team_h2h_social_mobilizer_accompany_team,
         'M2M/S2S: At least one team member is resident of same area' = team_s2s_team_members_resident_same_area,
         'M2M/S2S: Both vaccinators trained before this campaign' = team_s2s_vaccinators_trained,
         'M2M/S2S: OPV vials are kept dry' = team_s2s_opv_vials_kept_dry,
         'M2M/S2s: Team visited by supervisor in the field and tally-sheet signed' = team_s2s_supervisor_visit_teams,
         'M2M/S2S: Vaccination session held at site/masjid' = team_s2s_vaccination_session,
         'M2M/S2S: Vaccination session has any IEC material displayed prominently' = team_s2s_iec_material_displayed,
         'M2M/S2S: All team members are present at the site/masjid' = team_s2s_team_present,
         'M2M/S2S: Team is clear on the work they are supposed to do today' = team_s2s_team_work_is_clear,
         'M2M/S2S: Team has list of sites/masjids' = team_s2s_team_have_session_list,
         'M2M/S2S: Team has sufficient OPV vials' = team_s2s_suficient_op_vvials,
         'M2M/S2S: Team has vaccine carrier with frozen ice-packs' = team_s2s_carier_with_frozen_ice_packs,
         'M2M/S2S: Team has tally sheets' = team_s2s_team_have_tallysheet,
         'M2M/S2S: Team has correct knowledge about VVM' = team_s2s_correct_vv_mknowledge,
         'M2M/S2S: At least one vaccine vial with VVM is in stage not optimal for use' = team_s2s_vvm_not_optimal_for_use,
         'M2M/S2S: Finger-marking done correctly' = team_s2s_finger_marking,
         'M2M/S2S: Tally-sheets marked correctly' = team_s2s_marking_tally_sheets,
         'M2M/S2S: Social mobilizer properly announcing and mobilizing children' = team_s2s_sm_announcing_and_mobilizing_children,
         'M2M/S2S: Team proactively seeking children walking past' = team_s2s_seeking_children_walking_past_session,
         'M2M/S2S: Used vaccine is matching with the number of children recorded vaccinated' = team_s2s_used_vaccin_match_with_children_recorded,
         'M2M/S2S: Team is asking about AFP cases' = team_s2s_team_asking_af_pcases_children,
         
         'M2M/S2S: Organized vaccination session is being held as per plan' = team_s2s_vaccination_held_plan,
         'M2M/S2S: Social Mobilizer is with the Team' = team_s2s_s_mwith_team,
         'M2M/S2S: Team is engaging local community / village elders in mobilizing children to session site' = team_s2s_team_engaging_lcv_emobilizing,
         'M2M/S2S: Local Community is Supporting Mobilization Activities' = team_s2s_local_community_supporting,
         'M2M/S2S: Masjid Announcements are Taking Place' = team_s2s_masjid_announcement,
         'M2M/S2S: Social Mobilizer is using the micro-census to mobilize children for the sessions' = team_s2s_sm_updating_microcensus_for_mobilize_chidren,
         'M2M/S2S: Social Mobilizer is updating microcensus to record missed children' = team_s2s_sm_updating_microcensus_for_record_missed,
         'M2M/S2S: Pluses are being used for child mobilization' = team_s2s_pluses_used_children_mobilization,
         'M2M/S2S: Pluses distributor is with the team' = team_s2s_pluses_distributor)  %>%
  janitor::remove_empty(which=c("cols"))

icm_pcts_post_switch <- icm_pcts %>%
  filter(campaigns %in% (df_campaigns %>%
                           select(campaign_name, campaign_startdate) %>%
                           unique() %>%
                           filter(campaign_startdate > as.Date("2024-10-01") &
                                    campaign_startdate < as.Date("2025-05-01")))$campaign_name) %>%
  rename('H2H: Poorly Covered Area' = household_poorly_covered_area,
         'H2H: Missed Area' = household_missed_area,
         'H2H: Supervisor is moving in the field to supervise the team' = revisit_supervisorvisitingj,
         'H2H: Supervisor has compiled list of absent/refusal children' = revisit_teamhavinglistj,
         'H2H: Team has list/record of remaining missed (absent/refusal) children' = revisit_teamgoing_h2hj,
         'H2H: Team going house-to-house to vaccinate the remaining missed (absent/refusal) children' = revisit_teamgoing_h2hj2,
         'Is supervisor resident of his assigned cluster?' = supervisor_resident_of_cluster,
         'Has supervisor been trained before campaign?' = supervisor_trained_before_campaign,
         'Is supervisor carrying OPV?' = supervisor_carrying_opv,
         'Is supervisor carrying itinerary/plan and session site list?' = supervisor_revised_maps,
         'Is the supervisor filling out the team monitoring checklist?' = supervisor_filling_households_check_list,
         'Is supervisor using transport in the field?' = supervisor_transport_supervision,
         'Is supervisor supervising the teams as per the plan?' = supervisor_supervising_teams_plan,
         'Supervisor has daily supervision plan' = supervisor_daily_supervision_plan,
         'H2H: At least one team member is resident of same area' = team_h2h_team_members_resident_same_area,
         'H2H: Both vaccinators trained before this campaign' = team_h2h_vaccinators_trained,
         'H2H: Team has a clear itinerary/map' = team_h2h_carrying_copy_of_microplan1,
         'H2H: Team is able to clearly describe their area and boundaires' = team_h2h_describe_areas_map,
         'H2H: Vaccination team going house-to-house' = team_h2h_going_house_to_house,
         'H2H: OPV vials are kept dry' = team_h2h_opv_vials_kept_dry,
         'H2H: Based on observation of VVM, vaccine is usable' = team_h2h_vaccine_usable,
         'H2H: Team is specifcally asking about 0-11 months children' = team_h2h_team_asking_children_under1,
         'H2H: Team is screening for guest/new-born/sleeping/sick children' = team_h2h_team_screening_guest_new_born_children_under1,
         'H2H: Team is recording names of absent/refusal children on back of tally sheet' = team_h2h_team_recording_absent_refusal_children_under1,
         'H2H: Team is asking about AFP cases' = team_h2h_team_asking_af_pcases_children_under1,
         'H2H: Team visited by supervisor in the field and tally-sheet signed' = team_h2h_supervisor_visit_teams,
         'H2H: At least one team member is CHW' = team_h2h_one_member_of_team_chw,
         'H2H: At least one team member is female' = team_h2h_female_team_members,
         'H2H: Social mobilizer accompanies vaccination team' = team_h2h_social_mobilizer_accompany_team,
         'M2M/S2S: Both team members are resident of same area' = team_s2s_team_members_resident_same_area,
         'M2M/S2S: Both vaccinators trained before this campaign' = team_s2s_vaccinators_trained,
         'M2M/S2S: OPV vials are kept dry' = team_s2s_opv_vials_kept_dry,
         'M2M/S2s: Team visited by supervisor in the field' = team_s2s_supervisor_visit_teams,
         'M2M/S2S: Vaccination session site is appropriate' = team_s2s_vaccination_session,
         'M2M/S2S: Vaccination session has any IEC material displayed prominently' = team_s2s_iec_material_displayed,
         'M2M/S2S: All team members are present at the site/masjid' = team_s2s_team_present,
         'M2M/S2S: Team is clear on the work they are supposed to do today' = team_s2s_team_work_is_clear,
         'M2M/S2S: Team has list of sites/ day plan/ itinerary' = team_s2s_team_have_session_list,
         'M2M/S2S: Team has sufficient OPV vials' = team_s2s_suficient_op_vvials,
         'M2M/S2S: Team has vaccine carrier with frozen ice-packs' = team_s2s_carier_with_frozen_ice_packs,
         'M2M/S2S: Team has tally sheets' = team_s2s_team_have_tallysheet,
         'M2M/S2S: Team has correct knowledge about VVM' = team_s2s_correct_vv_mknowledge,
         'M2M/S2S: At least one vaccine vial with VVM is in stage not optimal for use' = team_s2s_vvm_not_optimal_for_use,
         'M2M/S2S: Finger-marking done correctly' = team_s2s_finger_marking,
         'M2M/S2S: Tally-sheets marked correctly' = team_s2s_marking_tally_sheets,
         'M2M/S2S: Social mobilizer properly announcing and mobilizing children' = team_s2s_sm_announcing_and_mobilizing_children,
         'M2M/S2S: Team proactively seeking children walking past' = team_s2s_seeking_children_walking_past_session,
         'M2M/S2S: Used vaccine is matching with the number of children recorded vaccinated' = team_s2s_used_vaccin_match_with_children_recorded,
         'M2M/S2S: Team is asking about AFP cases' = team_s2s_team_asking_af_pcases_children,
         
         'M2M/S2S: Organized vaccination session is being held as per plan' = team_s2s_vaccination_held_plan,
         'M2M/S2S: Social Mobilizer is with the Team' = team_s2s_s_mwith_team,
         'M2M/S2S: Team is engaging local community / village elders in mobilizing children to session site' = team_s2s_team_engaging_lcv_emobilizing,
         'M2M/S2S: Local Community is Supporting Mobilization Activities' = team_s2s_local_community_supporting,
         'M2M/S2S: Masjid Announcements are Taking Place' = team_s2s_masjid_announcement,
         'M2M/S2S: Social Mobilizer is using the micro-census to mobilize children for the sessions' = team_s2s_sm_updating_microcensus_for_mobilize_chidren,
         'M2M/S2S: Social Mobilizer is updating microcensus to record missed children' = team_s2s_sm_updating_microcensus_for_record_missed,
         'M2M/S2S: Pluses are being used for child mobilization' = team_s2s_pluses_used_children_mobilization,
         'M2M/S2S: Pluses distributor is with the team' = team_s2s_pluses_distributor)  %>%
  janitor::remove_empty(which=c("cols"))

icm_pcts_period3 <- icm_pcts %>%
  filter(campaigns %in% (df_campaigns %>%
                           select(campaign_name, campaign_startdate) %>%
                           unique() %>%
                           filter(campaign_startdate >= as.Date("2025-05-01")))$campaign_name) %>%
  # filter(visit_date_fmt >= "2025-05-01") %>%
  rename('H2H: Poorly Covered Area' = household_poorly_covered_area,
         'H2H: Missed Area' = household_missed_area,
         'H2H: Supervisor is moving in the field to supervise the team' = revisit_supervisorvisitingj,
         'H2H: Supervisor has compiled list of absent/refusal children' = revisit_teamhavinglistj,
         'H2H: Team has list/record of remaining missed (absent/refusal) children' = revisit_teamgoing_h2hj,
         'H2H: Team going house-to-house to vaccinate the remaining missed (absent/refusal) children' = revisit_teamgoing_h2hj2,
         
         '1) Is supervisor resident of his assigned cluster?' = supervisor_resident_of_cluster,
         '2) Is the supervisor literate as per the guidelines?' = supervisor_literate,
         '3) Was the supervisor trained before the current campaign?' = supervisor_trained_before_campaign,
         '4) Is the supervisor carrying a copy of the microplan?' = supervisor_carrying_maps,
         '5) Are busy locations including transit points in the microplan?' = supervisor_revised_maps,
         '6) Is there a monitoring plan for daily revisit?' = supervisor_supervising_teams_plan,
         '7) Has the supervisor filled the the team and site monitoring checklist?' = supervisor_filling_households_check_list,
         '8) Does the cluster itinerary include an updated list of HRMP?' = supervisor_updated_hrmp,
         '9) Is the supervisor following the daily supervision plan?' = supervisor_daily_supervision_plan,
         '10) Is the supervisor ensuring proper tallying, recordkeeping, and taking corrective actions when needed?' = supervisor_supervisor_tallying_recordkeeping_corrective_actions,
         '11) Is the supervisor carrying a vaccine carrier with sufficient OPV vaccines and Vitamin A or Albendazone (if applicable)?' = supervisor_carrying_opv,
         '12) Is the supervisor carrying additional logistics?' = supervisor_carrying_logistics,
         '13) Has the supervisor conducted coordination meetings with community elders, influencers, and school/madrassa teachers before the campaign?' = supervisor_conducted_coordination_meetings,
         '14) Is the supervisor communicating field issues (refusals, gaps) to higher levels at the end of the day?' = supervisor_field_issuesto_higher_levels,
         'Is transport provided for supervision based on microplan?' = supervisor_transport_supervision,
         
         
         'H2H: At least one team member is resident of same area' = team_h2h_team_members_resident_same_area,
         'H2H: Both vaccinators trained before this campaign' = team_h2h_vaccinators_trained,
         'H2H: Team has a clear itinerary/map' = team_h2h_carrying_copy_of_microplan1,
         'H2H: Team is able to clearly describe their area and boundaires' = team_h2h_describe_areas_map,
         'H2H: Vaccination team going house-to-house' = team_h2h_going_house_to_house,
         'H2H: OPV vials are kept dry' = team_h2h_opv_vials_kept_dry,
         'H2H: Based on observation of VVM, vaccine is usable' = team_h2h_vaccine_usable,
         'H2H: Team is specifcally asking about 0-11 months children' = team_h2h_team_asking_children_under1,
         'H2H: Team is screening for guest/new-born/sleeping/sick children' = team_h2h_team_screening_guest_new_born_children_under1,
         'H2H: Team is recording names of absent/refusal children on back of tally sheet' = team_h2h_team_recording_absent_refusal_children_under1,
         'H2H: Team is asking about AFP cases' = team_h2h_team_asking_af_pcases_children_under1,
         'H2H: Team visited by supervisor in the field and tally-sheet signed' = team_h2h_supervisor_visit_teams,
         'H2H: At least one team member is CHW' = team_h2h_one_member_of_team_chw,
         'H2H: At least one team member is female' = team_h2h_female_team_members,
         'H2H: Social mobilizer accompanies vaccination team' = team_h2h_social_mobilizer_accompany_team,
          
         '1) Vaccination site is set up as per micro plan' = site_site_setupas_microplan,
         '2) Number of houses per site is appropriate (i.e. 5 or fewer)' = site_number_houses_appropriate,
         '3) Site selection is suitable and acceptable for the community' = site_site_suitablefor_community,
         '4) Site session well-organized' = site_site_well_organized,
         '5) Adequate time spent for vaccination of target children in site' = site_adequate_time_spentfor_target,
         '6) Team members present as per microplan' = site_team_present,                                       
         '7) Social Mobilizer assigned to the vaccination team' = site_s_mwith_team,
         '8) Social Mobilizer is resident of area/locality' = site_sm_resident_locality,
         '9) Social Mobilizer coordinated with local elders or Wakil Guzar for vaccination session prior to campaign' = site_sm_coordinated_with_local_elders,
         '10a) Social Mobilizer is assisting via announcements on handheld megaphone' = site_sm_assisting_megaphones,
         '10b) Social Mobilizer is assisting via mobilization of community elders' = site_sm_assisting_elders,
         '10c) Social Mobilizer is assisting via seeking out absent/missed children' = site_sm_assisting_seeking_missed,
         '10d) Social Mobilizer is assisting via community mobilization' = site_sm_assisting_community,
         '11) Social Mobilizer is using the child registration book to verify all children at the site' = site_sm_using_child_registration_book,
         '12) Enablers present at the site' = site_enablers_present,                                   
         '13) Enablers assisting in providing information about missed children' = site_enablers_assisting,
         '14) Refusal families are in the visited site' = site_refusal_familiesin_visited_site,
         '15) Vaccination team asking about the number of families and eligible children during the visit' = site_team_asking_about_number_families_eligible_children,
         '16) Vaccination team recording missed children by reason in the missed children form' = site_recording_missed_childrenby_reason,
         '17) Team is completing vaccination at the sites as per the microplan' = site_completing_vaccination_sitesper_microplan,
         '18) Logistical issues were observed at the site' = site_logistical_issues_observed,                         
         '19) Team is revisiting all sites to vaccinate missed children' = site_team_revisiting_all_sites,
         '20) Revisit timings take into account the availability of caregivers' = site_revisit_timings,
         
         '1) Both team members are resident of same area (village)' = team_s2s_team_members_resident_same_area,
         '2) Both vaccinators trained before this campaign' = team_s2s_vaccinators_trained,
         '3) At least one team member is a Community Health Worker' = team_s2s_one_member_of_team_chw,
         '4) At least one team member is female' = team_s2s_female_team_members,
         '5) Team has a clear itinerary/map' = team_s2s_carrying_copy_of_microplan1,
         '6) Team itinerary includes the number and type of HRMP' = team_s2s_updated_hrmp,
         '7) Microplan lists village elders/enablers who support vaccination efforts' = team_s2s_microplan_list_village_elders, 
         '8) Team has a plan for daily revisit' = team_s2s_daily_revisit_plan,
         '9) Team members know their daily target' = team_s2s_have_daily_target,
         '10) Team has vaccine carrier with frozen ice-packs' = team_s2s_carier_with_frozen_ice_packs,
         '11) OPV vials are kept cool and dry' = team_s2s_opv_vials_kept_dry,
         '12) Based on observation of VVM, vaccine is usable' = team_s2s_vaccine_usable,
         '13) Number of children vaccinated matches with vials used' = team_s2s_used_vaccin_match_with_children_recorded,
         '14) Team is asking caregivers for details of all children' = team_s2s_team_screening_guest_new_born_children_under1,
         '15) Team is specifically asking about children aged 0-11 months' = team_s2s_team_asking_children_under1,
         '16) Team is properly recording children vaccinated' = team_s2s_marking_tally_sheets,
         '17) Team is recording all missed children with reasons and using the missed children registration form (if applicable)' = team_s2s_missed_children_registration,
         '18) In areas using the child registration book, team is cross-verifying information with the register (if applicable)' = team_s2s_cross_verifying_information,
         '19) Team is properly finger-marking after administering OPV' = team_s2s_finger_marking,
         '20) Team is asking about AFP cases' = team_s2s_team_asking_af_pcases_children,
         '21) Team is giving key vaccination messages to the families after vaccination' = team_s2s_giving_key_vaccination_messages,
         '22) Team has been visited by a supervisor in the field and tally sheet is signed' = team_s2s_supervisor_visit_teams,
         '23) Social Mobilizer accompanies the vaccination team in the field' = team_s2s_s_mwith_team,
         '24) Social Mobilizer is carrying and updating the child registration booklet' = team_s2s_sm_updating_child_booklet,
         '25) Social Mobilizer supports bringing children to sites' = team_s2s_sm_announcing_and_mobilizing_children,
         '26) Social Mobilizer has hand held megaphone' = team_s2s_sm_has_megaphones,
         '27) Social Mobilizer is assisting with community announcements' = team_s2s_sm_assisting_with_community_announcements,
         
         "1) Both vaccinators trained before this campaign" = ipv_session_vaccinators_trained,
         "2) Vaccinators able to administer fIPV confidently" = ipv_session_administer_fipv_confidently,
         "3) Vaccinators aware of open vial policy applicable on IPV" = ipv_session_aware_open_vial_policy,
         "4) Vaccinators aware of correct age group for IPV vaccination" = ipv_session_aware_correct_age_group,
         "5) Vaccinators recording OPV and IPV after each administration in the tally sheet" = ipv_session_recording_in_tally_sheet,
         "6) Vaccinators marking two separate fingers after IPV and OPV administration" = ipv_session_marking_two_fingers,
         "7) Tropis ID injector checked for charge status each time before use" = ipv_session_check_tropis_charge,
         "8) Syringe correctly filled with the appropriate quantity and free of air bubbles" = ipv_session_syringe_filled_correctly,
         "9) Injector pointed upward while syringe, vial and adaptor assembly is inserted into injector" = ipv_session_injector_upward_insertion,
         "10) Entire assembly is inverted before detaching the needle" = ipv_session_invert_before_needle_detach,
         "11) Correct pressure, injection angle, and injection site location are ensured" = ipv_session_correct_pressure_and_angle,
         "12) Injection site is dry  immediately after administration" =  ipv_session_site_dry_after_injection,
         "13) Visible bleeding at the injection site after administration" = ipv_session_visible_bleeding_after_injection,
         "14) Any vaccine vials with VVM are in stage not optimal for use" = ipv_session_vvm_stage_not_optimal,
         "15) Icepacks are conditioned to avoid vaccine freezing" = ipv_session_icepacks_conditioned,
         "16) Session vaccine carrier properly closed and out of direct sunlight" = ipv_session_carrier_properly_closed,
         "17) Adequate Tropis devices and syringes are available for the expected number of children" = ipv_session_adequate_devices_and_syringes,
         "18) Vaccination team follows the planned microplan itinerary for area" = ipv_session_follow_microplan_itinerary,
         "19) Team started work on time as per microplan" = ipv_session_start_on_time_per_microplan,
         "20) Sufficient tally sheets, pens, and forms are available with the team" = ipv_session_sufficient_supplies_available,
         "21) Children are screened for age eligibility before IPV administration" = ipv_session_screen_age_eligibility,
         "22) Both IPV and OPV are administered to eligible children as per protocol" = ipv_session_administer_both_vaccines,
         "23) Social mobilizer is present and actively supporting the vaccination team" = ipv_session_s_mwith_team,
         "24) Caregivers aware of the fIPV campaign before the team's visit" = ipv_session_caregivers_aware_before_visit,
         "25) Local community is supporting mobilization activities" = ipv_session_community_supporting_mobilization,
         "26) Masjid announcements are taking place" = ipv_session_sm_assisting_with_community_announcements, 
         "27) Team is visited by supervisor in the field" = ipv_session_supervisor_visit_teams,
         "28) Microplan includes daily target" = ipv_session_microplan_include_daily_target
         ) %>%
  janitor::remove_empty(which=c("cols"))


         



icm_pcts <- icm_pcts_pre_switch %>%
  bind_rows(icm_pcts_post_switch) %>%
  bind_rows(icm_pcts_period3)

# icm_pcts_cluster_numerator <- icm_pcts %>%
#   ungroup() %>%
#   select(-c("visit_date_fmt", "campaign_day")) %>%
#   group_by(rcode, pcode, dcode, ccode,  
#            region, province, district, clustername,
#            form_type, age_group, 
#            campaigns) %>%
#   summarise_all(~sum(., na.rm=T)) %>%
#   ungroup() %>%
#   pivot_longer(cols = -c("rcode", "pcode", "dcode", "ccode",  
#                          "region", "province", "district", "clustername",
#                          "form_type", "age_group", "campaigns"),
#                names_to = "indicator",
#                values_to = "numerator")

icm_pcts_cluster_numerator <- as.data.table(icm_pcts)
icm_pcts_cluster_numerator <- icm_pcts_cluster_numerator[
  , lapply(.SD, function(x) sum(as.numeric(x), na.rm = TRUE)),
  by = .(rcode, pcode, dcode, ccode, region, province, district, clustername, form_type, age_group, campaigns),
  .SDcols = !c("visit_date_fmt", "campaign_day")
]
icm_pcts_cluster_numerator <- melt(
  icm_pcts_cluster_numerator,
  id.vars = c("rcode", "pcode", "dcode", "ccode", "region", "province", "district", "clustername", "form_type", "age_group", "campaigns"),
  variable.name = "indicator",
  value.name = "numerator"
)

icm_pcts_district_day_numerator <- as.data.table(icm_pcts)

# Summarize by groups and calculate the sum for all other columns
icm_pcts_district_day_numerator <- icm_pcts_district_day_numerator[
  , lapply(.SD, function(x) sum(as.numeric(x), na.rm = TRUE)),
  by = .(rcode, pcode, dcode, region, province, district, form_type, age_group, campaign_day, campaigns),
  .SDcols = !c("visit_date_fmt")
]

# Melt the data.table to transform it into long format
icm_pcts_district_day_numerator <- melt(
  icm_pcts_district_day_numerator,
  id.vars = c("rcode", "pcode", "dcode", "ccode", "region", "province", "district", "clustername", "form_type", "age_group", "campaigns", "campaign_day"),
  variable.name = "indicator",
  value.name = "numerator"
)

icm_pcts_cluster_denominator <- as.data.table(icm_pcts)

# Summarize by groups and count non-NA values for all other columns
icm_pcts_cluster_denominator <- icm_pcts_cluster_denominator[
  , lapply(.SD, function(x) sum(!is.na(x))),
  by = .(rcode, pcode, dcode, ccode, region, province, district, clustername, form_type, age_group, campaigns),
  .SDcols = !c("visit_date_fmt", "campaign_day")
]

# Melt the data.table to transform it into long format
icm_pcts_cluster_denominator <- melt(
  icm_pcts_cluster_denominator,
  id.vars = c("rcode", "pcode", "dcode", "ccode", "region", "province", "district", "clustername", "form_type", "age_group", "campaigns"),
  variable.name = "indicator",
  value.name = "denominator"
)

# Filter out rows where the denominator is 0
icm_pcts_cluster_denominator <- icm_pcts_cluster_denominator[denominator != 0]

icm_pcts_district_day_denominator <- as.data.table(icm_pcts)

# Summarize by groups and count non-NA values for all other columns
icm_pcts_district_day_denominator <- icm_pcts_district_day_denominator[
  , lapply(.SD, function(x) sum(!is.na(x))),
  by = .(rcode, pcode, dcode, region, province, district, form_type, age_group, campaign_day, campaigns),
  .SDcols = !c("visit_date_fmt")
]

# Melt the data.table to transform it into long format
icm_pcts_district_day_denominator <- melt(
  icm_pcts_district_day_denominator,
  id.vars = c("rcode", "pcode", "dcode", "region", "province", "district", "form_type", "age_group", "campaigns", "campaign_day"),
  variable.name = "indicator",
  value.name = "denominator"
)

# Filter out rows where the denominator is 0
icm_pcts_district_day_denominator <- icm_pcts_district_day_denominator[denominator != 0]

icm_pcts_cluster <- icm_pcts_cluster_denominator %>%
  filter(!is.na(denominator) & denominator != 0) %>%
  left_join(icm_pcts_cluster_numerator,
            by = c("rcode", "pcode", "dcode", "ccode",  
                   "region", "province", "district", "clustername",
                   "form_type", "age_group", "campaigns", "indicator")) %>%
  mutate(pct = ifelse(!is.na(denominator) & denominator != 0 & !is.na(numerator), numerator / denominator, NA_real_))

icm_pcts_day_district <- icm_pcts_district_day_denominator %>%
  filter(!is.na(denominator) & denominator != 0) %>%
  left_join(icm_pcts_district_day_numerator %>%
              select(-c("region", "province", "district")),
            by=c("rcode", "pcode", "dcode",  
                 "form_type", "age_group", "campaigns", "campaign_day", "indicator")) %>%
  mutate(pct = ifelse(!is.na(denominator) & denominator != 0 & !is.na(numerator), numerator / denominator, NA_integer_)) 

icm_pcts_district <- icm_pcts_cluster %>%
  select(-c("ccode", "clustername", "pct")) %>%
  group_by(rcode, pcode, dcode, 
           region, province, district, 
           form_type, age_group,
           campaigns, indicator) %>%
  summarise_at(c("numerator", "denominator"), ~sum(., na.rm=T)) %>%
  mutate(pct = ifelse(!is.na(denominator) & denominator != 0 & !is.na(numerator), numerator / denominator, NA_integer_)) %>%
  ungroup()

icm_pcts_day_district <- icm_pcts_day_district %>%
  select(-c("pct")) %>%
  group_by(rcode, pcode, dcode, 
           region, province, district, 
           form_type, age_group,
           campaigns, campaign_day, indicator) %>%
  summarise_at(c("numerator", "denominator"), ~sum(., na.rm=T)) %>%
  mutate(pct = ifelse(!is.na(denominator) & denominator != 0 & !is.na(numerator), numerator / denominator, NA_integer_)) %>%
  ungroup()

icm_pcts_province <- icm_pcts_district %>%
  select(-c("dcode", "district", "pct")) %>%
  group_by(rcode, pcode, 
           region, province, 
           form_type, age_group,
           campaigns, indicator) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  mutate(pct = ifelse(!is.na(denominator) & denominator != 0 & !is.na(numerator), numerator / denominator, NA_integer_)) %>%
  ungroup()

icm_pcts_day_province <- icm_pcts_day_district %>%
  select(-c("dcode", "district", "pct")) %>%
  group_by(rcode, pcode, 
           region, province, 
           form_type, age_group,
           campaigns, campaign_day, indicator) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  mutate(pct = ifelse(!is.na(denominator) & denominator != 0 & !is.na(numerator), numerator / denominator, NA_integer_)) %>%
  ungroup()

icm_pcts_region <- icm_pcts_province %>%
  select(-c("pcode", "province", "pct")) %>%
  group_by(rcode, 
           region, 
           form_type, age_group,
           campaigns, indicator) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  mutate(pct = ifelse(!is.na(denominator) & denominator != 0 & !is.na(numerator), numerator / denominator, NA_integer_)) %>%
  ungroup()

icm_pcts_day_region <- icm_pcts_day_province %>%
  select(-c("pcode", "province", "pct")) %>%
  group_by(rcode, 
           region, 
           form_type, age_group,
           campaigns, campaign_day, indicator) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  mutate(pct = ifelse(!is.na(denominator) & denominator != 0 & !is.na(numerator), numerator / denominator, NA_integer_)) %>%
  ungroup()

icm_pcts_campaign <- icm_pcts_region %>%
  select(-c("rcode", "region", "pct")) %>%
  group_by(form_type, age_group,
           campaigns, indicator) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  mutate(pct = ifelse(!is.na(denominator) & denominator != 0 & !is.na(numerator), numerator / denominator, NA_integer_)) %>%
  ungroup()

icm_pcts_day_campaign <- icm_pcts_day_region %>%
  select(-c("rcode", "region", "pct")) %>%
  group_by(form_type, age_group,
           campaigns, campaign_day, indicator) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  mutate(pct = ifelse(!is.na(denominator) & denominator != 0 & !is.na(numerator), numerator / denominator, NA_integer_)) %>%
  ungroup()

icm_completeness_cluster <- icm_pcts_cluster %>%
  filter(age_group == "0-59 Months" | is.na(age_group)) %>%
  select(campaigns, region, province, district, clustername,
         rcode, pcode, dcode, ccode, form_type) %>%
  unique() %>%
  mutate(reported = 1) %>%
  full_join(campaign_rpdc %>%
              select(campaign_name, 
                     rcode, pcode, dcode, ccode) %>%
              distinct() %>%
              mutate(form_type = "Household Monitoring"),
            by=c("campaigns" = "campaign_name",
                 "rcode", "pcode", "dcode", "ccode",
                 "form_type")) %>%
  full_join(campaign_rpdc %>%
              select(campaign_name, 
                     rcode, pcode, dcode, ccode) %>%
              distinct() %>%
              mutate(form_type = "Monitoring for Revisit Strategy"),
            by=c("campaigns" = "campaign_name",
                 "rcode", "pcode", "dcode", "ccode",
                 "form_type")) %>%
  full_join(campaign_rpdc %>%
              select(campaign_name, 
                     rcode, pcode, dcode, ccode) %>%
              distinct() %>%
              mutate(form_type = "Supervisor Monitoring"),
            by=c("campaigns" = "campaign_name",
                 "rcode", "pcode", "dcode", "ccode",
                 "form_type")) %>%
  full_join(campaign_rpdc %>%
              select(campaign_name, 
                     rcode, pcode, dcode, ccode) %>%
              distinct() %>%
              mutate(form_type = "Team Monitoring"),
            by=c("campaigns" = "campaign_name",
                 "rcode", "pcode", "dcode", "ccode",
                 "form_type")) %>%
  full_join(campaign_rpdc %>%
              select(campaign_name, 
                     rcode, pcode, dcode, ccode) %>%
              distinct() %>%
              mutate(form_type = "Site Monitoring"),
            by=c("campaigns" = "campaign_name",
                 "rcode", "pcode", "dcode", "ccode",
                 "form_type")) %>%
  full_join(campaign_rpdc %>%
              select(campaign_name, 
                     rcode, pcode, dcode, ccode) %>%
              distinct() %>%
              mutate(form_type = "IPV Session Monitoring"),
            by=c("campaigns" = "campaign_name",
                 "rcode", "pcode", "dcode", "ccode",
                 "form_type")) %>%
  mutate(reported = ifelse(is.na(reported),0,reported)) %>%
  mutate(denominator = 1,
         pct = reported) %>%
  rename(numerator = reported) %>%
  mutate(age_group = ifelse(form_type %in% c("Household Monitoring", "Monitoring for Revisit Strategy"), "0-59 Months", NA_character_)) %>%
  mutate(indicator = "Percent of Clusters with ICM Form Reported") %>%
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
  arrange(campaigns, region, province, district, ccode)

icm_completeness_district <- icm_completeness_cluster %>%
  group_by(campaigns, region, province, district,
           rcode, pcode, dcode, 
           form_type, age_group, indicator) %>%
  summarise_at(c("numerator", "denominator"), ~sum(., na.rm=T)) %>%
  mutate(pct = numerator/denominator)

icm_completeness_province <- icm_completeness_cluster %>%
  group_by(campaigns, region, province,
           rcode, pcode, 
           form_type, age_group, indicator) %>%
  summarise_at(c("numerator", "denominator"), ~sum(., na.rm=T)) %>%
  mutate(pct = numerator/denominator)

icm_completeness_region <- icm_completeness_cluster %>%
  group_by(campaigns, region,
           rcode, 
           form_type, age_group, indicator) %>%
  summarise_at(c("numerator", "denominator"), ~sum(., na.rm=T)) %>%
  mutate(pct = numerator/denominator)

icm_completeness_national <- icm_completeness_cluster %>%
  group_by(campaigns, 
           form_type, age_group, indicator) %>%
  summarise_at(c("numerator", "denominator"), ~sum(., na.rm=T)) %>%
  mutate(pct = numerator/denominator)

icm_pcts_cluster <- icm_pcts_cluster %>%
  bind_rows(icm_completeness_cluster)

icm_pcts_district <- icm_pcts_district %>%
  bind_rows(icm_completeness_district)

icm_pcts_province <- icm_pcts_province %>%
  bind_rows(icm_completeness_province)

icm_pcts_region <- icm_pcts_region %>%
  bind_rows(icm_completeness_region)

icm_pcts_campaign <- icm_pcts_campaign %>%
  bind_rows(icm_completeness_national)

