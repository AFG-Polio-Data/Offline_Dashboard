# ICM Household Indicators

icm_household_cluster_aggregated <- icm_household_h2h_numeric %>%
  select(-c("visit_date_fmt", "campaign_day")) %>%
  group_by(form_type, rcode, pcode, dcode, ccode,  
           region, province, district, clustername, campaigns, age_group) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  ungroup()

icm_household_cluster_day_aggregated <- icm_household_h2h_numeric %>%
  select(-c("visit_date_fmt")) %>%
  group_by(form_type, rcode, pcode, dcode,  
           region, province, district, ccode, clustername, campaigns, campaign_day, age_group) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  ungroup()

icm_household_cluster_coverage_recall <- icm_household_cluster_aggregated %>%
  select(form_type, rcode, pcode, dcode,  
         region, province, district, ccode, clustername, campaigns, age_group,
         children_vaccinated_recall, children_living_in_houses) %>%
  rename(numerator = children_vaccinated_recall,
         denominator = children_living_in_houses) %>%
  mutate(indicator = "H2H: Coverage, recall") %>%
  mutate(pct = case_when(!is.na(numerator) & !is.na(denominator) & denominator != 0 ~ numerator / denominator,
                         TRUE ~ NA_integer_))

icm_household_cluster_day_coverage_recall <- icm_household_cluster_day_aggregated %>%
  select(form_type, rcode, pcode, dcode, ccode,  
         region, province, district, clustername, campaigns, campaign_day, age_group,
         children_vaccinated_recall, children_living_in_houses) %>%
  rename(numerator = children_vaccinated_recall,
         denominator = children_living_in_houses) %>%
  mutate(indicator = "H2H: Coverage, recall") %>%
  mutate(pct = case_when(!is.na(numerator) & !is.na(denominator) & denominator != 0 ~ numerator / denominator,
                         TRUE ~ NA_integer_)) 

icm_household_cluster_coverage_fm <- icm_household_cluster_aggregated %>%
  select(form_type, rcode, pcode, dcode, ccode,  
         region, province, district, clustername, campaigns, age_group,
         children_seen_by_monitor, children_finger_marking) %>%
  rename(numerator = children_finger_marking,
         denominator = children_seen_by_monitor) %>%
  mutate(indicator = "H2H: Coverage, finger-marked") %>%
  mutate(pct = case_when(!is.na(numerator) &
                           !is.na(denominator) &
                           denominator != 0 ~ numerator / denominator,
                         TRUE ~ NA)) 

icm_household_cluster_day_coverage_fm <- icm_household_cluster_day_aggregated %>%
  select(form_type, rcode, pcode, dcode, ccode,  
         region, province, district, clustername, campaigns, campaign_day, age_group,
         children_seen_by_monitor, children_finger_marking) %>%
  rename(numerator = children_finger_marking,
         denominator = children_seen_by_monitor) %>%
  mutate(indicator = "H2H: Coverage, finger-marked") %>%
  mutate(pct = case_when(!is.na(numerator) &
                           !is.na(denominator) &
                           denominator != 0 ~ numerator / denominator,
                         TRUE ~ NA)) 

icm_household_cluster_coverage <- icm_household_cluster_coverage_fm %>%
  bind_rows(icm_household_cluster_coverage_recall)

icm_household_cluster_day_coverage <- icm_household_cluster_day_coverage_fm %>%
  bind_rows(icm_household_cluster_day_coverage_recall)

# mutate(recall_coverage, fm_coverage, missed_children, team_not_come, child_absent, newborn_sick_sleep, refusal, poor_screening, door_marking_correct, door_marking_incorrect, door_marking_not_marked, )
icm_household_district_coverage <- icm_household_cluster_coverage %>%
  select(form_type, rcode, pcode, dcode,  
         region, province, district, campaigns, age_group, indicator,
         numerator, denominator) %>%
  group_by(form_type, rcode, pcode, dcode,  
           region, province, district, campaigns, age_group, indicator) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  mutate(pct = case_when(!is.na(numerator) &
                           !is.na(denominator) &
                           denominator != 0 ~ numerator / denominator,
                         TRUE ~ NA)) %>%
  ungroup()

icm_household_district_day_coverage <- icm_household_cluster_day_coverage %>%
  select(form_type, rcode, pcode, dcode,  
         region, province, district, campaigns, campaign_day, age_group, indicator,
         numerator, denominator) %>%
  group_by(form_type, rcode, pcode, dcode,  
           region, province, district, campaigns, campaign_day, age_group, indicator) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  mutate(pct = case_when(!is.na(numerator) &
                           !is.na(denominator) &
                           denominator != 0 ~ numerator / denominator,
                         TRUE ~ NA)) %>%
  ungroup()

icm_household_province_coverage <- icm_household_district_coverage %>%
  select(form_type, rcode, pcode,  
         region, province, campaigns, age_group, indicator,
         numerator, denominator) %>%
  group_by(form_type, rcode, pcode,  
           region, province, campaigns, age_group, indicator) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  mutate(pct = case_when(!is.na(numerator) &
                           !is.na(denominator) &
                           denominator != 0 ~ numerator / denominator,
                         TRUE ~ NA)) %>%
  ungroup()

icm_household_province_day_coverage <- icm_household_district_day_coverage %>%
  select(form_type, rcode, pcode,  
         region, province, campaigns, campaign_day, age_group, indicator,
         numerator, denominator) %>%
  group_by(form_type, rcode, pcode,  
           region, province, campaigns, campaign_day, age_group, indicator) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  mutate(pct = case_when(!is.na(numerator) &
                           !is.na(denominator) &
                           denominator != 0 ~ numerator / denominator,
                         TRUE ~ NA)) %>%
  ungroup()

icm_household_region_coverage <- icm_household_province_coverage %>%
  select(form_type, rcode,  
         region, campaigns, age_group, indicator,
         numerator, denominator) %>%
  group_by(form_type, rcode,  
           region, campaigns, age_group, indicator) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  mutate(pct = case_when(!is.na(numerator) &
                           !is.na(denominator) &
                           denominator != 0 ~ numerator / denominator,
                         TRUE ~ NA)) %>%
  ungroup()

icm_household_region_day_coverage <- icm_household_province_day_coverage %>%
  select(form_type, rcode,  
         region, campaigns, campaign_day, age_group, indicator,
         numerator, denominator) %>%
  group_by(form_type, rcode,  
           region, campaigns, campaign_day, age_group, indicator) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  mutate(pct = case_when(!is.na(numerator) &
                           !is.na(denominator) &
                           denominator != 0 ~ numerator / denominator,
                         TRUE ~ NA)) %>%
  ungroup()

icm_household_campaign_coverage <- icm_household_region_coverage %>%
  select(form_type, campaigns, age_group, indicator,
         numerator, denominator) %>%
  group_by(form_type, campaigns, age_group, indicator) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  mutate(pct = case_when(!is.na(numerator) &
                           !is.na(denominator) &
                           denominator != 0 ~ numerator / denominator,
                         TRUE ~ NA)) %>%
  ungroup()

icm_household_campaign_day_coverage <- icm_household_region_day_coverage %>%
  select(form_type, campaigns, campaign_day, age_group, indicator,
         numerator, denominator) %>%
  group_by(form_type, campaigns, campaign_day, age_group, indicator) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  mutate(pct = case_when(!is.na(numerator) &
                           !is.na(denominator) &
                           denominator != 0 ~ numerator / denominator,
                         TRUE ~ NA)) %>%
  ungroup()

icm_household_reasons_missed_cluster <- icm_household_cluster_aggregated %>%
  ungroup() %>%
  select(form_type, rcode, pcode, dcode, ccode,  
         region, province, district, clustername, campaigns, age_group,
         team_not_come, child_absent, newborn_sick_sleep, refusal, poor_screening) %>%
  pivot_longer(cols=-c("form_type", "rcode", "pcode", "dcode", "ccode",  
                       "region", "province", "district", "clustername", "campaigns", "age_group"),
               names_to = "cat",
               values_to = "numerator") %>%
  mutate(indicator = "H2H: Reasons Missed") %>%
  filter(numerator > 0) %>%
  mutate(indicator_type = "cat_dist")

icm_household_door_marking_cluster <- icm_household_cluster_aggregated %>%
  ungroup() %>%
  select(form_type, rcode, pcode, dcode, ccode,  
         region, province, district, clustername, campaigns, age_group,
         door_marking_correct, door_marking_incorrect, door_marking_not_marked) %>%
  pivot_longer(cols=-c("form_type", "rcode", "pcode", "dcode",  
                       "region", "province", "district", "ccode", "clustername", "campaigns", "age_group"),
               names_to = "cat",
               values_to = "numerator") %>%
  mutate(indicator = "H2H: Door Marking") %>%
  filter(numerator > 0) %>%
  mutate(indicator_type = "cat_dist")

icm_cat_dist_cluster <- bind_rows(icm_household_door_marking_cluster,
                                  icm_household_reasons_missed_cluster,
) %>%
  mutate(cat = case_when(cat == "door_marking_correct" ~ "Correct",
                         cat == "door_marking_incorrect" ~ "Incorrect",
                         cat == "door_marking_not_marked" ~ "Not Marked",
                         cat == "child_absent" ~ "Child Absent",
                         cat == "newborn_sick_sleep" ~ "Newborn/Sick/Sleep",
                         cat == "poor_screening" ~ "Ignored by team",
                         cat == "refusal" ~ "Refusal",
                         cat == "team_not_come" ~ "Team not come",
                         TRUE ~ cat))

icm_cat_dist_district <- icm_cat_dist_cluster %>%
  group_by(form_type, rcode, pcode, dcode,  
           region, province, district, campaigns, age_group, indicator, indicator_type, cat) %>%
  summarise(numerator = sum(numerator, na.rm=T)) %>%
  ungroup()

icm_cat_dist_province <- icm_cat_dist_district %>%
  group_by(form_type, rcode, pcode,  
           region, province, campaigns, age_group, indicator, indicator_type, cat) %>%
  summarise(numerator = sum(numerator, na.rm=T)) %>%
  ungroup()

icm_cat_dist_region <- icm_cat_dist_district %>%
  group_by(form_type, rcode,  
           region, campaigns, age_group, indicator, indicator_type, cat) %>%
  summarise(numerator = sum(numerator, na.rm=T)) %>%
  ungroup()

icm_cat_dist_campaign <- icm_cat_dist_district %>%
  group_by(form_type, campaigns, age_group, indicator, indicator_type, cat) %>%
  summarise(numerator = sum(numerator, na.rm=T)) %>%
  ungroup()