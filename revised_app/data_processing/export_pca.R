export_pca <- all_data_list$df_apmis_list$`PCA H2H` %>%
  select(-c("fm_0_59m_coverage", "fm_coverage")) %>%
  mutate_at(c("radio", "tv", "mullah", "teacher", "chw", "community_elder", "poster", "social_mobilizer", "other"), ~as.numeric(.)) %>%
  bind_rows(all_data_list$df_apmis_list$`PCA M2M` %>%
              select(-c("fm_0_59m_coverage", "fm_coverage")) %>%
              mutate_at(c("radio", "tv", "mullah"), ~as.numeric(.))) %>%
  bind_rows(all_data_list$df_apmis_list$`PCA S2S` %>%
              select(-c("fm_coverage")) %>%
              mutate_at(c("radio", "tv", "mullah"), ~as.numeric(.))) %>% 
  bind_rows(all_data_list$df_apmis_list$`SIA PCA` %>%
              mutate(visit_date_fmt = as.Date(visit_date_fmt)) %>%
              select(-c("fm_coverage")) %>%
              rename(social_mobilizer = sm,
                     other_reasons = other,
                     other = others) %>%
              mutate_at(c("total_hrmp_houses", "not_heared", "radio", "tv", "mullah", "social_mobilizer", "other", "community_elder", "visited_houses", "nomad", "returnees", "straddling", "idp", "non_hrmp",
                          "team_no", "t0_59_m", "total_children_no_fm", "team_did_not_visit", "newborn", "sleep", "sick", "other_reasons", "vaccinated_by_monitors",
                          "total_seen", "total_fm", "child_not_available", "no_one_home", "not_aware", "refusal"), ~as.numeric(.))) %>%
  bind_rows(all_data_list$df_apmis_list$`fIPV PCA` %>%
              mutate(visit_date = as.character(visit_date), 
                     visit_date_fmt = as.Date(visit_date_fmt)) %>%
              # select(-c("fm_coverage")) %>%
              rename(social_mobilizer = sm,
                     other_reasons = other,
                     other = others) %>%
              mutate_at(c("total_hrmp_houses", "not_heared", "radio", "tv", "mullah", "social_mobilizer", "other", "community_elder", "visited_houses", "nomad", "returnees", "straddling", "idp", "non_hrmp",
                          "team_no", "t0_59_m", "team_did_not_visit", "newborn", "sleep", "sick", "other_reasons",
                           "child_not_available", "no_one_home", "not_aware", "refusal"), ~as.numeric(.))) %>%
  select(campaigns, rcode, pcode, dcode, visited_houses, vaccinated_by_monitors, nomad, returnees, straddling, idp, non_hrmp, male_seen011m, male_fm011m, male_seen1259m, male_fm1259m, female_seen011m, female_fm011m, female_seen1259m, female_fm1259m) %>%
  group_by(campaigns, rcode, pcode, dcode) %>%
  summarise_all(~sum(as.numeric(.), na.rm=T)) %>%
  mutate_at(c("nomad", "returnees", "straddling", "idp", "non_hrmp", "male_seen011m", "male_fm011m", "male_seen1259m", "male_fm1259m", "female_seen011m", "female_fm011m", "female_seen1259m", "female_fm1259m"),
            ~ifelse(campaigns %in% (df_campaigns %>%
                                      select(campaign_name, campaign_startdate) %>%
                                      unique() %>%
                                      filter(campaign_startdate <= as.Date("2024-10-01")))$campaign_name, NA_integer_, .))

export_pca_summary <- all_data_list$apmis_indicators$district_indicators %>%
  filter(data_source == "pca") %>%
  filter(indicator %in% c("pca_fm_coverage_0_11m", "pca_fm_coverage_12_59m", "pca_fm_coverage_0_59m",
                          "pca_recall_coverage_0_59m", "pca_fm_coverage_ipv")) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, indicator, numerator, denominator) 

export_pca_summary1 <- export_pca_summary %>%
  filter(indicator == "pca_fm_coverage_0_11m") %>%
  rename("fmarked_0_11m" = numerator,
         "screened_0_11m" = denominator) %>%
  select(-c("indicator"))

export_pca_summary2 <- export_pca_summary %>%
  filter(indicator == "pca_fm_coverage_0_59m") %>%
  rename("fmarked_0_59m" = numerator,
         "screened_0_59m" = denominator) %>%
  select(-c("indicator"))

export_pca_summary3 <- export_pca_summary %>%
  filter(indicator == "pca_fm_coverage_12_59m") %>%
  rename("fmarked_12_59m" = numerator,
         "screened_12_59m" = denominator) %>%
  select(-c("indicator"))

export_pca_summary4 <- export_pca_summary %>%
  filter(indicator == "pca_recall_coverage_0_59m") %>%
  rename("recall_vaccinated_0_59m" = numerator,
         "recall_total_children" = denominator) %>%
  select(-c("indicator"))

export_pca_summary5 <- export_pca_summary %>%
  filter(indicator == "pca_fm_coverage_ipv") %>%
  rename("ipv_fmarked_4_59m" = numerator,
         "ipv_screened_4_59m" = denominator) %>%
  select(-c("indicator"))


export_pca_summary <- export_pca_summary1 %>%
  full_join(export_pca_summary2) %>%
  full_join(export_pca_summary3) %>%
  full_join(export_pca_summary4) %>%
  full_join(export_pca_summary5) %>%
  filter(!is.na(region) & !is.na(province) & !is.na(district) &
           !is.na(rcode) & !is.na(pcode) & !is.na(dcode)) %>%
  rowwise() %>%
  mutate(recall_coverage_0_59m = paste0(round(recall_vaccinated_0_59m / recall_total_children, 3)*100,"%"),
         fm_coverage_0_59m = paste0(round(fmarked_0_59m / screened_0_59m, 3)*100,"%"),
         fm_coverage_0_11m = paste0(round(fmarked_0_11m / screened_0_11m, 3)*100,"%"),
         fm_coverage_12_59m = paste0(round(fmarked_12_59m / screened_12_59m, 3)*100,"%"),
         ipv_fm_coverage_4_59m = paste0(round(ipv_fmarked_4_59m / ipv_screened_4_59m, 3)*100,"%")) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode,
         recall_total_children,recall_vaccinated_0_59m, recall_coverage_0_59m,
         screened_0_11m, screened_12_59m, screened_0_59m,
         fmarked_0_11m, fmarked_12_59m, fmarked_0_59m,
         fm_coverage_0_11m, fm_coverage_12_59m, fm_coverage_0_59m, 
         ipv_fmarked_4_59m, ipv_screened_4_59m,
         ipv_fm_coverage_4_59m
  ) 

export_pca_modality <- all_data_list$apmis_indicators$district_indicators %>%
  filter(data_source == "pca") %>%
  filter(indicator == "pca_modality") %>%
  select(campaign_name, rcode, pcode, dcode, value) %>%
  rename(modality = value)

export_pca_reasons_missed <- all_data_list$apmis_indicators$district_indicators %>%
  filter(data_source == "pca") %>%
  filter(indicator == "pca_reasons_missed") %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, category, numerator) %>%
  group_by(campaign_name, region, rcode, province, pcode, district, dcode, category) %>%
  summarise(numerator = sum(numerator, na.rm=T)) %>%
  ungroup() %>%
  pivot_wider(names_from = category, 
              values_from = numerator) %>%
  select(campaign_name, rcode, pcode, dcode, 
         team_did_not_visit, absent_school_madarasa_hf, absent_market_street, absent_travel, absent_others, refusal_misperception, refusal_decision_maker_notat_home, refusal, newborn, sick, sleep, other_reasons, other, child_not_available, no_one_home, not_aware, mosque_is_far, site_is_far, house_far_from_site, team_did_not_visit_the_site_area) %>%
  rowwise() %>%
  mutate(other_reasons = sum(other_reasons, other, na.rm=T),
         mosque_is_far = sum(mosque_is_far, site_is_far, house_far_from_site, na.rm=T),
         team_did_not_visit = sum(team_did_not_visit, team_did_not_visit_the_site_area, na.rm=T)) %>%
  ungroup() %>%
  select(-c("other", "site_is_far", "house_far_from_site", "team_did_not_visit_the_site_area"))

export_pca_door_marking <- all_data_list$apmis_indicators$district_indicators %>%
  filter(data_source == "pca") %>%
  filter(indicator == "pca_door_marking") %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, category, numerator) %>%
  group_by(campaign_name, region, rcode, province, pcode, district, dcode, category) %>%
  summarise(numerator = sum(numerator, na.rm=T)) %>%
  ungroup() %>%
  pivot_wider(names_from = category, 
              values_from = numerator) %>%
  select(campaign_name, rcode, pcode, dcode, 
         dm_correct, dm_in_correct, no_door_markings)

export_pca_awareness <- all_data_list$apmis_indicators$district_indicators %>%
  filter(data_source == "pca") %>%
  filter(indicator == "pca_awareness_yn") %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, numerator, denominator) %>%
  group_by(campaign_name, region, rcode, province, pcode, district, dcode) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(no_awareness = denominator - numerator) %>%
  ungroup() %>%
  rename(aware = numerator) %>%
  select(campaign_name, rcode, pcode, dcode, 
         aware, no_awareness)

export_pca_awareness_source <- all_data_list$apmis_indicators$district_indicators %>%
  filter(data_source == "pca") %>%
  filter(indicator == "pca_awareness_source") %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, category, numerator) %>%
  group_by(campaign_name, region, rcode, province, pcode, district, dcode, category) %>%
  summarise(numerator = sum(numerator, na.rm=T)) %>%
  ungroup() %>%
  pivot_wider(names_from = category, 
              values_from = numerator) 
for(i in c("radio", "tv", "mullah", "teacher", "chw", "community_elder", "poster", "social_mobilizer", "sm", "other")){
  if(!(i %in% colnames(export_pca_awareness_source))){
    export_pca_awareness_source[i] <- 0
  }
}
export_pca_awareness_source  <- export_pca_awareness_source %>%
  select(campaign_name, rcode, pcode, dcode, 
         radio, tv, mullah, teacher, chw, community_elder, poster, social_mobilizer, sm, other) %>%
  rowwise() %>%
  mutate(social_mobilizer = sum(social_mobilizer, sm, na.rm=T)) %>%
  ungroup() %>%
  select(-c("sm"))

export_pca_final <- export_pca %>%
  rename(campaign_name = campaigns) %>%
  right_join(export_pca_modality,
             by = c("campaign_name", "rcode", "pcode", "dcode")) %>%
  full_join(export_pca_summary,
            by = c("campaign_name", "rcode", "pcode", "dcode")) %>%
  full_join(export_pca_reasons_missed,
            by = c("campaign_name", "rcode", "pcode", "dcode")) %>%
  full_join(export_pca_door_marking,
            by = c("campaign_name", "rcode", "pcode", "dcode")) %>%
  full_join(export_pca_awareness,
            by = c("campaign_name", "rcode", "pcode", "dcode")) %>%
  full_join(export_pca_awareness_source,
            by = c("campaign_name", "rcode", "pcode", "dcode")) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, 
         modality, visited_houses, vaccinated_by_monitors,
         recall_total_children, recall_vaccinated_0_59m,
         recall_coverage_0_59m, 
         screened_0_11m, screened_12_59m, screened_0_59m,                 
         fmarked_0_11m, fmarked_12_59m, fmarked_0_59m,
         everything()) %>%
  filter(!is.na(region) & !is.na(rcode) & !is.na(province) & !is.na(pcode) &
           !is.na(district) & !is.na(dcode) & !is.na(campaign_name)) %>%
  mutate(recall_coverage_0_59m = ifelse(campaign_name %in% (df_campaigns %>%
                                                              select(campaign_name, campaign_startdate) %>%
                                                              unique() %>%
                                                              filter(campaign_startdate <= as.Date("2024-10-01")))$campaign_name, recall_coverage_0_59m, NA_character_)) %>%
  rowwise() %>%
  mutate(fm_coverage_male_0_11m = paste0(round(male_fm011m/male_seen011m, 3)*100, "%"),
         fm_coverage_female_0_11m = paste0(round(female_fm011m/female_seen011m, 3)*100, "%"),
         fm_coverage_male_12_59m = paste0(round(male_fm1259m/male_seen1259m, 3)*100, "%"),
         fm_coverage_female_12_59m = paste0(round(female_fm1259m/female_seen1259m, 3)*100, "%"),
         fm_coverage_male_0_59m = paste0(round(sum(male_fm011m, male_fm1259m, na.rm=T)/sum(male_seen011m, male_seen1259m, na.rm=T), 3)*100, "%"),
         fm_coverage_female_0_59m = paste0(round(sum(female_fm011m, female_fm1259m, na.rm=T)/sum(female_seen011m, female_seen1259m, na.rm=T), 3)*100, "%")) %>%
  ungroup() %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, modality, visited_houses, vaccinated_by_monitors, 
         recall_total_children, recall_vaccinated_0_59m, recall_coverage_0_59m, 
         male_seen011m, female_seen011m, screened_0_11m, 
         male_seen1259m, female_seen1259m, screened_12_59m, 
         screened_0_59m, 
         male_fm011m, female_fm011m, fmarked_0_11m, 
         male_fm1259m, female_fm1259m, fmarked_12_59m, 
         fmarked_0_59m, 
         fm_coverage_male_0_11m, fm_coverage_female_0_11m, fm_coverage_0_11m, 
         fm_coverage_male_12_59m, fm_coverage_female_12_59m, fm_coverage_12_59m, 
         fm_coverage_male_0_59m, fm_coverage_female_0_59m, fm_coverage_0_59m,
         ipv_screened_4_59m, ipv_fmarked_4_59m, ipv_fm_coverage_4_59m,
         team_did_not_visit, absent_school_madarasa_hf, absent_market_street, absent_travel, absent_others, 
         refusal_misperception, refusal_decision_maker_notat_home, refusal, newborn, sick, sleep, other_reasons, 
         child_not_available, no_one_home, not_aware, mosque_is_far, 
         dm_correct, dm_in_correct, no_door_markings, 
         aware, no_awareness, 
         radio, tv, mullah, teacher, chw, community_elder, poster, social_mobilizer, other,
         nomad, returnees, straddling, idp, non_hrmp) %>%
  mutate(across((c("fm_coverage_male_0_11m", "fm_coverage_female_0_11m", 
                   "fm_coverage_male_12_59m", "fm_coverage_female_12_59m",
                   "fm_coverage_male_0_59m", "fm_coverage_female_0_59m")), 
                ~ifelse(campaign_name %in% (df_campaigns %>%
                                              select(campaign_name, campaign_startdate) %>%
                                              unique() %>%
                                              filter(campaign_startdate <= as.Date("2024-10-01")))$campaign_name, NA_character_, .))) %>%
  arrange(campaign_name, region, province, district)  %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, everything()) 

all_data_list$export_pca <- export_pca_final

