### --- PCA Data --------------------------------
# Combine PCA data from PCA H2H, PCA S2S and PCA M2M
all_pca <- combine_pca_modalities(apmis_pca_h2h = df_apmis_list[["PCA H2H"]], 
                                  apmis_pca_m2m = df_apmis_list[["PCA M2M"]],
                                  apmis_pca_s2s = df_apmis_list[["PCA S2S"]],
                                  new_apmis_pca_s2s = df_apmis_list[["SIA PCA"]],
                                  fipv_pca = df_apmis_list[["fIPV PCA"]]) |>
  mutate(campaign_uuid = as.character(campaign_uuid))


all_pca_5_10 <- df_apmis_list[["PCA (5-10)"]] %>%
  mutate(recall_5_10y_coverage = 100*(v510y/t510y_home),
         fm_5_10y_coverage = 100*(fm510y / t510y_seen)) %>%
  mutate(campaign_uuid = as.character(campaign_uuid))


modality_cluster_summary <- process_modality_summary(all_pca, c("region", "province", "district", "clustername", "rcode", "pcode", "dcode", "ccode", "campaign_uuid", "modality"))
modality_district_summary <- process_modality_summary(all_pca, c("region", "province", "district", "rcode", "pcode", "dcode", "campaign_uuid", "modality"))
modality_province_summary <- process_modality_summary(all_pca, c("region", "province", "rcode", "pcode", "campaign_uuid", "modality"))
modality_region_summary <- process_modality_summary(all_pca, c("region", "rcode", "campaign_uuid", "modality"))
modality_national_summary <- process_modality_summary(all_pca, c("campaign_uuid", "modality"))

# Summarize reasons for missed children (n and pct), by campaign and district
pca_reasons_missed_by_campaign_and_cluster <- all_pca %>%
  group_by(campaign_uuid, region, rcode, province, pcode, district, dcode, clustername, ccode) %>%
  summarise(
    across(
      c(
        "team_did_not_visit", "absent_school_madarasa_hf", "absent_market_street", "absent_travel", "absent_others",
        "refusal_misperception", "refusal_decision_maker_notat_home", "refusal", "newborn", "sleep", "sick",
        "other_reasons", "other", "child_not_available", "no_one_home", "mosque_is_far", "not_aware", 
        "site_is_far", "house_far_from_site", "team_did_not_visit_the_site_area"
      ),
      ~sum(as.numeric(.), na.rm = TRUE)
    )
  ) %>%
  pivot_longer(
    cols = -c(campaign_uuid, region, rcode, province, pcode, district, dcode, clustername, ccode),
    names_to = "pca_reason",
    values_to = "count"
  ) %>%
  mutate(campaign_uuid = as.character(campaign_uuid)) %>%
  left_join(df_campaigns, by = "campaign_uuid")

pca_reasons_missed_by_campaign_and_district <- all_pca %>%
  group_by(campaign_uuid, region, rcode, province, pcode, district, dcode) %>%
  summarise(
    across(
      c(
        "team_did_not_visit", "absent_school_madarasa_hf", "absent_market_street", "absent_travel", "absent_others",
        "refusal_misperception", "refusal_decision_maker_notat_home", "refusal", "newborn", "sleep", "sick",
        "other_reasons", "other", "child_not_available", "no_one_home", "mosque_is_far", "not_aware", 
        "site_is_far", "house_far_from_site", "team_did_not_visit_the_site_area"
      ),
      ~sum(as.numeric(.), na.rm = TRUE)
    )
  ) %>%
  pivot_longer(
    cols = -c(campaign_uuid, region, rcode, province, pcode, district, dcode),
    names_to = "pca_reason",
    values_to = "count"
  ) %>%
  mutate(campaign_uuid = as.character(campaign_uuid)) %>%
  left_join(df_campaigns, by = "campaign_uuid")

pca_reasons_missed_by_campaign_and_district_5_10 <- all_pca_5_10 %>%
  group_by(campaign_uuid, region, rcode, province, pcode, district, dcode) %>%
  summarise(
    across(
      c(
        "team_did_not_visit", "absent_school_madarasa_hf", "absent_market_street", "absent_travel", "absent_others",
        "refusal_misperception", "refusal_decision_maker_notat_home", "newborn", "sleep", "sick", "other_reasons"
      ),
      ~sum(as.numeric(.), na.rm = TRUE)
    )
  ) %>%
  pivot_longer(
    cols = -c(campaign_uuid, region, rcode, province, pcode, district, dcode),
    names_to = "pca_reason",
    values_to = "count"
  ) %>%
  mutate(campaign_uuid = as.character(campaign_uuid)) %>%
  left_join(df_campaigns, by = "campaign_uuid")

# Summarize door marking accuracy (n and pct), by campaign and region
pca_door_markings_by_campaign_and_cluster <- df_apmis_list[["PCA H2H"]] %>%
  group_by(campaign_uuid, region, rcode, province, pcode, district, dcode, clustername, ccode) %>% 
  summarise_at(c("dm_correct", "dm_in_correct", "no_door_markings"),
               ~sum(as.numeric(.), na.rm=T)) %>%
  pivot_longer(!c(campaign_uuid, region, rcode, province, pcode, district, dcode, clustername, ccode), names_to = "pca_reason", values_to = "count") %>%
  mutate(campaign_uuid = as.character(campaign_uuid)) |>
  full_join(df_campaigns, by = c('campaign_uuid'), relationship = "many-to-many") 

pca_door_markings_by_campaign_and_district <- df_apmis_list[["PCA H2H"]] %>%
  group_by(campaign_uuid, region, rcode, province, pcode, district, dcode) %>% 
  summarise_at(c("dm_correct", "dm_in_correct", "no_door_markings"),
               ~sum(as.numeric(.), na.rm=T)) %>%
  pivot_longer(!c(campaign_uuid, region, rcode, province, pcode, district, dcode), names_to = "pca_reason", values_to = "count") %>%
  mutate(campaign_uuid = as.character(campaign_uuid)) |>
  full_join(df_campaigns, by = c('campaign_uuid'), relationship = "many-to-many") 


# Summarize sources of campaign awareness, by campaign and modality
pca_campaign_awareness <- all_pca %>%
  group_by(campaign_uuid, region, rcode, province, pcode, district, dcode, clustername, ccode, modality) %>%
  summarise(
    across(
      c("radio", "tv", "mullah", "teacher", "chw", "community_elder", "poster", "social_mobilizer", "sm", "other"),
      ~sum(as.integer(.), na.rm = TRUE)
    )
  ) %>%
  pivot_longer(
    cols = -c(campaign_uuid, region, rcode, province, pcode, district, dcode, clustername, ccode, modality),
    names_to = "aware_type",
    values_to = "count"
  ) %>%
  mutate(campaign_uuid = as.character(campaign_uuid)) %>%
  left_join(df_campaigns, by = "campaign_uuid")


pca_campaign_awareness_yn <- all_pca %>%
  filter(!(formname %in% c("SIA PCA", "fIPV PCA"))) %>%
  group_by(campaign_uuid, region, rcode, province, pcode, district, dcode, clustername, ccode, modality) %>%
  summarise_at(c("heard_yes", "heard_no"),
               ~sum(as.numeric(.), na.rm=T)) %>%
  bind_rows(all_pca %>%
              filter(formname %in% c("SIA PCA", "fIPV PCA")) %>%
              mutate_at(c("heard_yes", "heard_no", "visited_houses", "not_heared"), ~as.numeric(.)) %>%
              mutate(heard_yes = case_when(!is.na(heard_yes) | !is.na(heard_no) ~ heard_yes,
                                           is.na(not_heared) ~ visited_houses,
                                           TRUE ~ visited_houses- not_heared),
                     heard_no = case_when(is.na(heard_yes) ~ visited_houses,
                                          TRUE ~ visited_houses - heard_yes)) %>%
              group_by(campaign_uuid, region, rcode, province, pcode, district, dcode, clustername, ccode, modality) %>%
              summarise_at(c("heard_yes", "heard_no"),
                           ~sum(as.numeric(.), na.rm=T))
  ) %>%
  pivot_longer(!c(campaign_uuid, region, rcode, province, pcode, district, dcode, clustername, ccode, modality), names_to = "aware", values_to = "count") %>%
  mutate(campaign_uuid = as.character(campaign_uuid)) |>
  full_join(df_campaigns, by = 'campaign_uuid', relationship = "many-to-many") 


# PCA Summaries
pca_summary_national <- summarize_pca(input_data = all_pca, grouping_variables = c("campaign_uuid", "campaigns"))

pca_summary_region <- summarize_pca(input_data = all_pca, grouping_variables = c("campaign_uuid", "campaigns", "region", "rcode"))

pca_summary_province <- summarize_pca(input_data = all_pca, grouping_variables = c("campaign_uuid", "campaigns", "region", "rcode", "province", "pcode"))


pca_summary_district <- summarize_pca(input_data = all_pca, grouping_variables = c("campaign_uuid", "campaigns", "region", "rcode", "province", "pcode", "district", "dcode"))

pca_summary_cluster <- summarize_pca(input_data = all_pca, grouping_variables = c("campaign_uuid", "campaigns", "region", "rcode", "province", "pcode", "district", "dcode", "clustername", "ccode"))

pca_summary_national_5_10 <- summarize_pca_5_10(input_data = all_pca_5_10, grouping_variables = c("campaign_uuid", "campaigns")) 

pca_summary_region_5_10 <- summarize_pca_5_10(input_data = all_pca_5_10, grouping_variables = c("campaign_uuid", "campaigns", "region", "rcode"))

pca_summary_province_5_10 <- summarize_pca_5_10(input_data = all_pca_5_10, grouping_variables = c("campaign_uuid", "campaigns", "region", "rcode", "province", "pcode"))


pca_summary_district_5_10 <- summarize_pca_5_10(input_data = all_pca_5_10, grouping_variables = c("campaign_uuid", "campaigns", "region", "rcode", "province", "pcode", "district", "dcode"))

pca_summary_cluster_5_10 <- summarize_pca_5_10(input_data = all_pca_5_10, grouping_variables = c("campaign_uuid", "campaigns", "region", "rcode", "province", "pcode", "district", "dcode", "clustername", "ccode"))

pca_completeness_cluster <- pca_summary_cluster %>%
  select(campaign_uuid, campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode) %>%
  mutate(reported = 1) %>%
  full_join(campaign_rpdc %>%
              select(campaign_uuid, campaign_name, rcode, pcode, dcode, ccode) %>%
              distinct() %>%
              rename(campaigns = campaign_name),
            by=c("campaign_uuid", "campaigns", "rcode", "pcode", "dcode", "ccode")) %>%
  mutate(reported = ifelse(is.na(reported), 0, reported)) %>%
  mutate(total = 1,
         pct_reported = reported) %>%
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


pca_completeness_district <- pca_completeness_cluster %>% 
  group_by(campaign_uuid, campaigns, region, rcode, province, pcode, district, dcode) %>%
  summarise(reported = sum(pct_reported, na.rm=T),
            total = n()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(pct_reported = reported / total) %>%
  ungroup()

pca_completeness_province <- pca_completeness_cluster %>% 
  group_by(campaign_uuid, campaigns, region, rcode, province, pcode) %>%
  summarise(reported = sum(pct_reported, na.rm=T),
            total = n()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(pct_reported = reported / total) %>%
  ungroup()

pca_completeness_region <- pca_completeness_cluster %>% 
  group_by(campaign_uuid, campaigns, region, rcode) %>%
  summarise(reported = sum(pct_reported, na.rm=T),
            total = n()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(pct_reported = reported / total) %>%
  ungroup()

pca_completeness_national <- pca_completeness_cluster %>% 
  group_by(campaign_uuid, campaigns) %>%
  summarise(reported = sum(pct_reported, na.rm=T),
            total = n()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(pct_reported = reported / total) %>%
  ungroup()

pca_completeness_cluster_5_10 <- pca_summary_cluster_5_10 %>%
  select(campaign_uuid, campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode) %>%
  mutate(reported = 1) %>%
  full_join(campaign_rpdc %>%
              select(campaign_uuid, campaign_name, rcode, pcode, dcode, ccode) %>%
              distinct() %>%
              rename(campaigns = campaign_name),
            by=c("campaign_uuid", "campaigns", "rcode", "pcode", "dcode", "ccode")) %>%
  mutate(reported = ifelse(is.na(reported), 0, reported)) %>%
  mutate(total = reported,
         pct_reported = reported) %>%
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

pca_completeness_district_5_10 <- pca_completeness_cluster_5_10 %>% 
  group_by(campaign_uuid, campaigns, region, rcode, province, pcode, district, dcode) %>%
  summarise(reported = sum(pct_reported, na.rm=T),
            total = n()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(pct_reported = reported / total) %>%
  ungroup()

pca_completeness_province_5_10 <- pca_completeness_cluster_5_10 %>% 
  group_by(campaign_uuid, campaigns, region, rcode, province, pcode) %>%
  summarise(reported = sum(pct_reported, na.rm=T),
            total = n()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(pct_reported = reported / total) %>%
  ungroup()

pca_completeness_region_5_10 <- pca_completeness_cluster_5_10 %>% 
  group_by(campaign_uuid, campaigns, region, rcode) %>%
  summarise(reported = sum(pct_reported, na.rm=T),
            total = n()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(pct_reported = reported / total) %>%
  ungroup()

pca_completeness_national_5_10 <- pca_completeness_cluster_5_10 %>% 
  group_by(campaign_uuid, campaigns) %>%
  summarise(reported = sum(pct_reported, na.rm=T),
            total = n()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(pct_reported = reported / total) %>%
  ungroup()

#HRMP summary
hrmp_campaigns <- all_pca %>% 
  select(campaign_uuid, hrm_pchildren_seen)  %>%
  filter(hrm_pchildren_seen > 0) %>%
  select(campaign_uuid) %>%
  unique()

pca_summary_cluster_hrmp <- all_pca %>%
  filter(campaign_uuid %in% hrmp_campaigns$campaign_uuid) %>%
  select(
    campaign_uuid, region, rcode, province, pcode, district, dcode,
    clustername, ccode, campaigns,
    hrm_pchildren_seen, total_hrmpfm, total_hrmp_houses, visited_houses,
    nomad, returnees, idp, straddling, non_hrmp
  ) %>%
  
  # Convert selected columns to numeric
  mutate(across(
    c(hrm_pchildren_seen, total_hrmpfm, total_hrmp_houses, visited_houses,
      nomad, returnees, idp, straddling, non_hrmp),
    as.numeric
  )) %>%
  
  # Replace NA in hrm_pchildren_seen with 0 if houses are recorded
  mutate(hrm_pchildren_seen = ifelse(is.na(hrm_pchildren_seen) & !is.na(total_hrmp_houses), 0, hrm_pchildren_seen)) %>%
  
  # Clean total_hrmpfm and cap at hrm_pchildren_seen
  mutate(total_hrmpfm = case_when(
    is.na(hrm_pchildren_seen) ~ NA_real_,
    hrm_pchildren_seen == 0 ~ 0,
    is.na(total_hrmpfm) & hrm_pchildren_seen > 0 ~ 0,
    total_hrmpfm > hrm_pchildren_seen ~ hrm_pchildren_seen,
    TRUE ~ total_hrmpfm
  )) %>%
  
  # Recalculate total_hrmp_houses from its components
  mutate(total_hrmp_houses = ifelse(is.na(nomad) & is.na(returnees) & is.na(idp) & is.na(straddling), NA_real_,
                                    rowSums(across(c(nomad, returnees, idp, straddling)), na.rm = TRUE)),
         non_hrmp = visited_houses - total_hrmp_houses) %>%

  # Round numeric columns to 0 decimals
  mutate(across(
    c(total_hrmpfm, hrm_pchildren_seen, total_hrmp_houses, visited_houses,
      nomad, returnees, idp, straddling, non_hrmp),
    ~round(., 0)
  )) %>%
  
  # Group by cluster and campaign info
  group_by(campaign_uuid, campaigns, region, rcode, province, pcode, district, dcode, clustername, ccode) %>%
  summarise(across(
    where(is.numeric),
    ~sum(as.numeric(.), na.rm = TRUE),
    .names = "{.col}"
  )) %>%
  ungroup() %>%
  
  # Calculate derived indicators
  rowwise() %>%
  mutate(
    hrmp_coverage_calc = ifelse(hrm_pchildren_seen == 0, NA, total_hrmpfm / hrm_pchildren_seen),
    hrmp_pct_of_houses = ifelse(visited_houses == 0 | is.na(total_hrmp_houses), NA, total_hrmp_houses / visited_houses),
    hrmp_pct_of_houses_nomad = ifelse(visited_houses == 0 | is.na(nomad), NA, nomad / visited_houses),
    hrmp_pct_of_houses_returnees = ifelse(visited_houses == 0 | is.na(returnees), NA, returnees / visited_houses),
    hrmp_pct_of_houses_idp = ifelse(visited_houses == 0 | is.na(idp), NA, idp / visited_houses),
    hrmp_pct_of_houses_straddling = ifelse(visited_houses == 0 | is.na(straddling), NA, straddling / visited_houses)
  ) %>%
  ungroup()

pca_summary_district_hrmp <- pca_summary_cluster_hrmp %>%
  select(-c("clustername", "ccode")) %>%
  group_by(campaign_uuid, campaigns, region, rcode, province, pcode, district, dcode) %>%
  summarise_all(~sum(as.numeric(.), na.rm=T)) %>%
  mutate(
    hrmp_coverage_calc = ifelse(hrm_pchildren_seen == 0, NA, total_hrmpfm / hrm_pchildren_seen),
    hrmp_pct_of_houses = ifelse(visited_houses == 0 | is.na(total_hrmp_houses), NA, total_hrmp_houses / visited_houses),
    hrmp_pct_of_houses_nomad = ifelse(visited_houses == 0 | is.na(nomad), NA, nomad / visited_houses),
    hrmp_pct_of_houses_returnees = ifelse(visited_houses == 0 | is.na(returnees), NA, returnees / visited_houses),
    hrmp_pct_of_houses_idp = ifelse(visited_houses == 0 | is.na(idp), NA, idp / visited_houses),
    hrmp_pct_of_houses_straddling = ifelse(visited_houses == 0 | is.na(straddling), NA, straddling / visited_houses)
  ) %>%
  ungroup()

pca_summary_province_hrmp <- pca_summary_district_hrmp %>%
  select(-c("district", "dcode")) %>%
  group_by(campaign_uuid, campaigns, region, rcode, province, pcode) %>%
  summarise_all(~sum(as.numeric(.), na.rm=T)) %>%
  mutate(
    hrmp_coverage_calc = ifelse(hrm_pchildren_seen == 0, NA, total_hrmpfm / hrm_pchildren_seen),
    hrmp_pct_of_houses = ifelse(visited_houses == 0 | is.na(total_hrmp_houses), NA, total_hrmp_houses / visited_houses),
    hrmp_pct_of_houses_nomad = ifelse(visited_houses == 0 | is.na(nomad), NA, nomad / visited_houses),
    hrmp_pct_of_houses_returnees = ifelse(visited_houses == 0 | is.na(returnees), NA, returnees / visited_houses),
    hrmp_pct_of_houses_idp = ifelse(visited_houses == 0 | is.na(idp), NA, idp / visited_houses),
    hrmp_pct_of_houses_straddling = ifelse(visited_houses == 0 | is.na(straddling), NA, straddling / visited_houses)
  ) %>%
  ungroup()

pca_summary_region_hrmp <- pca_summary_province_hrmp %>%
  select(-c("province", "pcode")) %>%
  group_by(campaign_uuid, campaigns, region, rcode) %>%
  summarise_all(~sum(as.numeric(.), na.rm=T)) %>%
  mutate(
    hrmp_coverage_calc = ifelse(hrm_pchildren_seen == 0, NA, total_hrmpfm / hrm_pchildren_seen),
    hrmp_pct_of_houses = ifelse(visited_houses == 0 | is.na(total_hrmp_houses), NA, total_hrmp_houses / visited_houses),
    hrmp_pct_of_houses_nomad = ifelse(visited_houses == 0 | is.na(nomad), NA, nomad / visited_houses),
    hrmp_pct_of_houses_returnees = ifelse(visited_houses == 0 | is.na(returnees), NA, returnees / visited_houses),
    hrmp_pct_of_houses_idp = ifelse(visited_houses == 0 | is.na(idp), NA, idp / visited_houses),
    hrmp_pct_of_houses_straddling = ifelse(visited_houses == 0 | is.na(straddling), NA, straddling / visited_houses)
  ) %>%
  ungroup()

pca_summary_national_hrmp <- pca_summary_region_hrmp %>%
  select(-c("region", "rcode")) %>%
  group_by(campaign_uuid, campaigns) %>%
  summarise_all(~sum(as.numeric(.), na.rm=T)) %>%
  mutate(
    hrmp_coverage_calc = ifelse(hrm_pchildren_seen == 0, NA, total_hrmpfm / hrm_pchildren_seen),
    hrmp_pct_of_houses = ifelse(visited_houses == 0 | is.na(total_hrmp_houses), NA, total_hrmp_houses / visited_houses),
    hrmp_pct_of_houses_nomad = ifelse(visited_houses == 0 | is.na(nomad), NA, nomad / visited_houses),
    hrmp_pct_of_houses_returnees = ifelse(visited_houses == 0 | is.na(returnees), NA, returnees / visited_houses),
    hrmp_pct_of_houses_idp = ifelse(visited_houses == 0 | is.na(idp), NA, idp / visited_houses),
    hrmp_pct_of_houses_straddling = ifelse(visited_houses == 0 | is.na(straddling), NA, straddling / visited_houses)
  ) %>%
  ungroup()

#Modality
ind_modality_cluster <- modality_cluster_summary %>%
  select(region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, modality) %>%
  rename(label = modality) %>%
  mutate(numerator = NA_integer_,
         denominator = NA_integer_,
         value = label,
         category = NA_character_,
         indicator = "modality",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, numerator, denominator, value, label) 

ind_modality_district <- modality_district_summary %>%
  select(region, province, district, rcode, pcode, dcode, campaign_name, modality) %>%
  rename(label = modality) %>%
  mutate(numerator = NA_integer_,
         denominator = NA_integer_,
         value = label,
         category = NA_character_,
         indicator = "modality",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_modality_province <- modality_province_summary %>%
  select(region, province, rcode, pcode, campaign_name, modality) %>%
  rename(label = modality) %>%
  mutate(numerator = NA_integer_,
         denominator = NA_integer_,
         value = label,
         category = NA_character_,
         indicator = "modality",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator, value, label) 

ind_modality_region <- modality_region_summary %>%
  select(region, rcode, campaign_name, modality) %>%
  rename(label = modality) %>%
  mutate(numerator = NA_integer_,
         denominator = NA_integer_,
         value = label,
         category = NA_character_,
         indicator = "modality",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator, value, label) 

ind_modality_national <- modality_national_summary %>%
  select(campaign_name, modality) %>%
  rename(label = modality) %>%
  mutate(numerator = NA_integer_,
         denominator = NA_integer_,
         value = label,
         category = NA_character_,
         indicator = "modality",
         data_source = "pca") %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator, value, label) 

#PCA FM Coverage
ind_pca_fm_cov_0_59m_cluster <- pca_summary_cluster %>%
  select(region, province, district, clustername, rcode, pcode, dcode, ccode, campaigns, children_seen_0_59m, children_fm_0_59m, fm_0_59m_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_0_59m,
         denominator = children_seen_0_59m,
         value = fm_0_59m_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_0_59m",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_0_59m_district <- pca_summary_district %>%
  select(region, province, district, rcode, pcode, dcode, campaigns, children_seen_0_59m, children_fm_0_59m, fm_0_59m_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_0_59m,
         denominator = children_seen_0_59m,
         value = fm_0_59m_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator,accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_0_59m",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_0_59m_province <- pca_summary_province %>%
  select(region, province, rcode, pcode, campaigns, children_seen_0_59m, children_fm_0_59m, fm_0_59m_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_0_59m,
         denominator = children_seen_0_59m,
         value = fm_0_59m_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_0_59m",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_0_59m_region <- pca_summary_region %>%
  select(region, rcode, campaigns, children_seen_0_59m, children_fm_0_59m, fm_0_59m_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_0_59m,
         denominator = children_seen_0_59m,
         value = fm_0_59m_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_0_59m",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_0_59m_national <- pca_summary_national %>%
  select(campaigns, children_seen_0_59m, children_fm_0_59m, fm_0_59m_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_0_59m,
         denominator = children_seen_0_59m,
         value = fm_0_59m_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_0_59m",
         data_source = "pca") %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator, value, label) 

#fm 0-11m
ind_pca_fm_cov_0_11m_cluster <- pca_summary_cluster %>%
  select(region, province, district, clustername, rcode, pcode, dcode, ccode, campaigns, children_seen_0_11m, children_fm_0_11m, fm_0_11m_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_0_11m,
         denominator = children_seen_0_11m,
         value = fm_0_11m_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_0_11m",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_0_11m_district <- pca_summary_district %>%
  select(region, province, district, rcode, pcode, dcode, campaigns, children_seen_0_11m, children_fm_0_11m, fm_0_11m_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_0_11m,
         denominator = children_seen_0_11m,
         value = fm_0_11m_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_0_11m",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_0_11m_province <- pca_summary_province %>%
  select(region, province, rcode, pcode, campaigns, children_seen_0_11m, children_fm_0_11m, fm_0_11m_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_0_11m,
         denominator = children_seen_0_11m,
         value = fm_0_11m_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_0_11m",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_0_11m_region <- pca_summary_region %>%
  select(region, rcode, campaigns, children_seen_0_11m, children_fm_0_11m, fm_0_11m_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_0_11m,
         denominator = children_seen_0_11m,
         value = fm_0_11m_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_0_11m",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_0_11m_national <- pca_summary_national %>%
  select(campaigns, children_seen_0_11m, children_fm_0_11m, fm_0_11m_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_0_11m,
         denominator = children_seen_0_11m,
         value = fm_0_11m_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_0_11m",
         data_source = "pca") %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator, value, label) 

#fm 12-59m
ind_pca_fm_cov_12_59m_cluster <- pca_summary_cluster %>%
  select(region, province, district, clustername, rcode, pcode, dcode, ccode, campaigns, children_seen_12_59m, children_fm_12_59m, fm_12_59m_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_12_59m,
         denominator = children_seen_12_59m,
         value = fm_12_59m_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_12_59m",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_12_59m_district <- pca_summary_district %>%
  select(region, province, district, rcode, pcode, dcode, campaigns, children_seen_12_59m, children_fm_12_59m, fm_12_59m_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_12_59m,
         denominator = children_seen_12_59m,
         value = fm_12_59m_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_12_59m",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_12_59m_province <- pca_summary_province %>%
  select(region, province, rcode, pcode, campaigns, children_seen_12_59m, children_fm_12_59m, fm_12_59m_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_12_59m,
         denominator = children_seen_12_59m,
         value = fm_12_59m_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_12_59m",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_12_59m_region <- pca_summary_region %>%
  select(region, rcode, campaigns, children_seen_12_59m, children_fm_12_59m, fm_12_59m_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_12_59m,
         denominator = children_seen_12_59m,
         value = fm_12_59m_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_12_59m",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_12_59m_national <- pca_summary_national %>%
  select(campaigns, children_seen_12_59m, children_fm_12_59m, fm_12_59m_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_12_59m,
         denominator = children_seen_12_59m,
         value = fm_12_59m_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_12_59m",
         data_source = "pca") %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator, value, label) 


#fm female
ind_pca_fm_cov_female_cluster <- pca_summary_cluster %>%
  select(region, province, district, clustername, rcode, pcode, dcode, ccode, campaigns, children_seen_female, children_fm_female, fm_female_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_female,
         denominator = children_seen_female,
         value = fm_female_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_female",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_female_district <- pca_summary_district %>%
  select(region, province, district, rcode, pcode, dcode, campaigns, children_seen_female, children_fm_female, fm_female_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_female,
         denominator = children_seen_female,
         value = fm_female_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_female",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_female_province <- pca_summary_province %>%
  select(region, province, rcode, pcode, campaigns, children_seen_female, children_fm_female, fm_female_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_female,
         denominator = children_seen_female,
         value = fm_female_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_female",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_female_region <- pca_summary_region %>%
  select(region, rcode, campaigns,  children_seen_female, children_fm_female, fm_female_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_female,
         denominator = children_seen_female,
         value = fm_female_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_female",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_female_national <- pca_summary_national %>%
  select(campaigns, children_seen_female, children_fm_female, fm_female_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_female,
         denominator = children_seen_female,
         value = fm_female_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_female",
         data_source = "pca") %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator, value, label) 

#fm male
ind_pca_fm_cov_male_cluster <- pca_summary_cluster %>%
  select(region, province, district, clustername, rcode, pcode, dcode, ccode, campaigns, children_seen_male, children_fm_male, fm_male_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_male,
         denominator = children_seen_male,
         value = fm_male_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_male",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_male_district <- pca_summary_district %>%
  select(region, province, district, rcode, pcode, dcode, campaigns, children_seen_male, children_fm_male, fm_male_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_male,
         denominator = children_seen_male,
         value = fm_male_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_male",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_male_province <- pca_summary_province %>%
  select(region, province, rcode, pcode, campaigns, children_seen_male, children_fm_male, fm_male_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_male,
         denominator = children_seen_male,
         value = fm_male_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_male",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_male_region <- pca_summary_region %>%
  select(region, rcode, campaigns,  children_seen_male, children_fm_male, fm_male_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_male,
         denominator = children_seen_male,
         value = fm_male_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_male",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_male_national <- pca_summary_national %>%
  select(campaigns, children_seen_male, children_fm_male, fm_male_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = children_fm_male,
         denominator = children_seen_male,
         value = fm_male_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_male",
         data_source = "pca") %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator, value, label) 


# fIPV fm coverage
ind_pca_fm_cov_fipv_cluster <- pca_summary_cluster %>%
  select(region, province, district, clustername, rcode, pcode, dcode, ccode, campaigns, ipv_found_fm459, ipv_seen459, fm_4_59m_ipv_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = ipv_found_fm459,
         denominator = ipv_seen459,
         value = fm_4_59m_ipv_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_ipv",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_fipv_district <- pca_summary_district %>%
  select(region, province, district, rcode, pcode, dcode, campaigns, ipv_found_fm459, ipv_seen459, fm_4_59m_ipv_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = ipv_found_fm459,
         denominator = ipv_seen459,
         value = fm_4_59m_ipv_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_ipv",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_fipv_province <- pca_summary_province %>%
  select(region, province, rcode, pcode, campaigns, ipv_found_fm459, ipv_seen459, fm_4_59m_ipv_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = ipv_found_fm459,
         denominator = ipv_seen459,
         value = fm_4_59m_ipv_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_ipv",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_fipv_region <- pca_summary_region %>%
  select(region, rcode, campaigns,  ipv_found_fm459, ipv_seen459, fm_4_59m_ipv_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = ipv_found_fm459,
         denominator = ipv_seen459,
         value = fm_4_59m_ipv_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_ipv",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator, value, label) 

ind_pca_fm_cov_fipv_national <- pca_summary_national %>%
  select(campaigns, ipv_found_fm459, ipv_seen459, fm_4_59m_ipv_coverage_calc) %>%
  rename(campaign_name = campaigns,
         numerator = ipv_found_fm459,
         denominator = ipv_seen459,
         value = fm_4_59m_ipv_coverage_calc) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_ipv",
         data_source = "pca") %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator, value, label) 

#PCA recall Coverage
ind_pca_recall_cov_0_59m_cluster <- pca_summary_cluster %>%
  select(region, province, district, clustername, rcode, pcode, dcode, ccode, campaigns, total_children_0_59m, total_vaccinated_recall_0_59m, recall_coverage_0_59m) %>%
  rename(campaign_name = campaigns,
         numerator = total_vaccinated_recall_0_59m,
         denominator = total_children_0_59m,
         value = recall_coverage_0_59m) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "recall_coverage_0_59m",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, numerator, denominator, value, label) %>%
  filter(campaign_name %in% (df_campaigns %>%
                               select(campaign_name, campaign_startdate) %>%
                               unique() %>%
                               filter(campaign_startdate <= as.Date("2024-10-01")))$campaign_name)

ind_pca_recall_cov_0_59m_district <- pca_summary_district %>%
  select(region, province, district, rcode, pcode, dcode, campaigns, total_children_0_59m, total_vaccinated_recall_0_59m, recall_coverage_0_59m) %>%
  rename(campaign_name = campaigns,
         numerator = total_vaccinated_recall_0_59m,
         denominator = total_children_0_59m,
         value = recall_coverage_0_59m) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "recall_coverage_0_59m",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) %>%
  filter(campaign_name %in% (df_campaigns %>%
                               select(campaign_name, campaign_startdate) %>%
                               unique() %>%
                               filter(campaign_startdate <= as.Date("2024-10-01")))$campaign_name)

ind_pca_recall_cov_0_59m_province <- pca_summary_province %>%
  select(region, province, rcode, pcode, campaigns, total_children_0_59m, total_vaccinated_recall_0_59m, recall_coverage_0_59m) %>%
  rename(campaign_name = campaigns,
         numerator = total_vaccinated_recall_0_59m,
         denominator = total_children_0_59m,
         value = recall_coverage_0_59m) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "recall_coverage_0_59m",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator, value, label) %>%
  filter(campaign_name %in% (df_campaigns %>%
                               select(campaign_name, campaign_startdate) %>%
                               unique() %>%
                               filter(campaign_startdate <= as.Date("2024-10-01")))$campaign_name)

ind_pca_recall_cov_0_59m_region <- pca_summary_region %>%
  select(region, rcode, campaigns, total_children_0_59m, total_vaccinated_recall_0_59m, recall_coverage_0_59m) %>%
  rename(campaign_name = campaigns,
         numerator = total_vaccinated_recall_0_59m,
         denominator = total_children_0_59m,
         value = recall_coverage_0_59m) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "recall_coverage_0_59m",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator, value, label) %>%
  filter(campaign_name %in% (df_campaigns %>%
                               select(campaign_name, campaign_startdate) %>%
                               unique() %>%
                               filter(campaign_startdate <= as.Date("2024-10-01")))$campaign_name)

ind_pca_recall_cov_0_59m_national <- pca_summary_national %>%
  select(campaigns, total_children_0_59m, total_vaccinated_recall_0_59m, recall_coverage_0_59m) %>%
  rename(campaign_name = campaigns,
         numerator = total_vaccinated_recall_0_59m,
         denominator = total_children_0_59m,
         value = recall_coverage_0_59m) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "recall_coverage_0_59m",
         data_source = "pca") %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator, value, label) %>%
  filter(campaign_name %in% (df_campaigns %>%
                               select(campaign_name, campaign_startdate) %>%
                               unique() %>%
                               filter(campaign_startdate <= as.Date("2024-10-01")))$campaign_name)

#Pct of clusters with < 95% Finger-mark Coverage
ind_pca_pct_clusters_lt95_fm_cov_district <- ind_pca_fm_cov_0_59m_cluster %>%
  filter(!is.na(value)) %>%
  mutate(value_lt95 = case_when(value < 0.95 ~ 1,
                                TRUE ~ 0)) %>%
  group_by(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name) %>%
  summarise(numerator = sum(value_lt95, na.rm=T),
            denominator = n()) %>%
  ungroup() %>%
  mutate(value = numerator/denominator,
         label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         indicator = "pct_clusters_lt95_fm_cov")

ind_pca_pct_clusters_lt95_fm_cov_province  <- ind_pca_fm_cov_0_59m_cluster %>%
  filter(!is.na(value)) %>%
  mutate(value_lt95 = case_when(value < 0.95 ~ 1,
                                TRUE ~ 0)) %>%
  group_by(data_source, indicator, category, region, province, rcode, pcode, campaign_name) %>%
  summarise(numerator = sum(value_lt95, na.rm=T),
            denominator = n()) %>%
  ungroup() %>%
  mutate(value = numerator/denominator,
         label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         indicator = "pct_clusters_lt95_fm_cov") 

ind_pca_pct_clusters_lt95_fm_cov_region  <- ind_pca_fm_cov_0_59m_cluster %>%
  filter(!is.na(value)) %>%
  mutate(value_lt95 = case_when(value < 0.95 ~ 1,
                                TRUE ~ 0)) %>%
  group_by(data_source, indicator, category, region, rcode, campaign_name) %>%
  summarise(numerator = sum(value_lt95, na.rm=T),
            denominator = n()) %>%
  ungroup() %>%
  mutate(value = numerator/denominator,
         label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         indicator = "pct_clusters_lt95_fm_cov")

ind_pca_pct_clusters_lt95_fm_cov_national  <- ind_pca_fm_cov_0_59m_cluster %>%
  filter(!is.na(value)) %>%
  mutate(value_lt95 = case_when(value < 0.95 ~ 1,
                                TRUE ~ 0)) %>%
  group_by(data_source, indicator, category, campaign_name) %>%
  summarise(numerator = sum(value_lt95, na.rm=T),
            denominator = n()) %>%
  ungroup() %>%
  mutate(value = numerator/denominator,
         label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         indicator = "pct_clusters_lt95_fm_cov")

#PCA Reasons Missed - district
ind_pca_reasons_missed_cluster <- pca_reasons_missed_by_campaign_and_cluster %>%
  select(region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, pca_reason, count) %>%
  group_by(region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name) %>%
  mutate(
    total = sum(count, na.rm = TRUE),
    value = count / total,
    label = ifelse(!is.na(value), paste0(round(value, 2) * 100, "% (", scales::comma(count, accuracy=1), "/", scales::comma(total, accuracy=1), ")"), NA_character_),
    category = pca_reason,
    indicator = "reasons_missed",
    data_source = "pca"
  ) %>%
  ungroup() %>%
  rename(numerator = count, denominator = total) %>%
  select(
    data_source, indicator, category, region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, 
    numerator, denominator, value, label
  )

ind_pca_reasons_missed_rates_cluster <- pca_reasons_missed_by_campaign_and_cluster %>%
  select(region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, pca_reason, count) %>%
  distinct() %>%
  ungroup() %>%
  left_join(ind_pca_fm_cov_0_59m_cluster %>%
            ungroup() %>%
            select(campaign_name, region, province, district, clustername, rcode, pcode, dcode, ccode, denominator) %>%
            distinct(),
            by=c("campaign_name", "region", "province", "district", "clustername", "rcode", "pcode", "dcode", "ccode")) %>%
  mutate(
    value = count / denominator,
    label = ifelse(!is.na(value), paste0(round(value, 3) * 1000, " per 1000 seen"), NA_character_),
    category = pca_reason,
    indicator = "reasons_missed_rates",
    data_source = "pca"
  ) %>%
  rename(numerator = count) %>%
  select(
    data_source, indicator, category, region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, 
    numerator, denominator, value, label
  )

ind_pca_reasons_missed_district <- pca_reasons_missed_by_campaign_and_district %>%
  select(region, province, district, rcode, pcode, dcode, campaign_name, pca_reason, count) %>%
  group_by(region, province, district, rcode, pcode, dcode, campaign_name) %>%
  mutate(
    total = sum(count, na.rm = TRUE),
    value = count / total,
    label = ifelse(!is.na(value), paste0(round(value, 2) * 100, "% (", scales::comma(count, accuracy=1), "/", scales::comma(total, accuracy=1), ")"), NA_character_),
    category = pca_reason,
    indicator = "reasons_missed",
    data_source = "pca"
  ) %>%
  ungroup() %>%
  rename(numerator = count, denominator = total) %>%
  select(
    data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, 
    numerator, denominator, value, label
  )

ind_pca_reasons_missed_rates_district <- pca_reasons_missed_by_campaign_and_district %>%
  select(region, province, district, rcode, pcode, dcode, campaign_name, pca_reason, count) %>%
  distinct() %>%
  ungroup() %>%
  left_join(ind_pca_fm_cov_0_59m_district %>%
              ungroup() %>%
              select(campaign_name, region, province, district, rcode, pcode, dcode, denominator) %>%
              distinct(),
            by=c("campaign_name", "region", "province", "district", "rcode", "pcode", "dcode")) %>%
  mutate(
    value = count / denominator,
    label = ifelse(!is.na(value), paste0(round(value, 3) * 1000, " per 1000 screened"), NA_character_),
    category = pca_reason,
    indicator = "reasons_missed_rates",
    data_source = "pca"
  ) %>%
  rename(numerator = count) %>%
  select(
    data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, 
    numerator, denominator, value, label
  )

ind_pca_reasons_missed_province <- ind_pca_reasons_missed_district %>%
  group_by(indicator, data_source, category, campaign_name, region, province, rcode, pcode) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator, value, label) 

ind_pca_reasons_missed_rates_province <- ind_pca_reasons_missed_district %>%
  group_by(indicator, data_source, category, campaign_name, region, province, rcode, pcode) %>%
  summarise(numerator = sum(numerator, na.rm=T)) %>%
  ungroup() %>%
  left_join(ind_pca_fm_cov_0_59m_province %>%
              ungroup() %>%
              select(campaign_name, region, province, rcode, pcode, denominator) %>%
              distinct(),
            by=c("campaign_name", "region", "province", "rcode", "pcode")) %>%
  mutate(
    value = numerator / denominator,
    label = ifelse(!is.na(value), paste0(round(value, 3) * 1000, " per 1000 screened"), NA_character_),
    indicator = "reasons_missed_rates",
    data_source = "pca"
  ) %>%
  select(
    data_source, indicator, category, region, province, rcode, pcode, campaign_name, 
    numerator, denominator, value, label
  )

ind_pca_reasons_missed_region <- ind_pca_reasons_missed_district %>%
  group_by(indicator, data_source, category, campaign_name, region, rcode) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator, value, label) 

ind_pca_reasons_missed_rates_region <- ind_pca_reasons_missed_district %>%
  group_by(indicator, data_source, category, campaign_name, region, rcode) %>%
  summarise(numerator = sum(numerator, na.rm=T)) %>%
  ungroup() %>%
  left_join(ind_pca_fm_cov_0_59m_region %>%
              ungroup() %>%
              select(campaign_name, region, rcode, denominator) %>%
              distinct(),
            by=c("campaign_name", "region", "rcode")) %>%
  mutate(
    value = numerator / denominator,
    label = ifelse(!is.na(value), paste0(round(value, 3) * 1000, " per 1000 screened"), NA_character_),
    indicator = "reasons_missed_rates",
    data_source = "pca"
  ) %>%
  select(
    data_source, indicator, category, region, rcode, campaign_name, 
    numerator, denominator, value, label
  )

ind_pca_reasons_missed_national <- ind_pca_reasons_missed_district %>%
  group_by(indicator, data_source, category, campaign_name) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator, value, label) 

ind_pca_reasons_missed_rates_national <- ind_pca_reasons_missed_district %>%
  group_by(indicator, data_source, category, campaign_name) %>%
  summarise(numerator = sum(numerator, na.rm=T)) %>%
  ungroup() %>%
  left_join(ind_pca_fm_cov_0_59m_national %>%
              ungroup() %>%
              select(campaign_name, denominator) %>%
              distinct(),
            by=c("campaign_name")) %>%
  mutate(
    value = numerator / denominator,
    label = ifelse(!is.na(value), paste0(round(value, 3) * 1000, " per 1000 screened"), NA_character_),
    indicator = "reasons_missed_rates",
    data_source = "pca"
  ) %>%
  select(
    data_source, indicator, category, campaign_name, 
    numerator, denominator, value, label
  )

#PCA Door Marking
ind_pca_door_marking_cluster <- pca_door_markings_by_campaign_and_cluster %>%
  ungroup() %>%
  select(region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, pca_reason, count) %>%
  group_by(region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name) %>%
  mutate(total = sum(count, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = count / total) %>%
  ungroup() %>%
  rename(numerator = count,
         denominator = total) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = pca_reason,
         indicator = "door_marking",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, numerator, denominator, value, label) 

ind_pca_door_marking_district <- pca_door_markings_by_campaign_and_district %>%
  ungroup() %>%
  select(region, province, district, rcode, pcode, dcode, campaign_name, pca_reason, count) %>%
  group_by(region, province, district, rcode, pcode, dcode, campaign_name) %>%
  mutate(total = sum(count, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = count / total) %>%
  ungroup() %>%
  rename(numerator = count,
         denominator = total) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = pca_reason,
         indicator = "door_marking",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_pca_door_marking_province <- ind_pca_door_marking_district %>%
  group_by(indicator, data_source, category, campaign_name, region, province, rcode, pcode) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator, value, label) 

ind_pca_door_marking_region <- ind_pca_door_marking_district %>%
  group_by(indicator, data_source, category, campaign_name, region, rcode) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator, value, label) 

ind_pca_door_marking_national <- ind_pca_door_marking_district %>%
  group_by(indicator, data_source, category, campaign_name) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator, value, label) 

#PCA Awareness YN
ind_pca_awareness_yn_cluster <- pca_campaign_awareness_yn %>%
  ungroup() %>%
  select(region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, aware, count) %>%
  group_by(region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name) %>%
  mutate(total = sum(count, na.rm=T)) %>%
  ungroup() %>%
  group_by(region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, aware) %>%
  summarise(count = sum(count, na.rm=T),
            total = max(total, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = count / total) %>%
  ungroup() %>%
  rename(numerator = count,
         denominator = total) %>%
  filter(aware =="heard_yes") %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "awareness_yn",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, numerator, denominator, value, label) 

ind_pca_awareness_yn_district <- pca_campaign_awareness_yn %>%
  ungroup() %>%
  select(region, province, district, rcode, pcode, dcode, campaign_name, aware, count) %>%
  group_by(region, province, district, rcode, pcode, dcode, campaign_name) %>%
  mutate(total = sum(count, na.rm=T)) %>%
  ungroup() %>%
  group_by(region, province, district, rcode, pcode, dcode, campaign_name, aware) %>%
  summarise(count = sum(count, na.rm=T),
            total = max(total, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = count / total) %>%
  ungroup() %>%
  rename(numerator = count,
         denominator = total) %>%
  filter(aware =="heard_yes") %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "awareness_yn",
         data_source = "pca") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_pca_awareness_yn_province <- ind_pca_awareness_yn_district %>%
  group_by(indicator, data_source, category, campaign_name, region, province, rcode, pcode) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator, value, label) 

ind_pca_awareness_yn_region <- ind_pca_awareness_yn_district %>%
  group_by(indicator, data_source, category, campaign_name, region, rcode) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator, value, label) 

ind_pca_awareness_yn_national <- ind_pca_awareness_yn_district %>%
  group_by(indicator, data_source, category, campaign_name) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator, value, label) 

#PCA Awareness Source
ind_pca_awarenss_source_cluster <- pca_campaign_awareness %>%
  select(region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, aware_type, count) %>%
  group_by(region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name) %>%
  mutate(total = sum(count, na.rm = TRUE)) %>%
  group_by(region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name, aware_type) %>%
  summarise(
    count = sum(count, na.rm = TRUE),
    total = max(total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    value = count / total,
    label = ifelse(!is.na(value), paste0(round(value, 2) * 100, "% (", scales::comma(count, accuracy=1), "/", scales::comma(total, accuracy=1), ")"), NA_character_),
    category = aware_type,
    indicator = "awareness_source",
    data_source = "pca"
  ) %>%
  rename(numerator = count, denominator = total) %>%
  select(
    data_source, indicator, category, region, province, district, clustername, rcode, pcode, dcode, ccode, campaign_name,
    numerator, denominator, value, label
  )

ind_pca_awarenss_source_district <- pca_campaign_awareness %>%
  select(region, province, district, rcode, pcode, dcode, campaign_name, aware_type, count) %>%
  group_by(region, province, district, rcode, pcode, dcode, campaign_name) %>%
  mutate(total = sum(count, na.rm = TRUE)) %>%
  group_by(region, province, district, rcode, pcode, dcode, campaign_name, aware_type) %>%
  summarise(
    count = sum(count, na.rm = TRUE),
    total = max(total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    value = count / total,
    label = ifelse(!is.na(value), paste0(round(value, 2) * 100, "% (", scales::comma(count, accuracy=1), "/", scales::comma(total, accuracy=1), ")"), NA_character_),
    category = aware_type,
    indicator = "awareness_source",
    data_source = "pca"
  ) %>%
  rename(numerator = count, denominator = total) %>%
  select(
    data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name,
    numerator, denominator, value, label
  )

ind_pca_awarenss_source_province <- ind_pca_awarenss_source_district %>%
  group_by(indicator, data_source, category, campaign_name, region, province, rcode, pcode) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator, value, label) 

ind_pca_awarenss_source_region <- ind_pca_awarenss_source_district %>%
  group_by(indicator, data_source, category, campaign_name, region, rcode) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator, value, label) 

ind_pca_awarenss_source_national <- ind_pca_awarenss_source_district %>%
  group_by(indicator, data_source, category, campaign_name) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator, value, label) 

ind_pca_completeness_cluster <- pca_completeness_cluster %>%
  mutate(data_source = "pca",
         indicator = "completeness",
         category = NA_character_,
         label = paste0(round(pct_reported,2)*100,"% (", scales::comma(reported, accuracy=1), "/", scales::comma(total, accuracy=1),")")) %>%
  rename(campaign_name = campaigns,
         numerator = reported,
         denominator = total,
         value = pct_reported) %>%
  select(data_source, indicator, category, region, province, district, clustername, 
         rcode, pcode, dcode, ccode, 
         campaign_name,
         numerator, denominator, value, label)

ind_pca_completeness_district <- pca_completeness_district %>%
  mutate(data_source = "pca",
         indicator = "completeness",
         category = NA_character_,
         label = paste0(round(pct_reported,2)*100,"% (", scales::comma(reported, accuracy=1), "/", scales::comma(total, accuracy=1),")")) %>%
  rename(campaign_name = campaigns,
         numerator = reported,
         denominator = total,
         value = pct_reported) %>%
  select(data_source, indicator, category, region, province, district, 
         rcode, pcode, dcode, 
         campaign_name,
         numerator, denominator, value, label)

ind_pca_completeness_province <- pca_completeness_province %>%
  mutate(data_source = "pca",
         indicator = "completeness",
         category = NA_character_,
         label = paste0(round(pct_reported,2)*100,"% (", scales::comma(reported, accuracy=1), "/", scales::comma(total, accuracy=1),")")) %>%
  rename(campaign_name = campaigns,
         numerator = reported,
         denominator = total,
         value = pct_reported) %>%
  select(data_source, indicator, category, region, province, 
         rcode, pcode, 
         campaign_name,
         numerator, denominator, value, label)

ind_pca_completeness_region <- pca_completeness_region %>%
  mutate(data_source = "pca",
         indicator = "completeness",
         category = NA_character_,
         label = paste0(round(pct_reported,2)*100,"% (", scales::comma(reported, accuracy=1), "/", scales::comma(total, accuracy=1),")")) %>%
  rename(campaign_name = campaigns,
         numerator = reported,
         denominator = total,
         value = pct_reported) %>%
  select(data_source, indicator, category, region, 
         rcode, 
         campaign_name,
         numerator, denominator, value, label)

ind_pca_completeness_national <- pca_completeness_national %>%
  mutate(data_source = "pca",
         indicator = "completeness",
         category = NA_character_,
         label = paste0(round(pct_reported,2)*100,"% (", scales::comma(reported, accuracy=1), "/", scales::comma(total, accuracy=1),")")) %>%
  rename(campaign_name = campaigns,
         numerator = reported,
         denominator = total,
         value = pct_reported) %>%
  select(data_source, indicator, category, 
         campaign_name,
         numerator, denominator, value, label)



ind_pca_hrmp_cov_cluster <- pca_summary_cluster_hrmp %>%
  mutate(data_source = "pca",
         indicator = "fm_coverage_hrmp_0_59m",
         category = NA_character_,
         label = paste0(round(hrmp_coverage_calc,2)*100,"% (", scales::comma(total_hrmpfm, accuracy=1), "/", scales::comma(hrm_pchildren_seen, accuracy=1),")")) %>%
  rename(campaign_name = campaigns,
         numerator = total_hrmpfm,
         denominator = hrm_pchildren_seen,
         value = hrmp_coverage_calc) %>%
  select(data_source, indicator, category, region, province, district, clustername, 
         rcode, pcode, dcode, ccode, 
         campaign_name,
         numerator, denominator, value, label) %>%
  filter(denominator != 0)

ind_pca_hrmp_cov_district <- pca_summary_district_hrmp %>%
  mutate(data_source = "pca",
         indicator = "fm_coverage_hrmp_0_59m",
         category = NA_character_,
         label = paste0(round(hrmp_coverage_calc,2)*100,"% (", scales::comma(total_hrmpfm, accuracy=1), "/", scales::comma(hrm_pchildren_seen, accuracy=1),")")) %>%
  rename(campaign_name = campaigns,
         numerator = total_hrmpfm,
         denominator = hrm_pchildren_seen,
         value = hrmp_coverage_calc) %>%
  select(data_source, indicator, category, region, province, district, 
         rcode, pcode, dcode, 
         campaign_name,
         numerator, denominator, value, label) %>%
  filter(denominator != 0)

ind_pca_hrmp_cov_province <- pca_summary_province_hrmp %>%
  mutate(data_source = "pca",
         indicator = "fm_coverage_hrmp_0_59m",
         category = NA_character_,
         label = paste0(round(hrmp_coverage_calc,2)*100,"% (", scales::comma(total_hrmpfm, accuracy=1), "/", scales::comma(hrm_pchildren_seen, accuracy=1),")")) %>%
  rename(campaign_name = campaigns,
         numerator = total_hrmpfm,
         denominator = hrm_pchildren_seen,
         value = hrmp_coverage_calc) %>%
  select(data_source, indicator, category, region, province, 
         rcode, pcode, 
         campaign_name,
         numerator, denominator, value, label) %>%
  filter(denominator != 0)


ind_pca_hrmp_cov_region <- pca_summary_region_hrmp %>%
  mutate(data_source = "pca",
         indicator = "fm_coverage_hrmp_0_59m",
         category = NA_character_,
         label = paste0(round(hrmp_coverage_calc,2)*100,"% (", scales::comma(total_hrmpfm, accuracy=1), "/", scales::comma(hrm_pchildren_seen, accuracy=1),")")) %>%
  rename(campaign_name = campaigns,
         numerator = total_hrmpfm,
         denominator = hrm_pchildren_seen,
         value = hrmp_coverage_calc) %>%
  select(data_source, indicator, category, region, 
         rcode, 
         campaign_name,
         numerator, denominator, value, label) %>%
  filter(denominator != 0)


ind_pca_hrmp_cov_national <- pca_summary_national_hrmp %>%
  mutate(data_source = "pca",
         indicator = "fm_coverage_hrmp_0_59m",
         category = NA_character_,
         label = paste0(round(hrmp_coverage_calc,2)*100,"% (", scales::comma(total_hrmpfm, accuracy=1), "/", scales::comma(hrm_pchildren_seen, accuracy=1),")")) %>%
  rename(campaign_name = campaigns,
         numerator = total_hrmpfm,
         denominator = hrm_pchildren_seen,
         value = hrmp_coverage_calc) %>%
  select(data_source, indicator, category,  
         campaign_name,
         numerator, denominator, value, label) %>%
  filter(denominator != 0)


ind_pca_hrmp_pct_houses_cluster1 <- pca_summary_cluster_hrmp %>%
  mutate(data_source = "pca",
         indicator = "hrmp_pct_of_houses",
         category = NA_character_,
         label = paste0(round(hrmp_pct_of_houses,2)*100,"% (", scales::comma(total_hrmp_houses, accuracy=1), "/", scales::comma(visited_houses, accuracy=1),")")) %>%
  rename(campaign_name = campaigns,
         numerator = total_hrmp_houses,
         denominator = visited_houses,
         value = hrmp_pct_of_houses) %>%
  select(data_source, indicator, category, region, province, district, clustername, 
         rcode, pcode, dcode, ccode, 
         campaign_name,
         numerator, denominator, value, label) %>%
  filter(denominator != 0)

ind_pca_hrmp_pct_houses_cluster2 <- pca_summary_cluster_hrmp %>%
  mutate(data_source = "pca",
         indicator = "hrmp_pct_of_houses_nomad",
         category = NA_character_,
         label = paste0(round(hrmp_pct_of_houses_nomad,2)*100,"% (", scales::comma(nomad, accuracy=1), "/", scales::comma(visited_houses, accuracy=1),")")) %>%
  rename(campaign_name = campaigns,
         numerator = nomad,
         denominator = visited_houses,
         value = hrmp_pct_of_houses_nomad) %>%
  select(data_source, indicator, category, region, province, district, clustername, 
         rcode, pcode, dcode, ccode, 
         campaign_name,
         numerator, denominator, value, label) %>%
  filter(denominator != 0)

ind_pca_hrmp_pct_houses_cluster3 <- pca_summary_cluster_hrmp %>%
  mutate(data_source = "pca",
         indicator = "hrmp_pct_of_houses_returnees",
         category = NA_character_,
         label = paste0(round(hrmp_pct_of_houses_returnees,2)*100,"% (", scales::comma(returnees, accuracy=1), "/", scales::comma(visited_houses, accuracy=1),")")) %>%
  rename(campaign_name = campaigns,
         numerator = returnees,
         denominator = visited_houses,
         value = hrmp_pct_of_houses_returnees) %>%
  select(data_source, indicator, category, region, province, district, clustername, 
         rcode, pcode, dcode, ccode, 
         campaign_name,
         numerator, denominator, value, label) %>%
  filter(denominator != 0)

ind_pca_hrmp_pct_houses_cluster4 <- pca_summary_cluster_hrmp %>%
  mutate(data_source = "pca",
         indicator = "hrmp_pct_of_houses_idp",
         category = NA_character_,
         label = paste0(round(hrmp_pct_of_houses_idp,2)*100,"% (", scales::comma(idp, accuracy=1), "/", scales::comma(visited_houses, accuracy=1),")")) %>%
  rename(campaign_name = campaigns,
         numerator = idp,
         denominator = visited_houses,
         value = hrmp_pct_of_houses_idp) %>%
  select(data_source, indicator, category, region, province, district, clustername, 
         rcode, pcode, dcode, ccode, 
         campaign_name,
         numerator, denominator, value, label) %>%
  filter(denominator != 0)

ind_pca_hrmp_pct_houses_cluster5 <- pca_summary_cluster_hrmp %>%
  mutate(data_source = "pca",
         indicator = "hrmp_pct_of_houses_straddling",
         category = NA_character_,
         label = paste0(round(hrmp_pct_of_houses_straddling,2)*100,"% (", scales::comma(straddling, accuracy=1), "/", scales::comma(visited_houses, accuracy=1),")")) %>%
  rename(campaign_name = campaigns,
         numerator = straddling,
         denominator = visited_houses,
         value = hrmp_pct_of_houses_straddling) %>%
  select(data_source, indicator, category, region, province, district, clustername, 
         rcode, pcode, dcode, ccode, 
         campaign_name,
         numerator, denominator, value, label) %>%
  filter(denominator != 0)

ind_pca_hrmp_pct_houses_cluster <- ind_pca_hrmp_pct_houses_cluster1 %>%
  bind_rows(ind_pca_hrmp_pct_houses_cluster2) %>%
  bind_rows(ind_pca_hrmp_pct_houses_cluster3) %>%
  bind_rows(ind_pca_hrmp_pct_houses_cluster4) %>%
  bind_rows(ind_pca_hrmp_pct_houses_cluster5)

ind_pca_hrmp_pct_houses_district <- ind_pca_hrmp_pct_houses_cluster %>%
  select(data_source, indicator, category, region, province, district, 
         rcode, pcode, dcode, 
         campaign_name,
         numerator, denominator) %>%
  group_by(data_source, indicator, category, region, province, district, 
           rcode, pcode, dcode, 
           campaign_name) %>%
  summarise_all(~sum(as.numeric(.), na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = ifelse(denominator > 0 & !is.na(numerator), numerator / denominator, NA_real_),
         label = paste0(round(value,2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1),")")) %>%
  filter(denominator != 0 & !is.na(value))

ind_pca_hrmp_pct_houses_province <- ind_pca_hrmp_pct_houses_cluster %>%
  select(data_source, indicator, category, region, province, 
         rcode, pcode, 
         campaign_name,
         numerator, denominator) %>%
  group_by(data_source, indicator, category, region, province, 
           rcode, pcode, 
           campaign_name) %>%
  summarise_all(~sum(as.numeric(.), na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = ifelse(denominator > 0 & !is.na(numerator), numerator / denominator, NA_real_),
         label = paste0(round(value,2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1),")")) %>%
  filter(denominator != 0 & !is.na(value))

ind_pca_hrmp_pct_houses_region <- ind_pca_hrmp_pct_houses_cluster %>%
  select(data_source, indicator, category, region, 
         rcode, 
         campaign_name,
         numerator, denominator) %>%
  group_by(data_source, indicator, category, region, 
           rcode, 
           campaign_name) %>%
  summarise_all(~sum(as.numeric(.), na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = ifelse(denominator > 0 & !is.na(numerator), numerator / denominator, NA_real_),
         label = paste0(round(value,2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1),")")) %>%
  filter(denominator != 0 & !is.na(value))

ind_pca_hrmp_pct_houses_national <- ind_pca_hrmp_pct_houses_cluster %>%
  select(data_source, indicator, category, 
         campaign_name,
         numerator, denominator) %>%
  group_by(data_source, indicator, category, 
           campaign_name) %>%
  summarise_all(~sum(as.numeric(.), na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = ifelse(denominator > 0 & !is.na(numerator), numerator / denominator, NA_real_),
         label = paste0(round(value,2)*100,"% (", scales::comma(numerator, accuracy=1), "/", scales::comma(denominator, accuracy=1),")")) %>%
  filter(denominator != 0 & !is.na(value))

