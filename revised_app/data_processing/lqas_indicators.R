lqas_lot_level <- summarize_lqas_data_to_lot(input_data = df_apmis_list$`LQAS - Cluster Data Collection Form`,
                                             grouping_variables = c("campaign_uuid", "region", "rcode", "province", "pcode", "district", "dcode", "lot_no"),
                                             lot_pass_n_missed_threshold = 3,
                                             lot_clusters_complete = 6) |>
  mutate(campaign_uuid = as.character(campaign_uuid)) |>
  left_join(df_campaigns, by = c('campaign_uuid'), relationship = "many-to-many")

lqas_lot_level2 <- summarize_lqas_data_to_lot(input_data = df_apmis_list$`fIPV LQAS Cluster Form` %>%
                                                rename(fmh1 = fmopvh1,
                                                       fmh2 = fmopvh2,
                                                       fmh3 = fmopvh3,
                                                       fmh4 = fmopvh4,
                                                       fmh5 = fmopvh5,
                                                       fmh6 = fmopvh6,
                                                       fmh7 = fmopvh7,
                                                       fmh8 = fmopvh8,
                                                       fmh9 = fmopvh9,
                                                       fmh10 = fmopvh10),
                                             grouping_variables = c("campaign_uuid", "region", "rcode", "province", "pcode", "district", "dcode", "lot_no"),
                                             lot_pass_n_missed_threshold = 3,
                                             lot_clusters_complete = 6) |>
  mutate(campaign_uuid = as.character(campaign_uuid)) |>
  left_join(df_campaigns, by = c('campaign_uuid'), relationship = "many-to-many")

lqas_lot_level3 <- summarize_lqas_data_to_lot(input_data = df_apmis_list$`fIPV LQAS Cluster Form` %>%
                                                rename(fmh1 = f_mf_ipvh1,
                                                       fmh2 = f_mf_ipvh2,
                                                       fmh3 = f_mf_ipvh3,
                                                       fmh4 = f_mf_ipvh4,
                                                       fmh5 = f_mf_ipvh5,
                                                       fmh6 = f_mf_ipvh6,
                                                       fmh7 = f_mf_ipvh7,
                                                       fmh8 = f_mf_ipvh8,
                                                       fmh9 = f_mf_ipvh9,
                                                       fmh10 = f_mf_ipvh10),
                                              grouping_variables = c("campaign_uuid", "region", "rcode", "province", "pcode", "district", "dcode", "lot_no"),
                                              lot_pass_n_missed_threshold = 3,
                                              lot_clusters_complete = 6) |>
  mutate(campaign_uuid = as.character(campaign_uuid)) |>
  left_join(df_campaigns, by = c('campaign_uuid'), relationship = "many-to-many") %>%
  select(campaign_uuid, region, rcode, province, pcode, district, dcode, lot_no, lot_pass_fail, total, fm, not_fm, fm_coverage) %>%
  rename(fipv_lot_pass_fail = lot_pass_fail,
         fipv_total = total,
         fipv_fm = fm,
         fipv_not_fm = not_fm,
         fipv_fm_coverage = fm_coverage)

lqas_lot_level2 <- lqas_lot_level2 %>% full_join(lqas_lot_level3, by=c("campaign_uuid", "region", "rcode", "province", "pcode", "district", "dcode", "lot_no"))

lqas_lot_level <- lqas_lot_level %>%
  mutate_at(c("rcode", "pcode", "dcode", "lot_no"), ~as.numeric(.)) %>%
  bind_rows(lqas_lot_level2 %>%
              mutate_at(c("rcode", "pcode", "dcode", "lot_no"), ~as.numeric(.))) 

lqas_district_level <- lqas_lot_level %>%
  group_by(campaign_uuid, region, rcode, province, pcode, district, dcode) %>%
  summarise(n_lots_pass = sum(lot_pass_fail == "Pass"),
            n_lots_fail = sum(lot_pass_fail == "Fail"),
            n_lots_incomplete = sum(lot_pass_fail == "Incomplete Data"),
            n_lots_pass_fipv = sum(fipv_lot_pass_fail == "Pass"),
            n_lots_fail_fipv = sum(fipv_lot_pass_fail == "Fail"),
            n_lots_incomplete_fipv = sum(fipv_lot_pass_fail == "Incomplete Data")) %>%
  ungroup() %>%
  rowwise() %>%
  mutate_at(c("n_lots_pass", "n_lots_fail", "n_lots_incomplete"), ~ifelse(is.na(.), 0, .)) %>%
  mutate(pct_lots_pass = ifelse(sum(n_lots_pass, n_lots_fail, na.rm=T) > 0, n_lots_pass / sum(n_lots_pass, n_lots_fail, na.rm=T), NA_real_),
         pct_lots_pass_fipv = ifelse(sum(n_lots_pass_fipv, n_lots_fail_fipv, na.rm=T) > 0, n_lots_pass_fipv / sum(n_lots_pass_fipv, n_lots_fail_fipv, na.rm=T), NA_real_)) %>%
  ungroup() %>%
  mutate(campaign_uuid = as.character(campaign_uuid)) |>
  full_join(df_campaigns, by = c('campaign_uuid'), relationship = "many-to-many")

lqas_lot_level_5_10 <- summarize_lqas_data_to_lot(input_data = df_apmis_list$`LQAS - Cluster Data Collection Form (5-10)`,
                                                  grouping_variables = c("campaign_uuid", "region", "rcode", "province", "pcode", "district", "dcode", "lot_no"),
                                                  lot_pass_n_missed_threshold = 3,
                                                  lot_clusters_complete = 6) |>
  mutate(campaign_uuid = as.character(campaign_uuid)) |>
  full_join(df_campaigns, by = c('campaign_uuid'), relationship = "many-to-many")

lqas_district_level_5_10 <- lqas_lot_level_5_10 %>%
  group_by(campaign_uuid, region, rcode, province, pcode, district, dcode) %>%
  summarise(n_lots_pass = sum(lot_pass_fail == "Pass"),
            n_lots_fail = sum(lot_pass_fail == "Fail"),
            n_lots_incomplete = sum(lot_pass_fail == "Incomplete Data")) %>%
  ungroup() %>%
  rowwise() %>%
  mutate_at(c("n_lots_pass", "n_lots_fail", "n_lots_incomplete"), ~ifelse(is.na(.), 0, .)) %>%
  mutate(pct_lots_pass = n_lots_pass / sum(n_lots_pass, n_lots_fail, na.rm=T)) %>%
  ungroup() %>%
  mutate(campaign_uuid = as.character(campaign_uuid)) |>
  full_join(df_campaigns, by = c('campaign_uuid'), relationship = "many-to-many")

# Reasons for missed children (n and pct) by campaign and province
lqas_reasons_missed_by_campaign_and_province <- lqas_lot_level %>%
  group_by(campaign_uuid, region, rcode, province, pcode, district, dcode) %>%
  summarise_at(c("Absent", "NewBorn", "NoTeam", "Other", "Refuse", "Sick", "Sleep", "SiteTooFar", "ExpectedHomeVisit", "NoVaccineAtSite", "ParentsDidNotKnowAboutCampaign", "ParentForgotOrNoTime", "SickSleep", "VeryLongQueue"),
               ~sum(., na.rm=T)) %>%
  pivot_longer(!c(campaign_uuid, region, rcode, province, pcode, district, dcode), names_to = "reason", values_to = "count") %>%
  mutate(campaign_uuid = as.character(campaign_uuid)) |>
  full_join(df_campaigns, by = c('campaign_uuid'), relationship = "many-to-many")


lqas_reasons_missed_by_campaign_and_province_5_10 <- lqas_lot_level_5_10 %>%
  group_by(campaign_uuid, region, rcode, province, pcode, district, dcode) %>%
  summarise_at(c("Absent", "NewBorn", "NoTeam", "Other", "Refuse", "Sick", "Sleep"),
               ~sum(., na.rm=T)) %>%
  pivot_longer(!c(campaign_uuid, region, rcode, province, pcode, district, dcode), names_to = "reason", values_to = "count") %>%
  mutate(campaign_uuid = as.character(campaign_uuid)) |>
  full_join(df_campaigns, by = c('campaign_uuid'), relationship = "many-to-many")

ind_lqas_result_district <- lqas_district_level %>%
  ungroup() %>%
  select(region, province, district, rcode, pcode, dcode, campaign_name, n_lots_pass, n_lots_fail) %>%
  rowwise() %>%
  mutate(denominator = sum(n_lots_pass, n_lots_fail, na.rm=T),
         num_value = n_lots_pass / denominator) %>%
  ungroup() %>%
  mutate(value = case_when(denominator == 0 ~ "Incomplete Data",
                           num_value < 1 ~ "One or More Lots Failed",
                           num_value == 1 ~ "All Lots Passed",
                           TRUE ~ NA_character_)) %>%
  rename(numerator = n_lots_pass) %>%
  mutate(label = ifelse(!is.na(value), paste0(value, " (",numerator, " of ", denominator, " lots passed)" ), NA_integer_),
         category = NA_character_,
         indicator = "result",
         data_source = "lqas") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_lqas_result_province <- lqas_district_level %>%
  ungroup() %>%
  select(region, province, rcode, pcode, campaign_name, n_lots_pass, n_lots_fail) %>%
  group_by(region, province, rcode, pcode, campaign_name) %>%
  summarise(denominator = sum(n_lots_pass, n_lots_fail, na.rm=T),
            numerator = sum(n_lots_pass, na.rm=T)) %>%
  ungroup() %>%
  filter(denominator > 0 & !is.na(denominator)) %>%
  mutate(value = ifelse(is.na(denominator) | denominator == 0, NA, numerator/denominator)) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (",scales::comma(numerator, accuracy=1), " of ", scales::comma(denominator, accuracy=1), " lots passed)" ), NA_integer_),
         category = NA_character_,
         indicator = "result",
         data_source = "lqas") %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator, value, label) 

ind_lqas_result_region <- ind_lqas_result_province %>%
  ungroup() %>%
  select(region, rcode, campaign_name, numerator, denominator) %>%
  group_by(region, rcode, campaign_name) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = ifelse(is.na(denominator) | denominator == 0, NA, numerator/denominator),
         label = ifelse(!is.na(value) & denominator != 0, paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1)," of ",scales::comma(denominator, accuracy=1)," lots passed)"), NA_character_),
         category = NA_character_,
         indicator = "result",
         data_source = "lqas") %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator, value, label) 

ind_lqas_result_national <- ind_lqas_result_region %>%
  ungroup() %>%
  select(campaign_name, numerator, denominator) %>%
  group_by(campaign_name) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = ifelse(is.na(denominator) | denominator == 0, NA, numerator/denominator),
         label = ifelse(!is.na(value) & denominator != 0, paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1)," of ",scales::comma(denominator, accuracy=1)," lots passed)"), NA_character_),
         category = NA_character_,
         indicator = "result",
         data_source = "lqas") %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator, value, label) 

#fIPV result

ind_lqas_result_fipv_district <- lqas_district_level %>%
  ungroup() %>%
  select(region, province, district, rcode, pcode, dcode, campaign_name, n_lots_pass_fipv, n_lots_fail_fipv) %>%
  rowwise() %>%
  mutate(denominator = sum(n_lots_pass_fipv, n_lots_fail_fipv, na.rm=T),
         num_value = n_lots_pass_fipv / denominator) %>%
  ungroup() %>%
  mutate(value = case_when(denominator == 0 ~ "Incomplete Data",
                           num_value < 1 ~ "One or More Lots Failed",
                           num_value == 1 ~ "All Lots Passed",
                           TRUE ~ NA_character_)) %>%
  rename(numerator = n_lots_pass_fipv) %>%
  mutate(label = ifelse(!is.na(value), paste0(value, " (",numerator, " of ", denominator, " lots passed)" ), NA_integer_),
         category = NA_character_,
         indicator = "fipv_result",
         data_source = "lqas") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) %>%
  filter(!is.na(denominator) & denominator != 0)

ind_lqas_result_fipv_province <- lqas_district_level %>%
  ungroup() %>%
  select(region, province, rcode, pcode, campaign_name, n_lots_pass_fipv, n_lots_fail_fipv) %>%
  group_by(region, province, rcode, pcode, campaign_name) %>%
  summarise(denominator = sum(n_lots_pass_fipv, n_lots_fail_fipv, na.rm=T),
            numerator = sum(n_lots_pass_fipv, na.rm=T)) %>%
  ungroup() %>%
  filter(denominator > 0 & !is.na(denominator)) %>%
  mutate(value = ifelse(is.na(denominator) | denominator == 0, NA, numerator/denominator)) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (",scales::comma(numerator, accuracy=1), " of ", scales::comma(denominator, accuracy=1), " lots passed)" ), NA_integer_),
         category = NA_character_,
         indicator = "fipv_result",
         data_source = "lqas") %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator, value, label) %>%
  filter(!is.na(denominator) & denominator != 0)

ind_lqas_result_fipv_region <- ind_lqas_result_fipv_province %>%
  ungroup() %>%
  select(region, rcode, campaign_name, numerator, denominator) %>%
  group_by(region, rcode, campaign_name) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = ifelse(is.na(denominator) | denominator == 0, NA, numerator/denominator),
         label = ifelse(!is.na(value) & denominator != 0, paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1)," of ",scales::comma(denominator, accuracy=1)," lots passed)"), NA_character_),
         category = NA_character_,
         indicator = "fipv_result",
         data_source = "lqas") %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator, value, label) 

ind_lqas_result_fipv_national <- ind_lqas_result_fipv_region %>%
  ungroup() %>%
  select(campaign_name, numerator, denominator) %>%
  group_by(campaign_name) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = ifelse(is.na(denominator) | denominator == 0, NA, numerator/denominator),
         label = ifelse(!is.na(value) & denominator != 0, paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1)," of ",scales::comma(denominator, accuracy=1)," lots passed)"), NA_character_),
         category = NA_character_,
         indicator = "fipv_result",
         data_source = "lqas") %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator, value, label) 


#LQAS Reasons Missed - district
ind_lqas_reasons_missed_district <- lqas_reasons_missed_by_campaign_and_province %>%
  ungroup() %>%
  select(region, province, district, rcode, pcode, dcode, campaign_name, reason, count) %>%
  group_by(region, province, district, rcode, pcode, dcode, campaign_name) %>%
  mutate(total = sum(count, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = count / total) %>%
  ungroup() %>%
  rename(numerator = count,
         denominator = total) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = reason,
         indicator = "reasons_missed",
         data_source = "lqas") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_lqas_reasons_missed_province <- ind_lqas_reasons_missed_district %>%
  group_by(indicator, data_source, category, campaign_name, region, province, rcode, pcode) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator, value, label) 

ind_lqas_reasons_missed_region <- ind_lqas_reasons_missed_district %>%
  group_by(indicator, data_source, category, campaign_name, region, rcode) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator, value, label) 

ind_lqas_reasons_missed_national <- ind_lqas_reasons_missed_district %>%
  group_by(indicator, data_source, category, campaign_name) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator, value, label) 


