ooh_fms_district_level <- df_apmis_list$`Out of house Finger Mark Survey H2H` %>%
  select(campaigns, region, rcode, province, pcode, district, dcode, 
         screened, f_marked, 
         seen059m_male, seen059m_female,
         fm059m_male, fm059m_female,
         team_not_come, child_absent, refusal, newborn_sick_sleep, other) %>%
  rename(absent = child_absent,
         screened_male = seen059m_male,
         screened_female = seen059m_female,
         f_marked_male = fm059m_male,
         f_marked_female = fm059m_female) %>%
  rowwise() %>%
  mutate(f_marked_male = ifelse(is.na(screened_male) | screened_male == 0, NA, f_marked_male),
         f_marked_female = ifelse(is.na(screened_female) | screened_female == 0, NA, f_marked_female),
         f_marked = ifelse(is.na(screened) | screened == 0, NA, f_marked)) %>%
  ungroup() %>%
  bind_rows(df_apmis_list$`Out of house Finger Mark Survey M2M` %>%
              select(campaigns, region, rcode, province, pcode, district, dcode, 
                     screened, f_marked, 
                     seen011m, fm011m, 
                     seen1259m, fm1259m,
                     team_not_come, absent, refusal, not_aware, too_far, no_men, other) %>%
              rename(screened_011m = seen011m,
                     screened_1259m = seen1259m,
                     f_marked_011m = fm011m,
                     f_marked_1259m = fm1259m) %>%
              rowwise() %>%
              mutate(f_marked_011m = ifelse(is.na(screened_011m) | screened_011m == 0, NA, f_marked_011m),
                     f_marked_1259m = ifelse(is.na(screened_1259m) | screened_1259m == 0, NA, f_marked_1259m),
                     f_marked = ifelse(is.na(screened) | screened == 0, NA, f_marked)) %>%
              ungroup()) %>% 
  bind_rows(df_apmis_list$`Out of house Finger Mark Survey S2S` %>%
              select(campaigns, region, rcode, province, pcode, district, dcode, 
                     screened, f_marked, 
                     seen011m, fm011m,
                     seen1259m, fm1259m,
                     team_not_come, absent, refusal, not_aware, too_far, no_men, other) %>%
              rename(screened_011m = seen011m,
                     screened_1259m = seen1259m,
                     f_marked_011m = fm011m,
                     f_marked_1259m = fm1259m) %>%
              rowwise() %>%
              mutate(f_marked_011m = ifelse(is.na(screened_011m) | screened_011m == 0, NA, f_marked_011m),
                     f_marked_1259m = ifelse(is.na(screened_1259m) | screened_1259m == 0, NA, f_marked_1259m),
                     f_marked = ifelse(is.na(screened) | screened == 0, NA, f_marked)) %>%
              ungroup()) %>%
  bind_rows(df_apmis_list$`SIA Finger Mark Survey` %>%
              select(campaigns, region, rcode, province, pcode, district, dcode, 
                     screened, f_marked, 
                     male_seen011m, male_fm011m,
                     female_seen011m, female_fm011m,
                     male_seen1259m, male_fm1259m,
                     female_seen1259m, female_fm1259m,
                     team_not_come, absent, refusal, not_aware, too_far, no_men, other, newborn, sick, sleep) %>%
              mutate_at(c("screened", "f_marked", 
                          "male_seen011m", "male_fm011m",
                          "female_seen011m", "female_fm011m",
                          "male_seen1259m", "male_fm1259m",
                          "female_seen1259m", "female_fm1259m",
                          "team_not_come", "absent", "refusal", "not_aware", "too_far", "no_men", "other", "newborn", "sick", "sleep"), ~as.numeric(.)) %>%
              rowwise() %>%
              mutate(screened_011m = sum(male_seen011m, female_seen011m, na.rm=T),
                     screened_1259m = sum(male_seen1259m, female_seen1259m, na.rm=T),
                     f_marked_011m = sum(male_fm011m, female_fm011m, na.rm=T),
                     f_marked_1259m = sum(male_fm1259m, female_fm1259m, na.rm=T),
                     screened_male = sum(male_seen011m, male_seen1259m, na.rm=T),
                     screened_female = sum(female_seen011m, female_seen1259m, na.rm=T),
                     f_marked_male = sum(male_fm011m, male_fm1259m, na.rm=T),
                     f_marked_female = sum(female_fm011m, female_fm1259m, na.rm=T)) %>%
              ungroup() %>%
              select(-c("male_seen011m", "male_fm011m",
                        "female_seen011m", "female_fm011m",
                        "male_seen1259m", "male_fm1259m",
                        "female_seen1259m", "female_fm1259m")) %>%
              rowwise() %>%
              mutate(f_marked_011m = ifelse(is.na(screened_011m) | screened_011m == 0, NA, f_marked_011m),
                     f_marked_1259m = ifelse(is.na(screened_1259m) | screened_1259m == 0, NA, f_marked_1259m),
                     f_marked_male = ifelse(is.na(screened_male) | screened_male == 0, NA, f_marked_male),
                     f_marked_female = ifelse(is.na(screened_female) | screened_female == 0, NA, f_marked_female),
                     f_marked = ifelse(is.na(screened) | screened == 0, NA, f_marked)) %>%
              ungroup()) %>%
  bind_rows(df_apmis_list$`fIPV Finger Mark Survey` %>%
              select(campaigns, region, rcode, province, pcode, district, dcode, 
                     opv_seen059m, opvfm059m, 
                     ipv_seen459m, ipvfm459m,
                     team_not_come, absent, refusal, other, nss) %>%
              mutate_at(c("opv_seen059m", "opvfm059m", 
                          "ipv_seen459m", "ipvfm459m",
                          "team_not_come", "absent", "refusal", "other", "nss"), ~as.numeric(.)) %>%
              rename(screened = opv_seen059m,
                     f_marked = opvfm059m,
                     ipv_screened = ipv_seen459m,
                     ipv_f_marked = ipvfm459m,
                     newborn_sick_sleep = nss) %>%
              rowwise() %>%
              mutate(f_marked = ifelse(is.na(screened) | screened == 0, NA, f_marked),
                     ipv_f_marked = ifelse(is.na(ipv_screened) | ipv_screened == 0, NA, ipv_f_marked)) %>%
              ungroup()) %>%
  group_by(campaigns, region, rcode, province, pcode, district, dcode) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(ooh_fm_coverage = ifelse(screened > 0, f_marked / screened, NA_real_),
         ooh_fm_coverage_011m = ifelse(screened_011m > 0, f_marked_011m / screened_011m, NA_real_),
         ooh_fm_coverage_1259m = ifelse(screened_1259m > 0, f_marked_1259m / screened_1259m, NA_real_),
         ooh_fm_coverage_male = ifelse(screened_male > 0, f_marked_male / screened_male, NA_real_),
         ooh_fm_coverage_female = ifelse(screened_female > 0, f_marked_female /screened_female, NA_real_),
         ooh_ipv_fm_coverage = ifelse(ipv_screened > 0, ipv_f_marked / ipv_screened, NA_real_)) %>%
  ungroup() 

ooh_fms_completeness_district <- ooh_fms_district_level %>%
  select(campaigns, region, rcode, province, pcode, district, dcode) %>%
  mutate(reported = 1) %>%
  full_join(campaign_rpd %>%
              select(campaign_name, rcode, pcode, dcode) %>%
              distinct() %>%
              rename(campaigns = campaign_name),
            by=c("campaigns", "rcode", "pcode", "dcode")) %>%
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
  select(-c("APMIS_Region", "APMIS_Province", "APMIS_District")) %>%
  left_join(campaign_rpd %>%
              select(rcode, pcode, dcode) %>%   
              group_by(rcode, pcode, dcode) %>%  
              slice(1) %>%             
              ungroup() %>%
              distinct(),
            by=c("rcode", "pcode", "dcode")) %>%
  arrange(campaigns, region, province, district)

ooh_fms_completeness_province <- ooh_fms_completeness_district %>% 
  group_by(campaigns, region, rcode, province, pcode) %>%
  summarise(reported = sum(pct_reported, na.rm=T),
            total = n()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(pct_reported = reported / total) %>%
  ungroup()

ooh_fms_completeness_region <- ooh_fms_completeness_district %>% 
  group_by(campaigns, region, rcode) %>%
  summarise(reported = sum(pct_reported, na.rm=T),
            total = n()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(pct_reported = reported / total) %>%
  ungroup()

ooh_fms_completeness_national <- ooh_fms_completeness_district %>% 
  group_by(campaigns) %>%
  summarise(reported = sum(pct_reported, na.rm=T),
            total = n()) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(pct_reported = reported / total) %>%
  ungroup()

ind_ooh_fm_coverage_district <- ooh_fms_district_level %>%
  select(region, province, district, rcode, pcode, dcode, campaigns, screened, f_marked, ooh_fm_coverage) %>%
  rename(campaign_name = campaigns,
         numerator = f_marked,
         denominator = screened,
         value = ooh_fm_coverage) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage",
         data_source = "ooh") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_ooh_fm_coverage_011m_district <- ooh_fms_district_level %>%
  select(region, province, district, rcode, pcode, dcode, campaigns, screened_011m, f_marked_011m, ooh_fm_coverage_011m) %>%
  rename(campaign_name = campaigns,
         numerator = f_marked_011m,
         denominator = screened_011m,
         value = ooh_fm_coverage_011m) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_011m",
         data_source = "ooh") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_ooh_fm_coverage_1259m_district <- ooh_fms_district_level %>%
  select(region, province, district, rcode, pcode, dcode, campaigns, screened_1259m, f_marked_1259m, ooh_fm_coverage_1259m) %>%
  rename(campaign_name = campaigns,
         numerator = f_marked_1259m,
         denominator = screened_1259m,
         value = ooh_fm_coverage_1259m) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_1259m",
         data_source = "ooh") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_ooh_fm_coverage_male_district <- ooh_fms_district_level %>%
  select(region, province, district, rcode, pcode, dcode, campaigns, screened_male, f_marked_male, ooh_fm_coverage_male) %>%
  rename(campaign_name = campaigns,
         numerator = f_marked_male,
         denominator = screened_male,
         value = ooh_fm_coverage_male) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_male",
         data_source = "ooh") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_ooh_fm_coverage_female_district <- ooh_fms_district_level %>%
  select(region, province, district, rcode, pcode, dcode, campaigns, screened_female, f_marked_female, ooh_fm_coverage_female) %>%
  rename(campaign_name = campaigns,
         numerator = f_marked_female,
         denominator = screened_female,
         value = ooh_fm_coverage_female) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_female",
         data_source = "ooh") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_ooh_fm_coverage_ipv_district <- ooh_fms_district_level %>%
  select(region, province, district, rcode, pcode, dcode, campaigns, ipv_screened, ipv_f_marked, ooh_ipv_fm_coverage) %>%
  rename(campaign_name = campaigns,
         numerator = ipv_f_marked,
         denominator = ipv_screened,
         value = ooh_ipv_fm_coverage) %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = NA_character_,
         indicator = "fm_coverage_ipv",
         data_source = "ooh") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_ooh_fm_coverage_district <- ind_ooh_fm_coverage_district %>%
  bind_rows(ind_ooh_fm_coverage_011m_district) %>%
  bind_rows(ind_ooh_fm_coverage_1259m_district) %>%
  bind_rows(ind_ooh_fm_coverage_male_district) %>%
  bind_rows(ind_ooh_fm_coverage_female_district) %>%
  bind_rows(ind_ooh_fm_coverage_ipv_district %>%
              mutate(label = as.character(label)))

ind_ooh_fm_coverage_province <- ind_ooh_fm_coverage_district %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator) %>%
  group_by(data_source, indicator, category, region, province, rcode, pcode, campaign_name) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(numerator = ifelse(is.na(numerator) & !is.na(denominator), 0, numerator)) %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator, value, label) 

ind_ooh_fm_coverage_region <- ind_ooh_fm_coverage_district %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator) %>%
  group_by(data_source, indicator, category, region, rcode, campaign_name) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(numerator = ifelse(is.na(numerator) & !is.na(denominator), 0, numerator)) %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator, value, label) 

ind_ooh_fm_coverage_national <- ind_ooh_fm_coverage_district %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator) %>%
  group_by(data_source, indicator, category, campaign_name) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(numerator = ifelse(is.na(numerator) & !is.na(denominator), 0, numerator)) %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator, value, label) 

#OOH Reasons Missed - district
ooh_fms_reasons_missed <- ooh_fms_district_level %>%
  select(campaigns, region, province, district, rcode, pcode, dcode, team_not_come, absent, refusal, newborn_sick_sleep, other, not_aware, too_far, no_men) %>%
  pivot_longer(cols=-c("campaigns", "region", "province", "district", "rcode", "pcode", "dcode"),
               names_to = "reason",
               values_to = "numerator")

ind_ooh_reasons_missed_district <-  ooh_fms_reasons_missed %>%
  rename(campaign_name = campaigns) %>%
  ungroup() %>%
  select(region, province, district, rcode, pcode, dcode, campaign_name, reason, numerator) %>%
  group_by(region, province, district, rcode, pcode, dcode, campaign_name) %>%
  mutate(denominator = sum(numerator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_),
         category = reason,
         indicator = "reasons_missed",
         data_source = "ooh") %>%
  select(data_source, indicator, category, region, province, district, rcode, pcode, dcode, campaign_name, numerator, denominator, value, label) 

ind_ooh_reasons_missed_province <- ind_ooh_reasons_missed_district %>%
  group_by(indicator, data_source, category, campaign_name, region, province, rcode, pcode) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, region, province, rcode, pcode, campaign_name, numerator, denominator, value, label) 

ind_ooh_reasons_missed_region <- ind_ooh_reasons_missed_district %>%
  group_by(indicator, data_source, category, campaign_name, region, rcode) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, region, rcode, campaign_name, numerator, denominator, value, label) 

ind_ooh_reasons_missed_national <- ind_ooh_reasons_missed_district %>%
  group_by(indicator, data_source, category, campaign_name) %>%
  summarise(numerator = sum(numerator, na.rm=T),
            denominator = sum(denominator, na.rm=T)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(value = numerator / denominator) %>%
  ungroup() %>%
  mutate(label = ifelse(!is.na(value), paste0(round(value, 2)*100, "% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1),")"), NA_integer_)) %>%
  select(data_source, indicator, category, campaign_name, numerator, denominator, value, label) 

ind_ooh_completeness_district <- ooh_fms_completeness_district %>%
  mutate(data_source = "ooh",
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

ind_ooh_completeness_province <- ooh_fms_completeness_province %>%
  mutate(data_source = "ooh",
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

ind_ooh_completeness_region <- ooh_fms_completeness_region %>%
  mutate(data_source = "ooh",
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

ind_ooh_completeness_national <- ooh_fms_completeness_national %>%
  mutate(data_source = "ooh",
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

