export_ooh <- all_data_list$df_apmis_list$`Out of house Finger Mark Survey S2S` %>%
  mutate(modality = "S2S/M2M") %>%
  rename(s2s_total_seen011m = seen011m, 
         s2s_total_fm011m = fm011m, 
         s2s_total_seen1259m = seen1259m, 
         s2s_total_fm1259m = fm1259m) %>%
  bind_rows(all_data_list$df_apmis_list$`Out of house Finger Mark Survey M2M` %>%
              mutate(modality = "S2S/M2M") %>%
              rename(s2s_total_seen011m = seen011m, 
                     s2s_total_fm011m = fm011m, 
                     s2s_total_seen1259m = seen1259m, 
                     s2s_total_fm1259m = fm1259m)) %>%
  bind_rows(all_data_list$df_apmis_list$`Out of house Finger Mark Survey H2H` %>%
              mutate(modality = "H2H") %>%
              mutate(visit_date_fmt = as.Date(visit_date_fmt))) %>%
  bind_rows(all_data_list$df_apmis_list$`SIA Finger Mark Survey` %>%
              mutate_at(c("male_seen011m", "female_seen011m",
                          "male_seen1259m", "female_seen1259m",
                          "male_fm011m", "female_fm011m",
                          "male_fm1259m", "female_fm1259m",
                          "screened", 
                          "f_marked", 
                          "not_vaccined",
                          "team_not_come", "absent", "refusal", "not_aware", "too_far", "no_men", "other", "newborn", "sick", "sleep"), ~as.numeric(.)) %>%
              rowwise() %>%
              mutate(total_seen011m = sum(male_seen011m, female_seen011m, na.rm=T),
                     total_seen1259m = sum(male_seen1259m, female_seen1259m, na.rm=T),
                     total_fm_011m = sum(male_fm011m, female_fm011m, na.rm=T),
                     total_fm_1259m = sum(male_fm1259m, female_fm1259m, na.rm=T)) %>%
              select(campaigns, region, rcode, province, pcode, district, dcode, 
                     male_seen011m, female_seen011m, total_seen011m,
                     male_fm011m, female_fm011m, total_fm_011m,
                     male_seen1259m, female_seen1259m, total_seen1259m,
                     male_fm1259m, female_fm1259m, total_fm_1259m,
                     screened, 
                     f_marked, 
                     not_vaccined,
                     team_not_come, absent, refusal, not_aware, too_far, no_men, other, newborn, sick, sleep) %>%
              rename(s2s_male_seen011m = male_seen011m,
                     s2s_female_seen011m = female_seen011m,
                     s2s_male_seen1259m = male_seen1259m,
                     s2s_female_seen1259m = female_seen1259m,
                     s2s_total_seen011m = total_seen011m,
                     s2s_total_seen1259m = total_seen1259m,
                     
                     s2s_male_fm011m = male_fm011m,
                     s2s_female_fm011m = female_fm011m,
                     s2s_male_fm1259m = male_fm1259m,
                     s2s_female_fm1259m = female_fm1259m,
                     s2s_total_fm011m = total_fm_011m,
                     s2s_total_fm1259m = total_fm_1259m,
                     child_absent = absent) %>%
              rowwise() %>%
              mutate(newborn_sick_sleep = sum(newborn, sick, sleep, na.rm=T)) %>%
              ungroup() %>%
              select(-c("newborn", "sick", "sleep"))) %>%
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
  select(campaigns, region, rcode, province, pcode, district, dcode,
         seen059m_male, vac059m_male, fm059m_male, seen059m_female, vac059m_female, fm059m_female,
         s2s_male_seen011m, s2s_female_seen011m, s2s_total_seen011m,
         s2s_male_fm011m, s2s_female_fm011m, s2s_total_fm011m,
         s2s_male_seen1259m, s2s_female_seen1259m, s2s_total_seen1259m,
         s2s_male_fm1259m, s2s_female_fm1259m, s2s_total_fm1259m,
         screened, vaccinated, f_marked,
         ipv_screened, ipv_f_marked,
         not_vaccined, team_not_come, child_absent, refusal, newborn_sick_sleep, other, not_aware, too_far, no_men) %>%
  group_by(campaigns, region, rcode, province, pcode, district, dcode) %>%
  summarise_all(~sum(., na.rm=T)) %>%
  ungroup() %>%
  mutate(total_fm_coverage_059m = paste0(round(f_marked    / screened,    3)*100,"%"),
         ipv_fm_coverage =        paste0(round(ipv_f_marked/ipv_screened, 3)*100,"%")) %>%
  select(campaigns, region, rcode, province, pcode, district, dcode,
         seen059m_male, vac059m_male, fm059m_male, seen059m_female, vac059m_female, fm059m_female,
         s2s_male_seen011m, s2s_female_seen011m, s2s_total_seen011m,
         s2s_male_fm011m, s2s_female_fm011m, s2s_total_fm011m,
         s2s_male_seen1259m, s2s_female_seen1259m, s2s_total_seen1259m,
         s2s_male_fm1259m, s2s_female_fm1259m, s2s_total_fm1259m,
         screened, vaccinated, f_marked, total_fm_coverage_059m,
         ipv_screened, ipv_f_marked, ipv_fm_coverage,
         not_vaccined, team_not_come, child_absent, refusal, newborn_sick_sleep, other, not_aware, too_far, no_men) %>%
  rename(campaign_name = campaigns,
         h2h_seen059m_male = seen059m_male, 
         h2h_vac059m_male = vac059m_male,
         h2h_fm059m_male = fm059m_male,
         h2h_seen059m_female = seen059m_female,
         h2h_vac059m_female = vac059m_female,
         h2h_fm059m_female = fm059m_female,
         total_seen_059m = screened,
         total_vac059m = vaccinated,
         total_fm059m = f_marked) %>%
  select(-c("h2h_vac059m_male", "h2h_vac059m_female", "total_vac059m")) %>%
  mutate(across(c("s2s_male_seen011m", "s2s_female_seen011m", "s2s_male_fm011m", "s2s_female_fm011m", "s2s_male_seen1259m", "s2s_female_seen1259m", "s2s_male_fm1259m", "s2s_female_fm1259m"),
                ~ifelse(campaign_name %in% (df_campaigns %>%
                                              select(campaign_name, campaign_startdate) %>%
                                              unique() %>%
                                              filter(campaign_startdate <= as.Date("2024-10-01")))$campaign_name, NA_integer_, .))) %>%
  mutate(across(c("ipv_f_marked", "ipv_screened"),
                ~ifelse(campaign_name %in% (df_campaigns %>%
                                              select(campaign_name) %>%
                                              unique() %>%
                                              filter(grepl("IPV", campaign_name)))$campaign_name, ., NA_integer_)))



original_colnames <- colnames(export_ooh)

# Perform the join and retain the structure
export_ooh <- export_ooh %>%
  left_join(rpd_list, by=c("rcode" = "APMIS_RCODE",
                           "pcode" = "APMIS_PCODE",
                           "dcode" = "APMIS_DCODE")) %>%
  mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
         province = ifelse(!is.na(APMIS_Province), APMIS_Province, province),
         district = ifelse(!is.na(APMIS_District), APMIS_District, district)) %>%
  select(all_of(original_colnames)) %>%
  arrange(campaign_name, region, province, district)  %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, everything()) 

all_data_list$export_ooh <- export_ooh
