export_lqas <- all_data_list$apmis_indicators$district_indicators %>%
  filter(data_source == "lqas" & indicator %in% c("lqas_result", "lqas_fipv_result")) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, indicator,
         numerator, denominator, value) %>%
  rowwise() %>%
  mutate(total_lots_failed  = denominator - numerator) %>%
  ungroup() %>%
  rename(total_lots_complete = denominator,
         total_lots_passed = numerator,
         district_result = value) %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, indicator,
         total_lots_complete, total_lots_passed, total_lots_failed, district_result) %>%
  left_join(all_data_list$apmis_indicators$district_indicators %>%
              filter(data_source == "lqas" & indicator == "lqas_reasons_missed") %>%
              select(campaign_name, rcode, pcode, dcode, region, province, district,
                     category, numerator) %>%
              pivot_wider(names_from = category,
                          values_from = numerator),
            by=c("campaign_name", "rcode", "pcode", "dcode")) %>%
  filter(!is.na(rcode) & !is.na(pcode) & !is.na(dcode)) %>%
  rename(region = region.x,
         province = province.x,
         district = district.x) %>%
  mutate(region = ifelse(is.na(region), region.y, region),
         province = ifelse(is.na(province), province.y, province),
         district = ifelse(is.na(district), district.y, district)) %>%
  select(-c("region.y", "province.y", "district.y")) %>%
  left_join(all_data_list$df_apmis_list$campaign_district_pop %>%
              select(rcode, pcode, dcode, region_name, province_name, district_name) %>%
              unique(),
            by=c("rcode", "pcode", "dcode")) %>%
  mutate(region = ifelse(is.na(region), region_name, region),
         province = ifelse(is.na(province), province_name, province),
         district = ifelse(is.na(district), district_name, district)) %>%
  select(-c("region_name", "province_name", "district_name"))


for(i in c("NoTeam", "Absent", "NewBorn", "Sick", "Sleep", "Refuse", "Other", "SiteTooFar", "ExpectedHomeVisit", "NoVaccineAtSite", "ParentsDidNotKnowAboutCampaign", "ParentForgotOrNoTime", "SickSleep", "VeryLongQueue")){
  if(!(i %in% colnames(export_lqas))){
    export_lqas[i] <- 0
  }
}
export_lqas <- export_lqas %>%
  rowwise() %>%
  mutate(pct_of_lots_passed = ifelse(!is.na(total_lots_complete) & total_lots_complete > 0 & !is.na(total_lots_passed), 
                                     paste0(round(total_lots_passed / total_lots_complete,2)*100,"%"),
                                     NA_character_)) %>%
  ungroup() %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode,
         total_lots_complete, total_lots_passed, total_lots_failed, pct_of_lots_passed,
         NoTeam, Absent, NewBorn, Sick, Sleep, Refuse, SiteTooFar, ExpectedHomeVisit, NoVaccineAtSite, ParentsDidNotKnowAboutCampaign, ParentForgotOrNoTime, SickSleep, VeryLongQueue, Other) %>%
  arrange(campaign_name, region, province, district)   %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, everything()) 

all_data_list$export_lqas <- export_lqas
