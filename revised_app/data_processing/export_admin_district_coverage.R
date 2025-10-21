export_district_admin_coverage <- all_data_list$df_apmis_list$campaign_district_pop %>%
  filter(!(grepl("IPV", campaign_name))) %>%
  filter(ageGroup == "AGE_0_4") %>%
  select(campaign_name, region_name, rcode, province_name, pcode, district_name, dcode, campaign_startdate, district_population) %>%
  rename(region = region_name, 
         province=province_name,
         district=district_name) %>%
  arrange(campaign_startdate) %>%
  select(-c("campaign_startdate")) %>%
  left_join(export_admin_h2h_0_4_gt %>%
              ungroup() %>%
              select(campaign_name, rcode, pcode, dcode, total_children_vaccinated_day1_3gt, total_children_vaccinated_day4gt) %>%
              group_by(campaign_name, rcode, pcode, dcode) %>%
              summarise_all(~sum(., na.rm=T)),
            by=c("campaign_name", "rcode", "pcode", "dcode")) %>%
  rowwise() %>%
  mutate(total_children_vaccinated_h2h = sum(total_children_vaccinated_day1_3gt, total_children_vaccinated_day4gt, na.rm=T)) %>%
  ungroup() %>%
  select(-c("total_children_vaccinated_day1_3gt", "total_children_vaccinated_day4gt")) %>%
  left_join(export_admin_s2s_m2m %>%
              select(-c("clustername", "ccode", "region", "province", "district")) %>%
              ungroup() %>%
              select(campaign_name, rcode, pcode, dcode, total_children_vaccinated_days13) %>%
              group_by(campaign_name, rcode, pcode, dcode) %>%
              summarise_all(~sum(., na.rm=T)),
            by=c("campaign_name", "rcode", "pcode", "dcode")) %>%
  rename(total_children_vaccinated_s2s_m2m = total_children_vaccinated_days13) %>%
  mutate_at(c("district_population", "total_children_vaccinated_h2h",
              "total_children_vaccinated_s2s_m2m"), ~ifelse(is.na(.), 0, .)) %>%
  rowwise() %>%
  mutate(total_children_vaccinated = sum(total_children_vaccinated_h2h, total_children_vaccinated_s2s_m2m, na.rm=T),
         admin_coverage = paste0(round(total_children_vaccinated / district_population,3)*100,"%")) %>%
  ungroup() %>%
  rename(  `Target Population (0-59m)` = district_population,
           `H2H: Total Children Vaccinated (0-59m)` = total_children_vaccinated_h2h,
           `S2S/M2M: Total Children Vaccinated (0-59m)` = total_children_vaccinated_s2s_m2m,
           `Total Children vaccinated (0-59m)` = total_children_vaccinated,
           `Admin Coverage (0-59m) (%)` = admin_coverage
  )  %>%
  arrange(campaign_name, region, province, district)  %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, everything()) 

export_district_admin_coverage_fipv <- all_data_list$df_apmis_list$campaign_district_pop %>%
  filter(ageGroup == "AGE_0_4") %>%
  filter(grepl("IPV", campaign_name)) %>%
  select(campaign_name, region_name, rcode, province_name, pcode, district_name, dcode, campaign_startdate, district_population) %>%
  rename(region = region_name, 
         province=province_name,
         district=district_name) %>%
  arrange(campaign_startdate) %>%
  select(-c("campaign_startdate")) %>%
  left_join(export_admin_ipv_7day %>%
              ungroup() %>%
              select(campaign_name, rcode, pcode, dcode, total_opv_vaccinated, total_ipv_vaccinated) %>%
              group_by(campaign_name, rcode, pcode, dcode) %>%
              summarise_all(~sum(., na.rm=T)),
            by=c("campaign_name", "rcode", "pcode", "dcode")) %>%
  mutate_at(c("district_population", "total_opv_vaccinated",
              "total_ipv_vaccinated"), ~ifelse(is.na(.), 0, .)) %>%
  rowwise() %>%
  mutate(opv_admin_coverage = paste0(round(total_opv_vaccinated / district_population,3)*100,"%"),
         ipv_admin_coverage = paste0(round(total_ipv_vaccinated / district_population,3)*100,"%")) %>%
  ungroup() %>%
  rename(  `Target Population (0-59m)` = district_population,
           `OPV: Total Children Vaccinated (0-59m)` = total_opv_vaccinated,
           `IPV: Total Children Vaccinated (4-59m)` = total_ipv_vaccinated,
           `OPV Admin Coverage (0-59m) (%)` = opv_admin_coverage,
           `IPV Admin Coverage (4-59m) (%)` = ipv_admin_coverage
  )  %>%
  arrange(campaign_name, region, province, district)  %>%
  select(campaign_name, region, rcode, province, pcode, district, dcode, everything()) 
