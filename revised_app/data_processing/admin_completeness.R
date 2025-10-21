admin_cluster_completeness <- admin_cluster %>%
  mutate(
    day1_reported = as.integer(total_children_vaccinated_day1 > 0 | no_f_ull_vials_received_day1 > 0),
    day2_reported = as.integer(total_children_vaccinated_day2 > 0 | no_f_ull_vials_received_day2 > 0),
    day3_reported = as.integer(total_children_vaccinated_day3 > 0 | no_f_ull_vials_received_day3 > 0),
    day4_reported = if_else(max_days >= 4, as.integer(total_children_vaccinated_day4 > 0 | no_f_ull_vials_received_day4 > 0), NA_integer_),
    day5_reported = if_else(max_days >= 5, as.integer(total_children_vaccinated_day5 > 0 | no_f_ull_vials_received_day5 > 0), NA_integer_),
    day6_reported = if_else(max_days >= 6, as.integer(total_children_vaccinated_day6 > 0 | no_f_ull_vials_received_day6 > 0), NA_integer_),
    day7_reported = if_else(max_days >= 7, as.integer(total_children_vaccinated_day7 > 0 | no_f_ull_vials_received_day7 > 0), NA_integer_)
    # reporting_complete = case_when(max_days == 3 & (day1_reported == 1 & day2_reported ==1 & day3_reported == 1) ~ 1,
    #                                max_days == 4 & (day1_reported == 1 & day2_reported ==1 & day3_reported == 1 & day4_reported == 1) ~ 1,
    #                                max_days == 5 & (day1_reported == 1 & day2_reported ==1 & day3_reported == 1 & day4_reported == 1 & day5_reported == 1) ~ 1,
    #                                max_days == 6 & (day1_reported == 1 & day2_reported ==1 & day3_reported == 1 & day4_reported == 1 & day5_reported == 1 & day6_reported == 1) ~ 1,
    #                                max_days == 7 & (day1_reported == 1 & day2_reported ==1 & day3_reported == 1 & day4_reported == 1 & day5_reported == 1 & day6_reported == 1 & day7_reported == 1) ~ 1,
    #                                TRUE ~ 0)
  ) %>%
  select(campaigns, region, province, district, clustername,
         rcode, pcode, dcode, ccode, age_group,
         day1_reported, day2_reported, day3_reported, 
         day4_reported, day5_reported, day6_reported, day7_reported) %>%
  distinct() %>%
  filter(age_group != "4-59 Months") %>%
  full_join(campaign_rpdc %>%
              select(campaign_name, 
                     region_name, province_name, district_name, cluster_name, rcode, pcode, dcode, ccode) %>%
              group_by(campaign_name, rcode, pcode, dcode, ccode) %>%
              slice(1) %>%
              ungroup() %>%
              mutate(age_group = "0-59 Months"),
            by=c("campaigns"="campaign_name",
                 "rcode",
                 "pcode",
                 "dcode", 
                 "ccode",
                 "age_group")) %>%
  mutate(region = ifelse(is.na(region), region_name, region),
         province = ifelse(is.na(province), province_name, province),
         district = ifelse(is.na(district), district_name, district),
         clustername = ifelse(is.na(clustername), cluster_name, clustername)) %>%
  select(-c("region_name", "province_name", "district_name", "cluster_name")) %>%
 full_join(campaign_rpdc %>%
              select(campaign_name, 
                     region_name, province_name, district_name, cluster_name, rcode, pcode, dcode, ccode) %>%
              group_by(campaign_name, rcode, pcode, dcode, ccode) %>%
              slice(1) %>%
              ungroup() %>%
              mutate(age_group = "5-10 Years"),
            by=c("campaigns"="campaign_name",
                 "rcode",
                 "pcode",
                 "dcode", 
                 "ccode",
                 "age_group")) %>%
  mutate(region = ifelse(is.na(region), region_name, region),
         province = ifelse(is.na(province), province_name, province),
         district = ifelse(is.na(district), district_name, district),
         clustername = ifelse(is.na(clustername), cluster_name, clustername)) %>%
  select(-c("region_name", "province_name", "district_name", "cluster_name"))  %>%
  left_join(admin_max_days_district %>%
              select(campaigns, rcode, pcode, dcode, age_group, max_days) %>%
              unique(),
            by=c("campaigns", "rcode", "pcode", "dcode", "age_group")) %>%
  mutate(max_days = ifelse(is.na(max_days), case_when(grepl("IPV", toupper(campaigns)) ~ 7,
                                                      TRUE ~ 4), max_days)) %>%
  mutate(day1_reported = ifelse(is.na(day1_reported) & max_days >= 1, 0, day1_reported),
         day2_reported = ifelse(is.na(day2_reported) & max_days >= 2, 0, day2_reported),
         day3_reported = ifelse(is.na(day3_reported) & max_days >= 3, 0, day3_reported),
         day4_reported = ifelse(is.na(day4_reported) & max_days >= 4, 0, day4_reported),
         day5_reported = ifelse(is.na(day5_reported) & max_days >= 5, 0, day5_reported),
         day6_reported = ifelse(is.na(day6_reported) & max_days >= 6, 0, day6_reported),
         day7_reported = ifelse(is.na(day7_reported) & max_days >= 7, 0, day7_reported)) %>%
  rowwise() %>%
  mutate(day1_expected = ifelse(max_days >= 1, 1, 0),
         day2_expected = ifelse(max_days >= 2, 1, 0),
         day3_expected = ifelse(max_days >= 3, 1, 0),
         day4_expected = ifelse(max_days >= 4, 1, 0),
         day5_expected = ifelse(max_days >= 5, 1, 0),
         day6_expected = ifelse(max_days >= 6, 1, 0),
         day7_expected = ifelse(max_days >= 7, 1, 0)) %>%
  mutate(reporting_complete = case_when(sum(day1_reported, day2_reported, day3_reported, day4_reported, day5_reported, day6_reported, day7_reported, na.rm=T) == 0 ~ 0,
                                        sum(day1_reported, day2_reported, day3_reported, day4_reported, day5_reported, day6_reported, day7_reported, na.rm=T) == sum(day1_expected, day2_expected, day3_expected, day4_expected, day5_expected, day6_expected, day7_expected, na.rm=T) ~ 1,
                                        TRUE ~ 0)) %>%
  ungroup() %>%
  mutate(reporting_complete = ifelse(is.na(reporting_complete), 0, reporting_complete),
         total = 1,
         day1_pct_reported = ifelse(day1_expected > 0, day1_reported/day1_expected, NA),
         day2_pct_reported = ifelse(day2_expected > 0, day2_reported/day2_expected, NA),
         day3_pct_reported = ifelse(day3_expected > 0, day3_reported/day3_expected, NA),
         day4_pct_reported = ifelse(day4_expected > 0, day4_reported/day4_expected, NA),
         day5_pct_reported = ifelse(day5_expected > 0, day5_reported/day5_expected, NA),
         day6_pct_reported = ifelse(day6_expected > 0, day6_reported/day6_expected, NA),
         day7_pct_reported = ifelse(day7_expected > 0, day7_reported/day7_expected, NA),
         total_pct_reported = reporting_complete/total)  %>%
  # select(-c("clustername")) %>%
  left_join(campaign_rpdc %>%
              select(cluster_name, rcode, pcode, dcode, ccode) %>%  
              group_by(rcode, pcode, dcode, ccode) %>%     
              slice(1) %>%          
              ungroup() %>%
              distinct(),
            by=c("rcode", "pcode", "dcode", "ccode")) %>%
  mutate(clustername = ifelse(is.na(clustername), cluster_name, clustername)) %>%
  select(-c("cluster_name")) %>%
  # rename(clustername = cluster_name) %>%
  left_join(rpd_list %>%
              select(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE, APMIS_Region, APMIS_Province, APMIS_District) %>%
              unique(),
            by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
  mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
         province = ifelse(!is.na(APMIS_Province), APMIS_Province, province),
         district = ifelse(!is.na(APMIS_District), APMIS_District, district)) %>%
  select(-c("APMIS_Region", "APMIS_Province", "APMIS_District")) %>%
  arrange(campaigns, region, province, district, ccode)

admin_district_completeness <- admin_cluster_completeness %>%
  group_by(campaigns, region, province, district,
           rcode, pcode, dcode,
           age_group) %>%
  summarise_at(c("day1_reported", "day2_reported", "day3_reported", "day4_reported", "day5_reported", "day6_reported", "day7_reported",
                 "day1_expected", "day2_expected", "day3_expected", "day4_expected", "day5_expected", "day6_expected", "day7_expected",
                 "reporting_complete", "total"), ~sum(., na.rm=T)) %>%
  ungroup() %>%
  mutate(day1_pct_reported = ifelse(day1_expected > 0, day1_reported/day1_expected, NA),
         day2_pct_reported = ifelse(day2_expected > 0, day2_reported/day2_expected, NA),
         day3_pct_reported = ifelse(day3_expected > 0, day3_reported/day3_expected, NA),
         day4_pct_reported = ifelse(day4_expected > 0, day4_reported/day4_expected, NA),
         day5_pct_reported = ifelse(day5_expected > 0, day5_reported/day5_expected, NA),
         day6_pct_reported = ifelse(day6_expected > 0, day6_reported/day6_expected, NA),
         day7_pct_reported = ifelse(day7_expected > 0, day7_reported/day7_expected, NA),
         total_pct_reported = reporting_complete/total) 

admin_province_completeness <- admin_cluster_completeness %>%
  group_by(campaigns, region, province,
           rcode, pcode,
           age_group) %>%
  summarise_at(c("day1_reported", "day2_reported", "day3_reported", "day4_reported", "day5_reported", "day6_reported", "day7_reported",
                 "day1_expected", "day2_expected", "day3_expected", "day4_expected", "day5_expected", "day6_expected", "day7_expected",
                 "reporting_complete", "total"), ~sum(., na.rm=T)) %>%
  ungroup() %>%
  mutate(day1_pct_reported = ifelse(day1_expected > 0, day1_reported/day1_expected, NA),
         day2_pct_reported = ifelse(day2_expected > 0, day2_reported/day2_expected, NA),
         day3_pct_reported = ifelse(day3_expected > 0, day3_reported/day3_expected, NA),
         day4_pct_reported = ifelse(day4_expected > 0, day4_reported/day4_expected, NA),
         day5_pct_reported = ifelse(day5_expected > 0, day5_reported/day5_expected, NA),
         day6_pct_reported = ifelse(day6_expected > 0, day6_reported/day6_expected, NA),
         day7_pct_reported = ifelse(day7_expected > 0, day7_reported/day7_expected, NA),
         total_pct_reported = reporting_complete/total) 

admin_region_completeness <- admin_cluster_completeness %>%
  group_by(campaigns, region, 
           rcode,
           age_group) %>%
  summarise_at(c("day1_reported", "day2_reported", "day3_reported", "day4_reported", "day5_reported", "day6_reported", "day7_reported",
                 "day1_expected", "day2_expected", "day3_expected", "day4_expected", "day5_expected", "day6_expected", "day7_expected",
                 "reporting_complete", "total"), ~sum(., na.rm=T)) %>%
  ungroup() %>%
  mutate(day1_pct_reported = ifelse(day1_expected > 0, day1_reported/day1_expected, NA),
         day2_pct_reported = ifelse(day2_expected > 0, day2_reported/day2_expected, NA),
         day3_pct_reported = ifelse(day3_expected > 0, day3_reported/day3_expected, NA),
         day4_pct_reported = ifelse(day4_expected > 0, day4_reported/day4_expected, NA),
         day5_pct_reported = ifelse(day5_expected > 0, day5_reported/day5_expected, NA),
         day6_pct_reported = ifelse(day6_expected > 0, day6_reported/day6_expected, NA),
         day7_pct_reported = ifelse(day7_expected > 0, day7_reported/day7_expected, NA),
         total_pct_reported = reporting_complete/total) 

admin_national_completeness <- admin_cluster_completeness %>%
  group_by(campaigns,
           age_group) %>%
  summarise_at(c("day1_reported", "day2_reported", "day3_reported", "day4_reported", "day5_reported", "day6_reported", "day7_reported",
                 "day1_expected", "day2_expected", "day3_expected", "day4_expected", "day5_expected", "day6_expected", "day7_expected",
                 "reporting_complete", "total"), ~sum(., na.rm=T)) %>%
  ungroup() %>%
  mutate(day1_pct_reported = ifelse(day1_expected > 0, day1_reported/day1_expected, NA),
         day2_pct_reported = ifelse(day2_expected > 0, day2_reported/day2_expected, NA),
         day3_pct_reported = ifelse(day3_expected > 0, day3_reported/day3_expected, NA),
         day4_pct_reported = ifelse(day4_expected > 0, day4_reported/day4_expected, NA),
         day5_pct_reported = ifelse(day5_expected > 0, day5_reported/day5_expected, NA),
         day6_pct_reported = ifelse(day6_expected > 0, day6_reported/day6_expected, NA),
         day7_pct_reported = ifelse(day7_expected > 0, day7_reported/day7_expected, NA),
         total_pct_reported = reporting_complete/total) 
