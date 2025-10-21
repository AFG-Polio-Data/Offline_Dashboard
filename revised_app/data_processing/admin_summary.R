admin_cluster <- admin_all %>%
  select(-c("modality", "max_days", "visit_date_day1_fmt", "visit_date_day2_fmt", "visit_date_day3_fmt", "visit_date_day4_fmt", "visit_date_day5_fmt", "visit_date_day6_fmt", "visit_date_day7_fmt", "vacc_wastage"))  %>%
  group_by(campaigns, region, province, district, clustername, rcode, pcode, dcode, ccode, age_group, vaccine_type) %>%
  summarise_all(~sum(as.numeric(.), na.rm=T)) %>%
  ungroup() 


for(i in c("no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", "no_site_visited_day4", "no_site_visited_day5", "no_site_visited_day6", "no_site_visited_day7")){
  if(!(i %in% colnames(admin_cluster))){
    admin_cluster <- admin_cluster %>%
      mutate({{i}} := NA_real_)
  }
}

admin_cluster <- admin_cluster %>%
  mutate(
    # Vectorized calculation for 'vacc_wastage'
    vacc_wastage = ifelse(opv_doses_used > 0 & !is.na(opv_doses_used), 
                          (opv_doses_used - total_vaccinated) / opv_doses_used, NA),

    # Convert 0 to NA for specific columns using across()
    across(c("no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", "no_site_visited_day4", "no_site_visited_day5", "no_site_visited_day6", "no_site_visited_day7", "total_sites_visited"), 
           ~ifelse(. == 0, NA, .))
  ) %>%
  ungroup() %>%
  left_join(admin_max_days_cluster %>%
              ungroup(), 
            by=c("campaigns", "rcode", "pcode", "dcode", "ccode", "age_group")) %>%
  left_join(admin_modality_cluster_summary %>%
              ungroup() %>%
              select(campaigns, rcode, pcode, dcode, ccode, modality) %>%
              group_by(campaigns, rcode, pcode, dcode, ccode) %>%
              slice(1) %>%
              ungroup(),
            by=c("campaigns", "rcode", "pcode", "dcode", "ccode")) %>%
  mutate(
    # Convert 0 to NA for specific columns using across()
    across(c("no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", "no_site_visited_day4", "no_site_visited_day5", "no_site_visited_day6", "no_site_visited_day7", "total_sites_visited", "hrmp_vaccinated"), 
           ~ifelse(. == 0, NA, .)),
    
    # Calculate 'site_density' directly without rowwise
    site_density = ifelse(
      modality %in% c("S2S", "M2M/S2S", "M2M") & 
        !is.na(total_sites_visited) & total_sites_visited > 0 & 
        !is.na(total_vaccinated) & total_vaccinated > 0, 
      total_vaccinated / total_sites_visited, NA
    ),
    
    pct_hrmp = ifelse(!is.na(hrmp_vaccinated), hrmp_vaccinated / total_vaccinated, NA)
  )

admin_district <- admin_cluster %>%
  select(-c("modality", "vacc_wastage", "clustername", "ccode", "max_days")) %>%
  group_by(campaigns, region, province, district, rcode, pcode, dcode, age_group, vaccine_type) %>%
  summarise_all(~sum(as.numeric(.), na.rm=T)) %>%
  ungroup() %>%
  
  # First, handle 0 values in specific columns
  mutate(across(c("no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", "no_site_visited_day4", "no_site_visited_day5", "no_site_visited_day6", "no_site_visited_day7", "total_sites_visited", "hrmp_vaccinated"),
                ~ifelse(. == 0, NA, .))) %>%
  
  # Then, calculate vacc_wastage
  mutate(vacc_wastage = ifelse(opv_doses_used > 0 & !is.na(opv_doses_used), 
                               (opv_doses_used - total_vaccinated) / opv_doses_used, NA),
         pct_hrmp = ifelse(!is.na(hrmp_vaccinated), hrmp_vaccinated / total_vaccinated, NA)) %>%
  
  
  # Join necessary data
  left_join(df_apmis_list$campaign_district_pop %>%
              select(campaign_name, ageGroup, district_population, rcode, pcode, dcode, region_name, province_name, district_name) %>%
              rename(target_population = district_population,
                     region = region_name,
                     province = province_name,
                     district = district_name) %>%
              filter(!is.na(rcode) & !is.na(pcode) & !is.na(dcode) &
                       target_population != 0 & !is.na(target_population) &
                       !is.na(ageGroup)) %>%
              mutate(ageGroup = case_when(ageGroup == "AGE_0_4" ~ "0-59 Months",
                                          ageGroup == "AGE_5_10" ~ "5-10 Years",
                                          TRUE ~ ageGroup)) %>%
              bind_rows(df_apmis_list$campaign_district_pop %>%
                          filter(grepl("IPV", campaign_name)) %>%
                          select(campaign_name, ageGroup, district_population, rcode, pcode, dcode, region_name, province_name, district_name) %>%
                          rename(target_population = district_population,
                                 region = region_name,
                                 province = province_name,
                                 district = district_name) %>%
                          filter(!is.na(rcode) & !is.na(pcode) & !is.na(dcode) &
                                   target_population != 0 & !is.na(target_population) &
                                   !is.na(ageGroup)) %>%
                          mutate(ageGroup = case_when(ageGroup == "AGE_0_4" ~ "4-59 Months",
                                                      ageGroup == "AGE_5_10" ~ "5-10 Years",
                                                      TRUE ~ ageGroup))), 
            by = c("campaigns" = "campaign_name", "age_group" = "ageGroup", "rcode", "pcode", "dcode", "region", "province", "district")) %>%
  left_join(admin_max_days_district, 
            by = c("campaigns", "rcode", "pcode", "dcode", "age_group")) %>%
  left_join(admin_modality_district_summary %>%
              select(campaigns, rcode, pcode, dcode, modality) %>%
              group_by(campaigns, rcode, pcode, dcode) %>%
              slice(1) %>%
              ungroup(),
            by = c("campaigns", "rcode", "pcode", "dcode")) %>%
  
  # Now calculate site_density after the join
  mutate(site_density = ifelse(modality %in% c("S2S", "M2M/S2S", "M2M") & !is.na(total_sites_visited) & total_sites_visited > 0 & !is.na(total_vaccinated) & total_vaccinated > 0, 
                               total_vaccinated / total_sites_visited,
                               NA)) %>%
  
  # Continue with cumulative coverage calculations (use row-wise summation for correct coverage)
  mutate(
    cum_cov_day1 = ifelse(target_population > 0 & !is.na(target_population), 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day2 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 1, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + select(., starts_with("total_children_vaccinated_day2")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day3 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 2, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day4 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 3, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")) + 
                                    select(., starts_with("total_children_vaccinated_day4")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day5 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 4, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")) + 
                                    select(., starts_with("total_children_vaccinated_day4")) + 
                                    select(., starts_with("total_children_vaccinated_day5")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day6 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 5, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")) + 
                                    select(., starts_with("total_children_vaccinated_day4")) + 
                                    select(., starts_with("total_children_vaccinated_day5")) + 
                                    select(., starts_with("total_children_vaccinated_day6")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day7 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 6, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")) + 
                                    select(., starts_with("total_children_vaccinated_day4")) + 
                                    select(., starts_with("total_children_vaccinated_day5")) + 
                                    select(., starts_with("total_children_vaccinated_day6")) + 
                                    select(., starts_with("total_children_vaccinated_day7")), na.rm = TRUE) / target_population, 
                          NA),
    
    # Corrected total_coverage calculation by specifying columns for each cumulative day
    total_coverage = ifelse(target_population > 0 & !is.na(target_population),
                            pmax(cum_cov_day1, cum_cov_day2, cum_cov_day3, cum_cov_day4, cum_cov_day5, cum_cov_day6, cum_cov_day7, na.rm = TRUE),
                            NA)
  ) %>%
  ungroup()

admin_province <- admin_district %>%
  select(-c("modality", "vacc_wastage", "district", "dcode", "max_days")) %>%
  group_by(campaigns, region, province, rcode, pcode, age_group, vaccine_type) %>%
  summarise_all(~sum(as.numeric(.), na.rm=T)) %>%
  ungroup() %>%
  
  # Handle 0 values in specific columns
  mutate_at(c("no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", "no_site_visited_day4", "no_site_visited_day5", "no_site_visited_day6", "no_site_visited_day7", "total_sites_visited", "hrmp_vaccinated"), 
            ~ifelse(. == 0, NA, .)) %>%
  
  # Calculate vacc_wastage
  mutate(vacc_wastage = ifelse(opv_doses_used > 0 & !is.na(opv_doses_used), 
                                                                  (opv_doses_used - total_vaccinated) / opv_doses_used, NA),
         pct_hrmp = ifelse(!is.na(hrmp_vaccinated), hrmp_vaccinated / total_vaccinated, NA)) %>%
  
  left_join(admin_max_days_province, 
            by = c("campaigns", "rcode", "pcode", "age_group")) %>%
  
  # Calculate cumulative coverage row-wise (use rowSums for this)
  mutate(
    cum_cov_day1 = ifelse(target_population > 0 & !is.na(target_population), 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day2 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 1, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + select(., starts_with("total_children_vaccinated_day2")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day3 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 2, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day4 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 3, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")) + 
                                    select(., starts_with("total_children_vaccinated_day4")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day5 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 4, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")) + 
                                    select(., starts_with("total_children_vaccinated_day4")) + 
                                    select(., starts_with("total_children_vaccinated_day5")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day6 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 5, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")) + 
                                    select(., starts_with("total_children_vaccinated_day4")) + 
                                    select(., starts_with("total_children_vaccinated_day5")) + 
                                    select(., starts_with("total_children_vaccinated_day6")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day7 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 6, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")) + 
                                    select(., starts_with("total_children_vaccinated_day4")) + 
                                    select(., starts_with("total_children_vaccinated_day5")) + 
                                    select(., starts_with("total_children_vaccinated_day6")) + 
                                    select(., starts_with("total_children_vaccinated_day7")), na.rm = TRUE) / target_population, 
                          NA),
    
    # Calculate total coverage
    total_coverage = ifelse(target_population > 0 & !is.na(target_population),
                            pmax(cum_cov_day1, cum_cov_day2, cum_cov_day3, cum_cov_day4, cum_cov_day5, cum_cov_day6, cum_cov_day7, na.rm = TRUE),
                            NA)
  ) %>%
  
  # Join modality data
  left_join(admin_modality_province_summary %>%
              select(campaigns, rcode, pcode, modality) %>%
              group_by(campaigns, rcode, pcode) %>%
              slice(1) %>%
              ungroup(),
            by = c("campaigns", "rcode", "pcode")) %>%
  
  # Handle 0 values in site visitation columns again
  mutate_at(c("no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", "no_site_visited_day4", "no_site_visited_day5", "no_site_visited_day6", "no_site_visited_day7", "total_sites_visited"), 
            ~ifelse(. == 0, NA, .)) %>%
  
  # Calculate site_density
  mutate(site_density = ifelse(modality %in% c("S2S", "M2M/S2S", "M2M") & !is.na(total_sites_visited) & total_sites_visited > 0 & !is.na(total_vaccinated) & total_vaccinated > 0, 
                               total_vaccinated / total_sites_visited,
                               NA)) %>%
  
  ungroup()

admin_region <- admin_province %>%
  select(-c("modality", "vacc_wastage", "province", "pcode", "max_days")) %>%
  group_by(campaigns, region, rcode, age_group, vaccine_type) %>%
  summarise_all(~sum(as.numeric(.), na.rm = TRUE)) %>%
  ungroup() %>%
  
  # Handle 0 values in specific columns
  mutate_at(c("no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", "no_site_visited_day4", "no_site_visited_day5", "no_site_visited_day6", "no_site_visited_day7", "total_sites_visited", "hrmp_vaccinated"), 
            ~ifelse(. == 0, NA, .)) %>%
  
  # Calculate vacc_wastage
  mutate(vacc_wastage = ifelse(opv_doses_used > 0 & !is.na(opv_doses_used), 
                                                                  (opv_doses_used - total_vaccinated) / opv_doses_used, NA),
      pct_hrmp = ifelse(!is.na(hrmp_vaccinated), hrmp_vaccinated / total_vaccinated, NA)) %>%
  
  left_join(admin_max_days_region, 
            by = c("campaigns", "rcode", "age_group")) %>%
  
  # Calculate cumulative coverage row-wise (use rowSums for this)
  mutate(
    cum_cov_day1 = ifelse(target_population > 0 & !is.na(target_population), 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day2 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 1, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + select(., starts_with("total_children_vaccinated_day2")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day3 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 2, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day4 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 3, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")) + 
                                    select(., starts_with("total_children_vaccinated_day4")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day5 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 4, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")) + 
                                    select(., starts_with("total_children_vaccinated_day4")) + 
                                    select(., starts_with("total_children_vaccinated_day5")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day6 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 5, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")) + 
                                    select(., starts_with("total_children_vaccinated_day4")) + 
                                    select(., starts_with("total_children_vaccinated_day5")) + 
                                    select(., starts_with("total_children_vaccinated_day6")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day7 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 6, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")) + 
                                    select(., starts_with("total_children_vaccinated_day4")) + 
                                    select(., starts_with("total_children_vaccinated_day5")) + 
                                    select(., starts_with("total_children_vaccinated_day6")) + 
                                    select(., starts_with("total_children_vaccinated_day7")), na.rm = TRUE) / target_population, 
                          NA),
    
    # Calculate total coverage
    total_coverage = ifelse(target_population > 0 & !is.na(target_population),
                            pmax(cum_cov_day1, cum_cov_day2, cum_cov_day3, cum_cov_day4, cum_cov_day5, cum_cov_day6, cum_cov_day7, na.rm = TRUE),
                            NA)
  ) %>%
  
  # Join modality data
  left_join(admin_modality_region_summary %>%
              select(campaigns, rcode, modality) %>%
              group_by(campaigns, rcode) %>%
              slice(1) %>%
              ungroup(),
            by = c("campaigns", "rcode")) %>% 
  
  # Handle 0 values in site visitation columns again
  mutate_at(c("no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", "no_site_visited_day4", "no_site_visited_day5", "no_site_visited_day6", "no_site_visited_day7", "total_sites_visited"), 
            ~ifelse(. == 0, NA, .)) %>%
  
  # Calculate site_density
  mutate(site_density = ifelse(modality %in% c("S2S", "M2M/S2S", "M2M") & !is.na(total_sites_visited) & total_sites_visited > 0 & !is.na(total_vaccinated) & total_vaccinated > 0, 
                               total_vaccinated / total_sites_visited,
                               NA)) %>%
  
  ungroup()

admin_campaign <- admin_region %>%
  select(-c("modality", "vacc_wastage", "region", "rcode", "max_days")) %>%
  group_by(campaigns, age_group, vaccine_type) %>%
  summarise_all(~sum(as.numeric(.), na.rm = TRUE)) %>%
  ungroup() %>%
  
  # Handle 0 values in specific columns
  mutate_at(c("no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", "no_site_visited_day4", "no_site_visited_day5", "no_site_visited_day6", "no_site_visited_day7", "total_sites_visited", "hrmp_vaccinated"), 
            ~ifelse(. == 0, NA, .)) %>%
  
  # Calculate vacc_wastage
  mutate(vacc_wastage = ifelse(opv_doses_used > 0 & !is.na(opv_doses_used), 
                                                                  (opv_doses_used - total_vaccinated) / opv_doses_used, NA),
         pct_hrmp = ifelse(!is.na(hrmp_vaccinated), hrmp_vaccinated / total_vaccinated, NA)) %>%
  
  left_join(admin_max_days_campaign, 
            by = c("campaigns", "age_group")) %>%
  
  # Calculate cumulative coverage row-wise (use rowSums for this)
  mutate(
    cum_cov_day1 = ifelse(target_population > 0 & !is.na(target_population), 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day2 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 1, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + select(., starts_with("total_children_vaccinated_day2")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day3 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 2, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day4 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 3, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")) + 
                                    select(., starts_with("total_children_vaccinated_day4")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day5 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 4, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")) + 
                                    select(., starts_with("total_children_vaccinated_day4")) + 
                                    select(., starts_with("total_children_vaccinated_day5")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day6 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 5, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")) + 
                                    select(., starts_with("total_children_vaccinated_day4")) + 
                                    select(., starts_with("total_children_vaccinated_day5")) + 
                                    select(., starts_with("total_children_vaccinated_day6")), na.rm = TRUE) / target_population, 
                          NA),
    cum_cov_day7 = ifelse(target_population > 0 & !is.na(target_population) & max_days > 6, 
                          rowSums(select(., starts_with("total_children_vaccinated_day1")) + 
                                    select(., starts_with("total_children_vaccinated_day2")) + 
                                    select(., starts_with("total_children_vaccinated_day3")) + 
                                    select(., starts_with("total_children_vaccinated_day4")) + 
                                    select(., starts_with("total_children_vaccinated_day5")) + 
                                    select(., starts_with("total_children_vaccinated_day6")) + 
                                    select(., starts_with("total_children_vaccinated_day7")), na.rm = TRUE) / target_population, 
                          NA),
    
    # Calculate total coverage
    total_coverage = ifelse(target_population > 0 & !is.na(target_population),
                            pmax(cum_cov_day1, cum_cov_day2, cum_cov_day3, cum_cov_day4, cum_cov_day5, cum_cov_day6, cum_cov_day7, na.rm = TRUE),
                            NA)
  ) %>%
  
  # Join modality data
  left_join(admin_modality_national_summary %>%
              select(campaigns, modality) %>%
              group_by(campaigns) %>%
              slice(1) %>%
              ungroup(),
            by = c("campaigns")) %>%
  
  # Handle 0 values in site visitation columns again
  mutate_at(c("no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", "no_site_visited_day4", "no_site_visited_day5", "no_site_visited_day6", "no_site_visited_day7", "total_sites_visited"), 
            ~ifelse(. == 0, NA, .)) %>%
  
  # Calculate site_density
  mutate(site_density = ifelse(modality %in% c("S2S", "M2M/S2S", "M2M") & !is.na(total_sites_visited) & total_sites_visited > 0 & !is.na(total_vaccinated) & total_vaccinated > 0, 
                               total_vaccinated / total_sites_visited,
                               NA)) %>%
  
  ungroup()

admin_cluster <- admin_cluster %>%
  # select(-c("region", "province", "district")) %>%
  left_join(rpd_list, by=c("rcode" = "APMIS_RCODE",
                           "pcode" = "APMIS_PCODE",
                           "dcode" = "APMIS_DCODE")) %>%
  mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
         province = ifelse(!is.na(APMIS_Province), APMIS_Province, province),
         district = ifelse(!is.na(APMIS_District), APMIS_District, district))

admin_district <- admin_district %>%
  # select(-c("region", "province", "district")) %>%
  left_join(rpd_list, by=c("rcode" = "APMIS_RCODE",
                           "pcode" = "APMIS_PCODE",
                           "dcode" = "APMIS_DCODE")) %>%
  mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
         province = ifelse(!is.na(APMIS_Province), APMIS_Province, province),
         district = ifelse(!is.na(APMIS_District), APMIS_District, district))

admin_province <- admin_province %>%
  # select(-c("region", "province")) %>%
  left_join(rpd_list %>%
              select(APMIS_RCODE, APMIS_Region,
                     APMIS_PCODE, APMIS_Province) %>%
              unique(),
            by=c("rcode" = "APMIS_RCODE",
                 "pcode" = "APMIS_PCODE")) %>%
  mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region),
         province = ifelse(!is.na(APMIS_Province), APMIS_Province, province))

admin_region <- admin_region %>%
  # select(-c("region")) %>%
  left_join(rpd_list %>%
              select(APMIS_RCODE, APMIS_Region) %>%
              unique(),
            by=c("rcode" = "APMIS_RCODE")) %>%
  mutate(region = ifelse(!is.na(APMIS_Region), APMIS_Region, region))


keep_zero_hrmp <- admin_campaign %>%
  filter(!is.na(hrmp_vaccinated))

admin_cluster <- admin_cluster %>%
  mutate_at(c("hrmp_vaccinated", "pct_hrmp"), ~ifelse(is.na(.) & campaigns %in% keep_zero_hrmp$campaigns, 0, .))
admin_district <- admin_district %>%
  mutate_at(c("hrmp_vaccinated", "pct_hrmp"), ~ifelse(is.na(.) & campaigns %in% keep_zero_hrmp$campaigns, 0, .))
admin_province <- admin_province %>%
  mutate_at(c("hrmp_vaccinated", "pct_hrmp"), ~ifelse(is.na(.) & campaigns %in% keep_zero_hrmp$campaigns, 0, .))
admin_region <- admin_region %>%
  mutate_at(c("hrmp_vaccinated", "pct_hrmp"), ~ifelse(is.na(.) & campaigns %in% keep_zero_hrmp$campaigns, 0, .))
admin_campaign <- admin_campaign %>%
  mutate_at(c("hrmp_vaccinated", "pct_hrmp"), ~ifelse(is.na(.) & campaigns %in% keep_zero_hrmp$campaigns, 0, .))

