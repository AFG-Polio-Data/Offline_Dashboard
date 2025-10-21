#OPV part of fIPV campaign

opv_7day <- df_apmis_list$`fIPV OPV DC Daily Compilation Day 1-7` %>%
  select(
    region, province, district, campaigns, rcode, pcode, dcode, ccode, formname,
    campaign_uuid, uuid, clustername, phase, campaign_date, creation_date,
    change_date, campaign_form_meta_uuid, region_uuid, province_uuid,
    district_uuid, community_uuid, archived, ispublished, isverified, source,
    
    visit_date_day1_fmt, visit_date_day2_fmt, visit_date_day3_fmt, visit_date_day4_fmt, visit_date_day5, visit_date_day6_fmt, visit_date_day7_fmt,
    no_site_visited_day1, no_site_visited_day2, no_site_visited_day3, no_site_visited_day4, no_site_visited_day5, no_site_visited_day6, no_site_visited_day7,
    opv_total_children_vaccinated_day1, opv_total_children_vaccinated_day2, opv_total_children_vaccinated_day3, opv_total_children_vaccinated_day4, opv_total_children_vaccinated_day5, opv_total_children_vaccinated_day6, opv_total_children_vaccinated_day7,
    opv_no_vials_used_day1, opv_no_vials_used_day2, opv_no_vials_used_day3, opv_no_vials_used_day4, opv_no_vials_used_day5, opv_no_vials_used_day6, opv_no_vials_used_day7,
    opv_no_vials_distributed_day1, opv_no_vials_distributed_day2, opv_no_vials_distributed_day3, opv_no_vials_distributed_day4, opv_no_vials_distributed_day5, opv_no_vials_distributed_day6, opv_no_vials_distributed_day7,
    opv_additional_children_vaccinated_day7,
    opvhrmp_vaccinated_day1, opvhrmp_vaccinated_day2, opvhrmp_vaccinated_day3, opvhrmp_vaccinated_day4, opvhrmp_vaccinated_day5, opvhrmp_vaccinated_day6
  ) %>%
  # Remove the "opv" prefix
  rename_with(~ gsub("^opv", "", .x), starts_with("opv")) %>%
  rename_with(~ gsub("^_", "", .x), starts_with("_")) %>%
  rename_with(~ gsub("^no_vials_distributed", "no_f_ull_vials_received", .x), starts_with("no_vials")) %>%
  
  # Convert all numeric-like columns to numeric
  mutate(across(
    matches("(total_children_vaccinated|additional_children_vaccinated|no_vials_used|no_site_visited|hrmp_vaccinated)"),
    ~ suppressWarnings(as.numeric(.))
  )) %>%
  
  mutate(
    total_vaccinated = rowSums(across(
      c(total_children_vaccinated_day1:total_children_vaccinated_day7
        # ,
        # additional_children_vaccinated_day7
        )
    ), na.rm = TRUE),
    
    opv_vials_used = rowSums(across(no_vials_used_day1:no_vials_used_day7), na.rm = TRUE),
    opv_doses_used = opv_vials_used * 20,
    
    vacc_wastage = ifelse(opv_doses_used > 0,
                          (opv_doses_used - total_vaccinated) / opv_doses_used,
                          NA),
    
    total_sites_visited = rowSums(across(starts_with("no_site_visited")), na.rm = TRUE),
    
    hrmp_vaccinated = ifelse(
      rowSums(!is.na(across(starts_with("hrmp_vaccinated")))) == 0,
      NA,
      rowSums(across(starts_with("hrmp_vaccinated")), na.rm = TRUE)
    ),
    
    pct_hrmp = ifelse(!is.na(hrmp_vaccinated),
                      hrmp_vaccinated / total_vaccinated, NA),
    
    vaccine_type = "OPV"
  ) %>%
  
  # Select final variables (quotes required in any_of)
  select(any_of(c(
    "formname", "campaigns", "region", "province", "district", "clustername",
    "rcode", "pcode", "dcode", "ccode", "vaccine_type",
    "visit_date_day1_fmt", "visit_date_day2_fmt", "visit_date_day3_fmt", "visit_date_day4_fmt", "visit_date_day5_fmt", "visit_date_day6_fmt", "visit_date_day7_fmt",
    "total_children_vaccinated_day1", "total_children_vaccinated_day2",
    "total_children_vaccinated_day3", "total_children_vaccinated_day4",
    "total_children_vaccinated_day5", "total_children_vaccinated_day6",
    "total_children_vaccinated_day7", 
    "total_vaccinated", "opv_vials_used", "opv_doses_used", "vacc_wastage",
    "no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", 
    "no_site_visited_day4", "no_site_visited_day5", "no_site_visited_day6",  "no_site_visited_day7",
    "total_sites_visited",
    "no_f_ull_vials_received_day1", "no_f_ull_vials_received_day2",
    "no_f_ull_vials_received_day3", "no_f_ull_vials_received_day4",
    "no_f_ull_vials_received_day5", "no_f_ull_vials_received_day6",
    "no_f_ull_vials_received_day7", 
    "hrmp_vaccinated", "pct_hrmp"
  ))) %>%
  
  # Round select numeric vars
  mutate(across(any_of(
    c("total_children_vaccinated_day1", "total_children_vaccinated_day2",
      "total_children_vaccinated_day3", "total_children_vaccinated_day4",
      "total_children_vaccinated_day5", "total_children_vaccinated_day6",
      "total_children_vaccinated_day7",
      "total_vaccinated", "opv_vials_used", "opv_doses_used",
      "no_site_visited_day1", "no_site_visited_day2",
      "no_site_visited_day3", "no_site_visited_day4",
      "no_site_visited_day5", "no_site_visited_day6",
      "no_site_visited_day7",
      "total_sites_visited", "hrmp_vaccinated")),
    ~ round(., 0)
  ))


ipv_7day <-  df_apmis_list$`fIPV OPV DC Daily Compilation Day 1-7` %>%
  # ipv_7day <- temp %>% 
  select(
    region, province, district, campaigns, rcode, pcode, dcode, ccode, formname,
    campaign_uuid, uuid, clustername, phase, campaign_date, creation_date,
    change_date, campaign_form_meta_uuid, region_uuid, province_uuid,
    district_uuid, community_uuid, archived, ispublished, isverified, source,
    
    visit_date_day1_fmt, visit_date_day2_fmt, visit_date_day3_fmt, visit_date_day4_fmt, visit_date_day5_fmt, visit_date_day6_fmt, visit_date_day7_fmt,
    no_site_visited_day1, no_site_visited_day2, no_site_visited_day3, no_site_visited_day4, no_site_visited_day5, no_site_visited_day6, no_site_visited_day7,
    ipv_total_children_vaccinated_day1, ipv_total_children_vaccinated_day2, ipv_total_children_vaccinated_day3, ipv_total_children_vaccinated_day4, ipv_total_children_vaccinated_day5, ipv_total_children_vaccinated_day6, ipv_total_children_vaccinated_day7,
    ipv_no_vials_used_day1, ipv_no_vials_used_day2, ipv_no_vials_used_day3, ipv_no_vials_used_day4, ipv_no_vials_used_day5, ipv_no_vials_used_day6, ipv_no_vials_used_day7,
    ipv_no_vials_distributed_day1, ipv_no_vials_distributed_day2, ipv_no_vials_distributed_day3, ipv_no_vials_distributed_day4, ipv_no_vials_distributed_day5, ipv_no_vials_distributed_day6, ipv_no_vials_distributed_day7,
    ipv_additional_children_vaccinated_day7,
    ipvhrmp_vaccinated_day1, ipvhrmp_vaccinated_day2, ipvhrmp_vaccinated_day3, ipvhrmp_vaccinated_day4, ipvhrmp_vaccinated_day5, ipvhrmp_vaccinated_day6, ipvhrmp_vaccinated_day6
  ) %>%
  # Remove the "opv" prefix
  rename_with(~ gsub("^ipv", "", .x), starts_with("ipv")) %>%
  rename_with(~ gsub("^_", "", .x), starts_with("_")) %>%
  rename_with(~ gsub("^no_vials_distributed", "no_f_ull_vials_received", .x), starts_with("no_vials")) %>%
  
  # Convert all numeric-like columns safely
  mutate(across(
    matches("(total_children_vaccinated|additional_children_vaccinated|no_vials_used|no_site_visited|hrmp_vaccinated)"),
    ~ suppressWarnings(as.numeric(.))
  )) %>%
  
  mutate(
    total_vaccinated = rowSums(across(
      c(total_children_vaccinated_day1:total_children_vaccinated_day7
        # ,
        # additional_children_vaccinated_day7
        )
    ), na.rm = TRUE),
    
    opv_vials_used = rowSums(across(no_vials_used_day1:no_vials_used_day7), na.rm = TRUE),
    opv_doses_used = opv_vials_used * 25,
    
    vacc_wastage = ifelse(opv_doses_used > 0,
                          (opv_doses_used - total_vaccinated) / opv_doses_used,
                          NA),
    
    total_sites_visited = rowSums(across(starts_with("no_site_visited")), na.rm = TRUE),
    
    hrmp_vaccinated = ifelse(
      rowSums(!is.na(across(starts_with("hrmp_vaccinated")))) == 0,
      NA,
      rowSums(across(starts_with("hrmp_vaccinated")), na.rm = TRUE)
    ),
    
    pct_hrmp = ifelse(!is.na(hrmp_vaccinated),
                      hrmp_vaccinated / total_vaccinated, NA),
    
    vaccine_type = "IPV"
  ) %>%
  
  # Select final variables (quotes required in any_of)
  select(any_of(c(
    "formname", "campaigns", "region", "province", "district", "clustername",
    "rcode", "pcode", "dcode", "ccode", "vaccine_type", 
    "visit_date_day1_fmt", "visit_date_day2_fmt", "visit_date_day3_fmt", "visit_date_day4_fmt", "visit_date_day5_fmt", "visit_date_day6_fmt", "visit_date_day7_fmt",
    "total_children_vaccinated_day1", "total_children_vaccinated_day2",
    "total_children_vaccinated_day3", "total_children_vaccinated_day4",
    "total_children_vaccinated_day5", "total_children_vaccinated_day6",
    "total_children_vaccinated_day7", 
    "total_vaccinated", "opv_vials_used", "opv_doses_used", "vacc_wastage",
    "no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", 
    "no_site_visited_day4", "no_site_visited_day5", "no_site_visited_day6",  "no_site_visited_day7",
    "total_sites_visited",
    "no_f_ull_vials_received_day1", "no_f_ull_vials_received_day2",
    "no_f_ull_vials_received_day3", "no_f_ull_vials_received_day4",
    "no_f_ull_vials_received_day5", "no_f_ull_vials_received_day6",
    "no_f_ull_vials_received_day7", 
    "hrmp_vaccinated", "pct_hrmp"
  ))) %>%
  
  # Round select numeric vars
  mutate(across(any_of(
    c("total_children_vaccinated_day1", "total_children_vaccinated_day2",
      "total_children_vaccinated_day3", "total_children_vaccinated_day4",
      "total_children_vaccinated_day5", "total_children_vaccinated_day6",
      "total_children_vaccinated_day7",
      "total_vaccinated", "opv_vials_used", "opv_doses_used",
      "no_site_visited_day1", "no_site_visited_day2",
      "no_site_visited_day3", "no_site_visited_day4",
      "no_site_visited_day5", "no_site_visited_day6",
      "no_site_visited_day7",
      "total_sites_visited", "hrmp_vaccinated")),
    ~ round(., 0)
  ))

  