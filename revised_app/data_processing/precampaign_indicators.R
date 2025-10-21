# Get datasets -------------------------------------------------------------

flw_data <- df_apmis_list[["FLWs Operation Kit DC"]] %>%
  rename(campaign_name = campaigns)
training_data_vol_sm <- df_apmis_list[["Training Monitoring Checklist"]] %>%
  rename(campaign_name = campaigns) %>%
  mutate(
    region = ifelse(!is.na(region_name) & region_name != "", region_name, region),
    province = ifelse(!is.na(province_name) & province_name != "", province_name, province),
    district = ifelse(!is.na(district_name) & district_name != "", district_name, district)
  )
training_data_dc <- df_apmis_list[["DC Training Monitoring"]] %>%
  rename(campaign_name = campaigns) %>%
  mutate(
    region = ifelse(!is.na(region_name) & region_name != "", region_name, region),
    province = ifelse(!is.na(province_name) & province_name != "", province_name, province),
    district = ifelse(!is.na(district_name) & district_name != "", district_name, district)
  )
training_data_cluster <- df_apmis_list[["Cluster Training Monitoring"]] %>%
  rename(campaign_name = campaigns) %>%
  mutate(
    region = ifelse(!is.na(region_name) & region_name != "", region_name, region),
    province = ifelse(!is.na(province_name) & province_name != "", province_name, province),
    district = ifelse(!is.na(district_name) & district_name != "", district_name, district)
  )

summarise_training_data <- function(group_vars = c("campaign_name", "district")) {
  # Summarize participant characteristics
  vol_sm_characteristics <- training_data_vol_sm %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      b3profile_new = sum(as.numeric(b3profile_new), na.rm = TRUE),
      b3profile_females = sum(as.numeric(b3profile_females), na.rm = TRUE),
      b3profile_literate = sum(as.numeric(b3profile_literate), na.rm = TRUE),
      b3profile_residents = sum(as.numeric(b3profile_residents), na.rm = TRUE),
      b1present_volunteers = sum(as.numeric(b1present_volunteers), na.rm = TRUE),
      b1present_sm = sum(as.numeric(b1present_sm), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      vol_sm_pct_new = ifelse((b1present_volunteers + b1present_sm) == 0, NA, b3profile_new / (b1present_volunteers + b1present_sm)),
      vol_sm_pct_female = ifelse((b1present_volunteers + b1present_sm) == 0, NA, b3profile_females / (b1present_volunteers + b1present_sm)),
      vol_sm_pct_literate = ifelse((b1present_volunteers + b1present_sm) == 0, NA, b3profile_literate / (b1present_volunteers + b1present_sm)),
      vol_sm_pct_resident = ifelse((b1present_volunteers + b1present_sm) == 0, NA, b3profile_residents / (b1present_volunteers + b1present_sm))
    )
  
  # Summarize knowledge scores
  vol_sm_avg_knowledge <- training_data_vol_sm %>%
    mutate(
      f1_score = rowSums(across(c("f1part1", "f1part2", "f1part3"), ~ grepl("Right", .x))),
      f2_score = rowSums(across(c("f2part1", "f2part2", "f2part3"), ~ grepl("Right", .x))),
      f3_score = rowSums(across(c("f3part1", "f3part2", "f3part3"), ~ grepl("Right", .x))),
      f4_score = rowSums(across(c("f4part1", "f4part2", "f4part3"), ~ grepl("Right", .x))),
      f5_score = rowSums(across(c("f5part1", "f5part2", "f5part3"), ~ grepl("Right", .x))),
      f6_score = rowSums(across(c("f6part1", "f6part2", "f6part3"), ~ grepl("Right", .x))),
      f7_score = rowSums(across(c("f7part1", "f7part2", "f7part3"), ~ grepl("Right", .x))),
      total_score = f1_score + f2_score + f3_score + f4_score + f5_score + f6_score + f7_score
    ) %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      avg_total_score = sum(total_score, na.rm = TRUE) / (7*3 * n()),
      avg_f1_score = sum(f1_score, na.rm = TRUE) / (3 * n()),
      avg_f2_score = sum(f2_score, na.rm = TRUE) / (3 * n()),
      avg_f3_score = sum(f3_score, na.rm = TRUE) / (3 * n()),
      avg_f4_score = sum(f4_score, na.rm = TRUE) / (3 * n()),
      avg_f5_score = sum(f5_score, na.rm = TRUE) / (3 * n()),
      avg_f6_score = sum(f6_score, na.rm = TRUE) / (3 * n()),
      avg_f7_score = sum(f7_score, na.rm = TRUE) / (3 * n()),
      .groups = "drop"
    )
  
  # Attendance summary
  attendance_summary <- list(
    training_data_vol_sm %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(total_volunteers = sum(as.numeric(b1present_volunteers), na.rm = TRUE),
                total_sm = sum(as.numeric(b1present_sm), na.rm = TRUE), .groups = "drop"),
    
    training_data_dc %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(total_dc = sum(as.numeric(b1present_dc), na.rm = TRUE), .groups = "drop"),
    
    training_data_cluster %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(total_cs = sum(as.numeric(b1present_cs), na.rm = TRUE), .groups = "drop")
  ) %>%
    reduce(full_join, by = group_vars) %>%
    mutate(across(starts_with("total_"), ~replace_na(.x, 0))) %>%
    mutate(total_attendance_all = total_volunteers + total_sm + total_dc + total_cs)
  
  # Session counts
  training_sessions <- list(
    training_data_vol_sm %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(n_vol_sm_sessions = n(), .groups = "drop"),
    
    training_data_dc %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(n_dc_sessions = n(), .groups = "drop"),
    
    training_data_cluster %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(n_cs_sessions = n(), .groups = "drop")
  ) %>%
    reduce(full_join, by = group_vars) %>%
    mutate(across(starts_with("n_"), ~replace_na(.x, 0))) %>%
    mutate(total_sessions_all = n_vol_sm_sessions + n_dc_sessions + n_cs_sessions)
  
  # Combine all
  list(
    vol_sm_characteristics,
    vol_sm_avg_knowledge,
    attendance_summary,
    training_sessions
  ) %>%
    reduce(full_join, by = group_vars) %>%
    arrange(across(all_of(group_vars)))
}

training_district <- summarise_training_data(c("campaign_name", "region", "rcode", "province", "pcode", "district", "dcode"))
training_province <- summarise_training_data(c("campaign_name", "region", "rcode", "province", "pcode"))
training_region <- summarise_training_data(c("campaign_name", "region", "rcode"))
training_national <- summarise_training_data(c("campaign_name"))


# Keep only necessary columns ---------------------------------------------

flw_data <- flw_data %>%
  select(
    campaign_name,
    rcode,
    pcode,
    dcode,
    title,
    gender,
    resident,
    new_selected,
    reason_new_selected,
    paid
  )

# Create total vaccinator count and density indicators --------------------

total_flws_district <- flw_data %>%
  mutate(
    title_cat = case_when(
      title %in% c("Vol", "VolFix", "VolNomad", "VolSpecial", "VolTransit") ~ "vaccinator",
      title %in% c("SM") ~ "social_mobilizer",
      title %in% c("CS") ~ "cluster_supervisor",
      title %in% c("DC") ~ "district_coordinator",
      title %in% c("DataOperator", "DEMT/DST", "DHO", "REMT/PEMT") ~ "district_support",
      title %in% c("FM", "ICM", "LQAS", "PCM") ~ "monitor",
      TRUE ~ title
    )
  ) %>%
  group_by(campaign_name, rcode, pcode, dcode, title_cat) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = title_cat,
              values_from = count,
              values_fill = 0) 

## add in categories if not present
for (i in c(
  "vaccinator",
  "social_mobilizer",
  "cluster_supervisor",
  "district_coordinator",
  "district_support",
  "monitor"
)) {
  if (!(i %in% colnames(total_flws_district))) {
    total_flws_district[[i]] <- 0
  }
}

# Calculate indicators
calc_indicators <- function(df){
  df %>% 
    mutate(pct_paid_among_not_new = if_else(old_flw > 0, paid / old_flw, NA),
           flws_pct_newly_selected = if_else(total_flws > 0, new_flw / total_flws, NA),
           flws_pct_female = if_else(total_flws > 0, female_flw / total_flws, NA),
           flws_pct_resident = if_else(total_flws > 0, flw_resident / total_flws, NA),
           new_flw = if_else(is.na(new_flw) &
                               total_flws > 0, 0, new_flw),
           flw_resident = if_else(is.na(flw_resident) &
                                    total_flws > 0, 0, flw_resident),
           female_flw = if_else(is.na(female_flw) &
                                  total_flws > 0, 0, female_flw),
           paid = if_else(is.na(paid) & old_flw > 0, 0, paid),
           pct_paid_among_not_new = paid / old_flw,
           vaccinators_per_1000 = if_else(target_population > 0,
                                          1000 * (vaccinator / target_population), 
                                          NA),
           social_mobilizers_per_1000 = if_else(target_population > 0,
                                                1000 * (social_mobilizer / target_population),
                                                NA))
}

total_flws_district <- total_flws_district %>%
  mutate(
    total_flws = sum(
      vaccinator,
      social_mobilizer,
      cluster_supervisor,
      district_coordinator,
      district_support,
      monitor,
      na.rm = T
    )
  ) %>% 
  # Add number of female FLW by district
  left_join(flw_data %>%
              filter(gender %in% c("F", "Female")) %>%
              group_by(campaign_name, rcode, pcode, dcode) %>%
              summarise(female_flw = n()),
            by = c("campaign_name", "rcode", "pcode", "dcode")) %>% 
  # Add number of resident FLW by district
  left_join(flw_data %>%
              filter(resident %in% c("TRUE", "yes")) %>%
              group_by(campaign_name, rcode, pcode, dcode) %>%
              summarise(flw_resident = n()), 
            by = c("campaign_name", "rcode", "pcode", "dcode")) %>% 
  # Add number of new FLW by district
  left_join(flw_data %>%
              filter(new_selected %in% c("TRUE", "yes")) %>%
              group_by(campaign_name, rcode, pcode, dcode) %>%
              summarise(new_flw = n()), 
            by = c("campaign_name", "rcode", "pcode", "dcode")) %>% 
  # Add number of paid FLW by district
  left_join(flw_data %>%
              filter(new_selected %in% c("FALSE", "no")) %>%
              group_by(campaign_name, rcode, pcode, dcode) %>%
              summarise(old_flw = n()),
            by = c("campaign_name", "rcode", "pcode", "dcode")) %>% 
  # Add number of paid among only old FLW
  left_join(flw_data %>%
              filter(new_selected %in% c("FALSE", "no")) %>%
              filter(paid %in% c("yes", "TRUE")) %>%
              group_by(campaign_name, rcode, pcode, dcode) %>%
              summarise(paid = n()),
            by = c("campaign_name", "rcode", "pcode", "dcode")) %>%
  # Add population 
  left_join(
    df_apmis_list$campaign_district_pop %>%
      filter(ageGroup == "AGE_0_4") %>%
      select(campaign_name, rcode, pcode, dcode, district_population) %>% 
      rename(target_population=district_population),
    by = c("campaign_name" = "campaign_name", "rcode", "pcode", "dcode")
  ) %>%
  # Add RPD & campaign names
  left_join(
    campaign_rpd %>%
      select(campaign_name, rcode, pcode, dcode, region, province, district),
    by = c("campaign_name" = "campaign_name", "rcode", "pcode", "dcode")
  ) %>%
  calc_indicators()
  
total_flws_province <- total_flws_district %>%
  group_by(campaign_name, rcode, pcode, region, province) %>%
  summarise(across(
    c(
      "cluster_supervisor",
      "district_coordinator",
      "district_support",
      "monitor",
      "social_mobilizer",
      "vaccinator",
      "total_flws",
      "female_flw",
      "flw_resident",
      "old_flw",
      "paid",
      "target_population",
      "new_flw"
    ),
    ~ sum(., na.rm = T)
    )
  ) %>%
  ungroup() %>%
  calc_indicators()

total_flws_region <- total_flws_district %>%
  group_by(campaign_name, rcode, region) %>%
  summarise(across(
    c(
      "cluster_supervisor",
      "district_coordinator",
      "district_support",
      "monitor",
      "social_mobilizer",
      "vaccinator",
      "total_flws",
      "female_flw",
      "flw_resident",
      "old_flw",
      "paid",
      "target_population",
      "new_flw"
    ),
    ~ sum(., na.rm = T)
  )
  ) %>%
  ungroup() %>%
  calc_indicators()

total_flws_national <- total_flws_district %>%
  group_by(campaign_name) %>%
  summarise(
    across(
    c(
      "cluster_supervisor",
      "district_coordinator",
      "district_support",
      "monitor",
      "social_mobilizer",
      "vaccinator",
      "total_flws",
      "female_flw",
      "flw_resident",
      "old_flw",
      "paid",
      "target_population",
      "new_flw"
    ),
    ~ sum(., na.rm = T)
    )
  ) %>%
  ungroup() %>%
  calc_indicators()

for (i in c(
  "total_flws_district",
  "total_flws_province",
  "total_flws_region",
  "total_flws_national"
)) {
  df <- get(i) %>%
    rename(campaign_name = campaign_name) %>%
    mutate(form_type = "flw_operation_kit") %>%
    select(any_of(
      c(
        "form_type",
        "campaign_name",
        "rcode",
        "pcode",
        "dcode",
        "region",
        "province",
        "district",
        "total_flws",
        "vaccinator",
        "social_mobilizer",
        "cluster_supervisor",
        "district_coordinator",
        "district_support",
        "monitor",
        "vaccinators_per_1000",
        "social_mobilizers_per_1000",
        "flws_pct_newly_selected",
        "flws_pct_female",
        "flws_pct_resident",
        "pct_paid_among_not_new"
      )
    ))
  assign(i, df)
}

precampaign_indicators <- list(
  precampaign_flw_district = total_flws_district,
  precampaign_flw_province = total_flws_province,
  precampaign_flw_region = total_flws_region,
  precampaign_flw_national = total_flws_national,
  training_district = training_district,
  training_province = training_province,
  training_region = training_region,
  training_national = training_national
)

# Remove all except precampaign_indicators
rm(
  flw_data,
  total_flws_district,
  total_flws_province,
  total_flws_region,
  total_flws_national,
  training_district,
  training_province,
  training_region,
  training_national,
  training_data_vol_sm,
  training_data_dc,
  training_data_cluster,
  summarise_training_data
)

# Clean up unused memory
gc()
