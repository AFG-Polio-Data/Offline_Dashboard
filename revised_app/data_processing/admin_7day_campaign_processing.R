admin_7day_5_10yr <- df_apmis_list$`DC Daily Compilation Day 1-7 (5-10)` %>%
  select(
    campaigns, region, province, district, clustername, rcode, pcode, dcode, ccode, 
    visit_date_day1_fmt, visit_date_day2_fmt, visit_date_day3_fmt, visit_date_day4_fmt, visit_date_day5_fmt, visit_date_day6_fmt, visit_date_day7_fmt,
    total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, total_children_vaccinated_day5, total_children_vaccinated_day6, total_children_vaccinated_day7, 
    no_f_ull_vials_used_day1, no_f_ull_vials_used_day2, no_f_ull_vials_used_day3, no_f_ull_vials_used_day4, no_f_ull_vials_used_day5, no_f_ull_vials_used_day6, no_f_ull_vials_used_day7,
    no_f_ull_vials_received_day1, no_f_ull_vials_received_day2, no_f_ull_vials_received_day3, no_f_ull_vials_received_day4, no_f_ull_vials_received_day5, no_f_ull_vials_received_day6, no_f_ull_vials_received_day7,
    remaining_absent_day7, remaining_nss_day7, remaining_refusal_day7,
    remaining_absent_day1, remaining_absent_day2, remaining_absent_day3, remaining_absent_day4, remaining_absent_day5, remaining_absent_day6, remaining_absent_day7,
    number_remaining_absent_day1, number_remaining_absent_day2, number_remaining_absent_day3, number_remaining_absent_day4, number_remaining_absent_day5, number_remaining_absent_day6, number_remaining_absent_day7,
    remaining_nss_day1, remaining_nss_day2, remaining_nss_day3, remaining_nss_day4, remaining_nss_day5, remaining_nss_day6, remaining_nss_day7,
    remaining_refusal_day1, remaining_refusal_day2, remaining_refusal_day3, remaining_refusal_day4, remaining_refusal_day5, remaining_refusal_day6, remaining_refusal_day7
  ) %>%
  mutate(
    total_vaccinated = rowSums(select(., starts_with("total_children_vaccinated")), na.rm = TRUE),
    opv_vials_used = rowSums(select(., starts_with("no_f_ull_vials_used")), na.rm = TRUE),
    opv_doses_used = opv_vials_used * 20,
    vacc_wastage = ifelse(opv_doses_used >= 0, (opv_doses_used - total_vaccinated) / opv_doses_used, NA),
    recorded_missed_absent = remaining_absent_day7,
    recorded_missed_nss = remaining_nss_day7,
    recorded_missed_refusal = remaining_refusal_day7)

admin_7day_5_10yr <- admin_7day_5_10yr %>%
  mutate(
    recorded_missed_total = rowSums(select(., recorded_missed_absent, recorded_missed_nss, recorded_missed_refusal), na.rm = TRUE),
    remaining_missed_day1 = rowSums(select(., remaining_absent_day1, number_remaining_absent_day1, remaining_nss_day1, remaining_refusal_day1), na.rm = TRUE),
    remaining_missed_day2 = rowSums(select(., remaining_absent_day2, number_remaining_absent_day2, remaining_nss_day2, remaining_refusal_day2), na.rm = TRUE),
    remaining_missed_day3 = rowSums(select(., remaining_absent_day3, number_remaining_absent_day3, remaining_nss_day3, remaining_refusal_day3), na.rm = TRUE),
    remaining_missed_day4 = rowSums(select(., remaining_absent_day4, number_remaining_absent_day4, remaining_nss_day4, remaining_refusal_day4), na.rm = TRUE),
    remaining_missed_day5 = rowSums(select(., remaining_absent_day5, number_remaining_absent_day5, remaining_nss_day5, remaining_refusal_day5), na.rm = TRUE),
    remaining_missed_day6 = rowSums(select(., remaining_absent_day6, number_remaining_absent_day6, remaining_nss_day6, remaining_refusal_day6), na.rm = TRUE),
    remaining_missed_day7 = rowSums(select(., remaining_absent_day7, number_remaining_absent_day7, remaining_nss_day7, remaining_refusal_day7), na.rm = TRUE)
  ) %>%
  select(
    campaigns, region, province, district, clustername, rcode, pcode, dcode, ccode, visit_date_day1_fmt, visit_date_day2_fmt, visit_date_day3_fmt, visit_date_day4_fmt, visit_date_day5_fmt, visit_date_day6_fmt, visit_date_day7_fmt,
    total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, total_children_vaccinated_day5, total_children_vaccinated_day6, total_children_vaccinated_day7, total_vaccinated, 
    opv_vials_used, opv_doses_used, vacc_wastage,
    recorded_missed_absent, recorded_missed_nss, recorded_missed_refusal, recorded_missed_total,
    remaining_absent_day1, remaining_absent_day2, remaining_absent_day3, remaining_absent_day4, remaining_absent_day5, remaining_absent_day6, remaining_absent_day7,
    number_remaining_absent_day1, number_remaining_absent_day2, number_remaining_absent_day3, number_remaining_absent_day4, number_remaining_absent_day5, number_remaining_absent_day6, number_remaining_absent_day7,
    remaining_nss_day1, remaining_nss_day2, remaining_nss_day3, remaining_nss_day4, remaining_nss_day5, remaining_nss_day6, remaining_nss_day7,
    remaining_refusal_day1, remaining_refusal_day2, remaining_refusal_day3, remaining_refusal_day4, remaining_refusal_day5, remaining_refusal_day6, remaining_refusal_day7,
    remaining_missed_day1, remaining_missed_day2, remaining_missed_day3, remaining_missed_day4, remaining_missed_day5, remaining_missed_day6, remaining_missed_day7,
    no_f_ull_vials_received_day1, no_f_ull_vials_received_day2, no_f_ull_vials_received_day3, no_f_ull_vials_received_day4, no_f_ull_vials_received_day5, no_f_ull_vials_received_day6, no_f_ull_vials_received_day7
  ) %>%
  mutate(
    across(
      c(
        "total_children_vaccinated_day1", "total_children_vaccinated_day2", "total_children_vaccinated_day3", "total_children_vaccinated_day4", 
        "total_children_vaccinated_day5", "total_children_vaccinated_day6", "total_children_vaccinated_day7", "total_vaccinated", 
        "opv_vials_used", "opv_doses_used",
        "recorded_missed_absent", "recorded_missed_nss", "recorded_missed_refusal", "recorded_missed_total",
        "remaining_absent_day1", "remaining_absent_day2", "remaining_absent_day3", "remaining_absent_day4", "remaining_absent_day5", "remaining_absent_day6", "remaining_absent_day7",
        "number_remaining_absent_day1", "number_remaining_absent_day2", "number_remaining_absent_day3", "number_remaining_absent_day4", "number_remaining_absent_day5", "number_remaining_absent_day6", "number_remaining_absent_day7",
        "remaining_nss_day1", "remaining_nss_day2", "remaining_nss_day3", "remaining_nss_day4", "remaining_nss_day5", "remaining_nss_day6", "remaining_nss_day7",
        "remaining_refusal_day1", "remaining_refusal_day2", "remaining_refusal_day3", "remaining_refusal_day4", "remaining_refusal_day5", "remaining_refusal_day6", "remaining_refusal_day7",
        "remaining_missed_day1", "remaining_missed_day2", "remaining_missed_day3", "remaining_missed_day4", "remaining_missed_day5", "remaining_missed_day6", "remaining_missed_day7"
      ), 
      round, 0
    )
  ) 




admin_7day_0_5yr <- df_apmis_list$`DC Daily Compilation Day 1-7 (0-5)` %>% 
  select(campaigns, region, province, district, clustername, rcode, pcode, dcode, ccode, 
         visit_date_day1_fmt, visit_date_day2_fmt, visit_date_day3_fmt, visit_date_day4_fmt, visit_date_day5_fmt, visit_date_day6_fmt, visit_date_day7_fmt,
         total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, total_children_vaccinated_day5, total_children_vaccinated_day6, total_children_vaccinated_day7, 
         no_f_ull_vials_used_day1, no_f_ull_vials_used_day2, no_f_ull_vials_used_day3, no_f_ull_vials_used_day4, no_f_ull_vials_used_day5, no_f_ull_vials_used_day6, no_f_ull_vials_used_day7,
         no_f_ull_vials_received_day1, no_f_ull_vials_received_day2, no_f_ull_vials_received_day3, no_f_ull_vials_received_day4, no_f_ull_vials_received_day5, no_f_ull_vials_received_day6, no_f_ull_vials_received_day7,
         remaining_absent_day7, remaining_nss_day7, remaining_refusal_day7,
         remaining_absent_day1, remaining_absent_day2, remaining_absent_day3, remaining_absent_day4, remaining_absent_day5, remaining_absent_day6, remaining_absent_day7,
         number_remaining_absent_day1, number_remaining_absent_day2, number_remaining_absent_day3, number_remaining_absent_day4, number_remaining_absent_day5, number_remaining_absent_day6, number_remaining_absent_day7,
         remaining_nss_day1, remaining_nss_day2, remaining_nss_day3, remaining_nss_day4, remaining_nss_day5, remaining_nss_day6, remaining_nss_day7,
         remaining_refusal_day1, remaining_refusal_day2, remaining_refusal_day3, remaining_refusal_day4, remaining_refusal_day5, remaining_refusal_day6, remaining_refusal_day7) %>%
  rowwise() %>%
  mutate(total_vaccinated = sum(total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, total_children_vaccinated_day5, total_children_vaccinated_day6, total_children_vaccinated_day7, na.rm=T),
         opv_vials_used = sum(no_f_ull_vials_used_day1, no_f_ull_vials_used_day2, no_f_ull_vials_used_day3, no_f_ull_vials_used_day4, no_f_ull_vials_used_day5, no_f_ull_vials_used_day6, no_f_ull_vials_used_day7, na.rm=T),
         opv_doses_used = opv_vials_used*20,
         vacc_wastage = ifelse(opv_doses_used >= 0, (opv_doses_used - total_vaccinated)/(opv_doses_used), NA),
         recorded_missed_absent = remaining_absent_day7,
         recorded_missed_nss = remaining_nss_day7,
         recorded_missed_refusal = remaining_refusal_day7,
         recorded_missed_total = sum(recorded_missed_absent, recorded_missed_nss, recorded_missed_refusal, na.rm=T),
         remaining_missed_day1 = sum(remaining_absent_day1, number_remaining_absent_day1, remaining_nss_day1, remaining_refusal_day1, na.rm=T),
         remaining_missed_day2 = sum(remaining_absent_day2, number_remaining_absent_day2, remaining_nss_day2, remaining_refusal_day2, na.rm=T),
         remaining_missed_day3 = sum(remaining_absent_day3, number_remaining_absent_day3, remaining_nss_day3, remaining_refusal_day3, na.rm=T),
         remaining_missed_day4 = sum(remaining_absent_day4, number_remaining_absent_day4, remaining_nss_day4, remaining_refusal_day4, na.rm=T),
         remaining_missed_day5 = sum(remaining_absent_day5, number_remaining_absent_day5, remaining_nss_day5, remaining_refusal_day5, na.rm=T),
         remaining_missed_day6 = sum(remaining_absent_day6, number_remaining_absent_day6, remaining_nss_day6, remaining_refusal_day6, na.rm=T),
         remaining_missed_day7 = sum(remaining_absent_day7, number_remaining_absent_day7, remaining_nss_day7, remaining_refusal_day7, na.rm=T)) %>%
  ungroup() %>%
  select(campaigns, region, province, district, clustername, rcode, pcode, dcode, ccode, visit_date_day1_fmt, visit_date_day2_fmt, visit_date_day3_fmt, visit_date_day4_fmt, visit_date_day5_fmt, visit_date_day6_fmt, visit_date_day7_fmt,
         total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, total_children_vaccinated_day5, total_children_vaccinated_day6, total_children_vaccinated_day7, total_vaccinated, 
         opv_vials_used, opv_doses_used, vacc_wastage,
         recorded_missed_absent, recorded_missed_nss, recorded_missed_refusal, recorded_missed_total,
         remaining_absent_day1, remaining_absent_day2, remaining_absent_day3, remaining_absent_day4, remaining_absent_day5, remaining_absent_day6, remaining_absent_day7,
         number_remaining_absent_day1, number_remaining_absent_day2, number_remaining_absent_day3, number_remaining_absent_day4, number_remaining_absent_day5, number_remaining_absent_day6, number_remaining_absent_day7,
         remaining_nss_day1, remaining_nss_day2, remaining_nss_day3, remaining_nss_day4, remaining_nss_day5, remaining_nss_day6, remaining_nss_day7,
         remaining_refusal_day1, remaining_refusal_day2, remaining_refusal_day3, remaining_refusal_day4, remaining_refusal_day5, remaining_refusal_day6, remaining_refusal_day7,
         remaining_missed_day1, remaining_missed_day2, remaining_missed_day3, remaining_missed_day4, remaining_missed_day5, remaining_missed_day6, remaining_missed_day7,
         no_f_ull_vials_received_day1, no_f_ull_vials_received_day2, no_f_ull_vials_received_day3, no_f_ull_vials_received_day4, no_f_ull_vials_received_day5, no_f_ull_vials_received_day6, no_f_ull_vials_received_day7
  ) %>%
  mutate(across(
    c("total_children_vaccinated_day1", "total_children_vaccinated_day2", "total_children_vaccinated_day3", "total_children_vaccinated_day4", 
      "total_children_vaccinated_day5", "total_children_vaccinated_day6", "total_children_vaccinated_day7", "total_vaccinated", 
      "opv_vials_used", "opv_doses_used",
      "recorded_missed_absent", "recorded_missed_nss", "recorded_missed_refusal", "recorded_missed_total",
      "remaining_absent_day1", "remaining_absent_day2", "remaining_absent_day3", "remaining_absent_day4", "remaining_absent_day5", "remaining_absent_day6", "remaining_absent_day7",
      "number_remaining_absent_day1", "number_remaining_absent_day2", "number_remaining_absent_day3", "number_remaining_absent_day4", "number_remaining_absent_day5", "number_remaining_absent_day6", "number_remaining_absent_day7",
      "remaining_nss_day1", "remaining_nss_day2", "remaining_nss_day3", "remaining_nss_day4", "remaining_nss_day5", "remaining_nss_day6", "remaining_nss_day7",
      "remaining_refusal_day1", "remaining_refusal_day2", "remaining_refusal_day3", "remaining_refusal_day4", "remaining_refusal_day5", "remaining_refusal_day6", "remaining_refusal_day7",
      "remaining_missed_day1", "remaining_missed_day2", "remaining_missed_day3", "remaining_missed_day4", "remaining_missed_day5", "remaining_missed_day6", "remaining_missed_day7"), 
    round, 0))
