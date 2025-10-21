admin_h2h <- df_apmis_list$`DC Daily Compilation Day 1-4 H2H` %>% 
  select(
    campaigns, region, province, district, clustername, rcode, pcode, dcode, ccode,
    visit_date_day1_fmt, visit_date_day2_fmt, visit_date_day3_fmt, visit_date_day4_fmt,
    total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, 
    no_f_ull_vials_received_day1, no_f_ull_vials_received_day2, no_f_ull_vials_received_day3, no_f_ull_vials_received_day4,
    no_f_ull_vials_used_day1, no_f_ull_vials_used_day2, no_f_ull_vials_used_day3, no_f_ull_vials_used_day4, 
    remaining_absent_day4gt, remaining_absent_after_c_day4gt, remaining_nss_day4gt, remaining_refusal_day4gt,
    remaining_absent_day1, remaining_absent_day2, remaining_absent_day3, remaining_absent_day4,
    number_remaining_absent_day1, number_remaining_absent_day2, number_remaining_absent_day3, number_remaining_absent_day4,
    remaining_nss_day1, remaining_nss_day2, remaining_nss_day3, remaining_nss_day4,
    remaining_refusal_day1, remaining_refusal_day2, remaining_refusal_day3, remaining_refusal_day4
  ) %>%
  mutate(
    total_vaccinated = rowSums(select(., starts_with("total_children_vaccinated")), na.rm = TRUE),
    opv_vials_used = rowSums(select(., starts_with("no_f_ull_vials_used")), na.rm = TRUE),
    opv_doses_used = opv_vials_used * 20,
    vacc_wastage = ifelse(opv_doses_used >= 0, (opv_doses_used - total_vaccinated) / opv_doses_used, NA),
    recorded_missed_absent = rowSums(select(., starts_with("remaining_absent_day4gt"), starts_with("remaining_absent_after_c_day4gt")), na.rm = TRUE),
    recorded_missed_nss = remaining_nss_day4gt,
    recorded_missed_refusal = remaining_refusal_day4gt,
    recorded_missed_total = rowSums(select(., starts_with("remaining_absent_day"), starts_with("remaining_nss_day"), starts_with("remaining_refusal_day")), na.rm = TRUE),
    remaining_missed_day1 = rowSums(select(., starts_with("remaining_absent_day1"), starts_with("remaining_nss_day1"), starts_with("remaining_refusal_day1")), na.rm = TRUE),
    remaining_missed_day2 = rowSums(select(., starts_with("remaining_absent_day2"), starts_with("remaining_nss_day2"), starts_with("remaining_refusal_day2")), na.rm = TRUE),
    remaining_missed_day3 = rowSums(select(., starts_with("remaining_absent_day3"), starts_with("remaining_nss_day3"), starts_with("remaining_refusal_day3")), na.rm = TRUE),
    remaining_missed_day4 = rowSums(select(., starts_with("remaining_absent_day4"), starts_with("remaining_nss_day4"), starts_with("remaining_refusal_day4")), na.rm = TRUE)
  ) %>%
  select(
    campaigns, region, province, district, clustername, rcode, pcode, dcode, ccode,
    visit_date_day1_fmt, visit_date_day2_fmt, visit_date_day3_fmt, visit_date_day4_fmt,
    total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, 
    total_vaccinated, opv_vials_used, opv_doses_used, vacc_wastage,
    recorded_missed_absent, recorded_missed_nss, recorded_missed_refusal, recorded_missed_total,
    remaining_absent_day1, remaining_absent_day2, remaining_absent_day3, remaining_absent_day4,
    number_remaining_absent_day1, number_remaining_absent_day2, number_remaining_absent_day3, number_remaining_absent_day4,
    remaining_nss_day1, remaining_nss_day2, remaining_nss_day3, remaining_nss_day4,
    remaining_refusal_day1, remaining_refusal_day2, remaining_refusal_day3, remaining_refusal_day4,
    remaining_missed_day1, remaining_missed_day2, remaining_missed_day3, remaining_missed_day4,
    no_f_ull_vials_received_day1, no_f_ull_vials_received_day2, no_f_ull_vials_received_day3, no_f_ull_vials_received_day4
  ) %>%
  mutate(across(
    c(
      total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, total_vaccinated, 
      opv_vials_used, opv_doses_used,
      recorded_missed_absent, recorded_missed_nss, recorded_missed_refusal, recorded_missed_total,
      remaining_absent_day1, remaining_absent_day2, remaining_absent_day3, remaining_absent_day4,
      number_remaining_absent_day1, number_remaining_absent_day2, number_remaining_absent_day3, number_remaining_absent_day4,
      remaining_nss_day1, remaining_nss_day2, remaining_nss_day3, remaining_nss_day4,
      remaining_refusal_day1, remaining_refusal_day2, remaining_refusal_day3, remaining_refusal_day4,
      remaining_missed_day1, remaining_missed_day2, remaining_missed_day3, remaining_missed_day4
    ), round, 0
  ))