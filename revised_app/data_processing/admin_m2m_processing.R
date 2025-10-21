admin_m2m <- df_apmis_list$`DC Daily Compilation Day 1-3 M2M` %>%
  select(
    campaigns, region, province, district, clustername, rcode, pcode, dcode, ccode, 
    visit_date_day1_fmt, visit_date_day2_fmt, visit_date_day3_fmt,
    total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3,
    no_vials_used_day1, no_vials_used_day2, no_vials_used_day3,
    no_vials_distributed_day1, no_vials_distributed_day2, no_vials_distributed_day3
  ) %>%
  rename(no_f_ull_vials_received_day1 = no_vials_distributed_day1,
         no_f_ull_vials_received_day2 = no_vials_distributed_day2,
         no_f_ull_vials_received_day3 = no_vials_distributed_day3) %>%
  mutate(
    total_vaccinated = rowSums(select(., starts_with("total_children_vaccinated")), na.rm = TRUE),
    opv_vials_used = rowSums(select(., starts_with("no_vials_used")), na.rm = TRUE),
    opv_doses_used = opv_vials_used * 20,
    vacc_wastage = ifelse(opv_doses_used >= 0, (opv_doses_used - total_vaccinated) / opv_doses_used, NA)
  ) %>%
  select(
    campaigns, region, province, district, clustername, rcode, pcode, dcode, ccode, 
    visit_date_day1_fmt, visit_date_day2_fmt, visit_date_day3_fmt,
    total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_vaccinated, 
    opv_vials_used, opv_doses_used, vacc_wastage,
    no_f_ull_vials_received_day1, no_f_ull_vials_received_day2, no_f_ull_vials_received_day3
  ) %>%
  mutate(
    across(
      c("total_children_vaccinated_day1", "total_children_vaccinated_day2", "total_children_vaccinated_day3", "total_vaccinated", 
        "opv_vials_used", "opv_doses_used"), 
      round, 0
    )
  )