icm_household_h2h <- df_apmis_list$`ICM Household Monitoring H2H` %>%
  mutate(age_group = "0-59 Months",
         newborn_sick_sleep = as.numeric(newborn_sick_sleep),
         team_no = as.numeric(team_no)) %>%
  bind_rows(df_apmis_list$`ICM Household Monitoring (5-10)` %>%
              mutate(age_group = "5-10 Years")) %>%
  select(
    rcode,
    pcode,
    dcode,
    ccode,
    region,
    province,
    district,
    clustername,
    campaigns,
    visit_date_fmt,
    campaign_day,
    age_group,
    number_of_houses_visited,
    children_living_in_houses,
    children_vaccinated_recall,
    missed_children,
    team_not_come,
    child_absent,
    newborn_sick_sleep,
    refusal,
    poor_screening,
    children_seen_by_monitor,
    children_finger_marking,
    door_marking_correct,
    door_marking_incorrect,
    door_marking_not_marked,
    missed_children_recall_reasons,
    missed_children_recall_tally,
    poorly_covered_area,
    missed_area
  ) %>%
  mutate(across(
    where( ~ all(
      . %in% c("yes", "no", "na", "TRUE", "FALSE", NA)
    )),
    ~ case_when(
      . == "yes" ~ 1,
      . == "no" ~ 0,
      . == "TRUE" ~ 1,
      . == "FALSE" ~ 0,
      . == "na" ~ NA,
      . == NA ~ NA
    )
  )) %>%
  mutate(form_type = "Household Monitoring")

icm_household_h2h_pct <- icm_household_h2h %>%
  select(
    rcode,
    pcode,
    dcode,
    ccode,
    region,
    province,
    district,
    clustername,
    form_type,
    age_group,
    campaigns,
    visit_date_fmt,
    campaign_day,
    age_group,
    poorly_covered_area,
    missed_area
  ) %>%
  rename(household_poorly_covered_area = poorly_covered_area,
         household_missed_area = missed_area)

icm_household_h2h_numeric <- icm_household_h2h %>%
  select(-c("poorly_covered_area", "missed_area"))
