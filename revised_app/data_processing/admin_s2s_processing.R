for(i in c("visit_date_day1", "visit_date_day2", "visit_date_day3", "visit_date_day4")){
  if(i %in% colnames(df_apmis_list$`South SIA DC Daily Compilation Day 1-4`)){
    df_apmis_list$`South SIA DC Daily Compilation Day 1-4` <- df_apmis_list$`South SIA DC Daily Compilation Day 1-4` %>%
      mutate_at(i, ~as.character(.))
  }
}
for(i in c("visit_date_day1", "visit_date_day2", "visit_date_day3", "visit_date_day4")){
  if(i %in% colnames(df_apmis_list$`East SIA DC Daily Compilation Day 1-4`)){
    df_apmis_list$`East SIA DC Daily Compilation Day 1-4` <- df_apmis_list$`East SIA DC Daily Compilation Day 1-4` %>%
      mutate_at(i, ~as.character(.))
  }
}

dfs <- list(
  df_apmis_list$`DC Daily Compilation Day 1-3`,
  df_apmis_list$`East SIA DC Daily Compilation Day 1-3`,
  df_apmis_list$`South SIA DC Daily Compilation Day 1-4`,
  df_apmis_list$`East SIA DC Daily Compilation Day 1-4`
)

# ---- Step 1: determine column classes across all dfs safely ----
col_types <- map(dfs, function(df) map_chr(df, ~ class(.x)[1]))
all_cols  <- unique(unlist(map(dfs, names)))

type_summary <- map(all_cols, function(col) {
  types <- map_chr(dfs, ~ if (col %in% names(.x)) class(.x[[col]])[1] else NA_character_)
  unique(na.omit(types))
})
names(type_summary) <- all_cols

# ---- Step 2: decide target coercion type for each column ----
target_type <- map_chr(type_summary, function(types) {
  if (length(types) == 1) return(types)
  # if any numeric appears, coerce to numeric
  if ("numeric" %in% types || "integer" %in% types || "double" %in% types) return("numeric")
  # if any logical + character, make character
  if (all(types %in% c("logical", "character"))) return("character")
  # default: character (safest)
  "character"
})

# ---- Step 3: harmonize each dataframe to those target types ----
harmonize_types <- function(df) {
  for (col in intersect(names(df), names(target_type))) {
    tgt <- target_type[[col]]
    if (tgt == "numeric" && !is.numeric(df[[col]]))
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    else if (tgt == "character" && !is.character(df[[col]]))
      df[[col]] <- as.character(df[[col]])
  }
  df
}

# ---- Step 4: apply harmonization + bind ----
admin_s2s <- dfs %>%
  map(harmonize_types) %>%
  bind_rows()

for(i in c("total_children_vaccinated_day1", "total_children_vaccinated_day2", "total_children_vaccinated_day3", "total_children_vaccinated_day4", "total_children_vaccinated_days1_4",
           "no_vials_distributed_day1", "no_vials_distributed_day2", "no_vials_distributed_day3", "no_vials_distributed_day4",
           "no_vials_used_day1", "no_vials_used_day2", "no_vials_used_day3", "no_vials_used_day4", "no_vials_used_days1_4",
           "missed_childrenfound011m_day1", "missed_childrenfound1259m_day1", "revisit_children_vaccinated011m_day1", "revisit_children_vaccinated1259m_day1", 
           "missed_childrenfound011m_day2", "missed_childrenfound1259m_day2", "revisit_children_vaccinated011m_day2", "revisit_children_vaccinated1259m_day2",
           "missed_childrenfound011m_day3", "missed_childrenfound1259m_day3", "revisit_children_vaccinated011m_day3", "revisit_children_vaccinated1259m_day3",
           "no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", "no_site_visited_day4", "no_site_visited_days1_4",
           "transit_vaccinated011m_day1", "transit_vaccinated011m_day2",     "transit_vaccinated011m_day3", "transit_vaccinated011m_day4",  "transit_vaccinated011m_days1_4", 
           "transit_vaccinated1259m_day1", "transit_vaccinated1259m_day2",    "transit_vaccinated1259m_day3", "transit_vaccinated1259m_day4",    "transit_vaccinated1259m_days1_3", "transit_vaccinated1259m_days1_4",
           "transit_team_day1",               "transit_team_day2",               "transit_team_day3",
           "hrmp_covered011m_day1", "hrmp_covered011m_day2", "hrmp_covered011m_day3", "hrmp_covered011m_day4", "hrmp_covered011m_day1_4",
           "hrmp_covered1259m_day1", "hrmp_covered1259m_day2", "hrmp_covered1259m_day3", "hrmp_covered1259m_day4", "hrmp_covered1259m_day1_4",
           "additional_children_vaccinated_day4"
)){
  if(!(i %in% colnames(admin_s2s))){
    admin_s2s <- admin_s2s %>%
      mutate({{i}} := NA_integer_)
  }
}
admin_s2s <- admin_s2s %>%
  select(
    formname, campaigns, region, province, district, clustername, rcode, pcode, dcode, ccode, 
    visit_date_day1_fmt, visit_date_day2_fmt, visit_date_day3_fmt, visit_date_day4_fmt,
    total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, 
    no_vials_used_day1, no_vials_used_day2, no_vials_used_day3, no_vials_used_day4,
    no_vials_distributed_day1, no_vials_distributed_day2, no_vials_distributed_day3, no_vials_distributed_day4,
    missed_childrenfound011m_day1, missed_childrenfound1259m_day1, revisit_children_vaccinated011m_day1, revisit_children_vaccinated1259m_day1, transit_team_day1,
    missed_childrenfound011m_day2, missed_childrenfound1259m_day2, revisit_children_vaccinated011m_day2, revisit_children_vaccinated1259m_day2, transit_team_day2,
    missed_childrenfound011m_day3, missed_childrenfound1259m_day3, revisit_children_vaccinated011m_day3, revisit_children_vaccinated1259m_day3, transit_team_day3,
    no_site_visited_day1, no_site_visited_day2, no_site_visited_day3, no_site_visited_day4,
    additional_children_vaccinated_day4,
    hrmp_covered011m_day1, hrmp_covered011m_day2, hrmp_covered011m_day3, hrmp_covered011m_day4,
    hrmp_covered1259m_day1, hrmp_covered1259m_day2, hrmp_covered1259m_day3, hrmp_covered1259m_day4, 
    
  ) %>%
  rename(no_f_ull_vials_received_day1 = no_vials_distributed_day1,
         no_f_ull_vials_received_day2 = no_vials_distributed_day2,
         no_f_ull_vials_received_day3 = no_vials_distributed_day3,
         no_f_ull_vials_received_day4 = no_vials_distributed_day4) %>%
  mutate(
    total_sites_visited = rowSums(select(., starts_with("no_site_visited")), na.rm = TRUE),
    total_vaccinated = rowSums(select(., starts_with("total_children_vaccinated")), na.rm = TRUE),
    opv_vials_used = rowSums(select(., starts_with("no_vials_used")), na.rm = TRUE),
    opv_doses_used = opv_vials_used * 20,
    vacc_wastage = ifelse(opv_doses_used >= 0, (opv_doses_used - total_vaccinated) / opv_doses_used, NA),
    hrmp_vaccinated = ifelse(
      rowSums(!is.na(select(., starts_with("hrmp_covered")))) == 0,
      NA,
      rowSums(select(., starts_with("hrmp_covered")), na.rm = TRUE)
    ),
    pct_hrmp = ifelse(!is.na(hrmp_vaccinated), hrmp_vaccinated / total_vaccinated, NA)
  ) %>%
  select(
    formname, campaigns, region, province, district, clustername, rcode, pcode, dcode, ccode, 
    visit_date_day1_fmt, visit_date_day2_fmt, visit_date_day3_fmt,
    total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, total_vaccinated, 
    opv_vials_used, opv_doses_used, vacc_wastage, no_site_visited_day1, no_site_visited_day2, no_site_visited_day3, no_site_visited_day4, total_sites_visited,
    no_f_ull_vials_received_day1, no_f_ull_vials_received_day2, no_f_ull_vials_received_day3, no_f_ull_vials_received_day4, hrmp_vaccinated, pct_hrmp
  ) %>%
  mutate(
    across(
      c(
        "total_children_vaccinated_day1", "total_children_vaccinated_day2", "total_children_vaccinated_day3", "total_children_vaccinated_day4", "total_vaccinated", 
        "opv_vials_used", "opv_doses_used", 
        "no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3", "no_site_visited_day4", "total_sites_visited", "hrmp_vaccinated"
      ), 
      round, 0
    )
  ) 

