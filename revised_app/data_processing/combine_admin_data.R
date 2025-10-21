# admin_all <- admin_h2h %>%
#   mutate(age_group = "0-59 Months",
#          max_days = 4,
#          modality = "H2H") %>%
#   bind_rows(admin_m2m %>%
#               mutate(age_group = "0-59 Months",
#                      max_days = 3,
#                      modality = "M2M")) %>%
#   bind_rows(admin_s2s %>%
#               mutate(age_group = "0-59 Months",
#                      max_days = case_when(grepl("1-4", formname) ~ 4,
#                                           grepl("1-7", formname) ~ 7,
#                                           TRUE ~ 3),
#                      # max_days = 3,
#                      modality = "S2S")) %>%
#   bind_rows(admin_7day_0_5yr %>%
#               mutate(age_group = "0-59 Months",
#                      max_days = 7,
#                      modality = "H2H")) %>%
#   bind_rows(admin_7day_5_10yr %>%
#               mutate(age_group = "5-10 Years",
#                      max_days = 7,
#                      modality = "H2H")) %>%
#   bind_rows(ipv_7day %>%
#               mutate(age_group = "4-59 Months",
#                      max_days = 7,
#                      modality = "S2S",
#                      vaccine_type = "IPV")) %>%
#   bind_rows(opv_7day %>%
#               mutate(age_group = "0-59 Months",
#                      max_days = 7,
#                      modality = "S2S",
#                      vaccine_type = "OPV")) %>%
#   mutate(vaccine_type = ifelse(is.na(vaccine_type), "OPV", vaccine_type))

# Step 1: Create the list of all component datasets before combining
admin_list <- list(
  admin_h2h %>% mutate(age_group = "0-59 Months", max_days = 4, modality = "H2H"),
  admin_m2m %>% mutate(age_group = "0-59 Months", max_days = 3, modality = "M2M"),
  admin_s2s %>% mutate(
    age_group = "0-59 Months",
    max_days = case_when(
      grepl("1-4", formname) ~ 4,
      grepl("1-7", formname) ~ 7,
      TRUE ~ 3
    ),
    modality = "S2S"
  ),
  admin_7day_0_5yr %>% mutate(age_group = "0-59 Months", max_days = 7, modality = "H2H"),
  admin_7day_5_10yr %>% mutate(age_group = "5-10 Years", max_days = 7, modality = "H2H"),
  ipv_7day %>% mutate(age_group = "4-59 Months", max_days = 7, modality = "S2S", vaccine_type = "IPV"),
  opv_7day %>% mutate(age_group = "0-59 Months", max_days = 7, modality = "S2S", vaccine_type = "OPV")
)

# Step 2: Identify column types across all dataframes
col_types <- map(admin_list, function(df) map_chr(df, ~ class(.x)[1]))
all_cols <- unique(unlist(map(admin_list, names)))

type_summary <- map(all_cols, function(col) {
  types <- map_chr(admin_list, ~ if (col %in% names(.x)) class(.x[[col]])[1] else NA_character_)
  unique(na.omit(types))
})
names(type_summary) <- all_cols

# Step 3: Determine target type for each column
target_type <- map_chr(type_summary, function(types) {
  if (length(types) == 1) return(types)
  if (any(types %in% c("numeric", "integer", "double"))) return("numeric")
  if (any(types %in% c("logical", "character"))) return("character")
  "character"
})

# Step 4: Harmonize types before combining
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

# Step 5: Apply harmonization and combine
admin_all <- admin_list %>%
  map(harmonize_types) %>%
  bind_rows() %>%
  mutate(vaccine_type = ifelse(is.na(vaccine_type), "OPV", vaccine_type))

