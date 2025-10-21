# modules/admin_table_module.R

adminTableUI <- function(id) {
  ns <- NS(id)
  DT::DTOutput(ns("admin_table"))
}

adminTableServer <- function(id, admin_filtered_sia_data_age_filtered, 
                             reactive_admin_indicator,  
                             reactive_zoom_region, reactive_zoom_province, reactive_zoom_district, reactive_vaccine_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


admin_table_data <- reactive({
  req(admin_filtered_sia_data_age_filtered())
  req(reactive_zoom_region())
  req(reactive_zoom_province())
  req(reactive_zoom_district())
  req(reactive_admin_indicator())
  req(reactive_vaccine_type())
  
  admin_var <- reactive_admin_indicator()
  
  withProgress(message = 'Calculation in progress...',
               value = 0, {
                 if(!("All" %in% reactive_zoom_district())){
                   if(admin_var %in% c("Remaining Recorded Missed", "Missed Child Conversion")){
                     data <- admin_filtered_sia_data_age_filtered()$conversion_cluster
                   } else{
                     data <- admin_filtered_sia_data_age_filtered()$cluster 
                     data2 <- admin_filtered_sia_data_age_filtered()$cluster_completeness %>%
                       select(region, province, district, clustername, ccode) %>%
                       rename(clustername2 = clustername) %>%
                       distinct()
                     data <- data %>%
                       full_join(data2, by=c("region", "province", "district", "ccode")) %>%
                       mutate(clustername = ifelse(is.na(clustername), clustername2, clustername)) %>%
                       select(-c("clustername2"))
                   }
                   data <- data %>%
                     rename(Region = region,
                            Province = province,
                            District = district,
                            Cluster = clustername,
                            `Cluster ID` = ccode) %>%
                     ungroup() %>%
                     distinct()
                 } else{
                   if(!("All" %in% reactive_zoom_province())){
                     if(admin_var %in% c("Remaining Recorded Missed", "Missed Child Conversion")){
                       data <- admin_filtered_sia_data_age_filtered()$conversion_district
                     } else{
                       data <- admin_filtered_sia_data_age_filtered()$district
                       data2 <- admin_filtered_sia_data_age_filtered()$district_completeness %>%
                         mutate(completeness = paste0(round(total_pct_reported,2)*100,"% (",reporting_complete,"/",total,")")) %>%
                         select(region, province, district, completeness)
                       data <- data %>%
                         full_join(data2, by=c("region", "province", "district"))
                       
                     }
                     data <- data %>%
                       rename(Region = region,
                              Province = province,
                              District = district) %>%
                       ungroup()
                   } else{
                     if(!("All" %in% reactive_zoom_region())){
                       if(admin_var %in% c("Remaining Recorded Missed", "Missed Child Conversion")){
                         data <- admin_filtered_sia_data_age_filtered()$conversion_province
                       }else{
                         data <- admin_filtered_sia_data_age_filtered()$province
                         data2 <- admin_filtered_sia_data_age_filtered()$province_completeness %>%
                           mutate(completeness = paste0(round(total_pct_reported,2)*100,"% (",reporting_complete,"/",total,")")) %>%
                           select(region, province, completeness)
                         data <- data %>%
                           full_join(data2, by=c("region", "province"))
                       }
                       data <- data %>%
                         rename(Region = region,
                                Province = province) %>%
                         ungroup()
                     } else{
                       if(admin_var %in%  c("Remaining Recorded Missed", "Missed Child Conversion")){
                         data <- admin_filtered_sia_data_age_filtered()$conversion_region
                       }else{
                         data <- admin_filtered_sia_data_age_filtered()$region
                         data2 <- admin_filtered_sia_data_age_filtered()$region_completeness %>%
                           mutate(completeness = paste0(round(total_pct_reported,2)*100,"% (",reporting_complete,"/",total,")")) %>%
                           select(region, completeness)
                         data <- data %>%
                           full_join(data2, by=c("region"))
                       }
                       data <- data %>%
                         rename(Region = region) %>%
                         ungroup()
                     }}}
                 incProgress(1/1)
               }) #End Progress
  return(data)
})

admin_table_group_vars <- reactive({
  req(admin_table_data())
  req(reactive_zoom_district())
  req(reactive_zoom_province()) 
  
  if(!("All" %in% reactive_zoom_district())){
    group_vars <- c("Region", "Province", "District", "Cluster ID", "Cluster")
  } else{
    if(!("All" %in% reactive_zoom_province())){
      group_vars <- c("Region", "Province", "District")
    } else{
      if(!("All" %in% reactive_zoom_region())){
        group_vars <- c("Region", "Province")
      } else{
        group_vars <- c("Region")
      }
    }
  }
  return(group_vars)
})

admin_table_df <- reactive({
  req(admin_table_group_vars())
  req(admin_table_data())
  req(reactive_zoom_district())
  req(reactive_zoom_province()) 
  req(reactive_admin_indicator())
  
  admin_var <- reactive_admin_indicator()
  
  withProgress(message = 'Calculation in progress...',
               value = 0, {
                 
                 group_vars <- admin_table_group_vars()
                 data <- admin_table_data()
                 if(!(admin_var %in% c("Remaining Recorded Missed", "Missed Child Conversion"))){
                   if("Cluster" %in% group_vars){
                     data <- data %>%
                       select(all_of(group_vars), 
                              modality,
                              total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, total_children_vaccinated_day5, total_children_vaccinated_day6, total_children_vaccinated_day7, total_vaccinated, 
                              opv_vials_used, opv_doses_used, vacc_wastage, 
                              recorded_missed_absent, recorded_missed_nss, recorded_missed_refusal, recorded_missed_total, total_sites_visited, site_density, hrmp_vaccinated, pct_hrmp)
                   } else{
                     data <- data %>%
                       mutate(site_density = round(site_density,0)) %>%
                       select(all_of(group_vars), 
                              modality, target_population,
                              total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, total_children_vaccinated_day5, total_children_vaccinated_day6, total_children_vaccinated_day7, total_vaccinated, 
                              total_coverage, completeness,
                              opv_vials_used, opv_doses_used, vacc_wastage, 
                              recorded_missed_absent, recorded_missed_nss, recorded_missed_refusal, recorded_missed_total, total_sites_visited, site_density, hrmp_vaccinated, pct_hrmp)
                   }
                   data <- data %>%
                     rename(Modality = modality,
                            `Vaccinated - Day 1` = total_children_vaccinated_day1,
                            `Vaccinated - Day 2` = total_children_vaccinated_day2,
                            `Vaccinated - Day 3` = total_children_vaccinated_day3,
                            `Vaccinated - Day 4` = total_children_vaccinated_day4,
                            `Vaccinated - Day 5` = total_children_vaccinated_day5,
                            `Vaccinated - Day 6` = total_children_vaccinated_day6,
                            `Vaccinated - Day 7` = total_children_vaccinated_day7,
                            `Total Vaccinated` = total_vaccinated,
                            `Vials Used` = opv_vials_used,
                            `Doses Used` = opv_doses_used,
                            `Vaccine Wastage (%)` = vacc_wastage,
                            `(H2H only) Recorded Missed after Campaign: Absent` = recorded_missed_absent,
                            `(H2H only) Recorded Missed after Campaign: Newborn/Sleep/Sick` = recorded_missed_nss,
                            `(H2H only) Recorded Missed after Campaign: Refusal` = recorded_missed_refusal,
                            `(H2H only) Recorded Missed after Campaign` = recorded_missed_total,
                            `(S2S only) Total Sites Visited` = total_sites_visited,
                            `(S2S only) Avg. Vaccinated per Site` = site_density,
                            `HRMP Vaccinated` = hrmp_vaccinated, 
                            `% HRMP, of Vaccinated` = pct_hrmp)
                   if(!("Cluster" %in% group_vars)){
                     data <- data %>%
                       rename(`Reporting Completeness - % of Clusters Fully Reported` = completeness,
                              `Target Population` = target_population,
                              `Vaccine Coverage (%)` = total_coverage)
                     
                   }
                   
                   data <- data %>%
                     select(-c("Doses Used", "(H2H only) Recorded Missed after Campaign: Absent",
                               "(H2H only) Recorded Missed after Campaign: Newborn/Sleep/Sick",
                               "(H2H only) Recorded Missed after Campaign: Refusal"))
                 }
                 if(admin_var == "Remaining Recorded Missed"){
                   data <- data %>%
                     select(all_of(group_vars), 
                            recorded_missed_reason_absent1, recorded_missed_reason_absent1_vaccinated,
                            recorded_missed_reason_absent2, recorded_missed_reason_absent2_vaccinated,
                            recorded_missed_reason_nss, recorded_missed_reason_nss_vaccinated,
                            recorded_missed_reason_refusal, recorded_missed_reason_refusal_vaccinated, 
                            recorded_missed_total, recorded_missed_total_vaccinated) %>%
                     mutate(remaining_missed_reason_absent1 = recorded_missed_reason_absent1 - recorded_missed_reason_absent1_vaccinated, 
                            remaining_missed_reason_absent2 = recorded_missed_reason_absent2 - recorded_missed_reason_absent2_vaccinated, 
                            remaining_missed_reason_nss = recorded_missed_reason_nss - recorded_missed_reason_nss_vaccinated, 
                            remaining_missed_reason_refusal = recorded_missed_reason_refusal - recorded_missed_reason_refusal_vaccinated, 
                            remaining_missed_total = recorded_missed_total - recorded_missed_total_vaccinated) %>%
                     select(all_of(group_vars),
                            remaining_missed_reason_absent1, remaining_missed_reason_absent2, remaining_missed_reason_nss, remaining_missed_reason_refusal,
                            remaining_missed_total) %>%
                     rename(`Remaining Absent (Return during Campaign)` = remaining_missed_reason_absent1,
                            `Remaining Absent (Return after Campaign)` = remaining_missed_reason_absent2,
                            `Remaining Newborn/Sleep/Sick` = remaining_missed_reason_nss,
                            `Remaining Refusal` = remaining_missed_reason_refusal,
                            `Total Remaining Missed` = remaining_missed_total)
                 }
                 if(admin_var == "Missed Child Conversion"){
                   data <- data %>%
                     select(all_of(group_vars),
                            recorded_missed_reason_absent1, recorded_missed_reason_absent1_vaccinated, recorded_missed_reason_absent1_conversion_pct,
                            recorded_missed_reason_absent2, recorded_missed_reason_absent2_vaccinated, recorded_missed_reason_absent2_conversion_pct,
                            recorded_missed_reason_nss, recorded_missed_reason_nss_vaccinated, recorded_missed_reason_nss_conversion_pct,
                            recorded_missed_reason_refusal, recorded_missed_reason_refusal_vaccinated, recorded_missed_reason_refusal_conversion_pct,
                            # recorded_missed_day1, recorded_missed_day1_vaccinated, recorded_missed_day1_conversion_pct,
                            # recorded_missed_day2, recorded_missed_day2_vaccinated, recorded_missed_day2_conversion_pct,
                            # recorded_missed_day3, recorded_missed_day3_vaccinated, recorded_missed_day3_conversion_pct,
                            # recorded_missed_day4, recorded_missed_day4_vaccinated, recorded_missed_day4_conversion_pct,
                            # recorded_missed_day5, recorded_missed_day5_vaccinated, recorded_missed_day5_conversion_pct,
                            # recorded_missed_day6, recorded_missed_day6_vaccinated, recorded_missed_day6_conversion_pct,
                            recorded_missed_revisit_day_vaccinated,
                            recorded_missed_total, recorded_missed_total_vaccinated, recorded_missed_total_conversion_pct) %>%
                     ungroup() %>%
                     rename(`Return During Campaign: Recorded` = recorded_missed_reason_absent1,
                            `Return During Campaign: Vaccinated` = recorded_missed_reason_absent1_vaccinated,
                            `Return During Campaign: % Vaccinated` = recorded_missed_reason_absent1_conversion_pct,
                            `Return After Campaign: Recorded` = recorded_missed_reason_absent2,
                            `Return After Campaign: Vaccinated` = recorded_missed_reason_absent2_vaccinated,
                            `Return After Campaign: % Vaccinated` = recorded_missed_reason_absent2_conversion_pct,
                            `Newborn/ Sleep/Sick: Recorded` = recorded_missed_reason_nss,
                            `Newborn/ Sleep/Sick: Vaccinated` = recorded_missed_reason_nss_vaccinated,
                            `Newborn/ Sleep/Sick: % Vaccinated` = recorded_missed_reason_nss_conversion_pct,
                            `Refusal: Recorded` = recorded_missed_reason_refusal,
                            `Refusal: Vaccinated` = recorded_missed_reason_refusal_vaccinated,
                            `Refusal: % Vaccinated` = recorded_missed_reason_refusal_conversion_pct,
                            `Total Missed: Recorded` = recorded_missed_total,
                            `Total Missed: Vaccinated` = recorded_missed_total_vaccinated,
                            `Total Missed: % Vaccinated` = recorded_missed_total_conversion_pct,
                            # `Day 1: Recorded` = recorded_missed_day1,
                            # `Day 1: Vaccinated` = recorded_missed_day1_vaccinated,
                            # `Day 1: % Vaccinated` = recorded_missed_day1_conversion_pct,
                            # 
                            # `Day 2: Recorded` = recorded_missed_day2,
                            # `Day 2: Vaccinated` = recorded_missed_day2_vaccinated,
                            # `Day 2: % Vaccinated` = recorded_missed_day2_conversion_pct,
                            # 
                            # `Day 3: Recorded` = recorded_missed_day3,
                            # `Day 3: Vaccinated` = recorded_missed_day3_vaccinated,
                            # `Day 3: % Vaccinated` = recorded_missed_day3_conversion_pct,
                            # 
                            # `Day 4: Recorded` = recorded_missed_day4,
                            # `Day 4: Vaccinated` = recorded_missed_day4_vaccinated,
                            # `Day 4: % Vaccinated` = recorded_missed_day4_conversion_pct,
                            # 
                            # `Day 5: Recorded` = recorded_missed_day5,
                            # `Day 5: Vaccinated` = recorded_missed_day5_vaccinated,
                            # `Day 5: % Vaccinated` = recorded_missed_day5_conversion_pct,
                            # 
                            # `Day 6: Recorded` = recorded_missed_day6,
                            # `Day 6: Vaccinated` = recorded_missed_day6_vaccinated,
                            # `Day 6: % Vaccinated` = recorded_missed_day6_conversion_pct,
                            
                            `Revisit Day: Vaccinated` = recorded_missed_revisit_day_vaccinated,
                            
                            `Total Missed: Recorded` = recorded_missed_total,
                            `Total Missed: Vaccinated` = recorded_missed_total_vaccinated,
                            `Total Missed: % Vaccinated` = recorded_missed_total_conversion_pct
                     ) 
                   
                 }
                 
                 
                 #Remove any empty columns
                 data <- data %>%
                   janitor::remove_empty(which=c("cols"))
                 data <- data %>% select_if(function(col) !all(col == 0))
                 
                 
               }) #End Progress
  return(data)
})

output$admin_table <- DT::renderDT({
  req(admin_table_df())
  req(reactive_zoom_district())
  req(reactive_zoom_province()) 
  req(reactive_admin_indicator())
  
  admin_var <- reactive_admin_indicator()
  table_df <- admin_table_df() %>%
    distinct()
  group_vars <- names(table_df)[names(table_df) %in% c("Region", "Province", "District", "Cluster ID", "Cluster")]
  
  # Calculate totals for each column
  
  total_row <- sapply(table_df, calculate_total)
  
  # Convert total row to a data frame row
  total_row <- as.data.frame(t(total_row), stringsAsFactors = FALSE)
  colnames(total_row) <- colnames(table_df)
  
  
  # Handle grouping variables for the total row
  for (i in seq_along(group_vars)) {
    if (i == length(group_vars)) {
      total_row[[group_vars[i]]] <- "Total"
    } else {
      if (group_vars[i] == "Cluster ID"){
        total_row[[group_vars[i]]] <- NA
      } else {
        total_row[[group_vars[i]]] <- unique(table_df[[group_vars[i]]])[1]
      }
    }}
  
  if(all(c("Total Vaccinated", "Target Population") %in% colnames(total_row))){
    total_row <- total_row %>%
      rowwise() %>%
      mutate(`Vaccine Coverage (%)` = as.numeric(str_remove_all(`Total Vaccinated`, ",")) / as.numeric(str_remove_all(`Target Population`,","))) %>%
      ungroup() 
  }
  if(all(c("Vials Used", "Total Vaccinated") %in% colnames(total_row))){
    if(reactive_vaccine_type() == "OPV"){
    total_row <- total_row %>%
      mutate(`Vaccine Wastage (%)` = ((as.numeric(str_remove_all(`Vials Used`,",")) * 20) - as.numeric(str_remove_all(`Total Vaccinated`,","))) / (as.numeric(str_remove_all(`Vials Used`,",")) * 20)) %>%
      ungroup()
    }
    if(reactive_vaccine_type() == "IPV"){
      total_row <- total_row %>%
        mutate(`Vaccine Wastage (%)` = ((as.numeric(str_remove_all(`Vials Used`,",")) * 25) - as.numeric(str_remove_all(`Total Vaccinated`,","))) / (as.numeric(str_remove_all(`Vials Used`,",")) * 25)) %>%
        ungroup()
    }
  }
  if(all(c("Return During Campaign: Recorded", "Return During Campaign: Vaccinated", "Return During Campaign: % Vaccinated") %in% colnames(total_row))){
    total_row <- total_row %>%
      mutate(`Return During Campaign: % Vaccinated` = ((as.numeric(str_remove_all(`Return During Campaign: Vaccinated`,","))) / as.numeric(str_remove_all(`Return During Campaign: Recorded`,",")))) %>%
      ungroup()
  }
  if(all(c("Return After Campaign: Recorded", "Return After Campaign: Vaccinated", "Return After Campaign: % Vaccinated") %in% colnames(total_row))){
    total_row <- total_row %>%
      mutate(`Return After Campaign: % Vaccinated` = ((as.numeric(str_remove_all(`Return After Campaign: Vaccinated`,","))) / as.numeric(str_remove_all(`Return After Campaign: Recorded`,",")))) %>%
      ungroup()
  }
  if(all(c("Newborn/ Sleep/Sick: Recorded", "Newborn/ Sleep/Sick: Vaccinated", "Newborn/ Sleep/Sick: % Vaccinated") %in% colnames(total_row))){
    total_row <- total_row %>%
      mutate(`Newborn/ Sleep/Sick: % Vaccinated` = ((as.numeric(str_remove_all(`Newborn/ Sleep/Sick: Vaccinated`,","))) / as.numeric(str_remove_all(`Newborn/ Sleep/Sick: Recorded`,",")))) %>%
      ungroup()
  }
  if(all(c("Refusal: Recorded", "Refusal: Vaccinated", "Refusal: % Vaccinated") %in% colnames(total_row))){
    total_row <- total_row %>%
      mutate(`Refusal: % Vaccinated` = ((as.numeric(str_remove_all(`Refusal: Vaccinated`,","))) / as.numeric(str_remove_all(`Refusal: Recorded`,",")))) %>%
      ungroup()
  }
  if(all(c("Day 1: Recorded", "Day 1: Vaccinated", "Day 1: % Vaccinated") %in% colnames(total_row))){
    total_row <- total_row %>%
      mutate(`Day 1: % Vaccinated` = ((as.numeric(str_remove_all(`Day 1: Vaccinated`,","))) / as.numeric(str_remove_all(`Day 1: Recorded`,",")))) %>%
      ungroup()
  }
  if(all(c("Day 2: Recorded", "Day 2: Vaccinated", "Day 2: % Vaccinated") %in% colnames(total_row))){
    total_row <- total_row %>%
      mutate(`Day 2: % Vaccinated` = ((as.numeric(str_remove_all(`Day 2: Vaccinated`,","))) / as.numeric(str_remove_all(`Day 2: Recorded`,",")))) %>%
      ungroup()
  }
  if(all(c("Day 3: Recorded", "Day 3: Vaccinated", "Day 3: % Vaccinated") %in% colnames(total_row))){
    total_row <- total_row %>%
      mutate(`Day 3: % Vaccinated` = ((as.numeric(str_remove_all(`Day 3: Vaccinated`,","))) / as.numeric(str_remove_all(`Day 3: Recorded`,",")))) %>%
      ungroup()
  }
  if(all(c("Day 4: Recorded", "Day 4: Vaccinated", "Day 4: % Vaccinated") %in% colnames(total_row))){
    total_row <- total_row %>%
      mutate(`Day 4: % Vaccinated` = ((as.numeric(str_remove_all(`Day 4: Vaccinated`,","))) / as.numeric(str_remove_all(`Day 4: Recorded`,",")))) %>%
      ungroup()
  }
  if(all(c("Day 5: Recorded", "Day 5: Vaccinated", "Day 5: % Vaccinated") %in% colnames(total_row))){
    total_row <- total_row %>%
      mutate(`Day 5: % Vaccinated` = ((as.numeric(str_remove_all(`Day 5: Vaccinated`,","))) / as.numeric(str_remove_all(`Day 5: Recorded`,",")))) %>%
      ungroup()
  }
  if(all(c("Day 6: Recorded", "Day 6: Vaccinated", "Day 6: % Vaccinated") %in% colnames(total_row))){
    total_row <- total_row %>%
      mutate(`Day 6: % Vaccinated` = ((as.numeric(str_remove_all(`Day 6: Vaccinated`,","))) / as.numeric(str_remove_all(`Day 6: Recorded`,",")))) %>%
      ungroup()
  }
  if(all(c("Total Missed: Recorded", "Total Missed: Vaccinated", "Total Missed: % Vaccinated") %in% colnames(total_row))){
    total_row <- total_row %>%
      mutate(`Total Missed: % Vaccinated` = ((as.numeric(str_remove_all(`Total Missed: Vaccinated`,","))) / as.numeric(str_remove_all(`Total Missed: Recorded`,",")))) %>%
      ungroup()
  }
  if(all(c("Total Vaccinated", "HRMP Vaccinated") %in% colnames(total_row))){
    total_row <- total_row %>%
      rowwise() %>%
      mutate(`% HRMP, of Vaccinated` = as.numeric(str_remove_all(`HRMP Vaccinated`, ",")) / as.numeric(str_remove_all(`Total Vaccinated`,","))) %>%
      ungroup() 
  }
  
  #Get modality for total_row
  if(!(admin_var %in% c("Remaining Recorded Missed", "Missed Child Conversion"))){
    if(!("All" %in% reactive_zoom_district())){
      total_row$Modality = admin_filtered_sia_data_age_filtered()$district$modality[1]
    } else{
      if(!("All" %in% reactive_zoom_province())){
        total_row$Modality = admin_filtered_sia_data_age_filtered()$province$modality[1]
      } else{
        if(!("All" %in% reactive_zoom_region() )){
          total_row$Modality = admin_filtered_sia_data_age_filtered()$region$modality[1]
        } else{
          total_row$Modality = admin_filtered_sia_data_age_filtered()$national$modality[1]
        }
      }
    }
  }
  if(all(c("(S2S only) Avg. Vaccinated per Site") %in% colnames(total_row))){
    total_row <- total_row %>%
      mutate(`(S2S only) Avg. Vaccinated per Site` = ifelse(Modality %in% c("S2S", "M2M", "M2M/S2S"), round(((as.numeric(str_remove_all(`Total Vaccinated`,","))) / as.numeric(str_remove_all(`(S2S only) Total Sites Visited`,","))),0), NA_integer_)) %>%
      ungroup()
  }
  # if(all(c("Completeness - % of Clusters Fully Reported") %in% colnames(total_row))){
  #   total_row <- total_row %>%
  #     mutate(`Completeness - % of Clusters Fully Reported` = ifelse(Modality %in% c("S2S", "M2M", "M2M/S2S"), round(((as.numeric(str_remove_all(`Total Vaccinated`,","))) / as.numeric(str_remove_all(`(S2S only) Total Sites Visited`,","))),0), NA_integer_)) %>%
  #     ungroup()
  # }
  
  # Combine total row with original data
  table_df <- rbind(total_row, table_df)
  
  for(i in c("Target Population", "Vaccinated - Day 1", "Vaccinated - Day 2", "Vaccinated - Day 3", "Vaccinated - Day 4", "Vaccinated - Day 5", "Vaccinated - Day 6", "Vaccinated - Day 7", "Total Vaccinated", "Vials Used", "(H2H only) Recorded Missed after Campaign", 
             "Remaining Missed - Day 1", "Remaining Missed - Day 2", "Remaining Missed - Day 3", "Remaining Missed - Day 4", "Remaining Missed - Day 5", "Remaining Missed - Day 6", "Remaining Missed - Day 7", "Total Recorded Missed after Campaign",
             "Return During Campaign: Recorded", "Return During Campaign: Vaccinated",
             "Return After Campaign: Recorded", "Return After Campaign: Vaccinated",
             "Newborn/ Sleep/Sick: Recorded", "Newborn/ Sleep/Sick: Vaccinated",
             "Refusal: Recorded", "Refusal: Vaccinated",
             "Total Missed: Recorded", "Total Missed: Vaccinated",
             "Day 1: Recorded", "Day 1: Vaccinated",
             "Day 2: Recorded", "Day 2: Vaccinated",
             "Day 3: Recorded", "Day 3: Vaccinated",
             "Day 4: Recorded", "Day 4: Vaccinated",
             "Day 5: Recorded", "Day 5: Vaccinated",
             "Day 6: Recorded", "Day 6: Vaccinated",
             "Revisit Day: Vaccinated",
             "Total Missed: Recorded", "Total Missed: Vaccinated",
             "(S2S only) Total Sites Visited", "(S2S only) Avg. Vaccinated per Site",
             "HRMP Vaccinated",
             "Remaining Absent (Return during Campaign)",
             "Remaining Absent (Return after Campaign)",
             "Remaining Newborn/Sleep/Sick",
             "Remaining Refusal",
             "Total Remaining Missed")){
    if(i %in% colnames(table_df)){
      table_df <- table_df %>%
        mutate(!!i := as.numeric(.data[[i]]))
    }
  }
  
  #Sorting
  # Extract percentages into new columns if they exist
  percent_columns <- c("Reporting Completeness - % of Clusters Fully Reported")
  for (col in percent_columns) {
    if (col %in% names(table_df)) {
      table_df[[paste0(col, "_Pct")]] <- as.numeric(sub("%.*", "", table_df[[col]]))
    }
  }
  
  # Determine the indices of the original and new percent columns
  percent_col_indices <- which(names(table_df) %in% percent_columns)
  percent_pct_col_indices <- which(names(table_df) %in% paste0(percent_columns, "_Pct"))
  
  # Create columnDefs for sorting
  column_defs <- list()
  for (i in seq_along(percent_columns)) {
    pct_col <- paste0(percent_columns[i], "_Pct")
    if (pct_col %in% names(table_df)) {
      column_defs <- append(column_defs, list(
        list(orderData = percent_pct_col_indices[i] - 1, targets = percent_col_indices[i] - 1)
      ))
    }
  }
  
  # Add visibility settings for percentage columns
  if (length(percent_pct_col_indices) > 0) {
    column_defs <- append(column_defs, list(list(visible = FALSE, targets = percent_pct_col_indices - 1)))
  }
  
  # Ensure columnDefs is correctly formatted
  column_defs <- Filter(Negate(is.null), column_defs)
  
  # Create the datatable output
  datatable_output <- DT::datatable(
    data = table_df,
    extensions = 'Buttons',
    callback = JS("$.fn.dataTable.ext.errMode = 'none';"),
    options = list(
      ordering = TRUE,
      columnDefs = c(
        list(
          list(targets = "_all", className = "dt-center"),
          list(targets = "_all", width = "auto")  # Set column width to auto
        ),
        column_defs
      ),
      dom = 'Bfrtp',
      pageLength = -1,  # Display all rows on a single page
      scrollY = TRUE,
      scrollX = TRUE,
      stateSave = FALSE,
      server = FALSE,
      lengthMenu = list(c(-1), c("All")),
      paging = FALSE,
      buttons = list(
        list(
          extend = 'excel',
          text = 'Download Table',
          filename = "Admin_Data_Summary",
          title = "Admin Data Summary",
          exportOptions = list(
            modifier = list(page = 'all')
          )
        )
      )
    ),
    rownames = FALSE
  ) %>%
    # Apply bold formatting to the 'Total' row
    formatStyle(
      columns = colnames(table_df),
      valueColumns = group_vars[length(group_vars)],
      target = 'row',
      backgroundColor = styleEqual("Total", 'lightgrey'),
      fontWeight = styleEqual("Total", 'bold')
    )
  if("Vaccine Coverage (%)" %in% colnames(table_df)){
    # datatable_output <- formatPercentage(datatable_output, columns = c("Vaccine Coverage (%)"), digits=0)
    # Function to convert hex colors to RGBA with specified opacity
    hex_to_rgba <- function(hex, alpha = 0.5) {
      rgb <- round(col2rgb(hex))
      sprintf("rgba(%d, %d, %d, %.1f)", rgb[1], rgb[2], rgb[3], alpha)
    }
    
    # Function to reorder colors and apply intervals with 50% opacity
    get_color_intervals <- function(colors) {
      # Reorder colors to match intervals <70, 70-89, 90+
      rgba_colors <- sapply(colors[c("<85%", "85-89%", "90-94%", "95-100%", ">100%")], hex_to_rgba, alpha = 0.5)
      intervals <- c(0.8499999, 0.89999, 0.949999, 1)  # Cut points for three color bins
      styleInterval(intervals, rgba_colors)
    }
    
    datatable_output <- datatable_output %>%
      formatPercentage("Vaccine Coverage (%)", digits=0) %>%
      formatStyle(
        "Vaccine Coverage (%)",
        backgroundColor = get_color_intervals(colors_coverage_bins2)
      )
  }
  
  if("Vaccine Wastage (%)" %in% colnames(table_df)){
    datatable_output <- formatPercentage(datatable_output, columns = c("Vaccine Wastage (%)"), digits=0)
  }
  if("% HRMP, of Vaccinated" %in% colnames(table_df)){
    datatable_output <- formatPercentage(datatable_output, columns = c("% HRMP, of Vaccinated"), digits=0)
  }
  if("% of Missed: Absent" %in% colnames(table_df)){
    datatable_output <- formatPercentage(datatable_output, columns = c("% of Missed: Absent"), digits=0)
  }
  if("% of Missed: Newborn/Sleep/Sick" %in% colnames(table_df)){
    datatable_output <- formatPercentage(datatable_output, columns = c("% of Missed: Newborn/Sleep/Sick"), digits=0)
  }
  if("% of Missed: Refusal" %in% colnames(table_df)){
    datatable_output <- formatPercentage(datatable_output, columns = c("% of Missed: Refusal"), digits=0)
  }
  
  # List of relevant columns to format if they exist in table_df
  columns_to_format <- c("Return During Campaign: % Vaccinated",
                         "Return After Campaign: % Vaccinated",
                         "Newborn/ Sleep/Sick: % Vaccinated",
                         "Refusal: % Vaccinated",
                         "Total Missed: % Vaccinated",
                         "Day 1: % Vaccinated",
                         "Day 2: % Vaccinated", 
                         "Day 3: % Vaccinated", 
                         "Day 4: % Vaccinated", 
                         "Day 5: % Vaccinated", 
                         "Day 6: % Vaccinated",
                         "Total: % Vaccinated")
  
  # Function to convert hex colors to RGBA with specified opacity
  hex_to_rgba <- function(hex, alpha = 0.5) {
    rgb <- round(col2rgb(hex))
    sprintf("rgba(%d, %d, %d, %.1f)", rgb[1], rgb[2], rgb[3], alpha)
  }
  
  # Function to reorder colors and apply intervals with 50% opacity
  get_color_intervals <- function(colors) {
    # Reorder colors to match intervals <70, 70-89, 90+
    rgba_colors <- sapply(colors[c("<70%", "70-89%", "90-100%")], hex_to_rgba, alpha = 0.5)
    intervals <- c(0.70, 0.90)  # Cut points for three color bins
    styleInterval(intervals, rgba_colors)
  }
  
  # Loop through each column and apply formatting if it exists
  for (col_name in columns_to_format) {
    if (col_name %in% colnames(table_df)) {
      datatable_output <- datatable_output %>%
        formatPercentage(col_name, digits=0) %>%
        formatStyle(
          col_name,
          backgroundColor = get_color_intervals(colors_conversion_bins)
        )
    }
  }
  
  if("Vaccine Wastage (%)" %in% colnames(table_df)){
    datatable_output <- formatPercentage(datatable_output, columns = c("Vaccine Wastage (%)"), digits=0)
  } 
  if("% HRMP, of Vaccinated" %in% colnames(table_df)){
    datatable_output <- formatPercentage(datatable_output, columns = c("% HRMP, of Vaccinated"), digits=0)
  } 
  for(i in c("Target Population", "Vaccinated - Day 1", "Vaccinated - Day 2", "Vaccinated - Day 3", "Vaccinated - Day 4", "Vaccinated - Day 5", "Vaccinated - Day 6", "Vaccinated - Day 7", "Total Vaccinated", "Vials Used", "(H2H only) Recorded Missed after Campaign", 
             "Remaining Missed - Day 1", "Remaining Missed - Day 2", "Remaining Missed - Day 3", "Remaining Missed - Day 4", "Remaining Missed - Day 5", "Remaining Missed - Day 6", "Remaining Missed - Day 7", "Total Recorded Missed after Campaign",
             "Return During Campaign: Recorded", "Return During Campaign: Vaccinated",
             "Return After Campaign: Recorded", "Return After Campaign: Vaccinated",
             "Newborn/ Sleep/Sick: Recorded", "Newborn/ Sleep/Sick: Vaccinated",
             "Refusal: Recorded", "Refusal: Vaccinated",
             "Total Missed: Recorded", "Total Missed: Vaccinated",
             "Day 1: Recorded Missed", "Day 1: Remaining Missed",
             "Day 2: Recorded Missed", "Day 2: Remaining Missed",
             "Day 3: Recorded Missed", "Day 3: Remaining Missed",
             "Total: Recorded Missed", "Total: Remaining Missed", 
             "Day 1: Recorded", "Day 1: Vaccinated",
             "Day 2: Recorded", "Day 2: Vaccinated",
             "Day 3: Recorded", "Day 3: Vaccinated",
             "Day 4: Recorded", "Day 4: Vaccinated",
             "Day 5: Recorded", "Day 5: Vaccinated",
             "Day 6: Recorded", "Day 6: Vaccinated",
             "Revisit Day: Vaccinated",
             "Total Missed: Recorded", "Total Missed: Vaccinated",
             "HRMP Vaccinated",
             "(S2S only) Total Sites Visited", 
             "(S2S only) Avg. Vaccinated per Site",
             "Remaining Absent (Return during Campaign)",
             "Remaining Absent (Return after Campaign)",
             "Remaining Newborn/Sleep/Sick",
             "Remaining Refusal",
             "Total Remaining Missed")){
    if(i %in% colnames(table_df)){
      datatable_output <- DT::formatCurrency(datatable_output, columns = i, currency = "", interval = 3, mark = ",", digits=0)
    }
  }
  
  
  datatable_output
}) 
})
}