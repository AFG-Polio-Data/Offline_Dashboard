adminCardUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("admin_card"))
}

adminCardServer <- function(id, admin_filtered_sia_data_age_filtered, 
                            reactive_admin_indicator, reactive_zoom_region, 
                            reactive_zoom_province, reactive_zoom_district) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$admin_card <- renderUI({
      req(reactive_admin_indicator())
      req(admin_filtered_sia_data_age_filtered())
      req(reactive_zoom_district())
      req(reactive_zoom_province())
      req(reactive_zoom_region())
      
      withProgress(message = 'Calculation in progress...',
                   value = 0, {
                     
                     admin_var <- reactive_admin_indicator()
                     
                     if(admin_var %in% c("Missed Child Conversion", "Remaining Recorded Missed")){
                       if(!("All" %in% reactive_zoom_district())){
                         data <- admin_filtered_sia_data_age_filtered()$conversion_district
                         data <- data %>%
                           ungroup()
                       } else{
                         if(!("All" %in% reactive_zoom_province())){
                           data <- admin_filtered_sia_data_age_filtered()$conversion_province
                           data <- data %>%
                             ungroup()
                         } else{
                           if(!("All" %in% reactive_zoom_region())){
                             data <- admin_filtered_sia_data_age_filtered()$conversion_region
                             data <- data %>%
                               ungroup()
                           } else{
                             data <- admin_filtered_sia_data_age_filtered()$conversion_national
                             data <- data %>%
                               ungroup()
                           }}}
                     } 
                     else{
                       if(admin_var == "Reporting Completeness (% of Clusters with Complete Data)"){
                         if(!("All" %in% reactive_zoom_district())){
                           data <- admin_filtered_sia_data_age_filtered()$district_completeness
                           data <- data %>%
                             ungroup()
                         } else{
                           if(!("All" %in% reactive_zoom_province())){
                             data <- admin_filtered_sia_data_age_filtered()$province_completeness
                             data <- data %>%
                               ungroup()
                           } else{
                             if(!("All" %in% reactive_zoom_region())){
                               data <- admin_filtered_sia_data_age_filtered()$region_completeness
                               data <- data %>%
                                 ungroup()
                             } else{
                               data <- admin_filtered_sia_data_age_filtered()$national_completeness
                               data <- data %>%
                                 ungroup()
                             }}}
                       } else{
                       if(!("All" %in% reactive_zoom_district())){
                         data <- admin_filtered_sia_data_age_filtered()$district
                         data <- data %>%
                           ungroup()
                       } else{
                         if(!("All" %in% reactive_zoom_province())){
                           data <- admin_filtered_sia_data_age_filtered()$province
                           data <- data %>%
                             ungroup()
                         } else{
                           if(!("All" %in% reactive_zoom_region())){
                             data <- admin_filtered_sia_data_age_filtered()$region
                             data <- data %>%
                               ungroup()
                           } else{
                             data <- admin_filtered_sia_data_age_filtered()$national
                             data <- data %>%
                               ungroup()
                           }}}
                     }}
                     
                     if(admin_var == "Cumulative Coverage"){
                       data <- data %>%
                         select(total_coverage)
                       value <- paste0(round(data$total_coverage[1],2)*100,"%")
                       label <- "Coverage (Vaccinated / Targeted)"
                     }
                     if(admin_var == "Site Density (S2S Only)"){
                       data <- data %>%
                         select(site_density)
                       value <- paste0(round(data$site_density[1],0))
                       value <- ifelse(as.character(value) == "NA", "(Available only for fully S2S areas)", value)
                       label <- HTML("Average Number Vaccinated<br>per Site in S2S Areas")
                     }
                     if(admin_var == "Missed Child Conversion"){
                       data <- data %>%
                         select(recorded_missed_total_conversion_pct)
                       value <- paste0(round(data$recorded_missed_total_conversion_pct[1],2)*100,"%")
                       label <- "Of recorded missed children, % successfully vaccinated at revisit"
                     }
                     if(admin_var == "Total Vaccinated"){
                       data <- data %>%
                         rowwise() %>%
                         mutate(total_vaccinated = sum(total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, total_children_vaccinated_day5, total_children_vaccinated_day6, total_children_vaccinated_day7, na.rm=T)) %>%
                         ungroup()
                       value <- scales::comma(data$total_vaccinated[1], accuracy=1)
                       label <- "Total Vaccinated"
                     }
                     if(admin_var == "HRMP Vaccinated"){
                       data <- data %>%
                         ungroup()
                       value <- scales::comma(data$hrmp_vaccinated[1], accuracy=1)
                       label <- "Total Vaccinated among High-Risk Mobile Populations"
                     }
                     if(admin_var == "HRMP Percent of Total Vaccinated"){
                       data <- data %>%
                         ungroup()
                       value <-  paste0(round(data$pct_hrmp[1],2)*100, "%")
                       label <- "Of total vaccinated, % that are High-Risk Mobile Populations"
                                 # High-Risk Mobile Population Percent of Total Vaccinated"
                     }
                     if(admin_var == "Vaccine Wastage"){
                       data <- data %>%
                         select(vacc_wastage)
                       value <- paste0(round(data$vacc_wastage[1],2)*100, "%")
                       label <- "Vaccine Wastage"
                     }
                     if(admin_var == "Target Population"){
                       data <- data %>%
                         select(target_population)
                       value <- scales::comma(data$target_population[1], accuracy=1)
                       label <- "Target Population"
                     }
                     if(admin_var == "Remaining Recorded Missed"){
                       data <- data %>%
                         rowwise() %>%
                         mutate(total_recorded_missed = recorded_missed_total - recorded_missed_total_vaccinated) %>%
                         ungroup()
                       value <- scales::comma(data$total_recorded_missed[1], accuracy=1)
                       label <- "Total Recorded Missed Remaining Unvaccinated"
                     }
                     if(admin_var == "Modality"){
                       data <- data %>%
                         select(modality)
                       value <- data$modality[1]
                       label <- "Modality (From Admin Data)"
                     }
                     if(admin_var == "Reporting Completeness (% of Clusters with Complete Data)"){
                       data <- data %>%
                         select(total_pct_reported)
                       value <- paste0(round(data$total_pct_reported[1],2)*100,"%")
                       label <- "Clusters with Complete Data (Data reported for all expected days)"
                     }
                   })
      tagList(
        tags$p(style = "font-weight: bold; margin: 0; max-width: 100%;" , 
               value),
        tags$p(
          style="margin: 0; max-width: 100%;",
          label)
      )
    })
  })
}