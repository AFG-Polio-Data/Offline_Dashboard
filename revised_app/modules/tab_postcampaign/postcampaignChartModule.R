# modules/pcm_chart_module.R

pcmChartUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("pcm_chart"), height = "360px")
}

pcmChartServer <- function(id, pcm_filtered_sia_data, reactive_pcm_indicator, chart_type, reactive_zoom_region,
                           reactive_zoom_province,
                           reactive_zoom_district) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$pcm_chart <- renderPlotly({
      req(pcm_filtered_sia_data())
      req(reactive_pcm_indicator())


                     pcm_var <- reactive_pcm_indicator()

                     if(pcm_var %in% c("PCA - Data Verification and Publication", "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication")){
                       data <- pcm_filtered_sia_data()$district %>%
                         summarize_at(c("not_verified_or_published", "verified_but_not_published", "published_and_verified"), ~sum(., na.rm=T)) %>%
                         pivot_longer(cols=-c()) %>%
                         mutate(value_cat = case_when(name == "not_verified_or_published" ~ "Not Verified or Published",
                                                  name == "verified_but_not_published" ~ "Verified but not Published",
                                                  name == "published_and_verified" ~ "Published and Verified")) %>%
                         mutate(value_cat = factor(value_cat, levels=c("Not Verified or Published", "Verified but not Published", "Published and Verified"))) %>%
                         arrange(desc(value_cat)) %>%
                         filter(value != 0)
                       yaxis_text <- "Total Submissions"

                     } else{
                       if(pcm_var %in% c("LQAS - Pct of Lots Passed (OPV)", "LQAS - Pct of Lots Passed (IPV)", "LQAS - Reasons Missed (% of Missed)")){
                        data <- pcm_filtered_sia_data()$district_indicators
                        yaxis_text <- "Number of Lots"
                       } else{
                         if(pcm_var %in% c("PCA - Finger-Mark Coverage (0-59m, OPV)",
                                           "PCA - Finger-Mark Coverage (0-11m, OPV)",
                                           "PCA - Finger-Mark Coverage (12-59m, OPV)",
                                           "PCA - Finger-Mark Coverage (HRMP, OPV)",
                                           "Out-of-house Survey - Finger-Mark Coverage (0-59m, OPV)", 
                                           "Out-of-house Survey - Finger-Mark Coverage (4-59m, IPV)", 
                                           "PCA - HRMP Percent of Houses Visited",
                                           "PCA - Finger-Mark Coverage (4-59m, IPV)")){
                           if(reactive_zoom_region() == "All"){
                             data <- pcm_filtered_sia_data()$national_indicators
                           } else{
                             if(reactive_zoom_province() == "All"){
                               data <- pcm_filtered_sia_data()$region_indicators
                             } else{
                               if(reactive_zoom_district() == "All"){
                                 data <- pcm_filtered_sia_data()$province_indicators
                               } else{
                                   data <- pcm_filtered_sia_data()$district_indicators
                               }
                             }
                           }
                           
                         } else{
                         data <- pcm_filtered_sia_data()$district_indicators
                         yaxis_text <- "Number of Districts"
                       }}}
                     
                    if(!(pcm_var %in% c("PCA - Data Verification and Publication", "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication"))){
                       
                     #Get type of indicator
                     indicator_type <- unique(data$indicator_type)

                     if("cat_dist" %in% indicator_type){
                       data_long <- data %>%
                         select("category", "rcode", "pcode", "dcode", "region", "province", "district", "numerator") %>%
                         filter(numerator != 0) %>%
                         rowwise() %>%
                         mutate(row_id = list(1:numerator)) %>%  # Create a list of row ids for each group
                         unnest(row_id) %>%  # Expand the data frame by unnesting the list
                         select(-numerator)  # Remove the numerator column if not needed in the final result

                       #Categorize for lqas_reasons_missed, ooh_reasons_missed, pca_awareness_source, pca_door_marking, and pca_reasons_missed
                       if(pcm_var == "PCA - Reasons Missed (% of Missed)"){
                         data <- data_long %>%
                           mutate(value_cat = case_when(category == "absent_market_street" ~ "Absent",
                                                        category == "absent_school_madarasa_hf" ~ "Absent",
                                                        category == "absent_travel" ~ "Absent",
                                                        category == "absent_others" ~ "Absent",

                                                        category == "child_not_available" ~ "Child Not Available",

                                                        category == "mosque_is_far" ~ "Vaccination site is too far",
                                                        category == "house_far_from_site" ~ "Vaccination site is too far",
                                                        category == "site_is_far" ~ "Vaccination site is too far",

                                                        category == "newborn" ~ "Newborn/Sleeping/Sick",
                                                        category == "sleep" ~ "Newborn/Sleeping/Sick",
                                                        category == "sick" ~ "Newborn/Sleeping/Sick",

                                                        category == "no_one_home" ~ "No One Available to Take Child to Site",
                                                        category == "not_aware" ~ "Not Aware",
                                                        category == "refusal" ~ "Refusal",

                                                        category == "refusal_decision_maker_notat_home" ~ "Refusal",
                                                        category == "refusal_misperception" ~ "Refusal",

                                                        category == "team_did_not_visit" ~ "Team did not Visit",
                                                        category == "team_did_not_visit_the_site_area" ~ "Team did not Visit",

                                                        category == "other_reasons" ~ "Other",
                                                        category == "other" ~ "Other"
                           )) %>%
                           mutate(value_cat = factor(value_cat, levels = c("Absent", "Newborn/Sleeping/Sick", "Refusal", "Team did not Visit", "Not Aware", "Vaccination site is too far", "Child Not Available", "No One Available to Take Child to Site", "Other"))) %>%
                           arrange(desc(value_cat))
                       }
                       if(pcm_var == "Out-of-house Survey - Reasons Missed (% of Missed)"){
                         data <- data_long %>%
                           mutate(value_cat = case_when(category == "absent" ~ "Absent",
                                                        category == "too_far" ~ "Vaccination site is too far",
                                                        category == "newborn_sick_sleep" ~ "Newborn/Sleeping/Sick",
                                                        category == "nss" ~ "Newborn/Sleeping/Sick",
                                                        category == "no_men" ~ "No One Available to Take Child to Site",
                                                        category == "not_aware" ~ "Not Aware",
                                                        category == "refusal" ~ "Refusal",
                                                        category == "team_not_come" ~ "Team did not Visit",
                                                        category == "other" ~ "Other"
                           )) %>%
                           mutate(value_cat = factor(value_cat, levels = c("Absent", "Vaccination site is too far", "Newborn/Sleeping/Sick", "No One Available to Take Child to Site",
                                                                           "Not Aware", "Refusal", "Team did not Visit", "Other"))) %>%
                           arrange(desc(value_cat))
                       }
                       if(pcm_var == "LQAS - Reasons Missed (% of Missed)"){
                         data <- data_long %>%
                           mutate(value_cat = case_when(category == "Absent" ~ "Absent",
                                                        category == "NewBorn" ~ "Newborn/Sleeping/Sick",
                                                        category == "Sleep" ~ "Newborn/Sleeping/Sick",
                                                        category == "Sick" ~ "Newborn/Sleeping/Sick",
                                                        category == "SickSleep" ~ "Newborn/Sleeping/Sick",
                                                        category == "Refuse" ~ "Refusal",
                                                        category == "NoTeam" ~ "Team did not Visit",
                                                        category == "Other" ~ "Other",
                                                        category == "ExpectedHomeVisit" ~ "Expected Home Visit",
                                                        category == "NoVaccineAtSite" ~ "No Vaccine at Site",
                                                        category == "ParentForgotOrNoTime" ~ "Parent Forgot or No Time",
                                                        category == "ParentsDidNotKnowAboutCampaign" ~ "Not Aware",
                                                        category == "SiteTooFar" ~ "Vaccination site is too far",
                                                        category == "VeryLongQueue" ~ "Very Long Queue"
                           )) %>%
                           mutate(value_cat = factor(value_cat, levels=c("Absent", "Newborn/Sleeping/Sick", "Refusal", "Team did not Visit", "Expected Home Visit", "No Vaccine at Site", "Parent Forgot or No Time", "Not Aware", "Vaccination site is too far", "Very Long Queue", "Other"))) %>%
                           arrange(desc(value_cat))
                       }
                       if(pcm_var == "PCA - Sources of Awareness"){
                         data <- data_long %>%
                           mutate(value_cat = case_when(category == "chw" ~ "Community Health Worker",
                                                        category == "community_elder" ~ "Community Elder",
                                                        category == "mullah" ~ "Mullah",
                                                        category == "poster" ~ "Poster",
                                                        category == "radio" ~ "Radio",
                                                        category == "social_mobilizer" ~ "Social Mobilizer",
                                                        category == "sm" ~ "Social Mobilizer",
                                                        category == "teacher" ~ "Teacher",
                                                        category == "tv" ~ "TV",
                                                        category == "other" ~ "Other"
                           )) %>%
                           mutate(value_cat = factor(value_cat, levels = c("Community Health Worker", "Community Elder", "Mullah", "Poster", "Radio", "Social Mobilizer", "Teacher", "TV", "Other"))) %>%
                           arrange(desc(value_cat))
                       }
                       if(pcm_var == "PCA - Door Marking"){
                         data <- data_long %>%
                           mutate(value_cat = case_when(category == "dm_correct" ~ "Correct",
                                                        category == "dm_in_correct" ~ "Incorrect",
                                                        category == "no_door_markings" ~ "Not Marked"
                           )) %>%
                           mutate(value_cat = factor(value_cat, levels = c("Correct", "Incorrect", "Not Marked"))) %>%
                           arrange(desc(value_cat))
                       }
                     }
                     if("rate" %in% indicator_type){
                       if(pcm_var == "PCA - Reasons Missed (per 1000 Screened)"){
                         
                         if("All" %in% reactive_zoom_region()){
                           data <- pcm_filtered_sia_data()$national_indicators
                         } else{
                           if("All" %in% reactive_zoom_province()){
                             data <- pcm_filtered_sia_data()$region_indicators
                           } else{
                             if("All" %in% reactive_zoom_district()){
                               data <- pcm_filtered_sia_data()$province_indicators
                             } else{
                               data <- pcm_filtered_sia_data()$district_indicators
                             }
                           }
                         }
                       data <- data %>%
                         select("category", "numerator", "denominator") %>%
                         mutate(value_cat = case_when(category == "absent_market_street" ~ "Absent",
                                                      category == "absent_school_madarasa_hf" ~ "Absent",
                                                      category == "absent_travel" ~ "Absent",
                                                      category == "absent_others" ~ "Absent",
                                                      
                                                      category == "child_not_available" ~ "Child Not Available",
                                                      
                                                      category == "mosque_is_far" ~ "Vaccination site is too far",
                                                      category == "house_far_from_site" ~ "Vaccination site is too far",
                                                      category == "site_is_far" ~ "Vaccination site is too far",
                                                      
                                                      category == "newborn" ~ "Newborn/Sleeping/Sick",
                                                      category == "sleep" ~ "Newborn/Sleeping/Sick",
                                                      category == "sick" ~ "Newborn/Sleeping/Sick",
                                                      
                                                      category == "no_one_home" ~ "No One Available to Take Child to Site",
                                                      category == "not_aware" ~ "Not Aware",
                                                      category == "refusal" ~ "Refusal",
                                                      
                                                      category == "refusal_decision_maker_notat_home" ~ "Refusal",
                                                      category == "refusal_misperception" ~ "Refusal",
                                                      
                                                      category == "team_did_not_visit" ~ "Team did not Visit",
                                                      category == "team_did_not_visit_the_site_area" ~ "Team did not Visit",
                                                      
                                                      category == "other_reasons" ~ "Other",
                                                      category == "other" ~ "Other"
                         )) %>%
                         mutate(value_cat = factor(value_cat, levels = c("Absent", "Newborn/Sleeping/Sick", "Refusal", "Team did not Visit", "Not Aware", "Vaccination site is too far", "Child Not Available", "No One Available to Take Child to Site", "Other"))) %>%
                         arrange(desc(value_cat)) %>%
                         group_by(value_cat) %>%
                         summarise(numerator = sum(numerator, na.rm=T),
                                   denominator = max(denominator, na.rm=T)) %>%
                         ungroup() %>%
                         filter(numerator != 0) %>%
                         rowwise() %>%
                         mutate(value = round(numerator/denominator*1000,0)) %>%
                         ungroup()
                         
                       }
                       
                     }
                     if("cat" %in% indicator_type){
                       if(pcm_var %in% c("LQAS - Pct of Lots Passed (OPV)", "LQAS - Pct of Lots Passed (IPV)")){
                         data <- data %>%
                           summarise(numerator = sum(numerator, na.rm=T),
                                     denominator = sum(denominator, na.rm=T)) %>%
                           rowwise() %>%
                           mutate(n_pass = numerator,
                                  n_fail = denominator - numerator) %>%
                           ungroup() %>%
                           select(n_pass, n_fail)

                         data <- bind_rows(
                                tibble(value = rep("Passed Lots", data$n_pass)),
                                tibble(value = rep("Failed Lots", data$n_fail))
                            ) %>%
                           mutate(value_cat  = factor(value, levels=c("Passed Lots", "Failed Lots"))) %>%
                           arrange(desc(value_cat))
                       }
                       if(pcm_var == "PCA - Modality"){
                         data <- data %>%
                           mutate(value_cat = factor(value, levels = c("H2H", "M2M", "S2S", "H2H/M2M", "H2H/S2S", "M2M/S2S", "H2H/M2M/S2S"))) %>%
                           arrange(desc(value_cat))
                       }
                     }
                    }

                     # Extract non-NA values from the specified variable
                     data_cat <- sort(unique(data[["value_cat"]][!is.na(data[["value_cat"]]) & !(data[["value_cat"]] %in% c("na", "Na"))]))

                     # pct, pca_reasons_missed, ooh_reasons_missed, lqas_reasons_missed, pca_awareness_source, pca_door_marking, lqas_result, pca_modality

                       if(pcm_var %in% c("PCA - Reasons Missed (% of Missed)", "PCA - Reasons Missed (per 1000 Screened)")){
                         pcm_colors <- colors_reasons_missed_pca[names(colors_reasons_missed_pca) %in% data_cat]
                       } else{
                         if(pcm_var == "Out-of-house Survey - Reasons Missed (% of Missed)"){
                           pcm_colors <- colors_reasons_missed_ooh[names(colors_reasons_missed_ooh) %in% data_cat]
                         } else{
                           if(pcm_var == "LQAS - Reasons Missed (% of Missed)"){
                             pcm_colors <- colors_reasons_missed_lqas[names(colors_reasons_missed_lqas) %in% data_cat]
                           } else{
                             if(pcm_var == "PCA - Sources of Awareness"){
                               pcm_colors <- colors_awareness_source_pca[names(colors_awareness_source_pca) %in% data_cat]
                             } else{
                               if(pcm_var == "PCA - Door Marking"){
                                 pcm_colors <- colors_door_marking_pca[names(colors_door_marking_pca) %in% data_cat]
                               } else{
                                 if(pcm_var %in% c("LQAS - Pct of Lots Passed (OPV)", "LQAS - Pct of Lots Passed (IPV)")){
                                   pcm_colors <- colors_passfail_bins2[names(colors_passfail_bins2) %in% data_cat]
                                 } else{
                                   if(pcm_var == "PCA - Modality"){
                                     pcm_colors <- colors_modality_bins[names(colors_modality_bins) %in% data_cat]
                                   } else{
                                     if(pcm_var %in% c("PCA - Data Verification and Publication", "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication")){
                                       pcm_colors <- colors_verification_bins[names(colors_verification_bins) %in% data_cat]
                                     }
                                   }}}}}}}



      # Create district-colored maps
      if(pcm_var %in% c("PCA - Finger-Mark Coverage (0-59m, OPV)",
                        "PCA - Finger-Mark Coverage (0-11m, OPV)",
                        "PCA - Finger-Mark Coverage (12-59m, OPV)",
                        "PCA - Finger-Mark Coverage (HRMP, OPV)",
                        "PCA - Finger-Mark Coverage (4-59m, IPV)",
                        "Out-of-house Survey - Finger-Mark Coverage (0-59m, OPV)",
                        "Out-of-house Survey - Finger-Mark Coverage (4-59m, IPV)")){
        
        plot_data <- data %>%
          mutate(
            group = case_when(
              indicator %in% c("pca_fm_coverage_0_59m", "ooh_fm_coverage") ~ "OPV: Overall",
              indicator %in% c("pca_fm_coverage_0_11m", "pca_fm_coverage_12_59m", "ooh_fm_coverage_011m", "ooh_fm_coverage_1259m") ~ "OPV: Age",
              indicator %in% c("pca_fm_coverage_female", "pca_fm_coverage_male", "ooh_fm_coverage_male", "ooh_fm_coverage_female") ~ "OPV: Gender",
              indicator == "pca_fm_coverage_hrmp_0_59m" ~ "OPV: HRMP",
              indicator %in% c("pca_fm_coverage_ipv", "ooh_fm_coverage_ipv") ~ "IPV: Overall"
            ),
            x_label = case_when(
              indicator %in% c("pca_fm_coverage_0_59m", "ooh_fm_coverage") ~ "OPV: Overall",
              indicator %in% c("pca_fm_coverage_0_11m", "ooh_fm_coverage_011m") ~ "OPV: Ages 0–11 months",
              indicator %in% c("pca_fm_coverage_12_59m", "ooh_fm_coverage_1259m") ~ "OPV: Ages 12–59 months",
              indicator %in% c("pca_fm_coverage_female", "ooh_fm_coverage_female") ~ "OPV: Female",
              indicator %in% c("pca_fm_coverage_male", "ooh_fm_coverage_male") ~ "OPV: Male",
              indicator == "pca_fm_coverage_hrmp_0_59m" ~ "OPV: HRMP",
              indicator %in% c("pca_fm_coverage_ipv", "ooh_fm_coverage_ipv") ~ "IPV: Overall"
            ),
            value = as.numeric(value),
            percent_label = paste0(round(value * 100), "%"),
            group = factor(group, levels = c("OPV: Overall", "OPV: Age", "OPV: Gender", "OPV: HRMP", "IPV: Overall")),
            x_label = factor(x_label, levels = c(
              "OPV: Overall", 
              "OPV: Ages 0–11 months", "OPV: Ages 12–59 months",
              "OPV: Female", "OPV: Male",
              "OPV: HRMP", "IPV: Overall"
            ))) %>%
          filter(!is.na(value) & value > 0) %>%  # DROP if no data (NA or zero)
          arrange(group, x_label)
        
        # Step 2: Define manual colors by group
        group_colors <- c(
          "OPV: Overall" = "#8dd3c7",
          "OPV: Age" = "#80b1d3",
          "OPV: Gender" = "#fdb462",
          "OPV: HRMP" = "#fccde5",
          "IPV: Overall" = "#a6d854"
        )
        
        # Step 3: ggplot base
        plot <- ggplot(plot_data, aes(x = x_label, y = value, fill = group, text = label)) +
          geom_col(color = "black", width = 0.7, alpha=0.7) +
          geom_text(aes(label = percent_label), vjust = 0, nudge_y = 0.04, size = 3.5) +
          scale_fill_manual(values = group_colors) +
          scale_y_continuous(
            labels = percent_format(accuracy = 1),
            limits = c(0, 1.15),
            breaks = seq(0, 1, by = 0.2)  # 0%, 20%, ..., 100%
          ) +
          labs(x = NULL, y = "Finger-Mark Coverage (%)") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 30, hjust = 1),
            legend.position = "none"
          )
        
        fig <- ggplotly(plot, tooltip = "text") %>%
          layout(
            height = 320,  # shorter overall chart height
            margin = list(t = 30, b = 80),  # extra space for angled x-axis labels
            legend = list(
              orientation = "h",
              x = 0.5,
              y = -0.6,
              xanchor = "center",
              yanchor = "bottom",
              title = ""
            )
          ) %>%
          config(
            displaylogo = FALSE,
            modeBarButtons = list(list("toImage"))
          )
      }
      if(pcm_var %in% c("PCA - Reasons Missed (per 1000 Screened)")){
        # Arrange data first
        data <- data %>%
          arrange(desc(value)) %>%
          mutate(
            tooltip_text = paste0(value_cat, ": ", round(value, 0), " per 1000 screened"),
            value_cat = factor(value_cat, levels = value_cat)  # Set levels in sorted order
          )
        
        # Plot
        plot <- ggplot(data = data) +
          geom_col(aes(x = value_cat, y = value, fill = value_cat, text = tooltip_text), 
                   color = "black", show.legend = FALSE) +
          scale_fill_manual(values = pcm_colors) +
          scale_y_continuous(labels = scales::comma, limits = c(0, 1.1 * max(data$value))) +
          labs(x = NULL, y = HTML("Number Missed<br>per 1000 screened")) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1),
            legend.position = "none"
          )
        
        # Convert to plotly
        fig <- ggplotly(plot, tooltip = "text") %>%
          layout(
            height = 410,
            margin = list(t = 0, b = 50)
          ) %>%
          config(
            displaylogo = FALSE,
            modeBarButtons = list(list("toImage"))
          )
      }
      if(pcm_var %in% c("PCA - HRMP Percent of Houses Visited")){
                       
                       plot_data <- data %>%
                         mutate(
                           group = case_when(
                             indicator %in% c("pca_hrmp_pct_of_houses") ~ "All HRMP",
                             indicator %in% c("pca_hrmp_pct_of_houses_nomad") ~ "HRMP Type",
                             indicator %in% c("pca_hrmp_pct_of_houses_returnees") ~ "HRMP Type",
                             indicator %in% c("pca_hrmp_pct_of_houses_idp") ~ "HRMP Type",
                             indicator %in% c("pca_hrmp_pct_of_houses_straddling") ~ "HRMP Type"
                           ),
                           x_label = case_when(
                             indicator %in% c("pca_hrmp_pct_of_houses") ~ "All HRMP",
                             indicator %in% c("pca_hrmp_pct_of_houses_nomad") ~ "Nomad",
                             indicator %in% c("pca_hrmp_pct_of_houses_returnees") ~ "Returnees",
                             indicator %in% c("pca_hrmp_pct_of_houses_idp") ~ "IDP",
                             indicator %in% c("pca_hrmp_pct_of_houses_straddling") ~ "Straddling"
                           ),
                           value = as.numeric(value),
                           percent_label = paste0(round(value, 3) * 100, "%"),
                           group = factor(group, levels = c("All HRMP", "HRMP Type")),
                           x_label = factor(x_label, levels = c(
                             "All HRMP", 
                             "Nomad", "Returnees", "IDP", "Straddling"
                           ))) %>%
                         filter(!is.na(value) & value > 0) %>%  # DROP if no data (NA or zero)
                         arrange(group, x_label)
                       
                       # Step 2: Define manual colors by group
                       group_colors <- c(
                         "All HRMP" = "#8dd3c7",
                         "HRMP Type" = "#fdb462"
                       )
                       
                       # Step 3: ggplot base
                       # Compute dynamic y-axis max
                       y_max <- max(as.numeric(plot_data$value), na.rm = TRUE) * 1.15
                       
                       # Determine dynamic accuracy (e.g., 0.1%, 0.5%, 1%) depending on range
                       accuracy_val <- case_when(
                         y_max <= 0.01 ~ 0.01,  # e.g. 0.1%
                         y_max <= 0.05 ~ 0.025,
                         y_max <= 0.1 ~ 0.05,
                         y_max <= 0.5 ~ 0.1,
                         y_max <= 1 ~ 0.2,
                         TRUE ~ 0.5  # fallback
                       )
                       
                       # Build plot
                       plot <- ggplot(plot_data, aes(x = x_label, y = value, fill = group, text = label)) +
                         geom_col(color = "black", width = 0.7, alpha = 0.7) +
                         geom_text(aes(label = percent_label), vjust = 0, nudge_y = y_max/10, size = 3.5) +
                         scale_fill_manual(values = group_colors) +
                         scale_y_continuous(
                           labels = label_percent(accuracy = accuracy_val),
                           limits = c(0, y_max)
                         ) +
                         labs(x = NULL, y = "Percent of Houses Visited in PCA") +
                         theme_minimal() +
                         theme(
                           axis.text.x = element_text(angle = 30, hjust = 1),
                           legend.position = "none"
                         )
                       
                       fig <- ggplotly(plot, tooltip = "text") %>%
                         layout(
                           height = 320,  # shorter overall chart height
                           margin = list(t = 30, b = 80),  # extra space for angled x-axis labels
                           legend = list(
                             orientation = "h",
                             x = 0.5,
                             y = -0.6,
                             xanchor = "center",
                             yanchor = "bottom",
                             title = ""
                           )
                         ) %>%
                         config(
                           displaylogo = FALSE,
                           modeBarButtons = list(list("toImage"))
                         )
                     }               
      if(pcm_var %in% c("PCA - Recall Coverage (0-59m)",
                        "PCA - Percent Aware")){

              plot <- ggplot(data=data) +
                geom_histogram(aes(x=as.numeric(value)), fill="lightblue", color="black") +
                scale_x_continuous(labels = percent_format(accuracy = 1)) +  # Format x-axis as percent
                labs(x=pcm_var,
                     y=yaxis_text) +
                theme_minimal() +
                theme(
                  axis.text.x = element_text(margin = margin(t = -10))  # Adjust the margin to move labels closer
                )


              plotly_plot <- ggplotly(plot, tooltip=NULL)

                fig <- plotly_plot %>%
                  layout(
                    legend = list(
                      orientation = "h",      # Horizontal orientation
                      x = 0.5,                # Center horizontally
                      y = -0.6,               # Adjust to move below the plot
                      xanchor = "center",
                      yanchor = "bottom",
                      title = ""
                    )
                  )

              fig <- fig %>%
                plotly::config(displaylogo=FALSE,
                       modeBarButtons = (list(list("toImage"))))

      }

      if(pcm_var %in% c("PCA - Modality", "LQAS - Pct of Lots Passed (OPV)", "LQAS - Pct of Lots Passed (IPV)", "PCA - Reasons Missed (% of Missed)", "PCA - Sources of Awareness", "PCA - Door Marking", "Out-of-house Survey - Reasons Missed (% of Missed)", "LQAS - Reasons Missed (% of Missed)",
                        "PCA - Data Verification and Publication", "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication")){
        if(pcm_var %in% c("PCA - Reasons Missed (% of Missed)", "Out-of-house Survey - Reasons Missed (% of Missed)", "LQAS - Reasons Missed (% of Missed)")){
          yaxis_text <- "% of Missed Children"
          if(pcm_var %in% c("Out-of-house Survey - Reasons Missed (% of Missed)")){
            xaxis_text <- "Reasons Missed"
          } else{
            xaxis_text <- pcm_var
          }
        }
        if(pcm_var %in% c("PCA - Sources of Awareness", "PCA - Door Marking")){
          yaxis_text <- "Percent of Households"
          xaxis_text <- pcm_var
        }
        if(pcm_var %in% c("PCA - Modality")){
          yaxis_text <- "Percent of Districts"
          xaxis_text <- pcm_var
        }
        if(pcm_var %in% c("LQAS - Pct of Lots Passed (OPV)", "LQAS - Pct of Lots Passed (IPV)")){
          yaxis_text <- "Percent of Lots"
          xaxis_text <- "LQAS: Percent of Lots Passed and Failed"
        }
        if(pcm_var %in% c("PCA - Data Verification and Publication", "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication")){
          yaxis_text <- "Percent of Submissions"
          xaxis_text <- "Submission Verification and Publication Status"
        }
        if (chart_type() == "bar") {
          if(pcm_var %in% c("PCA - Data Verification and Publication", "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication")){
            data_sum <- data %>%
              mutate(percentage = value/sum(value)*100) %>%
              arrange(desc(value)) %>%
              mutate(tooltip_text = paste0(value_cat,": ", round(percentage,0), "% (", scales::comma(value,accuracy=1), "/", scales::comma(sum(value, na.rm=T), accuracy=1), ")"))
          } else{
            data_sum <- data %>%
              mutate(value_cat = as.character(value_cat)) %>%
              group_by(value_cat) %>%
              summarise(count = n()) %>%
              mutate(percentage = count / sum(count) * 100) %>%
              arrange(desc(count)) %>%
              mutate(tooltip_text = paste0(value_cat,": ", round(percentage,0), "% (", scales::comma(count,accuracy=1), "/", scales::comma(sum(count, na.rm=T), accuracy=1), ")"))
          }
        data_sum$value_cat <- factor(data_sum$value_cat, levels = data_sum$value_cat)

        plot <- ggplot(data = data_sum) +
          geom_col(aes(x = value_cat, y = percentage, fill = value_cat, text = tooltip_text), color = "black") +
          scale_fill_manual(values = pcm_colors) +
          scale_y_continuous(labels = percent_format(scale = 1), limits=c(0,100)) +  # Format y-axis as percentage
          labs(x = xaxis_text,
               y = yaxis_text) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1),
            legend.title = element_blank()
          ) +
          guides(fill = guide_legend(override.aes = list(colour = NULL)))  # Remove unused legend items


        fig <- ggplotly(plot, tooltip="text") %>%
          layout(showlegend = FALSE) %>%
          plotly::config(displaylogo=FALSE,
                 modeBarButtons = (list(list("toImage"))))
        }
        if (chart_type() == "pie") {

        # Summarize Data
          if(pcm_var %in% c("PCA - Data Verification and Publication", "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication")){
            data_sum <- data %>%
              mutate(percentage = value/sum(value)*100) %>%
              arrange(desc(value)) %>%
              mutate(tooltip_text = paste0(value_cat,": ", round(percentage,0), "% (", scales::comma(value,accuracy=1), "/", scales::comma(sum(value, na.rm=T), accuracy=1), ")"))
          } else{
            data_sum <- data %>%
              mutate(value_cat = as.character(value_cat)) %>%
              group_by(value_cat) %>%
              summarise(count = n()) %>%
              mutate(percentage = round(count / sum(count) * 100,0)) %>%
              arrange(desc(count)) %>%
              mutate(tooltip_text = paste0(value_cat, ": ", round(percentage, 0), "% (", 
                                           scales::comma(count, accuracy=1), "/", 
                                           scales::comma(sum(count, na.rm = TRUE), accuracy=1), ")"))
          }
        
        data_sum$value_cat <- factor(data_sum$value_cat, levels = data_sum$value_cat)
        
        # Ensure the data is sorted by highest to lowest percentage
        data_sum <- data_sum %>%
          arrange(desc(percentage))
        
        # Ensure colors match sorted data
        sorted_colors <- pcm_colors[match(data_sum$value_cat, names(pcm_colors))]
        
        fig <- plot_ly(
          data = data_sum, 
          labels = ~value_cat, 
          values = ~percentage, 
          type = 'pie',
          textinfo = 'none',  # Disable default text formatting
          texttemplate = "%{label}: %{value:.0f}%",  # Custom format: label + whole number percentage
          textposition = 'inside',  # Ensures all labels stay inside their segments
          text = ~paste0(value_cat, ": ", round(percentage, 0), "%"),  # Ensures rounded percentages
          hoverinfo = 'text',
          insidetextfont = list(size = 12, color = "black", family = "Arial"),  # Ensures labels are readable
          marker = list(colors = sorted_colors),
          direction = "clockwise",  # Orders segments properly
          pull = 0  # No explosion effect
        )
        
        fig <- fig %>% 
          config(displaylogo = FALSE, modeBarButtons = list(list("toImage"))) %>%
          layout(
            margin = list(l = 20, r = 20, t = 20, b = 20),  # Keep margins minimal
            uniformtext = list(minsize = 8, mode = "show"),  # Prevents unreadable labels
            showlegend = FALSE,  # Remove legend since labels are visible
            title = list(text = NULL)  # Removes the title
          )
        }

      }
      fig
    })
  })
}
