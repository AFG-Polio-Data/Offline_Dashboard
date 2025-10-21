################################################################################
# averageIndicatorCardUI and averageIndicatorCardServer
#
# These functions generate Cards displaying:
# - The average value of percentage indicators,
# - The total value of numeric indicators,
# - A custom aggregation of categorical indicators,
# aggregated across all campaigns for each district.
#
# - averageIndicatorCardUI: Defines the UI output container for the cards
# - averageIndicatorCardServer: Processes the data and renders the cards.
################################################################################

averageIndicatorCardUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("average_indicator_card"))
    # ,
    # leafletOutput(ns("average_indicator_card_leaflet"), height = "350px")
  )
}


averageIndicatorCardServer <- function(id, temporal_filtered_sia_data_v2, temporal_pretty_named_list, df_campaigns, 
                                      temporal_borders_district, temporal_borders_district_no_sia, campaign_rpd, 
                                      colors_coverage_bins2, colors_passfail_bins, colors_modality_bins, 
                                      colors_clusters_lt95_bin, colors_coverage_bins, colors_conversion_bins, temporal_legend_names_list, 
                                      input_temporal_indicator, input_temporal_map_base, input_zoom_region_select_temporal_v2, 
                                      input_zoom_province_select_temporal_v2, input_zoom_district_select_temporal_v2, selected_campaigns, shp_districts, 
                                      low_coverage_threshold, selected_reason_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    total_campaigns_card_value_reactive <- reactive({
      req(selected_campaigns())
      data <- campaign_rpd %>%
        filter(campaign_name %in% selected_campaigns()) %>%
        select(campaign_name, rcode, pcode, dcode) %>%
        left_join(shp_districts, by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) 
      
      filtered_df <- data %>%
        filter(if ("All" %in% input_zoom_region_select_temporal_v2()) TRUE else APMIS_Region %in% input_zoom_region_select_temporal_v2()) %>%
        filter(if ("All" %in% input_zoom_province_select_temporal_v2()) TRUE else APMIS_Province %in% input_zoom_province_select_temporal_v2()) %>%
        filter(if ("All" %in% input_zoom_district_select_temporal_v2()) TRUE else APMIS_District %in% input_zoom_district_select_temporal_v2())
      
      
      filtered_df <- filtered_df %>%
        select(campaign_name) %>%
        unique()

      total_campaigns_card_value <- nrow(filtered_df)
    })
    
    avg_card_value_reactive <- reactive({
      req(temporal_filtered_sia_data_v2())
      req(selected_reason_type())
      withProgress(message = 'Calculation in progress...', value = 0, {
        selected_var <- temporal_pretty_named_list[[input_temporal_indicator()]]
        selected_var <- selected_var[1]
        selected_reason_type <- selected_reason_type()
        df_map <- NULL
        
        #Process LQAS
        if(selected_var == "lqas_result"){
          if(!("All" %in% input_zoom_district_select_temporal_v2())){
            df_map <- temporal_filtered_sia_data_v2()$district_indicators
          } else{
            if(!("All" %in% input_zoom_province_select_temporal_v2())){
              df_map <- temporal_filtered_sia_data_v2()$province_indicators
            } else{
            if(!("All" %in% input_zoom_region_select_temporal_v2())){
              df_map <- temporal_filtered_sia_data_v2()$region_indicators
            } else{
                df_map <- temporal_filtered_sia_data_v2()$national_indicators
          }}}
          df_map <- df_map %>%
            mutate(
              numerator = as.numeric(numerator),
              denominator = as.numeric(denominator)
            ) %>%
            summarise(
              numerator = sum(numerator, na.rm = TRUE),
              denominator = sum(denominator, na.rm = TRUE),
              value_raw = numerator / denominator
            ) %>%
            mutate(
              value = paste0(
                round(value_raw * 100, 0), "% (",
                numerator, "/", denominator, ")"
              )
            ) %>%
            select(-value_raw)
          card_value <- df_map$value[1]
          card_label <- HTML("Percent of Lots Passed<br>Across All Campaigns")
        }
        # Process Percentage Indicators (Averaging)
        if(selected_var == "admin_coverage"){
          if(!("All" %in% input_zoom_district_select_temporal_v2())){
            df_map <- temporal_filtered_sia_data_v2()$district
          } else{
            if(!("All" %in% input_zoom_province_select_temporal_v2())){
              df_map <- temporal_filtered_sia_data_v2()$province
            } else{
              if(!("All" %in% input_zoom_region_select_temporal_v2())){
                df_map <- temporal_filtered_sia_data_v2()$region
              } else{
                df_map <- temporal_filtered_sia_data_v2()$national
              }}}
          df_map <- df_map %>%
            filter(age_group == "0-59 Months") %>%
            summarise(value = mean(total_coverage, na.rm = TRUE), .groups = "drop") %>%
            mutate(value = paste0(round(value,2)*100,"%")) 
          card_value <- df_map$value[1]
          card_label <- HTML("Average Admin Coverage<br>Across All Campaigns")
        }
        if(selected_var %in% c("ooh_fm_coverage", "pca_fm_coverage_hrmp_0_59m", "pca_fm_coverage_0_11m", "pca_fm_coverage_0_59m", "pca_fm_coverage_12_59m", "pca_recall_coverage_0_59m",
                               "pca_pct_clusters_lt95_fm_cov", "pca_completeness", "pca_reasons_missed_rates")){
          if(!("All" %in% input_zoom_district_select_temporal_v2())){
            df_map <- temporal_filtered_sia_data_v2()$district_indicators
          } else{
            if(!("All" %in% input_zoom_province_select_temporal_v2())){
              df_map <- temporal_filtered_sia_data_v2()$province_indicators
            } else{
              if(!("All" %in% input_zoom_region_select_temporal_v2())){
                df_map <- temporal_filtered_sia_data_v2()$region_indicators
              } else{
                df_map <- temporal_filtered_sia_data_v2()$national_indicators
              }}}
          
          if(selected_var == "pca_reasons_missed_rates"){
            df_map <- df_map %>%
              group_by(campaign_name) %>%
              summarise(numerator = sum(numerator, na.rm=T),
                        denominator = max(denominator, na.rm=T)) %>%
              ungroup() %>%
              mutate(value = numerator / denominator)
          }
          
          df_map <- df_map %>%
            mutate(value = as.numeric(value)) %>%
            summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
            mutate(value = ifelse(selected_var %in% c("pca_reasons_missed_rates"), paste0(round(value*1000,0)), paste0(round(value * 100, 0), "%"))) 
            
          card_value <- df_map$value[1]
          if(grepl("coverage", selected_var)){
            card_label <- HTML("Average Coverage<br>Across All Campaigns")
          }
          if(selected_var == "pca_completeness"){
            card_label <- HTML("Average Percent of Clusters<br>Reporting PCA Data<br>Across All Campaigns")
          }
          if(selected_var == "pca_pct_clusters_lt95_fm_cov"){
            card_label <- HTML("Average Percent of Clusters<br>with <95% Coverage<br>Across All Campaigns")
          }
          if(selected_var == "pca_reasons_missed_rates"){
            if(selected_reason_type %in% c("PCA - Missed due to Refusal (per 1000 Screened)")){
              card_label <- HTML("Average Missed due to<br>Refusal per 1000 screened")
            }
            if(selected_reason_type %in% c("PCA - Missed due to Child Not Available (per 1000 Screened)")){
              card_label <- HTML("Average Missed due to<br>Child Not Available per 1000 screened")
            }
            if(selected_reason_type %in% c("PCA - Missed due to Newborn/Sleeping/Sick (per 1000 Screened)")){
              card_label <- HTML("Average Missed due to<br>Newborn/Sleeping/Sick per 1000 screened")
            }
            if(selected_reason_type %in% c("PCA - Missed due to No Team (per 1000 Screened)")){
              card_label <- HTML("Average Missed due to<br>No Team per 1000 screened")
            }
            if(selected_reason_type %in% c("PCA - Missed due to Site Too Far (per 1000 Screened)")){
              card_label <- HTML("Average Missed due to<br>Site Too Far per 1000 screened")
            }
            if(selected_reason_type %in% c("PCA - Missed due to Not Aware (per 1000 Screened)")){
              card_label <- HTML("Average Missed due to<br>Not Aware per 1000 screened")
            }
            if(selected_reason_type %in% c("PCA - Missed due to No One Available to Take Child (per 1000 Screened)")){
              card_label <- HTML("Average Missed due to<br>No One Available to Take Child per 1000 screened")
            }
            if(selected_reason_type %in% c("PCA - Missed due to Other Reason (per 1000 Screened)")){
              card_label <- HTML("Average Missed due to<br>Other Reason per 1000 screened")
            }
          }
          
        }
        
        if (selected_var %in% c("admin_total_vaccinated", "admin_target_pop")) {
          if(!("All" %in% input_zoom_district_select_temporal_v2())){
            df_map <- temporal_filtered_sia_data_v2()$district
          } else{
            if(!("All" %in% input_zoom_province_select_temporal_v2())){
              df_map <- temporal_filtered_sia_data_v2()$province
            } else{
              if(!("All" %in% input_zoom_region_select_temporal_v2())){
                df_map <- temporal_filtered_sia_data_v2()$region
              } else{
                df_map <- temporal_filtered_sia_data_v2()$national
              }}}
          if(selected_var == "admin_total_vaccinated"){
          df_map <- df_map %>%
            filter(age_group == "0-59 Months") %>%
            summarise(value = sum(total_vaccinated, na.rm = TRUE), .groups = "drop") %>%
            arrange(desc(value)) %>%
            mutate(value = scales::comma(value, accuracy=1))
          card_value <- df_map$value[1]
          card_label <- HTML("Total Doses Administered<br>Across All Campaigns")
          }
          if(selected_var == "admin_target_pop"){
            df_map <- df_map %>%
              filter(age_group == "0-59 Months") %>%
              summarise(value = round(mean(target_population, na.rm = TRUE),0), .groups = "drop") %>%
              arrange(desc(value)) %>%
              mutate(value = scales::comma(value, accuracy=1))
            card_value <- df_map$value[1]
            card_label <- HTML("Average Targeted Population<br>Across All Campaigns")
          }
        }
        
        if(selected_var == "pca_clusters_consistently_low_coverage"){
          req(low_coverage_threshold())
          
          pca_cluster_level <- temporal_filtered_sia_data_v2()$cluster_indicators %>%
            filter(indicator == "pca_fm_coverage_0_59m") %>%
            select(campaign_name, region, province, district, clustername, rcode, pcode, dcode, ccode, numerator, denominator, value) %>%
            mutate_at(c("numerator", "denominator", "value"), ~as.numeric(.))
          
          threshold <- low_coverage_threshold()
          if (is.null(threshold) || length(threshold) == 0) {
            threshold <- 0.7 # Default to 70% if no value is available
          }
          pca_cluster_level <- pca_cluster_level %>%
            mutate(below_threshold = case_when(value <= threshold ~ 1, 
                                               value > threshold ~ 0,
                                               TRUE ~ NA))
          
          cluster_threshold_aggregated <- pca_cluster_level %>%
            ungroup() %>%
            group_by(region, province, district, clustername, rcode, pcode, dcode, ccode) %>%
            summarise(mean_coverage = mean(value, na.rm=T),
                      n_below_threshold = sum(below_threshold, na.rm=T),
                      n_campaigns = n()) %>%
            ungroup() %>%
            mutate(pct_below_threshold = n_below_threshold / n_campaigns) %>%
            mutate(consistently_low = case_when(n_campaigns >=2 & pct_below_threshold > 0.5 ~ 1, 
                                                n_campaigns >= 2 ~ 0,
                                                TRUE ~ NA))
          
          df_map <- cluster_threshold_aggregated %>%
            group_by(region, province, district, rcode, pcode, dcode) %>%
            summarise(total_consistently_low_coverage_clusters = sum(consistently_low, na.rm=T)) %>%
            ungroup() %>%
            filter(total_consistently_low_coverage_clusters >= 1)  
          
          
          card_value <- sum(df_map$total_consistently_low_coverage_clusters, na.rm=T)
          card_label <- HTML(paste0("Total Clusters With<br>Consistently Low PCA Coverage (<",round(threshold,2)*100,"%)"))
        }
        
        # Process Categorical Indicators (Most Frequent Category)
        if (selected_var %in% c("pca_modality")) {
          if(!("All" %in% input_zoom_district_select_temporal_v2())){
            df_map <- temporal_filtered_sia_data_v2()$district_indicators
          } else{
            if(!("All" %in% input_zoom_province_select_temporal_v2())){
              df_map <- temporal_filtered_sia_data_v2()$province_indicators
            } else{
              if(!("All" %in% input_zoom_region_select_temporal_v2())){
                df_map <- temporal_filtered_sia_data_v2()$region_indicators
              } else{
                df_map <- temporal_filtered_sia_data_v2()$national_indicators
              }}}
          df_map <- df_map %>%
            mutate(H2H = ifelse(grepl("H2H", value), 1, 0)) %>%
            mutate(S2S = ifelse(grepl("S2S", value), 1, 0)) %>%
            mutate(M2M = ifelse(grepl("M2M", value), 1, 0)) %>%
            summarise(H2H=sum(H2H, na.rm=T),
                      M2M = sum(M2M, na.rm=T),
                      S2S = sum(S2S, na.rm=T)) %>%
            ungroup() %>%
            mutate(value = case_when(
              H2H >= 1 & M2M >=1 & S2S >= 1 ~ "H2H/M2M/S2S",
              H2H >= 1 & M2M >=1 ~ "H2H/M2M",
              H2H >= 1 & S2S >=1 ~ "H2H/S2S",
              M2M >= 1 & S2S >=1 ~ "M2M/S2S",
              H2H >=1 ~ "H2H",
              S2S >=1 ~ "S2S",
              M2M >=1 ~ "M2M",
              TRUE ~ NA_character_
            )
            ) 
          card_value <- df_map$value[1]
          card_label <- HTML("Modalities Used<br>Across All Campaigns")
        }
        if (selected_var %in% c("admin_modality")) {
          if(!("All" %in% input_zoom_district_select_temporal_v2())){
            df_map <- temporal_filtered_sia_data_v2()$district
          } else{
            if(!("All" %in% input_zoom_province_select_temporal_v2())){
              df_map <- temporal_filtered_sia_data_v2()$province
            } else{
              if(!("All" %in% input_zoom_region_select_temporal_v2())){
                df_map <- temporal_filtered_sia_data_v2()$region
              } else{
                df_map <- temporal_filtered_sia_data_v2()$national
              }}}
          df_map <- df_map %>%
            filter(age_group == "0-59 Months") %>% 
            mutate(H2H = ifelse(grepl("H2H", modality), 1, 0)) %>%
            mutate(S2S = ifelse(grepl("S2S", modality), 1, 0)) %>%
            mutate(M2M = ifelse(grepl("M2M", modality), 1, 0)) %>%
            summarise(H2H=sum(H2H, na.rm=T),
                      M2M = sum(M2M, na.rm=T),
                      S2S = sum(S2S, na.rm=T)) %>%
            ungroup() %>%
            mutate(value = case_when(
              H2H >= 1 & M2M >=1 & S2S >= 1 ~ "H2H/M2M/S2S",
              H2H >= 1 & M2M >=1 ~ "H2H/M2M",
              H2H >= 1 & S2S >=1 ~ "H2H/S2S",
              M2M >= 1 & S2S >=1 ~ "M2M/S2S",
              H2H >=1 ~ "H2H",
              S2S >=1 ~ "S2S",
              M2M >=1 ~ "M2M",
              TRUE ~ NA_character_
            )
            ) 
          card_value <- df_map$value[1]
          card_label <- HTML("Modalities Used<br>Across All Campaigns")
        }
        
        incProgress(1)
      })
      
      return(list(card_value = card_value,
                  card_label = card_label))
    })
    output$average_indicator_card <- renderUI({
      avg_data <- avg_card_value_reactive()
      total_data <- total_campaigns_card_value_reactive()
      
      
      tagList(
        tags$style(HTML("
      .small-value-box .value {
        font-size: 12px !important; /* Decrease value font size */
        font-weight: bold;
      }
      .small-value-box .subtitle {
        font-size: 8px !important; /* Decrease label font size */
      }
    ")),
        
        valueBox(
          value = tags$div(class = "small-value-box", total_data),
          subtitle = tags$div(class = "small-value-box subtitle", HTML("Total Campaigns Conducted")),
          width = 12/2,
          color = "green"
        ),
        
        valueBox(
          value = tags$div(class = "small-value-box", avg_data$card_value),
          subtitle = tags$div(class = "small-value-box subtitle", avg_data$card_label),
          width = 12/2,
          color = "green"
        )
      )
    })
    
    
  })
}