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

trendChartUI <- function(id) {
  ns <- NS(id)
    plotlyOutput(ns("trend_chart"), height = "455px")
 }

trendChartServer <- function(id, temporal_filtered_sia_data_v2, temporal_pretty_named_list, df_campaigns, 
                                       temporal_borders_district, temporal_borders_district_no_sia, campaign_rpd, 
                                       colors_coverage_bins2, colors_passfail_bins, colors_modality_bins, 
                                       colors_clusters_lt95_bin, colors_coverage_bins, colors_conversion_bins, temporal_legend_names_list, 
                                       input_temporal_indicator, input_temporal_map_base, input_zoom_region_select_temporal_v2, 
                                       input_zoom_province_select_temporal_v2, input_zoom_district_select_temporal_v2, selected_campaigns, shp_districts, selected_reason_type) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    
    trend_data_reactive <- reactive({
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
            group_by(campaign_name) %>%
            summarise(
              numerator = sum(numerator, na.rm = TRUE),
              denominator = sum(denominator, na.rm = TRUE),
              value = numerator / denominator,
              .groups = "drop"
            )
         
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
            mutate(value = total_coverage)
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
              group_by(campaign_name, indicator) %>%
              summarise(numerator = sum(numerator, na.rm=T),
                        denominator = max(denominator, na.rm=T)) %>%
              ungroup() %>%
              mutate(value = numerator/denominator)
          }

          df_map <- df_map %>%
            mutate(value = as.numeric(value)) %>%
            rowwise() %>%
            mutate(value = ifelse(selected_var == "pca_reasons_missed_rates", value*1000, value)) %>%
            ungroup() %>%
            ungroup() 
          
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
            group_by(campaign_name) %>%
            summarise(value = sum(total_vaccinated, na.rm = TRUE), .groups = "drop") %>%
            ungroup() 
          }
          if(selected_var %in% c("admin_target_pop")){
            df_map <- df_map %>%
              filter(age_group == "0-59 Months") %>%
              group_by(campaign_name) %>%
              summarise(value = sum(target_population, na.rm = TRUE), .groups = "drop") %>%
              ungroup() 
          }
         
        }
        
        # Process Categorical Indicators (Most Frequent Category)
        if (selected_var %in% c("pca_modality")) {
            df_map <- temporal_filtered_sia_data_v2()$district_indicators %>%
              dplyr::ungroup()
          df_map <- df_map %>%
            dplyr::mutate(numerator = ifelse(value == "H2H", 1, 0)) %>%
            dplyr::group_by(campaign_name) %>%
            dplyr::summarise(numerator=sum(numerator, na.rm=T),
                      total = n()) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(value = numerator/total)
            
        }
        if (selected_var %in% c("admin_modality")) {
            df_map <- temporal_filtered_sia_data_v2()$district
          df_map <- df_map %>%
            filter(age_group == "0-59 Months") %>% 
            mutate(numerator = ifelse(modality == "H2H", 1, 0)) %>%
            group_by(campaign_name) %>%
            summarise(numerator=sum(numerator, na.rm=T),
                      total = n()) %>%
            ungroup() %>%
            mutate(value = numerator/total)
            
        }
        
        incProgress(1)
      })
      df_map <- df_map %>%
        left_join(df_campaigns %>%
                    select(campaign_name, campaign_startdate)) %>%
        arrange(campaign_startdate)
      
      campaign_levels <- unique(df_map$campaign_name)
      
      df_map <- df_map %>%
        mutate(campaign_name = factor(campaign_name, levels=campaign_levels))
      return(df_map)
    })
    output$trend_chart <- renderPlotly({
      data <- trend_data_reactive()
      selected_var <- temporal_pretty_named_list[[input_temporal_indicator()]]
      selected_var <- selected_var[1]
      
      if(!(grepl("modality", selected_var)) &
         !(grepl("total_vaccinated", selected_var)) &
         !(grepl("target_pop", selected_var))){
        
        if(selected_var == "pca_pct_clusters_lt95_fm_cov"){
          y_axis_label <- HTML("PCA - Percent of Clusters<br>with OPV FM Coverage <95%") 
        }else{
        if(selected_var == "pca_completeness"){
          y_axis_label <- HTML("PCA - Reporting Completeness<br>(% of Clusters with Published Data)")
        } else{
          if(selected_var == "ooh_fm_coverage"){
            y_axis_label <- HTML("Out-of-house Survey<br>Finger-Mark Coverage (0-59m, OPV)")
          } else{
            if(selected_var == "pca_reasons_missed_rates"){
            y_axis_label <- HTML("PCA - Number Missed OPV due to<br>Selected Reason per 1000 Screened")  
            }else{
          y_axis_label <- input_temporal_indicator()
        }}}}
        
        data <- data %>%
          rowwise() %>%
        mutate(tooltip_text = ifelse(selected_var == "pca_reasons_missed_rates", paste0(campaign_name, ": ", round(value,0), " per 1000 screened"), paste0(campaign_name,": ", round(value,2)*100, "%"))) %>%
        ungroup() 
        
      if(selected_var == "pca_reasons_missed_rates"){
        plot <- ggplot(data=data)+
          geom_point(aes(x=campaign_name, y=value, text = tooltip_text)) +
          geom_line(aes(x=campaign_name, y=value, group=1)) +
          scale_y_continuous(labels = scales::comma,limits=c(0,max(data$value)+0.05*max(data$value))) + 
          labs( y = y_axis_label) +
          theme_minimal() +
          theme(
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1),
            legend.title = element_blank()
          )
      } else{ 
      plot <- ggplot(data=data)+
        geom_point(aes(x=campaign_name, y=value, text = tooltip_text)) +
        geom_line(aes(x=campaign_name, y=value, group=1)) +
        scale_y_continuous(labels = percent_format(scale = 100), limits=c(0,max(max(data$value)+0.05*max(data$value), 1.05))) +  # Format y-axis as percentage
        labs( y = y_axis_label) +
        theme_minimal() +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1),
          legend.title = element_blank()
        )
      }
      } else{
        if(!(grepl("modality", selected_var))){
          # Determine maximum value for formatting
          max_value <- max(data$value, na.rm = TRUE)
          
          # Define formatting function
          format_y_labels <- function(x) {
            if (max_value >= 1e6) {
              return(paste0(round(x / 1e6, 1), "M"))
            } else {
              return(formatC(x, format="f", big.mark = ",", digits=0))
            }
          }
          
          # Generate plot
          plot <- ggplot(data = data) +
            geom_point(aes(x = campaign_name, y = value, text=paste0(campaign_name,": ", scales::comma(value, accuracy=1)))) +
            geom_line(aes(x=campaign_name, y=value, group=1)) +
            scale_y_continuous(labels = format_y_labels, limits=c(0, max(data$value, na.rm=T)+(0.05*max(data$value, na.rm=T)))) +  # Apply dynamic formatting
            labs(y = input_temporal_indicator()) +
            theme_minimal() +
            theme(
              axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1),
              legend.title = element_blank()
            )
          
        } else{
          if((grepl("modality", selected_var))){
            data <- data %>%
              mutate(tooltip_text = paste0(campaign_name,": ", round(value,2)*100, "%"))
            
            
            plot <- ggplot(data=data)+
              geom_point(aes(x=campaign_name, y=value, text = tooltip_text)) +
              geom_line(aes(x=campaign_name, y=value, group=1)) +
              scale_y_continuous(labels = percent_format(scale = 100), limits=c(0,max(max(data$value)+0.05*max(data$value), 1.05))) +  # Format y-axis as percentage
              labs( y = HTML("Percent of Districts<br>Fully House-to-House")) +
              theme_minimal() +
              theme(
                axis.title.x = element_blank(),
                axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1),
                legend.title = element_blank()
              )
            
          }
        }
      }
      
      fig <- ggplotly(plot, tooltip="text") %>%
        layout(showlegend = FALSE) %>%
        plotly::config(displaylogo=FALSE,
                       modeBarButtons = (list(list("toImage"))))
      fig
    })
    
    
  })
}