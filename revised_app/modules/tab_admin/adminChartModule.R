adminChartUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("admin_chart"), height = "300px")
}

adminChartServer <- function(id, admin_filtered_sia_data_age_filtered, 
                             reactive_admin_indicator, 
                             reactive_zoom_region, reactive_zoom_province, reactive_zoom_district) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$admin_chart <- renderPlotly({
      req(admin_filtered_sia_data_age_filtered())
      req(reactive_admin_indicator())
      req(reactive_zoom_region())
      req(reactive_zoom_province())
      req(reactive_zoom_district())
      # withProgress(message = 'Calculation in progress...',
      #              value = 0, {

                     admin_var <- reactive_admin_indicator()
                     if(admin_var %in%  c("Vaccine Wastage", "Site Density (S2S Only)", "HRMP Vaccinated", "HRMP Percent of Total Vaccinated")){
                       data <- admin_filtered_sia_data_age_filtered()$cluster
                       data <- data %>%
                         ungroup()
                     } else{
                       if(admin_var == "Reporting Completeness (% of Clusters with Complete Data)"){
                         if(!("All" %in% reactive_zoom_district())){
                           data <- admin_filtered_sia_data_age_filtered()$district_completeness
                         } else{
                           if(!("All" %in% reactive_zoom_province())){
                             data <- admin_filtered_sia_data_age_filtered()$province_completeness
                           } else{
                             if(!("All" %in% reactive_zoom_region())){
                               data <- admin_filtered_sia_data_age_filtered()$region_completeness
                             } else{
                               data <- admin_filtered_sia_data_age_filtered()$national_completeness
                             }
                           }
                         }
                       }else{
                       if(admin_var %in%  c("Target Population", "Modality")){
                         data <- admin_filtered_sia_data_age_filtered()$district
                         data <- data %>%
                           ungroup()
                         if(admin_var %in% c("Modality") & !("All" %in% reactive_zoom_district())){
                           data <- admin_filtered_sia_data_age_filtered()$cluster
                           data <- data %>%
                             ungroup()
                         }
                       } else{
                         if(!("All" %in% reactive_zoom_district())){
                           if(admin_var %in% c("Remaining Recorded Missed", "Missed Child Conversion")){

                             data <- admin_filtered_sia_data_age_filtered()$conversion_district
                           } else{
                             data <- admin_filtered_sia_data_age_filtered()$district
                           }
                           data <- data %>%
                             ungroup()
                         } else{
                        if(!("All" %in% reactive_zoom_province())){
                             if(admin_var %in% c("Remaining Recorded Missed", "Missed Child Conversion")){
                               data <- admin_filtered_sia_data_age_filtered()$conversion_province
                             } else{
                               data <- admin_filtered_sia_data_age_filtered()$province
                             }
                         data <- data %>%
                           ungroup()
                        } else{
                         if(!("All" %in% reactive_zoom_region())){
                           if(admin_var %in% c("Remaining Recorded Missed", "Missed Child Conversion")){
                             data <- admin_filtered_sia_data_age_filtered()$conversion_region
                           } else{
                             data <- admin_filtered_sia_data_age_filtered()$region
                           }
                           data <- data %>%
                             ungroup()
                         } else{
                           if(admin_var %in% c("Remaining Recorded Missed", "Missed Child Conversion")){
                             data <- admin_filtered_sia_data_age_filtered()$conversion_national
                           } else{
                             data <- admin_filtered_sia_data_age_filtered()$national
                           }
                           data <- data %>%
                             ungroup()
                         }
                          }}}}}

                       if(admin_var == "Cumulative Coverage"){
                         data <- data %>%
                           select(campaign_name, cum_cov_day1, cum_cov_day2, cum_cov_day3, cum_cov_day4, cum_cov_day5, cum_cov_day6, cum_cov_day7) %>%
                           pivot_longer(cols=-c("campaign_name"), names_to = "Campaign Day", values_to = "Cumulative Coverage") %>%
                           filter(!is.na(`Cumulative Coverage`)) %>%
                           mutate(`Campaign Day` = str_remove_all(`Campaign Day`, "cum_cov_"),
                                  `Campaign Day` = str_replace_all(`Campaign Day`, "day", "Day "))
                       }
                       if(admin_var == "Reporting Completeness (% of Clusters with Complete Data)"){
                         data <- data %>%
                           select(campaign_name, day1_pct_reported, day2_pct_reported, day3_pct_reported, day4_pct_reported, day5_pct_reported, day6_pct_reported, day1_pct_reported) %>%
                           pivot_longer(cols=-c("campaign_name"), names_to = "Campaign Day", values_to = "Clusters Reported") %>%
                           filter(!is.na(`Clusters Reported`)) %>%
                           mutate(`Campaign Day` = str_remove_all(`Campaign Day`, "_pct_reported"),
                                  `Campaign Day` = str_replace_all(`Campaign Day`, "day", "Day ")) %>%
                           filter(!(`Campaign Day` == "Day 4" & `Clusters Reported` == 0) &
                                    !(`Campaign Day` == "Day 5" & `Clusters Reported` == 0) &
                                    !(`Campaign Day` == "Day 6" & `Clusters Reported` == 0) &
                                    !(`Campaign Day` == "Day 7" & `Clusters Reported` == 0))
                       }
                       if(admin_var == "Total Vaccinated"){
                         data <- data %>%
                           select(campaign_name, total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, total_children_vaccinated_day5, total_children_vaccinated_day6, total_children_vaccinated_day7) %>%
                           pivot_longer(cols=-c("campaign_name"), names_to = "Campaign Day", values_to = "Children Vaccinated") %>%
                           filter(!is.na(`Children Vaccinated`) & `Children Vaccinated` != 0) %>%
                           mutate(`Campaign Day` = str_remove_all(`Campaign Day`, "total_children_vaccinated_"),
                                  `Campaign Day` = str_replace_all(`Campaign Day`, "day", "Day "))
                       }
                       if(admin_var == "Vaccine Wastage"){
                         data <- data %>%
                           mutate(value = vacc_wastage) %>%
                           select(rcode, pcode, dcode, ccode, value) %>%
                           filter(value >= -2 & value <= 2)
                         y_axis_label <- "Number of Clusters"
                         x_axis_label <- "Vaccine Wastage (%)"

                       }
                     if(admin_var == "HRMP Vaccinated"){
                       data <- data %>%
                         mutate(value = hrmp_vaccinated) %>%
                         select(rcode, pcode, dcode, ccode, value)
                       y_axis_label <- "Number of Clusters"
                       x_axis_label <- "Total HRMP Vaccinated"
                       
                     }
                     if(admin_var == "HRMP Percent of Total Vaccinated"){
                       data <- data %>%
                         mutate(value = pct_hrmp) %>%
                         select(rcode, pcode, dcode, ccode, value)
                       y_axis_label <- "Number of Clusters"
                       x_axis_label <- "% HRMP, of Vaccinated"
                       
                     }
                       if(admin_var == "Target Population"){
                         data <- data %>%
                           mutate(value = target_population) %>%
                           select(value)

                         y_axis_label <- "Number of Districts"
                         x_axis_label <- "Target Population"
                       }
                     if(admin_var == "Site Density (S2S Only)"){
                       data <- data %>%
                         mutate(value = round(site_density, 0)) %>%
                         select(value)

                       y_axis_label <- "Number of S2S Clusters"
                       x_axis_label <- "Avg. Number Vaccinated per Site"
                     }
                       if(admin_var == "Remaining Recorded Missed"){
                         data <- data %>%
                           select(campaign_name, 
                                  
                                  recorded_missed_reason_absent1, 
                                  recorded_missed_reason_absent1_vaccinated, 
                                  
                                  recorded_missed_reason_absent2, 
                                  recorded_missed_reason_absent2_vaccinated, 
                                  
                                  recorded_missed_reason_nss, 
                                  recorded_missed_reason_nss_vaccinated, 
                                  
                                  recorded_missed_reason_refusal, 
                                  recorded_missed_reason_refusal_vaccinated, 
                                  
                                  recorded_missed_total, recorded_missed_total_vaccinated
                                  
                                  
                           ) %>%
                           mutate(remaining_missed_reason_absent1 = recorded_missed_reason_absent1 - recorded_missed_reason_absent1_vaccinated, 
                                  remaining_missed_reason_absent2 = recorded_missed_reason_absent2 - recorded_missed_reason_absent2_vaccinated, 
                                  remaining_missed_reason_nss = recorded_missed_reason_nss - recorded_missed_reason_nss_vaccinated, 
                                  remaining_missed_reason_refusal = recorded_missed_reason_refusal - recorded_missed_reason_refusal_vaccinated, 
                                  remaining_missed_total = recorded_missed_total - recorded_missed_total_vaccinated) %>%
                           select(campaign_name,
                                  remaining_missed_reason_absent1, remaining_missed_reason_absent2, remaining_missed_reason_nss, remaining_missed_reason_refusal,
                                  remaining_missed_total) %>%
                           pivot_longer(cols = -c("campaign_name"),
                                        names_to = "group",
                                        values_to = "value") %>%
                           mutate(group = case_when(group == "remaining_missed_reason_absent1" ~ "Absent (Return during Campaign)",
                                                    group == "remaining_missed_reason_absent2" ~ "Absent (Return after Campaign)",
                                                    group == "remaining_missed_reason_nss" ~ "Newborn/Sleep/Sick",
                                                    group == "remaining_missed_reason_refusal" ~ "Refusal",
                                                    group == "remaining_missed_total" ~ "Total"),
                                  type = case_when(grepl("Day ", group) ~ "Campaign Day",
                                                   grepl("Total", group) ~ "Total",
                                                   TRUE ~ "Reason Missed")) %>%
                           filter(!is.na(value)) %>%
                           mutate(label = value)
                         
                       }
                     if(admin_var == "Missed Child Conversion"){
                       data_value <- data %>%
                         select(campaign_name, recorded_missed_reason_absent1_conversion_pct, recorded_missed_reason_absent2_conversion_pct, recorded_missed_reason_nss_conversion_pct, recorded_missed_reason_refusal_conversion_pct,
                                # recorded_missed_day1_conversion_pct, recorded_missed_day2_conversion_pct, recorded_missed_day3_conversion_pct, recorded_missed_day4_conversion_pct, recorded_missed_day5_conversion_pct, recorded_missed_day6_conversion_pct,
                                recorded_missed_total_conversion_pct
                                ) %>%
                         pivot_longer(cols = -c("campaign_name"),
                                      names_to = "group",
                                      values_to = "pct") %>%
                         mutate(group = case_when(group == "recorded_missed_reason_absent1_conversion_pct" ~ "Absent (Return during Campaign)",
                                                  group == "recorded_missed_reason_absent2_conversion_pct" ~ "Absent (Return after Campaign)",
                                                  group == "recorded_missed_reason_nss_conversion_pct" ~ "Newborn/Sleep/Sick",
                                                  group == "recorded_missed_reason_refusal_conversion_pct" ~ "Refusal",
                                                  # group == "recorded_missed_day1_conversion_pct" ~ "Day 1",
                                                  # group == "recorded_missed_day2_conversion_pct" ~ "Day 2",
                                                  # group == "recorded_missed_day3_conversion_pct" ~ "Day 3",
                                                  # group == "recorded_missed_day4_conversion_pct" ~ "Day 4",
                                                  # group == "recorded_missed_day5_conversion_pct" ~ "Day 5",
                                                  # group == "recorded_missed_day6_conversion_pct" ~ "Day 6",
                                                  group == "recorded_missed_total_conversion_pct" ~ "Total"),
                                type = case_when(grepl("Day ", group) ~ "Campaign Day",
                                                 grepl("Total", group) ~ "Total",
                                                 TRUE ~ "Reason Missed")) %>%
                         filter(!is.na(pct) & pct != 0)


                       data_label <- data %>%
                         rowwise() %>%
                         mutate(recorded_missed_reason_absent1_conversion_pct = paste0(round(recorded_missed_reason_absent1_conversion_pct,2)*100, "% (", recorded_missed_reason_absent1_vaccinated ,"/", recorded_missed_reason_absent1,")"),
                                recorded_missed_reason_absent2_conversion_pct = paste0(round(recorded_missed_reason_absent2_conversion_pct,2)*100, "% (", recorded_missed_reason_absent2_vaccinated ,"/", recorded_missed_reason_absent2,")"),
                                recorded_missed_reason_nss_conversion_pct = paste0(round(recorded_missed_reason_nss_conversion_pct,2)*100, "% (", recorded_missed_reason_nss_vaccinated ,"/", recorded_missed_reason_nss,")"),
                                recorded_missed_reason_refusal_conversion_pct = paste0(round(recorded_missed_reason_refusal_conversion_pct,2)*100, "% (", recorded_missed_reason_refusal_vaccinated ,"/", recorded_missed_reason_refusal,")"),
                                # recorded_missed_day1_conversion_pct = paste0(round(recorded_missed_day1_conversion_pct,2)*100, "% (", recorded_missed_day1_vaccinated ,"/", recorded_missed_day1,")"),
                                # recorded_missed_day2_conversion_pct = paste0(round(recorded_missed_day2_conversion_pct,2)*100, "% (", recorded_missed_day2_vaccinated ,"/", recorded_missed_day2,")"),
                                # recorded_missed_day3_conversion_pct = paste0(round(recorded_missed_day3_conversion_pct,2)*100, "% (", recorded_missed_day3_vaccinated ,"/", recorded_missed_day3,")"),
                                # recorded_missed_day4_conversion_pct = paste0(round(recorded_missed_day4_conversion_pct,2)*100, "% (", recorded_missed_day4_vaccinated ,"/", recorded_missed_day4,")"),
                                # recorded_missed_day5_conversion_pct = paste0(round(recorded_missed_day5_conversion_pct,2)*100, "% (", recorded_missed_day5_vaccinated ,"/", recorded_missed_day5,")"),
                                # recorded_missed_day6_conversion_pct = paste0(round(recorded_missed_day6_conversion_pct,2)*100, "% (", recorded_missed_day6_vaccinated ,"/", recorded_missed_day6,")"),
                                recorded_missed_total_conversion_pct = paste0(round(recorded_missed_total_conversion_pct,2)*100, "% (", recorded_missed_total_vaccinated ,"/", recorded_missed_total,")")
                         ) %>%
                         select(campaign_name, recorded_missed_reason_absent1_conversion_pct, recorded_missed_reason_absent2_conversion_pct, recorded_missed_reason_nss_conversion_pct, recorded_missed_reason_refusal_conversion_pct,
                                # recorded_missed_day1_conversion_pct, recorded_missed_day2_conversion_pct, recorded_missed_day3_conversion_pct, recorded_missed_day4_conversion_pct, recorded_missed_day5_conversion_pct, recorded_missed_day6_conversion_pct,
                                recorded_missed_total_conversion_pct
                         ) %>%
                         pivot_longer(cols = -c("campaign_name"),
                                      names_to = "group",
                                      values_to = "label") %>%
                         mutate(group = case_when(group == "recorded_missed_reason_absent1_conversion_pct" ~ "Absent (Return during Campaign)",
                                                  group == "recorded_missed_reason_absent2_conversion_pct" ~ "Absent (Return after Campaign)",
                                                  group == "recorded_missed_reason_nss_conversion_pct" ~ "Newborn/Sleep/Sick",
                                                  group == "recorded_missed_reason_refusal_conversion_pct" ~ "Refusal",
                                                  # group == "recorded_missed_day1_conversion_pct" ~ "Day 1",
                                                  # group == "recorded_missed_day2_conversion_pct" ~ "Day 2",
                                                  # group == "recorded_missed_day3_conversion_pct" ~ "Day 3",
                                                  # group == "recorded_missed_day4_conversion_pct" ~ "Day 4",
                                                  # group == "recorded_missed_day5_conversion_pct" ~ "Day 5",
                                                  # group == "recorded_missed_day6_conversion_pct" ~ "Day 6",
                                                  group == "recorded_missed_total_conversion_pct" ~ "Total"),
                                type = case_when(grepl("Day ", group) ~ "Campaign Day",
                                                 grepl("Total", group) ~ "Total",
                                                 TRUE ~ "Reason Missed"))


                       data <- data_value %>%
                         left_join(data_label,
                                   by=c("campaign_name", "group", "type"))
                     }
                     if(admin_var == "Modality"){
                       data <- data %>%
                         select(campaign_name, modality) %>%
                         group_by(campaign_name, modality) %>%
                         summarise(district_count = n()) %>%
                         ungroup() %>%
                         mutate(pct = round(district_count / sum(district_count), 2))
                     }


                     #Create Plot
                     if(admin_var == "Cumulative Coverage"){
                       plot <- ggplot(data=data) +
                         geom_col(aes(x=`Campaign Day`, y=`Cumulative Coverage`), fill="lightblue", color="black") +
                         geom_text(aes(x = `Campaign Day`, y = `Cumulative Coverage` + (0.05 * max(as.numeric(`Cumulative Coverage`), na.rm=T)),
                                       label = scales::percent(`Cumulative Coverage`, accuracy = 1)),
                                   vjust = -0.5) +  # Add labels above the columns
                        scale_y_continuous(labels = percent_format(accuracy = 1), limits=c(0, max(data$`Cumulative Coverage`, na.rm=T)+0.25), breaks=c(0, 0.25, 0.5, 0.75, 1)) +  # Format x-axis as percent
                        theme_minimal() +
                        theme(
                          axis.text.x = element_text(margin = margin(t = -10))  # Adjust the margin to move labels closer
                        )
                        plotly_plot <- ggplotly(plot, tooltip=NULL)
                        # Disable tooltip for geom_text layer
                        plotly_plot$x$data[[2]]$hoverinfo <- "none"
                        # Decrease font size of geom_text layer
                        plotly_plot$x$data[[2]]$textfont <- list(size = 12)  # Adjust the font size here

                     }
                     if(admin_var == "Reporting Completeness (% of Clusters with Complete Data)"){
                       plot <- ggplot(data=data) +
                         geom_col(aes(x=`Campaign Day`, y=`Clusters Reported`), fill="lightblue", color="black") +
                         geom_text(aes(x = `Campaign Day`, y = `Clusters Reported` + (0.05 * max(as.numeric(`Clusters Reported`), na.rm=T)),
                                       label = scales::percent(`Clusters Reported`, accuracy = 1)),
                                   vjust = -0.5) +  # Add labels above the columns
                         scale_y_continuous(labels = percent_format(accuracy = 1), limits=c(0, max(data$`Clusters Reported`, na.rm=T)+0.25), breaks=c(0, 0.25, 0.5, 0.75, 1)) +  # Format x-axis as percent
                         theme_minimal() +
                         theme(
                           axis.text.x = element_text(margin = margin(t = -10))  # Adjust the margin to move labels closer
                         )
                       plotly_plot <- ggplotly(plot, tooltip=NULL)
                       # Disable tooltip for geom_text layer
                       plotly_plot$x$data[[2]]$hoverinfo <- "none"
                       # Decrease font size of geom_text layer
                       plotly_plot$x$data[[2]]$textfont <- list(size = 12)  # Adjust the font size here
                       
                     }
                     if(admin_var == "Total Vaccinated"){
                       plot <- ggplot(data=data) +
                         geom_col(aes(x=`Campaign Day`, y=`Children Vaccinated`), fill="lightblue", color="black") +
                         geom_text(aes(x = `Campaign Day`, y = `Children Vaccinated` + (0.05*max(as.numeric(`Children Vaccinated`), na.rm=T)),
                                       label = `Children Vaccinated`),
                                   vjust = -0.5) +  # Add labels above the columns
                         theme_minimal() +
                         theme(
                           axis.text.x = element_text(margin = margin(t = -10))  # Adjust the margin to move labels closer
                         ) +
                         scale_y_continuous(labels = scales::comma)   # Format x-axis as comma-value
                       plotly_plot <- ggplotly(plot, tooltip=NULL)
                       # Disable tooltip for geom_text layer
                       plotly_plot$x$data[[2]]$hoverinfo <- "none"
                       # Decrease font size of geom_text layer
                       plotly_plot$x$data[[2]]$textfont <- list(size = 12)  # Adjust the font size here
                     }
                     if(admin_var %in% c("Vaccine Wastage", "Target Population", "Site Density (S2S Only)", "HRMP Percent of Total Vaccinated", "HRMP Vaccinated")){
                       plot <- ggplot(data=data) +
                         geom_histogram(aes(x=as.numeric(value)), fill="lightblue", color="black") +
                         labs(x=x_axis_label,
                              y=y_axis_label) +
                         theme_minimal() +
                         theme(
                           axis.text.x = element_text(margin = margin(t = -10))  # Adjust the margin to move labels closer
                         )
                       if(admin_var %in% c("Target Population", "Site Density (S2S Only)", "HRMP Vaccinated")){
                         plot <- plot +
                           scale_y_continuous(labels = scales::comma)   # Format x-axis as comma-value
                       }
                       if(admin_var %in% c("HRMP Percent of Total Vaccinated", "Vaccine Wastage")){
                         plot <- plot +
                           scale_y_continuous(labels = scales::comma) +
                           scale_x_continuous(labels = percent_format(accuracy = 1))
                       }
                       plotly_plot <- ggplotly(plot, tooltip=NULL)
                     }
                     if(admin_var == "Remaining Recorded Missed"){
                       data <- data %>%
                         mutate(group = case_when(
                           group == "Newborn/Sleep/Sick" ~ "Newborn/Sleep/Sick",
                           group == "Absent (Return after Campaign)" ~ "Absent (Return After)",
                           group == "Absent (Return during Campaign)" ~ "Absent (Return During)",
                           TRUE ~ group
                         )) %>%
                         mutate(group = factor(group, levels = c("Absent (Return During)",
                                                                 "Absent (Return After)",
                                                                 "Newborn/Sleep/Sick",
                                                                 "Refusal",
                                                                 "Total"))) %>%
                         arrange(group)
                       color_mapping <- c(
                         "Reason Missed" = "#66c2a5",  # Red
                         "Total" = "#8da0cb"          # Blue
                       )
                       filtered_colors <- color_mapping[names(color_mapping) %in% unique(data$type)]
                       
                       plot <- ggplot(data=data) +
                         geom_col(aes(x=group, y=`value`, text=label, group=type, fill=type), color="black") +
                         # facet_wrap(~type, nrow =1) +
                         # geom_text(aes(x = `Reason Missed`, y = `pct` + (0.05*max(as.numeric(data$`pct`), na.rm=T)), label = `label`),
                         #           vjust = -0.5) +  # Add labels above the columns
                         labs(y="Remaining Unvaccinated",
                              x="") +
                         theme_minimal() +
                         theme(
                           axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1, margin = margin(t = -10)),  # Rotate x-axis text
                           legend.position = "none"
                         ) +
                         scale_y_continuous(labels = scales::comma) +
                         scale_fill_manual(values = filtered_colors)
                       
                       plotly_plot <- ggplotly(plot, tooltip="text")
                     }
                     if(admin_var == "Missed Child Conversion"){

                       data <- data %>%
                         mutate(group = case_when(
                           group == "Newborn/Sleep/Sick" ~ "Newborn/Sleep/Sick",
                           group == "Absent (Return after Campaign)" ~ "Absent (Return After)",
                           group == "Absent (Return during Campaign)" ~ "Absent (Return During)",
                           TRUE ~ group
                         )) %>%
                         mutate(group = factor(group, levels = c("Absent (Return During)",
                                                                                     "Absent (Return After)",
                                                                                     "Newborn/Sleep/Sick",
                                                                                     "Refusal",
                                                                 "Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6",
                                                                 "Total"))) %>%
                         arrange(group)
                       color_mapping <- c(
                         "Reason Missed" = "#66c2a5",  # Red
                         "Campaign Day" = "#fc8d62",   # Green
                         "Total" = "#8da0cb"          # Blue
                       )
                       filtered_colors <- color_mapping[names(color_mapping) %in% unique(data$type)]

                       plot <- ggplot(data=data) +
                         geom_col(aes(x=group, y=`pct`, text=label, group=type, fill=type), color="black") +
                         # facet_wrap(~type, nrow =1) +
                         # geom_text(aes(x = `Reason Missed`, y = `pct` + (0.05*max(as.numeric(data$`pct`), na.rm=T)), label = `label`),
                         #           vjust = -0.5) +  # Add labels above the columns
                         labs(y="% Vaccinated",
                              x="") +
                         theme_minimal() +
                         theme(
                           axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1, margin = margin(t = -10)),  # Rotate x-axis text
                           legend.position = "none"
                         ) +
                         scale_y_continuous(labels = percent_format(scale = 100), limits=c(0,1)) +  # Format y-axis as percentage
                         scale_fill_manual(values = filtered_colors)

                       plotly_plot <- ggplotly(plot, tooltip="text")
                       # Disable tooltip for geom_text layer
                       # plotly_plot$x$data[[2]]$hoverinfo <- "none"
                       # Decrease font size of geom_text layer
                       # plotly_plot$x$data[[2]]$textfont <- list(size = 12)  # Adjust the font size here
                     }
                     if(admin_var == "Modality"){
                       admin_colors <- colors_modality_bins[names(colors_modality_bins) %in% data$modality]
                       if(!("All" %in% reactive_zoom_district())){
                         yaxis_text = "Percent of Clusters"
                       } else{
                         yaxis_text = "Percent of Districts"
                       }

                       plot <- ggplot(data = data) +
                         geom_col(aes(x = modality, y = pct, fill = modality, text = paste0(round(pct*100,0),"% (",district_count,"/",sum(district_count,na.rm=t),")")), color = "black") +
                         scale_fill_manual(values = admin_colors) +
                         scale_y_continuous(labels = percent_format(scale = 100), limits=c(0,1)) +  # Format y-axis as percentage
                         labs(x = "Modality",
                              y = yaxis_text) +
                         theme_minimal() +
                         theme(
                           axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1),
                           legend.title = element_blank()
                         ) +
                         guides(fill = guide_legend(override.aes = list(colour = NULL)))  # Remove unused legend items

                       plotly_plot <- ggplotly(plot, tooltip="text")
                     }

                     # }) #End Progress
                     if(admin_var != "Modality"){
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
                     } else{
                       fig <- plotly_plot %>%
                         layout(showlegend = FALSE)
                     }
                     fig <- fig %>%
                       plotly::config(displaylogo=FALSE,
                              modeBarButtons = (list(list("toImage"))))
                     fig

    })
  })
}