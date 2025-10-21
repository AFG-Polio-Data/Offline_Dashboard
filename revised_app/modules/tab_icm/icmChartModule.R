icmChartUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("icm_chart"), height = "300px")
}

icmChartServer <- function(id, icm_filtered_sia_data_form_indicator_age_filtered,
                           reactive_icm_indicator, reactive_zoom_region, reactive_zoom_province, reactive_zoom_district) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

output$icm_chart <- renderPlotly({
  req(icm_filtered_sia_data_form_indicator_age_filtered())
  req(reactive_icm_indicator())
  req(reactive_zoom_region())
  req(reactive_zoom_province())
  req(reactive_zoom_district())
  
  icm_var <- reactive_icm_indicator()
  
  if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_cat_dist_region$indicator){
    indicator_type <- "cat_dist"
    if("All" %in% reactive_zoom_region()){
      data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_cat_dist_campaign %>%
        filter(indicator == icm_var) %>%
        ungroup()
    } else{
      if("All" %in% reactive_zoom_province()){
        data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_cat_dist_region %>%
          filter(indicator == icm_var) %>%
          ungroup()
      } else{
        if("All" %in% reactive_zoom_district()){
          data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_cat_dist_province %>%
            filter(indicator == icm_var) %>%
            ungroup()
        } else{
          data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_cat_dist_district %>%
            filter(indicator == icm_var) %>%
            ungroup()
        }}}
  } else{
    indicator_type <- "pct"
    if("All" %in% reactive_zoom_region()){
      if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_campaign_day_coverage$indicator){
        data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_campaign_day_coverage %>%
          filter(indicator == icm_var) %>%
          ungroup()
      } else{
        if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_day_campaign$indicator){
          data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_day_campaign %>%
            filter(indicator == icm_var) %>%
            ungroup()
        } 
      }
    } else{
      if("All" %in% reactive_zoom_province()){
        if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_region_day_coverage$indicator){
          data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_region_day_coverage %>%
            filter(indicator == icm_var) %>%
            ungroup()
        } else{
          if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_day_region$indicator){
            data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_day_region %>%
              filter(indicator == icm_var) %>%
              ungroup()
          } 
        }
      } else{
        if("All" %in% reactive_zoom_district()){
          if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_province_day_coverage$indicator){
            data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_province_day_coverage %>%
              filter(indicator == icm_var) %>%
              ungroup()
          } else{
            if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_day_province$indicator){
              data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_day_province %>%
                filter(indicator == icm_var) %>%
                ungroup()
            } 
          }
        } else{
          if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_district_day_coverage$indicator){
            data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_district_day_coverage %>%
              filter(indicator == icm_var) %>%
              ungroup()
          } else{
            if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_day_district$indicator){
              data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_day_district %>%
                filter(indicator == icm_var) %>%
                ungroup()
            } 
          }
        }}}
    
  }
  
  
  
  if(indicator_type == "cat_dist"){
    
    data <- data %>%
      mutate(value_cat = cat) %>%
      mutate(value_cat = factor(value_cat, levels=unique(data$cat))) %>%
      arrange(desc(value_cat)) %>%
      mutate(color = case_when(value_cat == "Correct" ~ "#4daf4a",
                               value_cat == "Incorrect" ~ "#ff7f00",
                               value_cat == "Not Marked" ~ "#e41a1c",
                               value_cat == "Child Absent" ~ "#7fc97f",
                               value_cat == "Newborn/Sick/Sleep" ~ "#beaed4",
                               value_cat == "Ignored by team" ~ "#386cb0",
                               value_cat == "Refusal" ~ "#ffff99",
                               value_cat == "Team not come" ~"#f0027f",
                               TRUE ~ "grey")) %>%
      mutate(denominator = sum(numerator, na.rm=T)) %>%
      mutate(pct = numerator / denominator) %>%
      mutate(tooltip_text = paste0(value_cat,": ", round(pct,2)*100,"% (", scales::comma(numerator,accuracy=1),"/", scales::comma(denominator,accuracy=1),")"))
    
    # Filter and process data2 for legend creation
    data2 <- data %>%
      distinct(color, value_cat) %>%
      arrange(value_cat)
    
    # Extract unique categories and colors
    icm_cat <- unique(data2$value_cat)
    icm_colors <- unique(data2$color)
    
    if (length(icm_cat) != length(icm_colors)) {
      # Force them to be the same length
      min_length <- min(length(icm_cat), length(icm_colors))
      icm_cat <- head(icm_cat, min_length)
      icm_colors <- head(icm_colors, min_length)
    }
    
    data <- data %>%
      select(-c("color"))
    
    if(grepl("Reasons", icm_var, ignore.case=T)){
      yaxis_text <- "Percent of Missed Children"
    }
    if(grepl("Door", icm_var, ignore.case=T)){
      yaxis_text <- "Percent of Houses"
    }
    
    plot <- ggplot(data = data) +
      geom_col(aes(x = value_cat, y = pct, fill = value_cat, text = tooltip_text), color = "black") +
      scale_fill_manual(values = icm_colors) +
      scale_y_continuous(labels = percent_format(scale = 100)) +  # Format y-axis as percentage
      labs(x = icm_var,
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
  if(indicator_type == "pct"){
    if(grepl("Coverage", icm_var)){
      y_axis_text <- "Coverage"
    } else{
      y_axis_text <- "Percent 'Yes'"
    }
    
    data <- data %>%
      filter(!(campaign_day %in% c("pre_sia", "post_sia", "Missing")) & !is.na(campaign_day)) %>%
      mutate(campaign_day = str_to_sentence(str_replace_all(campaign_day, "_"," "))) %>%
      mutate(campaign_day = factor(campaign_day, levels = c("Day 1", "Day 2", "Day 3", "Day 4", "Day 5", "Day 6", "Day 7"))) %>%
      mutate(tooltip_text = paste0(round(pct,2)*100,"% (", scales::comma(numerator, accuracy=1),"/",scales::comma(denominator, accuracy=1), ")"))
    
    plot <- ggplot(data=data) +
      geom_col(aes(x=campaign_day, y=pct, text = tooltip_text), fill="lightblue", color="black") +
      geom_text(aes(x = campaign_day, y = pct + (0.05 * max(as.numeric(pct), na.rm=T)),
                    label = scales::percent(pct, accuracy = 1)),
                vjust = -0.5) +  # Add labels above the columns
      scale_y_continuous(labels = percent_format(accuracy = 1), limits=c(0, max(data$pct, na.rm=T)+(0.25*max(data$pct, na.rm=T)))) +  # Format y-axis as percent
      theme_minimal() +
      labs(x = "Campaign Day",
           y = y_axis_text) +
      theme(
        axis.text.x = element_text(angle=55, vjust=1, hjust =1, margin = margin(t = -10))  # Adjust the margin to move labels closer
      )
    if(max(data$pct, na.rm=T) >= 0.9){
      plot <- plot +
        scale_y_continuous(labels = percent_format(accuracy = 1), limits=c(0, max(data$pct, na.rm=T)+(0.25*max(data$pct, na.rm=T))), breaks=c(0, 0.25, 0.5, 0.75, 1))
    }
    
    plotly_plot <- ggplotly(plot, tooltip="text")  %>%
      layout(showlegend = FALSE) %>%
      plotly::config(displaylogo=FALSE,
                     modeBarButtons = (list(list("toImage"))))
    
    # Disable tooltip for geom_text layer
    plotly_plot$x$data[[2]]$hoverinfo <- "none"
    # Decrease font size of geom_text layer
    plotly_plot$x$data[[2]]$textfont <- list(size = 12)  # Adjust the font size here
    fig <- plotly_plot
   
  }
  
  fig
})
})
}