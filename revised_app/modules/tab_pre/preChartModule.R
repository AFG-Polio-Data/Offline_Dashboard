preChartUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("pre_chart"))
}

preChartServer <- function(id, pre_filtered_sia_data, 
                             reactive_pre_indicator, 
                             reactive_pre_form_type,
                             reactive_zoom_region, reactive_zoom_province, reactive_zoom_district) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$pre_chart <- renderPlotly({
      req(pre_filtered_sia_data())
      req(reactive_pre_indicator())
      req(reactive_zoom_region())
      req(reactive_zoom_province())
      req(reactive_zoom_district())
      
      pre_var <- reactive_pre_indicator()
      pre_form_type <- reactive_pre_form_type()
      
      if(pre_form_type == "FLW Operation Kit"){
        pre_district <- pre_filtered_sia_data()$precampaign_flw_district
        pre_province <- pre_filtered_sia_data()$precampaign_flw_province
        pre_region <- pre_filtered_sia_data()$precampaign_flw_region
        pre_national <- pre_filtered_sia_data()$precampaign_flw_national
      }
      if(pre_form_type == "Training Monitoring"){
        pre_district <- pre_filtered_sia_data()$training_district
        pre_province <- pre_filtered_sia_data()$training_province
        pre_region <- pre_filtered_sia_data()$training_region
        pre_national <- pre_filtered_sia_data()$training_national
      }
      
      data <- if (!("All" %in% reactive_zoom_district())) {
        pre_district %>%
          filter(region == reactive_zoom_region(),
                 province == reactive_zoom_province(),
                 district == reactive_zoom_district())
      } else if (!("All" %in% reactive_zoom_province())) {
        pre_province %>%
          filter(region == reactive_zoom_region(),
                 province == reactive_zoom_province())
      } else if (!("All" %in% reactive_zoom_region())) {
        pre_region %>%
          filter(region == reactive_zoom_region())
      } else{
        pre_national
      }
      
      if(pre_var == "Total Training Sessions"){
        data <- data %>%
          select(n_vol_sm_sessions, n_dc_sessions, n_cs_sessions) %>% 
          rename("Vol/SM Sessions" = n_vol_sm_sessions,
                 "DC Sessions" = n_dc_sessions,
                 "CS Sessions" = n_cs_sessions) %>%
          pivot_longer(cols=c("Vol/SM Sessions", "DC Sessions", "CS Sessions")) %>%
          mutate(label = scales::comma(value, accuracy=1),
                 name = factor(name, levels=c("Vol/SM Sessions", "CS Sessions", "DC Sessions")))
          
        yaxis_title <- "Total Training Sessions Held"
      }
      if(pre_var == "Total Persons Trained"){
        data <- data %>%
          select(total_volunteers, total_sm, total_dc, total_cs) %>% 
          rename("Volunteers" = total_volunteers,
                 "Social Mobilizers" = total_sm,
                 "District Coordinators" = total_dc,
                 "Cluster Supervisors" = total_cs) %>%
          pivot_longer(cols=c("Volunteers", "Social Mobilizers", "District Coordinators", "Cluster Supervisors")) %>%
          mutate(label = scales::comma(value, accuracy=1),
                 name = factor(name, levels=c("Volunteers", "Social Mobilizers", "Cluster Supervisors", "District Coordinators")))
        
        yaxis_title <- "Total Persons Trained"
      }
      if(pre_var == "Volunteer/Social Mobilizer Profile"){
        data <- data %>%
          select(vol_sm_pct_new, vol_sm_pct_female, vol_sm_pct_literate, vol_sm_pct_resident) %>%
          rename("New" = vol_sm_pct_new,
                "Female" = vol_sm_pct_female,
                "Literate" = vol_sm_pct_literate,
                "Resident" = vol_sm_pct_resident) %>%
          pivot_longer(cols=c("New", "Female", "Literate", "Resident")) %>%
          mutate(name = factor(name, levels=c("New", "Female", "Literate", "Resident")),
                label = scales::percent(value, accuracy=1))
        
        yaxis_title <- HTML("Percent of Vols and SMs")
      }
      if(pre_var == "Volunteer/Social Mobilizer Knowledge Score"){
        data <- data %>%
          select(avg_f1_score, avg_f2_score, avg_f3_score, avg_f4_score, avg_f5_score, avg_f6_score, avg_f7_score) %>%
          rename("Campaign Dates" = avg_f1_score,
                 "Target Age Groups" = avg_f2_score,
                 "Assigned Area" = avg_f3_score,
                 "Eligible Children" = avg_f4_score,
                 "Vaccine Vial Monitors" = avg_f5_score,
                 "Revisit Day Process" = avg_f6_score,
                 "Tally Sheet" = avg_f7_score) %>%
          pivot_longer(cols=c("Campaign Dates", "Target Age Groups", "Assigned Area", "Eligible Children", "Vaccine Vial Monitors", "Revisit Day Process", "Tally Sheet")) %>%
          mutate(
            name = factor(name, levels = c(
              "Campaign Dates", "Target Age Groups", "Assigned Area",
              "Eligible Children", "Vaccine Vial Monitors", "Revisit Day Process", "Tally Sheet"
            )),
            label = scales::percent(value, accuracy=1))
        
        yaxis_title <- HTML("Percent with<br>Correct Knowledge")
      }
      
      
      # Step 3: ggplot base
      y_range <- max(data$value, na.rm = TRUE)
      label_offset <- 0.04 * y_range  # 2% of the range
      
      
      plot <- ggplot(data, aes(x = name, y = value, text=NULL)) +
        geom_col(fill="steelblue", color = "black", width = 0.7, alpha=0.7) +
        geom_text(aes(label = label), vjust = 0, nudge_y = label_offset, size = 3.5) +
        labs(x = NULL, y = yaxis_title) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "none"
        )
      if(pre_var %in% c("Volunteer/Social Mobilizer Knowledge Score", "Volunteer/Social Mobilizer Profile")){
        plot <- plot +
        scale_y_continuous(
          labels = percent_format(accuracy = 1),
          limits = c(0, 1.15),
          breaks = seq(0, 1, by = 0.2)  # 0%, 20%, ..., 100%
        ) 
      }
      
      fig <- ggplotly(plot, tooltip="none") %>%
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
      
    })
  })
}