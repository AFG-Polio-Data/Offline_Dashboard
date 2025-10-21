# modules/pre_table_module.R

preTableUI <- function(id) {
  ns <- NS(id)
  DT::DTOutput(ns("pre_table"))
}

preTableServer <- function(id,
                           pre_filtered_sia_data,
                           reactive_pre_indicator,
                           reactive_pre_form_type,
                           reactive_zoom_region,
                           reactive_zoom_province,
                           reactive_zoom_district) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    pre_table_data <- reactive({
      req(pre_filtered_sia_data())
      req(reactive_zoom_region())
      req(reactive_zoom_province())
      req(reactive_zoom_district())
      req(reactive_pre_indicator())
      req(reactive_pre_form_type())
      
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
      
      if (!("All" %in% reactive_zoom_district())) {
        data <- pre_district %>%
          filter(region == reactive_zoom_region(),
                 province == reactive_zoom_province(),
                 district == reactive_zoom_district()) %>%
          rename(Region = region,
                Province = province,
                District = district)
        group_vars <- c("Region", "Province", "District")
      } else if (!("All" %in% reactive_zoom_province())) {
        data <- pre_district %>%
          filter(region == reactive_zoom_region(),
                 province == reactive_zoom_province()) %>%
          rename(Region = region,
                 Province = province,
                 District = district)
        group_vars <- c("Region", "Province", "District")
      } else if (!("All" %in% reactive_zoom_region())) {
        data <- pre_province %>%
          filter(region == reactive_zoom_region()) %>%
          rename(Region = region,
                 Province = province)
        group_vars <- c("Region", "Province")
      } else{
        data <- pre_region %>%
          rename(Region = region)
        group_vars <- c("Region")
      }
      
      if(pre_var == "Total Training Sessions"){
        data <- data %>%
          select(all_of(group_vars), total_sessions_all, n_vol_sm_sessions, n_dc_sessions, n_cs_sessions) %>% 
          rename("Total Sessions" = total_sessions_all,
                 "Vol/SM Sessions" = n_vol_sm_sessions,
                 "DC Sessions" = n_dc_sessions,
                 "CS Sessions" = n_cs_sessions)
        # %>%
        #   mutate(across(
        #     .cols = c("Total Sessions", 
        #               "Vol/SM Sessions",
        #               "DC Sessions",
        #               "CS Sessions"),
        #     .fns = ~ scales::comma(.x, accuracy = 1)
        #   ))
        
        yaxis_title <- "Total Training Sessions Held"
      }
      if(pre_var == "Total Persons Trained"){
        data <- data %>%
          select(all_of(group_vars), total_attendance_all, total_volunteers, total_sm, total_dc, total_cs) %>% 
          rename("Total Trained" = total_attendance_all,
                 "Volunteers" = total_volunteers,
                 "Social Mobilizers" = total_sm,
                 "District Coordinators" = total_dc,
                 "Cluster Supervisors" = total_cs) 
        # %>%
        #   mutate(across(
        #     .cols = c("Total Trained", "Volunteers", "Social Mobilizers", "District Coordinators", "Cluster Supervisors"),
        #     .fns = ~ scales::comma(.x, accuracy = 1)
        #   ))
        
        yaxis_title <- "Total Persons Trained"
      }
      if(pre_var == "Volunteer/Social Mobilizer Profile"){
        data <- data %>%
          select(all_of(group_vars), vol_sm_pct_new, vol_sm_pct_female, vol_sm_pct_literate, vol_sm_pct_resident) %>%
          rename("New" = vol_sm_pct_new,
                 "Female" = vol_sm_pct_female,
                 "Literate" = vol_sm_pct_literate,
                 "Resident" = vol_sm_pct_resident) 
        # %>%
        #   mutate(across(
        #     .cols = c("New", "Female", "Literate", "Resident"),
        #     .fns = ~ scales::percent(.x, accuracy = 1)
        #   ))
        
        yaxis_title <- HTML("Percent of Vols and SMs")
      }
      if(pre_var == "Volunteer/Social Mobilizer Knowledge Score"){
        data <- data %>%
          select(all_of(group_vars), avg_total_score, avg_f1_score, avg_f2_score, avg_f3_score, avg_f4_score, avg_f5_score, avg_f6_score, avg_f7_score) %>%
          rename("Total Score" = avg_total_score,
                "Campaign Dates" = avg_f1_score,
                 "Target Age Groups" = avg_f2_score,
                 "Assigned Area" = avg_f3_score,
                 "Eligible Children" = avg_f4_score,
                 "Vaccine Vial Monitors" = avg_f5_score,
                 "Revisit Day Process" = avg_f6_score,
                 "Tally Sheet" = avg_f7_score) 
        # %>%
        #   mutate(across(
        #     .cols = c("Total Score", "Campaign Dates", "Target Age Groups", "Assigned Area",
        #               "Eligible Children", "Vaccine Vial Monitors", "Revisit Day Process", "Tally Sheet"),
        #     .fns = ~ scales::percent(.x, accuracy = 1)
        #   ))
        
        yaxis_title <- HTML("Percent with<br>Correct Knowledge")
      }
      
      return(data)
    })
    
    pre_table_group_vars <- reactive({
      req(pre_table_data())
      req(reactive_zoom_district())
      req(reactive_zoom_province())
      
        if (!("All" %in% reactive_zoom_province())) {
          group_vars <- c("Region", 
                          "Province", 
                          "District")
        } else{
          if (!("All" %in% reactive_zoom_region())) {
            group_vars <- c("Region", 
                            "Province")
          } else{
            group_vars <- c("Region")
          }
        }
      
      return(group_vars)
    })
    
    pre_table_df <- reactive({
      req(pre_table_group_vars())
      req(pre_table_data())
      req(reactive_zoom_district())
      req(reactive_zoom_province())
      req(reactive_pre_indicator())
      
      pre_var <- reactive_pre_indicator()
      
      withProgress(message = 'Calculation in progress...', value = 0, {
        
        group_vars <- pre_table_group_vars()
        data <- pre_table_data() 
        
        #Remove any empty columns
        data <- data %>%
          janitor::remove_empty(which = c("cols"))
        data <- data %>% select_if(function(col)
          ! all(col == 0))
        
      }) #End Progress
      return(data)
    })
    
    
    output$pre_table <- DT::renderDT({
      
      percent_cols <- c(
        "Total Score", "Campaign Dates", "Target Age Groups", "Assigned Area",
        "Eligible Children", "Vaccine Vial Monitors", "Revisit Day Process", "Tally Sheet",
        "New", "Female", "Literate", "Resident"
      )
      numeric_cols <- c(
        "Total Trained", "Volunteers", "Social Mobilizers", "District Coordinators", "Cluster Supervisors",
        "Total Sessions", "Vol/SM Sessions", "DC Sessions", "CS Sessions"
      )
      
      cols_to_convert <- intersect(percent_cols, colnames(pre_table_df()))
      
      data <- pre_table_df()
      
      cols_to_convert <- intersect(percent_cols, colnames(data))
      
      data[cols_to_convert] <- lapply(data[cols_to_convert], function(x) {
        as.numeric(x)
      })
      
      datatable_output <- DT::datatable(
        data = data,
        extensions = 'Buttons',
        callback = JS("$.fn.dataTable.ext.errMode = 'none';"),
        options = list(
          # columnDefs = list(list(targets = length(colnames(table_df)), visible = TRUE)),
          dom = 'Bfrtp',
          pageLength = -1,  # Display all rows on a single page
          scrollY = T,
          scrollX = T,
          stateSave = FALSE,
          server=FALSE,
          lengthMenu=list(c(-1),c("All")),
          paging=FALSE,
          buttons = list(
            list(
              extend = 'excel',
              text = 'Download Table',
              filename = "PreCampaign_Indicator_Summary",
              title = "PreCampaign Indicator Summary",
              exportOptions = list(
                modifier = list(page = 'all')
              )
            )
          )
        ),
        rownames = FALSE
      ) %>%
        DT::formatCurrency(
          columns = intersect(numeric_cols, colnames(data)),
          currency = "",        # No symbol
          digits = 0, 
          mark = ","            # Comma as thousands separator
        ) %>%
        DT::formatPercentage(
          columns = intersect(percent_cols, colnames(data)),
          digits = 0
        )
    })
  })
}