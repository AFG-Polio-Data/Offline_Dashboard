preCardUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("pre_card"))
}

# "Total FLWs",
# "Vaccinators per 1000 Target Population",
# "Social Mobilizers per 1000 Target Population",
# "% of FLWs Newly Selected",
# "% of FLWs Female",
# "% of FLWS Resident of Assigned Area",
# "% of FLWs Paid for Last SIA"

preCardServer <- function(id,
                          pre_filtered_sia_data,
                          reactive_pre_indicator,
                          reactive_pre_form_type,
                          reactive_zoom_region,
                          reactive_zoom_province,
                          reactive_zoom_district) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$pre_card <- renderUI({
      
      req(reactive_pre_indicator())
      req(reactive_pre_form_type())
  
      req(pre_filtered_sia_data())
      req(reactive_zoom_district())
      req(reactive_zoom_province())
      req(reactive_zoom_region())
      
      withProgress(message = 'Calculation in progress...', value = 0, {
      
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
        
        
        
        if (pre_var == "Total FLWs") {
          value <- formatC(
            as.numeric(data$total_flws),
            big.mark = ",",
            format = "f",
            digits = 0
          )
          label <- "Total FLWs"
        }
        if (pre_var == "Vaccinators per 1000 Target Population") {
          value <- paste0(round(as.numeric(data$vaccinators_per_1000), 1))
          label <- HTML("Vaccinators<br>per 1000 Target Population")
        }
        if (pre_var == "Social Mobilizers per 1000 Target Population") {
          value <- paste0(round(as.numeric(
            data$social_mobilizers_per_1000
          ), 1))
          label <- "Social Mobilizers<br>per 1000 Target Population"
        }
        if (pre_var == "% of FLWs Newly Selected") {
          value <- paste0(round(data$flws_pct_newly_selected[1], 2) *
                            100, "%")
          label <- "% of FLWs Newly Selected"
        }
        if(pre_var == "% of FLWs Female"){
          value <- paste0(round(data$flws_pct_newly_selected,2)*100,"%")
          label <- "Social Mobilizers<br>per 1000 Target Population"
        }
        if (pre_var == "% of FLWS Resident of Assigned Area") {
          value <- paste0(round(as.numeric(data$flws_pct_resident), 2) *
                            100, "%")
          label <- "% of FLWs Resident of Assigned Area"
        }
        if (pre_var == "% of FLWs Paid for Last SIA") {
          value <- paste0(round(as.numeric(data$pct_paid_among_not_new), 2) *
                            100, "%")
          label <- "% of FLWs Paid for Last SIA"
        }
        
        if (pre_var == "Total Training Sessions") {
          value <- scales::comma(data$total_sessions_all)
          label <- "Total Training Sessions"
        }
        if (pre_var == "Total Persons Trained") {
          value <- scales::comma(data$total_attendance_all)
          label <- "Total Persons Trained"
        }
        if (pre_var == "Volunteer/Social Mobilizer Profile") {
          value <- "Volunteer/Social Mobilizer Profile"
          label <- ""
        }
        if (pre_var == "Volunteer/Social Mobilizer Knowledge Score") {
          value <- paste0(round(as.numeric(data$avg_total_score), 2) *
                            100, "%")
          label <- "Average Vol/SM Knowledge Score"
        }
        
        
      })
      tagList(
        tags$p(style = "font-weight: bold; margin: 0; max-width: 100%;" , value),
        tags$p(style = "margin: 0; max-width: 100%;", label)
      )
    })
  })
}