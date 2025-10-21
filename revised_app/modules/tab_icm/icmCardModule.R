icmCardUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("icm_card"))
}

icmCardServer <- function(id, icm_filtered_sia_data_form_indicator_age_filtered,
                          reactive_icm_indicator, reactive_zoom_region, reactive_zoom_province, reactive_zoom_district) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
output$icm_card <- renderUI({
  req(reactive_icm_indicator())
  req(icm_filtered_sia_data_form_indicator_age_filtered())
  req(reactive_zoom_region())
  req(reactive_zoom_province())
  req(reactive_zoom_district())
  
  icm_var <- reactive_icm_indicator()
  district_value <- NULL
  province_value <- NULL
  region_value <- NULL
  national_value <- NULL
  
  out <- NULL
  if (!(icm_var %in% c("H2H: Reasons Missed", "H2H: Door Marking"))) {
    if(icm_var %in% icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_campaign_coverage$indicator){
      district_data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_district_coverage %>%
        filter(indicator == icm_var) %>%
        ungroup()
      province_data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_province_coverage %>%
        filter(indicator == icm_var) %>%
        ungroup()
      region_data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_region_coverage %>%
        filter(indicator == icm_var) %>%
        ungroup()
      national_data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_household_campaign_coverage %>%
        filter(indicator == icm_var) %>%
        ungroup()
    } else{
      district_data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_district %>%
        filter(indicator == icm_var) %>%
        ungroup()
      province_data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_province %>%
        filter(indicator == icm_var) %>%
        ungroup()
      region_data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_region %>%
        filter(indicator == icm_var) %>%
        ungroup()
      national_data <- icm_filtered_sia_data_form_indicator_age_filtered()$icm_pcts_campaign %>%
        filter(indicator == icm_var) %>%
        ungroup()
    }
    
    district_value <- paste0(round(as.numeric(district_data$pct[1]), 2) * 100, "% (", scales::comma(as.numeric(district_data$numerator[1]), accuracy=1), "/", 
                             scales::comma(as.numeric(district_data$denominator[1]), accuracy=1), ")")
    district_label <- paste0(reactive_zoom_district(), " District Total")
    
    province_value <- paste0(round(as.numeric(province_data$pct[1]), 2) * 100, "% (", scales::comma(as.numeric(province_data$numerator[1]), accuracy=1), "/", 
                             scales::comma(as.numeric(province_data$denominator[1]), accuracy=1), ")")
    province_label <- paste0(reactive_zoom_province(), " Province Total")
    
    region_value <- paste0(round(as.numeric(region_data$pct[1]), 2) * 100, "% (", scales::comma(as.numeric(region_data$numerator[1]), accuracy=1), "/", 
                           scales::comma(as.numeric(region_data$denominator[1]), accuracy=1), ")")
    region_label <- paste0(reactive_zoom_region(), " Region Total")
    
    national_value <- paste0(round(as.numeric(national_data$pct[1]), 2) * 100, "% (", scales::comma(as.numeric(national_data$numerator[1]), accuracy=1), "/", 
                             scales::comma(as.numeric(national_data$denominator[1]), accuracy=1), ")")
    national_label <- "National Total"
    
    if (!("All" %in% reactive_zoom_district())) {
      out <- list(
        column(12, div(class = "custom-valuebox", tagList(
          tags$p(class="value", district_value),
          tags$p(class="label", district_label)
        ))))
      
    } else {
      if (!("All" %in% reactive_zoom_province())) {
        out <- list(
          column(12, div(class = "custom-valuebox", tagList(
            tags$p(class="value", province_value),
            tags$p(class="label", province_label)
          ))))
      } else {
        if (!("All" %in% reactive_zoom_region())) {
          out <- list(
            # column(12, div(class = "custom-valuebox", tagList(
            #   tags$p(class="value", national_value),
            #   tags$p(class="label", national_label)
            # ))),
            column(12, div(class = "custom-valuebox", tagList(
              tags$p(class="value", region_value),
              tags$p(class="label", region_label)
            ))))
        } else {
          out <- list(
            column(12, div(class = "custom-valuebox", tagList(
              tags$p(class="value", national_value),
              tags$p(class="label", national_label)
            ))))
        }
      }
    }
  }
  else{ out <- NULL}
  
  out
})
})
}