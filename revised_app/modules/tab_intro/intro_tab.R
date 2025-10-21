# modules/introTabModule.R
library(shiny)
library(dplyr)
library(lubridate)
library(purrr)
library(scales)

introTabUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("intro_card"))
}

introTabServer <- function(id, selected_campaigns, selected_campaign, campaign_filtered_sia_data, campaign_filtered_admin_data, campaign_filtered_icm_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Create reactive values to store indicators and loading status**
    data_loaded <- reactiveVal(FALSE)
    all_indicators <- reactiveVal(NULL)
    
    observe({
      req(selected_campaigns(), campaign_filtered_sia_data(), campaign_filtered_admin_data(), campaign_filtered_icm_data())
      
      withProgress(message = 'Calculation in progress...', value = 0, {
        
        latest_campaign <- selected_campaigns()[1]
        selected_campaign(latest_campaign)
        
        post_data <- campaign_filtered_sia_data()$national_indicators %>%
          filter(campaign_name == latest_campaign) %>%
          filter(indicator %in% c("lqas_result", "pca_fm_coverage_0_59m")) %>%
          select(indicator, value, numerator, denominator) %>%
          rename(pct = value)
        
        intra_data <- campaign_filtered_icm_data()$icm_household_campaign_coverage %>%
          filter(campaign_name == latest_campaign) %>%
          filter(indicator == "H2H: Coverage, finger-marked") %>%
          select(numerator, denominator, pct) %>%
          mutate(indicator = "icm_coverage")
        
        admin_data <- campaign_filtered_admin_data()$national %>%
          filter(campaign_name == latest_campaign) %>%
          select(total_vaccinated, target_population) %>%
          rowwise() %>%
          mutate(
            admin_coverage = ifelse(target_population > 0 & !is.na(target_population) & !is.na(total_vaccinated),
                                    total_vaccinated / target_population, NA_integer_),
            indicator = "admin_cov"
          ) %>%
          ungroup() %>%
          rename(pct = admin_coverage, numerator = total_vaccinated, denominator = target_population)
        
        # Store all indicators in `reactiveVal()` so they are accessible in renderUI**
        all_indicators(bind_rows(
          post_data %>% mutate(across(c(pct, numerator, denominator), as.numeric)),
          intra_data %>% mutate(across(c(pct, numerator, denominator), as.numeric)),
          admin_data %>% mutate(across(c(pct, numerator, denominator), as.numeric))
        ))
        
        # Set data_loaded(TRUE) to update UI**
        data_loaded(TRUE)
      })
    })
    
    # Define createValueBox() outside of renderUI()**
    createValueBox <- function(all_indicators, indicator_name, label_text, numerator_text, denominator_text) {
      if (!is.null(all_indicators) && !is.na(all_indicators$pct[all_indicators$indicator == indicator_name][1]) &
          all_indicators$pct[all_indicators$indicator == indicator_name][1] != 0) {
        value <- paste0(round(as.numeric(all_indicators$pct[all_indicators$indicator == indicator_name][1]), 2) * 100, "%")
        label <- HTML(paste0(
          "<span style='font-size: 10px;'>(",
          scales::comma(as.numeric(all_indicators$numerator[all_indicators$indicator == indicator_name][1]), accuracy=1),
          " ", numerator_text, " / ",
          scales::comma(as.numeric(all_indicators$denominator[all_indicators$indicator == indicator_name][1]), accuracy=1),
          " ", denominator_text, ")</span><br>",
          "<strong>", label_text, "</strong>"
        ))
      } else {
        value <- "No Data"
        label <- HTML(paste0("<strong>", label_text, "</strong>"))
      }
      
      div(class = "custom-valuebox", tagList(
        tags$p(class = "value", value),
        tags$p(class = "label", label)
      ))
    }
    
    output$intro_card <- renderUI({
      if (!data_loaded()) {
        return(
          div(
            style = "text-align: center; padding: 20px;",
            icon("spinner", class = "fa-spin fa-2x", style="color: #0dc5c1;"),
            h4("Data are loading. Please be patient.")
          )
        )
      }
      
      # Safely access `all_indicators()` and ensure it's not NULL**
      indicators_data <- all_indicators()
      req(indicators_data)
      
      tagList(
        column(12, div(class = "custom-valuebox_title", tags$p(selected_campaigns()[1]))),
        column(12, createValueBox(indicators_data, "admin_cov", "Admin OPV Coverage", "Vaccinated", "Targeted")),
        # column(12, createValueBox(indicators_data, "icm_coverage", "Intra-Campaign Monitoring Coverage (H2H Only)", "Finger-Marked", "Assessed")),
        column(12, createValueBox(indicators_data, "pca_fm_coverage_0_59m", "Post-Campaign Assessment OPV Coverage", "Finger-Marked", "Assessed")),
        column(12, createValueBox(indicators_data, "lqas_result", "Percent of Lots Passed LQAS (OPV)", "Lots Passed", "Lots Assessed"))
      )
    })
    return(list(data_loaded = data_loaded))
  })
}
