# modules/pcm_card_module.R

pcmCardUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("pcm_card"))
}

pcmCardServer <- function(id, pcm_filtered_sia_data, 
                          reactive_pcm_indicator, 
                          reactive_zoom_region, 
                          reactive_zoom_province, 
                          reactive_zoom_district, 
                          pcm_indicator_name_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$pcm_card <- renderUI({
      req(pcm_filtered_sia_data())
      req(reactive_pcm_indicator())
      req(reactive_zoom_region())
      req(reactive_zoom_province())
      req(reactive_zoom_district())
      
      pcm_var <- reactive_pcm_indicator()
      district_value <- NULL
      province_value <- NULL
      region_value <- NULL
      national_value <- NULL
      out <- NULL
      
      if (pcm_var %in% c("PCA - Finger-Mark Coverage (0-59m, OPV)",
                         "PCA - Finger-Mark Coverage (4-59m, IPV)",
                         "PCA - Finger-Mark Coverage (0-11m, OPV)",
                         "PCA - Finger-Mark Coverage (12-59m, OPV)",
                         "PCA - Recall Coverage (0-59m, OPV)",
                         "PCA - Percent of Clusters with OPV FM Coverage <95%",
                         "PCA - Percent Aware",
                         "PCA - Reporting Completeness (% of Clusters with Published Data)",
                         "Out-of-house Survey - Finger-Mark Coverage (0-59m, OPV)",
                         "Out-of-house Survey - Finger-Mark Coverage (4-59m, IPV)",
                         "Out-of-house Survey - Completeness",
                         "PCA - Finger-Mark Coverage (HRMP, OPV)",
                         "PCA - HRMP Percent of Houses Visited"
      )) {
        
        data <- pcm_filtered_sia_data()
        
        data$district_indicators <- data$district_indicators %>%
          filter(indicator == pcm_indicator_name_list[[pcm_var]][1])
        
        data$province_indicators <- data$province_indicators %>%
          filter(indicator == pcm_indicator_name_list[[pcm_var]][1])
        
        data$region_indicators <- data$region_indicators %>%
          filter(indicator == pcm_indicator_name_list[[pcm_var]][1])
        
        data$national_indicators <- data$national_indicators %>%
          filter(indicator == pcm_indicator_name_list[[pcm_var]][1])
        
        if (!("All" %in% reactive_zoom_district())) {
          district_value <- paste0(round(as.numeric(data$district_indicators$value[1]), 2) * 100, "%")
          district_label <- HTML(paste0("(", scales::comma(as.numeric(data$district_indicators$numerator[1]), accuracy=1), "/", 
                                        scales::comma(as.numeric(data$district_indicators$denominator[1]), accuracy=1), ")<br>",
                                        reactive_zoom_district(), " District Total"))
          
          province_value <- paste0(round(as.numeric(data$province_indicators$value[1]), 2) * 100, "%")
          province_label <- HTML(paste0("(", scales::comma(as.numeric(data$province_indicators$numerator[1]), accuracy=1), "/", 
                                        scales::comma(as.numeric(data$province_indicators$denominator[1]), accuracy=1), ")<br>",
                                        reactive_zoom_province(), " Province Total"))
          
          region_value <- paste0(round(as.numeric(data$region_indicators$value[1]), 2) * 100, "%")
          region_label <- HTML(paste0("(", scales::comma(as.numeric(data$region_indicators$numerator[1]), accuracy=1), "/", 
                                      scales::comma(as.numeric(data$region_indicators$denominator[1]), accuracy=1), ")<br>",
                                      reactive_zoom_region(), " Region Total"))
          
          national_value <- paste0(round(as.numeric(data$national_indicators$value[1]), 2) * 100, "%")
          national_label <- HTML(paste0("(", scales::comma(as.numeric(data$national_indicators$numerator[1]), accuracy=1), "/", 
                                        scales::comma(as.numeric(data$national_indicators$denominator[1]), accuracy=1), ")<br>National Total"))
          
          if(pcm_var %in% c("PCA - Finger-Mark Coverage (0-59m, OPV)",
                            "PCA - Finger-Mark Coverage (4-59m, IPV)",
                         "PCA - Finger-Mark Coverage (0-11m, OPV)",
                         "PCA - Finger-Mark Coverage (12-59m, OPV)",
                         "PCA - Finger-Mark Coverage (HRMP, OPV)",
                         "Out-of-house Survey - Finger-Mark Coverage (0-59m, OPV)",
                         "Out-of-house Survey - Finger-Mark Coverage (4-59m, IPV)",
                         "PCA - HRMP Percent of Houses Visited")){
            out <- list(
              column(12, div(class = "custom-valuebox", tagList(
                tags$p(class="value", district_value),
                tags$p(class="label", district_label)
              ))))
            
          } else{
          out <- list(
            column(12, div(class = "custom-valuebox", tagList(
              tags$p(class="value", national_value),
              tags$p(class="label", national_label)
            ))),
            column(12, div(class = "custom-valuebox", tagList(
              tags$p(class="value", region_value),
              tags$p(class="label", region_label)
            ))),
            column(12, div(class = "custom-valuebox", tagList(
              tags$p(class="value", province_value),
              tags$p(class="label", province_label)
            ))),
            column(12, div(class = "custom-valuebox", tagList(
              tags$p(class="value", district_value),
              tags$p(class="label", district_label)
            ))))
          }
        } else if (!("All" %in% reactive_zoom_province())) {
          province_value <- paste0(round(as.numeric(data$province_indicators$value[1]), 2) * 100, "%")
          province_label <- HTML(paste0("(", scales::comma(as.numeric(data$province_indicators$numerator[1]), accuracy=1), "/", 
                                        scales::comma(as.numeric(data$province_indicators$denominator[1]), accuracy=1), ")<br>",
                                        reactive_zoom_province(), " Province Total"))
          
          region_value <- paste0(round(as.numeric(data$region_indicators$value[1]), 2) * 100, "%")
          region_label <- HTML(paste0("(", scales::comma(as.numeric(data$region_indicators$numerator[1]), accuracy=1), "/", 
                                      scales::comma(as.numeric(data$region_indicators$denominator[1]), accuracy=1), ")<br>",
                                      reactive_zoom_region(), " Region Total"))
          
          national_value <- paste0(round(as.numeric(data$national_indicators$value[1]), 2) * 100, "%")
          national_label <- HTML(paste0("(", scales::comma(as.numeric(data$national_indicators$numerator[1]), accuracy=1), "/", 
                                        scales::comma(as.numeric(data$national_indicators$denominator[1]), accuracy=1), ")<br>National Total"))
          
          if(pcm_var %in% c("PCA - Finger-Mark Coverage (0-59m, OPV)",
                            "PCA - Finger-Mark Coverage (4-59m, IPV)",
                            "PCA - Finger-Mark Coverage (0-11m, OPV)",
                            "PCA - Finger-Mark Coverage (12-59m, OPV)",
                            "PCA - Finger-Mark Coverage (HRMP, OPV)",
                            "Out-of-house Survey - Finger-Mark Coverage (0-59m, OPV)", 
                            "Out-of-house Survey - Finger-Mark Coverage (4-59m, IPV)", 
                            "PCA - HRMP Percent of Houses Visited")){
            out <- list(
              column(12, div(class = "custom-valuebox", tagList(
                tags$p(class="value", province_value),
                tags$p(class="label", province_label)
              ))))
            
          } else{
            out <- list(
            column(12, div(class = "custom-valuebox", tagList(
              tags$p(class="value", national_value),
              tags$p(class="label", national_label)
            ))),
            column(12, div(class = "custom-valuebox", tagList(
              tags$p(class="value", region_value),
              tags$p(class="label", region_label)
            ))),
            column(12, div(class = "custom-valuebox", tagList(
              tags$p(class="value", province_value),
              tags$p(class="label", province_label)
            )))
          )
          }
        } else if (!("All" %in% reactive_zoom_region())) {
          region_value <- paste0(round(as.numeric(data$region_indicators$value[1]), 2) * 100, "%")
          region_label <- HTML(paste0("(", scales::comma(as.numeric(data$region_indicators$numerator[1]), accuracy=1), "/", 
                                      scales::comma(as.numeric(data$region_indicators$denominator[1]), accuracy=1), ")<br>",
                                      reactive_zoom_region(), " Region Total"))
          
          national_value <- paste0(round(as.numeric(data$national_indicators$value[1]), 2) * 100, "%")
          national_label <- HTML(paste0("(", scales::comma(as.numeric(data$national_indicators$numerator[1]), accuracy=1), "/", 
                                        scales::comma(as.numeric(data$national_indicators$denominator[1]), accuracy=1), ")<br>National Total"))
          
          if(pcm_var %in% c("PCA - Finger-Mark Coverage (0-59m, OPV)",
                            "PCA - Finger-Mark Coverage (4-59m, IPV)",
                            "PCA - Finger-Mark Coverage (0-11m, OPV)",
                            "PCA - Finger-Mark Coverage (12-59m, OPV)",
                            "PCA - Finger-Mark Coverage (HRMP, OPV)",
                            "Out-of-house Survey - Finger-Mark Coverage (0-59m, OPV)",
                            "Out-of-house Survey - Finger-Mark Coverage (4-59m, IPV)",
                            "PCA - HRMP Percent of Houses Visited")){
            out <- list(
              column(12, div(class = "custom-valuebox", tagList(
                tags$p(class="value", region_value),
                tags$p(class="label", region_label)
              ))))
            
          } else{
            out <- list(
            column(12, div(class = "custom-valuebox", tagList(
              tags$p(class="value", national_value),
              tags$p(class="label", national_label)
            ))),
            column(12, div(class = "custom-valuebox", tagList(
              tags$p(class="value", region_value),
              tags$p(class="label", region_label)
            )))
          )
          }
        } else {
          national_value <- paste0(round(as.numeric(data$national_indicators$value[1]), 2) * 100, "%")
          national_label <- HTML(paste0("(", scales::comma(as.numeric(data$national_indicators$numerator[1]), accuracy=1), "/", 
                                        scales::comma(as.numeric(data$national_indicators$denominator[1]), accuracy=1), ")<br>National Total"))
          
          out <- list(
            column(12, div(class = "custom-valuebox", tagList(
              tags$p(class="value", national_value),
              tags$p(class="label", national_label)
            )))
          )
        }
      } else { out <- NULL }
      
      out
    })
  })
}
