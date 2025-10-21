rightPanelUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("right_panel"))
}

rightPanelServer <- function(id, reactive_pcm_indicator, pcm_filtered_sia_data, 
                             reactive_zoom_region,
                             reactive_zoom_province,
                             reactive_zoom_district,
                             pcm_indicator_name_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$right_panel <- renderUI({
      req(reactive_pcm_indicator())
      
      if (reactive_pcm_indicator() %in% c(
        "PCA - Modality", "LQAS - Pct of Lots Passed (OPV)", "LQAS - Pct of Lots Passed (IPV)", "PCA - Reasons Missed (% of Missed)", 
        "PCA - Sources of Awareness", "PCA - Door Marking", "Out-of-house Survey - Reasons Missed (% of Missed)", 
        "LQAS - Reasons Missed (% of Missed)", 
        "PCA - Data Verification and Publication", "Out-of-house Survey - Data Verification and Publication", "LQAS - Data Verification and Publication")) {
        
        box(
          title = NULL,
          headerBorder = FALSE,
          width = NULL,
          align = "center",
          style = "height: 420px; overflow-y: auto; display: flex; flex-direction: column; justify-content: space-between;",
          
          radioButtons(
            inputId = ns("chart_type"),
            label = "Chart Type:",
            choices = c("Pie Chart" = "pie", "Bar Chart" = "bar"),
            selected = "pie",
            inline = TRUE,
            width = "100%"
          ),
          pcmChartUI(ns("pcm_chart_module")) %>% withSpinner(color = "#0dc5c1")
        )
        
      } else{
        if(reactive_pcm_indicator() %in% c(
          "PCA - Finger-Mark Coverage (0-59m, OPV)",
          "PCA - Finger-Mark Coverage (0-11m, OPV)",
          "PCA - Finger-Mark Coverage (12-59m, OPV)",
          "PCA - Finger-Mark Coverage (HRMP, OPV)", 
          "Out-of-house Survey - Finger-Mark Coverage (0-59m, OPV)",
          "Out-of-house Survey - Finger-Mark Coverage (4-59m, IPV)",
          "PCA - HRMP Percent of Houses Visited", 
          "PCA - Finger-Mark Coverage (4-59m, IPV)")
          ) {
          box(
            title = NULL,
            headerBorder = FALSE,
            width = NULL,
            align = "center",
            style = "height: 420px; display: flex; flex-direction: column; justify-content: space-between;",
            
              pcmCardUI(ns("pcm_card_module")),
              pcmChartUI(ns("pcm_chart_module")) %>% withSpinner(color = "#0dc5c1")
            
          )
        } else{
          if(reactive_pcm_indicator() %in% c("PCA - Reasons Missed (per 1000 Screened)")){
            box(
              title = NULL,
              headerBorder = FALSE,
              width = NULL,
              align = "center",
              style = "height: 420px; overflow-y: auto; display: flex; flex-direction: column; justify-content: flex-start;",
              
              pcmChartUI(ns("pcm_chart_module")) %>% withSpinner(color = "#0dc5c1")
            )
          }else {
          box(
            title = NULL,
            headerBorder = FALSE,
            width = NULL,
            align = "center",
            style = "height: 420px; overflow-y: auto; display: flex; flex-direction: column; justify-content: flex-start;",
            
            pcmCardUI(ns("pcm_card_module"))
          )
      }}}
    })
    
    selected_chart <- reactive({
      if (is.null(input$chart_type)) return("pie")
      input$chart_type
    })
    
    observe({
      req(reactive_pcm_indicator())
      req(selected_chart())
      
      pcmChartServer(
        id = "pcm_chart_module",
        pcm_filtered_sia_data = pcm_filtered_sia_data,
        reactive_pcm_indicator = reactive_pcm_indicator,
        chart_type = selected_chart,
        reactive_zoom_region,
        reactive_zoom_province,
        reactive_zoom_district
      )
      
      pcmCardServer(
        "pcm_card_module",
        pcm_filtered_sia_data = pcm_filtered_sia_data,
        reactive_pcm_indicator = reactive_pcm_indicator,
        reactive_zoom_region,
        reactive_zoom_province,
        reactive_zoom_district,
        pcm_indicator_name_list
      )
    })
  })
}
