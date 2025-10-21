
trends_rightPanelUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("trends_right_panel"))
}

trends_rightPanelServer <- function(id, reactive_temporal_indicator) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$trends_right_panel <- renderUI({
      req(reactive_temporal_indicator())
      
  
      if (reactive_temporal_indicator() %in% c("PCA - Clusters with Consistently Low OPV Coverage")) {
        box(
          trendsLowCoverageCardUI("trends_cluster_card") %>% withSpinner(color="#0dc5c1"),
          solidHeader = FALSE,
          status = NULL,
          title = NULL,
          width = NULL,
          style = "height: 475px; overflow: hidden;" # Matches total height of left column (175px + 350px)
        )
      } else {
        box(
          trendChartUI("trend_chart") %>% withSpinner(color="#0dc5c1"),
          solidHeader = FALSE,
          status = NULL,
          title = NULL,
          width = NULL,
          style = "height: 475px; overflow: hidden;" # Matches total height of left column (175px + 350px)
        )
      }
    })
  })
}
