
icmRightPanelUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("icm_right_panel"))
}

icmRightPanelServer <- function(id, reactive_icm_indicator) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
output$icm_right_panel <- renderUI({
  req(reactive_icm_indicator())
  
  if (reactive_icm_indicator() %in% c("H2H: Door Marking", "H2H: Reasons Missed")) {
    box(
      title = NULL,
      headerBorder = FALSE,
      tags$style(
        type = "text/css",
        ".box-header { display: none; }",
        "#icm_chart { height: calc(65vh - 80px) !important; align-items: center; margin-top: 0px; }"
      ),
      icmChartUI("icm_chart") %>% withSpinner(color = "#0dc5c1"),
      width = NULL,
      align = "center"
    )
  } else{
    if(reactive_icm_indicator() %in% c("Percent of Clusters with ICM Form Reported")){
      box(
        class = "custom-box",
        title = NULL,
        headerBorder = FALSE,
        tags$style(
          type = "text/css",
          ".box-header { display: none; }",
          "#icm_card { height: 60px !important; align-items: center; margin-bottom: 0px; }"
        ),
        icmCardUI("icm_card"),
        width = NULL,
        align = "center",
        style = "height: 80px; overflow: hidden; margin: 0; padding: 0;" # Adjusted height
      )
    } else {
    tagList(
      box(
        class = "custom-box",
        title = NULL,
        headerBorder = FALSE,
        tags$style(
          type = "text/css",
          ".box-header { display: none; }",
          "#icm_card { height: 60px !important; align-items: center; margin-bottom: 0px; }"
        ),
        icmCardUI("icm_card"),
        width = NULL,
        align = "center",
        style = "height: 80px; overflow: hidden; margin: 0; padding: 0;" # Adjusted height
      ),
      box(
        title = NULL,
        headerBorder = FALSE,
        tags$style(
          type = "text/css",
          ".box-header { display: none; }",
          "#icm_chart { height: calc(50vh - 80px) !important; align-items: center; margin-top: 0px; }"
        ),
        icmChartUI("icm_chart") %>% withSpinner(color = "#0dc5c1"),
        width = NULL,
        style = "height: 320px; overflow: hidden; margin: 0; padding: 0;" # Adjusted height
      )
    )
  }}
})
})
}
