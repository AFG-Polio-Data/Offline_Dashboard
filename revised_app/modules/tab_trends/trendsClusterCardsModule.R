trendsLowCoverageCardUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("trends_cluster_card"))
  )
}

trendsLowCoverageCardServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$trends_cluster_card <- renderUI({

      tagList(
        div(
          style = "border: 1px solid #ccc; padding: 15px; border-radius: 10px; background-color: #f9f9f9;",
          tags$p(
            style = "font-weight: bold; margin: 0; text-align: center;",
            "Definition of 'Consistently Low Coverage':"
          ),
          tags$p(
            style = "margin: 0; text-align: center;", 
            HTML("Any cluster with PCA coverage estimates for 2 or more campaigns in the selected date range, where over 50% of the campaigns had 'PCA Finger-Mark Coverage (0-59m)' below the selected low coverage threshold.<br><br>")
          ),
          sliderInput(
            ns("low_coverage_threshold"),
            "Specify the Low Coverage Threshold:",
            min = 0,
            max = 100,
            value = 70,
            step = 1,
            post = "%"
          )
        )
      )
    })
    
    # Return threshold as a reactive value
    return(reactive(input$low_coverage_threshold / 100))
  })
}