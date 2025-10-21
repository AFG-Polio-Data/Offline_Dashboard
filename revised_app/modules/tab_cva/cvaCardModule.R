cvaCardUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("cva_card"))
}

cvaCardServer <- function(id, 
                             cva_data,
                             selected_region,
                             selected_province,
                             selected_district,
                             cva_form_type,
                             selected_indicator,
                          start_month,
                          end_month) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$cva_card <- renderUI({
      req(cva_data())
      req(selected_indicator())
      req(selected_region())
      req(selected_province())
      req(selected_district())
      req(cva_form_type())
      req(start_month())
      req(end_month())
      
      cva_var <- selected_indicator()
      
      if(selected_region() == "All"){
        data <- cva_data()$cva_national
      } else{
        if(selected_province() == "All"){
          data <- cva_data()$cva_region %>%
            filter(region == selected_region())
        } else{
          if(selected_district() == "All"){
            data <- cva_data()$cva_province %>%
              filter(region == selected_region() &
                     province == selected_province())
          } else{
            data <- cva_data()$cva_district %>%
              filter(region == selected_region() &
                       province == selected_province() &
                       district == selected_district())
          }
        }}

      
      if(cva_form_type() %in% c("Combined", "Cross-Border") & cva_var == "Total OPV Doses Administered"){
        data <- data %>% 
          rename(value = opv_total) %>%
          select(value) %>%
          summarise(value = sum(value, na.rm=T)) %>%
          ungroup() %>%
          mutate(value = scales::comma(value))
        value <- data$value[1]
        if(start_month() == end_month()){
          label <- paste0("Total OPV Doses Administered in ", start_month())
        } else{
          label <- paste0("Total OPV Doses Administered: ", start_month(), " to ", end_month())
          
        }
      }
        
      if(cva_form_type() == "Permanent Transit Teams" & cva_var == "Total OPV Doses Administered"){
        data <- data %>% 
          rename(value = opv_total) %>%
          select(value) %>%
          summarise(value = sum(value, na.rm=T)) %>%
          ungroup() %>%
          mutate(value = scales::comma(value))
        value <- data$value[1]
        if(start_month() == end_month()){
          label <- paste0("Total OPV Doses Administered in ", end_month())
        } else{
          label <- paste0("Total OPV Doses Administered: ", start_month(), " to ", end_month())
          
        }
      }
      if(cva_form_type() == "IHR" & cva_var == "Total OPV Doses Administered"){
        data <- data %>% 
          rename(value = opv_total) %>%
          select(value) %>%
          summarise(value = sum(value, na.rm=T)) %>%
          ungroup() %>%
          mutate(value = scales::comma(value))
        value <- data$value[1]
        if(start_month() == end_month()){
          label <- paste0("Total OPV Doses Administered in ", end_month())
        } else{
          label <- paste0("Total OPV Doses Administered: ", start_month(), " to ", end_month())
          
        }
      }
      if(cva_form_type() == "Returnees" & cva_var == "Total OPV Doses Administered"){
        data <- data %>% 
          rename(value = opv_total) %>%
          select(value) %>%
          summarise(value = sum(value, na.rm=T)) %>%
          ungroup() %>%
          mutate(value = scales::comma(value))
        value <- data$value[1]
        if(start_month() == end_month()){
          label <- paste0("Total OPV Doses Administered in ", end_month())
        } else{
          label <- paste0("Total OPV Doses Administered: ", start_month(), " to ", end_month())
          
        }
      }
      if(cva_form_type() == "Returnees" & cva_var == "Total IPV Doses Administered"){
        data <- data %>% 
          rename(value = ipv_total) %>%
          select(value) %>%
          summarise(value = sum(value, na.rm=T)) %>%
          ungroup() %>%
          mutate(value = scales::comma(value))
        value <- data$value[1]
        if(start_month() == end_month()){
          label <- paste0("Total IPV Doses Administered in ", end_month())
        } else{
          label <- paste0("Total IPV Doses Administered: ", start_month(), " to ", end_month())
          
        }
      }
      if(cva_form_type() == "Cross-Border" & cva_var == "Refusal Rate"){
        data <- data %>% 
          select(screened_total, refusal_total) %>%
          summarise_all(~sum(., na.rm=T)) %>%
          ungroup() %>%
          rowwise() %>%
          mutate(value = ifelse(screened_total > 0 & refusal_total <= screened_total & !is.na(refusal_total),
                                refusal_total / screened_total, NA_real_)) %>%
          ungroup()
        value <- scales::percent(data$value[1], accuracy=1)
        if(start_month() == end_month()){
          label <- paste0("Refusal Rate (# of Refusals / # Screened) in", end_month())
        } else{
          label <- paste0("Refusal Rate (# of Refusals / # Screened): ", start_month(), " to ", end_month())
          
        }
      } 
      
    
    tagList(
      tags$p(style = "font-weight: bold; margin: 0; max-width: 100%;" , 
             value),
      tags$p(
        style="margin: 0; max-width: 100%;",
        label)
    )
    })
})
}