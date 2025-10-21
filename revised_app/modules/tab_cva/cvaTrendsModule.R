cvaTrendsUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("cva_trends"), height = "320px")
}

cvaTrendsServer <- function(id, 
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
    
    output$cva_trends <- renderPlotly({
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
      
      format_y_labels <- function(x) {
        if (max_value >= 1e6) {
          return(paste0(round(x / 1e6, 1), "M"))
        } else {
          return(formatC(x, format="f", big.mark = ",", digits=0))
        }
      }
      
      if(cva_form_type() %in% c("Cross-Border") & cva_var == "Total OPV Doses Administered"){
        data <- data %>% 
          mutate(date = format(date, "%Y-%b")) %>%
          mutate(date = as.Date(paste0(date, "-01"), "%Y-%b-%d")) %>%  # Ensure date is in Date format
          group_by(date) %>%
          summarise(value = sum(opv_total, na.rm=T)) %>%
          ungroup() %>%
          filter(!is.na(value))
        
        max_value <- max(data$value, na.rm = TRUE)
        
        plot <- ggplot(data=data)+
          geom_point(aes(x=date, y=value, text = scales::comma(value, accuracy=1)), color="black") +
          geom_line(aes(x=date, y=value, group=1), color="black") +
          scale_y_continuous(labels = format_y_labels, limits=c(0, max(data$value, na.rm=T)+(0.05*max(data$value, na.rm=T)))) +  # Apply dynamic formatting
          scale_x_date(labels = scales::date_format("%Y-%b"), breaks = "1 month") +  # Format date as %Y-%b and set breaks
          labs( y = "Total OPV Doses") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size=8),
            axis.text.y = element_text(size=8),
            axis.title.y = element_text(size=8),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.position = "none"
          )
        
      }
      if(cva_form_type() %in% c("Combined") & cva_var == "Total OPV Doses Administered"){
        data <- data %>% 
          mutate(date = format(date, "%Y-%b")) %>%
          mutate(date = as.Date(paste0(date, "-01"), "%Y-%b-%d")) %>%  # Ensure date is in Date format
          group_by(date) %>%
          summarise(value = sum(opv_total, na.rm=T)) %>%
          ungroup() %>%
          filter(!is.na(value)) 
        
        max_value <- max(data$value, na.rm = TRUE)
        
        plot <- ggplot(data=data)+
          geom_point(aes(x=date, y=value, text = scales::comma(value, accuracy=1)), color="black") +
          geom_line(aes(x=date, y=value, group=1), color="black") +
          scale_y_continuous(labels = format_y_labels, limits=c(0, max(data$value, na.rm=T)+(0.05*max(data$value, na.rm=T)))) +  # Apply dynamic formatting
          scale_x_date(labels = scales::date_format("%Y-%b"), breaks = "1 month") +  # Format date as %Y-%b and set breaks
          labs( y = "Total OPV Doses") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size=8),
            axis.text.y = element_text(size=8),
            axis.title.y = element_text(size=8),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.position = "none"
          )        
      }
      
      if(cva_form_type() == "Permanent Transit Teams" & cva_var == "Total OPV Doses Administered"){
        data <- data %>% 
          mutate(date = format(date, "%Y-%b")) %>%
          mutate(date = as.Date(paste0(date, "-01"), "%Y-%b-%d")) %>%  # Ensure date is in Date format
          group_by(date) %>%
          summarise(value = sum(opv_total, na.rm=T)) %>%
          ungroup() %>%
          filter(!is.na(value))
        
        max_value <- max(data$value, na.rm = TRUE)
        
        plot <- ggplot(data=data)+
          geom_point(aes(x=date, y=value, text = scales::comma(value, accuracy=1)), color="black") +
          geom_line(aes(x=date, y=value, group=1), color="black") +
          scale_y_continuous(labels = format_y_labels, limits=c(0, max(data$value, na.rm=T)+(0.05*max(data$value, na.rm=T)))) +  # Apply dynamic formatting
          scale_x_date(labels = scales::date_format("%Y-%b"), breaks = "1 month") +  # Format date as %Y-%b and set breaks
          labs( y = "Total OPV Doses") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size=8),
            axis.text.y = element_text(size=8),
            axis.title.y = element_text(size=8),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.position = "none"
          )   
      }
      
      if(cva_form_type() == "Returnees" & cva_var == "Total OPV Doses Administered"){
        data <- data %>%
          mutate(date = format(date, "%Y-%b")) %>%
          mutate(date = as.Date(paste0(date, "-01"), "%Y-%b-%d")) %>%  # Ensure date is in Date format
          group_by(date) %>%
          summarise(value = sum(opv_total, na.rm=T)) %>%
          ungroup() %>%
          filter(!is.na(value)) 
        
        max_value <- max(data$value, na.rm = TRUE)
        
        plot <- ggplot(data=data)+
          geom_point(aes(x=date, y=value, text = scales::comma(value, accuracy=1)), color="black") +
          geom_line(aes(x=date, y=value, group=1), color="black") +
          scale_y_continuous(labels = format_y_labels, limits=c(0, max(data$value, na.rm=T)+(0.05*max(data$value, na.rm=T)))) +  # Apply dynamic formatting
          scale_x_date(labels = scales::date_format("%Y-%b"), breaks = "1 month") +  # Format date as %Y-%b and set breaks
          labs( y = "Total OPV Doses") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size=8),
            axis.text.y = element_text(size=8),
            axis.title.y = element_text(size=8),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.position = "none"
          )   
      }
      if(cva_form_type() == "Returnees" & cva_var == "Total IPV Doses Administered"){
        data <- data %>%
          mutate(date = format(date, "%Y-%b")) %>%
          mutate(date = as.Date(paste0(date, "-01"), "%Y-%b-%d")) %>%  # Ensure date is in Date format
          group_by(date) %>%
          summarise(value = sum(ipv_total, na.rm=T)) %>%
          ungroup() %>%
          filter(!is.na(value)) 
        
        max_value <- max(data$value, na.rm = TRUE)
        
        plot <- ggplot(data=data)+
          geom_point(aes(x=date, y=value, text = scales::comma(value, accuracy=1)), color="black") +
          geom_line(aes(x=date, y=value, group=1), color="black") +
          scale_y_continuous(labels = format_y_labels, limits=c(0, max(data$value, na.rm=T)+(0.05*max(data$value, na.rm=T)))) +  # Apply dynamic formatting
          scale_x_date(labels = scales::date_format("%Y-%b"), breaks = "1 month") +  # Format date as %Y-%b and set breaks
          labs( y = "Total IPV Doses") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size=8),
            axis.text.y = element_text(size=8),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size=8),
            legend.title = element_blank(),
            legend.position = "none"
          )   
      }
      
      if(cva_form_type() == "Cross-Border" & cva_var == "Refusal Rate"){
        
        data <- data %>% 
          select(date, screened_total, refusal_total) %>%
          mutate(date = format(date, "%Y-%b")) %>%
          mutate(date = as.Date(paste0(date, "-01"), "%Y-%b-%d")) %>%  # Ensure date is in Date format
          group_by(date) %>%
          summarise_all(~sum(., na.rm=T)) %>%
          ungroup() %>%
          rowwise() %>%
          mutate(value = ifelse(screened_total > 0 & refusal_total <= screened_total & !is.na(refusal_total),
                                refusal_total / screened_total, NA_real_)) %>%
          ungroup() %>%
          filter(!is.na(value) & value != 0)
        
        max_value <- max(data$value, na.rm = TRUE)
        
        plot <- ggplot(data=data)+
          geom_point(aes(x=date, y=value, text = scales::percent(value, accuracy=1)), color="black") +
          geom_line(aes(x=date, y=value, group=1), color="black") +
          scale_y_continuous(labels = percent_format(scale = 100), limits=c(0,max(data$value)+0.05*max(data$value))) +  # Format y-axis as percentage
          scale_x_date(labels = scales::date_format("%Y-%b"), breaks = "1 month") +  # Format date as %Y-%b and set breaks
          labs( y = "Refusal Rate") +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size=8),
            axis.text.y = element_text(size=8),
            axis.title.y = element_text(size=8),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.position = "none"
          )
      } 
      
      if(cva_form_type() == "IHR" & cva_var == "Total OPV Doses Administered"){
        data <- data %>%
          mutate(date = format(date, "%Y-%b")) %>%
          mutate(date = as.Date(paste0(date, "-01"), "%Y-%b-%d")) %>%  # Ensure date is in Date format
          group_by(date) %>%
          summarise(value = sum(opv_total, na.rm=T)) %>%
          ungroup() %>%
          filter(!is.na(value))

        max_value <- max(data$value, na.rm = TRUE)
        
        plot <- ggplot(data=data)+
          geom_point(aes(x=date, y=value, text = scales::comma(value, accuracy=1)), color="black") +
          geom_line(aes(x=date, y=value, group=1), color="black") +
          scale_y_continuous(labels = format_y_labels, limits=c(0, max(data$value, na.rm=T)+(0.05*max(data$value, na.rm=T)))) +  # Apply dynamic formatting
          scale_x_date(labels = scales::date_format("%Y-%b"), breaks = "1 month") +  # Format date as %Y-%b and set breaks
          labs(y = "Total OPV Doses",
                ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1, size=8),
            axis.text.y = element_text(size=8),
            axis.title.y = element_text(size=8),
            axis.title.x = element_blank(),
            legend.title = element_blank(),
            legend.position = "none"
          )   
      }
      
      plotly_plot <- ggplotly(plot, tooltip="text")
      
      fig <- plotly_plot %>%
        layout(
          legend = list(
            showlegend=FALSE
          ),
          xaxis = list(
            tickfont = list(size = 8)  # Decrease font size for x-axis ticks
          ),
          yaxis = list(
            tickfont = list(size = 8),  # Decrease font size for y-axis ticks
            titlefont = list(size = 8)  # Decrease font size for y-axis title
          )
        )
      
      fig <- fig %>%
        plotly::config(displaylogo=FALSE,
                       modeBarButtons = (list(list("toImage"))))
      fig
      
    })
  })
}