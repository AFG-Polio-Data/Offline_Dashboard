cvaChartUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("cva_chart"), height = "320px")
}

cvaChartServer <- function(id, 
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
    
    output$cva_chart <- renderPlotly({
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
      
      
      if(cva_form_type() %in% c("Cross-Border") & cva_var == "Total OPV Doses Administered"){
        data <- data %>% 
          mutate(flow_type =  paste0(str_to_title(flow_type), "-Flow")) %>%
          group_by(flow_type) %>%
          summarise(`Age 0-10y` = sum(opv_0_10y, na.rm=T),
                    `Age above 10y` = sum(opv_above_10y, na.rm=T)) %>%
          select(flow_type, `Age 0-10y`, `Age above 10y`) %>%
          pivot_longer(-c("flow_type")) %>%
          mutate(group2 = case_when(name %in% c("Age 0-10y", "Age above 10y") ~ "Age",
                                   TRUE ~ NA_character_)) %>%
          rename(level = name,
                 group1 = flow_type) %>%
          filter(!is.na(value))
        
        plot <- ggplot(data = data) +
          geom_col(aes(x = interaction(level, group1), y = value, fill = group1, group = level, text=scales::comma(value)), color="black", position = "dodge") +
          labs(x = NULL, y = "Total OPV Doses", title = NULL) +
          scale_fill_manual(values = c("In-Flow" = "steelblue", "Out-Flow" = "#fc8d62")) +  # Specify colors for 'In' and 'Out'
          scale_x_discrete(labels = function(x) gsub("\\.", ": ", x)) +  # Add newlines between "In" and "Age Group"
          scale_y_continuous(labels = scales::comma) +
          guides(fill = "none") +  # Remove legend
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8),
                axis.text.y = element_text(size=8),
                axis.title.y = element_text(size=8))
                  
      }
      if(cva_form_type() %in% c("Combined") & cva_var == "Total OPV Doses Administered"){
        data <- data %>% 
          group_by(cva_type) %>%
          summarise(total = sum(opv_total, na.rm=T)) %>%
          select(cva_type, total) %>%
          pivot_longer(-c("cva_type")) %>%
          rename(level = name,
                 group1 = cva_type) %>%
          filter(!is.na(value)) %>%
          mutate(group1 = factor(group1, levels=c("Permanent Transit Teams", "Cross-Border", "Returnees", "IHR"))) %>%
          arrange(group1)
        
        plot <- ggplot(data = data) +
          geom_col(aes(x = group1, y = value, text=scales::comma(value)), fill="steelblue", color="black", position = "dodge") +
          labs(x = NULL, y = "Total OPV Doses", title = NULL) +
          scale_y_continuous(labels = scales::comma) +
          guides(fill = "none") +  # Remove legend
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8),
                axis.text.y = element_text(size=8),
                axis.title.y = element_text(size=8))
        
      }
      
      if(cva_form_type() == "Permanent Transit Teams" & cva_var == "Total OPV Doses Administered"){
        data <- data %>% 
          group_by(post_type) %>%
          summarise(`Age 0-59m` = sum(opv_0_59m, na.rm=T),
                    `Age 5-10y` = sum(opv_5_10y , na.rm=T)) %>%
          select(post_type, `Age 0-59m`, `Age 5-10y`) %>%
          pivot_longer(-c("post_type")) %>%
          mutate(group2 = case_when(name %in% c("Age 0-59m", "Age 5-10y") ~ "Age",
                                   TRUE ~ NA_character_)) %>%
          rename(level = name,
                 group1 = post_type) %>%
          filter(!is.na(value))
        
        plot <- ggplot(data = data) +
          geom_col(aes(x = interaction(level, group1), y = value, fill = group1, group = level, text=scales::comma(value)), color="black", position = "dodge") +
          labs(x = NULL, y = "Total OPV Doses", title = NULL) +
          scale_fill_manual(values = c("Nomad" = "steelblue", "PTT" = "#fc8d62")) +  # Specify colors for 'In' and 'Out'
          scale_x_discrete(labels = function(x) gsub("\\.", ": ", x)) +  # Add newlines between "In" and "Age Group"
          scale_y_continuous(labels = scales::comma) +
          guides(fill = "none") +  # Remove legend
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 25, hjust = 1, size=8),
                axis.text.y = element_text(size=8),
                axis.title.y = element_text(size=8))
        }
      
      if(cva_form_type() == "Returnees" & cva_var == "Total OPV Doses Administered"){
        data <- data %>% 
          summarise(`Age 0-59m` = sum(opv_0_59m, na.rm=T),
                    `Age 5-10y` = sum(opv_5_10y , na.rm=T),
                    `Age above 10y` = sum(opv_above_10y, na.rm=T)) %>%
          select(`Age 0-59m`, `Age 5-10y`, `Age above 10y`) %>%
          mutate(group="A") %>%
          pivot_longer(-c("group")) %>%
          mutate(group1 = case_when(name %in% c("Age 0-59m", "Age 5-10y", "Age above 10y") ~ name,
                                    TRUE ~ NA_character_)) %>%
          select(-c("group")) %>%
          filter(!is.na(value)) %>%
          mutate(group1 = factor(group1, levels=c("Age 0-59m", "Age 5-10y", "Age above 10y")))
        
        plot <- ggplot(data = data) +
          geom_col(aes(x = group1, y = value, text=scales::comma(value)), fill="steelblue", color="black", position = "dodge") +
          labs(x = NULL, y = "Total OPV Doses", title = NULL) +
          scale_y_continuous(labels = scales::comma) +
          guides(fill = "none") +  # Remove legend
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8),
                axis.text.y = element_text(size=8),
                axis.title.y = element_text(size=8))
      }
      if(cva_form_type() == "Returnees" & cva_var == "Total IPV Doses Administered"){
        data <- data %>% 
          summarise(`Male` = sum(ipv_male, na.rm=T),
                    `Female` = sum(ipv_female , na.rm=T),
                    `Unknown` = sum(ipv_unknown, na.rm=T)) %>%
          select(Male, Female, Unknown) %>%
          mutate(group="A") %>%
          pivot_longer(-c("group")) %>%
          mutate(group1 = case_when(name %in% c("Male", "Female", "Unknown") ~ name,
                                    TRUE ~ NA_character_)) %>%
          select(-c("group")) %>%
          filter(!is.na(value)) %>%
          mutate(group1 = factor(group1, levels=c("Female", "Male", "Unknown")))
        
        plot <- ggplot(data = data) +
          geom_col(aes(x = group1, y = value, text=scales::comma(value)), fill="steelblue", color="black", position = "dodge") +
          labs(x = NULL, y = "Total IPV Doses", title = NULL) +
          scale_y_continuous(labels = scales::comma) +
          guides(fill = "none") +  # Remove legend
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8),
                axis.text.y = element_text(size=8),
                axis.title.y = element_text(size=8))
      }
      
      if(cva_form_type() == "Cross-Border" & cva_var == "Refusal Rate"){
        data <- data %>% 
          select(flow_type, screened_0_10y, refusal_0_10y,
                 screened_above_10y, refusal_above_10y) %>%
          mutate(flow_type =  paste0(str_to_title(flow_type), "-Flow")) %>%
          group_by(flow_type) %>%
          summarize_all(~sum(., na.rm=T)) %>%
          ungroup() %>%
          mutate(`Age 0-10y` = ifelse(screened_0_10y > 0 & refusal_0_10y <= screened_0_10y & !is.na(refusal_0_10y),
                                      refusal_0_10y / screened_0_10y, NA_real_),
                 `Age above 10y` = ifelse(screened_above_10y > 0 & refusal_above_10y <= screened_above_10y & !is.na(refusal_above_10y),
                                          refusal_above_10y / screened_above_10y, NA_real_)) %>%
          select(flow_type, `Age 0-10y`, `Age above 10y`) %>%
          pivot_longer(-c("flow_type")) %>%
          mutate(group2 = case_when(name %in% c("Age 0-10y", "Age above 10y") ~ "Age",
                                    TRUE ~ NA_character_)) %>%
          rename(level = name,
                 group1 = flow_type) %>%
          filter(!is.na(value))
        
        plot <- ggplot(data = data) +
          geom_col(aes(x = interaction(level, group1), y = value, fill = group1, group = level, text=scales::percent(value)), color="black", position = "dodge") +
          labs(x = NULL, y = cva_var, title = NULL) +
          scale_fill_manual(values = c("In-Flow" = "steelblue", "Out-Flow" = "#fc8d62")) +  # Specify colors for 'In' and 'Out'
          scale_x_discrete(labels = function(x) gsub("\\.", ": ", x)) +  # Add newlines between "In" and "Age Group"
          scale_y_continuous(labels = scales::percent) +
          guides(fill = "none") +  # Remove legend
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8),
                axis.text.y = element_text(size=8),
                axis.title.y = element_text(size=8))
      } 
      
      if(cva_form_type() == "IHR" & cva_var == "Total OPV Doses Administered"){
        data <- cva_data()$cva_post 
        
        if(selected_region() == "All"){
          data <- data
        } else{
          if(selected_province() == "All"){
            data <- data %>%
              filter(region == selected_region())
          } else{
            if(selected_district() == "All"){
              data <- data %>%
                filter(region == selected_region() &
                         province == selected_province())
            } else{
              data <- data %>%
                filter(region == selected_region() &
                         province == selected_province() &
                         district == selected_district())
            }
          }}
        
        data <- data %>%
          mutate(group1 = ifelse(str_detect(post_name, district)==TRUE, post_name, paste0(district, ": ", post_name))) %>%
          group_by(group1) %>%
          summarise(value = sum(opv_total, na.rm=T)) %>%
          ungroup() %>%
          filter(!is.na(value))
        
        plot <- ggplot(data = data) +
          geom_col(aes(x = group1, y = value, text=scales::comma(value)), fill="steelblue", color="black", position = "dodge") +
          labs(x = NULL, y = "Total OPV Doses", title = NULL) +
          scale_y_continuous(labels = scales::comma) +
          guides(fill = "none") +  # Remove legend
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8),
                axis.text.y = element_text(size=8),
                axis.title.y = element_text(size=8))
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