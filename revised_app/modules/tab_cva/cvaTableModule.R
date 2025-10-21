# modules/cva_table_module.R

cvaTableUI <- function(id) {
  ns <- NS(id)
  DT::DTOutput(ns("cva_table"))
}

cvaTableServer <- function(id, 
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
    
    
    cva_table_data <- reactive({
      req(cva_data())
      req(selected_region())
      req(selected_province())
      req(selected_district())
      req(cva_form_type())
      req(selected_indicator())
      req(start_month())
      req(end_month())
      
      cva_var <- selected_indicator()
      cva_type <- cva_form_type()
        
        if(selected_region() == "All"){
          data <- cva_data()$cva_region
        } else{
          if(selected_province() == "All"){
            data <- cva_data()$cva_province %>%
              filter(region == selected_region())
          } else{
              if(selected_district() == "All"){
                data <- cva_data()$cva_district %>%
                  filter(region == selected_region() &
                         province == selected_province()) 
              } else{
                data <- cva_data()$cva_post %>%
                  filter(region == selected_region() &
                         province == selected_province() &
                         district == selected_district())
              }
          }}
          
        if(!("All" %in% selected_district())){
          group_vars <- c("region", "province", "district", "post_name")
        } else{
          if(!("All" %in% selected_province())){
            group_vars <- c("region", "province", "district")
          } else{
            if(!("All" %in% selected_region())){
              group_vars <- c("region", "province")
            } else{
              group_vars <- c("region")
            }
          }
        }
       
        if(cva_form_type() %in% c("Cross-Border") & cva_var == "Total OPV Doses Administered"){
          data <- data %>% 
            mutate(flow_type =  paste0(str_to_title(flow_type), "-Flow")) %>%
            group_by(across(all_of(group_vars)), flow_type) %>%
            summarise(value = sum(opv_total, na.rm=T)) %>%
            ungroup() %>%
            filter(!is.na(value)) %>%
            pivot_wider(names_from = flow_type, values_from = value) %>%
            full_join(
              data %>% 
                group_by(across(all_of(group_vars))) %>%
                summarise(Total = sum(opv_total, na.rm=T)) %>%
                ungroup() %>%
                filter(!is.na(Total)),
              by=c(group_vars)
            )
        }
        if(cva_form_type() %in% c("Combined") & cva_var == "Total OPV Doses Administered"){
          data <- data %>% 
            group_by(across(all_of(group_vars)), cva_type) %>%
            summarise(value = sum(opv_total, na.rm=T)) %>%
            ungroup() %>%
            filter(!is.na(value)) %>%
            pivot_wider(names_from = cva_type, values_from = value) %>%
            full_join(
              data %>% 
                group_by(across(all_of(group_vars))) %>%
                summarise(Total = sum(opv_total, na.rm=T)) %>%
                ungroup() %>%
                filter(!is.na(Total)),
              by=c(group_vars)
            )
          
        }
        
        if(cva_form_type() == "Permanent Transit Teams" & cva_var == "Total OPV Doses Administered"){
          data <- data %>% 
            group_by(across(all_of(group_vars)), post_type) %>%
            summarise(value = sum(opv_total, na.rm=T)) %>%
            ungroup() %>%
            filter(!is.na(value)) %>%
            pivot_wider(names_from = post_type, values_from = value) %>%
            full_join(
              data %>% 
                group_by(across(all_of(group_vars))) %>%
                summarise(Total = sum(opv_total, na.rm=T)) %>%
                ungroup() %>%
                filter(!is.na(Total)),
              by=c(group_vars)
            )
        
        }
        
        if(cva_form_type() == "Returnees" & cva_var == "Total OPV Doses Administered"){
          data <- data %>%
            group_by(across(all_of(group_vars))) %>%
            summarise(value = sum(opv_total, na.rm=T)) %>%
            ungroup() %>%
            filter(!is.na(value)) %>%
            rename(`Total OPV Doses` = value) %>%
            full_join(
              data %>%
                group_by(across(all_of(group_vars))) %>%
                summarise(value = sum(ipv_total, na.rm=T)) %>%
                ungroup() %>%
                filter(!is.na(value)) %>%
                rename(`Total IPV Doses` = value),
              by=c(group_vars)
            )
        }
        if(cva_form_type() == "Returnees" & cva_var == "Total IPV Doses Administered"){
          data <- data %>%
            group_by(across(all_of(group_vars))) %>%
            summarise(value = sum(opv_total, na.rm=T)) %>%
            ungroup() %>%
            filter(!is.na(value)) %>%
            rename(`Total OPV Doses` = value) %>%
            full_join(
              data %>%
                group_by(across(all_of(group_vars))) %>%
                summarise(value = sum(ipv_total, na.rm=T)) %>%
                ungroup() %>%
                filter(!is.na(value)) %>%
                rename(`Total IPV Doses` = value),
              by=c(group_vars)
            )
        
        }
        
        if(cva_form_type() == "Cross-Border" & cva_var == "Refusal Rate"){
          
          data <- data %>% 
            select(all_of(group_vars), flow_type, screened_total, refusal_total) %>%
            mutate(flow_type =  paste0(str_to_title(flow_type), "-Flow Refusal Rate")) %>%
            group_by(across(all_of(group_vars)), flow_type) %>%
            summarise_all(~sum(., na.rm=T)) %>%
            ungroup() %>%
            rowwise() %>%
            mutate(value = ifelse(screened_total > 0 & refusal_total <= screened_total & !is.na(refusal_total),
                                  refusal_total / screened_total, NA_real_)) %>%
            ungroup() %>%
            select(all_of(group_vars), flow_type, value) %>%
            filter(!is.na(value) & value != 0) %>%
            pivot_wider(names_from = flow_type, values_from = value) %>%
            full_join(data %>% 
                        select(all_of(group_vars), screened_total, refusal_total) %>%
                        group_by(across(all_of(group_vars))) %>%
                        summarise_all(~sum(., na.rm=T)) %>%
                        ungroup() %>%
                        rowwise() %>%
                        mutate(value = ifelse(screened_total > 0 & refusal_total <= screened_total & !is.na(refusal_total),
                                              refusal_total / screened_total, NA_real_)) %>%
                        ungroup() %>%
                        select(all_of(group_vars), value) %>%
                        filter(!is.na(value) & value != 0) %>%
                        rename(`Total Refusal Rate` = value),
                      by=c(group_vars))
          
        } 
        
        if(cva_form_type() == "IHR" & cva_var == "Total OPV Doses Administered"){
          data <- data %>%
            group_by(across(all_of(group_vars))) %>%
            summarise(value = sum(opv_total, na.rm=T)) %>%
            ungroup() %>%
            filter(!is.na(value)) %>%
            rename(`Total OPV Doses` = value)
          
        }
      if("region" %in% colnames(data)){
        data <- data %>%
          rename(Region = region)
      }
      if("province" %in% colnames(data)){
        data <- data %>%
          rename(Province = province)
      }
      if("district" %in% colnames(data)){
        data <- data %>%
          rename(District = district)
      }
      if("post_name" %in% colnames(data)){
        data <- data %>%
          rename(`Post Name` = post_name)
      }
        data 
          
      })
   
    
    output$cva_table <- DT::renderDT({
      req(cva_table_data())
      req(selected_district())
      req(selected_province()) 
      req(selected_region())
      
      cva_var <- selected_indicator()
      table_df <- cva_table_data()
      group_vars <- names(table_df)[names(table_df) %in% c("Region", "Province", "District", "Post Name")]
      
      # Calculate totals for each column
      
      total_row <- sapply(table_df, calculate_total)
      
      # Convert total row to a data frame row
      total_row <- as.data.frame(t(total_row), stringsAsFactors = FALSE)
      colnames(total_row) <- colnames(table_df)
      
      
      # Handle grouping variables for the total row
      for (i in seq_along(group_vars)) {
        if (i == length(group_vars)) {
          total_row[[group_vars[i]]] <- "Total"
        } else {
            total_row[[group_vars[i]]] <- unique(table_df[[group_vars[i]]])[1]
          }
        }
      
      # Combine total row with original data
      table_df <- rbind(total_row, table_df) 
      
      all_num_cols <- setdiff(colnames(table_df), group_vars)
      
      table_df <- table_df %>%
        mutate_at(all_num_cols, ~as.numeric(.))
      
      # Create the datatable output
      datatable_output <- DT::datatable(
        data = table_df,
        extensions = 'Buttons',
        callback = JS("$.fn.dataTable.ext.errMode = 'none';"),
        options = list(
          ordering = TRUE,
          dom = 'Bfrtp',
          pageLength = -1,  # Display all rows on a single page
          scrollY = TRUE,
          scrollX = TRUE,
          stateSave = FALSE,
          server = FALSE,
          lengthMenu = list(c(-1), c("All")),
          paging = FALSE,
          buttons = list(
            list(
              extend = 'excel',
              text = 'Download Table',
              filename = "CVA_Data_Summary",
              title = "CVA Data Summary",
              exportOptions = list(
                modifier = list(page = 'all')
              )
            )
          )
        ),
        rownames = FALSE
      ) %>%
        # Apply bold formatting to the 'Total' row
        formatStyle(
          columns = colnames(table_df),
          valueColumns = group_vars[length(group_vars)],
          target = 'row',
          backgroundColor = styleEqual("Total", 'lightgrey'),
          fontWeight = styleEqual("Total", 'bold')
        )
      
      for(i in c("In-Flow", "Out-Flow", "In-Flow Refusal Rate", "Out-Flow Refusal Rate", "Total Refusal Rate")){
        if(i %in% colnames(table_df) & cva_var == "Refusal Rate"){
        datatable_output <- formatPercentage(datatable_output, columns = i, digits=0)
        }
        if(i %in% colnames(table_df) & cva_var == "Total OPV Doses Administered"){
          datatable_output <- DT::formatCurrency(datatable_output, columns = i, currency = "", interval = 3, mark = ",", digits=0)
        }
        
      }
      
      for(i in c("Permanent Transit Teams", "IHR", "Returnees", "Cross-Border", "PTT", "Nomad", "Total OPV Doses", "Total IPV Doses", "Total")){
        if(i %in% colnames(table_df)){
          datatable_output <- DT::formatCurrency(datatable_output, columns = i, currency = "", interval = 3, mark = ",", digits=0)
        }
      }
      
      
      datatable_output
    }) 
  })
}