# modules/overviewTableModule.R
library(shiny)
library(dplyr)
library(DT)
library(scales)

overviewTableUI <- function(id) {
  ns <- NS(id)
  DTOutput(ns("overview_table"))
}

overviewTableServer <- function(id, overview_filtered_sia_data, overview_filtered_admin_data, 
                                input_zoom_region_select, input_zoom_province_select, input_zoom_district_select, input_campaign_select, campaign_rpdc, overview_filtered_admin_data_ipv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    overview_table_data <- reactive({
      req(overview_filtered_sia_data())
      req(overview_filtered_admin_data())
      req(overview_filtered_admin_data_ipv())
      
      withProgress(message = 'Calculation in progress...',
                   value = 0, {
                     
                     if(!("All" %in% input_zoom_district_select())){
                       data <- overview_filtered_sia_data()$cluster_indicators
                       
                       data2 <- campaign_rpdc %>%
                         filter(campaign_name == input_campaign_select,
                                region_name == input_zoom_region_select(),
                                province_name == input_zoom_province_select(),
                                district_name == input_zoom_district_select()) %>%
                         select(region_name, province_name, district_name, cluster_name, ccode) %>%
                         rename(clustername2 = cluster_name) %>%
                         distinct()
                       
                       data <- data %>%
                         full_join(data2, by=c("region" = "region_name", "province" = "province_name", "district" = "district_name", "ccode")) %>%
                         mutate(clustername = ifelse(is.na(clustername), clustername2, clustername)) %>%
                         select(-c("clustername2")) %>%
                         rename(Region = region,
                                Province = province,
                                District = district,
                                Cluster = clustername,
                                `Cluster ID` = ccode) %>%
                         ungroup()
                       
                       admin_data <- overview_filtered_admin_data()$cluster %>%
                         ungroup() %>%
                         mutate(
                           total_vaccinated = rowSums(across(starts_with("total_children_vaccinated_day")), na.rm = TRUE),
                           `Admin Data: Total OPV Vaccinated` = scales::comma(total_vaccinated, accuracy = 1)
                         )
                       
                       admin_data_ipv <- overview_filtered_admin_data_ipv()$cluster %>%
                         ungroup() %>%
                         mutate(
                           total_vaccinated = rowSums(across(starts_with("total_children_vaccinated_day")), na.rm = TRUE),
                           `Admin Data: Total IPV Vaccinated` = scales::comma(total_vaccinated, accuracy = 1)
                         )
                       
                       data2 <- campaign_rpdc %>%
                         filter(campaign_name == input_campaign_select,
                                region_name == input_zoom_region_select(),
                                province_name == input_zoom_province_select(),
                                district_name == input_zoom_district_select()) %>%
                         select(region_name, province_name, district_name, cluster_name, ccode) %>%
                         rename(clustername2 = cluster_name) %>%
                         distinct()
                       
                       admin_data <- admin_data %>%
                         full_join(data2, by=c("region" = "region_name", "province" = "province_name", "district" = "district_name", "ccode")) %>%
                         mutate(clustername = ifelse(is.na(clustername), clustername2, clustername)) %>%
                         select(-c("clustername2")) %>%
                         rename(Region = region,
                                Province = province,
                                District = district,
                                Cluster = clustername,
                                `Cluster ID` = ccode) %>%
                         select(Region, Province, District, `Cluster ID`, Cluster, `Admin Data: Total OPV Vaccinated`)
                       
                       admin_data_ipv <- admin_data_ipv %>%
                         full_join(data2, by=c("region" = "region_name", "province" = "province_name", "district" = "district_name", "ccode")) %>%
                         mutate(clustername = ifelse(is.na(clustername), clustername2, clustername)) %>%
                         select(-c("clustername2")) %>%
                         rename(Region = region,
                                Province = province,
                                District = district,
                                Cluster = clustername,
                                `Cluster ID` = ccode) %>%
                         select(Region, Province, District, `Cluster ID`, Cluster, `Admin Data: Total IPV Vaccinated`)
                       
                     } else{
                       if(!("All" %in% input_zoom_province_select())){
                         data <- overview_filtered_sia_data()$district_indicators
                         data <- data %>%
                           rename(Region = region,
                                  Province = province,
                                  District = district) %>%
                           ungroup()
                         
                         admin_data <- overview_filtered_admin_data()$district %>%
                           ungroup() %>%
                           mutate(
                             total_vaccinated = rowSums(across(starts_with("total_children_vaccinated_day")), na.rm = TRUE),
                             pct = ifelse(!is.na(target_population) & target_population != 0, 
                                          total_vaccinated / target_population, 
                                          NA_real_),
                             `Admin Data: OPV Coverage` = paste0(
                               round(pct * 100, 0), "% (", 
                               scales::comma(total_vaccinated, accuracy = 1), "/", 
                               scales::comma(target_population, accuracy = 1), ")"
                             )
                           ) %>%
                           rename(
                             Region = region,
                             Province = province,
                             District = district
                           ) %>%
                           select(Region, Province, District, `Admin Data: OPV Coverage`)
                         
                         admin_data_ipv <- overview_filtered_admin_data_ipv()$district %>%
                           ungroup() %>%
                           mutate(
                             total_vaccinated = rowSums(across(starts_with("total_children_vaccinated_day")), na.rm = TRUE),
                             pct = ifelse(!is.na(target_population) & target_population != 0, 
                                          total_vaccinated / target_population, 
                                          NA_real_),
                             `Admin Data: IPV Coverage` = paste0(
                               round(pct * 100, 0), "% (", 
                               scales::comma(total_vaccinated, accuracy = 1), "/", 
                               scales::comma(target_population, accuracy = 1), ")"
                             )
                           ) %>%
                           rename(
                             Region = region,
                             Province = province,
                             District = district
                           ) %>%
                           select(Region, Province, District, `Admin Data: IPV Coverage`)
                         
                       } else{
                         if(!("All" %in% input_zoom_region_select())){
                           data <- overview_filtered_sia_data()$province_indicators
                           data <- data %>%
                             rename(Region = region,
                                    Province = province) %>%
                             ungroup()
                           
                           admin_data <- overview_filtered_admin_data()$province %>%
                             ungroup() %>%
                             mutate(
                               total_vaccinated = rowSums(across(starts_with("total_children_vaccinated_day")), na.rm = TRUE),
                               pct = ifelse(!is.na(target_population) & target_population != 0,
                                            total_vaccinated / target_population,
                                            NA_real_),
                               `Admin Data: OPV Coverage` = paste0(
                                 round(pct * 100, 0), "% (",
                                 scales::comma(total_vaccinated, accuracy = 1), "/",
                                 scales::comma(target_population, accuracy = 1), ")"
                               )
                             ) %>%
                             rename(
                               Region = region,
                               Province = province
                             ) %>%
                             select(Region, Province, `Admin Data: OPV Coverage`)
                           
                           admin_data_ipv <- overview_filtered_admin_data_ipv()$province %>%
                             ungroup() %>%
                             mutate(
                               total_vaccinated = rowSums(across(starts_with("total_children_vaccinated_day")), na.rm = TRUE),
                               pct = ifelse(!is.na(target_population) & target_population != 0,
                                            total_vaccinated / target_population,
                                            NA_real_),
                               `Admin Data: IPV Coverage` = paste0(
                                 round(pct * 100, 0), "% (",
                                 scales::comma(total_vaccinated, accuracy = 1), "/",
                                 scales::comma(target_population, accuracy = 1), ")"
                               )
                             ) %>%
                             rename(
                               Region = region,
                               Province = province
                             ) %>%
                             select(Region, Province, `Admin Data: IPV Coverage`)
                           
                         } else{
                           data <- overview_filtered_sia_data()$region_indicators
                           data <- data %>%
                             rename(Region = region) %>%
                             ungroup()
                           admin_data <- overview_filtered_admin_data()$region %>%
                             ungroup() %>%
                             mutate(
                               total_vaccinated = rowSums(across(starts_with("total_children_vaccinated_day")), na.rm = TRUE),
                               pct = ifelse(!is.na(target_population) & target_population != 0,
                                            total_vaccinated / target_population,
                                            NA_real_),
                               `Admin Data: OPV Coverage` = paste0(
                                 round(pct * 100, 0), "% (",
                                 scales::comma(total_vaccinated, accuracy = 1), "/",
                                 scales::comma(target_population, accuracy = 1), ")"
                               )
                             ) %>%
                             rename(Region = region) %>%
                             select(Region, `Admin Data: OPV Coverage`)
                           
                           admin_data_ipv <- overview_filtered_admin_data_ipv()$region %>%
                             ungroup() %>%
                             mutate(
                               total_vaccinated = rowSums(across(starts_with("total_children_vaccinated_day")), na.rm = TRUE),
                               pct = ifelse(!is.na(target_population) & target_population != 0,
                                            total_vaccinated / target_population,
                                            NA_real_),
                               `Admin Data: IPV Coverage` = paste0(
                                 round(pct * 100, 0), "% (",
                                 scales::comma(total_vaccinated, accuracy = 1), "/",
                                 scales::comma(target_population, accuracy = 1), ")"
                               )
                             ) %>%
                             rename(Region = region) %>%
                             select(Region, `Admin Data: IPV Coverage`)
                           
                         }}}
                     
                     incProgress(1/1)
                   }) #End Progress
      
      data <- list("post_data" = data,
                   "admin_data" = admin_data,
                   "admin_data_ipv" = admin_data_ipv)
      
      return(data)
    })
    
    overview_table_group_vars <- reactive({
      if(!("All" %in% input_zoom_district_select())){
        group_vars <- c("Region", "Province", "District", "Cluster ID", "Cluster")
      } else{
        if(!("All" %in% input_zoom_province_select())){
          group_vars <- c("Region", "Province", "District")
        } else{
          if(!("All" %in% input_zoom_region_select())){
            group_vars <- c("Region", "Province")
          } else{
            group_vars <- c("Region")
          }
        }
      }
      return(group_vars)
    })
    
    overview_table_df <- reactive({
      
      req(overview_table_group_vars())
      req(overview_table_data())
      withProgress(message = 'Calculation in progress...',
                   value = 0, {
                     
                     group_vars <- overview_table_group_vars()
                     
                     if(!("All" %in% input_zoom_district_select())){
                       table_df <- overview_table_data()$post_data %>%
                         # table_df <- data %>%
                         filter(indicator %in% c("pca_fm_coverage_0_59m", "pca_fm_coverage_ipv")) %>%
                         select(all_of(group_vars), indicator, label) %>%
                         unique() %>%
                         mutate(indicator = case_when(indicator == "pca_fm_coverage_0_59m" ~ "PCA: OPV Coverage",
                                                      indicator == "pca_fm_coverage_ipv" ~ "PCA: IPV Coverage")) %>%
                         pivot_wider(id_cols = all_of(group_vars),
                                     names_from = indicator,
                                     values_from = label) 
                       
                       table_df_admin <- overview_table_data()$admin_data %>%
                         rowwise() %>%
                         ungroup() %>%
                         select(all_of(group_vars), `Admin Data: Total OPV Vaccinated`) %>%
                         unique() %>%
                         arrange(across(all_of({{group_vars}})))
                       
                       table_df_admin_ipv <- overview_table_data()$admin_data_ipv %>%
                         rowwise() %>%
                         ungroup() %>%
                         select(all_of(group_vars), `Admin Data: Total IPV Vaccinated`) %>%
                         unique() %>%
                         arrange(across(all_of({{group_vars}})))
                       
                       for(i in c("PCA: OPV Coverage", "PCA: IPV Coverage")){
                         if(!(i %in% colnames(table_df))){
                           table_df <- table_df %>%
                             mutate(!!i := NA_character_)
                         }
                       }
                       
                       table_df <- table_df %>%
                         select(all_of(group_vars), "PCA: OPV Coverage", "PCA: IPV Coverage") %>%
                         mutate(across(c("PCA: OPV Coverage", "PCA: IPV Coverage"), ~ as.character(.))) %>%
                         mutate(across(c("PCA: OPV Coverage", "PCA: IPV Coverage"), ~ case_when(
                           is.na(.) | is.null(.) | . == "NULL" ~ NA_character_,
                           TRUE ~ .
                         ))) %>%
                         arrange(across(all_of({{group_vars}})))
                       
                       table_df <- table_df %>%
                         full_join(table_df_admin,
                                   by=group_vars) %>%
                         full_join(table_df_admin_ipv,
                                   by=group_vars) %>%
                         select(all_of(group_vars), `Admin Data: Total OPV Vaccinated`, `Admin Data: Total IPV Vaccinated`, `PCA: OPV Coverage`, `PCA: IPV Coverage`)  %>%
                         unique() %>%
                         arrange(across(all_of({{group_vars}})))
                       
                     } else{
                       if(!("All" %in% input_zoom_province_select())){
                         table_df <- overview_table_data()$post_data %>%
                           filter(indicator %in% c("pca_fm_coverage_0_59m", "lqas_result", "pca_fm_coverage_ipv", "lqas_fipv_result")) %>%
                           select(all_of(group_vars), indicator, label) %>%
                           mutate(indicator = case_when(indicator == "pca_fm_coverage_0_59m" ~ "PCA: OPV Coverage",
                                                        indicator == "pca_fm_coverage_ipv" ~ "PCA: IPV Coverage",
                                                        indicator == "lqas_result" ~ "LQAS: OPV Result",
                                                        indicator == "lqas_fipv_result" ~ "LQAS: IPV Result")) %>%
                           pivot_wider(id_cols = all_of(group_vars),
                                       names_from = indicator,
                                       values_from = label)
                         for(i in c("PCA: OPV Coverage", "PCA: IPV Coverage", "LQAS: OPV Result", "LQAS: IPV Result")){
                           if(!(i %in% colnames(table_df))){
                             table_df <- table_df %>%
                               mutate(!!i := NA_character_)
                           }
                         }
                         table_df <- table_df %>%
                           select(all_of(group_vars), "PCA: OPV Coverage", "PCA: IPV Coverage", "LQAS: OPV Result", "LQAS: IPV Result") %>%
                           mutate(across(c("PCA: OPV Coverage", "PCA: IPV Coverage", "LQAS: OPV Result", "LQAS: IPV Result"), ~ as.character(.))) %>%
                           mutate(across(c("PCA: OPV Coverage", "PCA: IPV Coverage", "LQAS: OPV Result", "LQAS: IPV Result"), ~ case_when(
                             is.na(.) | is.null(.) | . == "NULL" ~ NA_character_,
                             TRUE ~ .
                           ))) %>%
                           arrange(across(all_of({{group_vars}})))
                         
                         table_df_admin <- overview_table_data()$admin_data %>%
                           rowwise() %>%
                           ungroup() %>%
                           select(all_of(group_vars), `Admin Data: OPV Coverage`) %>%
                           arrange(across(all_of({{group_vars}})))
                         
                         table_df_admin_ipv <- overview_table_data()$admin_data_ipv %>%
                           rowwise() %>%
                           ungroup() %>%
                           select(all_of(group_vars), `Admin Data: IPV Coverage`) %>%
                           arrange(across(all_of({{group_vars}})))
                         
                         table_df <- table_df %>%
                           full_join(table_df_admin,
                                     by=group_vars) %>%
                           full_join(table_df_admin_ipv,
                                     by=group_vars) %>%
                           select(all_of(group_vars), `Admin Data: OPV Coverage`, `Admin Data: IPV Coverage`, `PCA: OPV Coverage`, `PCA: IPV Coverage`, `LQAS: OPV Result`, `LQAS: IPV Result`)  %>%
                           arrange(across(all_of({{group_vars}})))
                         
                       } else{
                         table_df <- overview_table_data()$post_data %>%
                           mutate(indicator = ifelse(indicator == "lqas_pct_lots_pass", "lqas_result", indicator)) %>%
                           filter(indicator %in% c("pca_fm_coverage_0_59m", "lqas_result", "lqas_fipv_result", "pca_fm_coverage_ipv")) %>%
                           select(all_of(group_vars), indicator, label) %>%
                           mutate(indicator = case_when(indicator == "pca_fm_coverage_0_59m" ~ "PCA: OPV Coverage",
                                                        indicator == "pca_fm_coverage_ipv" ~ "PCA: IPV Coverage",
                                                        indicator == "lqas_result" ~ "LQAS: Pct of Lots Passed (OPV)",
                                                        indicator == "lqas_fipv_result" ~ "LQAS: Pct of Lots Passed (IPV)")) %>%
                           pivot_wider(id_cols = all_of(group_vars),
                                       names_from = indicator,
                                       values_from = label) 
                         for(i in c("PCA: OPV Coverage", "PCA: IPV Coverage", "LQAS: Pct of Lots Passed (OPV)", "LQAS: Pct of Lots Passed (IPV)")){
                           if(!(i %in% colnames(table_df))){
                             table_df <- table_df %>%
                               mutate(!!i := NA_character_)
                           }
                         }
                         table_df <- table_df %>%
                           select(all_of(group_vars), "PCA: OPV Coverage", "PCA: IPV Coverage", "LQAS: Pct of Lots Passed (OPV)", "LQAS: Pct of Lots Passed (IPV)") %>%
                           mutate(across(c("PCA: OPV Coverage", "PCA: IPV Coverage", "LQAS: Pct of Lots Passed (OPV)", "LQAS: Pct of Lots Passed (IPV)"), ~ as.character(.))) %>%
                           mutate(across(c("PCA: OPV Coverage", "PCA: IPV Coverage", "LQAS: Pct of Lots Passed (OPV)", "LQAS: Pct of Lots Passed (IPV)"), ~ case_when(
                             is.na(.) | is.null(.) | . == "NULL" ~ NA_character_,
                             TRUE ~ .
                           ))) %>%
                           arrange(across(all_of({{group_vars}})))
                         table_df_admin <- overview_table_data()$admin_data %>%
                           rowwise() %>%
                           ungroup() %>%
                           select(all_of(group_vars), `Admin Data: OPV Coverage`) %>%
                           arrange(across(all_of({{group_vars}})))
                         
                         table_df_admin_ipv <- overview_table_data()$admin_data_ipv %>%
                           rowwise() %>%
                           ungroup() %>%
                           select(all_of(group_vars), `Admin Data: IPV Coverage`) %>%
                           arrange(across(all_of({{group_vars}})))
                         
                         table_df <- table_df %>%
                           full_join(table_df_admin,
                                     by=group_vars) %>%
                           full_join(table_df_admin_ipv,
                                     by=group_vars) %>%
                           select(all_of(group_vars), `Admin Data: OPV Coverage`, `Admin Data: IPV Coverage`, `PCA: OPV Coverage`, `PCA: IPV Coverage`, `LQAS: Pct of Lots Passed (OPV)`, `LQAS: Pct of Lots Passed (IPV)`) %>%
                           arrange(across(all_of({{group_vars}})))
                       }}
                   }) #End Progress
     
      
      return(table_df)
    })
    
    output$overview_table <- DT::renderDT({
      
      req(overview_table_df())
      table_df <- overview_table_df()
      
      #Remove any empty columns
      table_df <- table_df %>%
        janitor::remove_empty(which=c("cols"))
      table_df <- table_df %>% select_if(function(col) !all(col == 0))
      
      group_vars <- names(table_df)[names(table_df) %in% c("Region", "Province", "District", "Cluster ID", "Cluster")]
      
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
          if (group_vars[i] == "Cluster ID"){
            total_row[[group_vars[i]]] <- NA
          } else {
            total_row[[group_vars[i]]] <- unique(table_df[[group_vars[i]]])[1]
          }
        }}
      
      # Combine total row with original data
      table_df <- rbind(total_row, table_df)
      
      #If admin - total vaccinated exists then force it into numeric format
      if("Admin Data: Total OPV Vaccinated" %in% colnames(table_df)){
        table_df <- table_df %>%
          mutate(`Admin Data: Total OPV Vaccinated` = as.numeric(str_remove_all(`Admin Data: Total OPV Vaccinated`, "[^0-9\\.]")))
      }
      if("Admin Data: Total IPV Vaccinated" %in% colnames(table_df)){
        table_df <- table_df %>%
          mutate(`Admin Data: Total IPV Vaccinated` = as.numeric(str_remove_all(`Admin Data: Total IPV Vaccinated`, "[^0-9\\.]")))
      }
      # Extract percentages into new columns if they exist
      percent_columns <- c("Admin Data: OPV Coverage", "PCA: OPV Coverage", "LQAS: Pct of Lots Passed (OPV)", 
                           "Admin Data: IPV Coverage", "PCA: IPV Coverage", "LQAS: Pct of Lots Passed (IPV)")
      for (col in percent_columns) {
        if (col %in% names(table_df)) {
          table_df[[paste0(col, "_Pct")]] <- as.numeric(sub("%.*", "", table_df[[col]]))
        }
      }
      
      # Determine the indices of the percent columns
      percent_col_indices <- which(names(table_df) %in% paste0(percent_columns, "_Pct"))
      
      # Create columnDefs for sorting
      column_defs <- lapply(seq_along(percent_columns), function(i) {
        pct_col <- paste0(percent_columns[i], "_Pct")
        if (pct_col %in% names(table_df)) {
          list(orderData = which(names(table_df) == pct_col) - 1, targets = which(names(table_df) == percent_columns[i]) - 1)
        }
      })
      column_defs <- Filter(Negate(is.null), column_defs)
      
      # Add visibility settings for percentage columns
      if (length(percent_col_indices) > 0) {
        column_defs <- append(column_defs, list(list(visible = FALSE, targets = percent_col_indices - 1)))
      }
      column_defs <- Filter(Negate(is.null), column_defs)
      
      # Create the datatable
      datatable_output <- DT::datatable(
        data = table_df,
        extensions = 'Buttons',
        callback = JS("$.fn.dataTable.ext.errMode = 'none';"),
        options = list(
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
              filename = "Campaign_Overview_Indicators",
              title = "Campaign Overview Indicators",
              exportOptions = list(
                modifier = list(page = 'all')
              )
            )
          ),
          columnDefs = column_defs
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
      for(i in c("Admin Data: Total OPV Vaccinated")){
        if(i %in% colnames(table_df)){
          datatable_output <- DT::formatCurrency(datatable_output, columns = i, currency = "", interval = 3, mark = ",", digits=0)
        }
      }
      return(datatable_output)
    })
    
    
    })
}
