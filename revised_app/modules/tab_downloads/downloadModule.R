# UI Function
downloadButtonsUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 8,
             downloadButton(ns("download_admin_cluster"), "Admin Data - Cluster Aggregated",
                            style = "background-color: white; color: black;", class = "download-button"))
    ),
    fluidRow(
      column(width = 8,
             downloadButton(ns("download_admin_district"), "Admin Data - District Aggregated",
                            style = "background-color: white; color: black;", class = "download-button"))
    ),
    fluidRow(
      column(width = 8,
             downloadButton(ns("download_precampaign_training_district"), "Pre-Campaign Training Data - District Aggregated",
                            style = "background-color: white; color: black;", class = "download-button"))
    ),
    fluidRow(
      column(width = 8,
             downloadButton(ns("download_icm_cluster"), "Intra-Campaign Monitoring Data - Cluster Aggregated",
                            style = "background-color: white; color: black;", class = "download-button"))
    ),
    fluidRow(
      column(width = 8,
             downloadButton(ns("download_icm_district"), "Intra-Campaign Monitoring Data - District Aggregated",
                            style = "background-color: white; color: black;", class = "download-button"))
    ),
    fluidRow(
      column(width = 8,
             downloadButton(ns("download_post_district"), "Post-Campaign Data - District Aggregated",
                            style = "background-color: white; color: black;", class = "download-button"))
    ),
    fluidRow(
      column(width = 8,
             downloadButton(ns("download_cva"), "Monthly Complementary Vaccination Activities Data (within date range specified in side-bar)",
                            style = "background-color: white; color: black;", class = "download-button"))
    ),
    fluidRow(
      column(width = 8,
             downloadButton(ns("download_campaign_rpdc"), "Active Geographies List",
                            style = "background-color: white; color: black;", class = "download-button"))
    ),
    fluidRow(
      column(width = 8,
             downloadButton(ns("download_completeness"), "Data Completeness: Submissions by Cluster and District",
                            style = "background-color: white; color: black;", class = "download-button"))
    )
  )
}

# Server Function
downloadTabServer <- function(
    id,
    selected_campaigns,
    all_apmis_data_list,
    campaign_rpdc,
    df_campaigns,
    shp_clusters,
    download_filtered_sia_data,  
    download_cva_data,
    session_inputs
    # ,
    # logger = NULL
){
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    download_region <- session_inputs$download_region
    download_province <- session_inputs$download_province
    download_district <- session_inputs$download_district
    
    debounced_download_campaign_list <- reactive({
      session_inputs$download_campaign_list()
    }) %>% debounce(500)
    
    download_campaign_filtered_sia_data <- reactive({
      req(download_filtered_sia_data())
      
      purrr::map(download_filtered_sia_data(), function(x){
        x %>% filter(campaign_name %in% debounced_download_campaign_list())
      })
    })
    download_cva_data_filtered <- reactive({
      req(download_cva_data())
      
      data_list <- download_cva_data()$cva_post
      
      filtered_list <- purrr::map(data_list, function(df) {
          if (!is.data.frame(df)) return(NULL)
          
          if (download_region() == "All") {
            return(df)
          } else if (download_province() == "All") {
            return(dplyr::filter(df, region == download_region()))
          } else if (download_district() == "All") {
            return(dplyr::filter(df, region == download_region(),
                                 province == download_province()))
          } else {
            return(dplyr::filter(df, region == download_region(),
                                 province == download_province(),
                                 district == download_district()))
          }
      })
      
      return(filtered_list)
    })
    
    output$download_cva <- downloadHandler(
      filename = "Complementary_Vaccination_Activities_Data.zip",
      content = function(file) {
        withProgress(
          message = 'Preparing Download...',
          value = 0,
          {
            data <- download_cva_data_filtered()
            
            combined <- data$combined %>%
              select(cva_type, date, year, month, region, province, district, rcode, pcode, dcode, post_name, everything())
            
            cb <- data$cb %>%
              select(colnames(data$cb)[!grepl("_rate", colnames(data$cb))]) %>%
              select(cva_type, date, year, month, region, province, district, rcode, pcode, dcode, post_name, flow_type, everything())

            ihr <- data$ihr %>%
              select(cva_type, date, year, month, region, province, district, rcode, pcode, dcode, post_name, everything())
            
            ptt <- data$ptt %>%
              select(cva_type, date, year, month, region, province, district, rcode, pcode, dcode, post_name, post_type, everything())
            
            returnees <- data$returnees %>%
              select(cva_type, date, year, month, region , province, district, rcode, pcode, dcode, post_name, everything())
            
            # Create a temporary directory to store CSV files
            temp_dir <- tempdir()
            existing_xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            file.remove(existing_xlsx_files)
            
            if(nrow(combined) >= 1){
              workbook <- openxlsx::createWorkbook()
              openxlsx::addWorksheet(wb=workbook, sheetName="Combined")
              openxlsx::addWorksheet(wb=workbook, sheetName="Cross-Border")
              openxlsx::addWorksheet(wb=workbook, sheetName="IHR")
              openxlsx::addWorksheet(wb=workbook, sheetName="PTT")
              openxlsx::addWorksheet(wb=workbook, sheetName="Returnees")
              
              writeData(workbook, sheet = "Combined", x = combined, startRow = 1, startCol = 1, colNames = TRUE)
              writeData(workbook, sheet = "Cross-Border", x = cb, startRow = 1, startCol = 1, colNames = TRUE)
              writeData(workbook, sheet = "IHR", x = ihr, startRow = 1, startCol = 1, colNames = TRUE)
              writeData(workbook, sheet = "PTT", x = ptt, startRow = 1, startCol = 1, colNames = TRUE)
              writeData(workbook, sheet = "Returnees", x = returnees, startRow = 1, startCol = 1, colNames = TRUE)
              
              
              openxlsx::saveWorkbook(workbook, file.path(temp_dir, paste0("Complementary_Vaccination_Activities_Data.xlsx")), overwrite=TRUE)
              
            }
            
            
            # Get list of all XLSX files in the temporary directory
            xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            
            # Create a temporary file for the ZIP archive
            zip_file <- tempfile(fileext = ".zip")
            
            # Zip the XLSX files without preserving folder structure
            utils::zip(zip_file, files = xlsx_files, flags = "-j")
            
            # Copy the zip file to the specified file location
            file.copy(zip_file, file)
            # unlink(temp_dir, recursive = TRUE, force = TRUE)
            
          }
        )
        
        # Log download
        # if (!is.null(logger)) logger$logUserEvent(
        #   page = "download_data",
        #   type = "download",
        #   value = "CVA Data",
        #   region = session_inputs$selected_region(),
        #   province = session_inputs$selected_province(),
        #   district = session_inputs$selected_district()
        # )
      }
    )
    
    output$download_admin_cluster <- downloadHandler(
      filename = "Campaign_Admin_Data_Cluster_Level.zip",
      content = function(file) {
        withProgress(
          message = 'Preparing Download...',
          value = 0,
          {
            data <- download_campaign_filtered_sia_data()
            # Create a temporary directory to store CSV files
            temp_dir <- tempdir()
            existing_xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            file.remove(existing_xlsx_files)
            
            if(nrow(data$export_admin_h2h_0_4_day1) >= 1){
              workbook <- openxlsx::loadWorkbook(file.path("data/Export_Templates/DC_Daily_Compilation_H2H_0_4_Cluster.xlsx"))
              writeData(workbook, sheet = "Day 1", x = data$export_admin_h2h_0_4_day1, startRow = 5, startCol = 1, colNames = FALSE)
              writeData(workbook, sheet = "Day 2", x = data$export_admin_h2h_0_4_day2, startRow = 5, startCol = 1, colNames = FALSE)
              writeData(workbook, sheet = "Day 3", x = data$export_admin_h2h_0_4_day3, startRow = 5, startCol = 1, colNames = FALSE)
              writeData(workbook, sheet = "Day 4", x = data$export_admin_h2h_0_4_day4, startRow = 5, startCol = 1, colNames = FALSE)
              writeData(workbook, sheet = "Grand Total", x = data$export_admin_h2h_0_4_gt, startRow = 5, startCol = 1, colNames = FALSE)
              # Create a percentage style
              percent_style <- createStyle(numFmt = "0%")
              
              all_hh_data_wide <- data$export_admin_h2h_0_4_day1 %>%
                full_join(data$export_admin_h2h_0_4_day2, by=c("campaign_name", "region", "rcode", "province", "pcode", "district", "dcode", "clustername", "ccode"))  %>%
                full_join(data$export_admin_h2h_0_4_day3, by=c("campaign_name", "region", "rcode", "province", "pcode", "district", "dcode", "clustername", "ccode"))  %>%
                full_join(data$export_admin_h2h_0_4_day4, by=c("campaign_name", "region", "rcode", "province", "pcode", "district", "dcode", "clustername", "ccode"))  %>%
                full_join(data$export_admin_h2h_0_4_gt, by=c("campaign_name", "region", "rcode", "province", "pcode", "district", "dcode", "clustername", "ccode"))
              
              colnames(all_hh_data_wide) <- gsub("f_", "f", colnames(all_hh_data_wide))
              
              openxlsx::addWorksheet(wb=workbook, sheetName="Machine_Readable_Wide")
              writeData(workbook, sheet = "Machine_Readable_Wide", x = all_hh_data_wide, startRow = 1, startCol = 1, colNames = TRUE)
              
              day1 <- data$export_admin_h2h_0_4_day1 %>% mutate(campaign_day = 1)
              day2 <- data$export_admin_h2h_0_4_day2 %>% mutate(campaign_day = 2)
              day3 <- data$export_admin_h2h_0_4_day3 %>% mutate(campaign_day = 3)
              day4 <- data$export_admin_h2h_0_4_day4 %>% mutate(campaign_day = 4) %>% select(-contains("_day1_3"))
              
              colnames(day1) <- gsub("_day1$", "", colnames(day1))
              colnames(day2) <- gsub("_day2$", "", colnames(day2))
              colnames(day3) <- gsub("_day3$", "", colnames(day3))
              colnames(day4) <- gsub("_day4$", "", colnames(day4))
              
              all_hh_data_long <- day1 %>%
                bind_rows(day2) %>%
                bind_rows(day3) %>%
                bind_rows(day4) %>%
                select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, campaign_day, everything()) %>%
                arrange(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, campaign_day)
              
              colnames(all_hh_data_long) <- gsub("f_", "f", colnames(all_hh_data_long))
              
              openxlsx::addWorksheet(wb=workbook, sheetName="Machine_Readable_Long")
              writeData(workbook, sheet = "Machine_Readable_Long", x = all_hh_data_long, startRow = 1, startCol = 1, colNames = TRUE)
              
              openxlsx::saveWorkbook(workbook, file.path(temp_dir, paste0("DC_Daily_Compilation_H2H_0_59_months_4_days_cluster_level.xlsx")), overwrite=TRUE)
              
            }
            
            if(nrow(data$export_admin_s2s_m2m) >= 1){
              export_admin_s2s_m2m <- data$export_admin_s2s_m2m %>%
                ungroup() %>%
                group_by(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode) %>%
                summarise_all(~ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))) %>%
                ungroup() %>%
                rowwise() %>%
                mutate_at(c("total_sites_visited", "no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3"), 
                          ~ifelse(. == 0, NA, .))
              
              day1 <- export_admin_s2s_m2m %>%
                select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything()) %>%
                select(-contains("_day2")) %>%
                select(-contains("_day3")) %>%
                select(-contains("_days13")) %>%
                select(-c("total_sites_visited")) %>%
                mutate(campaign_day = 1)
              
              day2 <- export_admin_s2s_m2m %>%
                select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything()) %>%
                select(-contains("_day1")) %>%
                select(-contains("_day3")) %>%
                select(-contains("_days13")) %>%
                select(-c("total_sites_visited")) %>%
                mutate(campaign_day = 2)
              
              day3 <- export_admin_s2s_m2m %>%
                select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, everything()) %>%
                select(-contains("_day1")) %>%
                select(-contains("_day2")) %>%
                select(-contains("_days13")) %>%
                select(-c("total_sites_visited")) %>%
                mutate(campaign_day = 3)
              
              colnames(day1) <- gsub("_day1$", "", colnames(day1))
              colnames(day2) <- gsub("_day2$", "", colnames(day2))
              colnames(day3) <- gsub("_day3$", "", colnames(day3))
              
              all_s2s_long <- day1 %>%
                bind_rows(day2) %>%
                bind_rows(day3) %>%
                select(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, campaign_day, everything()) %>%
                arrange(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode, campaign_day)
              
              colnames(all_s2s_long) <- gsub("f_", "f", colnames(all_s2s_long))
              
              workbook <- openxlsx::loadWorkbook(file.path("data/Export_Templates/DC_Daily_Compilation_S2S_M2M_0_4_Cluster.xlsx"))
              writeData(workbook, sheet = "S2S_Data", x = export_admin_s2s_m2m, startRow = 5, startCol = 1, colNames = FALSE)
              # writeData(workbook, sheet = "Missed Child Conversion Summary", x = data$export_conversion_cluster, startRow = 3, startCol = 1, colNames = FALSE)
              openxlsx::addWorksheet(wb=workbook, sheetName="Machine_Readable_Wide")
              writeData(workbook, sheet = "Machine_Readable_Wide", x = export_admin_s2s_m2m, startRow = 1, startCol = 1, colNames = TRUE)
              openxlsx::addWorksheet(wb=workbook, sheetName="Machine_Readable_Long")
              writeData(workbook, sheet = "Machine_Readable_Long", x = all_s2s_long, startRow = 1, startCol = 1, colNames = TRUE)
              openxlsx::saveWorkbook(workbook, file.path(temp_dir, paste0("DC_Daily_Compilation_S2S_M2M_0_59_months_3_days_cluster_level.xlsx")), overwrite=TRUE)
            }
            
            if(nrow(data$export_admin_ipv_7day) >= 1){
              export_admin_ipv_7day <- data$export_admin_ipv_7day %>%
                ungroup() %>%
                group_by(campaign_name, region, rcode, province, pcode, district, dcode, clustername, ccode) %>%
                summarise_all(~ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))) %>%
                ungroup() 
              
              workbook <- openxlsx::createWorkbook()
              openxlsx::addWorksheet(wb=workbook, sheetName="Data")
              writeData(workbook, sheet = "Data", x = export_admin_ipv_7day, startRow = 1, startCol = 1, colNames = TRUE)
              openxlsx::saveWorkbook(workbook, file.path(temp_dir, paste0("DC_Daily_Compilation_fIPV_7_days_cluster_level.xlsx")), overwrite=TRUE)
            }
            
            if(nrow(data$export_conversion_cluster) >= 1){
              workbook <- openxlsx::loadWorkbook(file.path("data/Export_Templates/Missed_Child_Conversion_Summary_Cluster.xlsx"))
              writeData(workbook, sheet = "Missed Child Conversion Summary", x = data$export_conversion_cluster, startRow = 3, startCol = 1, colNames = FALSE)
              # Create a percentage style
              percent_style <- createStyle(numFmt = "0%")
              
              addStyle(workbook, sheet = "Missed Child Conversion Summary", style = percent_style,
                       rows = 3:(nrow(data$export_conversion_cluster) + 2),
                       cols = c(13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 44),
                       gridExpand = TRUE)
              
              openxlsx::saveWorkbook(workbook, file.path(temp_dir, paste0("Missed_Child_Conversion_Summary_cluster_level.xlsx")), overwrite=TRUE)
              
            }
            
            # Get list of all XLSX files in the temporary directory
            xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            
            # Create a temporary file for the ZIP archive
            zip_file <- tempfile(fileext = ".zip")
            
            # Zip the XLSX files without preserving folder structure
            utils::zip(zip_file, files = xlsx_files, flags = "-j")
            
            # Copy the zip file to the specified file location
            file.copy(zip_file, file)
            # unlink(temp_dir, recursive = TRUE, force = TRUE)
            
          }
        )
        # Log download
        # if (!is.null(logger)) logger$logUserEvent(
        #   page = "download_data",
        #   type = "download",
        #   value = "Admin - Cluster Level",
        #   region = session_inputs$selected_region(),
        #   province = session_inputs$selected_province(),
        #   district = session_inputs$selected_district()
        # )
      }
    )
    
    output$download_admin_district <- downloadHandler(
      filename = "Campaign_Admin_Data_District_Level.zip",
      content = function(file) {
        withProgress(
          message = 'Preparing Download...',
          value = 0,
          {
            data <- download_campaign_filtered_sia_data()
            # Create a temporary directory to store CSV files
            temp_dir <- tempdir()
            # Remove existing CSV files from the temporary directory
            # existing_csv_files <- list.files(temp_dir, pattern = "\\.csv$", full.names = TRUE)
            # file.remove(existing_csv_files)
            existing_xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            file.remove(existing_xlsx_files)
            
            if(nrow(data$export_admin_h2h_0_4_day1) >= 1){
              export_admin_h2h_0_4_day1 <- data$export_admin_h2h_0_4_day1 %>%
                ungroup() %>%
                select(-c("clustername", "ccode")) %>%
                group_by(campaign_name, region, rcode, province, pcode, district, dcode) %>%
                summarise_all(~ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))) %>%
                ungroup()
              
              export_admin_h2h_0_4_day2 <- data$export_admin_h2h_0_4_day2 %>%
                ungroup() %>%
                select(-c("clustername", "ccode")) %>%
                group_by(campaign_name, region, rcode, province, pcode, district, dcode) %>%
                summarise_all(~ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))) %>%
                ungroup()
              
              export_admin_h2h_0_4_day3 <- data$export_admin_h2h_0_4_day3 %>%
                ungroup() %>%
                select(-c("clustername", "ccode")) %>%
                group_by(campaign_name, region, rcode, province, pcode, district, dcode) %>%
                summarise_all(~ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))) %>%
                ungroup()
              
              export_admin_h2h_0_4_day4 <- data$export_admin_h2h_0_4_day4 %>%
                ungroup() %>%
                select(-c("clustername", "ccode")) %>%
                group_by(campaign_name, region, rcode, province, pcode, district, dcode) %>%
                summarise_all(~ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))) %>%
                ungroup()
              
              export_admin_h2h_0_4_gt <- data$export_admin_h2h_0_4_gt %>%
                ungroup() %>%
                select(-c("clustername", "ccode")) %>%
                group_by(campaign_name, region, rcode, province, pcode, district, dcode) %>%
                summarise_all(~ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))) %>%
                ungroup()
              
              all_hh_data_wide <- export_admin_h2h_0_4_day1 %>%
                full_join(export_admin_h2h_0_4_day2, by=c("campaign_name", "region", "rcode", "province", "pcode", "district", "dcode"))  %>%
                full_join(export_admin_h2h_0_4_day3, by=c("campaign_name", "region", "rcode", "province", "pcode", "district", "dcode"))  %>%
                full_join(export_admin_h2h_0_4_day4, by=c("campaign_name", "region", "rcode", "province", "pcode", "district", "dcode"))  %>%
                full_join(export_admin_h2h_0_4_gt, by=c("campaign_name", "region", "rcode", "province", "pcode", "district", "dcode"))
              
              colnames(all_hh_data_wide) <- gsub("f_", "f", colnames(all_hh_data_wide))
              
              day1 <- export_admin_h2h_0_4_day1 %>% mutate(campaign_day = 1)
              day2 <- export_admin_h2h_0_4_day2 %>% mutate(campaign_day = 2)
              day3 <- export_admin_h2h_0_4_day3 %>% mutate(campaign_day = 3)
              day4 <- export_admin_h2h_0_4_day4 %>% mutate(campaign_day = 4) %>% select(-contains("_day1_3"))
              
              colnames(day1) <- gsub("_day1$", "", colnames(day1))
              colnames(day2) <- gsub("_day2$", "", colnames(day2))
              colnames(day3) <- gsub("_day3$", "", colnames(day3))
              colnames(day4) <- gsub("_day4$", "", colnames(day4))
              
              all_hh_data_long <- day1 %>%
                bind_rows(day2) %>%
                bind_rows(day3) %>%
                bind_rows(day4) %>%
                select(campaign_name, region, rcode, province, pcode, district, dcode, campaign_day, everything()) %>%
                arrange(campaign_name, region, rcode, province, pcode, district, dcode, campaign_day)
              
              colnames(all_hh_data_long) <- gsub("f_", "f", colnames(all_hh_data_long))
              
              workbook <- openxlsx::loadWorkbook(file.path("data/Export_Templates/DC_Daily_Compilation_H2H_0_4_District.xlsx"))
              writeData(workbook, sheet = "Day 1", x = export_admin_h2h_0_4_day1, startRow = 5, startCol = 1, colNames = FALSE)
              writeData(workbook, sheet = "Day 2", x = export_admin_h2h_0_4_day2, startRow = 5, startCol = 1, colNames = FALSE)
              writeData(workbook, sheet = "Day 3", x = export_admin_h2h_0_4_day3, startRow = 5, startCol = 1, colNames = FALSE)
              writeData(workbook, sheet = "Day 4", x = export_admin_h2h_0_4_day4, startRow = 5, startCol = 1, colNames = FALSE)
              writeData(workbook, sheet = "Grand Total", x = export_admin_h2h_0_4_gt, startRow = 5, startCol = 1, colNames = FALSE)
              # writeData(workbook, sheet = "Missed Child Conversion Summary", x = data$export_conversion_district, startRow = 3, startCol = 1, colNames = FALSE)
              openxlsx::addWorksheet(wb=workbook, sheetName="Machine_Readable_Wide")
              writeData(workbook, sheet = "Machine_Readable_Wide", x = all_hh_data_wide, startRow = 1, startCol = 1, colNames = TRUE)
              openxlsx::addWorksheet(wb=workbook, sheetName="Machine_Readable_Long")
              writeData(workbook, sheet = "Machine_Readable_Long", x = all_hh_data_long, startRow = 1, startCol = 1, colNames = TRUE)
              # Create a percentage style
              percent_style <- createStyle(numFmt = "0%")
              
              openxlsx::saveWorkbook(workbook, file.path(temp_dir, paste0("DC_Daily_Compilation_H2H_0_59_months_4_days_district_level.xlsx")), overwrite=TRUE)
            }
            
            if(nrow(data$export_admin_s2s_m2m) >= 1){
              export_admin_s2s_m2m <- data$export_admin_s2s_m2m %>%
                ungroup() %>%
                select(-c("clustername", "ccode")) %>%
                group_by(campaign_name, region, rcode, province, pcode, district, dcode) %>%
                summarise_all(~ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))) %>%
                ungroup() %>%
                rowwise() %>%
                mutate_at(c("total_sites_visited", "no_site_visited_day1", "no_site_visited_day2", "no_site_visited_day3"), ~ifelse(.==0,NA, .))
              
              day1 <- export_admin_s2s_m2m %>%
                select(campaign_name, region, rcode, province, pcode, district, dcode, everything()) %>%
                select(-contains("_day2")) %>%
                select(-contains("_day3")) %>%
                select(-contains("_days13")) %>%
                select(-c("total_sites_visited")) %>%
                mutate(campaign_day = 1)
              
              day2 <- export_admin_s2s_m2m %>%
                select(campaign_name, region, rcode, province, pcode, district, dcode, everything()) %>%
                select(-contains("_day1")) %>%
                select(-contains("_day3")) %>%
                select(-contains("_days13")) %>%
                select(-c("total_sites_visited")) %>%
                mutate(campaign_day = 2)
              
              day3 <- export_admin_s2s_m2m %>%
                select(campaign_name, region, rcode, province, pcode, district, dcode, everything()) %>%
                select(-contains("_day1")) %>%
                select(-contains("_day2")) %>%
                select(-contains("_days13")) %>%
                select(-c("total_sites_visited")) %>%
                mutate(campaign_day = 3)
              
              colnames(day1) <- gsub("_day1$", "", colnames(day1))
              colnames(day2) <- gsub("_day2$", "", colnames(day2))
              colnames(day3) <- gsub("_day3$", "", colnames(day3))
              
              all_s2s_long <- day1 %>%
                bind_rows(day2) %>%
                bind_rows(day3) %>%
                select(campaign_name, region, rcode, province, pcode, district, dcode, campaign_day, everything()) %>%
                arrange(campaign_name, region, rcode, province, pcode, district, dcode, campaign_day)
              
              colnames(all_s2s_long) <- gsub("f_", "f", colnames(all_s2s_long))
              
              workbook <- openxlsx::loadWorkbook(file.path("data/Export_Templates/DC_Daily_Compilation_S2S_M2M_0_4_District.xlsx"))
              writeData(workbook, sheet = "S2S_Data", x = export_admin_s2s_m2m, startRow = 5, startCol = 1, colNames = FALSE)
              openxlsx::addWorksheet(wb=workbook, sheetName="Machine_Readable_Wide")
              writeData(workbook, sheet = "Machine_Readable_Wide", x = export_admin_s2s_m2m, startRow = 1, startCol = 1, colNames = TRUE)
              openxlsx::addWorksheet(wb=workbook, sheetName="Machine_Readable_Long")
              writeData(workbook, sheet = "Machine_Readable_Long", x = all_s2s_long, startRow = 1, startCol = 1, colNames = TRUE)
              openxlsx::saveWorkbook(workbook, file.path(temp_dir, paste0("DC_Daily_Compilation_S2S_M2M_0_59_months_3_days_district_level.xlsx")), overwrite=TRUE)
            }
            
            if(nrow(data$export_admin_ipv_7day) >= 1){
              export_admin_ipv_7day <- data$export_admin_ipv_7day %>%
                ungroup() %>%
                select(-c("clustername", "ccode")) %>%
                group_by(campaign_name, region, rcode, province, pcode, district, dcode) %>%
                summarise_all(~ifelse(all(is.na(.)), NA, sum(., na.rm = TRUE))) %>%
                ungroup() 
              
              workbook <- openxlsx::createWorkbook()
              
              openxlsx::addWorksheet(wb=workbook, sheetName="Data")
              
              writeData(workbook, sheet = "Data", x = export_admin_ipv_7day, startRow = 1, startCol = 1, colNames = TRUE)
              openxlsx::saveWorkbook(workbook, file.path(temp_dir, paste0("DC_Daily_Compilation_fIPV_7_days_district_level.xlsx")), overwrite=TRUE)
            }
            
            if(nrow(data$export_conversion_district) >= 1){
              workbook <- openxlsx::loadWorkbook(file.path("data/Export_Templates/Missed_Child_Conversion_Summary_District.xlsx"))
              writeData(workbook, sheet = "Missed Child Conversion Summary", x = data$export_conversion_district, startRow = 3, startCol = 1, colNames = FALSE)
              # Create a percentage style
              percent_style <- createStyle(numFmt = "0%")
              
              addStyle(workbook, sheet = "Missed Child Conversion Summary", style = percent_style,
                       rows = 3:(nrow(data$export_conversion_cluster) + 2),
                       cols = c(11, 14, 17, 20, 23, 26, 29, 32, 35, 38, 42),
                       gridExpand = TRUE)
              
              openxlsx::saveWorkbook(workbook, file.path(temp_dir, paste0("Missed_Child_Conversion_Summary_district_level.xlsx")), overwrite=TRUE)
              
            }
            
            if(nrow(data$export_district_admin_coverage) >= 1){
              out <- data$export_district_admin_coverage %>%
                rename(`Campaign Name` = campaign_name,
                       `Region` = region,
                       RCODE = rcode,
                       Province = province,
                       PCODE = pcode,
                       District = district,
                       DCODE = dcode)
              workbook <- openxlsx::createWorkbook()
              openxlsx::addWorksheet(wb=workbook, sheetName="Sheet1")
              writeData(workbook, sheet = "Sheet1", x = out, startRow = 1, startCol = 1, colNames = TRUE)
              openxlsx::saveWorkbook(workbook, file.path(temp_dir, paste0("DC_Daily_Compilation_0_59_months_Coverage.xlsx")), overwrite=TRUE)
            }
            
            # Get list of all XLSX files in the temporary directory
            xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            
            # Create a temporary file for the ZIP archive
            zip_file <- tempfile(fileext = ".zip")
            
            # Zip the XLSX files without preserving folder structure
            utils::zip(zip_file, files = xlsx_files, flags = "-j")
            
            # Copy the zip file to the specified file location
            file.copy(zip_file, file)
            # unlink(temp_dir, recursive = TRUE, force = TRUE)
            
          }
        )
        # Log download
        # if (!is.null(logger)) logger$logUserEvent(
        #   page = "download_data",
        #   type = "download",
        #   value = "Admin - District Level",
        #   region = session_inputs$selected_region(),
        #   province = session_inputs$selected_province(),
        #   district = session_inputs$selected_district()
        # )
      }
    )
    
    output$download_post_district <- downloadHandler(
      filename = "Post_Campaign_Data_District_Level.zip",
      content = function(file) {
        withProgress(
          message = 'Preparing Download...',
          value = 0,
          {
            data <- download_campaign_filtered_sia_data()
            # Create a temporary directory to store CSV files
            temp_dir <- tempdir()
            existing_xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            file.remove(existing_xlsx_files)
            
            # if(nrow(data$export_ooh) >= 1 | nrow(data$export_lqas) >= 1 | nrow(data$export_pca) >= 1){
            workbook <- openxlsx::loadWorkbook(file.path("data/Export_Templates/Post_Campaign_District.xlsx"))
            writeData(workbook, sheet = "FM Survey (Out-of-House)", x = data$export_ooh, startRow = 3, startCol = 1, colNames = FALSE)
            writeData(workbook, sheet = "LQAS", x = data$export_lqas, startRow = 3, startCol = 1, colNames = FALSE)
            writeData(workbook, sheet = "PCA", x = data$export_pca, startRow = 3, startCol = 1, colNames = FALSE)
            openxlsx::saveWorkbook(workbook, file.path(temp_dir, paste0("Post_Campaign_0_59_months_district_level.xlsx")), overwrite=TRUE)
            
            workbook <- openxlsx::createWorkbook()
            openxlsx::addWorksheet(wb=workbook, sheetName="FM Survey (Out-of-House)")
            openxlsx::addWorksheet(wb=workbook, sheetName="LQAS")
            openxlsx::addWorksheet(wb=workbook, sheetName="PCA")
            writeData(workbook, sheet = "FM Survey (Out-of-House)", x = data$export_ooh, startRow = 1, startCol = 1, colNames = TRUE)
            writeData(workbook, sheet = "LQAS", x = data$export_lqas, startRow = 1, startCol = 1, colNames = TRUE)
            writeData(workbook, sheet = "PCA", x = data$export_pca, startRow = 1 ,startCol = 1, colNames = TRUE)
            openxlsx::saveWorkbook(workbook, file.path(temp_dir, paste0("Post_Campaign_0_59_months_district_level_Machine_Readable.xlsx")), overwrite=TRUE)
            # }
            
            # Get list of all XLSX files in the temporary directory
            xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            
            # Create a temporary file for the ZIP archive
            zip_file <- tempfile(fileext = ".zip")
            
            # Zip the XLSX files without preserving folder structure
            utils::zip(zip_file, files = xlsx_files, flags = "-j")
            
            # Copy the zip file to the specified file location
            file.copy(zip_file, file)
            
          }
        )
        # Log download
        # if (!is.null(logger)) logger$logUserEvent(
        #   page = "download_data",
        #   type = "download",
        #   value = "PostCampaign - District Level",
        #   region = session_inputs$selected_region(),
        #   province = session_inputs$selected_province(),
        #   district = session_inputs$selected_district()
        # )
      }
    )
    
    output$download_icm_district <- downloadHandler(
      filename = "Intra_Campaign_Monitoring_District_Level.zip",
      content = function(file) {
        withProgress(
          message = 'Preparing Download...',
          value = 0,
          {
            data <- download_campaign_filtered_sia_data()
            # Create a temporary directory to store CSV files
            temp_dir <- tempdir()
            existing_xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            file.remove(existing_xlsx_files)
            
            rename_columns <- function(df) {
              df %>%
                rename(
                  "Campaign Name" = campaign_name,
                  "Region" = region,
                  "RCODE" = rcode,
                  "Province" = province,
                  "PCODE" = pcode,
                  "District" = district,
                  "DCODE" = dcode
                )
            }
            
            # Apply the rename function to each dataset
            data$icm_household_summary_district <- rename_columns(data$icm_household_summary_district)
            data$icm_supervisor_monitoring_h2h_district <- rename_columns(data$icm_supervisor_monitoring_h2h_district)
            data$icm_team_monitoring_h2h_district <- rename_columns(data$icm_team_monitoring_h2h_district)
            data$icm_revisit_monitoring_h2h_district <- rename_columns(data$icm_revisit_monitoring_h2h_district)
            data$icm_team_monitoring_s2s_district <- rename_columns(data$icm_team_monitoring_s2s_district)
            data$icm_site_monitoring_s2s_district <- rename_columns(data$icm_site_monitoring_s2s_district)
            data$icm_session_monitoring_s2s_district <- rename_columns(data$icm_session_monitoring_s2s_district)
            
            # List of district-level dataset names
            district_targets <- c(
              "icm_household_summary_district",
              "icm_supervisor_monitoring_h2h_district",
              "icm_team_monitoring_h2h_district",
              "icm_revisit_monitoring_h2h_district",
              "icm_team_monitoring_s2s_district",
              "icm_site_monitoring_s2s_district",
              "icm_session_monitoring_s2s_cluster"
            )
            
            # Apply janitor::remove_empty to each one
            for (name in district_targets) {
              data[[name]] <- janitor::remove_empty(data[[name]], which = "cols")
            }
            
            wb <- createWorkbook()
            addWorksheet(wb, "IPV Session Monitoring")
            writeData(wb, sheet = "IPV Session Monitoring", x = data$icm_session_monitoring_s2s_district, startRow = 1, startCol = 1, colNames = TRUE)
            
            
            addWorksheet(wb, "Team Monitoring (S2S)")
            writeData(wb, sheet = "Team Monitoring (S2S)", x = data$icm_team_monitoring_s2s_district, startRow = 1, startCol = 1, colNames = TRUE)
            
            addWorksheet(wb, "Site Monitoring (S2S)")
            writeData(wb, sheet = "Site Monitoring (S2S)", x = data$icm_site_monitoring_s2s_district, startRow = 1, startCol = 1, colNames = TRUE)
            
            addWorksheet(wb, "Supervisor Monitoring")
            writeData(wb, sheet = "Supervisor Monitoring", x = data$icm_supervisor_monitoring_h2h_district, startRow = 1, startCol = 1, colNames = TRUE)
            
            addWorksheet(wb, "Household Monitoring (H2H)")
            writeData(wb, sheet = "Household Monitoring (H2H)", x = data$icm_household_summary_district, startRow = 1, startCol = 1, colNames = TRUE)
            
            addWorksheet(wb, "Team Monitoring (H2H)")
            writeData(wb, sheet = "Team Monitoring (H2H)", x = data$icm_team_monitoring_h2h_district, startRow = 1, startCol = 1, colNames = TRUE)
            
            addWorksheet(wb, "Monitoring for Revisit (H2H)")
            writeData(wb, sheet = "Monitoring for Revisit (H2H)", x = data$icm_revisit_monitoring_h2h_district, startRow = 1, startCol = 1, colNames = TRUE)
            
            openxlsx::saveWorkbook(wb, file.path(temp_dir, paste0("Intra_Campaign_Monitoring_0_59_months_district_level.xlsx")), overwrite=TRUE)
            
            # Get list of all XLSX files in the temporary directory
            xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            
            # Create a temporary file for the ZIP archive
            zip_file <- tempfile(fileext = ".zip")
            
            # Zip the XLSX files without preserving folder structure
            utils::zip(zip_file, files = xlsx_files, flags = "-j")
            
            # Copy the zip file to the specified file location
            file.copy(zip_file, file)
            
          }
        )
        # Log download
        # if (!is.null(logger)) logger$logUserEvent(
        #   page = "download_data",
        #   type = "download",
        #   value = "IntraCampaign - District Level",
        #   region = session_inputs$selected_region(),
        #   province = session_inputs$selected_province(),
        #   district = session_inputs$selected_district()
        # )
      }
    )
    
    output$download_icm_cluster <- downloadHandler(
      filename = "Intra_Campaign_Monitoring_Cluster_Level.zip",
      content = function(file) {
        withProgress(
          message = 'Preparing Download...',
          value = 0,
          {
            data <- download_campaign_filtered_sia_data()
            # Create a temporary directory to store CSV files
            temp_dir <- tempdir()
            existing_xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            file.remove(existing_xlsx_files)
            
            rename_columns <- function(df) {
              df %>%
                rename(
                  "Campaign Name" = campaign_name,
                  "Region" = region,
                  "RCODE" = rcode,
                  "Province" = province,
                  "PCODE" = pcode,
                  "District" = district,
                  "DCODE" = dcode,
                  "Cluster" = clustername,
                  "CCODE" = ccode
                )
            }
            
            # Apply the rename function to each dataset
            data$icm_household_summary_cluster <- rename_columns(data$icm_household_summary_cluster)
            data$icm_supervisor_monitoring_h2h_cluster <- rename_columns(data$icm_supervisor_monitoring_h2h_cluster)
            data$icm_team_monitoring_h2h_cluster <- rename_columns(data$icm_team_monitoring_h2h_cluster)
            data$icm_revisit_monitoring_h2h_cluster <- rename_columns(data$icm_revisit_monitoring_h2h_cluster)
            data$icm_team_monitoring_s2s_cluster <- rename_columns(data$icm_team_monitoring_s2s_cluster)
            data$icm_site_monitoring_s2s_cluster <- rename_columns(data$icm_site_monitoring_s2s_cluster)
            data$icm_session_monitoring_s2s_cluster <- rename_columns(data$icm_session_monitoring_s2s_cluster)
            
            # Vector of element names to clean
            targets <- c(
              "icm_household_summary_cluster",
              "icm_supervisor_monitoring_h2h_cluster",
              "icm_team_monitoring_h2h_cluster",
              "icm_revisit_monitoring_h2h_cluster",
              "icm_team_monitoring_s2s_cluster",
              "icm_site_monitoring_s2s_cluster", 
              "icm_session_monitoring_s2s_cluster"
            )
            
            # Apply janitor::remove_empty to each one
            for (name in targets) {
              data[[name]] <- janitor::remove_empty(data[[name]], which = "cols")
            }
            
            wb <- createWorkbook()
            addWorksheet(wb, "IPV Session Monitoring")
            writeData(wb, sheet = "IPV Session Monitoring", x = data$icm_session_monitoring_s2s_cluster, startRow = 1, startCol = 1, colNames = TRUE)
            
            addWorksheet(wb, "Supervisor Monitoring")
            writeData(wb, sheet = "Supervisor Monitoring", x = data$icm_supervisor_monitoring_h2h_cluster, startRow = 1, startCol = 1, colNames = TRUE)
            
            addWorksheet(wb, "Team Monitoring (S2S)")
            writeData(wb, sheet = "Team Monitoring (S2S)", x = data$icm_team_monitoring_s2s_cluster, startRow = 1, startCol = 1, colNames = TRUE)
            
            addWorksheet(wb, "Site Monitoring (S2S)")
            writeData(wb, sheet = "Site Monitoring (S2S)", x = data$icm_site_monitoring_s2s_cluster, startRow = 1, startCol = 1, colNames = TRUE)
            
            addWorksheet(wb, "Household Monitoring (H2H)")
            writeData(wb, sheet = "Household Monitoring (H2H)", x = data$icm_household_summary_cluster, startRow = 1, startCol = 1, colNames = TRUE)
            
            addWorksheet(wb, "Team Monitoring (H2H)")
            writeData(wb, sheet = "Team Monitoring (H2H)", x = data$icm_team_monitoring_h2h_cluster, startRow = 1, startCol = 1, colNames = TRUE)
            
            addWorksheet(wb, "Monitoring for Revisit (H2H)")
            writeData(wb, sheet = "Monitoring for Revisit (H2H)", x = data$icm_revisit_monitoring_h2h_cluster, startRow = 1, startCol = 1, colNames = TRUE)
            
            openxlsx::saveWorkbook(wb, file.path(temp_dir, paste0("Intra_Campaign_Monitoring_0_59_months_cluster_level.xlsx")), overwrite=TRUE)
            
            # Get list of all XLSX files in the temporary directory
            xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            
            # Create a temporary file for the ZIP archive
            zip_file <- tempfile(fileext = ".zip")
            
            # Zip the XLSX files without preserving folder structure
            utils::zip(zip_file, files = xlsx_files, flags = "-j")
            
            # Copy the zip file to the specified file location
            file.copy(zip_file, file)
            # unlink(temp_dir, recursive = TRUE, force = TRUE)
            
          }
        )
        # Log download
        # if (!is.null(logger)) logger$logUserEvent(
        #   page = "download_data",
        #   type = "download",
        #   value = "IntraCampaign - Cluster Level",
        #   region = session_inputs$selected_region(),
        #   province = session_inputs$selected_province(),
        #   district = session_inputs$selected_district()
        # )
      }
    )
    
    download_filtered_rpdc <- reactive({
      req(debounced_download_campaign_list())
      req(download_region())
      req(download_province())
      req(download_district())
      
      data <- campaign_rpdc %>%
        filter(campaign_name %in% debounced_download_campaign_list())
      
      if(download_region() != "All"){
        if("All" %in% download_region()){
          data <- data
        } else{
          if("All" %in% download_province()){
            data <- data %>%
              filter(region_name %in% download_region())
          } else{
            if("All" %in% download_district()){
              data <- data %>%
                filter(region_name %in% download_region() &
                         province_name %in% download_province())
            } else{
              data <- data %>%
                filter(region_name %in% download_region() &
                         province_name %in% download_province() &
                         district_name %in% download_district())
            }
          }
        }
      }
      
      data <- data %>% 
        select(campaign_name, rcode, region_name, pcode, province_name, dcode, district_name, ccode, cluster_name) %>%
        left_join(df_campaigns %>%
                    select(campaign_name, campaign_startdate) %>%
                    unique(),
                  by=c("campaign_name")) %>%
        left_join(shp_clusters %>%
                    sf::st_drop_geometry() %>%
                    mutate(cluster_polygon_available = "Yes") %>%
                    select(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE, APMIS_CCODE, cluster_polygon_available, start_date, end_date),
                  by=c("rcode" = "APMIS_RCODE", 
                       "pcode" = "APMIS_PCODE",
                       "dcode" = "APMIS_DCODE",
                       "ccode" = "APMIS_CCODE")) %>%
        mutate(cluster_polygon_available = case_when(campaign_startdate >= start_date &
                                                       campaign_startdate <= end_date &
                                                       cluster_polygon_available == "Yes" ~"Yes",
                                                     TRUE ~ "No")) %>%
        select(-c("start_date", "end_date")) %>%
        rename(`Campaign Name` = campaign_name,
               `Campaign Start Date` = campaign_startdate,
               `RCode` = rcode,
               `Region` = region_name,
               `PCode` = pcode,
               `Province` = province_name,
               `DCode` = dcode,
               `District` = district_name,
               `CCode` = ccode,
               `Cluster` = cluster_name,
               `Cluster Boundaries Mapped` = cluster_polygon_available) %>%
        select(`Campaign Name`, `Campaign Start Date`, RCode, Region, PCode, Province, DCode, District, CCode, Cluster, `Cluster Boundaries Mapped`)
      
      region <- data %>%
        mutate(mapped = ifelse(`Cluster Boundaries Mapped` == "Yes", 1, 0)) %>%
        group_by(`Campaign Name`, `Campaign Start Date`, RCode, Region) %>%
        summarise(`Total Clusters` = n()) %>%
        # ,
        #           `Clusters with Boundaries Mapped` = sum(mapped, na.rm=T)) %>%
        ungroup() 
      # %>%
      # rowwise() %>%
      # mutate(`Percent of Clusters with Boundaries Mapped` = round(`Clusters with Boundaries Mapped` /`Total Clusters`,2)) %>%
      # ungroup()
      
      province <- data %>%
        mutate(mapped = ifelse(`Cluster Boundaries Mapped` == "Yes", 1, 0)) %>%
        group_by(`Campaign Name`, `Campaign Start Date`, RCode, Region, PCode, Province) %>%
        summarise(`Total Clusters` = n()) %>%
        # ,
        #           `Clusters with Boundaries Mapped` = sum(mapped, na.rm=T)) %>%
        ungroup() 
      # %>%
      #   rowwise() %>%
      #   mutate(`Percent of Clusters with Boundaries Mapped` = round(`Clusters with Boundaries Mapped` /`Total Clusters`,2)) %>%
      #   ungroup()
      
      district <- data %>%
        mutate(mapped = ifelse(`Cluster Boundaries Mapped` == "Yes", 1, 0)) %>%
        group_by(`Campaign Name`, `Campaign Start Date`, RCode, Region, PCode, Province, DCode, District) %>%
        summarise(`Total Clusters` = n()) %>%
        # ,
        #           `Clusters with Boundaries Mapped` = sum(mapped, na.rm=T)) %>%
        ungroup() 
      # %>%
      #   rowwise() %>%
      #   mutate(`Percent of Clusters with Boundaries Mapped` = round(`Clusters with Boundaries Mapped` /`Total Clusters`,2)) %>%
      #   ungroup()
      
      
      out <- list(cluster = data,
                  district = district,
                  province = province,
                  region = region)
      
      return(out)
    })
    
    download_filtered_rpdc <- reactive({
      req(debounced_download_campaign_list())
      req(download_region())
      req(download_province())
      req(download_district())
      
      data <- campaign_rpdc %>%
        filter(campaign_name %in% debounced_download_campaign_list())
      
      if(download_region() != "All"){
        if("All" %in% download_region()){
          data <- data
        } else{
          if("All" %in% download_province()){
            data <- data %>%
              filter(region_name %in% download_region())
          } else{
            if("All" %in% download_district()){
              data <- data %>%
                filter(region_name %in% download_region() &
                         province_name %in% download_province())
            } else{
              data <- data %>%
                filter(region_name %in% download_region() &
                         province_name %in% download_province() &
                         district_name %in% download_district())
            }
          }
        }
      }
      
      data <- data %>% 
        select(campaign_name, rcode, region_name, pcode, province_name, dcode, district_name, ccode, cluster_name) %>%
        left_join(df_campaigns %>%
                    select(campaign_name, campaign_startdate) %>%
                    unique(),
                  by=c("campaign_name")) %>%
        left_join(shp_clusters %>%
                    sf::st_drop_geometry() %>%
                    mutate(cluster_polygon_available = "Yes") %>%
                    select(APMIS_RCODE, APMIS_PCODE, APMIS_DCODE, APMIS_CCODE, cluster_polygon_available, start_date, end_date),
                  by=c("rcode" = "APMIS_RCODE", 
                       "pcode" = "APMIS_PCODE",
                       "dcode" = "APMIS_DCODE",
                       "ccode" = "APMIS_CCODE")) %>%
        mutate(cluster_polygon_available = case_when(campaign_startdate >= start_date &
                                                       campaign_startdate <= end_date &
                                                       cluster_polygon_available == "Yes" ~"Yes",
                                                     TRUE ~ "No")) %>%
        select(-c("start_date", "end_date")) %>%
        rename(`Campaign Name` = campaign_name,
               `Campaign Start Date` = campaign_startdate,
               `RCode` = rcode,
               `Region` = region_name,
               `PCode` = pcode,
               `Province` = province_name,
               `DCode` = dcode,
               `District` = district_name,
               `CCode` = ccode,
               `Cluster` = cluster_name,
               `Cluster Boundaries Mapped` = cluster_polygon_available) %>%
        select(`Campaign Name`, `Campaign Start Date`, RCode, Region, PCode, Province, DCode, District, CCode, Cluster, `Cluster Boundaries Mapped`)
      
      region <- data %>%
        mutate(mapped = ifelse(`Cluster Boundaries Mapped` == "Yes", 1, 0)) %>%
        group_by(`Campaign Name`, `Campaign Start Date`, RCode, Region) %>%
        summarise(`Total Clusters` = n()) %>%
        # ,
        #           `Clusters with Boundaries Mapped` = sum(mapped, na.rm=T)) %>%
        ungroup() 
      # %>%
      # rowwise() %>%
      # mutate(`Percent of Clusters with Boundaries Mapped` = round(`Clusters with Boundaries Mapped` /`Total Clusters`,2)) %>%
      # ungroup()
      
      province <- data %>%
        mutate(mapped = ifelse(`Cluster Boundaries Mapped` == "Yes", 1, 0)) %>%
        group_by(`Campaign Name`, `Campaign Start Date`, RCode, Region, PCode, Province) %>%
        summarise(`Total Clusters` = n()) %>%
        # ,
        #           `Clusters with Boundaries Mapped` = sum(mapped, na.rm=T)) %>%
        ungroup() 
      # %>%
      #   rowwise() %>%
      #   mutate(`Percent of Clusters with Boundaries Mapped` = round(`Clusters with Boundaries Mapped` /`Total Clusters`,2)) %>%
      #   ungroup()
      
      district <- data %>%
        mutate(mapped = ifelse(`Cluster Boundaries Mapped` == "Yes", 1, 0)) %>%
        group_by(`Campaign Name`, `Campaign Start Date`, RCode, Region, PCode, Province, DCode, District) %>%
        summarise(`Total Clusters` = n()) %>%
        # ,
        #           `Clusters with Boundaries Mapped` = sum(mapped, na.rm=T)) %>%
        ungroup() 
      # %>%
      #   rowwise() %>%
      #   mutate(`Percent of Clusters with Boundaries Mapped` = round(`Clusters with Boundaries Mapped` /`Total Clusters`,2)) %>%
      #   ungroup()
      
      
      out <- list(cluster = data,
                  district = district,
                  province = province,
                  region = region)
      
      return(out)
    })
    
    output$download_campaign_rpdc <- downloadHandler(
      filename = "APMIS_Active_Geographies_List.zip",
      content = function(file) {
        withProgress(
          message = 'Preparing Download...',
          value = 0,
          {
            data <- download_filtered_rpdc()
            # Create a temporary directory to store CSV files
            temp_dir <- tempdir()
            existing_xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            file.remove(existing_xlsx_files)
            
            wb <- createWorkbook()
            addWorksheet(wb, "Regions")
            writeData(wb, sheet = "Regions", x = data$region, startRow = 1, startCol = 1, colNames = TRUE)
            addWorksheet(wb, "Provinces")
            writeData(wb, sheet = "Provinces", x = data$province, startRow = 1, startCol = 1, colNames = TRUE)
            addWorksheet(wb, "Districts")
            writeData(wb, sheet = "Districts", x = data$district, startRow = 1, startCol = 1, colNames = TRUE)
            addWorksheet(wb, "Clusters")
            writeData(wb, sheet = "Clusters", x = data$cluster, startRow = 1, startCol = 1, colNames = TRUE)
            
            # pct_style <- createStyle(numFmt = "0%")
            # addStyle(wb, sheet = "Regions", style = pct_style,
            #          rows = 2:(nrow(data$region)+1), cols = 7, gridExpand = TRUE)
            # addStyle(wb, sheet = "Provinces", style = pct_style,
            #          rows = 2:(nrow(data$province)+1), cols = 9, gridExpand = TRUE)
            # addStyle(wb, sheet = "Districts", style = pct_style,
            #          rows = 2:(nrow(data$district)+1), cols = 11, gridExpand = TRUE)
            
            openxlsx::saveWorkbook(wb, file.path(temp_dir, paste0("APMIS_Region_Province_District_Cluster_Lists.xlsx")), overwrite=TRUE)
            
            # Get list of all XLSX files in the temporary directory
            xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            
            # Create a temporary file for the ZIP archive
            zip_file <- tempfile(fileext = ".zip")
            
            # Zip the XLSX files without preserving folder structure
            utils::zip(zip_file, files = xlsx_files, flags = "-j")
            
            # Copy the zip file to the specified file location
            file.copy(zip_file, file)
            
          }
        )
        # Log download
        # if (!is.null(logger)) logger$logUserEvent(
        #   page = "download_data",
        #   type = "download",
        #   value = "Active Geographies",
        #   region = session_inputs$selected_region(),
        #   province = session_inputs$selected_province(),
        #   district = session_inputs$selected_district()
        # )
      }
    )
    
    output$download_precampaign_training_district <- downloadHandler(
      filename = "Precampaign_Training_Data_District_Aggregated.zip",
      content = function(file) {
        withProgress(
          message = 'Preparing Download...',
          value = 0,
          {
            data <- download_campaign_filtered_sia_data()$export_pre_campaign_training
            # Create a temporary directory to store CSV files
            temp_dir <- tempdir()
            existing_xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            file.remove(existing_xlsx_files)
            
            wb <- createWorkbook()
            addWorksheet(wb, "PreCampaign Training")
            writeData(wb, sheet = "PreCampaign Training", x = data, startRow = 1, startCol = 1, colNames = TRUE)
            
            pct_style <- createStyle(numFmt = "0%")
            addStyle(wb, sheet = "PreCampaign Training", style = pct_style,
                     rows = 2:(nrow(data)+1), cols = 21:32, gridExpand = TRUE)
            
            openxlsx::saveWorkbook(wb, file.path(temp_dir, paste0("Precampaign_Training_Data_District_Aggregated.xlsx")), overwrite=TRUE)
            
            # Get list of all XLSX files in the temporary directory
            xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            
            # Create a temporary file for the ZIP archive
            zip_file <- tempfile(fileext = ".zip")
            
            # Zip the XLSX files without preserving folder structure
            utils::zip(zip_file, files = xlsx_files, flags = "-j")
            
            # Copy the zip file to the specified file location
            file.copy(zip_file, file)
            
          }
        )
        # Log download
        # if (!is.null(logger)) logger$logUserEvent(
        #   page = "download_data",
        #   type = "download",
        #   value = "PreCampaign - District Level",
        #   region = session_inputs$selected_region(),
        #   province = session_inputs$selected_province(),
        #   district = session_inputs$selected_district()
        # )
      }
    )
    
    output$download_completeness <- downloadHandler(
      filename = "APMIS_Data_Completeness_Summary.zip",
      content = function(file) {
        withProgress(
          message = 'Preparing Download...',
          value = 0,
          {
            
            # Collect data frames into a list
            df_list <- list(
              pre_district = download_campaign_filtered_sia_data()$pre_district,
              admin_cluster = download_campaign_filtered_sia_data()$admin_cluster,
              icm_cluster = download_campaign_filtered_sia_data()$icm_cluster,
              post_cluster = download_campaign_filtered_sia_data()$post_cluster,
              post_district = download_campaign_filtered_sia_data()$post_district
            )
            
            # Apply renaming safely
            df_list <- lapply(
              df_list,
              \(df) rename(df, any_of(c(
                Campaign = "campaign_name",
                Region = "region",
                Province = "province",
                District = "district",
                Cluster = "clustername",
                RCODE = "rcode",
                PCODE = "pcode",
                DCODE = "dcode",
                CCODE = "ccode"
              )))
            )
            
            df_list <- lapply(
              df_list,
              \(df) df[, colSums(df != 0, na.rm = TRUE) > 0 | !sapply(df, is.numeric), drop = FALSE]
            )
            
            # Create a temporary directory to store CSV files
            temp_dir <- tempdir()
            existing_xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            file.remove(existing_xlsx_files)
            
            wb <- createWorkbook()
            
            # --- Guide sheet content ---
            guide_data <- data.frame(
              Sheet = c(
                "PreCampaign Records by District",
                "Admin Records by Cluster",
                "ICM Records by Cluster",
                "PCM Records by Cluster",
                "PCM Records by District"
              ),
              Description = c(
                "Number of pre-campaign training monitoring forms submitted per district.",
                "Number of 'DC Daily Compilation' forms submitted with complete data per campaign day, by cluster.",
                "Number of intra-campaign monitoring (ICM) checklist forms submitted per cluster.",
                "Number of post-campaign monitoring (PCM) forms submitted, verified, and published per cluster. (LQAS and OOH-FMS are reported at district level)",
                "Number of post-campaign monitoring (PCM) forms submitted, verified and published per district."
              ),
              stringsAsFactors = FALSE
            )
            
            # Add the guide sheet
            addWorksheet(wb, "Guide")
            writeData(wb, "Guide", guide_data, startRow = 1, startCol = 1, colNames = TRUE)
            
            
            addWorksheet(wb, "PreCampaign Records by District")
            writeData(wb, sheet = "PreCampaign Records by District", x = df_list$pre_district, startRow = 1, startCol = 1, colNames = TRUE)
            
            addWorksheet(wb, "Admin Records by Cluster")
            writeData(wb, sheet = "Admin Records by Cluster", x = df_list$admin_cluster, startRow = 1, startCol = 1, colNames = TRUE)
            
            addWorksheet(wb, "ICM Records by Cluster")
            writeData(wb, sheet = "ICM Records by Cluster", x = df_list$icm_cluster, startRow = 1, startCol = 1, colNames = TRUE)
            
            addWorksheet(wb, "PCM Records by Cluster")
            writeData(wb, sheet = "PCM Records by Cluster", x = df_list$post_cluster, startRow = 1, startCol = 1, colNames = TRUE)
            
            addWorksheet(wb, "PCM Records by District")
            writeData(wb, sheet = "PCM Records by District", x = df_list$post_district, startRow = 1, startCol = 1, colNames = TRUE)
            
            openxlsx::saveWorkbook(wb, file.path(temp_dir, paste0("APMIS_Data_Completeness_Summary.xlsx")), overwrite=TRUE)
            
            # Get list of all XLSX files in the temporary directory
            xlsx_files <- list.files(temp_dir, pattern = "\\.xlsx$", full.names = TRUE)
            
            # Create a temporary file for the ZIP archive
            zip_file <- tempfile(fileext = ".zip")
            
            # Zip the XLSX files without preserving folder structure
            utils::zip(zip_file, files = xlsx_files, flags = "-j")
            
            # Copy the zip file to the specified file location
            file.copy(zip_file, file)
            
          }
        )
       
      }
    )

  })
}