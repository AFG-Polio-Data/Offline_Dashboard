icm_reportModuleUI <- function(id) {
  ns <- NS(id)
  downloadButton(ns("download_icm_report"), "Download Report", class = "custom-download-btn")
}

icm_reportModuleServer <- function(id, report_data, campaign_name, region, province, district) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    triggered <- reactiveVal(NULL)
    
    output$download_icm_report <- downloadHandler(
      filename = function() {
        paste0("APMIS_IntraCampaign_Monitoring_Report_", campaign_name(), "_", Sys.Date(), ".pptx")
      },
      content = function(file) {
        showNotification("Generating report...", type = "message", duration = NULL, closeButton = FALSE, id = "report_notice")
        
        icm_data <- report_data()$icm_data
        
        icm_pcts_cluster <- icm_data$icm_pcts_cluster
        icm_pcts_district <- icm_data$icm_pcts_district
        icm_pcts_province <- icm_data$icm_pcts_province
        icm_pcts_region <- icm_data$icm_pcts_region
        icm_pcts_national <- icm_data$icm_pcts_campaign
        
        icm_pcts_district_day <- icm_data$icm_pcts_day_district
        
        #Summarize total ICM visits
        
        total_visits_by_type <- icm_data$icm_pcts_district %>%
          filter(indicator != "Percent of Clusters with ICM Form Reported") %>%
          group_by(form_type, rcode, pcode, dcode) %>%
          summarize(denominator = max(denominator, na.rm=T)) %>%
          ungroup() %>%
          group_by(form_type) %>%
          summarise(total_visits = sum(denominator, na.rm=T))
        
        total_clusters_with_type <- icm_data$icm_pcts_district %>%
          filter(indicator == "Percent of Clusters with ICM Form Reported") %>%
          filter(form_type %in% c("Site Monitoring", "Supervisor Monitoring", "Team Monitoring", "IPV Session Monitoring")) %>%
          group_by(form_type, rcode, pcode, dcode) %>%
          summarize(denominator = max(denominator, na.rm=T),
                    numerator = max(numerator, na.rm=T)) %>%
          ungroup() %>%
          group_by(form_type) %>%
          summarise(total_clusters_reported = sum(numerator, na.rm=T),
                    total_clusters = sum(denominator, na.rm=T)) %>%
          rowwise() %>%
          mutate(pct_clusters_reported = total_clusters_reported / total_clusters) %>%
          ungroup()
        
        indicators <- icm_data$icm_pcts_district %>%
          filter(indicator != "Percent of Clusters with ICM Form Reported") %>%
          group_by(form_type, indicator) %>%
          summarize(denominator = sum(denominator, na.rm=T),
                    numerator = sum(numerator, na.rm=T)) %>%
          ungroup() %>%
          rowwise() %>%
          mutate(pct = numerator / denominator) %>%
          ungroup()
        
        forms_available <- unique(indicators$form_type)
        
        summary_table <- total_visits_by_type %>%
          left_join(total_clusters_with_type, by=c("form_type")) %>%
          mutate(form_type = factor(form_type, levels=c("Team Monitoring", "Site Monitoring", "IPV Session Monitoring", "Supervisor Monitoring")),
                 pct_clusters_reported = round(pct_clusters_reported, 2)*100) %>%
          select(form_type, total_visits, total_clusters_reported, total_clusters, pct_clusters_reported) %>%
          rename(`Monitoring Type` = form_type,
                 `Number of Monitoring Visits` = total_visits,
                 `Number of Clusters with\nat least one Visit` = total_clusters_reported,
                 `Total Clusters` = total_clusters,
                 `Percent of Clusters with\nat least one Visit` = pct_clusters_reported) %>%
          arrange(`Monitoring Type`) %>%
          flextable::flextable() %>%
          flextable::colformat_num(
            j = "Percent of Clusters with\nat least one Visit",
            digits = 0,
            suffix = "%"
          ) %>%
          flextable::width(j = 1:5, width = c(2, 1.5, 1.5, 1.5, 1.5)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(part = "header", bg = "#0D6938") %>%
          flextable::color(part = "header", color = "white") %>%
          flextable::fontsize(size = 12, part = "all")
          
        
        #Team Monitoring Summary
        #Sections:
          # Team Composition and Training (1:4)
        team_monitoring_tbl1 <- indicators %>%
          filter(form_type == "Team Monitoring") %>%
          filter(indicator %in% c(
            "1) Both team members are resident of same area (village)",
            "2) Both vaccinators trained before this campaign",
            "3) At least one team member is a Community Health Worker",
            "4) At least one team member is female"
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Team Composition and Training") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(3.4, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(i = ~`Percent 'Yes'` < 90, j = "Percent 'Yes'", bg = "#F28E8E", part = "body")  %>%
          flextable::fontsize(size = 8, part = "all") # red for <90%
            
          
          # Itinerary, Planning and Field Execution (5:9)
        team_monitoring_tbl2 <- indicators %>%
          filter(form_type == "Team Monitoring") %>%
          filter(indicator %in% c(
            "5) Team has a clear itinerary/map",
            "6) Team itinerary includes the number and type of HRMP",
            "7) Microplan lists village elders/enablers who support vaccination efforts",
            "8) Team has a plan for daily revisit",
            "9) Team members know their daily target"
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Itinerary, Planning and Field Execution") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(3.4, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(i = ~`Percent 'Yes'` < 90, j = "Percent 'Yes'", bg = "#F28E8E", part = "body")  %>%
          flextable::fontsize(size = 8, part = "all") # red for <90%
          
        # Vaccine Handling (10:13)
        
        team_monitoring_tbl3 <- indicators %>%
          filter(form_type == "Team Monitoring") %>%
          filter(indicator %in% c(
            "10) Team has vaccine carrier with frozen ice-packs",
            "11) OPV vials are kept cool and dry",
            "12) Based on observation of VVM, vaccine is usable",
            "13) Number of children vaccinated matches with vials used"
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Vaccine Handling") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(3.4, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(i = ~`Percent 'Yes'` < 90, j = "Percent 'Yes'", bg = "#F28E8E", part = "body")  %>%
          flextable::fontsize(size = 8, part = "all") # red for <90%
        
          # Child Screening and Vaccination (14:21)
        team_monitoring_tbl4 <- indicators %>%
          filter(form_type == "Team Monitoring") %>%
          filter(indicator %in% c(
            "14) Team is asking caregivers for details of all children",
            "15) Team is specifically asking about children aged 0-11 months",
            "16) Team is properly recording children vaccinated", 
            "17) Team is recording all missed children with reasons and using the missed children registration form (if applicable)", 
            "18) In areas using the child registration book, team is cross-verifying information with the register (if applicable)", 
            "19) Team is properly finger-marking after administering OPV", 
            "20) Team is asking about AFP cases", 
            "21) Team is giving key vaccination messages to the families after vaccination" 
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Child Screening and Vaccination") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(3.1, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(i = ~`Percent 'Yes'` < 90, j = "Percent 'Yes'", bg = "#F28E8E", part = "body")  %>%
          flextable::fontsize(size = 8, part = "all") # red for <90%
        
          # Supervision (22)
        team_monitoring_tbl5 <- indicators %>%
          filter(form_type == "Team Monitoring") %>%
          filter(indicator %in% c(
            "22) Team has been visited by a supervisor in the field and tally sheet is signed" 
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Supervision") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(3.1, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(i = ~`Percent 'Yes'` < 90, j = "Percent 'Yes'", bg = "#F28E8E", part = "body")  %>%
          flextable::fontsize(size = 8, part = "all") # red for <90%
          
        # Social Mobilization (23:27)
        team_monitoring_tbl6 <- indicators %>%
          filter(form_type == "Team Monitoring") %>%
          filter(indicator %in% c(
            "23) Social Mobilizer accompanies the vaccination team in the field",
            "24) Social Mobilizer is carrying and updating the child registration booklet",
            "25) Social Mobilizer supports bringing children to sites",
            "26) Social Mobilizer has hand held megaphone",
            "27) Social Mobilizer is assisting with community announcements" 
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Social Mobilization") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(3.2, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(i = ~`Percent 'Yes'` < 90, j = "Percent 'Yes'", bg = "#F28E8E", part = "body")  %>%
          flextable::fontsize(size = 8, part = "all") # red for <90%  
        
        site_monitoring_tbl1 <- indicators %>%
          filter(form_type == "Site Monitoring") %>%
          filter(indicator %in% c(
            "1) Vaccination site is set up as per micro plan",
            "2) Number of houses per site is appropriate (i.e. 5 or fewer)",
            "3) Site selection is suitable and acceptable for the community",                                            
            "4) Site session well-organized",                                                                            
            "5) Adequate time spent for vaccination of target children in site",                                         
            "6) Team members present as per microplan"
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Site Setup and Team Attendance") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(4.5, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(i = ~`Percent 'Yes'` < 90, j = "Percent 'Yes'", bg = "#F28E8E", part = "body")  %>%
          flextable::fontsize(size = 8, part = "all") # red for <90%
        
        site_monitoring_tbl2 <- indicators %>%
          filter(form_type == "Site Monitoring") %>%
          filter(indicator %in% c(
            "7) Social Mobilizer assigned to the vaccination team",                                                      
            "8) Social Mobilizer is resident of area/locality",                                                          
            "9) Social Mobilizer coordinated with local elders or Wakil Guzar for vaccination session prior to campaign",
            "10a) Social Mobilizer is assisting via announcements on handheld megaphone",                                
            "10b) Social Mobilizer is assisting via mobilization of community elders",                                   
            "10c) Social Mobilizer is assisting via seeking out absent/missed children",                                 
            "10d) Social Mobilizer is assisting via community mobilization",
            "11) Social Mobilizer is using the child registration book to verify all children at the site"
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          mutate(Indicator = factor(Indicator, levels = c("7) Social Mobilizer assigned to the vaccination team",                                                      
                                                          "8) Social Mobilizer is resident of area/locality",                                                          
                                                          "9) Social Mobilizer coordinated with local elders or Wakil Guzar for vaccination session prior to campaign",
                                                          "10a) Social Mobilizer is assisting via announcements on handheld megaphone",                                
                                                          "10b) Social Mobilizer is assisting via mobilization of community elders",                                   
                                                          "10c) Social Mobilizer is assisting via seeking out absent/missed children",                                 
                                                          "10d) Social Mobilizer is assisting via community mobilization",
                                                          "11) Social Mobilizer is using the child registration book to verify all children at the site"))) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Social Mobilization Support") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(4.5, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(i = ~`Percent 'Yes'` < 90, j = "Percent 'Yes'", bg = "#F28E8E", part = "body")  %>%
          flextable::fontsize(size = 8, part = "all") # red for <90%
        
        site_monitoring_tbl3 <- indicators %>%
          filter(form_type == "Site Monitoring") %>%
          filter(indicator %in% c(
            "12) Enablers present at the site",                                                                          
            "13) Enablers assisting in providing information about missed children",                                     
            "14) Refusal families are in the visited site",                                                              
            "15) Vaccination team asking about the number of families and eligible children during the visit",           
            "16) Vaccination team recording missed children by reason in the missed children form",                      
            "17) Team is completing vaccination at the sites as per the microplan",                                      
            "18) Logistical issues were observed at the site",                                                           
            "19) Team is revisiting all sites to vaccinate missed children",                                            
            "2) Number of houses per site is appropriate (i.e. 5 or fewer)",                                             
            "20) Revisit timings take into account the availability of caregivers"
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Site Quality, Feedback, and Missed Children") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(4.5, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(
            i = ~`Percent 'Yes'` > 10 & 
              Indicator %in% c(
                "14) Refusal families are in the visited site", 
                "18) Logistical issues were observed at the site"
              ),
            j = "Percent 'Yes'",
            bg = "#F28E8E",
            part = "body"
          ) %>%
          flextable::bg(
            i = ~`Percent 'Yes'` < 90 & 
              !Indicator %in% c(
                "14) Refusal families are in the visited site", 
                "18) Logistical issues were observed at the site"
              ),
            j = "Percent 'Yes'",
            bg = "#F28E8E",
            part = "body"
          ) %>%
          flextable::fontsize(size = 8, part = "all")
        
        supervisor_monitoring_tbl1 <- indicators %>%
          filter(form_type == "Supervisor Monitoring") %>%
          filter(indicator %in% c(
            "1) Is supervisor resident of his assigned cluster?",
            "2) Is the supervisor literate as per the guidelines?",
            "3) Was the supervisor trained before the current campaign?"
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Residency and Training") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(4.5, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(
            i = ~`Percent 'Yes'` < 90,
            j = "Percent 'Yes'",
            bg = "#F28E8E",
            part = "body"
          ) %>%
          flextable::fontsize(size = 8, part = "all")
        
        supervisor_monitoring_tbl2 <- indicators %>%
          filter(form_type == "Supervisor Monitoring") %>%
          filter(indicator %in% c(
            "4) Is the supervisor carrying a copy of the microplan?", 
            "5) Are busy locations including transit points in the microplan?", 
            "6) Is there a monitoring plan for daily revisit?", 
            "7) Has the supervisor filled the the team and site monitoring checklist?",
            "8) Does the cluster itinerary include an updated list of HRMP?"
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Planning and Documentation") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(4.5, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(
            i = ~`Percent 'Yes'` < 90,
            j = "Percent 'Yes'",
            bg = "#F28E8E",
            part = "body"
          ) %>%
          flextable::fontsize(size = 8, part = "all")
        
        supervisor_monitoring_tbl3 <- indicators %>%
          filter(form_type == "Supervisor Monitoring") %>%
          filter(indicator %in% c(
            "9) Is the supervisor following the daily supervision plan?",
            "10) Is the supervisor ensuring proper tallying, recordkeeping, and taking corrective actions when needed?"
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          mutate(Indicator = factor(Indicator, levels =c("9) Is the supervisor following the daily supervision plan?",
                                                         "10) Is the supervisor ensuring proper tallying, recordkeeping, and taking corrective actions when needed?"
          ))) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Field Supervision Activities") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(4.5, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(
            i = ~`Percent 'Yes'` < 90,
            j = "Percent 'Yes'",
            bg = "#F28E8E",
            part = "body"
          ) %>%
          flextable::fontsize(size = 8, part = "all")
        
        supervisor_monitoring_tbl4 <- indicators %>%
          filter(form_type == "Supervisor Monitoring") %>%
          filter(indicator %in% c(
            "11) Is the supervisor carrying a vaccine carrier with sufficient OPV vaccines and Vitamin A or Albendazone (if applicable)?",
            "12) Is the supervisor carrying additional logistics?"
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Logistics and Supplies") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(4.5, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(
            i = ~`Percent 'Yes'` < 90,
            j = "Percent 'Yes'",
            bg = "#F28E8E",
            part = "body"
          ) %>%
          flextable::fontsize(size = 8, part = "all")
        
        supervisor_monitoring_tbl5 <- indicators %>%
          filter(form_type == "Supervisor Monitoring") %>%
          filter(indicator %in% c(
            "13) Has the supervisor conducted coordination meetings with community elders, influencers, and school/madrassa teachers before the campaign?",
            "14) Is the supervisor communicating field issues (refusals, gaps) to higher levels at the end of the day?"
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Communication and Community Support") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(4.5, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(
            i = ~`Percent 'Yes'` < 90,
            j = "Percent 'Yes'",
            bg = "#F28E8E",
            part = "body"
          ) %>%
          flextable::fontsize(size = 8, part = "all")
        
        session_monitoring_tbl1 <- indicators %>%
          filter(form_type == "IPV Session Monitoring") %>%
          filter(indicator %in% c(
            "1) Both vaccinators trained before this campaign",                                             
            "2) Vaccinators able to administer fIPV confidently",                                           
            "3) Vaccinators aware of open vial policy applicable on IPV",                                   
            "4) Vaccinators aware of correct age group for IPV vaccination",                                
            "5) Vaccinators recording OPV and IPV after each administration in the tally sheet",            
            "6) Vaccinators marking two separate fingers after IPV and OPV administration"     
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Vaccinator Knowledge, Attitude and Practice") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(3.4, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(
            i = ~`Percent 'Yes'` < 90,
            j = "Percent 'Yes'",
            bg = "#F28E8E",
            part = "body"
          ) %>%
          flextable::fontsize(size = 8, part = "all")
        
        session_monitoring_tbl2 <- indicators %>%
          filter(form_type == "IPV Session Monitoring") %>%
          filter(indicator %in% c(
            "7) Tropis ID injector checked for charge status each time before use",                         
            "8) Syringe correctly filled with the appropriate quantity and free of air bubbles",            
            "9) Injector pointed upward while syringe, vial and adaptor assembly is inserted into injector",
            "10) Entire assembly is inverted before detaching the needle",                                  
            "11) Correct pressure, injection angle, and injection site location are ensured",               
            "12) Injection site is dry  immediately after administration",                                  
            "13) Visible bleeding at the injection site after administration"      
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Injection Technique and Device Use") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(3.4, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(
            i = ~`Percent 'Yes'` < 90,
            j = "Percent 'Yes'",
            bg = "#F28E8E",
            part = "body"
          ) %>%
          flextable::fontsize(size = 8, part = "all")
        
        session_monitoring_tbl3 <- indicators %>%
          filter(form_type == "IPV Session Monitoring") %>%
          filter(indicator %in% c(
            "14) Any vaccine vials with VVM are in stage not optimal for use",                              
            "15) Icepacks are conditioned to avoid vaccine freezing",                                        
            "16) Session vaccine carrier properly closed and out of direct sunlight",                       
            "17) Adequate Tropis devices and syringes are available for the expected number of children"      
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Cold Chain and Vaccine Management") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(3.2, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(
            i = ~`Percent 'Yes'` < 90,
            j = "Percent 'Yes'",
            bg = "#F28E8E",
            part = "body"
          ) %>%
          flextable::fontsize(size = 8, part = "all")
        
        session_monitoring_tbl4 <- indicators %>%
          filter(form_type == "IPV Session Monitoring") %>%
          filter(indicator %in% c(
            "18) Vaccination team follows the planned microplan itinerary for area",                        
            "19) Team started work on time as per microplan",                                               
            "20) Sufficient tally sheets, pens, and forms are available with the team",                     
            "21) Children are screened for age eligibility before IPV administration",                      
            "22) Both IPV and OPV are administered to eligible children as per protocol"      
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Itinerary, Planning and Field Execution") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(3.2, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(
            i = ~`Percent 'Yes'` < 90,
            j = "Percent 'Yes'",
            bg = "#F28E8E",
            part = "body"
          ) %>%
          flextable::fontsize(size = 8, part = "all")
        
        session_monitoring_tbl5 <- indicators %>%
          filter(form_type == "IPV Session Monitoring") %>%
          filter(indicator %in% c(
            "23) Social mobilizer is present and actively supporting the vaccination team",                 
            "24) Caregivers aware of the fIPV campaign before the team's visit",                            
            "25) Local community is supporting mobilization activities",                                    
            "26) Masjid announcements are taking place"      
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Social Mobilization") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(3.2, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(
            i = ~`Percent 'Yes'` < 90,
            j = "Percent 'Yes'",
            bg = "#F28E8E",
            part = "body"
          ) %>%
          flextable::fontsize(size = 8, part = "all")
        
        session_monitoring_tbl6 <- indicators %>%
          filter(form_type == "IPV Session Monitoring") %>%
          filter(indicator %in% c(
            "27) Team is visited by supervisor in the field",                                               
            "28) Microplan includes daily target"      
          )) %>%
          select(indicator, pct) %>%
          mutate(pct = round(pct, 2) * 100) %>%
          rename(
            Indicator = indicator,
            `Percent 'Yes'` = pct
          ) %>%
          arrange(Indicator) %>%
          flextable::flextable() %>%
          flextable::add_header_lines("Supervision") %>%
          flextable::colformat_num(j = "Percent 'Yes'", digits = 0, suffix = "%") %>%
          flextable::width(j = 1:2, width = c(3.2, 1)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(i = 1, bg = "#0D6938", part = "header") %>%  # top title row (green)
          flextable::color(i = 1, color = "white", part = "header") %>%
          flextable::bg(i = 2, bg = "#FBCB9A", part = "header") %>%  # column names row (orange)
          flextable::color(i = 2, color = "black", part = "header") %>%
          flextable::bold(i = 1:2, bold = TRUE, part = "header") %>%
          flextable::bg(
            i = ~`Percent 'Yes'` < 90,
            j = "Percent 'Yes'",
            bg = "#F28E8E",
            part = "body"
          ) %>%
          flextable::fontsize(size = 8, part = "all")
        
        # summary_table
        # 
        # team_monitoring_tbl1
        # team_monitoring_tbl2
        # team_monitoring_tbl3
        # team_monitoring_tbl4
        # team_monitoring_tbl5
        # team_monitoring_tbl6
        # 
        # site_monitoring_tbl1
        # site_monitoring_tbl2
        # site_monitoring_tbl3
        # 
        # supervisor_monitoring_tbl1
        # supervisor_monitoring_tbl2
        # supervisor_monitoring_tbl3
        # supervisor_monitoring_tbl4
        # supervisor_monitoring_tbl5
        # 
        # session_monitoring_tbl1
        # session_monitoring_tbl2
        # session_monitoring_tbl3
        # session_monitoring_tbl4
        # session_monitoring_tbl5
        # session_monitoring_tbl6
        
        # Create Headers----- --------------------------------------------------
        
        slide1_title_text <- {
          loc <- if ("All" %in% region()) {
            NULL
          } else if ("All" %in% province()) {
            paste(region(), "Region")
          } else if ("All" %in% district()) {
            paste(province(), "Province")
          } else {
            paste(district(), "District")
          }
          
          if (is.null(loc)) {
            paste0(campaign_name(), ": Intra-Campaign Monitoring Summary")
          } else {
            paste0(campaign_name(), ", ", loc, ": Intra-Campaign Monitoring Summary")
          }
        }
        
        team_monitoring_title_text <- paste0("Team Monitoring: ", scales::comma(total_visits_by_type$total_visits[total_visits_by_type$form_type == "Team Monitoring"]), " Total Visits")
        site_monitoring_title_text <- paste0("Site Monitoring: ", scales::comma(total_visits_by_type$total_visits[total_visits_by_type$form_type == "Site Monitoring"]), " Total Visits")
        session_monitoring_title_text <- paste0("IPV Session Monitoring: ", scales::comma(total_visits_by_type$total_visits[total_visits_by_type$form_type == "IPV Session Monitoring"]), " Total Visits")
        supervisor_monitoring_title_text <- paste0("Supervisor Monitoring: ", scales::comma(total_visits_by_type$total_visits[total_visits_by_type$form_type == "Supervisor Monitoring"]), " Total Visits")
        
        slide1_bullets <- block_list(
  fpar(ftext("Intra-Campaign Monitoring for Site-to-Site SIAs is conducted by monitors who visit vaccination teams, sites, or cluster supervisors during SIAs and complete a checklist to identify gaps and provide constructive feedback. Completed checklists are compiled in APMIS.", 
             fp_text(font.size = 12))),
  fpar(),
  fpar(ftext("In this report, each checklist question is summarized as the percent of visits where the response was 'Yes', out of all valid responses.", 
             fp_text(font.size = 12))),
  fpar(),
  fpar(ftext("Results below 90% are highlighted in red, for easy identification of areas that could benefit from further investigation and improvement. The APMIS dashboard can be used for further investigation, where breakdowns of each question by district, cluster, and campaign day are available.", 
             fp_text(font.size = 12))),
  fpar(),
  fpar(ftext("The table below shows visit counts by type and the percent of clusters with at least one visit. Low numbers or percents may limit generalizability.", 
             fp_text(font.size = 12)))
)
        
        template <- officer::read_pptx(here::here("reports", "icm_report_slide_template.pptx"))
        
        my_pres <- template %>%
          add_slide(layout = "icm_intro_slide", master = "Office Theme") %>%
          ph_with(value = paste0("Data Source: APMIS, extracted on ", Sys.Date()),
                  location = ph_location_label(ph_label = "FooterBox")) %>%
          ph_with(value = slide1_title_text, location = ph_location_label(ph_label = "TitleBox")) %>%
          ph_with(value = slide1_bullets, location = ph_location_label(ph_label = "Text")) %>%
          ph_with(value = summary_table, location = ph_location_label(ph_label = "Table1"))
          
        if("Team Monitoring" %in% forms_available){
        my_pres <- my_pres %>%
          add_slide(layout = "icm_team_monitoring", master = "Office Theme") %>%
          ph_with(value = paste0("Data Source: APMIS, extracted on ", Sys.Date()),
                   location = ph_location_label(ph_label = "FooterBox")) %>%
          ph_with(value = team_monitoring_title_text, location = ph_location_label(ph_label = "TitleBox")) %>%
          ph_with(value = team_monitoring_tbl1, location = ph_location_label(ph_label = "Table1")) %>%
          ph_with(value = team_monitoring_tbl2, location = ph_location_label(ph_label = "Table2")) %>%
          ph_with(value = team_monitoring_tbl3, location = ph_location_label(ph_label = "Table3")) %>%
          ph_with(value = team_monitoring_tbl4, location = ph_location_label(ph_label = "Table4")) %>%
          ph_with(value = team_monitoring_tbl5, location = ph_location_label(ph_label = "Table5")) %>%
          ph_with(value = team_monitoring_tbl6, location = ph_location_label(ph_label = "Table6")) 
        }
        
        if("Site Monitoring" %in% forms_available){
        my_pres <- my_pres %>%
          add_slide(layout = "icm_site_monitoring", master = "Office Theme") %>%
          ph_with(value = paste0("Data Source: APMIS, extracted on ", Sys.Date()),
                  location = ph_location_label(ph_label = "FooterBox")) %>%
          ph_with(value = site_monitoring_title_text, location = ph_location_label(ph_label = "TitleBox")) %>%
          ph_with(value = site_monitoring_tbl1, location = ph_location_label(ph_label = "Table1")) %>%
          ph_with(value = site_monitoring_tbl2, location = ph_location_label(ph_label = "Table2")) %>%
          ph_with(value = site_monitoring_tbl3, location = ph_location_label(ph_label = "Table3"))
        }
        
        if("IPV Session Monitoring" %in% forms_available){
        my_pres <- my_pres %>%
          add_slide(layout = "icm_session_monitoring", master = "Office Theme") %>%
          ph_with(value = paste0("Data Source: APMIS, extracted on ", Sys.Date()),
                  location = ph_location_label(ph_label = "FooterBox")) %>%
          ph_with(value = session_monitoring_title_text, location = ph_location_label(ph_label = "TitleBox")) %>%
          ph_with(value = session_monitoring_tbl1, location = ph_location_label(ph_label = "Table1")) %>%
          ph_with(value = session_monitoring_tbl2, location = ph_location_label(ph_label = "Table2")) %>%
          ph_with(value = session_monitoring_tbl3, location = ph_location_label(ph_label = "Table3")) %>%
          ph_with(value = session_monitoring_tbl4, location = ph_location_label(ph_label = "Table4")) %>%
          ph_with(value = session_monitoring_tbl5, location = ph_location_label(ph_label = "Table5")) %>%
          ph_with(value = session_monitoring_tbl6, location = ph_location_label(ph_label = "Table6"))
        }
        
        if("Supervisor Monitoring" %in% forms_available){
        my_pres <- my_pres %>%
          add_slide(layout = "icm_supervisor_monitoring", master = "Office Theme") %>%
          ph_with(value = paste0("Data Source: APMIS, extracted on ", Sys.Date()),
                  location = ph_location_label(ph_label = "FooterBox")) %>%
          ph_with(value = supervisor_monitoring_title_text, location = ph_location_label(ph_label = "TitleBox")) %>%
          ph_with(value = supervisor_monitoring_tbl1, location = ph_location_label(ph_label = "Table1")) %>%
          ph_with(value = supervisor_monitoring_tbl2, location = ph_location_label(ph_label = "Table2")) %>%
          ph_with(value = supervisor_monitoring_tbl3, location = ph_location_label(ph_label = "Table3")) %>%
          ph_with(value = supervisor_monitoring_tbl4, location = ph_location_label(ph_label = "Table4")) %>%
          ph_with(value = supervisor_monitoring_tbl5, location = ph_location_label(ph_label = "Table5")) 
        }
        
        # At the very end:
        removeNotification("report_notice")
        print(my_pres, target = file)
        triggered(Sys.time())
      }
    )
    return(triggered)
  })
}