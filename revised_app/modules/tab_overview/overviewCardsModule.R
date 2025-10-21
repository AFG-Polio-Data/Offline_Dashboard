# modules/siaOverviewModule.R
library(shiny)
library(dplyr)

overviewCardsUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("sia_overview_cards"))
}

overviewCardsServer <- function(id, overview_filtered_sia_data, overview_filtered_admin_data, overview_filtered_admin_data_ipv, overview_camp_name) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$sia_overview_cards <- renderUI({
        req(overview_filtered_sia_data())
        req(overview_filtered_admin_data())
        req(overview_filtered_admin_data_ipv())
        req(overview_camp_name())
        
        if(grepl("IPV", overview_camp_name(), ignore.case = T)){
              pca_cov <- overview_filtered_sia_data()$district_indicators %>%
                filter(indicator == "pca_fm_coverage_0_59m") %>%
                ungroup() %>%
                summarise_at(c("numerator", "denominator"), ~sum(.,na.rm=T)) %>%
                rowwise() %>%
                mutate(pca_cov = paste0(round(100*(numerator / denominator), 0),"%"))
              
              if(is.na(pca_cov$numerator) | is.na(pca_cov$denominator) | pca_cov$denominator == 0){
                pca_cov <- "No Data"
              } else{pca_cov <-pca_cov$pca_cov[1]}
              
              pca_cov_ipv <- overview_filtered_sia_data()$district_indicators %>%
                filter(indicator == "pca_fm_coverage_ipv") %>%
                ungroup() %>%
                summarise_at(c("numerator", "denominator"), ~sum(.,na.rm=T)) %>%
                rowwise() %>%
                mutate(pca_cov_ipv = paste0(round(100*(numerator / denominator), 0),"%"))
              
              if(is.na(pca_cov_ipv$numerator) | is.na(pca_cov_ipv$denominator) | pca_cov_ipv$denominator == 0){
                pca_cov_ipv <- "No Data"
              } else{pca_cov_ipv <-pca_cov_ipv$pca_cov_ipv[1]}
              
              admin_cov <- overview_filtered_admin_data()$district %>%
                rowwise() %>%
                mutate(total_vaccinated = sum(total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, total_children_vaccinated_day5, total_children_vaccinated_day6, total_children_vaccinated_day7, na.rm=T)) %>%
                select(total_vaccinated, target_population) %>%
                ungroup() %>%
                summarise_all(~sum(., na.rm=T)) %>%
                rowwise() %>%
                mutate(pct = total_vaccinated / target_population) %>%
                mutate(admin_cov = paste0(round(100*(total_vaccinated / target_population), 0),"%"))
              
              if(is.na(admin_cov$total_vaccinated) | is.na(admin_cov$target_population) | admin_cov$target_population == 0){
                admin_cov <- "No Data"
              } else{admin_cov <-admin_cov$admin_cov[1]}
              
              admin_cov_ipv <- overview_filtered_admin_data_ipv()$district %>%
                rowwise() %>%
                mutate(total_vaccinated = sum(total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, total_children_vaccinated_day5, total_children_vaccinated_day6, total_children_vaccinated_day7, na.rm=T)) %>%
                select(total_vaccinated, target_population) %>%
                ungroup() %>%
                summarise_all(~sum(., na.rm=T)) %>%
                rowwise() %>%
                mutate(pct = total_vaccinated / target_population) %>%
                mutate(admin_cov_ipv = paste0(round(100*(total_vaccinated / target_population), 0),"%"))
              
              if(is.na(admin_cov_ipv$total_vaccinated) | is.na(admin_cov_ipv$target_population) | admin_cov_ipv$target_population == 0){
                admin_cov_ipv <- "No Data"
              } else{admin_cov_ipv <-admin_cov_ipv$admin_cov_ipv[1]}
              
              lqas_pct_lots_passed <- overview_filtered_sia_data()$district_indicators %>%
                filter(indicator == "lqas_result") %>%
                filter(value %in% c("One or More Lots Failed", "All Lots Passed")) %>%
                summarise(numerator = sum(numerator, na.rm=T),
                          total = sum(denominator, na.rm=T)) %>%
                mutate(pct_pass = paste0(round(numerator / total, 2)*100,"%")) 
              
              if(is.na(lqas_pct_lots_passed$numerator[1]) |
                 is.na(lqas_pct_lots_passed$total[1]) |
                 lqas_pct_lots_passed$total[1] == 0){
                lqas_pct_lots_passed <- "No Data"
              } else{lqas_pct_lots_passed <- lqas_pct_lots_passed$pct_pass[1]}
              
              lqas_pct_lots_passed_ipv <- overview_filtered_sia_data()$district_indicators %>%
                filter(indicator == "lqas_fipv_result") %>%
                filter(value %in% c("One or More Lots Failed", "All Lots Passed")) %>%
                summarise(numerator = sum(numerator, na.rm=T),
                          total = sum(denominator, na.rm=T)) %>%
                mutate(pct_pass = paste0(round(numerator / total, 2)*100,"%")) 
              
              if(is.na(lqas_pct_lots_passed_ipv$numerator[1]) |
                 is.na(lqas_pct_lots_passed_ipv$total[1]) |
                 lqas_pct_lots_passed_ipv$total[1] == 0){
                lqas_pct_lots_passed_ipv <- "No Data"
              } else{lqas_pct_lots_passed_ipv <- lqas_pct_lots_passed_ipv$pct_pass[1]}
              
              list(
                valueBox(
                  value = admin_cov,
                  subtitle = "OPV Cov. (Admin)",
                  width = 12/6,
                  color = "teal"
                ),
                valueBox(
                  value = admin_cov_ipv,
                  subtitle = "IPV Cov. (Admin)",
                  width = 12/6,
                  color = "teal"
                ),
                valueBox(
                  value = pca_cov,
                  subtitle = "OPV Cov. (PCA)",
                  width = 12/6,
                  color = "green"
                ),
                valueBox(
                  value = pca_cov_ipv,
                  subtitle = "IPV Cov. (PCA)",
                  width = 12/6,
                  color = "green"
                ),
                valueBox(
                  value = lqas_pct_lots_passed,
                  subtitle = "OPV % Lots Pass (LQAS)",
                  width = 12/6,
                  color = "olive"
                ),
                valueBox(
                  value = lqas_pct_lots_passed_ipv,
                  subtitle = "IPV % Lots Pass (LQAS)",
                  width = 12/6,
                  color = "olive"
                )
              )
        } else{
            pca_cov <- overview_filtered_sia_data()$district_indicators %>%
              filter(indicator == "pca_fm_coverage_0_59m") %>%
              ungroup() %>%
              summarise_at(c("numerator", "denominator"), ~sum(.,na.rm=T)) %>%
              rowwise() %>%
              mutate(pca_cov = paste0(round(100*(numerator / denominator), 0),"%"))
            
            if(is.na(pca_cov$numerator) | is.na(pca_cov$denominator) | pca_cov$denominator == 0){
              pca_cov <- "No Data"
            } else{pca_cov <-pca_cov$pca_cov[1]}
            
            admin_cov <- overview_filtered_admin_data()$district %>%
              rowwise() %>%
              mutate(total_vaccinated = sum(total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, total_children_vaccinated_day5, total_children_vaccinated_day6, total_children_vaccinated_day7, na.rm=T)) %>%
              select(total_vaccinated, target_population) %>%
              ungroup() %>%
              summarise_all(~sum(., na.rm=T)) %>%
              rowwise() %>%
              mutate(pct = total_vaccinated / target_population) %>%
              mutate(admin_cov = paste0(round(100*(total_vaccinated / target_population), 0),"%"))
            
            if(is.na(admin_cov$total_vaccinated) | is.na(admin_cov$target_population) | admin_cov$target_population == 0){
              admin_cov <- "No Data"
            } else{admin_cov <-admin_cov$admin_cov[1]}
            
            lqas_pct_lots_passed <- overview_filtered_sia_data()$district_indicators %>%
              filter(indicator == "lqas_result") %>%
              filter(value %in% c("One or More Lots Failed", "All Lots Passed")) %>%
              summarise(numerator = sum(numerator, na.rm=T),
                        total = sum(denominator, na.rm=T)) %>%
              mutate(pct_pass = paste0(round(numerator / total, 2)*100,"%")) 
            
            if(is.na(lqas_pct_lots_passed$numerator[1]) |
               is.na(lqas_pct_lots_passed$total[1]) |
               lqas_pct_lots_passed$total[1] == 0){
              lqas_pct_lots_passed <- "No Data"
            } else{lqas_pct_lots_passed <- lqas_pct_lots_passed$pct_pass[1]}
            
            list(
              valueBox(
                value = admin_cov,
                subtitle = "OPV Coverage (Admin Data)",
                width = 12/3,
                color = "green"
              ),
              valueBox(
                value = pca_cov,
                subtitle = "OPV Coverage (PCA)",
                width = 12/3,
                color = "green"
              ),
              valueBox(
                value = lqas_pct_lots_passed,
                subtitle = "Percent of Lots Passed (LQAS, OPV)",
                width = 12/3,
                color = "green"
              )
            )
        }
      })
  })
}

