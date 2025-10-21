reportModuleUI <- function(id) {
  ns <- NS(id)
  downloadButton(ns("download_report"), "Download Report", class = "custom-download-btn")
}

reportModuleServer <- function(id, report_data, report_data_all, report_borders_district, campaign_name, region, province, district) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    triggered <- reactiveVal(NULL)
    
    library(ggplot2)
    library(patchwork)
    
    template <- officer::read_pptx(here::here("reports", "apmis_slide_template.pptx"))
    # officer::layout_properties(template)
    my_pres <- template %>%
      remove_slide(index = 1)
    
    is_valid_df <- function(x) {
      is.data.frame(x) && nrow(x) > 0
    }
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("APMIS_Coverage_Report_", campaign_name(), "_", Sys.Date(), ".pptx")
      },
      content = function(file) {
        showNotification("Generating report...", type = "message", duration = NULL, closeButton = FALSE, id = "report_notice")
        
        if(grepl("IPV", campaign_name(), ignore.case=T)){
          vaccine_type <- "IPV"
        } else{
          vaccine_type <- "OPV"
        }
        
        ## All data for trend
          admin_data_trend <- report_data_all()$admin_data
          postcampaign_data_trend <- report_data_all()$postcampaign_data
          
          end_campaign_date <- df_campaigns %>%
            # filter(campaign_name == "Training IPV") %>%
            filter(campaign_name == campaign_name()) %>%
            pull(campaign_startdate)
          
          campaigns_pre_selected <- df_campaigns %>%
            filter(campaign_startdate <= end_campaign_date) %>%
            pull(campaign_name)
          
          admin_data_trend <- purrr::map(admin_data_trend, function(x){
            if("campaign_name" %in% colnames(x)){
              x <- x %>%
                filter(campaign_name %in% campaigns_pre_selected)
            } 
            return(x)
          })
          postcampaign_data_trend <- purrr::map(postcampaign_data_trend, function(x){
            if("region" %in% colnames(x)){
              x <- x %>%
                filter(campaign_name %in% campaigns_pre_selected)
            } 
            return(x)
          })
          
          if(!("All" %in% region())){
            admin_data_trend <- purrr::map(admin_data_trend, function(x){
              if("region" %in% colnames(x)){
                x <- x %>%
                  filter(region %in% region())
              } 
              return(x)
            })
            
            postcampaign_data_trend <- purrr::map(postcampaign_data_trend, function(x){
              if("region" %in% colnames(x)){
                x <- x %>%
                  filter(region %in% region())
              } 
              return(x)
            })
          }
          if(!("All" %in% province())){
            admin_data_trend <- purrr::map(admin_data_trend, function(x){
              if("province" %in% colnames(x)){
                x <- x %>%
                  filter(province %in% province())
              } 
              return(x)
            })
            
            postcampaign_data_trend <- purrr::map(postcampaign_data_trend, function(x){
              if("province" %in% colnames(x)){
                x <- x %>%
                  filter(province %in% province())
              } 
              return(x)
            })
          }
          if(!("All" %in% district())){
            admin_data_trend <- purrr::map(admin_data_trend, function(x){
              if("district" %in% colnames(x)){
                x <- x %>%
                  filter(district %in% district())
              } 
              return(x)
            })
            
            postcampaign_data_trend <- purrr::map(postcampaign_data_trend, function(x){
              if("district" %in% colnames(x)){
                x <- x %>%
                  filter(district %in% district())
              } 
              return(x)
            })
          }
        
          
        #PCA Coverage Trend
        pca_cov_trend <- postcampaign_data_trend$district_indicators %>%
          filter(indicator == "pca_fm_coverage_0_59m") %>%
          select(campaign_name, numerator, denominator) %>%
          group_by(campaign_name) %>%
          summarize_all(~sum(as.numeric(.), na.rm=T)) %>%
          ungroup() %>%
          filter(denominator != 0 & !is.na(denominator) & !is.na(numerator)) %>%
          mutate(value = numerator / denominator) %>%
          mutate(label = scales::percent(value, accuracy=1)) %>%
          left_join(df_campaigns %>%
                      select(campaign_name, campaign_startdate) %>%
                      distinct(),
                    by=c("campaign_name")) %>%
          arrange(campaign_startdate) 
        
        pca_cov_trend <- pca_cov_trend %>%
          mutate(campaign_name = factor(campaign_name, levels=unique(pca_cov_trend$campaign_name)))
        
        pca_trend_plot <- ggplot(data=pca_cov_trend)+
          geom_point(aes(x=campaign_name, y=value)) +
          geom_line(aes(x=campaign_name, y=value, group=1)) +
            geom_text_repel(
              aes(x = campaign_name, y = value, label = label),
              size = 3,
              max.overlaps = Inf,  # allow all labels to be plotted
              direction = "y",     # repel only vertically (optional)
              nudge_y = 0.01,       # small vertical nudge to reduce initial overlaps
              segment.color = NA
              ) +
          scale_y_continuous(labels = percent_format(scale = 100), limits=c(0,max(max(pca_cov_trend$value)+0.05*max(pca_cov_trend$value), 1.05))) +  # Format y-axis as percentage
          labs( y = "% Finger-Marked") +
          theme_minimal() +
          theme(
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1, size=9),
            axis.title.y = element_text(size=9),
            axis.text.y = element_text(size=9)
          )
        
        lqas_trend <- postcampaign_data_trend$district_indicators %>%
          filter(indicator == "lqas_result") %>%
          select(campaign_name, numerator, denominator) %>%
          group_by(campaign_name) %>%
          summarize_all(~sum(as.numeric(.), na.rm=T)) %>%
          ungroup() %>%
          filter(denominator != 0 & !is.na(denominator) & !is.na(numerator)) %>%
          mutate(value = numerator / denominator) %>%
          mutate(label = scales::percent(value, accuracy=1)) %>%
          left_join(df_campaigns %>%
                      select(campaign_name, campaign_startdate) %>%
                      distinct(),
                    by=c("campaign_name")) %>%
          arrange(campaign_startdate) 
        
        lqas_trend <- lqas_trend %>%
          mutate(campaign_name = factor(campaign_name, levels=unique(lqas_trend$campaign_name)))
        
        lqas_trend_plot <- ggplot(data=lqas_trend)+
          geom_point(aes(x=campaign_name, y=value)) +
          geom_line(aes(x=campaign_name, y=value, group=1)) +
          geom_text_repel(
            aes(x = campaign_name, y = value, label = label),
            size = 3,
            max.overlaps = Inf,  # allow all labels to be plotted
            direction = "y",     # repel only vertically (optional)
            nudge_y = 0.01,       # small vertical nudge to reduce initial overlaps
            segment.color = NA
          ) +
          scale_y_continuous(labels = percent_format(scale = 100), limits=c(0,max(max(lqas_trend$value)+0.05*max(lqas_trend$value), 1.05))) +  # Format y-axis as percentage
          labs( y = "% of Lots Passed") +
          theme_minimal() +
          theme(
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1),
            legend.title = element_blank()
          )
        
        ooh_trend <- postcampaign_data_trend$district_indicators %>%
          filter(indicator == "ooh_fm_coverage") %>%
          select(campaign_name, numerator, denominator) %>%
          group_by(campaign_name) %>%
          summarize_all(~sum(as.numeric(.), na.rm=T)) %>%
          ungroup() %>%
          filter(denominator != 0 & !is.na(denominator) & !is.na(numerator)) %>%
          mutate(value = numerator / denominator) %>%
          mutate(label = scales::percent(value, accuracy=1)) %>%
          left_join(df_campaigns %>%
                      select(campaign_name, campaign_startdate) %>%
                      distinct(),
                    by=c("campaign_name")) %>%
          arrange(campaign_startdate) 
        
        ooh_trend <- ooh_trend %>%
          mutate(campaign_name = factor(campaign_name, levels=unique(ooh_trend$campaign_name)))
        
        ooh_trend_plot <- ggplot(data=ooh_trend)+
          geom_point(aes(x=campaign_name, y=value)) +
          geom_line(aes(x=campaign_name, y=value, group=1)) +
          geom_text_repel(
            aes(x = campaign_name, y = value, label = label),
            size = 3,
            max.overlaps = Inf,  # allow all labels to be plotted
            direction = "y",     # repel only vertically (optional)
            nudge_y = 0.01,       # small vertical nudge to reduce initial overlaps
            segment.color = NA
          ) +
          scale_y_continuous(labels = percent_format(scale = 100), limits=c(0,max(max(ooh_trend$value)+0.05*max(ooh_trend$value), 1.05))) +  # Format y-axis as percentage
          labs( y = "% Finger-Marked") +
          theme_minimal() +
          theme(
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1),
            legend.title = element_blank()
          )
          
        admin_trend <- admin_data_trend$district %>%
          filter(vaccine_type == "OPV") %>%
          select(campaign_name, target_population, total_vaccinated) %>%
          rename(numerator = total_vaccinated,
                 denominator = target_population) %>%
          group_by(campaign_name) %>%
          summarize_all(~sum(as.numeric(.), na.rm=T)) %>%
          ungroup() %>%
          filter(denominator != 0 & !is.na(denominator) & !is.na(numerator)) %>%
          mutate(value = numerator / denominator) %>%
          mutate(label = scales::percent(value, accuracy=1)) %>%
          left_join(df_campaigns %>%
                      select(campaign_name, campaign_startdate) %>%
                      distinct(),
                    by=c("campaign_name")) %>%
          arrange(campaign_startdate) 
        
        admin_trend <- admin_trend %>%
          mutate(campaign_name = factor(campaign_name, levels=unique(admin_trend$campaign_name)))
        
        admin_trend_plot <- ggplot(data=admin_trend)+
          geom_point(aes(x=campaign_name, y=value)) +
          geom_line(aes(x=campaign_name, y=value, group=1)) +
          geom_text_repel(
            aes(x = campaign_name, y = value, label = label),
            size = 3,
            max.overlaps = Inf,  # allow all labels to be plotted
            direction = "y",     # repel only vertically (optional)
            nudge_y = 0.01,       # small vertical nudge to reduce initial overlaps
            segment.color = NA
          ) +
          scale_y_continuous(labels = percent_format(scale = 100), limits=c(0,max(admin_trend$value)+0.05*max(admin_trend$value))) +  # Format y-axis as percentage
          labs( y = "Admin Coverage (OPV)") +
          theme_minimal() +
          theme(
            axis.title.x = element_blank(),
            axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1),
            legend.title = element_blank()
          )
        
        if(vaccine_type == "IPV"){
          admin_trend_ipv <- admin_data_trend$district %>%
            filter(vaccine_type == "IPV") %>%
            select(campaign_name, target_population, total_vaccinated) %>%
            rename(numerator = total_vaccinated,
                   denominator = target_population) %>%
            group_by(campaign_name) %>%
            summarize_all(~sum(as.numeric(.), na.rm=T)) %>%
            ungroup() %>%
            filter(denominator != 0 & !is.na(denominator) & !is.na(numerator)) %>%
            mutate(value = numerator / denominator) %>%
            mutate(label = scales::percent(value, accuracy=1)) %>%
            left_join(df_campaigns %>%
                        select(campaign_name, campaign_startdate) %>%
                        distinct(),
                      by=c("campaign_name")) %>%
            arrange(campaign_startdate) 
          
          admin_trend_ipv <- admin_trend_ipv %>%
            mutate(campaign_name = factor(campaign_name, levels=unique(admin_trend_ipv$campaign_name)))
          
          admin_trend_plot_ipv <- ggplot(data=admin_trend_ipv)+
            geom_point(aes(x=campaign_name, y=value)) +
            geom_line(aes(x=campaign_name, y=value, group=1)) +
            geom_text_repel(
              aes(x = campaign_name, y = value, label = label),
              size = 3,
              max.overlaps = Inf,  # allow all labels to be plotted
              direction = "y",     # repel only vertically (optional)
              nudge_y = 0.01,       # small vertical nudge to reduce initial overlaps
              segment.color = NA
            ) +
            scale_y_continuous(labels = percent_format(scale = 100), limits=c(0,max(admin_trend_ipv$value)+0.05*max(admin_trend_ipv$value))) +  # Format y-axis as percentage
            labs( y = "Admin Coverage (IPV)") +
            theme_minimal() +
            theme(
              axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1),
              legend.title = element_blank()
            )
          
          pca_cov_trend_ipv <- postcampaign_data_trend$district_indicators %>%
            filter(indicator == "pca_fm_coverage_ipv") %>%
            select(campaign_name, numerator, denominator) %>%
            group_by(campaign_name) %>%
            summarize_all(~sum(as.numeric(.), na.rm=T)) %>%
            ungroup() %>%
            filter(denominator != 0 & !is.na(denominator) & !is.na(numerator)) %>%
            mutate(value = numerator / denominator) %>%
            mutate(label = scales::percent(value, accuracy=1)) %>%
            left_join(df_campaigns %>%
                        select(campaign_name, campaign_startdate) %>%
                        distinct(),
                      by=c("campaign_name")) %>%
            arrange(campaign_startdate) 
          
          pca_cov_trend_ipv <- pca_cov_trend_ipv %>%
            mutate(campaign_name = factor(campaign_name, levels=unique(pca_cov_trend_ipv$campaign_name)))
          
          pca_trend_plot_ipv <- ggplot(data=pca_cov_trend_ipv)+
            geom_point(aes(x=campaign_name, y=value)) +
            geom_line(aes(x=campaign_name, y=value, group=1)) +
            geom_text_repel(
              aes(x = campaign_name, y = value, label = label),
              size = 3,
              max.overlaps = Inf,  # allow all labels to be plotted
              direction = "y",     # repel only vertically (optional)
              nudge_y = 0.01,       # small vertical nudge to reduce initial overlaps
              segment.color = NA
            ) +
            scale_y_continuous(labels = percent_format(scale = 100), limits=c(0,max(max(pca_cov_trend_ipv$value)+0.05*max(pca_cov_trend_ipv$value), 1.05))) +  # Format y-axis as percentage
            labs( y = "% Finger-Marked") +
            theme_minimal() +
            theme(
              axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1, size=9),
              axis.title.y = element_text(size=9),
              axis.text.y = element_text(size=9)
            )
          
          lqas_trend_ipv <- postcampaign_data_trend$district_indicators %>%
            filter(indicator == "lqas_fipv_result") %>%
            select(campaign_name, numerator, denominator) %>%
            group_by(campaign_name) %>%
            summarize_all(~sum(as.numeric(.), na.rm=T)) %>%
            ungroup() %>%
            filter(denominator != 0 & !is.na(denominator) & !is.na(numerator)) %>%
            mutate(value = numerator / denominator) %>%
            mutate(label = scales::percent(value, accuracy=1)) %>%
            left_join(df_campaigns %>%
                        select(campaign_name, campaign_startdate) %>%
                        distinct(),
                      by=c("campaign_name")) %>%
            arrange(campaign_startdate) 
          
          lqas_trend_ipv <- lqas_trend_ipv %>%
            mutate(campaign_name = factor(campaign_name, levels=unique(lqas_trend_ipv$campaign_name)))
          
          lqas_trend_plot_ipv <- ggplot(data=lqas_trend_ipv)+
            geom_point(aes(x=campaign_name, y=value)) +
            geom_line(aes(x=campaign_name, y=value, group=1)) +
            geom_text_repel(
              aes(x = campaign_name, y = value, label = label),
              size = 3,
              max.overlaps = Inf,  # allow all labels to be plotted
              direction = "y",     # repel only vertically (optional)
              nudge_y = 0.01,       # small vertical nudge to reduce initial overlaps
              segment.color = NA
            ) +
            scale_y_continuous(labels = percent_format(scale = 100), limits=c(0,max(max(lqas_trend_ipv$value)+0.05*max(lqas_trend_ipv$value), 1.05))) +  # Format y-axis as percentage
            labs( y = "% of Lots Passed") +
            theme_minimal() +
            theme(
              axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1),
              legend.title = element_blank()
            )
          
          ooh_trend_ipv <- postcampaign_data_trend$district_indicators %>%
            filter(indicator == "ooh_fm_coverage_ipv") %>%
            select(campaign_name, numerator, denominator) %>%
            group_by(campaign_name) %>%
            summarize_all(~sum(as.numeric(.), na.rm=T)) %>%
            ungroup() %>%
            filter(denominator != 0 & !is.na(denominator) & !is.na(numerator)) %>%
            mutate(value = numerator / denominator) %>%
            mutate(label = scales::percent(value, accuracy=1)) %>%
            left_join(df_campaigns %>%
                        select(campaign_name, campaign_startdate) %>%
                        distinct(),
                      by=c("campaign_name")) %>%
            arrange(campaign_startdate) 
          
          ooh_trend_ipv <- ooh_trend_ipv %>%
            mutate(campaign_name = factor(campaign_name, levels=unique(ooh_trend_ipv$campaign_name)))
          
          ooh_trend_plot_ipv <- ggplot(data=ooh_trend_ipv)+
            geom_point(aes(x=campaign_name, y=value)) +
            geom_line(aes(x=campaign_name, y=value, group=1)) +
            geom_text_repel(
              aes(x = campaign_name, y = value, label = label),
              size = 3,
              max.overlaps = Inf,  # allow all labels to be plotted
              direction = "y",     # repel only vertically (optional)
              nudge_y = 0.01,       # small vertical nudge to reduce initial overlaps
              segment.color = NA
            ) +
            scale_y_continuous(labels = percent_format(scale = 100), limits=c(0,max(max(ooh_trend_ipv$value)+0.05*max(ooh_trend_ipv$value), 1.05))) +  # Format y-axis as percentage
            labs( y = "% Finger-Marked") +
            theme_minimal() +
            theme(
              axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1),
              legend.title = element_blank()
            )
        }
        
        
        if(region() == "All"){
        postcampaign_summary_data <- report_data()$postcampaign_data$national_indicators
        admin_summary_data <- report_data()$admin_data$conversion_national 
        admin_summary_data1 <- report_data()$admin_data$national 
        } else{
          if(province() == "All"){
            postcampaign_summary_data <- report_data()$postcampaign_data$region_indicators
            admin_summary_data <- report_data()$admin_data$conversion_region
            admin_summary_data1 <- report_data()$admin_data$region 
          } else{
            if(district() == "All"){
              postcampaign_summary_data <- report_data()$postcampaign_data$province_indicators
              admin_summary_data <- report_data()$admin_data$conversion_province
              admin_summary_data1 <- report_data()$admin_data$province 
            } else{
              postcampaign_summary_data <- report_data()$postcampaign_data$district_indicators
              admin_summary_data <- report_data()$admin_data$conversion_district
              admin_summary_data1 <- report_data()$admin_data$district 
            }
          }
        }
        
        data_long <- postcampaign_summary_data %>%
          filter(indicator == "pca_reasons_missed")
        
    if (is_valid_df(data_long) & nrow(data_long <- data_long %>%
                                      select("category", "numerator") %>%
                                      filter(numerator != 0))>0){
        data_long <- data_long %>%
          select("category", "numerator") %>%
          filter(numerator != 0) %>%
          rowwise() %>%
          mutate(row_id = list(1:numerator)) %>%  # Create a list of row ids for each group
          unnest(row_id) %>%  # Expand the data frame by unnesting the list
          select(-numerator)
        
        pca_pie_data <- data_long %>%
          mutate(category = case_when(category == "absent_market_street" ~ "Absent",
                 category == "absent_school_madarasa_hf" ~ "Absent",
                 category == "absent_travel" ~ "Absent",
                 category == "absent_others" ~ "Absent",
                 
                 category == "child_not_available" ~ "Child Not Available",
                 
                 category == "mosque_is_far" ~ "Vaccination site is too far",
                 category == "house_far_from_site" ~ "Vaccination site is too far",
                 category == "site_is_far" ~ "Vaccination site is too far",
                 
                 category == "newborn" ~ "Newborn/Sleeping/Sick",
                 category == "sleep" ~ "Newborn/Sleeping/Sick",
                 category == "sick" ~ "Newborn/Sleeping/Sick",
                 
                 category == "no_one_home" ~ "No One Available to Take Child to Site",
                 category == "not_aware" ~ "Not Aware",
                 category == "refusal" ~ "Refusal",
                 
                 category == "refusal_decision_maker_notat_home" ~ "Refusal",
                 category == "refusal_misperception" ~ "Refusal",
                 
                 category == "team_did_not_visit" ~ "Team did not Visit",
                 category == "team_did_not_visit_the_site_area" ~ "Team did not Visit",
                 
                 category == "other_reasons" ~ "Other",
                 category == "other" ~ "Other",
                 TRUE ~ "Other"
          )) %>%
      mutate(category = factor(category, levels = c("Absent", "Newborn/Sleeping/Sick", "Refusal", "Team did not Visit", "Not Aware", "Vaccination site is too far", "Child Not Available", "No One Available to Take Child to Site", "Other"))) %>%
      arrange(desc(category))
        
      
      pcm_colors <- colors_reasons_missed_pca[names(colors_reasons_missed_pca) %in% pca_pie_data$category]
      
      pca_pie_data <- pca_pie_data %>%
        mutate(category = as.character(category)) %>%
        group_by(category) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(
          percentage = round(count / sum(count) * 100, 0),
          tooltip_text = paste0(category, ": ", percentage, "% (", 
                                scales::comma(count), "/", 
                                scales::comma(sum(count)), ")"),
          label = paste0(category, ": ", percentage, "%")
        ) %>%
        arrange(desc(percentage)) %>%
        mutate(
          category = factor(category, levels = category),  # for fill + color
          fraction = percentage / sum(percentage),
          ymax = cumsum(fraction),
          ymin = c(0, head(ymax, -1)),
          mid = (ymin + ymax) / 2  # used to rotate the largest segment to 12
        )
      
      # Reuse pcm_colors based on category
      sorted_colors <- pcm_colors[as.character(pca_pie_data$category)]
      
      # Compute angle offset to rotate the largest slice to 12 o'clock
      
      # Final plot
      pca_pie <- ggplot(pca_pie_data, aes(ymax = ymax, ymin = ymin, xmax = 1, xmin = 0, fill = category)) +
        geom_rect(color = "white") +
        coord_polar(theta = "y", start = 0, direction = 1) +
        scale_fill_manual(
          name = paste0("Distribution of Reasons Missed (n=", scales::comma(sum(pca_pie_data$count, na.rm=T)),"):"),
          values = sorted_colors,
          labels = pca_pie_data$label
        ) +
        theme_void() +
        theme(
          legend.title = element_text(size=11),
          legend.text = element_text(size = 9),
          legend.position = "right"
        )
    } else{
      pca_pie <- NULL
    }
    
      data_long <- postcampaign_summary_data %>%
        filter(indicator == "lqas_reasons_missed") 
      
    if (is_valid_df(data_long) & nrow(data_long <- data_long %>%
                                      select("category", "numerator") %>%
                                      filter(numerator != 0))>0){  
      data_long <- data_long %>%
        select("category", "numerator") %>%
        filter(numerator != 0) %>%
        rowwise() %>%
        mutate(row_id = list(1:numerator)) %>%  # Create a list of row ids for each group
        unnest(row_id) %>%  # Expand the data frame by unnesting the list
        select(-numerator)
      
      lqas_pie_data <- data_long %>%
        mutate(category = case_when(category == "Absent" ~ "Absent",
                                    category == "NewBorn" ~ "Newborn/Sleeping/Sick",
                                    category == "Sleep" ~ "Newborn/Sleeping/Sick",
                                    category == "Sick" ~ "Newborn/Sleeping/Sick",
                                    category == "SickSleep" ~ "Newborn/Sleeping/Sick",
                                    category == "Refuse" ~ "Refusal",
                                    category == "NoTeam" ~ "Team did not Visit",
                                    category == "ExpectedHomeVisit" ~ "Expected Home Visit",
                                    category == "NoVaccineAtSite" ~ "No Vaccine at Site",
                                    category == "ParentForgotOrNoTime" ~ "Parent Forgot Or No Time",
                                    category == "ParentsDidNotKnowAboutCampaign" ~ "Not Aware",
                                    category == "SiteTooFar" ~ "Vaccination site is too far",
                                    category == "VeryLongQueue" ~ "Very Long Queue",
                                    category == "Other" ~ "Other",
                                    TRUE ~ "Other"
        )) %>%
        mutate(category = factor(category, levels = c("Absent", "Newborn/Sleeping/Sick", "Refusal", "Team did not Visit", "Expected Home Visit", "No Vaccine at Site", "Parent Forgot Or No Time", 
        "Not Aware", "Vaccination site is too far", "Very Long Queue", "Other"))) %>%
        arrange(desc(category))
      
      
      lqas_colors <- colors_reasons_missed_lqas[names(colors_reasons_missed_lqas) %in% lqas_pie_data$category]
      
      lqas_pie_data <- lqas_pie_data %>%
        mutate(category = as.character(category)) %>%
        group_by(category) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(
          percentage = round(count / sum(count) * 100, 0),
          tooltip_text = paste0(category, ": ", percentage, "% (", 
                                scales::comma(count), "/", 
                                scales::comma(sum(count)), ")"),
          label = paste0(category, ": ", percentage, "%")
        ) %>%
        arrange(desc(percentage)) %>%
        mutate(
          category = factor(category, levels = category),  # for fill + color
          fraction = percentage / sum(percentage),
          ymax = cumsum(fraction),
          ymin = c(0, head(ymax, -1)),
          mid = (ymin + ymax) / 2  # used to rotate the largest segment to 12
        )
      
      # Reuse pcm_colors based on category
      sorted_colors <- lqas_colors[as.character(lqas_pie_data$category)]
      
      # Compute angle offset to rotate the largest slice to 12 o'clock
      
      # Final plot
      lqas_pie <- ggplot(lqas_pie_data, aes(ymax = ymax, ymin = ymin, xmax = 1, xmin = 0, fill = category)) +
        geom_rect(color = "white") +
        coord_polar(theta = "y", start = 0, direction = 1) +
        scale_fill_manual(
          name = paste0("Distribution of Reasons Missed (n=", scales::comma(sum(lqas_pie_data$count, na.rm=T)),"):"),
          values = sorted_colors,
          labels = lqas_pie_data$label
        ) +
        theme_void() +
        theme(
          legend.title = element_text(size=11),
          legend.text = element_text(size = 9),
          legend.position = "right"
        )
    } else{
      lqas_pie <- NULL
    }
      
      data_long <- postcampaign_summary_data %>%
        filter(indicator == "ooh_reasons_missed") 
      
    if (is_valid_df(data_long) & nrow(data_long <- data_long %>%
                                      select("category", "numerator") %>%
                                      filter(numerator != 0) %>%
                                      rowwise())>0){
      data_long <- data_long %>%
        select("category", "numerator") %>%
        filter(numerator != 0) %>%
        rowwise() %>%
        mutate(row_id = list(1:numerator)) %>%  # Create a list of row ids for each group
        unnest(row_id) %>%  # Expand the data frame by unnesting the list
        select(-numerator)
      
      ooh_pie_data <- data_long %>%
        mutate(category = case_when(category == "absent" ~ "Absent",
                                    category == "too_far" ~ "Vaccination site is too far",
                                    category %in% c("nss", "newborn_sick_sleep") ~ "Newborn/Sleeping/Sick",
                                    category == "no_men" ~ "No One Available to Take Child to Site",
                                    category == "not_aware" ~ "Not Aware",
                                    category == "refusal" ~ "Refusal",
                                    category == "team_not_come" ~ "Team did not Visit",
                                    category == "other" ~ "Other",
                                    TRUE ~ "Other"
        )) %>%
        mutate(category = factor(category, levels = c("Absent", "Vaccination site is too far", "Newborn/Sleeping/Sick", "No One Available to Take Child to Site",
                                                      "Not Aware", "Refusal", "Team did not Visit", "Other"))) %>%
        arrange(desc(category))
      
      
      ooh_colors <- colors_reasons_missed_ooh[names(colors_reasons_missed_ooh) %in% ooh_pie_data$category]
      
      ooh_pie_data <- ooh_pie_data %>%
        mutate(category = as.character(category)) %>%
        group_by(category) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(
          percentage = round(count / sum(count) * 100, 0),
          tooltip_text = paste0(category, ": ", percentage, "% (", 
                                scales::comma(count), "/", 
                                scales::comma(sum(count)), ")"),
          label = paste0(category, ": ", percentage, "%")
        ) %>%
        arrange(desc(percentage)) %>%
        mutate(
          category = factor(category, levels = category),  # for fill + color
          fraction = percentage / sum(percentage),
          ymax = cumsum(fraction),
          ymin = c(0, head(ymax, -1)),
          mid = (ymin + ymax) / 2  # used to rotate the largest segment to 12
        )
      
      # Reuse pcm_colors based on category
      sorted_colors <- ooh_colors[as.character(ooh_pie_data$category)]
      
      # Compute angle offset to rotate the largest slice to 12 o'clock
      
      # Final plot
      ooh_pie <- ggplot(ooh_pie_data, aes(ymax = ymax, ymin = ymin, xmax = 1, xmin = 0, fill = category)) +
        geom_rect(color = "white") +
        coord_polar(theta = "y", start = 0, direction = 1) +
        scale_fill_manual(
          name = paste0("Distribution of Reasons Missed (n=", scales::comma(sum(ooh_pie_data$count, na.rm=T)),"):"),
          values = sorted_colors,
          labels = ooh_pie_data$label
        ) +
        theme_void() +
        theme(
          legend.title = element_text(size=11),
          legend.text = element_text(size = 9),
          legend.position = "right"
        )
    } else{
      ooh_pie <- NULL
    }
      
      
      data_long <- admin_summary_data %>%
        filter(age_group %in% c("0-59 Months", "4-59 Months")) %>%
        filter(vaccine_type == "OPV")
      
      if (is_valid_df(data_long)){
        data_long <- data_long %>%
        select(recorded_missed_reason_absent1, recorded_missed_reason_absent1_vaccinated, 
               recorded_missed_reason_absent2, recorded_missed_reason_absent2_vaccinated,
               recorded_missed_reason_nss, recorded_missed_reason_nss_vaccinated, 
               recorded_missed_reason_refusal,
               recorded_missed_reason_refusal_vaccinated) %>%
        rowwise() %>%
        mutate(remaining_absent1 = recorded_missed_reason_absent1 - recorded_missed_reason_absent1_vaccinated,
               remaining_absent2 = recorded_missed_reason_absent2 - recorded_missed_reason_absent2_vaccinated,
               remaining_nss = recorded_missed_reason_nss - recorded_missed_reason_nss_vaccinated,
               remaining_refusal = recorded_missed_reason_refusal - recorded_missed_reason_refusal_vaccinated) %>%
        ungroup() %>%
        select(remaining_absent1, remaining_absent2, remaining_nss, remaining_refusal) %>%
        mutate(group="A") %>%
        pivot_longer(cols=-c("group"), names_to = "category") %>%
        ungroup() %>%
        select(-group) %>%
        filter(value != 0) 
      
      if (is_valid_df(data_long)){
      data_long <- data_long %>%
        rowwise() %>%
        mutate(row_id = list(1:value)) %>%  # Create a list of row ids for each group
        unnest(row_id) %>%  # Expand the data frame by unnesting the list
        select(-value)
      
      
      admin_pie_data <- data_long %>%
        mutate(category = case_when(category == "remaining_absent1" ~ "Child Absent (Return During Campaign)",
                                    category == "remaining_absent2" ~ "Child Absent (Return After Campaign)",
                                    category == "remaining_nss" ~ "Newborn/Sleeping/Sick",
                                    category == "remaining_refusal" ~ "Refusal",
                                    TRUE ~ "Other"
        )) %>%
        mutate(category = factor(category, levels = c("Child Absent (Return During Campaign)", "Child Absent (Return After Campaign)", "Newborn/Sleeping/Sick", "Refusal", "Other"))) %>%
        arrange(desc(category))
      
      
      admin_colors <- colors_reasons_missed_admin[names(colors_reasons_missed_admin) %in% admin_pie_data$category]
      
      admin_pie_data <- admin_pie_data %>%
        mutate(category = as.character(category)) %>%
        group_by(category) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(
          percentage = round(count / sum(count) * 100, 0),
          tooltip_text = paste0(category, ": ", percentage, "% (", 
                                scales::comma(count), "/", 
                                scales::comma(sum(count)), ")"),
          label = paste0(category, ": ", percentage, "%")
        ) %>%
        arrange(desc(percentage)) %>%
        mutate(
          category = factor(category, levels = category),  # for fill + color
          fraction = percentage / sum(percentage),
          ymax = cumsum(fraction),
          ymin = c(0, head(ymax, -1)),
          mid = (ymin + ymax) / 2  # used to rotate the largest segment to 12
        )
      
      # Reuse pcm_colors based on category
      sorted_colors <- admin_colors[as.character(admin_pie_data$category)]
      
      # Compute angle offset to rotate the largest slice to 12 o'clock
      
      # Final plot
      admin_pie <- ggplot(admin_pie_data, aes(ymax = ymax, ymin = ymin, xmax = 1, xmin = 0, fill = category)) +
        geom_rect(color = "white") +
        coord_polar(theta = "y", start = 0, direction = 1) +
        scale_fill_manual(
          name = paste0("Recorded Missed\nRemaining Unvaccinated:\nDistribution of Reasons (n=", scales::comma(sum(admin_pie_data$count, na.rm=T)),"):"),
          values = sorted_colors,
          labels = admin_pie_data$label
        ) +
        theme_void() +
        theme(
          legend.title = element_text(size=11),
          legend.text = element_text(size = 9),
          legend.position = "right"
        )
      no_admin_pie <- FALSE
      } else{
        no_admin_pie <- TRUE
        admin_pie <- NULL
      }
      } else{
          admin_pie <- NULL
      }
      
      if(vaccine_type == "IPV"){
        data_long <- admin_summary_data %>%
          filter(age_group %in% c("0-59 Months", "4-59 Months")) %>%
          filter(vaccine_type == "IPV")
        
        if (is_valid_df(data_long)){
          data_long <- data_long %>%
            select(recorded_missed_reason_absent1, recorded_missed_reason_absent1_vaccinated, 
                   recorded_missed_reason_absent2, recorded_missed_reason_absent2_vaccinated,
                   recorded_missed_reason_nss, recorded_missed_reason_nss_vaccinated, 
                   recorded_missed_reason_refusal,
                   recorded_missed_reason_refusal_vaccinated) %>%
            rowwise() %>%
            mutate(remaining_absent1 = recorded_missed_reason_absent1 - recorded_missed_reason_absent1_vaccinated,
                   remaining_absent2 = recorded_missed_reason_absent2 - recorded_missed_reason_absent2_vaccinated,
                   remaining_nss = recorded_missed_reason_nss - recorded_missed_reason_nss_vaccinated,
                   remaining_refusal = recorded_missed_reason_refusal - recorded_missed_reason_refusal_vaccinated) %>%
            ungroup() %>%
            select(remaining_absent1, remaining_absent2, remaining_nss, remaining_refusal) %>%
            mutate(group="A") %>%
            pivot_longer(cols=-c("group"), names_to = "category") %>%
            ungroup() %>%
            select(-group) %>%
            filter(value != 0) 
          
          if (is_valid_df(data_long)){
            data_long <- data_long %>%
              rowwise() %>%
              mutate(row_id = list(1:value)) %>%  # Create a list of row ids for each group
              unnest(row_id) %>%  # Expand the data frame by unnesting the list
              select(-value)
            
            
            admin_pie_data <- data_long %>%
              mutate(category = case_when(category == "remaining_absent1" ~ "Child Absent (Return During Campaign)",
                                          category == "remaining_absent2" ~ "Child Absent (Return After Campaign)",
                                          category == "remaining_nss" ~ "Newborn/Sleeping/Sick",
                                          category == "remaining_refusal" ~ "Refusal",
                                          TRUE ~ "Other"
              )) %>%
              mutate(category = factor(category, levels = c("Child Absent (Return During Campaign)", "Child Absent (Return After Campaign)", "Newborn/Sleeping/Sick", "Refusal", "Other"))) %>%
              arrange(desc(category))
            
            
            admin_colors <- colors_reasons_missed_admin[names(colors_reasons_missed_admin) %in% admin_pie_data$category]
            
            admin_pie_data <- admin_pie_data %>%
              mutate(category = as.character(category)) %>%
              group_by(category) %>%
              summarise(count = n(), .groups = "drop") %>%
              mutate(
                percentage = round(count / sum(count) * 100, 0),
                tooltip_text = paste0(category, ": ", percentage, "% (", 
                                      scales::comma(count), "/", 
                                      scales::comma(sum(count)), ")"),
                label = paste0(category, ": ", percentage, "%")
              ) %>%
              arrange(desc(percentage)) %>%
              mutate(
                category = factor(category, levels = category),  # for fill + color
                fraction = percentage / sum(percentage),
                ymax = cumsum(fraction),
                ymin = c(0, head(ymax, -1)),
                mid = (ymin + ymax) / 2  # used to rotate the largest segment to 12
              )
            
            # Reuse pcm_colors based on category
            sorted_colors <- admin_colors[as.character(admin_pie_data$category)]
            
            # Compute angle offset to rotate the largest slice to 12 o'clock
            
            # Final plot
            admin_pie_ipv <- ggplot(admin_pie_data, aes(ymax = ymax, ymin = ymin, xmax = 1, xmin = 0, fill = category)) +
              geom_rect(color = "white") +
              coord_polar(theta = "y", start = 0, direction = 1) +
              scale_fill_manual(
                name = paste0("Recorded Missed\nRemaining Unvaccinated:\nDistribution of Reasons (n=", scales::comma(sum(admin_pie_data$count, na.rm=T)),"):"),
                values = sorted_colors,
                labels = admin_pie_data$label
              ) +
              theme_void() +
              theme(
                legend.title = element_text(size=11),
                legend.text = element_text(size = 9),
                legend.position = "right"
              )
            no_admin_pie_ipv <- FALSE
          } else{
            no_admin_pie_ipv <- TRUE
            admin_pie_ipv <- NULL
          }
        } else{
          admin_pie_ipv <- NULL
        }
      }
      
        
      #PCA Barchart
      
      pca_bar_data <- postcampaign_summary_data %>%
        filter(indicator %in% c("pca_fm_coverage_0_11m", 
                                "pca_fm_coverage_12_59m",
                                "pca_fm_coverage_female",
                                "pca_fm_coverage_hrmp_0_59m",
                                "pca_fm_coverage_male",
                                "pca_fm_coverage_0_59m")) 
      if (is_valid_df(pca_bar_data)){
      plot_data <- pca_bar_data %>%
        mutate(
          group = case_when(
            indicator %in% c("pca_fm_coverage_0_59m", "ooh_fm_coverage") ~ "Overall",
            indicator %in% c("pca_fm_coverage_0_11m", "pca_fm_coverage_12_59m", "ooh_fm_coverage_011m", "ooh_fm_coverage_1259m") ~ "Age",
            indicator %in% c("pca_fm_coverage_female", "pca_fm_coverage_male", "ooh_fm_coverage_male", "ooh_fm_coverage_female") ~ "Gender",
            indicator == "pca_fm_coverage_hrmp_0_59m" ~ "HRMP"
          ),
          x_label = case_when(
            indicator %in% c("pca_fm_coverage_0_59m", "ooh_fm_coverage") ~ "Overall",
            indicator %in% c("pca_fm_coverage_0_11m", "ooh_fm_coverage_011m") ~ "Ages 0–11m",
            indicator %in% c("pca_fm_coverage_12_59m", "ooh_fm_coverage_1259m") ~ "Ages 12–59m",
            indicator %in% c("pca_fm_coverage_female", "ooh_fm_coverage_female") ~ "Female",
            indicator %in% c("pca_fm_coverage_male", "ooh_fm_coverage_male") ~ "Male",
            indicator == "pca_fm_coverage_hrmp_0_59m" ~ "HRMP"
          ),
          value = as.numeric(value),
          percent_label = paste0(round(value * 100), "%"),
          group = factor(group, levels = c("Overall", "Age", "Gender", "HRMP")),
          x_label = factor(x_label, levels = c(
            "Overall", 
            "Ages 0–11m", "Ages 12–59m",
            "Female", "Male",
            "HRMP"
          ))) %>%
        filter(!is.na(value) & value > 0) %>%  # DROP if no data (NA or zero)
        arrange(group, x_label)
      
      # Step 2: Define manual colors by group
      group_colors <- c(
        "Overall" = "#8dd3c7",
        "Age" = "#80b1d3",
        "Gender" = "#fdb462",
        "HRMP" = "#fccde5"
      )
      
      # Step 3: ggplot base
      pca_bar <- ggplot(plot_data, aes(x = x_label, y = value, fill = group, text = label)) +
        geom_col(color = "black", width = 0.7, alpha=0.7) +
        geom_text(aes(label = percent_label), vjust = 0, nudge_y = 0.04, size = 3) +
        scale_fill_manual(values = group_colors) +
        scale_y_continuous(
          labels = percent_format(accuracy = 1),
          limits = c(0, 1.15),
          breaks = seq(0, 1, by = 0.2)  # 0%, 20%, ..., 100%
        ) +
        labs(x = NULL, y = "% Finger-Marked") +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1, size=9),
          axis.title.y = element_text(size=9),
          axis.text.y = element_text(size=9)
        )
      } else{
        pca_bar <- NULL
      }
      
      #LQAS Barchart
      #OOH Barchart
      ooh_bar_data <- postcampaign_summary_data %>%
        filter(indicator %in% c("ooh_fm_coverage", 
                                "ooh_fm_coverage_011m",
                                "ooh_fm_coverage_1259m",
                                "ooh_fm_coverage_male",
                                "ooh_fm_coverage_female")) 
      if (is_valid_df(ooh_bar_data)){
      plot_data <- ooh_bar_data %>%
        mutate(
          group = case_when(
            indicator %in% c("pca_fm_coverage_0_59m", "ooh_fm_coverage") ~ "Overall",
            indicator %in% c("pca_fm_coverage_0_11m", "pca_fm_coverage_12_59m", "ooh_fm_coverage_011m", "ooh_fm_coverage_1259m") ~ "Age",
            indicator %in% c("pca_fm_coverage_female", "pca_fm_coverage_male", "ooh_fm_coverage_male", "ooh_fm_coverage_female") ~ "Gender",
            indicator == "pca_fm_coverage_hrmp_0_59m" ~ "HRMP"
          ),
          x_label = case_when(
            indicator %in% c("pca_fm_coverage_0_59m", "ooh_fm_coverage") ~ "Overall",
            indicator %in% c("pca_fm_coverage_0_11m", "ooh_fm_coverage_011m") ~ "Ages 0–11m",
            indicator %in% c("pca_fm_coverage_12_59m", "ooh_fm_coverage_1259m") ~ "Ages 12–59m",
            indicator %in% c("pca_fm_coverage_female", "ooh_fm_coverage_female") ~ "Female",
            indicator %in% c("pca_fm_coverage_male", "ooh_fm_coverage_male") ~ "Male",
            indicator == "pca_fm_coverage_hrmp_0_59m" ~ "HRMP"
          ),
          value = as.numeric(value),
          percent_label = paste0(round(value * 100), "%"),
          group = factor(group, levels = c("Overall", "Age", "Gender", "HRMP")),
          x_label = factor(x_label, levels = c(
            "Overall", 
            "Ages 0–11m", "Ages 12–59m",
            "Female", "Male",
            "HRMP"
          ))) %>%
        filter(!is.na(value) & value > 0) %>%  # DROP if no data (NA or zero)
        arrange(group, x_label)
      
      # Step 2: Define manual colors by group
      group_colors <- c(
        "Overall" = "#8dd3c7",
        "Age" = "#80b1d3",
        "Gender" = "#fdb462",
        "HRMP" = "#fccde5"
      )
      
      # Step 3: ggplot base
      ooh_bar <- ggplot(plot_data, aes(x = x_label, y = value, fill = group, text = label)) +
        geom_col(color = "black", width = 0.7, alpha=0.7) +
        geom_text(aes(label = percent_label), vjust = 0, nudge_y = 0.04, size = 3) +
        scale_fill_manual(values = group_colors) +
        scale_y_continuous(
          labels = percent_format(accuracy = 1),
          limits = c(0, 1.15),
          breaks = seq(0, 1, by = 0.2)  # 0%, 20%, ..., 100%
        ) +
        labs(x = NULL, y = "% Finger-Marked") +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1, size=9),
          axis.title.y = element_text(size=9),
          axis.text.y = element_text(size=9)
        )
      } else{
        ooh_bar <- NULL
      }
      
      #Admin Barchart
      if (is_valid_df(admin_summary_data1)){
      data <- admin_summary_data1 %>%
        filter(age_group %in% c("4-59 Months", "0-59 Months")) %>%
        filter(vaccine_type == "OPV") %>%
        select(campaign_name, total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, total_children_vaccinated_day5, total_children_vaccinated_day6, total_children_vaccinated_day7) %>%
        pivot_longer(cols=-c("campaign_name"), names_to = "Campaign Day", values_to = "Children Vaccinated") %>%
        filter(!is.na(`Children Vaccinated`) & `Children Vaccinated` != 0) %>%
        mutate(`Campaign Day` = str_remove_all(`Campaign Day`, "total_children_vaccinated_"),
               `Campaign Day` = str_replace_all(`Campaign Day`, "day", "Day "))
      
      admin_bar <- ggplot(data=data) +
        geom_col(aes(x=`Campaign Day`, y=`Children Vaccinated`), fill="lightblue", color="black") +
        geom_text(aes(x = `Campaign Day`, y = `Children Vaccinated` + (0.05*max(as.numeric(`Children Vaccinated`), na.rm=T)),
                      label = scales::comma(`Children Vaccinated`)),
                  vjust = -0.5,
                  size = 2) +  # Add labels above the columns
        scale_y_continuous(labels = scales::comma,
                           limits = c(0, 1.1*max(as.numeric(data$`Children Vaccinated`), na.rm=T))) +
        theme_minimal() +
        theme(
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1, size=9),
          axis.title.y = element_text(size=9),
          axis.text.y = element_text(size=9)
        )
      } else{
        admin_bar <- NULL
      }
      
      if(vaccine_type == "IPV"){
        pca_bar_data <- postcampaign_summary_data %>%
          filter(indicator %in% c("pca_fm_coverage_ipv",
                                  "pca_fm_coverage_0_59m")) 
        if (is_valid_df(pca_bar_data)){
          plot_data <- pca_bar_data %>%
            mutate(
              group = case_when(
                indicator %in% c("pca_fm_coverage_0_59m", "ooh_fm_coverage") ~ "OPV",
                indicator %in% c("pca_fm_coverage_ipv") ~ "IPV"
               ),
              x_label = case_when(
                indicator %in% c("pca_fm_coverage_0_59m") ~ "OPV (0-59m)",
                indicator %in% c("pca_fm_coverage_ipv") ~ "IPV (4-59m)"
                ),
              value = as.numeric(value),
              percent_label = paste0(round(value * 100), "%"),
              group = factor(group, levels = c("OPV", "IPV")),
              x_label = factor(x_label, levels = c(
                "OPV (0-59m)", "IPV (4-59m)"
              ))) %>%
            filter(!is.na(value) & value > 0) %>%  # DROP if no data (NA or zero)
            arrange(group, x_label)
          
          # Step 2: Define manual colors by group
          group_colors <- c(
            "OPV" = "#8dd3c7",
            "IPV" = "#ffff99"
          )
          
          # Step 3: ggplot base
          pca_bar_ipv <- ggplot(plot_data, aes(x = x_label, y = value, fill = group, text = label)) +
            geom_col(color = "black", width = 0.7, alpha=0.7) +
            geom_text(aes(label = percent_label), vjust = 0, nudge_y = 0.04, size = 3) +
            scale_fill_manual(values = group_colors) +
            scale_y_continuous(
              labels = percent_format(accuracy = 1),
              limits = c(0, 1.15),
              breaks = seq(0, 1, by = 0.2)  # 0%, 20%, ..., 100%
            ) +
            labs(x = NULL, y = "% Finger-Marked") +
            theme_minimal() +
            theme(
              legend.position = "none",
              axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1, size=9),
              axis.title.y = element_text(size=9),
              axis.text.y = element_text(size=9)
            )
        } else{
          pca_bar_ipv <- NULL
        }
        
        #LQAS Barchart
        #OOH Barchart
        ooh_bar_data <- postcampaign_summary_data %>%
          filter(indicator %in% c("ooh_fm_coverage", 
                                  "ooh_fm_coverage_ipv")) 
        if (is_valid_df(ooh_bar_data)){
          plot_data <- ooh_bar_data %>%
            mutate(
              group = case_when(
                indicator %in% c("ooh_fm_coverage") ~ "OPV",
                indicator %in% c("ooh_fm_coverage_ipv") ~ "IPV"
                ),
              x_label = case_when(
                indicator %in% c("ooh_fm_coverage") ~ "OPV (0-59m)",
                indicator %in% c("ooh_fm_coverage_ipv") ~ "IPV (4-59m)"
              ),
              value = as.numeric(value),
              percent_label = paste0(round(value * 100), "%"),
              group = factor(group, levels = c("OPV", "IPV")),
              x_label = factor(x_label, levels = c(
                "OPV (0-59m)", "IPV (4-59m)"
              ))) %>%
            filter(!is.na(value) & value > 0) %>%  # DROP if no data (NA or zero)
            arrange(group, x_label)
          
          # Step 2: Define manual colors by group
          group_colors <- c(
            "OPV" = "#8dd3c7",
            "IPV" = "#ffff99"
          )
          
          # Step 3: ggplot base
          ooh_bar_ipv <- ggplot(plot_data, aes(x = x_label, y = value, fill = group, text = label)) +
            geom_col(color = "black", width = 0.7, alpha=0.7) +
            geom_text(aes(label = percent_label), vjust = 0, nudge_y = 0.04, size = 3) +
            scale_fill_manual(values = group_colors) +
            scale_y_continuous(
              labels = percent_format(accuracy = 1),
              limits = c(0, 1.15),
              breaks = seq(0, 1, by = 0.2)  # 0%, 20%, ..., 100%
            ) +
            labs(x = NULL, y = "% Finger-Marked") +
            theme_minimal() +
            theme(
              legend.position = "none",
              axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1, size=9),
              axis.title.y = element_text(size=9),
              axis.text.y = element_text(size=9)
            )
        } else{
          ooh_bar_ipv <- NULL
        }
        
        #Admin Barchart
        if (is_valid_df(admin_summary_data1)){
          data <- admin_summary_data1 %>%
            filter(age_group %in% c("4-59 Months", "0-59 Months")) %>%
            filter(vaccine_type == "IPV") %>%
            select(campaign_name, total_children_vaccinated_day1, total_children_vaccinated_day2, total_children_vaccinated_day3, total_children_vaccinated_day4, total_children_vaccinated_day5, total_children_vaccinated_day6, total_children_vaccinated_day7) %>%
            pivot_longer(cols=-c("campaign_name"), names_to = "Campaign Day", values_to = "Children Vaccinated") %>%
            filter(!is.na(`Children Vaccinated`) & `Children Vaccinated` != 0) %>%
            mutate(`Campaign Day` = str_remove_all(`Campaign Day`, "total_children_vaccinated_"),
                   `Campaign Day` = str_replace_all(`Campaign Day`, "day", "Day "))
          
          admin_bar_ipv <- ggplot(data=data) +
            geom_col(aes(x=`Campaign Day`, y=`Children Vaccinated`), fill="lightblue", color="black") +
            geom_text(aes(x = `Campaign Day`, y = `Children Vaccinated` + (0.05*max(as.numeric(`Children Vaccinated`), na.rm=T)),
                          label = scales::comma(`Children Vaccinated`)),
                      vjust = -0.5,
                      size = 2) +  # Add labels above the columns
            scale_y_continuous(labels = scales::comma,
                               limits = c(0, 1.1*max(as.numeric(data$`Children Vaccinated`), na.rm=T))) +
            theme_minimal() +
            theme(
              legend.position = "none",
              axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1, size=9),
              axis.title.y = element_text(size=9),
              axis.text.y = element_text(size=9)
            )
        } else{
          admin_bar_ipv <- NULL
        }
      }
      
      
        ## PCA Coverage Map --------------------------------------------------------
        data_pca <- report_data()$postcampaign_data$district_indicators %>%
          filter(indicator == "pca_fm_coverage_0_59m") %>%
          mutate(value_cat = case_when(value >= 0.95 ~ "95-100%",
                                       value >= 0.90 ~ "90-94%",
                                       value >= 0.85 ~ "85-89%",
                                       value < 0.85 ~ "<85%",
                                       TRUE ~ NA_character_)) %>%
          arrange(desc(value_cat))
        
        if (is_valid_df(data_pca) > 0){
        legend_title <- "PCA - Finger-Mark\nCoverage\n(0-59m, OPV)"
        
        report_borders_district <- report_borders_district()
        report_borders_province <- report_borders_district %>%
          group_by(APMIS_Region, APMIS_Province, APMIS_RCODE, APMIS_PCODE) %>%
          summarise(geometry = sf::st_union(geometry), .groups = "drop") 
        report_borders_region <- report_borders_district %>%
          group_by(APMIS_Region, APMIS_RCODE)  %>%
          summarise(geometry = sf::st_union(geometry), .groups = "drop")
        
        # Extract non-NA values from the specified variable
        data_cat <- sort(unique(data_pca[["value_cat"]][!is.na(data_pca[["value_cat"]]) & !(data_pca[["value_cat"]] %in% c("na", "Na"))]))
        
        # pct, pca_reasons_missed, ooh_reasons_missed, lqas_reasons_missed, pca_awareness_source, pca_door_marking, lqas_result, pca_modality
        map_colors <- colors_coverage_bins[names(colors_coverage_bins) %in% data_cat]
        
        data <- data_pca %>%
          inner_join(report_borders_district,
                     by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
          sf::st_as_sf()
        
        
        no_sia_districts <- report_borders_district %>%
          anti_join(campaign_rpd %>%
                      filter(campaign_name == campaign_name()),
                    by=c("APMIS_RCODE" = "rcode",
                         "APMIS_PCODE" = "pcode",
                         "APMIS_DCODE" = "dcode"))
        
        no_apmis_data_districts <- report_borders_district %>%
          inner_join(campaign_rpd %>%
                       filter(campaign_name == campaign_name()),
                     by=c("APMIS_RCODE" = "rcode",
                          "APMIS_PCODE" = "pcode",
                          "APMIS_DCODE" = "dcode")) %>%
          anti_join(data %>% sf::st_drop_geometry(), by=c("APMIS_RCODE" = "rcode",
                                                          "APMIS_PCODE" = "pcode",
                                                          "APMIS_DCODE" = "dcode"))
        
        # Add value_cat manually to other data
        no_apmis_data_districts$value_cat <- "No APMIS Data"
        no_sia_districts$value_cat <- "No Campaign"
        
        # Bind all
        combined_data <- bind_rows(data, no_apmis_data_districts, no_sia_districts)
        
        # Now plot
        pca_map_plot <- ggplot() +
          geom_sf(data = combined_data, aes(fill = value_cat), alpha=0.7) +
          geom_sf(data=report_borders_province, fill=NA, color="#252525", linewidth = 0.6) +
          geom_sf(data=report_borders_region, fill=NA, color="black", linewidth = 0.8) +
          coord_sf(crs = st_crs(4326), datum = NA) +
          scale_fill_manual(
            values = c(map_colors, "No APMIS Data" = "#525252", "No Campaign" = "#d9d9d9"),
            breaks = c(names(map_colors), "No APMIS Data", "No Campaign"),
            name = legend_title
          ) +
          theme_void() +
          theme(
            legend.title = element_text(size=11),
            legend.text = element_text(size = 9),
            legend.position = "left"
          )
        } else{
          pca_map_plot <- NULL
        }
      
      if(vaccine_type == "IPV"){
        ## PCA Coverage Map IPV --------------------------------------------------------
        data_pca_ipv <- report_data()$postcampaign_data$district_indicators %>%
          filter(indicator == "pca_fm_coverage_ipv") %>%
          mutate(value_cat = case_when(value >= 0.95 ~ "95-100%",
                                       value >= 0.90 ~ "90-94%",
                                       value >= 0.85 ~ "85-89%",
                                       value < 0.85 ~ "<85%",
                                       TRUE ~ NA_character_)) %>%
          arrange(desc(value_cat))
        
        if (is_valid_df(data_pca_ipv) > 0){
          legend_title <- "PCA - Finger-Mark\nCoverage\n(4-59m, IPV)"
          
          report_borders_district <- report_borders_district()
          report_borders_province <- report_borders_district %>%
            group_by(APMIS_Region, APMIS_Province, APMIS_RCODE, APMIS_PCODE) %>%
            summarise(geometry = sf::st_union(geometry), .groups = "drop") 
          report_borders_region <- report_borders_district %>%
            group_by(APMIS_Region, APMIS_RCODE)  %>%
            summarise(geometry = sf::st_union(geometry), .groups = "drop")
          
          # Extract non-NA values from the specified variable
          data_cat <- sort(unique(data_pca_ipv[["value_cat"]][!is.na(data_pca_ipv[["value_cat"]]) & !(data_pca_ipv[["value_cat"]] %in% c("na", "Na"))]))
          
          # pct, pca_reasons_missed, ooh_reasons_missed, lqas_reasons_missed, pca_awareness_source, pca_door_marking, lqas_result, pca_modality
          map_colors <- colors_coverage_bins[names(colors_coverage_bins) %in% data_cat]
          
          data <- data_pca_ipv %>%
            inner_join(report_borders_district,
                       by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
            sf::st_as_sf()
          
          
          no_sia_districts <- report_borders_district %>%
            anti_join(campaign_rpd %>%
                        filter(campaign_name == campaign_name()),
                      by=c("APMIS_RCODE" = "rcode",
                           "APMIS_PCODE" = "pcode",
                           "APMIS_DCODE" = "dcode"))
          
          no_apmis_data_districts <- report_borders_district %>%
            inner_join(campaign_rpd %>%
                         filter(campaign_name == campaign_name()),
                       by=c("APMIS_RCODE" = "rcode",
                            "APMIS_PCODE" = "pcode",
                            "APMIS_DCODE" = "dcode")) %>%
            anti_join(data %>% sf::st_drop_geometry(), by=c("APMIS_RCODE" = "rcode",
                                                            "APMIS_PCODE" = "pcode",
                                                            "APMIS_DCODE" = "dcode"))
          
          # Add value_cat manually to other data
          no_apmis_data_districts$value_cat <- "No APMIS Data"
          no_sia_districts$value_cat <- "No Campaign"
          
          # Bind all
          combined_data <- bind_rows(data, no_apmis_data_districts, no_sia_districts)
          
          # Now plot
          pca_map_plot_ipv <- ggplot() +
            geom_sf(data = combined_data, aes(fill = value_cat), alpha=0.7) +
            geom_sf(data=report_borders_province, fill=NA, color="#252525", linewidth = 0.6) +
            geom_sf(data=report_borders_region, fill=NA, color="black", linewidth = 0.8) +
            coord_sf(crs = st_crs(4326), datum = NA) +
            scale_fill_manual(
              values = c(map_colors, "No APMIS Data" = "#525252", "No Campaign" = "#d9d9d9"),
              breaks = c(names(map_colors), "No APMIS Data", "No Campaign"),
              name = legend_title
            ) +
            theme_void() +
            theme(
              legend.title = element_text(size=11),
              legend.text = element_text(size = 9),
              legend.position = "left"
            )
        } else{
          pca_map_plot_ipv <- NULL
        }
      }
      
        
      # Create Title ------------------------------------------------------------
        title_text <- {
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
            paste0(campaign_name(), ": Post-Campaign Assessment Finger-Mark Coverage (0-59m, OPV)")
          } else {
            paste0(campaign_name(), ", ", loc, ": Post-Campaign Assessment Finger-Mark Coverage (0-59m, OPV)")
          }
        }
      
      if(vaccine_type == "IPV"){
        title_text_ipv <- {
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
            paste0(campaign_name(), ": Post-Campaign Assessment Finger-Mark Coverage (4-59m, IPV)")
          } else {
            paste0(campaign_name(), ", ", loc, ": Post-Campaign Assessment Finger-Mark Coverage (4-59m, IPV)")
          }
        }
      }
        
        # Create Table -----------------------------------------------------------
        table_data <-  {
          data <- if ("All" %in% region()) {
            report_data()$postcampaign_data$region_indicators %>%
              filter(indicator == "pca_fm_coverage_0_59m") %>%
              rename(Region = APMIS_Region) %>%
              select(Region, denominator, numerator, value) %>%
              arrange(desc(value))
          } else if ("All" %in% province()) {
            report_data()$postcampaign_data$province_indicators %>%
              filter(indicator == "pca_fm_coverage_0_59m") %>%
              rename(Province = APMIS_Province) %>%
              select(Province, denominator, numerator, value) %>%
              arrange(desc(value))
          } else if ("All" %in% district()) {
            report_data()$postcampaign_data$district_indicators %>%
              filter(indicator == "pca_fm_coverage_0_59m") %>%
              rename(District = APMIS_District) %>%
              select(District, denominator, numerator, value) %>%
              arrange(desc(value))
          } else {
            report_data()$postcampaign_data$cluster_indicators %>%
              filter(indicator == "pca_fm_coverage_0_59m") %>%
              mutate(Cluster = paste0(ccode,": ", clustername)) %>%
              select(Cluster, denominator, numerator, value) %>%
              arrange(desc(value))
          }
        }
        
      # Ensure numeric and percent formatting
      if (is_valid_df(table_data)){
        table_data <- table_data %>%
          mutate_at(c("numerator", "denominator", "value"), ~as.numeric(.)) %>%
          mutate(value = round(value, 3) * 100) %>%
          rename(
            "Children Seen" = denominator,
            "Finger-Marked" = numerator,
            "Coverage\n(% Finger-Marked)" = value
          )
        
        # Add total row
        group_col <- names(table_data)[1]
        total_row <- data.frame(
          setNames("Total", group_col),
          `Children Seen` = sum(table_data$`Children Seen`, na.rm = TRUE),
          `Finger-Marked` = sum(table_data$`Finger-Marked`, na.rm = TRUE),
          `Coverage\n(% Finger-Marked)` = round(100 * sum(table_data$`Finger-Marked`, na.rm = TRUE) / 
            sum(table_data$`Children Seen`, na.rm = TRUE),1),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        names(total_row) <- names(table_data)
        table_data <- bind_rows(total_row, table_data) 
        
        # Create styled flextable
        table <- table_data %>%
          flextable::flextable() %>%
          flextable::colformat_num(
            j = "Coverage\n(% Finger-Marked)",
            digits = 0,
            suffix = "%"
          ) %>%
          flextable::width(j = 1:4, width = c(1.6, 1.3, 1.3, 1.6)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(part = "header", bg = "#0D6938") %>%
          flextable::color(part = "header", color = "white") %>%
          flextable::bg(
            i = 1,
            bg = "#FBCB9A",
            part = "body"
          ) %>%
          flextable::bold(i = 1, bold = TRUE, part = "body") %>%
          flextable::fontsize(size = 9, part = "all")
      } else{
        table <- NULL
      }
      
      if(vaccine_type == "IPV"){
        # Create Table -----------------------------------------------------------
        table_data <-  {
          data <- if ("All" %in% region()) {
            report_data()$postcampaign_data$region_indicators %>%
              filter(indicator == "pca_fm_coverage_ipv") %>%
              rename(Region = APMIS_Region) %>%
              select(Region, denominator, numerator, value) %>%
              arrange(desc(value))
          } else if ("All" %in% province()) {
            report_data()$postcampaign_data$province_indicators %>%
              filter(indicator == "pca_fm_coverage_ipv") %>%
              rename(Province = APMIS_Province) %>%
              select(Province, denominator, numerator, value) %>%
              arrange(desc(value))
          } else if ("All" %in% district()) {
            report_data()$postcampaign_data$district_indicators %>%
              filter(indicator == "pca_fm_coverage_ipv") %>%
              rename(District = APMIS_District) %>%
              select(District, denominator, numerator, value) %>%
              arrange(desc(value))
          } else {
            report_data()$postcampaign_data$cluster_indicators %>%
              filter(indicator == "pca_fm_coverage_ipv") %>%
              mutate(Cluster = paste0(ccode,": ", clustername)) %>%
              select(Cluster, denominator, numerator, value) %>%
              arrange(desc(value))
          }
        }
        
        # Ensure numeric and percent formatting
        if (is_valid_df(table_data)){
          table_data <- table_data %>%
            mutate_at(c("numerator", "denominator", "value"), ~as.numeric(.)) %>%
            mutate(value = round(value, 3) * 100) %>%
            rename(
              "Children Seen" = denominator,
              "Finger-Marked" = numerator,
              "Coverage\n(% Finger-Marked)" = value
            )
          
          # Add total row
          group_col <- names(table_data)[1]
          total_row <- data.frame(
            setNames("Total", group_col),
            `Children Seen` = sum(table_data$`Children Seen`, na.rm = TRUE),
            `Finger-Marked` = sum(table_data$`Finger-Marked`, na.rm = TRUE),
            `Coverage\n(% Finger-Marked)` = round(100 * sum(table_data$`Finger-Marked`, na.rm = TRUE) / 
                                                    sum(table_data$`Children Seen`, na.rm = TRUE),1),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          names(total_row) <- names(table_data)
          table_data <- bind_rows(total_row, table_data) 
          
          # Create styled flextable
          table_ipv <- table_data %>%
            flextable::flextable() %>%
            flextable::colformat_num(
              j = "Coverage\n(% Finger-Marked)",
              digits = 0,
              suffix = "%"
            ) %>%
            flextable::width(j = 1:4, width = c(1.6, 1.3, 1.3, 1.6)) %>%
            flextable::set_table_properties(layout = "autofit") %>%
            flextable::theme_vanilla() %>%
            flextable::bg(part = "header", bg = "#0D6938") %>%
            flextable::color(part = "header", color = "white") %>%
            flextable::bg(
              i = 1,
              bg = "#FBCB9A",
              part = "body"
            ) %>%
            flextable::bold(i = 1, bold = TRUE, part = "body") %>%
            flextable::fontsize(size = 9, part = "all")
        } else{
          table <- NULL
        }
      }
        
        
        # Create Definition Text --------------------------------------------------
        definition_text <- "PCA Finger-Mark Coverage refers to the percent of children seen in the post-campaign assessment who were finger marked.\nThe target coverage threshold is 95%."
        
        # Export to PPTX ----------------------------------------------------------
        
      if(!is.null(pca_map_plot) && !is.null(pca_bar) && !is.null(table) && !is.null(pca_trend_plot) && !is.null(pca_pie)){
         my_pres <- my_pres %>%
          add_slide(layout = "template5", master = "Office Theme") %>%
          ph_with(value = pca_map_plot, location = ph_location_label(ph_label = "MapBox")) %>%
          ph_with(value = pca_bar, location = ph_location_label(ph_label = "BarBox")) %>%
          ph_with(value = table, location = ph_location_label(ph_label = "TableBox")) %>%
          ph_with(value = pca_trend_plot, location = ph_location_label(ph_label = "TrendBox")) %>%
          ph_with(value = pca_pie, location = ph_location_label(ph_label = "PieBox")) %>%
          ph_with(
            value = fpar(ftext(definition_text, fp_text(font.size = 9))),
            location = ph_location_label(ph_label = "TextBox")
          ) %>%
          ph_with(value = title_text, location = ph_location_label(ph_label = "Title Box")) %>%
          ph_with(
            value = fpar(
              ftext(paste0("Data Source: APMIS, extracted on ", Sys.Date()), fp_text(font.size = 7)),
              fp_p = fp_par(text.align = "right")
            ),
            location = ph_location_label(ph_label = "Footer Box")
          )
      }
      if(vaccine_type == "IPV"){
        if(!is.null(pca_map_plot_ipv) && !is.null(pca_bar_ipv) && !is.null(table_ipv) && !is.null(pca_trend_plot_ipv) && !is.null(pca_pie)){
          my_pres <- my_pres %>%
            add_slide(layout = "template5", master = "Office Theme") %>%
            ph_with(value = pca_map_plot_ipv, location = ph_location_label(ph_label = "MapBox")) %>%
            ph_with(value = pca_bar_ipv, location = ph_location_label(ph_label = "BarBox")) %>%
            ph_with(value = table_ipv, location = ph_location_label(ph_label = "TableBox")) %>%
            ph_with(value = pca_trend_plot_ipv, location = ph_location_label(ph_label = "TrendBox")) %>%
            ph_with(value = pca_pie, location = ph_location_label(ph_label = "PieBox")) %>%
            ph_with(
              value = fpar(ftext(definition_text, fp_text(font.size = 9))),
              location = ph_location_label(ph_label = "TextBox")
            ) %>%
            ph_with(value = title_text_ipv, location = ph_location_label(ph_label = "Title Box")) %>%
            ph_with(
              value = fpar(
                ftext(paste0("Data Source: APMIS, extracted on ", Sys.Date()), fp_text(font.size = 7)),
                fp_p = fp_par(text.align = "right")
              ),
              location = ph_location_label(ph_label = "Footer Box")
            )
        }
      }
        
        ## LQAS Result Map --------------------------------------------------------
        
        data_lqas <- report_data()$postcampaign_data$district_indicators %>%
          # data_lqas <- all_apmis_data_list$apmis_indicators$district_indicators %>%
          #   filter(campaign_name == "May NID 2025") %>%
          filter(indicator == "lqas_result") %>%
          mutate(value_cat  = factor(value, levels=c("All Lots Passed", "One or More Lots Failed", "Incomplete Data"))) %>%
          arrange(desc(value_cat))
        
        if (is_valid_df(data_lqas) ){
        legend_title <- "LQAS Result (OPV)"
        
        report_borders_district <- report_borders_district()
        report_borders_province <- report_borders_district %>%
          group_by(APMIS_Region, APMIS_Province, APMIS_RCODE, APMIS_PCODE) %>%
          summarise(geometry = sf::st_union(geometry), .groups = "drop") 
        report_borders_region <- report_borders_district %>%
          group_by(APMIS_Region, APMIS_RCODE)  %>%
          summarise(geometry = sf::st_union(geometry), .groups = "drop")
        
        # Extract non-NA values from the specified variable
        data_cat <- sort(unique(data_lqas[["value_cat"]][!is.na(data_lqas[["value_cat"]]) & !(data_lqas[["value_cat"]] %in% c("na", "Na"))]))
        
        # pct, pca_reasons_missed, ooh_reasons_missed, lqas_reasons_missed, pca_awareness_source, pca_door_marking, lqas_result, pca_modality
        map_colors <- colors_passfail_bins[names(colors_passfail_bins) %in% data_cat]
        
        data <- data_lqas %>%
          inner_join(report_borders_district,
                     by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
          sf::st_as_sf()
        
        
        no_sia_districts <- report_borders_district %>%
          anti_join(campaign_rpd %>%
                      filter(campaign_name == campaign_name()),
                    by=c("APMIS_RCODE" = "rcode",
                         "APMIS_PCODE" = "pcode",
                         "APMIS_DCODE" = "dcode"))
        
        no_apmis_data_districts <- report_borders_district %>%
          inner_join(campaign_rpd %>%
                       filter(campaign_name == campaign_name()),
                     by=c("APMIS_RCODE" = "rcode",
                          "APMIS_PCODE" = "pcode",
                          "APMIS_DCODE" = "dcode")) %>%
          anti_join(data %>% sf::st_drop_geometry(), by=c("APMIS_RCODE" = "rcode",
                                                          "APMIS_PCODE" = "pcode",
                                                          "APMIS_DCODE" = "dcode"))
        
        # Add value_cat manually to other data
        no_apmis_data_districts$value_cat <- "No APMIS Data"
        no_sia_districts$value_cat <- "No Campaign"
        
        # Bind all
        combined_data <- bind_rows(data, no_apmis_data_districts, no_sia_districts)
        
        # Now plot
        map_plot <- ggplot() +
          geom_sf(data = combined_data, aes(fill = value_cat), alpha=0.7) +
          geom_sf(data=report_borders_province, fill=NA, color="#252525", linewidth = 0.6) +
          geom_sf(data=report_borders_region, fill=NA, color="black", linewidth = 0.8) +
          coord_sf(crs = st_crs(4326), datum = NA) +
          scale_fill_manual(
            values = c(map_colors, "No APMIS Data" = "#525252", "No Campaign" = "#d9d9d9"),
            breaks = c(names(map_colors), "No APMIS Data", "No Campaign"),
            name = legend_title
          ) +
          theme_void() +
          theme(
            legend.title = element_text(size=11),
            legend.text = element_text(size = 9),
            legend.position = "left"
          )
        } else{
          map_plot <- NULL
        }
        
        # Create Title ------------------------------------------------------------
        title_text <- {
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
            paste0(campaign_name(), ": LQAS Result (OPV)")
          } else {
            paste0(campaign_name(), ", ", loc, ": LQAS Result (OPV)")
          }
        }
        
        # Create Table -----------------------------------------------------------
        table_data <-  {
          data <- if ("All" %in% region()) {
            report_data()$postcampaign_data$region_indicators %>%
              filter(indicator == "lqas_result") %>%
              rename(Region = APMIS_Region) %>%
              select(Region, denominator, numerator, value) %>%
              arrange(desc(value))
          } else if ("All" %in% province()) {
            report_data()$postcampaign_data$province_indicators %>%
              filter(indicator == "lqas_result") %>%
              rename(Province = APMIS_Province) %>%
              select(Province, denominator, numerator, value) %>%
              arrange(desc(value))
          } else if ("All" %in% district()) {
            report_data()$postcampaign_data$district_indicators %>%
              filter(indicator == "lqas_result") %>%
              rename(District = APMIS_District) %>%
              select(District, denominator, numerator) %>%
              rowwise() %>%
              mutate(value = numerator/denominator) %>%
              ungroup() %>%
              arrange(desc(value))
          } else {
            report_data()$postcampaign_data$district_indicators %>%
              filter(indicator == "lqas_result") %>%
              rename(District = APMIS_District) %>%
              select(District, denominator, numerator) %>%
              rowwise() %>%
              mutate(value = numerator/denominator) %>%
              ungroup() %>%
              arrange(desc(value))
          }
        }
      if (is_valid_df(table_data)){  
      table_data <- table_data %>%
          mutate_at(c("numerator", "denominator", "value"), ~as.numeric(.))
        
        # Dynamically get the name of the first column (e.g., "District", "Province", etc.)
        group_col <- names(table_data)[1]
        
        # Create total row using data.frame()
        total_row <- data.frame(
          setNames("Total", group_col),
          denominator = sum(table_data$denominator, na.rm = TRUE),
          numerator = sum(table_data$numerator, na.rm = TRUE),
          value = sum(table_data$numerator, na.rm = TRUE) / sum(table_data$denominator, na.rm = TRUE),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        
        # Append to original
        names(total_row) <- names(table_data)
        table_data <- bind_rows(total_row, table_data)
        table_data <- table_data %>%
          mutate_at(c("numerator", "denominator", "value"), ~as.numeric(.)) %>%
          mutate(value = round(value,3) * 100) %>%
          rename("Lots Assessed" = denominator,
                 "Lots Passed" = numerator,
                 "Percent of\nLots Passed (OPV)" = value) 
        
        table <- table_data %>%
          flextable::flextable() %>%
          flextable::colformat_num(
            j = "Percent of\nLots Passed (OPV)",
            digits = 0,
            suffix = "%"
          ) %>%
          flextable::width(j = 1:4, width = c(1.6, 1.3, 1.3, 1.6)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(part = "header", bg = "#0D6938") %>%
          flextable::color(part = "header", color = "white") %>%
          flextable::bg(
            i = 1,  # last row is total
            bg = "#FBCB9A",        # light orange
            part = "body"
          ) %>%
          flextable::bold(i = 1, bold = TRUE, part = "body") %>%
          flextable::fontsize(size = 9, part = "all")
      } else{
        table <- NULL
      }
      
      if(vaccine_type == "IPV"){
        ## LQAS Result Map --------------------------------------------------------
        
        data_lqas_ipv <- report_data()$postcampaign_data$district_indicators %>%
          # data_lqas <- all_apmis_data_list$apmis_indicators$district_indicators %>%
          #   filter(campaign_name == "May NID 2025") %>%
          filter(indicator == "lqas_fipv_result") %>%
          mutate(value_cat  = factor(value, levels=c("All Lots Passed", "One or More Lots Failed", "Incomplete Data"))) %>%
          arrange(desc(value_cat))
        
        if (is_valid_df(data_lqas_ipv) ){
          legend_title <- "LQAS Result (IPV)"
          
          report_borders_district <- report_borders_district()
          report_borders_province <- report_borders_district %>%
            group_by(APMIS_Region, APMIS_Province, APMIS_RCODE, APMIS_PCODE) %>%
            summarise(geometry = sf::st_union(geometry), .groups = "drop") 
          report_borders_region <- report_borders_district %>%
            group_by(APMIS_Region, APMIS_RCODE)  %>%
            summarise(geometry = sf::st_union(geometry), .groups = "drop")
          
          # Extract non-NA values from the specified variable
          data_cat <- sort(unique(data_lqas_ipv[["value_cat"]][!is.na(data_lqas_ipv[["value_cat"]]) & !(data_lqas_ipv[["value_cat"]] %in% c("na", "Na"))]))
          
          # pct, pca_reasons_missed, ooh_reasons_missed, lqas_reasons_missed, pca_awareness_source, pca_door_marking, lqas_result, pca_modality
          map_colors <- colors_passfail_bins[names(colors_passfail_bins) %in% data_cat]
          
          data <- data_lqas_ipv %>%
            inner_join(report_borders_district,
                       by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
            sf::st_as_sf()
          
          
          no_sia_districts <- report_borders_district %>%
            anti_join(campaign_rpd %>%
                        filter(campaign_name == campaign_name()),
                      by=c("APMIS_RCODE" = "rcode",
                           "APMIS_PCODE" = "pcode",
                           "APMIS_DCODE" = "dcode"))
          
          no_apmis_data_districts <- report_borders_district %>%
            inner_join(campaign_rpd %>%
                         filter(campaign_name == campaign_name()),
                       by=c("APMIS_RCODE" = "rcode",
                            "APMIS_PCODE" = "pcode",
                            "APMIS_DCODE" = "dcode")) %>%
            anti_join(data %>% sf::st_drop_geometry(), by=c("APMIS_RCODE" = "rcode",
                                                            "APMIS_PCODE" = "pcode",
                                                            "APMIS_DCODE" = "dcode"))
          
          # Add value_cat manually to other data
          no_apmis_data_districts$value_cat <- "No APMIS Data"
          no_sia_districts$value_cat <- "No Campaign"
          
          # Bind all
          combined_data <- bind_rows(data, no_apmis_data_districts, no_sia_districts)
          
          # Now plot
          map_plot_ipv <- ggplot() +
            geom_sf(data = combined_data, aes(fill = value_cat), alpha=0.7) +
            geom_sf(data=report_borders_province, fill=NA, color="#252525", linewidth = 0.6) +
            geom_sf(data=report_borders_region, fill=NA, color="black", linewidth = 0.8) +
            coord_sf(crs = st_crs(4326), datum = NA) +
            scale_fill_manual(
              values = c(map_colors, "No APMIS Data" = "#525252", "No Campaign" = "#d9d9d9"),
              breaks = c(names(map_colors), "No APMIS Data", "No Campaign"),
              name = legend_title
            ) +
            theme_void() +
            theme(
              legend.title = element_text(size=11),
              legend.text = element_text(size = 9),
              legend.position = "left"
            )
        } else{
          map_plot_ipv <- NULL
        }
        
        # Create Title ------------------------------------------------------------
        title_text_ipv <- {
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
            paste0(campaign_name(), ": LQAS Result (IPV)")
          } else {
            paste0(campaign_name(), ", ", loc, ": LQAS Result (IPV)")
          }
        }
        
        # Create Table -----------------------------------------------------------
        table_data <-  {
          data <- if ("All" %in% region()) {
            report_data()$postcampaign_data$region_indicators %>%
              filter(indicator == "lqas_fipv_result") %>%
              rename(Region = APMIS_Region) %>%
              select(Region, denominator, numerator, value) %>%
              arrange(desc(value))
          } else if ("All" %in% province()) {
            report_data()$postcampaign_data$province_indicators %>%
              filter(indicator == "lqas_fipv_result") %>%
              rename(Province = APMIS_Province) %>%
              select(Province, denominator, numerator, value) %>%
              arrange(desc(value))
          } else if ("All" %in% district()) {
            report_data()$postcampaign_data$district_indicators %>%
              filter(indicator == "lqas_fipv_result") %>%
              rename(District = APMIS_District) %>%
              select(District, denominator, numerator) %>%
              rowwise() %>%
              mutate(value = numerator/denominator) %>%
              ungroup() %>%
              arrange(desc(value))
          } else {
            report_data()$postcampaign_data$district_indicators %>%
              filter(indicator == "lqas_fipv_result") %>%
              rename(District = APMIS_District) %>%
              select(District, denominator, numerator) %>%
              rowwise() %>%
              mutate(value = numerator/denominator) %>%
              ungroup() %>%
              arrange(desc(value))
          }
        }
        if (is_valid_df(table_data)){  
          table_data <- table_data %>%
            mutate_at(c("numerator", "denominator", "value"), ~as.numeric(.))
          
          # Dynamically get the name of the first column (e.g., "District", "Province", etc.)
          group_col <- names(table_data)[1]
          
          # Create total row using data.frame()
          total_row <- data.frame(
            setNames("Total", group_col),
            denominator = sum(table_data$denominator, na.rm = TRUE),
            numerator = sum(table_data$numerator, na.rm = TRUE),
            value = sum(table_data$numerator, na.rm = TRUE) / sum(table_data$denominator, na.rm = TRUE),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          
          # Append to original
          names(total_row) <- names(table_data)
          table_data <- bind_rows(total_row, table_data)
          table_data <- table_data %>%
            mutate_at(c("numerator", "denominator", "value"), ~as.numeric(.)) %>%
            mutate(value = round(value,3) * 100) %>%
            rename("Lots Assessed" = denominator,
                   "Lots Passed" = numerator,
                   "Percent of\nLots Passed (IPV)" = value) 
          
          table_ipv <- table_data %>%
            flextable::flextable() %>%
            flextable::colformat_num(
              j = "Percent of\nLots Passed (IPV)",
              digits = 0,
              suffix = "%"
            ) %>%
            flextable::width(j = 1:4, width = c(1.6, 1.3, 1.3, 1.6)) %>%
            flextable::set_table_properties(layout = "autofit") %>%
            flextable::theme_vanilla() %>%
            flextable::bg(part = "header", bg = "#0D6938") %>%
            flextable::color(part = "header", color = "white") %>%
            flextable::bg(
              i = 1,  # last row is total
              bg = "#FBCB9A",        # light orange
              part = "body"
            ) %>%
            flextable::bold(i = 1, bold = TRUE, part = "body") %>%
            flextable::fontsize(size = 9, part = "all")
        } else{
          table_ipv <- NULL
        }
      }
        
      
        # Create Definition Text --------------------------------------------------
        definition_text <- "Lot Quality Assurance Sampling (LQAS) classifies areas (Lots) as Pass or Fail using a sample of 60 children observed for finger-marking after a campaign:\nA lot passes if ≤3 are unmarked, and fails if ≥4 are unmarked."
       
      if(!is.null(map_plot) && !is.null(table) && !is.null(lqas_trend_plot) && !is.null(lqas_pie)){ 
        my_pres <- my_pres %>%
          add_slide(layout = "template5", master = "Office Theme") %>%
          ph_with(value = map_plot, location = ph_location_label(ph_label = "MapBox")) %>%
          ph_with(value = table, location = ph_location_label(ph_label = "TableBox")) %>%
          ph_with(value = lqas_trend_plot, location = ph_location_label(ph_label = "TrendBox")) %>%
          ph_with(value = lqas_pie, location = ph_location_label(ph_label = "PieBox")) %>%
          ph_with(
            value = fpar(ftext(definition_text, fp_text(font.size = 9))),
            location = ph_location_label(ph_label = "TextBox")
          ) %>%
          ph_with(value = title_text, location = ph_location_label(ph_label = "Title Box")) %>%
          ph_with(
            value = fpar(
              ftext(paste0("Data Source: APMIS, extracted on ", Sys.Date()), fp_text(font.size = 7)),
              fp_p = fp_par(text.align = "right")
            ),
            location = ph_location_label(ph_label = "Footer Box")
          )
      }
      if(vaccine_type == "IPV"){
        if(!is.null(map_plot_ipv) && !is.null(table_ipv) && !is.null(lqas_trend_plot_ipv) && !is.null(lqas_pie)){ 
          my_pres <- my_pres %>%
            add_slide(layout = "template5", master = "Office Theme") %>%
            ph_with(value = map_plot_ipv, location = ph_location_label(ph_label = "MapBox")) %>%
            ph_with(value = table_ipv, location = ph_location_label(ph_label = "TableBox")) %>%
            ph_with(value = lqas_trend_plot_ipv, location = ph_location_label(ph_label = "TrendBox")) %>%
            ph_with(value = lqas_pie, location = ph_location_label(ph_label = "PieBox")) %>%
            ph_with(
              value = fpar(ftext(definition_text, fp_text(font.size = 9))),
              location = ph_location_label(ph_label = "TextBox")
            ) %>%
            ph_with(value = title_text_ipv, location = ph_location_label(ph_label = "Title Box")) %>%
            ph_with(
              value = fpar(
                ftext(paste0("Data Source: APMIS, extracted on ", Sys.Date()), fp_text(font.size = 7)),
                fp_p = fp_par(text.align = "right")
              ),
              location = ph_location_label(ph_label = "Footer Box")
            )
        }
      }
        
        ## OOH Survey Result Map --------------------------------------------------------
        
        data_ooh <- report_data()$postcampaign_data$district_indicators %>%
          # data_ooh <- all_apmis_data_list$apmis_indicators$district_indicators %>%
          #   filter(campaign_name == "May NID 2025") %>%
          filter(indicator == "ooh_fm_coverage") %>%
          mutate(value_cat = case_when(value >= 0.95 ~ "95-100%",
                                       value >= 0.90 ~ "90-94%",
                                       value >= 0.85 ~ "85-89%",
                                       value < 0.85 ~ "<85%",
                                       TRUE ~ NA_character_)) %>%
          arrange(desc(value_cat))
        if (is_valid_df(data_ooh)){
        legend_title <- "Out-of-house\nSurvey - FM\nCoverage\n(0-59m, OPV)"
        
        report_borders_district <- report_borders_district()
        report_borders_province <- report_borders_district %>%
          group_by(APMIS_Region, APMIS_Province, APMIS_RCODE, APMIS_PCODE) %>%
          summarise(geometry = sf::st_union(geometry), .groups = "drop") 
        report_borders_region <- report_borders_district %>%
          group_by(APMIS_Region, APMIS_RCODE)  %>%
          summarise(geometry = sf::st_union(geometry), .groups = "drop")
        
        # Extract non-NA values from the specified variable
        data_cat <- sort(unique(data_ooh[["value_cat"]][!is.na(data_ooh[["value_cat"]]) & !(data_ooh[["value_cat"]] %in% c("na", "Na"))]))
        
        # pct, pca_reasons_missed, ooh_reasons_missed, lqas_reasons_missed, pca_awareness_source, pca_door_marking, lqas_result, pca_modality
        map_colors <- colors_coverage_bins[names(colors_coverage_bins) %in% data_cat]
        
        data <- data_ooh %>%
          inner_join(report_borders_district,
                     by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
          sf::st_as_sf()
        
        
        no_sia_districts <- report_borders_district %>%
          anti_join(campaign_rpd %>%
                      filter(campaign_name == campaign_name()),
                    by=c("APMIS_RCODE" = "rcode",
                         "APMIS_PCODE" = "pcode",
                         "APMIS_DCODE" = "dcode"))
        
        no_apmis_data_districts <- report_borders_district %>%
          inner_join(campaign_rpd %>%
                       filter(campaign_name == campaign_name()),
                     by=c("APMIS_RCODE" = "rcode",
                          "APMIS_PCODE" = "pcode",
                          "APMIS_DCODE" = "dcode")) %>%
          anti_join(data %>% sf::st_drop_geometry(), by=c("APMIS_RCODE" = "rcode",
                                                          "APMIS_PCODE" = "pcode",
                                                          "APMIS_DCODE" = "dcode"))
        
        # Add value_cat manually to other data
        no_apmis_data_districts$value_cat <- "No APMIS Data"
        no_sia_districts$value_cat <- "No Campaign"
        
        # Bind all
        combined_data <- bind_rows(data, no_apmis_data_districts, no_sia_districts)
        
        # Now plot
        map_plot <- ggplot() +
          geom_sf(data = combined_data, aes(fill = value_cat), alpha=0.7) +
          geom_sf(data=report_borders_province, fill=NA, color="#252525", linewidth = 0.6) +
          geom_sf(data=report_borders_region, fill=NA, color="black", linewidth = 0.8) +
          coord_sf(crs = st_crs(4326), datum = NA) +
          scale_fill_manual(
            values = c(map_colors, "No APMIS Data" = "#525252", "No Campaign" = "#d9d9d9"),
            breaks = c(names(map_colors), "No APMIS Data", "No Campaign"),
            name = legend_title
          ) +
          theme_void() +
          theme(
            legend.title = element_text(size=11),
            legend.text = element_text(size = 9),
            legend.position = "left"
          )
        } else{
          map_plot <- NULL
        }
        
        # Create Title ------------------------------------------------------------
        title_text <- {
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
            paste0(campaign_name(), ": Out-of-House Finger-Mark Survey Coverage (OPV)")
          } else {
            paste0(campaign_name(), ", ", loc, ": Out-of-House Finger-Mark Survey Coverage (OPV)")
          }
        }
        
        # Create Table -----------------------------------------------------------
        table_data <-  {
          data <- if ("All" %in% region()) {
            report_data()$postcampaign_data$region_indicators %>%
              filter(indicator == "ooh_fm_coverage") %>%
              rename(Region = APMIS_Region) %>%
              select(Region, denominator, numerator, value) %>%
              arrange(desc(value))
          } else if ("All" %in% province()) {
            report_data()$postcampaign_data$province_indicators %>%
              filter(indicator == "ooh_fm_coverage") %>%
              rename(Province = APMIS_Province) %>%
              select(Province, denominator, numerator, value) %>%
              arrange(desc(value))
          } else if ("All" %in% district()) {
            report_data()$postcampaign_data$district_indicators %>%
              filter(indicator == "ooh_fm_coverage") %>%
              rename(District = APMIS_District) %>%
              select(District, denominator, numerator, value) %>%
              arrange(desc(value))
          } else {
            report_data()$postcampaign_data$district_indicators %>%
              filter(indicator == "ooh_fm_coverage") %>%
              rename(District = APMIS_District) %>%
              select(District, denominator, numerator, value) %>%
              arrange(desc(value))
          }
        }
        
      if (is_valid_df(table_data)){
        # Convert to numeric and rename
        table_data <- table_data %>%
          mutate_at(c("numerator", "denominator", "value"), ~as.numeric(.)) %>%
          mutate(value = round(value, 3) * 100) %>%
          rename(
            "Children Seen" = denominator,
            "Finger-Marked" = numerator,
            "Coverage\n(% Finger-Marked, OPV)" = value
          )
        
        # Add total row
        group_col <- names(table_data)[1]
        total_row <- data.frame(
          setNames("Total", group_col),
          `Children Seen` = sum(table_data$`Children Seen`, na.rm = TRUE),
          `Finger-Marked` = sum(table_data$`Finger-Marked`, na.rm = TRUE),
          `Coverage\n(% Finger-Marked, OPV)` = round(100 * sum(table_data$`Finger-Marked`, na.rm = TRUE) /
            sum(table_data$`Children Seen`, na.rm = TRUE),1),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        names(total_row) <- names(table_data)
        table_data <- bind_rows(total_row, table_data)
        
        # Create styled flextable
        table <- table_data %>%
          flextable::flextable() %>%
          flextable::colformat_num(
            j = "Coverage\n(% Finger-Marked, OPV)",
            digits = 0,
            suffix = "%"
          ) %>%
          flextable::width(j = 1:4, width = c(1.6, 1.3, 1.3, 1.6)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(part = "header", bg = "#0D6938") %>%
          flextable::color(part = "header", color = "white") %>%
          flextable::bg(i = 1, bg = "#FBCB9A", part = "body") %>%
          flextable::bold(i = 1, bold = TRUE, part = "body") %>%
          flextable::fontsize(size = 9, part = "all")
      } else{
        table <- NULL
      }
      
      if(vaccine_type == "IPV"){
        ## OOH Survey Result Map --------------------------------------------------------
        
        data_ooh_ipv <- report_data()$postcampaign_data$district_indicators %>%
          # data_ooh <- all_apmis_data_list$apmis_indicators$district_indicators %>%
          #   filter(campaign_name == "May NID 2025") %>%
          filter(indicator == "ooh_fm_coverage_ipv") %>%
          mutate(value_cat = case_when(value >= 0.95 ~ "95-100%",
                                       value >= 0.90 ~ "90-94%",
                                       value >= 0.85 ~ "85-89%",
                                       value < 0.85 ~ "<85%",
                                       TRUE ~ NA_character_)) %>%
          arrange(desc(value_cat))
        if (is_valid_df(data_ooh_ipv)){
          legend_title <- "Out-of-house\nSurvey - FM\nCoverage\n(0-59m, OPV)"
          
          report_borders_district <- report_borders_district()
          report_borders_province <- report_borders_district %>%
            group_by(APMIS_Region, APMIS_Province, APMIS_RCODE, APMIS_PCODE) %>%
            summarise(geometry = sf::st_union(geometry), .groups = "drop") 
          report_borders_region <- report_borders_district %>%
            group_by(APMIS_Region, APMIS_RCODE)  %>%
            summarise(geometry = sf::st_union(geometry), .groups = "drop")
          
          # Extract non-NA values from the specified variable
          data_cat <- sort(unique(data_ooh_ipv[["value_cat"]][!is.na(data_ooh_ipv[["value_cat"]]) & !(data_ooh_ipv[["value_cat"]] %in% c("na", "Na"))]))
          
          # pct, pca_reasons_missed, ooh_reasons_missed, lqas_reasons_missed, pca_awareness_source, pca_door_marking, lqas_result, pca_modality
          map_colors <- colors_coverage_bins[names(colors_coverage_bins) %in% data_cat]
          
          data <- data_ooh_ipv %>%
            inner_join(report_borders_district,
                       by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
            sf::st_as_sf()
          
          
          no_sia_districts <- report_borders_district %>%
            anti_join(campaign_rpd %>%
                        filter(campaign_name == campaign_name()),
                      by=c("APMIS_RCODE" = "rcode",
                           "APMIS_PCODE" = "pcode",
                           "APMIS_DCODE" = "dcode"))
          
          no_apmis_data_districts <- report_borders_district %>%
            inner_join(campaign_rpd %>%
                         filter(campaign_name == campaign_name()),
                       by=c("APMIS_RCODE" = "rcode",
                            "APMIS_PCODE" = "pcode",
                            "APMIS_DCODE" = "dcode")) %>%
            anti_join(data %>% sf::st_drop_geometry(), by=c("APMIS_RCODE" = "rcode",
                                                            "APMIS_PCODE" = "pcode",
                                                            "APMIS_DCODE" = "dcode"))
          
          # Add value_cat manually to other data
          no_apmis_data_districts$value_cat <- "No APMIS Data"
          no_sia_districts$value_cat <- "No Campaign"
          
          # Bind all
          combined_data <- bind_rows(data, no_apmis_data_districts, no_sia_districts)
          
          # Now plot
          map_plot_ipv <- ggplot() +
            geom_sf(data = combined_data, aes(fill = value_cat), alpha=0.7) +
            geom_sf(data=report_borders_province, fill=NA, color="#252525", linewidth = 0.6) +
            geom_sf(data=report_borders_region, fill=NA, color="black", linewidth = 0.8) +
            coord_sf(crs = st_crs(4326), datum = NA) +
            scale_fill_manual(
              values = c(map_colors, "No APMIS Data" = "#525252", "No Campaign" = "#d9d9d9"),
              breaks = c(names(map_colors), "No APMIS Data", "No Campaign"),
              name = legend_title
            ) +
            theme_void() +
            theme(
              legend.title = element_text(size=11),
              legend.text = element_text(size = 9),
              legend.position = "left"
            )
        } else{
          map_plot_ipv <- NULL
        }
        
        # Create Title ------------------------------------------------------------
        title_text_ipv <- {
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
            paste0(campaign_name(), ": Out-of-House Finger-Mark Survey Coverage (IPV)")
          } else {
            paste0(campaign_name(), ", ", loc, ": Out-of-House Finger-Mark Survey Coverage (IPV)")
          }
        }
        
        # Create Table -----------------------------------------------------------
        table_data <-  {
          data <- if ("All" %in% region()) {
            report_data()$postcampaign_data$region_indicators %>%
              filter(indicator == "ooh_fm_coverage_ipv") %>%
              rename(Region = APMIS_Region) %>%
              select(Region, denominator, numerator, value) %>%
              arrange(desc(value))
          } else if ("All" %in% province()) {
            report_data()$postcampaign_data$province_indicators %>%
              filter(indicator == "ooh_fm_coverage_ipv") %>%
              rename(Province = APMIS_Province) %>%
              select(Province, denominator, numerator, value) %>%
              arrange(desc(value))
          } else if ("All" %in% district()) {
            report_data()$postcampaign_data$district_indicators %>%
              filter(indicator == "ooh_fm_coverage_ipv") %>%
              rename(District = APMIS_District) %>%
              select(District, denominator, numerator, value) %>%
              arrange(desc(value))
          } else {
            report_data()$postcampaign_data$district_indicators %>%
              filter(indicator == "ooh_fm_coverage_ipv") %>%
              rename(District = APMIS_District) %>%
              select(District, denominator, numerator, value) %>%
              arrange(desc(value))
          }
        }
        
        if (is_valid_df(table_data)){
          # Convert to numeric and rename
          table_data <- table_data %>%
            mutate_at(c("numerator", "denominator", "value"), ~as.numeric(.)) %>%
            mutate(value = round(value, 3) * 100) %>%
            rename(
              "Children Seen" = denominator,
              "Finger-Marked" = numerator,
              "Coverage\n(% Finger-Marked, IPV)" = value
            )
          
          # Add total row
          group_col <- names(table_data)[1]
          total_row <- data.frame(
            setNames("Total", group_col),
            `Children Seen` = sum(table_data$`Children Seen`, na.rm = TRUE),
            `Finger-Marked` = sum(table_data$`Finger-Marked`, na.rm = TRUE),
            `Coverage\n(% Finger-Marked, IPV)` = round(100 * sum(table_data$`Finger-Marked`, na.rm = TRUE) /
                                                    sum(table_data$`Children Seen`, na.rm = TRUE),1),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          names(total_row) <- names(table_data)
          table_data <- bind_rows(total_row, table_data)
          
          # Create styled flextable
          table_ipv <- table_data %>%
            flextable::flextable() %>%
            flextable::colformat_num(
              j = "Coverage\n(% Finger-Marked, IPV)",
              digits = 0,
              suffix = "%"
            ) %>%
            flextable::width(j = 1:4, width = c(1.6, 1.3, 1.3, 1.6)) %>%
            flextable::set_table_properties(layout = "autofit") %>%
            flextable::theme_vanilla() %>%
            flextable::bg(part = "header", bg = "#0D6938") %>%
            flextable::color(part = "header", color = "white") %>%
            flextable::bg(i = 1, bg = "#FBCB9A", part = "body") %>%
            flextable::bold(i = 1, bold = TRUE, part = "body") %>%
            flextable::fontsize(size = 9, part = "all")
        } else{
          table_ipv <- NULL
        }
      }
      
        # Create Definition Text --------------------------------------------------
        definition_text <- "The Out-of-House Finger-Mark Survey is conducted among a convenience sample of children seen in public places.\nCoverage is the percent of children seen who were finger-marked, and the target threshold is 95%."
        
      if(!is.null(map_plot) && !is.null(table) && !is.null(ooh_trend_plot) && !is.null(ooh_bar) && !is.null(ooh_pie)){
        my_pres <- my_pres %>%
          add_slide(layout = "template5", master = "Office Theme") %>%
          ph_with(value = map_plot, location = ph_location_label(ph_label = "MapBox")) %>%
          ph_with(value = table, location = ph_location_label(ph_label = "TableBox")) %>%
          ph_with(value = ooh_trend_plot, location = ph_location_label(ph_label = "TrendBox")) %>%
          ph_with(value = ooh_bar, location = ph_location_label(ph_label = "BarBox")) %>%
          ph_with(value = ooh_pie, location = ph_location_label(ph_label = "PieBox")) %>%
          ph_with(
            value = fpar(ftext(definition_text, fp_text(font.size = 9))),
            location = ph_location_label(ph_label = "TextBox")
          ) %>%
          ph_with(value = title_text, location = ph_location_label(ph_label = "Title Box")) %>%
          ph_with(
            value = fpar(
              ftext(paste0("Data Source: APMIS, extracted on ", Sys.Date()), fp_text(font.size = 7)),
              fp_p = fp_par(text.align = "right")
            ),
            location = ph_location_label(ph_label = "Footer Box")
          )
      }
      if(vaccine_type == "IPV"){
        if(!is.null(map_plot_ipv) && !is.null(table_ipv) && !is.null(ooh_trend_plot_ipv) && !is.null(ooh_bar_ipv) && !is.null(ooh_pie)){
          my_pres <- my_pres %>%
            add_slide(layout = "template5", master = "Office Theme") %>%
            ph_with(value = map_plot_ipv, location = ph_location_label(ph_label = "MapBox")) %>%
            ph_with(value = table_ipv, location = ph_location_label(ph_label = "TableBox")) %>%
            ph_with(value = ooh_trend_plot_ipv, location = ph_location_label(ph_label = "TrendBox")) %>%
            ph_with(value = ooh_bar_ipv, location = ph_location_label(ph_label = "BarBox")) %>%
            ph_with(value = ooh_pie, location = ph_location_label(ph_label = "PieBox")) %>%
            ph_with(
              value = fpar(ftext(definition_text, fp_text(font.size = 9))),
              location = ph_location_label(ph_label = "TextBox")
            ) %>%
            ph_with(value = title_text_ipv, location = ph_location_label(ph_label = "Title Box")) %>%
            ph_with(
              value = fpar(
                ftext(paste0("Data Source: APMIS, extracted on ", Sys.Date()), fp_text(font.size = 7)),
                fp_p = fp_par(text.align = "right")
              ),
              location = ph_location_label(ph_label = "Footer Box")
            )
        }
      }
        ## Admin Coverage  --------------------------------------------------------
        
        data_admin <- report_data()$admin_data$district %>%
        # data_admin <- all_apmis_data_list$apmis_admin_data$district %>%
        # filter(campaign_name == "May NID 2025") %>%
          filter(age_group %in% c("0-59 Months", "4-59 Months")) %>%
          filter(vaccine_type == "OPV") %>%
          select(campaign_name, APMIS_Region,  APMIS_Province, APMIS_District,  rcode, pcode, dcode, target_population, total_vaccinated, total_coverage) %>%
          rename(numerator = total_vaccinated,
                 denominator = target_population,
                 value = total_coverage) %>%
          mutate(value_cat = case_when(value > 1 ~ ">100%",
                                       value >= 0.95 ~ "95-100%",
                                       value >= 0.90 ~ "90-94%",
                                       value >= 0.85 ~ "85-89%",
                                       value < 0.85 ~ "<85%",
                                       TRUE ~ NA_character_)) %>%
          arrange(desc(value_cat))
        
      if (is_valid_df(data_admin)){
        legend_title <- "Admin Coverage\n(0-59m, OPV)"
        
        report_borders_district <- report_borders_district()
        report_borders_province <- report_borders_district %>%
          group_by(APMIS_Region, APMIS_Province, APMIS_RCODE, APMIS_PCODE) %>%
          summarise(geometry = sf::st_union(geometry), .groups = "drop") 
        report_borders_region <- report_borders_district %>%
          group_by(APMIS_Region, APMIS_RCODE)  %>%
          summarise(geometry = sf::st_union(geometry), .groups = "drop")
        
        # Extract non-NA values from the specified variable
        data_cat <- sort(unique(data_admin[["value_cat"]][!is.na(data_admin[["value_cat"]]) & !(data_admin[["value_cat"]] %in% c("na", "Na"))]))
        
        # pct, pca_reasons_missed, ooh_reasons_missed, lqas_reasons_missed, pca_awareness_source, pca_door_marking, lqas_result, pca_modality
        map_colors <- colors_coverage_bins2[names(colors_coverage_bins2) %in% data_cat]
        
        data <- data_admin %>%
          inner_join(report_borders_district,
                     by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
          sf::st_as_sf()
        
        
        no_sia_districts <- report_borders_district %>%
          anti_join(campaign_rpd %>%
                      filter(campaign_name == campaign_name()),
                    by=c("APMIS_RCODE" = "rcode",
                         "APMIS_PCODE" = "pcode",
                         "APMIS_DCODE" = "dcode"))
        
        no_apmis_data_districts <- report_borders_district %>%
          inner_join(campaign_rpd %>%
                       filter(campaign_name == campaign_name()),
                     by=c("APMIS_RCODE" = "rcode",
                          "APMIS_PCODE" = "pcode",
                          "APMIS_DCODE" = "dcode")) %>%
          anti_join(data %>% sf::st_drop_geometry(), by=c("APMIS_RCODE" = "rcode",
                                                          "APMIS_PCODE" = "pcode",
                                                          "APMIS_DCODE" = "dcode"))
        
        # Add value_cat manually to other data
        no_apmis_data_districts$value_cat <- "No APMIS Data"
        no_sia_districts$value_cat <- "No Campaign"
        
        # Bind all
        combined_data <- bind_rows(data, no_apmis_data_districts, no_sia_districts)
        
        # Now plot
        map_plot <- ggplot() +
          geom_sf(data = combined_data, aes(fill = value_cat), alpha=0.7) +
          geom_sf(data=report_borders_province, fill=NA, color="#252525", linewidth = 0.6) +
          geom_sf(data=report_borders_region, fill=NA, color="black", linewidth = 0.8) +
          coord_sf(crs = st_crs(4326), datum = NA) +
          scale_fill_manual(
            values = c(map_colors, "No APMIS Data" = "#525252", "No Campaign" = "#d9d9d9"),
            breaks = c(names(map_colors), "No APMIS Data", "No Campaign"),
            name = legend_title
          ) +
          theme_void() +
          theme(
            legend.title = element_text(size=11),
            legend.text = element_text(size = 9),
            legend.position = "left"
          )
      } else{
        map_plot <- NULL
      }
        
        # Create Title ------------------------------------------------------------
        title_text <- {
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
            paste0(campaign_name(), ": Admin Coverage (OPV)")
          } else {
            paste0(campaign_name(), ", ", loc, ": Admin Coverage (OPV)")
          }
        }
        
        # Create Table -----------------------------------------------------------
        table_data <-  {
          data <- if ("All" %in% region()) {
            report_data()$admin_data$region %>%
              filter(age_group == "0-59 Months") %>%
              select(APMIS_Region, target_population, total_vaccinated, total_coverage) %>%
              rename(numerator = total_vaccinated,
                     denominator = target_population,
                     value = total_coverage) %>%
              rename(Region = APMIS_Region) %>%
              select(Region, denominator, numerator, value) %>%
              arrange(desc(value))
          } else if ("All" %in% province()) {
            report_data()$admin_data$province %>%
              filter(age_group == "0-59 Months") %>%
              select(APMIS_Province, target_population, total_vaccinated, total_coverage) %>%
              rename(numerator = total_vaccinated,
                     denominator = target_population,
                     value = total_coverage) %>%
              rename(Province = APMIS_Province) %>%
              select(Province, denominator, numerator, value) %>%
              arrange(desc(value))
          } else if ("All" %in% district()) {
            report_data()$admin_data$district %>%
              filter(age_group == "0-59 Months") %>%
              select(APMIS_District, target_population, total_vaccinated, total_coverage) %>%
              rename(numerator = total_vaccinated,
                     denominator = target_population,
                     value = total_coverage) %>%
              rename(District = APMIS_District) %>%
              select(District, denominator, numerator, value) %>%
              arrange(desc(value))
          } else {
            report_data()$admin_data$district %>%
              filter(age_group == "0-59 Months") %>%
              select(APMIS_District, target_population, total_vaccinated, total_coverage) %>%
              rename(numerator = total_vaccinated,
                     denominator = target_population,
                     value = total_coverage) %>%
              rename(District = APMIS_District) %>%
              select(District, denominator, numerator, value) %>%
              arrange(desc(value))
          }
        }
      if (is_valid_df(table_data)){ 
        table_data <- table_data %>%
          mutate_at(c("numerator", "denominator", "value"), ~as.numeric(.)) %>%
          mutate(value = round(value, 3) * 100) %>%
          rename(
            "Target Population" = denominator,
            "Total OPV Vaccinated" = numerator,
            "Coverage\n(OPV Vaccinated/Target)" = value
          )
        
        # Add Total row
        group_col <- names(table_data)[1]
        total_row <- data.frame(
          setNames("Total", group_col),
          `Target Population` = sum(table_data$`Target Population`, na.rm = TRUE),
          `Total OPV Vaccinated` = sum(table_data$`Total OPV Vaccinated`, na.rm = TRUE),
          `Coverage\n(OPV Vaccinated/Target)` = round(100 * sum(table_data$`Total OPV Vaccinated`, na.rm = TRUE) /
            sum(table_data$`Target Population`, na.rm = TRUE),1),
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
        names(total_row) <- names(table_data)
        table_data <- bind_rows(total_row, table_data)
        
        # Build formatted flextable
        table <- table_data %>%
          flextable::flextable() %>%
          flextable::colformat_num(
            j = "Coverage\n(OPV Vaccinated/Target)",
            digits = 0,
            suffix = "%"
          ) %>%
          flextable::width(j = 1:4, width = c(1.6, 1.3, 1.3, 1.6)) %>%
          flextable::set_table_properties(layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::bg(part = "header", bg = "#0D6938") %>%
          flextable::color(part = "header", color = "white") %>%
          flextable::bg(i = 1, bg = "#FBCB9A", part = "body") %>%
          flextable::bold(i = 1, bold = TRUE, part = "body") %>%
          flextable::fontsize(size = 9, part = "all")
      } else{
        table <- NULL
      }
      
      if(vaccine_type == "IPV"){
        data_admin <- report_data()$admin_data$district %>%
          # data_admin <- all_apmis_data_list$apmis_admin_data$district %>%
          # filter(campaign_name == "May NID 2025") %>%
          filter(age_group %in% c("0-59 Months", "4-59 Months")) %>%
          filter(vaccine_type == "IPV") %>%
          select(campaign_name, APMIS_Region,  APMIS_Province, APMIS_District,  rcode, pcode, dcode, target_population, total_vaccinated, total_coverage) %>%
          rename(numerator = total_vaccinated,
                 denominator = target_population,
                 value = total_coverage) %>%
          mutate(value_cat = case_when(value > 1 ~ ">100%",
                                       value >= 0.95 ~ "95-100%",
                                       value >= 0.90 ~ "90-94%",
                                       value >= 0.85 ~ "85-89%",
                                       value < 0.85 ~ "<85%",
                                       TRUE ~ NA_character_)) %>%
          arrange(desc(value_cat))
        
        if (is_valid_df(data_admin)){
          legend_title <- "Admin Coverage\n(4-59m, IPV)"
          
          report_borders_district <- report_borders_district()
          report_borders_province <- report_borders_district %>%
            group_by(APMIS_Region, APMIS_Province, APMIS_RCODE, APMIS_PCODE) %>%
            summarise(geometry = sf::st_union(geometry), .groups = "drop") 
          report_borders_region <- report_borders_district %>%
            group_by(APMIS_Region, APMIS_RCODE)  %>%
            summarise(geometry = sf::st_union(geometry), .groups = "drop")
          
          # Extract non-NA values from the specified variable
          data_cat <- sort(unique(data_admin[["value_cat"]][!is.na(data_admin[["value_cat"]]) & !(data_admin[["value_cat"]] %in% c("na", "Na"))]))
          
          # pct, pca_reasons_missed, ooh_reasons_missed, lqas_reasons_missed, pca_awareness_source, pca_door_marking, lqas_result, pca_modality
          map_colors <- colors_coverage_bins2[names(colors_coverage_bins2) %in% data_cat]
          
          data <- data_admin %>%
            inner_join(report_borders_district,
                       by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
            sf::st_as_sf()
          
          
          no_sia_districts <- report_borders_district %>%
            anti_join(campaign_rpd %>%
                        filter(campaign_name == campaign_name()),
                      by=c("APMIS_RCODE" = "rcode",
                           "APMIS_PCODE" = "pcode",
                           "APMIS_DCODE" = "dcode"))
          
          no_apmis_data_districts <- report_borders_district %>%
            inner_join(campaign_rpd %>%
                         filter(campaign_name == campaign_name()),
                       by=c("APMIS_RCODE" = "rcode",
                            "APMIS_PCODE" = "pcode",
                            "APMIS_DCODE" = "dcode")) %>%
            anti_join(data %>% sf::st_drop_geometry(), by=c("APMIS_RCODE" = "rcode",
                                                            "APMIS_PCODE" = "pcode",
                                                            "APMIS_DCODE" = "dcode"))
          
          # Add value_cat manually to other data
          no_apmis_data_districts$value_cat <- "No APMIS Data"
          no_sia_districts$value_cat <- "No Campaign"
          
          # Bind all
          combined_data <- bind_rows(data, no_apmis_data_districts, no_sia_districts)
          
          # Now plot
          map_plot_ipv <- ggplot() +
            geom_sf(data = combined_data, aes(fill = value_cat), alpha=0.7) +
            geom_sf(data=report_borders_province, fill=NA, color="#252525", linewidth = 0.6) +
            geom_sf(data=report_borders_region, fill=NA, color="black", linewidth = 0.8) +
            coord_sf(crs = st_crs(4326), datum = NA) +
            scale_fill_manual(
              values = c(map_colors, "No APMIS Data" = "#525252", "No Campaign" = "#d9d9d9"),
              breaks = c(names(map_colors), "No APMIS Data", "No Campaign"),
              name = legend_title
            ) +
            theme_void() +
            theme(
              legend.title = element_text(size=11),
              legend.text = element_text(size = 9),
              legend.position = "left"
            )
        } else{
          map_plot_ipv <- NULL
        }
        
        # Create Title ------------------------------------------------------------
        title_text_ipv <- {
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
            paste0(campaign_name(), ": Admin Coverage (IPV)")
          } else {
            paste0(campaign_name(), ", ", loc, ": Admin Coverage (IPV)")
          }
        }
        
        # Create Table -----------------------------------------------------------
        table_data <-  {
          data <- if ("All" %in% region()) {
            report_data()$admin_data$region %>%
              filter(age_group == "4-59 Months") %>%
              select(APMIS_Region, target_population, total_vaccinated, total_coverage) %>%
              rename(numerator = total_vaccinated,
                     denominator = target_population,
                     value = total_coverage) %>%
              rename(Region = APMIS_Region) %>%
              select(Region, denominator, numerator, value) %>%
              arrange(desc(value))
          } else if ("All" %in% province()) {
            report_data()$admin_data$province %>%
              filter(age_group == "4-59 Months") %>%
              select(APMIS_Province, target_population, total_vaccinated, total_coverage) %>%
              rename(numerator = total_vaccinated,
                     denominator = target_population,
                     value = total_coverage) %>%
              rename(Province = APMIS_Province) %>%
              select(Province, denominator, numerator, value) %>%
              arrange(desc(value))
          } else if ("All" %in% district()) {
            report_data()$admin_data$district %>%
              filter(age_group == "4-59 Months") %>%
              select(APMIS_District, target_population, total_vaccinated, total_coverage) %>%
              rename(numerator = total_vaccinated,
                     denominator = target_population,
                     value = total_coverage) %>%
              rename(District = APMIS_District) %>%
              select(District, denominator, numerator, value) %>%
              arrange(desc(value))
          } else {
            report_data()$admin_data$district %>%
              filter(age_group == "4-59 Months") %>%
              select(APMIS_District, target_population, total_vaccinated, total_coverage) %>%
              rename(numerator = total_vaccinated,
                     denominator = target_population,
                     value = total_coverage) %>%
              rename(District = APMIS_District) %>%
              select(District, denominator, numerator, value) %>%
              arrange(desc(value))
          }
        }
        if (is_valid_df(table_data)){ 
          table_data <- table_data %>%
            mutate_at(c("numerator", "denominator", "value"), ~as.numeric(.)) %>%
            mutate(value = round(value, 3) * 100) %>%
            rename(
              "Target Population" = denominator,
              "Total IPV Vaccinated" = numerator,
              "Coverage\n(IPV Vaccinated/Target)" = value
            )
          
          # Add Total row
          group_col <- names(table_data)[1]
          total_row <- data.frame(
            setNames("Total", group_col),
            `Target Population` = sum(table_data$`Target Population`, na.rm = TRUE),
            `Total IPV Vaccinated` = sum(table_data$`Total IPV Vaccinated`, na.rm = TRUE),
            `Coverage\n(IPV Vaccinated/Target)` = round(100 * sum(table_data$`Total IPV Vaccinated`, na.rm = TRUE) /
                                                      sum(table_data$`Target Population`, na.rm = TRUE),1),
            check.names = FALSE,
            stringsAsFactors = FALSE
          )
          names(total_row) <- names(table_data)
          table_data <- bind_rows(total_row, table_data)
          
          # Build formatted flextable
          table_ipv <- table_data %>%
            flextable::flextable() %>%
            flextable::colformat_num(
              j = "Coverage\n(IPV Vaccinated/Target)",
              digits = 0,
              suffix = "%"
            ) %>%
            flextable::width(j = 1:4, width = c(1.6, 1.3, 1.3, 1.6)) %>%
            flextable::set_table_properties(layout = "autofit") %>%
            flextable::theme_vanilla() %>%
            flextable::bg(part = "header", bg = "#0D6938") %>%
            flextable::color(part = "header", color = "white") %>%
            flextable::bg(i = 1, bg = "#FBCB9A", part = "body") %>%
            flextable::bold(i = 1, bold = TRUE, part = "body") %>%
            flextable::fontsize(size = 9, part = "all")
        } else{
          table_ipv <- NULL
        }
      }
      
        # Create Definition Text --------------------------------------------------
        definition_text <- "Admin coverage is the total recorded vaccinated by the vaccination teams, divided by the target population in an area. Results >100% often indicate an underestimated target population.\nDistribution of reasons for remaining recorded missed children is only representative of areas where missed children are systematically recorded by reason."
    
    if(!is.null(map_plot) && !is.null(admin_bar) && !is.null(table) && !is.null(admin_trend_plot)){
        if(no_admin_pie == TRUE){
        my_pres <- my_pres %>%
          add_slide(layout = "template5", master = "Office Theme") %>%
          ph_with(value = map_plot, location = ph_location_label(ph_label = "MapBox")) %>%
          ph_with(value = admin_bar, location = ph_location_label(ph_label = "BarBox")) %>%
          ph_with(value = table, location = ph_location_label(ph_label = "TableBox")) %>%
          ph_with(value = admin_trend_plot, location = ph_location_label(ph_label = "TrendBox")) %>%
          ph_with(
            value = fpar(ftext(definition_text, fp_text(font.size = 9))),
            location = ph_location_label(ph_label = "TextBox")
          ) %>%
          ph_with(value = title_text, location = ph_location_label(ph_label = "Title Box")) %>%
          ph_with(
            value = fpar(
              ftext(paste0("Data Source: APMIS, extracted on ", Sys.Date()), fp_text(font.size = 7)),
              fp_p = fp_par(text.align = "right")
            ),
            location = ph_location_label(ph_label = "Footer Box")
          )
        } else{
          my_pres <- my_pres %>%
            add_slide(layout = "template5", master = "Office Theme") %>%
            ph_with(value = map_plot, location = ph_location_label(ph_label = "MapBox")) %>%
            ph_with(value = admin_bar, location = ph_location_label(ph_label = "BarBox")) %>%
            ph_with(value = table, location = ph_location_label(ph_label = "TableBox")) %>%
            ph_with(value = admin_trend_plot, location = ph_location_label(ph_label = "TrendBox")) %>%
            ph_with(value = admin_pie, location = ph_location_label(ph_label = "PieBox")) %>%
            ph_with(
              value = fpar(ftext(definition_text, fp_text(font.size = 9))),
              location = ph_location_label(ph_label = "TextBox")
            ) %>%
            ph_with(value = title_text, location = ph_location_label(ph_label = "Title Box")) %>%
            ph_with(
              value = fpar(
                ftext(paste0("Data Source: APMIS, extracted on ", Sys.Date()), fp_text(font.size = 7)),
                fp_p = fp_par(text.align = "right")
              ),
              location = ph_location_label(ph_label = "Footer Box")
            )
        }
    }
      
      if(vaccine_type == "IPV"){
        if(!is.null(map_plot_ipv) && !is.null(admin_bar_ipv) && !is.null(table_ipv) && !is.null(admin_trend_plot_ipv)){
          if(no_admin_pie_ipv == TRUE){
            my_pres <- my_pres %>%
              add_slide(layout = "template5", master = "Office Theme") %>%
              ph_with(value = map_plot_ipv, location = ph_location_label(ph_label = "MapBox")) %>%
              ph_with(value = admin_bar_ipv, location = ph_location_label(ph_label = "BarBox")) %>%
              ph_with(value = table_ipv, location = ph_location_label(ph_label = "TableBox")) %>%
              ph_with(value = admin_trend_plot_ipv, location = ph_location_label(ph_label = "TrendBox")) %>%
              ph_with(
                value = fpar(ftext(definition_text, fp_text(font.size = 9))),
                location = ph_location_label(ph_label = "TextBox")
              ) %>%
              ph_with(value = title_text_ipv, location = ph_location_label(ph_label = "Title Box")) %>%
              ph_with(
                value = fpar(
                  ftext(paste0("Data Source: APMIS, extracted on ", Sys.Date()), fp_text(font.size = 7)),
                  fp_p = fp_par(text.align = "right")
                ),
                location = ph_location_label(ph_label = "Footer Box")
              )
          } else{
            my_pres <- my_pres %>%
              add_slide(layout = "template5", master = "Office Theme") %>%
              ph_with(value = map_plot_ipv, location = ph_location_label(ph_label = "MapBox")) %>%
              ph_with(value = admin_bar_ipv, location = ph_location_label(ph_label = "BarBox")) %>%
              ph_with(value = table_ipv, location = ph_location_label(ph_label = "TableBox")) %>%
              ph_with(value = admin_trend_plot_ipv, location = ph_location_label(ph_label = "TrendBox")) %>%
              ph_with(value = admin_pie_ipv, location = ph_location_label(ph_label = "PieBox")) %>%
              ph_with(
                value = fpar(ftext(definition_text, fp_text(font.size = 9))),
                location = ph_location_label(ph_label = "TextBox")
              ) %>%
              ph_with(value = title_text_ipv, location = ph_location_label(ph_label = "Title Box")) %>%
              ph_with(
                value = fpar(
                  ftext(paste0("Data Source: APMIS, extracted on ", Sys.Date()), fp_text(font.size = 7)),
                  fp_p = fp_par(text.align = "right")
                ),
                location = ph_location_label(ph_label = "Footer Box")
              )
          }
        }
      }
      
# Triangulation slides -----------------------------------------------------
        #Slide 1: 3 coverage maps + 1 combined map
      if (is_valid_df(data_pca) && is_valid_df(data_ooh) && is_valid_df(data_lqas)){
        data_triangulation <- data_pca %>%
          mutate(value = as.numeric(value)) %>%
          mutate(value_cat = case_when(is.na(value) ~ NA_character_,
                                       value <= 0.95 ~ "Coverage ≤95% or One or More Lots Failed",
                                       value > 0.95 ~ "Coverage >95% or All Lots Passed"),
                 source = "Post-Campaign\nAssessment") %>%
          select(source, rcode, pcode, dcode, value_cat) %>%
          bind_rows(
            data_ooh %>%
              mutate(value = as.numeric(value)) %>%
              mutate(value_cat = case_when(is.na(value) ~ NA_character_,
                                           value <= 0.95 ~ "Coverage ≤95% or One or More Lots Failed",
                                           value > 0.95 ~ "Coverage >95% or All Lots Passed"),
                     source = "Out-of-House\nFinger-Mark Survey") %>%
              select(source, rcode, pcode, dcode, value_cat)
          ) %>%
          bind_rows(
            data_lqas %>%
            mutate(value_cat = case_when(is.na(value) ~ NA_character_,
                                         value == "One or More Lots Failed" ~ "Coverage ≤95% or One or More Lots Failed",
                                         value == "All Lots Passed" ~ "Coverage >95% or All Lots Passed"),
                   source = "LQAS") %>%
              select(source, rcode, pcode, dcode, value_cat)
          )
        
        # Ensure value_cat is a factor with all desired levels
        all_cats <- c("Coverage ≤95% or One or More Lots Failed", "Coverage >95% or All Lots Passed")  # replace with your full set of categories
        data_triangulation$value_cat <- factor(data_triangulation$value_cat, levels = all_cats)
        
        data_triangulation2 <- data_triangulation %>%
          group_by(rcode, pcode, dcode, value_cat) %>%
          summarise(count = n(), .groups = "drop") %>%
          pivot_wider(
            names_from = value_cat,
            values_from = count,
            values_fill = 0
          ) 
        if(!("Coverage ≤95% or One or More Lots Failed" %in% colnames(data_triangulation2))){
          data_triangulation2$`Coverage ≤95% or One or More Lots Failed` <- 0
        }
        data_triangulation2 <- data_triangulation2  %>%
          mutate(value_cat = case_when(`Coverage ≤95% or One or More Lots Failed` ==  3 ~ "PCA≤95%, OOH-FM Survey≤95% and LQAS failed",
                                       `Coverage ≤95% or One or More Lots Failed` ==  2 ~ "Either two (PCA≤95% or OOH-FM Survey≤95% or LQAS failed)",
                                       `Coverage ≤95% or One or More Lots Failed` ==  1 ~ "Either one (PCA≤95% or OOH-FM Survey≤95% or LQAS failed)",
                                       `Coverage ≤95% or One or More Lots Failed` ==  0 ~ "PCA>95%, OOH-FM Survey>95% and LQAS passed",
                                       TRUE ~ NA_character_),
                source = "Combined" 
          )
        
        data_triangulation <- data_triangulation  %>%
          bind_rows(data_triangulation2)
           
        data_triangulation <- report_borders_district %>%
          inner_join(data_triangulation,
                       by = c("APMIS_RCODE" = "rcode",
                            "APMIS_PCODE" = "pcode",
                            "APMIS_DCODE" = "dcode")) %>%
          sf::st_as_sf()
        
        # Step 2: Add 'No Campaign' districts
        no_sia_districts <- report_borders_district %>%
          anti_join(campaign_rpd %>%
                      filter(campaign_name == campaign_name()),
                    by = c("APMIS_RCODE" = "rcode",
                           "APMIS_PCODE" = "pcode",
                           "APMIS_DCODE" = "dcode")) %>%
          rename(rcode = APMIS_RCODE, pcode = APMIS_PCODE, dcode = APMIS_DCODE) %>%
          select(rcode, pcode, dcode, geometry) %>%
          sf::st_as_sf()
        
        
        no_apmis_data_districts <- report_borders_district %>%
          inner_join(campaign_rpd %>%
                       filter(campaign_name == campaign_name()),
                     by=c("APMIS_RCODE" = "rcode",
                          "APMIS_PCODE" = "pcode",
                          "APMIS_DCODE" = "dcode")) %>%
          anti_join(data_triangulation %>% sf::st_drop_geometry(), by=c("APMIS_RCODE",
                                                          "APMIS_PCODE",
                                                          "APMIS_DCODE")) %>%
          sf::st_as_sf()
        
        # Step 3: Combine both groups
        
        # Add value_cat manually to other data
        no_apmis_data_districts$value_cat <- "No APMIS Data"
        no_sia_districts$value_cat <- "No Campaign"
        

        # Bind all
        combined_data <- bind_rows(data_triangulation, no_apmis_data_districts, no_sia_districts)
        
        # Step 4: Define color palette
        map_colors <- c(
          "PCA≤95%, OOH-FM Survey≤95% and LQAS failed" = "#e41a1c",
          "Either two (PCA≤95% or OOH-FM Survey≤95% or LQAS failed)" = "#ff7f00",
          "Either one (PCA≤95% or OOH-FM Survey≤95% or LQAS failed)" = "#ffff33",
          "PCA>95%, OOH-FM Survey>95% and LQAS passed" = "#4daf4a",
          "Coverage ≤95% or One or More Lots Failed"     = "#e41a1c",
          "Coverage >95% or All Lots Passed"      = "#4daf4a",
          "No Data" = "#d9d9d9"
        )
        
        # Step 5: Plot
        map_plot1 <-
          ggplot() +
          geom_sf(data = report_borders_district %>% mutate(value_cat = "No Data"), aes(fill = value_cat), size = 0.1, alpha = 0.75) +
          geom_sf(data = combined_data %>% filter(source != "Combined"), aes(fill = value_cat), size = 0.1, alpha = 0.75) +
          geom_sf(data = report_borders_province, fill = NA, color = "#252525", linewidth = 0.6) +
          geom_sf(data = report_borders_region, fill = NA, color = "black", linewidth = 0.8) +
          coord_sf(crs = st_crs(4326), datum = NA) +
          facet_wrap(~source, nrow = 1) +
          scale_fill_manual(
            values = map_colors,
            name = NULL
          ) +
          theme_void(base_size = 11) +
          theme(
            legend.position = "bottom", 
            strip.text = element_text(size = 11)  # enable facet labels
          )
        
        map_plot2 <-
          ggplot() +
          geom_sf(data = report_borders_district %>% mutate(value_cat = "No Data"), aes(fill = value_cat), size = 0.1, alpha = 0.75) +
          geom_sf(data = combined_data %>% filter(source == "Combined") %>%
                    mutate(value_cat = factor(value_cat, levels=c("PCA≤95%, OOH-FM Survey≤95% and LQAS failed",
                                                                  "Either two (PCA≤95% or OOH-FM Survey≤95% or LQAS failed)",
                                                                  "Either one (PCA≤95% or OOH-FM Survey≤95% or LQAS failed)",
                                                                  "PCA>95%, OOH-FM Survey>95% and LQAS passed", 
                                                                  "No Data"))) %>%
                    arrange(value_cat), aes(fill = value_cat), size = 0.1, alpha = 0.75) +
          geom_sf(data = report_borders_province, fill = NA, color = "#252525", linewidth = 0.4) +
          geom_sf(data = report_borders_region, fill = NA, color = "black", linewidth = 0.6) +
          coord_sf(crs = st_crs(4326), datum = NA) +
          facet_wrap(~source, nrow = 1) +
          scale_fill_manual(
            values = map_colors,
            name = "Combined Result"
          ) +
          theme_void(base_size = 13) +
          theme(
            legend.position = "right", 
            strip.text = element_blank()  # enable facet labels
          )
        
        title_text <- {
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
            paste0(campaign_name(), ": Triangulation Across Post-Campaign Monitoring Types (OPV)")
          } else {
            paste0(campaign_name(), ", ", loc, ": Triangulation Across Post-Campaign Monitoring Types (OPV)")
          }
        }
          
        
        my_pres <- my_pres %>%
          add_slide(layout = "triangulation1", master = "Office Theme") %>%
          ph_with(value = map_plot2, location = ph_location_label(ph_label = "Box1")) %>%
          ph_with(value = map_plot1, location = ph_location_label(ph_label = "Text Box")) %>%
          ph_with(value = fpar(ftext(title_text, fp_text(font.size = 18, color = "white", bold = FALSE))),
            location = ph_location_label(ph_label = "Title Box")) %>%
          ph_with(
            value = fpar(
              ftext(paste0("Data Source: APMIS, extracted on ", Sys.Date()), fp_text(font.size = 7)),
              fp_p = fp_par(text.align = "right")
            ),
            location = ph_location_label(ph_label = "Footer Box")
          )
      
        #Triangulation slide: Correlation Matrix
      #   coverage_df <- data_pca %>%
      #     select(APMIS_Region, APMIS_Province, APMIS_District, rcode, pcode, dcode, value) %>%
      #     rename(pca_coverage = value) %>%
      #     full_join(data_lqas %>%
      #                 select(APMIS_Region, APMIS_Province, APMIS_District, rcode, pcode, dcode, value) %>%
      #                 rename(lqas_result = value),
      #               by=c("APMIS_Region", "APMIS_Province", "APMIS_District", "rcode", "pcode", "dcode")) %>%
      #     full_join(data_ooh %>%
      #                 select(APMIS_Region, APMIS_Province, APMIS_District, rcode, pcode, dcode, value) %>%
      #                 rename(ooh_coverage = value),
      #               by=c("APMIS_Region", "APMIS_Province", "APMIS_District", "rcode", "pcode", "dcode")) %>%
      #     full_join(data_admin %>%
      #                 select(APMIS_Region, APMIS_Province, APMIS_District, rcode, pcode, dcode, value, numerator, denominator) %>%
      #                 rename(admin_coverage = value,
      #                        total_vaccinated = numerator,
      #                        target_pop = denominator),
      #               by=c("APMIS_Region", "APMIS_Province", "APMIS_District", "rcode", "pcode", "dcode")) %>%
      #     mutate_at(c("pca_coverage", "admin_coverage", "ooh_coverage"), ~as.numeric(.)) %>%
      #     mutate(lqas_result = ifelse(is.na(lqas_result), "Incomplete Data", lqas_result)) %>%
      #     mutate(lqas_result = factor(lqas_result, levels=c("All Lots Passed", "One or More Lots Failed", "Incomplete Data")))
      #   
      #   #only make district-correlation slide if at province level or higher:
      # if ("All" %in% district()){
      #   library(ggplot2)
      #   library(patchwork)
      #   library(dplyr)
      #   library(scales)
      #   dark_orange <- "#cc4c02"
      #   light_orange <- "#fed98e"
      #   
      #   # ---- Facet-style Theme ----
      #   plot_theme_facet <- theme_minimal(base_size = 11) +
      #     theme(
      #       panel.grid = element_blank(),
      #       panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      #       strip.background = element_rect(fill = "#006400", color = NA),
      #       strip.text = element_text(color = "white", face = "bold", size = 9),
      #       axis.text = element_text(size = 8),
      #       axis.title = element_text(size = 9),
      #       plot.margin = margin(5, 5, 5, 5)
      #     )
      #   
      #   # ---- Scatter Plot Function with R² and p-value ----
      #   scatter_plot <- function(x, y, label, xlab, ylab) {
      #     df <- coverage_df %>%
      #       filter(!is.na(.data[[x]]), !is.na(.data[[y]])) %>%
      #       mutate(label = label)
      #     
      #     if (nrow(df) == 0) return(NULL)
      #     
      #     model <- lm(df[[y]] ~ df[[x]])
      #     summary_model <- summary(model)
      #     r_squared <- formatC(summary_model$r.squared, format = "f", digits = 2)
      #     t_stat    <- formatC(summary_model$coefficients[2, "t value"], format = "f", digits = 0)
      #     
      #     p_val     <- summary_model$coefficients[2, "Pr(>|t|)"]
      #     p_label   <- ifelse(p_val < 0.01, "p < 0.01", 
      #                         paste0("p = ", formatC(p_val, format = "f", digits = 2)))
      #     
      #     annotation <- paste0("R² = ", r_squared,
      #                          "\nt = ", t_stat,
      #                          "\n", p_label)
      #     
      #     x_max <- max(df[[x]], na.rm = TRUE)
      #     y_max <- max(df[[y]], na.rm = TRUE)
      #     
      #     ggplot(df, aes_string(x = x, y = y)) +
      #       geom_point(alpha = 0.4, size=1) +
      #       geom_smooth(method = "lm", se = TRUE, 
      #                   color = dark_orange, fill = light_orange, alpha=0.8) +
      #       annotate("text", x = -Inf, y = -Inf, hjust = -0.1, vjust = -1,
      #                label = annotation, size = 3, fontface = "italic") +
      #       labs(x = xlab, y = ylab) +
      #       scale_x_continuous(labels = scales::percent_format(accuracy = 1),
      #                          limits = c(axis_min, ceiling(x_max * 100) / 100)) +
      #       scale_y_continuous(labels = scales::percent_format(accuracy = 1),
      #                          limits = c(axis_min, ceiling(y_max * 100) / 100)) +
      #       facet_wrap(~label) +
      #       plot_theme_facet
      #   }
      #   
      #   # ---- Box Plot Function ----
      #   box_plot <- function(y, label, ylab) {
      #     df <- coverage_df %>%
      #       filter(!is.na(.data[[y]]), lqas_result %in% c("All Lots Passed", "One or More Lots Failed")) %>%
      #       mutate(label = label)
      #     
      #     if (nrow(df) == 0 || length(unique(df$lqas_result)) < 2) {
      #       return(
      #         ggplot() +
      #           annotate("text", x = 0.5, y = 0.5,
      #                    label = paste("Not enough LQAS group data for", label),
      #                    size = 4, fontface = "italic") +
      #           theme_void()
      #       )
      #     }
      #     
      #     kw <- kruskal.test(df[[y]] ~ df$lqas_result)
      #     
      #     stat <- formatC(kw$statistic, format = "f", digits = 0)
      #     p_val <- kw$p.value
      #     p_label <- ifelse(p_val < 0.01, "p < 0.01", paste0("p = ", formatC(p_val, format = "f", digits = 2)))
      #     annotation <- paste0("χ² = ", stat, "\n", p_label)
      #     
      #     y_max <- max(df[[y]], na.rm = TRUE)
      #     
      #     ggplot(df, aes(x = lqas_result, y = .data[[y]])) +
      #       geom_jitter(width = 0.2, alpha = 0.4, size = 1) +
      #       geom_boxplot(outlier.shape = NA,
      #                    color = dark_orange,
      #                    fill = light_orange, alpha = 0.5) +
      #       annotate("text", x = -Inf, y = -Inf, hjust = -0.1, vjust = -1,
      #                label = annotation, size = 3, fontface = "italic") +
      #       labs(x = "LQAS District Result", y = ylab) +
      #       scale_y_continuous(labels = scales::percent_format(accuracy = 1),
      #                          limits = c(axis_min, ceiling(y_max * 100) / 100)) +
      #       facet_wrap(~label) +
      #       plot_theme_facet +
      #       theme(axis.text.x = element_text(angle = 20, hjust = 1))
      #   }
      #   
      #   axis_min <- floor(min(
      #     coverage_df$pca_coverage,
      #     coverage_df$ooh_coverage,
      #     coverage_df$admin_coverage,
      #     na.rm = TRUE
      #   ) * 100) / 100  # e.g. rounds to nearest 0.01
      #   
      #   p1 <- scatter_plot("ooh_coverage", "pca_coverage", "PCA vs OOH-FMS", "OOH-FMS Coverage", "PCA Coverage")
      #   p2 <- scatter_plot("admin_coverage", "pca_coverage", "PCA vs Admin", "Admin Coverage", "PCA Coverage")
      #   p3 <- scatter_plot("admin_coverage", "ooh_coverage", "OOH-FMS vs Admin", "Admin Coverage", "OOH-FMS Coverage")
      #   
      #   if(length(unique(coverage_df$lqas_result[coverage_df$lqas_result %in% c("All Lots Passed", "One or More Lots Failed")])) == 2){
      #   # Bottom row: boxplots
      #   b1 <- box_plot("pca_coverage", "PCA vs LQAS", "PCA Coverage")
      #   b2 <- box_plot("ooh_coverage", "OOH-FMS vs LQAS", "OOH-FMS Coverage")
      #   b3 <- box_plot("admin_coverage", "Admin vs LQAS", "Admin Coverage")
      #   
      #   # Final Layout
      #   plot_matrix <- (p1 | p3 | p2) / (b1 | b2 | b3) +
      #     plot_layout(heights = c(1, 1)) +
      #     plot_annotation(
      #       # title = "Correlation between Coverage Estimates and LQAS Results, at the District Level",
      #       title = NULL,
      #       subtitle = NULL,
      #       theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
      #     )
      #   } else{
      #     plot_matrix <- (p1 | p3 | p2) +
      #       plot_layout(heights = c(1)) +
      #       plot_annotation(
      #         # title = "Correlation between Coverage Estimates and LQAS Results, at the District Level",
      #         title = NULL,
      #         subtitle = NULL,
      #         theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
      #       )
      #   }
      #   
      #   subtitle_text <- {
      #     loc <- if ("All" %in% region()) {
      #       NULL
      #     } else if ("All" %in% province()) {
      #       paste(region(), "Region")
      #     } else if ("All" %in% district()) {
      #       paste(province(), "Province")
      #     } else {
      #       paste(district(), "District")
      #     }
      #     
      #     if (is.null(loc)) {
      #       paste0(
      #         "Triangulation of district-level coverage estimates across the Post Campaign Assessment (PCA), Out-of-House Finger-Mark Survey (OOH-FMS), Administrative Data (Admin), and Lot Quality Assurance Sampling (LQAS). ",
      #         "Each point represents a district in the ", campaign_name(), ". ",
      #         "Scatterplot trend lines show linear regression with 95% CI (t-test); ",
      #         "boxplots show coverage distributions by LQAS category (Kruskal-Wallis χ² test).",
      #         sep = "\n"
      #       )
      #     } else {
      #       paste0(
      #         "Triangulation of district-level coverage estimates across PCA, OOH-FMS, Admin, and LQAS",
      #         paste0("Each point represents a district in the ", campaign_name(), ", ", loc, "."),
      #         "Scatterplot trend lines show linear regression with 95% CI (t-test); ",
      #         "boxplots show coverage distributions by LQAS result (Kruskal-Wallis χ² test).",
      #         sep = "\n"
      #       )
      #     }
      #   }
      #   
      #   # ---- Interpretation Helpers ----
      #   interpret_r2 <- function(r2, direction) {
      #     dir_text <- if (direction == "positive") "positive" else if (direction == "negative") "negative" else ""
      #     
      #     if (r2 >= 0.7) return(paste("Strong", dir_text, "correlation"))
      #     if (r2 >= 0.4) return(paste("Moderate", dir_text, "correlation"))
      #     if (r2 >= 0.2) return(paste("Weak", dir_text, "correlation"))
      #     return("Not meaningfully correlated")
      #   }
      #   
      #   interpret_p <- function(p) {
      #     if (p < 0.01) return("p < 0.01")
      #     paste0("p = ", round(p, 2))
      #   }
      #   
      #   describe_correlation <- function(x, y, label) {
      #     df <- coverage_df %>% filter(!is.na(.data[[x]]), !is.na(.data[[y]]))
      #     
      #     # Handle case with insufficient data
      #     if (nrow(df) < 2) {
      #       return(paste0("• ", label, ": Not enough data to assess correlation."))
      #     }
      #     
      #     model <- lm(df[[y]] ~ df[[x]])
      #     s <- summary(model)
      #     
      #     r2_val <- s$r.squared
      #     r2 <- formatC(r2_val, digits = 2, format = "f")
      #     t_val <- s$coefficients[2, "t value"]
      #     t <- formatC(t_val, digits = 0, format = "f")
      #     p <- s$coefficients[2, "Pr(>|t|)"]
      #     p_text <- interpret_p(p)
      #     
      #     direction <- if (t_val > 0) "positive" else if (t_val < 0) "negative" else "none"
      #     
      #     # 1. Not meaningful + not significant
      #     if (r2_val < 0.2 && p >= 0.05) {
      #       return(paste0("• ", label, ": No meaningful correlation was observed (R² = ", r2,
      #                     ", t = ", t, ", ", p_text, ")."))
      #     }
      #     
      #     # 2. Not meaningful + significant
      #     if (r2_val < 0.2 && p < 0.05) {
      #       return(paste0("• ", label, ": A statistically significant but weak ",
      #                     ifelse(direction == "positive", "positive", "negative"),
      #                     " relationship was observed (R² = ", r2, ", t = ", t, ", ", p_text, ")."))
      #     }
      #     
      #     # 3. Meaningful correlation (weak/moderate/strong)
      #     phrasing <- interpret_r2(r2_val, direction)
      #     significance <- if (p < 0.05) {
      #       "statistically significant"
      #     } else {
      #       "not statistically significant"
      #     }
      #     
      #     paste0("• ", label, ": ", phrasing, " (", significance,
      #            "; R² = ", r2, ", t = ", t, ", ", p_text, ").")
      #   }
      #   
      #   describe_lqas <- function(y, label) {
      #     df <- coverage_df %>%
      #       filter(!is.na(.data[[y]]), lqas_result %in% c("All Lots Passed", "One or More Lots Failed"))
      #     
      #     if (nrow(df) == 0 || length(unique(df$lqas_result)) < 2) {
      #       return(paste0("• ", label, " vs. LQAS: Not enough data for comparison."))
      #     }
      #     
      #     kw <- kruskal.test(df[[y]] ~ df$lqas_result)
      #     stat <- round(kw$statistic, 0)
      #     p <- kw$p.value
      #     p_text <- interpret_p(p)
      #     
      #     if (p < 0.05) {
      #       medians <- df %>%
      #         group_by(lqas_result) %>%
      #         summarise(median = median(.data[[y]], na.rm = TRUE)) %>%
      #         pivot_wider(names_from = lqas_result, values_from = median)
      #       
      #       diff <- round((medians[[1]] - medians[[2]]) * 100, 1)
      #       diff_text <- if (diff > 0) {
      #         paste0("Coverage was ", diff, "% higher in districts where all lots passed")
      #       } else {
      #         paste0("Coverage was ", abs(diff), "% lower in districts where all lots passed")
      #       }
      #       
      #       paste0("• ", label, " vs. LQAS: ", diff_text, " (", p_text, ").")
      #     } else {
      #       paste0("• ", label, " vs. LQAS: No significant difference in coverage by LQAS result (", p_text, ").")
      #     }
      #   }
      #   
      #   # Scatter comparisons
      #   c1 <- describe_correlation("ooh_coverage", "pca_coverage", "PCA vs OOH-FMS")
      #   c3 <- describe_correlation("admin_coverage", "pca_coverage", "PCA vs Admin")
      #   c2 <- describe_correlation("admin_coverage", "ooh_coverage", "OOH-FMS vs Admin")
      #   
      #   # LQAS comparisons
      #   if(length(unique(coverage_df$lqas_result[coverage_df$lqas_result %in% c("All Lots Passed", "One or More Lots Failed")])) == 2){
      #   l1 <- describe_lqas("pca_coverage", "PCA")
      #   l2 <- describe_lqas("ooh_coverage", "OOH-FMS")
      #   l3 <- describe_lqas("admin_coverage", "Admin")
      #   
      #   # Combine and print
      #   summary_text <- paste(c1, "", c2, "", c3, "", l1, "", l2, "", l3, sep = "\n")
      #   } else{
      #     summary_text <- paste(c1, "", c2, "", c3, sep = "\n")
      #   }
      #   title_text <- {
      #     loc <- if ("All" %in% region()) {
      #       NULL
      #     } else if ("All" %in% province()) {
      #       paste(region(), "Region")
      #     } else if ("All" %in% district()) {
      #       paste(province(), "Province")
      #     } else {
      #       paste(district(), "District")
      #     }
      #     
      #     if (is.null(loc)) {
      #       paste0(campaign_name(), ":  Triangulation – Correlation of District-Level OPV Coverage Across PCA, OOH-FMS, Admin and LQAS")
      #     } else {
      #       paste0(campaign_name(), ", ", loc, ": Triangulation – Correlation of District-Level OPV Coverage Across PCA, OOH-FMS, Admin and LQAS")
      #     }
      #   }
      #   
      #   # --- Format text
      #   text_style <- fp_text(font.size = 12)
      #   
      #   # --- Add bullets manually with newlines
      #   bullet_lines <- strsplit(summary_text, "\n")[[1]]
      #   bullet_text <- paste0(bullet_lines, collapse = "\n")
      #   
      #   # --- Single formatted paragraph with line breaks
      #   summary_fpar <- fpar(ftext(bullet_text, text_style))
      #   
      #   # --- Also format subtitle
      #   subtitle_fpar <- fpar(ftext(subtitle_text, text_style))
      #   
      #   my_pres <- my_pres %>%
      #     add_slide(layout = "template4", master = "Office Theme") %>%
      #     ph_with(value = plot_matrix, location = ph_location_label(ph_label = "Map Box")) %>%
      #     ph_with(value = summary_fpar, location = ph_location_label(ph_label = "Legend Box")) %>%
      #     ph_with(value = subtitle_fpar, location = ph_location_label(ph_label = "Text Box")) %>%
      #     ph_with(value = fpar(ftext(title_text, fp_text(font.size = 18, color = "white", bold = FALSE))),
      #             location = ph_location_label(ph_label = "Title Box")) %>%
      #     ph_with(
      #       value = fpar(
      #         ftext(paste0("Data Source: APMIS, extracted on ", Sys.Date()), fp_text(font.size = 7)),
      #         fp_p = fp_par(text.align = "right")
      #       ),
      #       location = ph_location_label(ph_label = "Footer Box")
      #     )
      # }
      }
      if(vaccine_type == "IPV"){
      if (is_valid_df(data_pca_ipv) && is_valid_df(data_ooh_ipv) && is_valid_df(data_lqas_ipv)){
        data_triangulation <- data_pca_ipv %>%
          mutate(value = as.numeric(value)) %>%
          mutate(value_cat = case_when(is.na(value) ~ NA_character_,
                                       value <= 0.95 ~ "Coverage ≤95% or One or More Lots Failed",
                                       value > 0.95 ~ "Coverage >95% or All Lots Passed"),
                 source = "Post-Campaign\nAssessment") %>%
          select(source, rcode, pcode, dcode, value_cat) %>%
          bind_rows(
            data_ooh_ipv %>%
              mutate(value = as.numeric(value)) %>%
              mutate(value_cat = case_when(is.na(value) ~ NA_character_,
                                           value <= 0.95 ~ "Coverage ≤95% or One or More Lots Failed",
                                           value > 0.95 ~ "Coverage >95% or All Lots Passed"),
                     source = "Out-of-House\nFinger-Mark Survey") %>%
              select(source, rcode, pcode, dcode, value_cat)
          ) %>%
          bind_rows(
            data_lqas_ipv %>%
              mutate(value_cat = case_when(is.na(value) ~ NA_character_,
                                           value == "One or More Lots Failed" ~ "Coverage ≤95% or One or More Lots Failed",
                                           value == "All Lots Passed" ~ "Coverage >95% or All Lots Passed"),
                     source = "LQAS") %>%
              select(source, rcode, pcode, dcode, value_cat)
          )
        
        # Ensure value_cat is a factor with all desired levels
        all_cats <- c("Coverage ≤95% or One or More Lots Failed", "Coverage >95% or All Lots Passed")  # replace with your full set of categories
        data_triangulation$value_cat <- factor(data_triangulation$value_cat, levels = all_cats)
        
        data_triangulation2 <- data_triangulation %>%
          group_by(rcode, pcode, dcode, value_cat) %>%
          summarise(count = n(), .groups = "drop") %>%
          pivot_wider(
            names_from = value_cat,
            values_from = count,
            values_fill = 0
          ) 
        if(!("Coverage ≤95% or One or More Lots Failed" %in% colnames(data_triangulation2))){
          data_triangulation2$`Coverage ≤95% or One or More Lots Failed` <- 0
        }
        data_triangulation2 <- data_triangulation2  %>%
          mutate(value_cat = case_when(`Coverage ≤95% or One or More Lots Failed` ==  3 ~ "PCA≤95%, OOH-FM Survey≤95% and LQAS failed",
                                       `Coverage ≤95% or One or More Lots Failed` ==  2 ~ "Either two (PCA≤95% or OOH-FM Survey≤95% or LQAS failed)",
                                       `Coverage ≤95% or One or More Lots Failed` ==  1 ~ "Either one (PCA≤95% or OOH-FM Survey≤95% or LQAS failed)",
                                       `Coverage ≤95% or One or More Lots Failed` ==  0 ~ "PCA>95%, OOH-FM Survey>95% and LQAS passed",
                                       TRUE ~ NA_character_),
                 source = "Combined" 
          )
        
        data_triangulation <- data_triangulation  %>%
          bind_rows(data_triangulation2)
        
        data_triangulation <- report_borders_district %>%
          inner_join(data_triangulation,
                     by = c("APMIS_RCODE" = "rcode",
                            "APMIS_PCODE" = "pcode",
                            "APMIS_DCODE" = "dcode")) %>%
          sf::st_as_sf()
        
        # Step 2: Add 'No Campaign' districts
        no_sia_districts <- report_borders_district %>%
          anti_join(campaign_rpd %>%
                      filter(campaign_name == campaign_name()),
                    by = c("APMIS_RCODE" = "rcode",
                           "APMIS_PCODE" = "pcode",
                           "APMIS_DCODE" = "dcode")) %>%
          rename(rcode = APMIS_RCODE, pcode = APMIS_PCODE, dcode = APMIS_DCODE) %>%
          select(rcode, pcode, dcode, geometry) %>%
          sf::st_as_sf()
        
        
        no_apmis_data_districts <- report_borders_district %>%
          inner_join(campaign_rpd %>%
                       filter(campaign_name == campaign_name()),
                     by=c("APMIS_RCODE" = "rcode",
                          "APMIS_PCODE" = "pcode",
                          "APMIS_DCODE" = "dcode")) %>%
          anti_join(data_triangulation %>% sf::st_drop_geometry(), by=c("APMIS_RCODE",
                                                                        "APMIS_PCODE",
                                                                        "APMIS_DCODE")) %>%
          sf::st_as_sf()
        
        # Step 3: Combine both groups
        
        # Add value_cat manually to other data
        no_apmis_data_districts$value_cat <- "No APMIS Data"
        no_sia_districts$value_cat <- "No Campaign"
        
        
        # Bind all
        combined_data <- bind_rows(data_triangulation, no_apmis_data_districts, no_sia_districts)
        
        # Step 4: Define color palette
        map_colors <- c(
          "PCA≤95%, OOH-FM Survey≤95% and LQAS failed" = "#e41a1c",
          "Either two (PCA≤95% or OOH-FM Survey≤95% or LQAS failed)" = "#ff7f00",
          "Either one (PCA≤95% or OOH-FM Survey≤95% or LQAS failed)" = "#ffff33",
          "PCA>95%, OOH-FM Survey>95% and LQAS passed" = "#4daf4a",
          "Coverage ≤95% or One or More Lots Failed"     = "#e41a1c",
          "Coverage >95% or All Lots Passed"      = "#4daf4a",
          "No Data" = "#d9d9d9"
        )
        
        # Step 5: Plot
        map_plot1 <-
          ggplot() +
          geom_sf(data = report_borders_district %>% mutate(value_cat = "No Data"), aes(fill = value_cat), size = 0.1, alpha = 0.75) +
          geom_sf(data = combined_data %>% filter(source != "Combined"), aes(fill = value_cat), size = 0.1, alpha = 0.75) +
          geom_sf(data = report_borders_province, fill = NA, color = "#252525", linewidth = 0.6) +
          geom_sf(data = report_borders_region, fill = NA, color = "black", linewidth = 0.8) +
          coord_sf(crs = st_crs(4326), datum = NA) +
          facet_wrap(~source, nrow = 1) +
          scale_fill_manual(
            values = map_colors,
            name = NULL
          ) +
          theme_void(base_size = 11) +
          theme(
            legend.position = "bottom", 
            strip.text = element_text(size = 11)  # enable facet labels
          )
        
        map_plot2 <-
          ggplot() +
          geom_sf(data = report_borders_district %>% mutate(value_cat = "No Data"), aes(fill = value_cat), size = 0.1, alpha = 0.75) +
          geom_sf(data = combined_data %>% filter(source == "Combined") %>%
                    mutate(value_cat = factor(value_cat, levels=c("PCA≤95%, OOH-FM Survey≤95% and LQAS failed",
                                                                  "Either two (PCA≤95% or OOH-FM Survey≤95% or LQAS failed)",
                                                                  "Either one (PCA≤95% or OOH-FM Survey≤95% or LQAS failed)",
                                                                  "PCA>95%, OOH-FM Survey>95% and LQAS passed", 
                                                                  "No Data"))) %>%
                    arrange(value_cat), aes(fill = value_cat), size = 0.1, alpha = 0.75) +
          geom_sf(data = report_borders_province, fill = NA, color = "#252525", linewidth = 0.4) +
          geom_sf(data = report_borders_region, fill = NA, color = "black", linewidth = 0.6) +
          coord_sf(crs = st_crs(4326), datum = NA) +
          facet_wrap(~source, nrow = 1) +
          scale_fill_manual(
            values = map_colors,
            name = "Combined Result"
          ) +
          theme_void(base_size = 13) +
          theme(
            legend.position = "right", 
            strip.text = element_blank()  # enable facet labels
          )
        
        title_text <- {
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
            paste0(campaign_name(), ": Triangulation Across Post-Campaign Monitoring Types (IPV)")
          } else {
            paste0(campaign_name(), ", ", loc, ": Triangulation Across Post-Campaign Monitoring Types (IPV)")
          }
        }
        
        
        my_pres <- my_pres %>%
          add_slide(layout = "triangulation1", master = "Office Theme") %>%
          ph_with(value = map_plot2, location = ph_location_label(ph_label = "Box1")) %>%
          ph_with(value = map_plot1, location = ph_location_label(ph_label = "Text Box")) %>%
          ph_with(value = fpar(ftext(title_text, fp_text(font.size = 18, color = "white", bold = FALSE))),
                  location = ph_location_label(ph_label = "Title Box")) %>%
          ph_with(
            value = fpar(
              ftext(paste0("Data Source: APMIS, extracted on ", Sys.Date()), fp_text(font.size = 7)),
              fp_p = fp_par(text.align = "right")
            ),
            location = ph_location_label(ph_label = "Footer Box")
          )
      }
      }
        #PCA Reasons Missed per 1000 slide
        
        ## Trend
        if(!("All" %in% district())){
          pca_trend_reasons <- postcampaign_data_trend$district_indicators
        } else{
          if(!("All" %in% province())){
            pca_trend_reasons <- postcampaign_data_trend$province_indicators
          } else{
            if(!("All" %in% region())){
              pca_trend_reasons <- postcampaign_data_trend$region_indicators
            } else{
              pca_trend_reasons <- postcampaign_data_trend$national_indicators
            }}}
        
        df <- pca_trend_reasons %>%
          filter(indicator == "pca_reasons_missed_rates") %>%
          select(campaign_name, category, numerator, denominator) %>%
          mutate(category = case_when(category == "absent_market_street" ~ "Absent",
                               category == "absent_school_madarasa_hf" ~ "Absent",
                               category == "absent_travel" ~ "Absent",
                               category == "absent_others" ~ "Absent",
                               
                               category == "child_not_available" ~ "Child Not Available",
                               
                               category == "mosque_is_far" ~ "Vaccination site is too far",
                               category == "house_far_from_site" ~ "Vaccination site is too far",
                               category == "site_is_far" ~ "Vaccination site is too far",
                               
                               category == "newborn" ~ "Newborn/Sleeping/Sick",
                               category == "sleep" ~ "Newborn/Sleeping/Sick",
                               category == "sick" ~ "Newborn/Sleeping/Sick",
                               
                               category == "no_one_home" ~ "No One Available to Take Child to Site",
                               category == "not_aware" ~ "Not Aware",
                               category == "refusal" ~ "Refusal",
                               
                               category == "refusal_decision_maker_notat_home" ~ "Refusal",
                               category == "refusal_misperception" ~ "Refusal",
                               
                               category == "team_did_not_visit" ~ "Team did not Visit",
                               category == "team_did_not_visit_the_site_area" ~ "Team did not Visit",
                               
                               category == "other_reasons" ~ "Other",
                               category == "other" ~ "Other",
                               TRUE ~ "Other"
          )) %>%
      group_by(campaign_name, category) %>%
      summarise(numerator = sum(numerator, na.rm=T),
                denominator = max(denominator, na.rm=T)) %>%
      ungroup() %>%
      mutate(value = (numerator/denominator)*1000) %>%
          left_join(df_campaigns %>%
                                                               select(campaign_name, campaign_startdate) %>%
                                                               distinct(),
                                                             by=c("campaign_name")) %>%
          arrange(campaign_startdate) 
      
      reasons_set <- df %>%
        filter(numerator != 0) %>%
        group_by(category) %>%
        summarise(numerator = sum(numerator, na.rm=T)) %>%
        ungroup() %>%
        arrange(desc(numerator)) %>%
        pull(category)
      
      df <- df %>%
        filter(category %in% reasons_set) %>%
        mutate(category = factor(category, levels = reasons_set)) %>%
        arrange(campaign_startdate) 
      
      df <- df %>%
        mutate(campaign_name = factor(campaign_name, levels=unique(df$campaign_name))) 
      
      df$category_wrapped <- str_wrap(df$category, width = 20)
      
      # reasons_trends_plot <- ggplot(data = df) +
      #   geom_point(aes(x = campaign_name, y = value)) +
      #   geom_text(aes(x = campaign_name, y = value, label = round(value, 0)), size = 3, vjust = -1) +
      #   geom_line(aes(x = campaign_name, y = value, group = 1)) +
      #   facet_wrap(~category_wrapped, nrow = 3) +
      #   scale_y_continuous(labels = scales::comma, limits = c(0, max(df$value, na.rm = TRUE) * 1.1)) +
      #   labs(y = "Missed per 1000 Screened") +
      #   theme_minimal() +
      #   theme(
      #     axis.title.x = element_blank(),
      #     axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
      #     legend.title = element_blank(),
      #     strip.background = element_rect(fill = "darkgreen", color = NA),
      #     strip.text = element_text(color = "white", face = "bold", size = 9),
      #     panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
      #   )
      
      if (is_valid_df(df)){
      reasons_trends_plot2 <- ggplot(data = df) +
        geom_point(aes(x = campaign_name, y = value)) +
        geom_text(aes(x = campaign_name, y = value, label = round(value, 0)), size = 3, vjust = -1) +
        geom_line(aes(x = campaign_name, y = value, group = 1)) +
        facet_wrap(~category_wrapped, nrow = 2) +
        scale_y_continuous(labels = scales::comma, limits = c(0, max(df$value, na.rm = TRUE) * 1.1)) +
        labs(y = "Missed per 1000 Screened") +
        theme_minimal() +
        theme(
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1),
          legend.title = element_blank(),
          strip.background = element_rect(fill = "darkgreen", color = NA),
          strip.text = element_text(color = "white", face = "bold", size = 9),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
        )
      } else{
        reasons_trends_plot2 <- NULL
      }
      
      df <- postcampaign_data_trend$district_indicators %>%
        filter(indicator == "pca_reasons_missed_rates") %>%
        filter(campaign_name == campaign_name()) %>%
        # filter(campaign_name == "May NID 2025") %>%
        select(rcode, pcode, dcode, category, numerator, denominator) %>%
        mutate(category = case_when(category == "absent_market_street" ~ "Absent",
                                    category == "absent_school_madarasa_hf" ~ "Absent",
                                    category == "absent_travel" ~ "Absent",
                                    category == "absent_others" ~ "Absent",
                                    
                                    category == "child_not_available" ~ "Child Not Available",
                                    
                                    category == "mosque_is_far" ~ "Vaccination site is too far",
                                    category == "house_far_from_site" ~ "Vaccination site is too far",
                                    category == "site_is_far" ~ "Vaccination site is too far",
                                    
                                    category == "newborn" ~ "Newborn/Sleeping/Sick",
                                    category == "sleep" ~ "Newborn/Sleeping/Sick",
                                    category == "sick" ~ "Newborn/Sleeping/Sick",
                                    
                                    category == "no_one_home" ~ "No One Available to Take Child to Site",
                                    category == "not_aware" ~ "Not Aware",
                                    category == "refusal" ~ "Refusal",
                                    
                                    category == "refusal_decision_maker_notat_home" ~ "Refusal",
                                    category == "refusal_misperception" ~ "Refusal",
                                    
                                    category == "team_did_not_visit" ~ "Team did not Visit",
                                    category == "team_did_not_visit_the_site_area" ~ "Team did not Visit",
                                    
                                    category == "other_reasons" ~ "Other",
                                    category == "other" ~ "Other",
                                    TRUE ~ "Other"
        )) %>%
        group_by(rcode, pcode, dcode, category) %>%
        summarise(numerator = sum(numerator, na.rm=T),
                  denominator = max(denominator, na.rm=T)) %>%
        ungroup() %>%
        mutate(value = (numerator/denominator)*1000) %>%
        mutate(value_cat = case_when(value > 40 ~ ">40",
                                     value >20 ~ "20-40",
                                     value >0 ~ "1-19",
                                     TRUE ~ "0")) %>%
        arrange(desc(value_cat))
      
      df <- df %>%
        filter(category %in% reasons_set) %>%
        mutate(category = factor(category, levels = reasons_set)) %>%
        arrange(category, desc(value_cat))
      
      
      df$category_wrapped <- str_wrap(df$category, width = 20)
      
      
      
      legend_title <- "Total Missed\nper 1000 Screened"
      
      report_borders_district <- report_borders_district()
      report_borders_province <- report_borders_district %>%
        group_by(APMIS_Region, APMIS_Province, APMIS_RCODE, APMIS_PCODE) %>%
        summarise(geometry = sf::st_union(geometry), .groups = "drop") 
      report_borders_region <- report_borders_district %>%
        group_by(APMIS_Region, APMIS_RCODE)  %>%
        summarise(geometry = sf::st_union(geometry), .groups = "drop")
      
      # Extract non-NA values from the specified variable
      data_cat <- sort(unique(df[["value_cat"]][!is.na(df[["value_cat"]]) & !(df[["value_cat"]] %in% c("na", "Na"))]))
      
      # pct, pca_reasons_missed, ooh_reasons_missed, lqas_reasons_missed, pca_awareness_source, pca_door_marking, lqas_result, pca_modality
      map_colors <- colors_reasons_missed_pca_general[names(colors_reasons_missed_pca_general) %in% data_cat]
      
      data <- df %>%
        inner_join(report_borders_district,
                   by=c("rcode" = "APMIS_RCODE", "pcode" = "APMIS_PCODE", "dcode" = "APMIS_DCODE")) %>%
        sf::st_as_sf()
      
      
      no_sia_districts <- report_borders_district %>%
        anti_join(campaign_rpd %>%
                    filter(campaign_name == campaign_name()),
                  by=c("APMIS_RCODE" = "rcode",
                       "APMIS_PCODE" = "pcode",
                       "APMIS_DCODE" = "dcode"))
      
      no_apmis_data_districts <- report_borders_district %>%
        inner_join(campaign_rpd %>%
                     filter(campaign_name == campaign_name()),
                   by=c("APMIS_RCODE" = "rcode",
                        "APMIS_PCODE" = "pcode",
                        "APMIS_DCODE" = "dcode")) %>%
        anti_join(data %>% sf::st_drop_geometry(), by=c("APMIS_RCODE" = "rcode",
                                                        "APMIS_PCODE" = "pcode",
                                                        "APMIS_DCODE" = "dcode"))
      
      # Add value_cat manually to other data
      no_apmis_data_districts$value_cat <- "No APMIS Data"
      no_sia_districts$value_cat <- "No Campaign"
      
      # Bind all
      # Step 1: Get unique category_wrapped values
      categories <- unique(data$category_wrapped)
      
      # Step 2: Expand no_apmis_data_districts and no_sia_districts
      expand_with_categories <- function(df, categories) {
        df %>%
          mutate(dummy = 1) %>%
          full_join(tibble(category_wrapped = categories, dummy = 1), by = "dummy") %>%
          select(-dummy)
      }
      
      # Step 3: Apply to both datasets
      no_apmis_expanded <- expand_with_categories(no_apmis_data_districts, categories)
      no_sia_expanded   <- expand_with_categories(no_sia_districts, categories)
      
      # Step 4: Bind all together
      combined_data <- bind_rows(data, no_apmis_expanded, no_sia_expanded)
      
      # Now plot
      
      if (is_valid_df(combined_data)){
      pca_reasons_map_plot2 <- ggplot() +
        geom_sf(data = combined_data, aes(fill = value_cat), alpha=0.7, linewidth = 0.1) +
        geom_sf(data=report_borders_province, fill=NA, color="#252525", linewidth = 0.2) +
        geom_sf(data=report_borders_region, fill=NA, color="black", linewidth = 0.4) +
        facet_wrap(~category_wrapped, nrow = 2) +
        coord_sf(crs = st_crs(4326), datum = NA) +
        scale_fill_manual(
          values = c(map_colors, "No APMIS Data" = "#525252", "No Campaign" = "#d9d9d9"),
          breaks = c(names(map_colors), "No APMIS Data", "No Campaign"),
          name = legend_title
        ) +
        theme_void() +
        theme(
          legend.title = element_text(size=9),
          legend.text = element_text(size = 9),
          legend.position = "bottom",
          strip.background = element_rect(fill = "darkgreen", color = NA),
          strip.text = element_text(color = "white", face = "bold", size = 9),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
        )
      } else{
        pca_reasons_map_plot2 <- NULL
      }
        
      title_text <- {
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
          paste0(campaign_name(), ": Total Missed due to Each Reason per 1000 Screened in PCA")
        } else {
          paste0(campaign_name(), ", ", loc, ": Total Missed due to Each Reason per 1000 Screened in PCA")
        }
      }
      
      if(!is.null(pca_reasons_map_plot2)){
      my_pres <- my_pres %>%
        add_slide(layout = "single_object_template", master = "Office Theme") %>%
        ph_with(value = pca_reasons_map_plot2, location = ph_location_label(ph_label = "Map Box")) %>%
        ph_with(value = fpar(ftext(title_text, fp_text(font.size = 18, color = "white", bold = FALSE))),
                location = ph_location_label(ph_label = "Title Box")) %>%
        ph_with(
          value = fpar(
            ftext(paste0("Data Source: APMIS, extracted on ", Sys.Date()), fp_text(font.size = 7)),
            fp_p = fp_par(text.align = "right")
          ),
          location = ph_location_label(ph_label = "Footer Box")
        )
      }
      
      if(!is.null(reasons_trends_plot2)){
      my_pres <- my_pres %>%
        add_slide(layout = "single_object_template", master = "Office Theme") %>%
        ph_with(value = reasons_trends_plot2, location = ph_location_label(ph_label = "Map Box")) %>%
        ph_with(value = fpar(ftext(title_text, fp_text(font.size = 18, color = "white", bold = FALSE))),
                location = ph_location_label(ph_label = "Title Box")) %>%
        ph_with(
          value = fpar(
            ftext(paste0("Data Source: APMIS, extracted on ", Sys.Date()), fp_text(font.size = 7)),
            fp_p = fp_par(text.align = "right")
          ),
          location = ph_location_label(ph_label = "Footer Box")
        )
      }
        
        # At the very end:
        removeNotification("report_notice")
        print(my_pres, target = file)
        triggered(Sys.time())
      }
    )
    # Return reactive that tracks download
    return(triggered)
  })
}