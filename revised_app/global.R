# === R Packages =========================
# Remove Shiny package
# detach("package:shiny", unload = TRUE)
# remove.packages("shiny")
# 
# # Reinstall Shiny package with version 1.8.0
# install.packages("shiny_1.8.0.tar.gz", repos=NULL, type="source")
# renv::install("shiny_1.8.0.tar.gz", repos=NULL, type="source")
# # Load the newly installed version of Shiny
# library(shiny)
# 
# # Check the version of Shiny installed
# packageVersion("shiny")

library(colorspace)
library(patchwork)
library(flextable)
library(officer)
library(rvg)
library(rsconnect)
library(httr)
library(shinyjs)
library(manipulateWidget)
library(leaflet.minicharts)
library(leaflet.extras)
library(ISOweek)
library(htmlwidgets)
library(leaflet.extras2)
library(readr)
library(fresh)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus) #devtools::install_github("RinteRface/shinydashboardPlus")
library(shinycssloaders)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(lubridate)
library(sf)
library(DT)
library(here)
library(jsonlite)
library(purrr)
library(rio)
library(stringr)
library(tidyverse)
library(curl)
# library(OpenStreetMap)
library(plotly)
library(shinymanager)
library(scrypt)
library(shinyWidgets)
library(formattable)
library(leaflet)
library(openxlsx)
library(RColorBrewer)
library(stringi)
library(sparkline)
library(scales)
library(janitor)


# === Load Helper Functions =========================
source("components/funcs.R")

# === Data Import =========================
all_apmis_data_list <- read_rds("data/all_apmis_data_list.Rds")
df_apmis_list <- all_apmis_data_list$df_apmis_list
icm_indicators <- all_apmis_data_list$icm_indicators
pre_indicators <- all_apmis_data_list$precampaign_indicators
apmis_admin_data <- all_apmis_data_list$apmis_admin_data
apmis_indicators <- all_apmis_data_list$apmis_indicators
precampaign_indicators <- all_apmis_data_list$precampaign_indicators
shp_clusters <- all_apmis_data_list$shp_clusters
shp_districts <- all_apmis_data_list$shp_districts
shp_provinces <- all_apmis_data_list$shp_provinces
shp_regions <- all_apmis_data_list$shp_regions
shp_cluster_pts <- all_apmis_data_list$cluster_centroids
shp_district_pts <- all_apmis_data_list$district_centroids
shp_provinces_pts <- all_apmis_data_list$province_centroids
shp_regions_pts <- all_apmis_data_list$region_centroids
df_campaigns <- all_apmis_data_list$df_campaigns
campaign_rpd <- all_apmis_data_list$campaign_rpd
campaign_rpdc <- all_apmis_data_list$campaign_rpdc
post_campaign_validation <- all_apmis_data_list$post_campaign_data_validation

cva_data <- readRDS("data/cva_data.Rds")
cva_data_date <- format(readRDS("data/cva_data_date.Rds")[[1]], "%Y-%b-%d")

campaigns_post_may_2025 <- df_campaigns$campaign_name[df_campaigns$campaign_startdate >= "2025-05-01"]

acronyms_df <- rio::import("data/acronyms.xlsx")

# === Load Modules =========================
source("modules/sidebar/logout_module.R", local=TRUE)
source("modules/sidebar/userManagementModule.R", local=TRUE)

source("modules/tab_intro/intro_tab.R", local=TRUE)

source("modules/tab_overview/overviewCardsModule.R", local=TRUE)
source("modules/tab_overview/overviewPcaMapModule.R", local=TRUE)
source("modules/tab_overview/overviewAdminMapModule.R", local=TRUE)
source("modules/tab_overview/overviewLqasMapModule.R", local=TRUE)
source("modules/tab_overview/overviewTableModule.R", local=TRUE)

source("modules/tab_trends/trendMapsModule.R", local=TRUE)
source("modules/tab_trends/trendsTableModule.R", local=TRUE)
source("modules/tab_trends/trendsTotalCampaignsMap.R", local=TRUE)
source("modules/tab_trends/trendsAvgIndicatorMap.R", local=TRUE)
source("modules/tab_trends/trendsAvgIndicatorCards.R", local=TRUE)
source("modules/tab_trends/trendsChartModule.R", local=TRUE)
source("modules/tab_trends/trendsRightPanelModule.R", local=TRUE)
source("modules/tab_trends/trendsClusterCardsModule.R", local=TRUE)

source("modules/tab_postcampaign/postcampaignMapModule.R", local=TRUE)
source("modules/tab_postcampaign/postcampaignChartModule.R", local=TRUE)
source("modules/tab_postcampaign/postcampaignCardsModule.R", local=TRUE)
source("modules/tab_postcampaign/postcampaignRightPanelModule.R", local=TRUE)
source("modules/tab_postcampaign/postcampaignTableModule.R", local=TRUE)

source("modules/tab_admin/adminMapModule.R", local=TRUE)
source("modules/tab_admin/adminCardsModule.R", local=TRUE)
source("modules/tab_admin/adminChartModule.R", local=TRUE)
source("modules/tab_admin/adminTableModule.R", local=TRUE)

source("modules/tab_icm/icmMapModule.R", local=TRUE)
source("modules/tab_icm/icmCardModule.R", local=TRUE)
source("modules/tab_icm/icmChartModule.R", local=TRUE)
source("modules/tab_icm/icmRightPanelModule.R", local=TRUE)
source("modules/tab_icm/icmTableModule.R", local=TRUE)

source("modules/tab_pre/preMapModule.R", local=TRUE)
source("modules/tab_pre/preCardModule.R", local=TRUE)
source("modules/tab_pre/preChartModule.R", local=TRUE)
source("modules/tab_pre/preTableModule.R", local=TRUE)

source("modules/tab_reports/slidesModule.R", local=TRUE)
source("modules/tab_reports/icm_slidesModule.R", local=TRUE)

source("modules/tab_downloads/downloadModule.R", local=TRUE)

source("modules/tab_cva/cvaMapModule.R", local=TRUE)
source("modules/tab_cva/cvaCardModule.R", local=TRUE)
source("modules/tab_cva/cvaChartModule.R", local=TRUE)
source("modules/tab_cva/cvaTrendsModule.R", local=TRUE)
source("modules/tab_cva/cvaTableModule.R", local=TRUE)

# === Load Color Schemes =========================
source("components/color_schemes.R", local=TRUE)

