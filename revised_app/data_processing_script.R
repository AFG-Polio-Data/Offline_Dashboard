# === Initialize ===============================================================
print("load_packages_and_functions")
source("data_processing/load_packages_and_functions.R")
print("load_data")
source("data_processing/load_data.R")

# === Metadata Processing ======================================================
print("remove_unneccessary_campaigns")
source("data_processing/remove_unneccessary_campaigns.R")
print("process_rpdc_metadata")
source("data_processing/process_rpdc_metadata.R")

# === Add campaign day where missing ===========================================
print("general_date_cleanup")
source("data_processing/general_date_cleanup.R")

# === Post-Campaign Indicators =================================================
print("pca_indicators.R")
source("data_processing/pca_indicators.R")
print("lqas_indicators.R")
source("data_processing/lqas_indicators.R")
print("create_lqas_long_format.R")
source("data_processing/create_lqas_long_format.R")
print("ooh_fms_indicators.R")
source("data_processing/ooh_fms_indicators.R")
print("combine_post_campaign_indicators.R")
source("data_processing/combine_post_campaign_indicators.R")

# === Admin Data Processing ====================================================
print("admin_h2h_processing.R")
source("data_processing/admin_h2h_processing.R")
print("admin_m2m_processing.R")
source("data_processing/admin_m2m_processing.R")

print("admin_fipv_processing.R")
source("data_processing/admin_fipv_processing.R")

print("admin_s2s_processing.R")
source("data_processing/admin_s2s_processing.R")
print("admin_7day_campaign_processing.R")
source("data_processing/admin_7day_campaign_processing.R")
print("combine_admin_data.R")
source("data_processing/combine_admin_data.R")

# === Admin Indicators =========================================================
print("admin_modality_summary.R")
source("data_processing/admin_modality_summary.R")
print("admin_calculate_max_days.R")
source("data_processing/admin_calculate_max_days.R")
print("admin_summary.R")
source("data_processing/admin_summary.R")
print("admin_completeness.R")
source("data_processing/admin_completeness.R")
print("combine_admin_indicators.R")
source("data_processing/combine_admin_indicators.R")

# === Pre-Campaing Indicators ==================================================
print("precampaign_indicators.R")
source("data_processing/precampaign_indicators.R")

# === Environment Cleanup ======================================================
print("remove_interim_files.R")
source("data_processing/remove_interim_files_1.R")

# === ICM Data Processing ======================================================
print("icm_supervisor_processing.R")
source("data_processing/icm_supervisor_processing.R")
print("icm_revisit_processing.R")
source("data_processing/icm_revisit_processing.R")
print("icm_team_processing.R")
source("data_processing/icm_team_processing.R")
print("icm_household_processing.R")
source("data_processing/icm_household_processing.R")
print("icm_site_processing.R")
source("data_processing/icm_site_processing.R")
print("icm_session_processing.R")
source("data_processing/icm_session_processing.R")

# === ICM Indicators ===========================================================
print("icm_household_indicators.R")
source("data_processing/icm_household_indicators.R")
print("icm_combine_pct_indicators.R")
source("data_processing/icm_combine_pct_indicators.R")
print("combine_icm_indicators.R")
source("data_processing/combine_icm_indicators.R")

# === Environment Cleanup ======================================================
print("remove_interim_files.R")
source("data_processing/remove_interim_files.R")

# === Prepare files exported through 'Download Data' Page  =====================
print("export_admin_h2h_daywise.R")
source("data_processing/export_admin_h2h_daywise.R")
print("export_admin_s2s_m2m_daywise.R")
source("data_processing/export_admin_s2s_m2m_daywise.R")
print("export_admin_district_coverage.R")
source("data_processing/export_admin_district_coverage.R")
print("export_admin_combine_files.R")
source("data_processing/export_admin_combine_files.R")
print("export_admin_conversion.R")
source("data_processing/export_admin_conversion.R")
print("export_pca.R")
source("data_processing/export_pca.R")
print("export_lqas.R")
source("data_processing/export_lqas.R")
print("export_ooh_fms.R")
source("data_processing/export_ooh_fms.R")
print("export_icm.R")
source("data_processing/export_icm.R")
print("export_completeness.R")
source("data_processing/export_completeness.R")

# === Remove any lingering grouping  ===========================================
print("ungroup_all_files.R")
source("data_processing/ungroup_all_files.R")

# === Save and Finish  =========================================================
print("save")
saveRDS(all_data_list, "data/all_apmis_data_list.Rds")

rm(list=ls())
