#remove everything not used in server
# Names of the dataframes to keep
keep_df <- c("df_apmis_list", "icm_indicators", "apmis_admin_data", "apmis_indicators", "precampaign_indicators",
             "shp_districts", "shp_provinces", "shp_regions", "shp_clusters",
             "district_centroids", "province_centroids", "region_centroids", "cluster_centroids",
             "df_campaigns", "rpd_list", "campaign_rpdc", "campaign_rpd", "post_campaign_data_validation") 

# List all objects in the environment
all_objects <- ls()

# Identify dataframes that are not in the keep list
remove_df <- setdiff(all_objects[sapply(all_objects, function(x) is.data.frame(get(x)))], keep_df)

# Remove the identified dataframes
rm(list = remove_df)

# Clear up memory
gc()

all_data_list <- list("df_apmis_list" = df_apmis_list,
                      "icm_indicators" = icm_indicators,
                      "apmis_admin_data" = apmis_admin_data,
                      "apmis_indicators" = apmis_indicators,
                      "precampaign_indicators" = precampaign_indicators,
                      "shp_clusters" = shp_clusters,
                      "shp_districts" = shp_districts,
                      "shp_provinces" = shp_provinces,
                      "shp_regions" = shp_regions,
                      "cluster_centroids" = cluster_centroids,
                      "district_centroids" = district_centroids,
                      "province_centroids" = province_centroids,
                      "region_centroids" = region_centroids,
                      "df_campaigns" = df_campaigns,
                      "campaign_rpdc" = campaign_rpdc,
                      "campaign_rpd" = campaign_rpd,
                      "post_campaign_data_validation" = post_campaign_data_validation)