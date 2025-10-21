#remove everything not used in server
# Names of the dataframes to keep
keep_df <- c("df_apmis_list", "apmis_admin_data", "apmis_indicators", "precampaign_indicators",
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