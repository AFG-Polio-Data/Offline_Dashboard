# ------------------------------------------------------------------
# update_data.R
# Pulls the latest all_apmis_data_list.Rds file from EMRO Analytics
# and saves it into revised_app/data/
# Prompts user for username and password at runtime
# ------------------------------------------------------------------

# ---- Configuration ----
data_url <- "https://emro-polio-analytics.org/data/all_apmis_data_list.Rds"
dest_dir <- file.path("revised_app", "data")
dest_file <- file.path(dest_dir, "all_apmis_data_list.Rds")

# ---- Libraries ----
if (!requireNamespace("httr", quietly = TRUE)) {
  install.packages("httr", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("getPass", quietly = TRUE)) {
  install.packages("getPass", repos = "https://cloud.r-project.org")
}

library(httr)
library(getPass)

# ---- Ask for credentials ----
cat("Please enter your EMRO Analytics credentials.\n")
username <- readline(prompt = "Username: ")
password <- getPass::getPass("Password: ")

# ---- Create folder if needed ----
if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

# ---- Download file ----
cat("\nDownloading latest data file...\n")

resp <- GET(
  url = data_url,
  authenticate(username, password),
  write_disk(dest_file, overwrite = TRUE),
  timeout(120)
)

if (status_code(resp) == 200) {
  cat("✓ Data successfully downloaded and saved to:\n  ", normalizePath(dest_file), "\n")
} else {
  stop("✗ Download failed. Status code: ", status_code(resp))
}
