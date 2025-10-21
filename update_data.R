# ------------------------------------------------------------------
# update_data.R
# Pulls the latest datasets from EMRO Analytics and saves them into
# revised_app/data/
# ------------------------------------------------------------------

local({
  # ---- Configuration ----
  base_url <- "https://emro-polio-analytics.org/data/"
  files <- c("all_apmis_data_list.Rds", "cva_data.Rds", "cva_data_date.Rds")
  dest_dir <- file.path("revised_app", "data")
  
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
  
  # ---- Ensure output directory exists ----
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  
  # ---- Download loop ----
  for (f in files) {
    data_url <- paste0(base_url, f)
    dest_file <- file.path(dest_dir, f)
    
    cat("\nDownloading:", f, "...\n")
    
    resp <- GET(
      url = data_url,
      authenticate(username, password),
      write_disk(dest_file, overwrite = TRUE),
      timeout(120)
    )
    
    if (status_code(resp) == 200) {
      cat("✓ Successfully saved:", normalizePath(dest_file), "\n")
    } else {
      cat("✗ Failed to download", f, "(status:", status_code(resp), ")\n")
    }
  }
  
  cat("\nAll downloads completed.\n")
})

