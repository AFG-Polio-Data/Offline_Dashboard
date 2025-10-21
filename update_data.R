# ------------------------------------------------------------------
# update_data.R
# Downloads all required datasets from EMRO Analytics
# Saves to revised_app/data/, creating the folder if missing.
# Username is fixed as 'cdc_emro'; password is prompted.
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
  
  # ---- Credentials ----
  username <- "cdc_emro"
  cat("Logging in as:", username, "\n")
  password <- getPass::getPass("Password: ")
  
  # ---- Ensure destination folder exists ----
  if (!dir.exists(dest_dir)) {
    cat("Creating data folder:", dest_dir, "\n")
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # ---- Download loop ----
  for (f in files) {
    data_url <- paste0(base_url, f)
    dest_file <- file.path(dest_dir, f)
    
    cat("\nDownloading:", f, "...\n")
    
    tmp_file <- tempfile(fileext = ".Rds")
    
    resp <- tryCatch({
      GET(
        url = data_url,
        authenticate(username, password),
        write_disk(tmp_file, overwrite = TRUE),
        timeout(120)
      )
    }, error = function(e) e)
    
    if (inherits(resp, "error")) {
      cat("✗ Error downloading", f, ":", resp$message, "\n")
    } else if (status_code(resp) == 200 && file.exists(tmp_file)) {
      # Try rename first; if it fails (e.g., different drive), fall back to copy
      ok <- tryCatch({
        file.rename(tmp_file, dest_file)
      }, warning = function(w) FALSE, error = function(e) FALSE)
      
      if (!ok) {
        file.copy(tmp_file, dest_file, overwrite = TRUE)
        unlink(tmp_file)
      }
      
      if (file.exists(dest_file)) {
        cat("✓ Saved:", normalizePath(dest_file), "\n")
      } else {
        cat("✗ Failed to save", f, "\n")
      }
    } else {
      cat("✗ Failed to download", f, "(status:", status_code(resp), ")\n")
    }
  }
  
  cat("\nAll downloads completed.\n")
})

