# Load required libraries
library(Rnssp)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(readr)
library(dplyr)

# You also have to update your out_dir to where you want the download to save
# for example: I want to download the data to the following directory in my PC
# C:/Users/NYALL/Documents/Trial
# so I set my out_dir to the following:

out_dir <- paste0(parent_dir, "/raw_data")

# the slash must be / and not \. If you wish to use \ then it must be double: \\
# Please update the out_dir directory to the relevant location for your download

setwd(out_dir)

# To do once, or when password changes - You need to put your essence username and password in
if (isTRUE(first_time)){
  myProfile <- create_profile()
  save(myProfile, file = paste0(parent_dir, "/myProfile.rda"))
}

load(paste0(parent_dir, "/myProfile.rda"))

do_download_historical_data <- TRUE # TRUE FALSE
do_download_addhoc_data <- TRUE # TRUE FALSE

# Helper: build DataDetails URL
build_datadetails_url <- function(
    start_date,
    end_date,
    field_list,
    geographySystem = "hospitalregion",
    geographies = NULL,          # NULL = no geography filter (pull all you have access to)
    datasource = "va_hosp",
    userId = 7410,
    medicalGroupingSystem = "essencesyndromes",
    timeResolution = "daily"
) {
  field_list_url <- paste0("&field=", paste(field_list, collapse = "&field="))
  
  geo_url <- ""
  if (!is.null(geographies) && length(geographies) > 0) {
    geo_url <- paste0("&geography=", paste(geographies, collapse = "&geography="))
  }
  
  base_url <- paste0(
    "https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails/csv?",
    "datasource=", datasource, "&",
    "startDate=1Jan2026&endDate=1Jan2026&",  # placeholders
    "medicalGroupingSystem=", medicalGroupingSystem, "&",
    "userId=", userId, "&",
    "percentParam=noPercent&aqtTarget=DataDetails&",
    "geographySystem=", geographySystem, "&",
    "detector=nodetectordetector&",
    "timeResolution=", timeResolution
  )
  
  url <- paste0(base_url, field_list_url, geo_url)
  
  change_dates(
    url,
    start_date = as.character(as.Date(start_date)),
    end_date   = as.character(as.Date(end_date))
  )
}

download_nssp_chunked <- function(
    end_date = Sys.Date(),
    months_back = 16, # we set this to 16 instead of 15 so we have roughly 30 days back up incase there is a lag issue
    refresh_days = 30,
    chunk = c("day"),
    out_dir,
    field_list = c(
      "C_Unique_Patient_ID",
      "DischargeDiagnosis",
      "ChiefComplaintParsed",
      "Age",
      "C_Visit_Date_Time",
      "C_Visit_Date_Source",
      "C_Patient_Class",
      "Region",
      "Hospital",
      "HospitalZip",
      "Patient_Zip",
      "DischargeDiagnosisUpdates",
      "DischargeDiagnosisMDTUpdates",
      "DischargeDisposition",
      "HasBeenAdmitted"
    ),
    geographySystem = "hospitalregion",
    geographies = NULL,
    datasource = "va_hosp",
    userId = 7410,
    medicalGroupingSystem = "essencesyndromes",
    timeResolution = "daily",
    combine = FALSE
) {
  if (chunk != "day") {
    stop("This version is designed for daily files only. Use chunk = 'day'.")
  }
  
  end_date <- as.Date(end_date)
  start_date <- end_date %m-% months(months_back)
  refresh_start <- max(start_date, end_date - (refresh_days - 1))
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  # Find existing daily files
  existing_files <- list.files(
    out_dir,
    pattern = "^NSSP_data_\\d{4}-\\d{2}-\\d{2}_to_\\d{4}-\\d{2}-\\d{2}\\.csv$",
    full.names = FALSE
  )
  
  # Only treat true daily files (start == end) as completed days
  file_dates <- regexec("^NSSP_data_(\\d{4}-\\d{2}-\\d{2})_to_(\\d{4}-\\d{2}-\\d{2})\\.csv$", existing_files)
  file_parts <- regmatches(existing_files, file_dates)
  
  existing_dates <- vapply(file_parts, function(x) {
    if (length(x) == 3 && x[2] == x[3]) x[2] else NA_character_
  }, character(1))
  
  existing_dates <- as.Date(existing_dates, format = "%Y-%m-%d")
  existing_dates <- existing_dates[!is.na(existing_dates)]
  
  # Full historical range and gap-fill dates
  target_dates <- seq.Date(start_date, end_date, by = "day")
  missing_dates_full <- setdiff(target_dates, existing_dates)
  
  # Always refresh the most recent N days, even if they already exist
  refresh_dates_recent <- seq.Date(refresh_start, end_date, by = "day")
  
  # First fill historical gaps, then refresh recent dates
  dates_to_download <- c(missing_dates_full, refresh_dates_recent)
  dates_to_download <- sort(unique(as.Date(dates_to_download, origin = "1970-01-01")))
  
  if (length(dates_to_download) == 0) {
    message("Your downloads are up to date")
    return(invisible(NULL))
  }
  
  message(sprintf(
    "Ensuring full coverage from %s to %s, then refreshing the most recent %d days (%s to %s).",
    start_date, end_date, refresh_days, refresh_start, end_date
  ))
  
  files <- character(0)
  
  for (d in dates_to_download) {
    s <- as.Date(d)
    e <- as.Date(d)
    
    url <- build_datadetails_url(
      start_date = s,
      end_date = e,
      field_list = field_list,
      geographySystem = geographySystem,
      geographies = geographies,
      datasource = datasource,
      userId = userId,
      medicalGroupingSystem = medicalGroupingSystem,
      timeResolution = timeResolution
    )
    
    out_file <- file.path(out_dir, sprintf("NSSP_data_%s_to_%s.csv", s, e))
    files <- c(files, out_file)
    
    if (s >= refresh_start) {
      message(sprintf("Refreshing %s ...", s))
    } else {
      message(sprintf("Downloading missing %s ...", s))
    }
    
    api_data <- get_api_data(url, fromCSV = TRUE)
    write.csv(api_data, out_file, row.names = FALSE)
  }
  
  if (!combine) {
    return(invisible(files))
  }
  
  message("Combining all daily files in requested range...")
  files_to_combine <- file.path(
    out_dir,
    sprintf("NSSP_data_%s_to_%s.csv", target_dates, target_dates)
  )
  files_to_combine <- files_to_combine[file.exists(files_to_combine)]
  
  df <- bind_rows(lapply(files_to_combine, readr::read_csv, show_col_types = FALSE))
  combined_path <- file.path(
    out_dir,
    sprintf("NSSP_data_%s_to_%s_COMBINED.csv", start_date, end_date)
  )
  write.csv(df, combined_path, row.names = FALSE)
  message(sprintf("Saved combined file: %s", combined_path))
  
  invisible(list(files = files, combined = combined_path))
}

download_nssp_chunked(out_dir = out_dir)
