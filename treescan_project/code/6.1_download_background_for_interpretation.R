# This script is to fill in this year and three previous years of data

# Create function to get download for 3 years back
download_nssp_chunked_background <- function(
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
      "HasBeenAdmitted",
      "Sex",
      "C_Race",
      "C_Ethnicity",
      "Admit_Reason_Code",
      "ModeOfArrival",
      "Travel_History",
      "TriageNotesParsed", 
      "Discharge_Date_Time"
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
  
  # Get start date of the script "1_...R" 16 month NSSP data download
  # This will be the upper bound on when for this extra data gathering
  end_date <- END_DATE %m-% months(16) - 1
  
  start_date <- paste0(year(Sys.Date()) - 3, "-01-01")
  
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
  target_dates <- seq.Date(as.Date(start_date), as.Date(end_date), by = "day")
  missing_dates_full <- setdiff(target_dates, existing_dates)
  
  # First fill historical gaps, then refresh recent dates
  dates_to_download <- c(missing_dates_full)
  dates_to_download <- sort(unique(as.Date(dates_to_download, origin = "1970-01-01")))
  
  if (length(dates_to_download) == 0) {
    message("Your downloads are up to date")
    return(invisible(NULL))
  }
  
  files <- character(0)
  
  for (d in dates_to_download) {
    print(paste0("Downloading ", as.Date(d)))
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

download_nssp_chunked_background(out_dir = out_dir)

