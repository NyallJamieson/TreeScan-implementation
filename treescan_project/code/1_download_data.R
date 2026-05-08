# Load required libraries
library(Rnssp)
library(tidyverse)
library(lubridate)
library(readr)

out_dir <- file.path(parent_dir, "raw_data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# To do once, or when password changes
if (isTRUE(first_time)) {
  myProfile <- create_profile()
  save(myProfile, file = file.path(parent_dir, "myProfile.rda"))
}

load(file.path(parent_dir, "myProfile.rda"))

# Helper: build DataDetails URL
build_datadetails_url <- function(
    start_date,
    end_date,
    field_list,
    geographySystem = "hospitalregion",
    geographies = NULL,
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
  
  paste0(
    "https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails/csv?",
    "datasource=", datasource,
    "&startDate=", format(as.Date(start_date), "%d%b%Y"),
    "&endDate=", format(as.Date(end_date), "%d%b%Y"),
    "&medicalGroupingSystem=", medicalGroupingSystem,
    "&userId=", userId,
    "&percentParam=noPercent",
    "&aqtTarget=DataDetails",
    "&geographySystem=", geographySystem,
    "&detector=nodetectordetector",
    "&timeResolution=", timeResolution,
    field_list_url,
    geo_url
  )
}

# Write one bulk chunk out as daily files
write_daily_files <- function(df, out_dir, date_col = "C_Visit_Date_Time") {
  if (!date_col %in% names(df)) {
    stop(sprintf("Column '%s' not found in data.", date_col))
  }
  
  df <- df %>%
    mutate(file_date = as.Date(.data[[date_col]]))
  
  bad_dates <- is.na(df$file_date)
  if (any(bad_dates)) {
    warning(sprintf(
      "%s rows had missing/unparseable %s and were skipped.",
      sum(bad_dates), date_col
    ))
    df <- df[!bad_dates, , drop = FALSE]
  }
  
  if (nrow(df) == 0) {
    message("No valid rows to write after date parsing.")
    return(invisible(character(0)))
  }
  
  split_df <- split(df, df$file_date)
  out_files <- character(0)
  
  for (d in names(split_df)) {
    daily_df <- split_df[[d]] %>% select(-file_date)
    out_file <- file.path(out_dir, sprintf("NSSP_data_%s_to_%s.csv", d, d))
    readr::write_csv(daily_df, out_file)
    out_files <- c(out_files, out_file)
  }
  
  invisible(out_files)
}

# Read which daily files already exist
get_existing_daily_dates <- function(out_dir) {
  existing_files <- list.files(
    out_dir,
    pattern = "^NSSP_data_\\d{4}-\\d{2}-\\d{2}_to_\\d{4}-\\d{2}-\\d{2}\\.csv$",
    full.names = FALSE
  )
  
  m <- regexec(
    "^NSSP_data_(\\d{4}-\\d{2}-\\d{2})_to_(\\d{4}-\\d{2}-\\d{2})\\.csv$",
    existing_files
  )
  parts <- regmatches(existing_files, m)
  
  dates <- vapply(parts, function(x) {
    if (length(x) == 3 && x[2] == x[3]) x[2] else NA_character_
  }, character(1))
  
  as.Date(dates[!is.na(dates)])
}

# Build up to 4 backfill chunks over missing historical range
make_backfill_chunks <- function(start_date, end_date, max_chunks = 4) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  if (start_date > end_date) {
    return(list())
  }
  
  total_days <- as.integer(end_date - start_date) + 1
  n_chunks <- min(max_chunks, total_days)
  base_size <- total_days %/% n_chunks
  remainder <- total_days %% n_chunks
  
  chunks <- vector("list", n_chunks)
  current_start <- start_date
  
  for (i in seq_len(n_chunks)) {
    this_size <- base_size + if (i <= remainder) 1 else 0
    current_end <- current_start + days(this_size - 1)
    
    chunks[[i]] <- list(
      start_date = current_start,
      end_date = current_end
    )
    
    current_start <- current_end + days(1)
  }
  
  chunks
}

download_nssp_backfill_and_refresh <- function(
    end_date = final_date,
    months_back = 16,
    refresh_days = 30,
    out_dir,
    field_list = c(
      "C_Unique_Patient_ID",
      "DischargeDiagnosis",
      "C_Visit_Date_Time",
      "DischargeDiagnosisUpdates",
      "DischargeDiagnosisMDTUpdates",
      "HasBeenAdmitted",
      "Region"
    ),
    geographySystem = "hospitalregion",
    geographies = NULL,
    datasource = "va_hosp",
    userId = 7410,
    medicalGroupingSystem = "essencesyndromes",
    timeResolution = "daily",
    date_col = "C_Visit_Date_Time"
) {
  end_date <- as.Date(end_date)
  full_start_date <- end_date %m-% months(months_back)
  refresh_start <- end_date - days(refresh_days - 1)
  
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }
  
  target_dates <- seq.Date(full_start_date, end_date, by = "day")
  existing_dates <- get_existing_daily_dates(out_dir)
  existing_dates <- existing_dates[existing_dates >= full_start_date & existing_dates <= end_date]
  
  missing_dates <- setdiff(target_dates, existing_dates)
  
  # Historical backfill only covers dates before refresh window
  historical_missing <- missing_dates[missing_dates < refresh_start]
  
  results <- list(
    backfill_files = character(0),
    refresh_files = character(0)
  )
  
  # BACKFILL SECTION
  if (length(historical_missing) == 0) {
    message("No historical backfill needed before refresh window.")
  } else {
    backfill_start <- min(historical_missing)
    backfill_end <- max(historical_missing)
    
    backfill_chunks <- make_backfill_chunks(
      start_date = backfill_start,
      end_date = backfill_end,
      max_chunks = 4
    )
    
    message(sprintf(
      "Historical backfill needed from %s to %s. Downloading in %s chunk(s).",
      backfill_start, backfill_end, length(backfill_chunks)
    ))
    
    for (i in seq_along(backfill_chunks)) {
      s <- backfill_chunks[[i]]$start_date
      e <- backfill_chunks[[i]]$end_date
      
      message(sprintf("Backfill chunk %s/%s: %s to %s", i, length(backfill_chunks), s, e))
      
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
      
      t0 <- Sys.time()
      api_data <- get_api_data(url, fromCSV = TRUE)
      t1 <- Sys.time()
      
      message(sprintf(
        "  Downloaded %s rows in %.1f sec",
        format(nrow(api_data), big.mark = ","),
        as.numeric(difftime(t1, t0, units = "secs"))
      ))
      
      written <- write_daily_files(api_data, out_dir = out_dir, date_col = date_col)
      results$backfill_files <- c(results$backfill_files, written)
      
      message(sprintf("  Wrote %s daily file(s)", length(written)))
    }
  }
  
  # REFRESH SECTION
  refresh_dates <- seq.Date(refresh_start, end_date, by = "day")
  refresh_file_paths <- file.path(
    out_dir,
    sprintf("NSSP_data_%s_to_%s.csv", refresh_dates, refresh_dates)
  )
  
  existing_refresh_files <- refresh_file_paths[file.exists(refresh_file_paths)]
  if (length(existing_refresh_files) > 0) {
    file.remove(existing_refresh_files)
    message(sprintf(
      "Removed %s existing daily file(s) in refresh window (%s to %s).",
      length(existing_refresh_files), refresh_start, end_date
    ))
  }
  
  message(sprintf(
    "Refreshing last %s days: %s to %s",
    refresh_days, refresh_start, end_date
  ))
  
  refresh_url <- build_datadetails_url(
    start_date = refresh_start,
    end_date = end_date,
    field_list = field_list,
    geographySystem = geographySystem,
    geographies = geographies,
    datasource = datasource,
    userId = userId,
    medicalGroupingSystem = medicalGroupingSystem,
    timeResolution = timeResolution
  )
  
  t0 <- Sys.time()
  refresh_data <- get_api_data(refresh_url, fromCSV = TRUE)
  t1 <- Sys.time()
  
  message(sprintf(
    "  Refreshed %s rows in %.1f sec",
    format(nrow(refresh_data), big.mark = ","),
    as.numeric(difftime(t1, t0, units = "secs"))
  ))
  
  refresh_written <- write_daily_files(refresh_data, out_dir = out_dir, date_col = date_col)
  results$refresh_files <- refresh_written
  
  message(sprintf("  Wrote %s refreshed daily file(s)", length(refresh_written)))
  
  invisible(results)
}

# Run it
download_nssp_backfill_and_refresh(
  out_dir = out_dir
)
