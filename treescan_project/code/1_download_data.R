# Load required libraries
library(Rnssp)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(readr)
library(dplyr)

# install Rnssp package
devtools::install_github("cdcgov/Rnssp")
# Rnssp instructions: https://cdn.ymaws.com/www.cste.org/resource/resmgr/2016billets/RStudio_ESSENCE_API_Guide_D.html


# You also have to update your out_dir to where you want the download to save
# for example: I want to download the data to the following directory in my PC
# C:/Users/NYALL/Documents/Trial
# so I set my out_dir to the following:

out_dir <- paste0(parent_dir, "/raw_data")

# the slash must be / and not \. If you wish to use \ then it must be double: \\
# Please update the out_dir directory to the relevant location for your download

setwd(out_dir)

# To do once, or when password changes - You need to put your essence username and password in
myProfile <- create_profile()
save(myProfile, file = paste0(parent_dir, "/myProfile.rda"))

load("~/myProfile.rda")

do_download_historical_data <- TRUE # TRUE FALSE
do_download_addhoc_data <- TRUE # TRUE FALSE

# READ
# BEFORE
# RUNNING:

# You have to change the counties_list line yourself
# for example if you are running for the following states in Texas:
# Bastrop, Caldwell, Hays, Travis, Williamson,
# then you replace line number 35 with the code commented below after the # symbol
# counties_list <- c("tx_bastrop","tx_caldwell","tx_hays","tx_travis","tx_williamson")

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

# Main: chunked downloader
download_nssp_chunked <- function(
    end_date = Sys.Date(),
    months_back = 15,
    chunk = c("month", "week"),
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
      "Patient_Country",
      "Hospital"
    ),
    geographySystem = "hospitalregion",
    geographies = NULL,
    datasource = "va_hosp",
    userId = 7410,
    medicalGroupingSystem = "essencesyndromes",
    timeResolution = "daily",
    combine = FALSE
) {
  chunk <- match.arg(chunk)
  end_date <- as.Date(end_date)
  start_date <- end_date %m-% months(months_back)
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Build chunk boundaries
  by_unit <- if (chunk == "month") "1 month" else "1 week"
  breaks <- seq(from = floor_date(start_date, unit = chunk),
                to   = ceiling_date(end_date, unit = chunk),
                by   = by_unit)
  
  # Ensure we cover start_date/end_date exactly
  breaks[1] <- start_date
  if (tail(breaks, 1) < end_date) breaks <- c(breaks, end_date)
  
  message(sprintf("Pulling NSSP data in %s chunks: %s to %s", chunk, start_date, end_date))
  
  files <- c()
  
  for (i in seq_len(length(breaks) - 1)) {
    s <- as.Date(breaks[i])
    e <- as.Date(breaks[i + 1] - 1)  # inclusive end for chunk
    if (i == length(breaks) - 1) e <- end_date
    
    url <- build_datadetails_url(
      start_date = s, end_date = e,
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
    
    message(sprintf("Downloading %s to %s ...", s, e))
    
    # Stream directly to disk (avoids R 2GB string limit)
    api_data <- get_api_data(url, fromCSV = TRUE)
    write_csv(api_data, out_file)
  }
  
  if (!combine) return(invisible(files))
  
  # Optional combine (can be memory heavy; only do if you really need one object)
  message("Combining chunks...")
  df <- bind_rows(lapply(files, read_csv, show_col_types = FALSE))
  combined_path <- file.path(out_dir, sprintf("NSSP_data_%s_to_%s_COMBINED.csv", start_date, end_date))
  write_csv(df, combined_path)
  message(sprintf("Saved combined file: %s", combined_path))
  
  invisible(list(files = files, combined = combined_path))
}

download_nssp_chunked(out_dir = out_dir, chunk = "week", combine = FALSE)

# Main: chunked downloader FOR LATER EXTENSION
download_nssp_chunked_year <- function(
    year,
    months_back = 12,
    chunk = c("month", "week"),
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
      "Patient_Country",
      "Hospital"
    ),
    geographySystem = "hospitalregion",
    geographies = NULL,
    datasource = "va_hosp",
    userId = 7410,
    medicalGroupingSystem = "essencesyndromes",
    timeResolution = "daily",
    combine = FALSE
) {
  end_date = paste0(year,"-12-31")
  
  chunk <- match.arg(chunk)
  end_date <- as.Date(end_date)
  start_date <- paste0(year,"-01-01")
  
  out_dir <- "C:/Users/nj7786/Documents/treescan_project/data_for_interpretation"
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  # Build chunk boundaries
  by_unit <- if (chunk == "month") "1 month" else "1 week"
  breaks <- seq(from = floor_date(start_date, unit = chunk),
                to   = ceiling_date(end_date, unit = chunk),
                by   = by_unit)
  
  # Ensure we cover start_date/end_date exactly
  breaks[1] <- start_date
  if (tail(breaks, 1) < end_date) breaks <- c(breaks, end_date)
  
  message(sprintf("Pulling NSSP data in %s chunks: %s to %s", chunk, start_date, end_date))
  
  files <- c()
  
  for (i in seq_len(length(breaks) - 1)) {
    s <- as.Date(breaks[i])
    e <- as.Date(breaks[i + 1] - 1)  # inclusive end for chunk
    if (i == length(breaks) - 1) e <- end_date
    
    url <- build_datadetails_url(
      start_date = s, end_date = e,
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
    
    message(sprintf("Downloading %s to %s ...", s, e))
    
    # Stream directly to disk (avoids R 2GB string limit)
    api_data <- get_api_data(url, fromCSV = TRUE)
    write_csv(api_data, out_file)
  }
  
  if (!combine) return(invisible(files))
  
  # Optional combine (can be memory heavy; only do if you really need one object)
  message("Combining chunks...")
  df <- bind_rows(lapply(files, read_csv, show_col_types = FALSE))
  combined_path <- file.path(out_dir, sprintf("NSSP_data_%s_to_%s_COMBINED.csv", start_date, end_date))
  write_csv(df, combined_path)
  message(sprintf("Saved combined file: %s", combined_path))
  
  invisible(list(files = files, combined = combined_path))
}
