# Load required libraries
library(dplyr)
library(stringr)
library(data.table)

# Find path
path <- paste0(parent_dir, "/raw_data") 

# Load data
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Dates we want to get cleaned
dates_16m <- seq.Date(to = Sys.Date() - 1, from = Sys.Date() %m-% months(16), by = "day")

# Now get corresponding files
files_from_dates <- file.path(
  parent_dir,
  "raw_data",
  paste0("NSSP_data_", dates_16m, "_to_", dates_16m, ".csv")
)

# Join each chunked data together
df_all <- do.call(rbind, lapply(files_from_dates, read.csv, stringsAsFactors = FALSE))

# Set as data table for speed
setDT(df_all)

# Clean
df_all[, codes := vapply(DischargeDiagnosis, function(x) {
  if (is.na(x)) return(NA_character_)
  
  codes <- unlist(strsplit(x, ";", fixed = TRUE))
  codes <- unique(codes[codes != ""])
  
  if (length(codes) == 0) NA_character_ else paste(codes, collapse = " ")
}, character(1))]

# Sort out severity
# severity <- ifelse(df_all$C_Visit_Date_Source == "Admit", "A", "V")

# We update severity to use the "HasBeenAdmitted" variable
severity <- ifelse(df_all$HasBeenAdmitted == 1, "A", "V")

# let's get data ready to put into create count file
key <- df_all$C_Unique_Patient_ID
date <- as.Date(df_all$C_Visit_Date_Time)

# Clean
data <- data.frame("key" = key, "date" = date, "diagnosis_codes" = df_all$codes, "severity" = severity)
data <- data[which(data$diagnosis_codes != "NA"), ]

# Create output path
out_file <- file.path(
  parent_dir,
  "data",
  "datasets",
  paste0("dataset_", Sys.Date(), ".rds")
)

# Ensure directory exists
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

# Save data
saveRDS(data, file = out_file)

# Now get subset of data for the lag assessment (if lag assessment is required)

# What month-year are we in?
year_month <- format(Sys.Date(), "%Y-%m")

monthly_lags_assessed <- list.files(
  paste0(parent_dir, "/lag/curves"),
  pattern = "\\.rds$"
)

# Now create the lag data if today is the day we do lag assessment
if (!isTRUE(paste0("lag_curve_", year_month, ".rds") %in% monthly_lags_assessed)){
  
  dir.create(paste0(parent_dir, "/data/data for lag"), recursive = TRUE, showWarnings = FALSE)
  
  # First we need to define some key dates
  current_date <- Sys.Date()
  current_date_minus_14 <- Sys.Date() - 14
  current_date_minus_90 <- Sys.Date() - 90
  dates_for_lag <- seq.Date(from = current_date_minus_90, to = current_date_minus_14, by = "day")
  
  # Now subset data to time period we're testing for lag structure
  df_all_for_lag <- df_all[which(as.Date(df_all$C_Visit_Date_Time) %in% dates_for_lag), ]
  
  saveRDS(df_all_for_lag, paste0(parent_dir, "/data/data for lag/data_for_lag.rds"))
}
