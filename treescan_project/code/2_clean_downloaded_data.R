# Load required libraries
library(dplyr)
library(stringr)

# Find path
path <- paste0(parent_dir, "/raw_data") 

# Load data
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Join each chunked data together
df_all <- do.call(rbind, lapply(files, read.csv, stringsAsFactors = FALSE))

# Before we want to finish cleaning we need to assess the lag

# First we need to define some key dates
current_date <- Sys.Date()
current_date_minus_14 <- Sys.Date() - 14
current_date_minus_90 <- Sys.Date() - 90
dates_for_lag <- seq.Date(from = current_date_minus_90, to = current_date_minus_14, by = "day")

# Now subset data to time period we're testing for lag structure
df_all_for_lag <- df_all[which(as.Date(df_all$C_Visit_Date_Time) %in% dates_for_lag), ]

# Now source in the lag assessment script
source(paste0(parent_dir, "/code/2.1_assess_lag.R"))

# Set file date for identifying dataset
END_DATE <- Sys.Date() - optimal_minimal_lag
START_DATE <- END_DATE - 90
dates_for_treescan <- seq.Date(from = START_DATE, to = END_DATE, by = "day")

# let's get data ready to put into create count file
key <- df_all$C_Unique_Patient_ID
date <- as.Date(df_all$C_Visit_Date_Time)

# Clean
df_all <- df_all %>%
  mutate(codes = sapply(DischargeDiagnosis, function(x) {
    paste(gsub("\\.", "", unlist(strsplit(x, ";"))), collapse = " ")
  }))

# Sort out severity
# severity <- ifelse(df_all$C_Visit_Date_Source == "Admit", "A", "V")

# We update severity to use the "HasBeenAdmitted" variable
severity <- ifelse(df_all$HasBeenAdmitted == 1, "A", "V")

# Clean
data <- data.frame("key" = key, "date" = date, "diagnosis_codes" = df_all$codes, "severity" = severity)
data <- data[which(data$diagnosis_codes != "NA"), ]

# Save data
write.table(
  data,
  file = paste0(parent_dir, "/data/datasets/dataset_", END_DATE, ".txt"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)
