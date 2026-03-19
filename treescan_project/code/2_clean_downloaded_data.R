# Load required libraries
library(dplyr)
library(stringr)

# Find path
path <- paste0(parent_dir, "/raw_data") 

# Load data
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Join each chunked data together
df_all <- do.call(rbind, lapply(files, read.csv, stringsAsFactors = FALSE))

# let's get data ready to put into create count file
key <- df_all$C_Unique_Patient_ID
date <- as.Date(df_all$C_Visit_Date_Time)

# Clean
df_all <- df_all %>%
  mutate(codes = sapply(DischargeDiagnosis, function(x) {
    paste(gsub("\\.", "", unlist(strsplit(x, ";"))), collapse = " ")
  }))

# Sort out severity
severity <- ifelse(df_all$C_Visit_Date_Source == "Admit", "A", "V")

# Clean
data <- data.frame("key" = key, "date" = date, "diagnosis_codes" = df_all$codes, "severity" = severity)
data <- data[which(data$diagnosis_codes != "NA"), ]

# Save data
write.table(
  data,
  file = paste0(parent_dir, "/data/dataset.txt"),
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)
