# Load required libraries
library(Rnssp)
library(dplyr)
library(lubridate)
library(readr)

# Where we save these files temporarily
out_dir2 <- file.path(parent_dir, "data_for_interpretation")
dir.create(out_dir2, recursive = TRUE, showWarnings = FALSE)

# Get the data in the way ESSENCE API wants
fmt_essence_date <- function(x) {
  x <- as.Date(x)
  paste0(as.integer(format(x, "%d")), format(x, "%b%Y"))
}

# Get nodes formatted correctly
fmt_node <- function(Nodes) {
  paste0("&dischargeDiagnosis=%5E", Nodes, "%5E", collapse = "")
}

# Get node API coding
node_code <- fmt_node(Nodes)

# This code builds the url to download the data
build_url <- function(start_date, end_date, diagnosis_code = node) {
  paste0(
    "https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails/csv?",
    "datasource=va_er",
    "&startDate=", fmt_essence_date(start_date),
    "&medicalGroupingSystem=essencesyndromes",
    "&userId=8230",
    "&endDate=", fmt_essence_date(end_date),
    "&percentParam=noPercent",
    "&aqtTarget=DataDetails",
    "&geographySystem=region",
    "&detector=probrepswitch",
    "&timeResolution=daily",
    node_code,
    "&field=C_Unique_Patient_ID&field=DischargeDiagnosis&field=ChiefComplaintParsed&field=Age&field=C_Visit_Date_Time&field=C_Visit_Date_Source&field=C_Patient_Class&field=Region&field=Hospital&field=HospitalZip&field=Patient_Zip&field=DischargeDiagnosisUpdates&field=DischargeDiagnosisMDTUpdates&field=DischargeDisposition&field=HasBeenAdmitted&field=Sex&field=C_Race&field=C_Ethnicity&field=Admit_Reason_Code&field=ModeOfArrival&field=Travel_History&field=TriageNotesParsed&field=Discharge_Date_Time&field=Diagnosis_Combo&field=TriageNotesOrig&field=ChiefComplaintUpdates"
  )
}

# This splits up the download into yearly chunks to make it manageable
make_yearly_chunks <- function(start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  starts <- seq(floor_date(start_date, "year"), floor_date(end_date, "year"), by = "1 year")
  
  lapply(starts, function(s) {
    s2 <- max(s, start_date)
    e2 <- min(ceiling_date(s, "year") - days(1), end_date)
    list(start = s2, end = e2)
  })
}

# Now saves the data in yearly files
write_yearly <- function(df, out_dir2) {
  if (!"C_Visit_Date_Time" %in% names(df)) return(invisible(NULL))
  
  df <- df %>%
    mutate(file_year = format(as.Date(C_Visit_Date_Time), "%Y")) %>%
    filter(!is.na(file_year))
  
  if (nrow(df) == 0) return(invisible(NULL))
  
  split_df <- split(df, df$file_year)
  
  for (y in names(split_df)) {
    readr::write_csv(
      split_df[[y]] %>% select(-file_year),
      file.path(out_dir2, paste0("NSSP_", y, ".csv"))
    )
  }
  
  invisible(NULL)
}

# Function to now download the data and use all above functions together
download_nssp <- function(start_date, end_date, diagnosis_code = node, out_dir_path) {
  chunks <- make_yearly_chunks(start_date, end_date)
  log <- vector("list", length(chunks))
  
  for (i in seq_along(chunks)) {
    s <- chunks[[i]]$start
    e <- chunks[[i]]$end
    url <- build_url(s, e, diagnosis_code)
    
    message("Chunk ", i, "/", length(chunks), ": ", s, " to ", e)
    
    dat <- tryCatch(
      myProfile$get_api_data(url, fromCSV = TRUE),
      error = function(err) NULL
    )
    
    if (is.null(dat) || !is.data.frame(dat) || nrow(dat) == 0) {
      message("  skipped")
      log[[i]] <- data.frame(
        start = as.character(s),
        end = as.character(e),
        status = "failed_or_empty",
        rows = NA_integer_
      )
      next
    }
    
    message("  rows: ", nrow(dat))
    write_yearly(dat, out_dir_path)
    
    log[[i]] <- data.frame(
      start = as.character(s),
      end = as.character(e),
      status = "ok",
      rows = nrow(dat)
    )
  }
  
  bind_rows(log)
}

if (length(Nodes) > 0){
  # Download the data
  res <- download_nssp(
    start_date = as.Date("2023-01-01"),
    end_date = Sys.Date(),
    out_dir_path = out_dir2
  )
} else {
  print("You have no signals")
}

# Create a regex pattern for exact matches between semicolons
pattern <- paste0(";", paste(Nodes, collapse = ";|;"), ";")

# Make sure we don't have any incorrect nodes
#for (j in list.files(out_dir2, pattern = "\\.csv$", full.names = TRUE)) {
#  print(j)
#  A <- read.csv(j)
#  
#  # Filter rows with at least one exact match
#  filtered_df <- A %>%
#    filter(str_detect(DischargeDiagnosis, pattern))
#  
#  write.csv(filtered_df, j, row.names = FALSE)
#}
