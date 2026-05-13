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
fmt_node <- function(Nodes, op = "OR") {
  expr <- paste0("%5E", Nodes, "%5E", collapse = paste0(",", op, ","))
  paste0("&dischargeDiagnosis=", expr)
}

# We need to make sure what nodes are actually valid first and then format the API url
# Function to normalize node codes (remove prefixes and periods)
clean_node <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("^(0-|1-|2-)", "", x)  # Remove TreeScan prefixes
  x <- gsub("\\.", "", x)          # Remove decimal points
  toupper(x)
}

# Load the TreeScan ICD-10 tree file
tree_path <- file.path(parent_dir, "data", "Tree_File_2026_wide_format.txt")
icd10_tree <- read.delim(tree_path, stringsAsFactors = FALSE, check.names = FALSE)

# Identify columns that contain ICD-10 node codes
node_cols <- intersect(c("Name1", paste0("Level", 1:8)), names(icd10_tree))

# Extract and clean all valid nodes from the tree
valid_nodes <- unique(unlist(icd10_tree[, node_cols], use.names = FALSE))
valid_nodes_clean <- clean_node(valid_nodes)

# Clean the input Nodes vector
nodes_clean <- clean_node(filtered_nodes)

# Identify valid nodes
valid_nodes <- filtered_nodes[(nodes_clean %in% valid_nodes_clean)]

# Stop execution if any invalid nodes are found
if (length(valid_nodes) == 0) {
  NO <- TRUE
  stop(
    paste(
      "The following entries in 'Nodes' are not valid TreeScan ICD-10 nodes:",
      paste(unique(valid_nodes), collapse = ", ")
    ),
    call. = FALSE
  )
} else {
  print(valid_nodes)
}

# If all nodes are valid, proceed
node_code <- fmt_node(valid_nodes)

# This code builds the url to download the data
build_url <- function(start_date, end_date, diagnosis_code = node_code) {
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
    diagnosis_code,
    "&field=C_Unique_Patient_ID&field=DischargeDiagnosis&field=ChiefComplaintParsed&field=Age&field=C_Visit_Date_Time&field=C_Visit_Date_Source&field=C_Patient_Class&field=Region&field=Hospital&field=HospitalZip&field=Patient_Zip&field=DischargeDiagnosisUpdates&field=DischargeDiagnosisMDTUpdates&field=DischargeDisposition&field=HasBeenAdmitted&field=Sex&field=C_Race&field=C_Ethnicity&field=Admit_Reason_Code&field=ModeOfArrival&field=Travel_History&field=TriageNotesParsed&field=Discharge_Date_Time&field=Diagnosis_Combo&field=TriageNotesOrig&field=ChiefComplaintUpdates&field=HospitalName"
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
  
  text_cols <- intersect(
    c("ChiefComplaintParsed", "TriageNotesOrig", "TriageNotesParsed",
      "Diagnosis_Combo", "Admit_Reason_Code", "DischargeDiagnosis",
      "ChiefComplaintUpdates"),
    names(df)
  )
  
  for (nm in text_cols) {
    df[[nm]] <- normalize_text_utf8(df[[nm]])
  }
  
  for (y in names(split_df)) {
    readr::write_csv(
      split_df[[y]] %>% select(-file_year),
      file.path(out_dir2, paste0("NSSP_", y, ".csv"))
    )
  }
  
  invisible(NULL)
}

write_yearly <- function(df, out_dir2) {
  if (!"C_Visit_Date_Time" %in% names(df)) return(invisible(NULL))
  
  df <- df %>%
    mutate(file_year = format(as.Date(C_Visit_Date_Time), "%Y")) %>%
    filter(!is.na(file_year))
  
  if (nrow(df) == 0) return(invisible(NULL))
  
  text_cols <- intersect(
    c("ChiefComplaintParsed", "TriageNotesOrig", "TriageNotesParsed",
      "Diagnosis_Combo", "Admit_Reason_Code", "DischargeDiagnosis",
      "ChiefComplaintUpdates"),
    names(df)
  )
  
  for (nm in text_cols) {
    df[[nm]] <- normalize_text_utf8(df[[nm]])
  }
  
  split_df <- split(df, df$file_year)
  
  for (y in names(split_df)) {
    readr::write_csv(
      split_df[[y]] %>% select(-file_year),
      file.path(out_dir2, paste0("NSSP_", y, ".csv"))
    )
  }
  
  invisible(NULL)
}

# Retry wrapper for flaky ESSENCE/API communication
get_api_data_retry <- function(url, max_attempts = 4, wait_seconds = 5) {
  last_error <- NULL
  
  for (attempt in seq_len(max_attempts)) {
    message("  attempt ", attempt, "/", max_attempts)
    
    dat <- tryCatch(
      myProfile$get_api_data(url, fromCSV = TRUE),
      error = function(err) {
        last_error <<- conditionMessage(err)
        NULL
      }
    )
    
    # Success case
    if (!is.null(dat) && is.data.frame(dat) && nrow(dat) > 0) {
      return(list(
        data = dat,
        status = "ok",
        attempts = attempt,
        error_message = NA_character_
      ))
    }
    
    # Returned an empty data frame
    if (!is.null(dat) && is.data.frame(dat) && nrow(dat) == 0) {
      last_error <- "API returned 0 rows"
    }
    
    # Returned something weird
    if (!is.null(dat) && !is.data.frame(dat)) {
      last_error <- paste("API returned object of class:", paste(class(dat), collapse = ", "))
    }
    
    # Wait before retrying, except after final attempt
    if (attempt < max_attempts) {
      message("  retrying in ", wait_seconds, " sec...")
      Sys.sleep(wait_seconds)
    }
  }
  
  list(
    data = NULL,
    status = "failed_after_retries",
    attempts = max_attempts,
    error_message = last_error
  )
}

# Function to download the data with retries
download_nssp <- function(start_date, end_date, diagnosis_code = node_code, out_dir_path,
                          max_attempts = 4, wait_seconds = 5) {
  chunks <- make_yearly_chunks(start_date, end_date)
  log <- vector("list", length(chunks))
  
  for (i in seq_along(chunks)) {
    s <- chunks[[i]]$start
    e <- chunks[[i]]$end
    url <- build_url(s, e, diagnosis_code)
    
    message("Chunk ", i, "/", length(chunks), ": ", s, " to ", e)
    
    res <- get_api_data_retry(
      url = url,
      max_attempts = max_attempts,
      wait_seconds = wait_seconds
    )
    
    dat <- res$data
    
    if (is.null(dat)) {
      message("  failed after ", res$attempts, " attempts")
      if (!is.na(res$error_message)) {
        message("  last error: ", res$error_message)
      }
      
      log[[i]] <- data.frame(
        start = as.character(s),
        end = as.character(e),
        status = res$status,
        rows = NA_integer_,
        attempts = res$attempts,
        error_message = ifelse(is.na(res$error_message), "", res$error_message),
        stringsAsFactors = FALSE
      )
      next
    }
    
    message("  rows: ", nrow(dat))
    write_yearly(dat, out_dir_path)
    
    log[[i]] <- data.frame(
      start = as.character(s),
      end = as.character(e),
      status = "ok",
      rows = nrow(dat),
      attempts = res$attempts,
      error_message = "",
      stringsAsFactors = FALSE
    )
  }
  
  bind_rows(log)
}

# Just incase you closed server and the login details aren't loaded
load(file.path(parent_dir, "myProfile.rda"))

if (length(valid_nodes) > 0) {
  # Download the data
  res <- download_nssp(
    start_date = paste0(year(final_date)-3, "-01-01"),
    end_date = final_date,
    out_dir_path = out_dir2,
    max_attempts = 5,
    wait_seconds = 15
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
