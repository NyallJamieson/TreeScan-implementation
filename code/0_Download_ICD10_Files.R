## DOWNLOAD AND EXTRACT ICD-10 FILES
## Downloads ICD-10-CM code files from CMS and extracts only needed files

#' Download ICD-10-CM Files from CMS
#' 
#' Downloads the ZIP files containing ICD-10-CM codes, extracts only the needed
#' files (XML tabular and TXT code descriptions), and saves them to the project.
#' 
#' @param year Numeric year for ICD-10-CM codes (default: 2026)
#' @return Invisible TRUE if successful
download_icd10_files <- function(year = 2026) {
  
  # Load required packages
  suppressPackageStartupMessages({
    library(here)
  })
  
  message("Downloading ICD-10-CM files for year ", year, "...")
  
  # Create destination directory if it doesn't exist
  dest_dir <- here("input_data", "ICD10_code_tree")
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE)
  }
  
  # Define file paths
  xml_file <- here(dest_dir, paste0("icd10cm_tabular_", year, ".xml"))
  txt_file <- here(dest_dir, paste0("icd10cm_codes_", year, ".txt"))
  
  # Check if files already exist
  if (file.exists(xml_file) && file.exists(txt_file)) {
    message("✓ ICD-10-CM files for ", year, " already exist, skipping download")
    return(invisible(TRUE))
  }
  
  ## Download and extract XML tabular file ----
  if (!file.exists(xml_file)) {
    message("→ Downloading tabular XML file...")
    
    # Download ZIP to temp location
    zip_url <- paste0("https://www.cms.gov/files/zip/", year, "-code-tables-tabular-and-index.zip")
    temp_zip <- tempfile(fileext = ".zip")
    
    tryCatch({
      download.file(zip_url, temp_zip, mode = "wb", quiet = TRUE)
      
      # List files in ZIP to find the XML
      zip_contents <- unzip(temp_zip, list = TRUE)
      xml_pattern <- paste0("icd10cm_tabular_", year, "\\.xml")
      xml_in_zip <- grep(xml_pattern, zip_contents$Name, value = TRUE, ignore.case = TRUE)
      
      if (length(xml_in_zip) == 0) {
        stop("Could not find ", xml_pattern, " in the downloaded ZIP file")
      }
      
      # Extract only the XML file
      unzip(temp_zip, files = xml_in_zip[1], exdir = dest_dir, junkpaths = TRUE)
      
      # Rename if necessary (in case of different casing)
      extracted_file <- file.path(dest_dir, basename(xml_in_zip[1]))
      if (extracted_file != xml_file) {
        file.rename(extracted_file, xml_file)
      }
      
      message("✓ Downloaded and extracted XML file")
      
    }, error = function(e) {
      stop("Failed to download XML file: ", e$message)
    }, finally = {
      unlink(temp_zip)
    })
  } else {
    message("✓ XML file already exists")
  }
  
  ## Download and extract TXT codes file ----
  if (!file.exists(txt_file)) {
    message("→ Downloading code descriptions TXT file...")
    
    # Download ZIP to temp location
    zip_url <- paste0("https://www.cms.gov/files/zip/", year, "-code-descriptions-tabular-order.zip")
    temp_zip <- tempfile(fileext = ".zip")
    
    tryCatch({
      download.file(zip_url, temp_zip, mode = "wb", quiet = TRUE)
      
      # List files in ZIP to find the TXT
      zip_contents <- unzip(temp_zip, list = TRUE)
      txt_pattern <- paste0("icd10cm_codes_", year, "\\.txt")
      txt_in_zip <- grep(txt_pattern, zip_contents$Name, value = TRUE, ignore.case = TRUE)
      
      if (length(txt_in_zip) == 0) {
        stop("Could not find ", txt_pattern, " in the downloaded ZIP file")
      }
      
      # Extract only the TXT file
      unzip(temp_zip, files = txt_in_zip[1], exdir = dest_dir, junkpaths = TRUE)
      
      # Rename if necessary
      extracted_file <- file.path(dest_dir, basename(txt_in_zip[1]))
      if (extracted_file != txt_file) {
        file.rename(extracted_file, txt_file)
      }
      
      message("✓ Downloaded and extracted TXT file")
      
    }, error = function(e) {
      stop("Failed to download TXT file: ", e$message)
    }, finally = {
      unlink(temp_zip)
    })
  } else {
    message("✓ TXT file already exists")
  }
  
  message("✓ All ICD-10-CM files ready for year ", year)
  return(invisible(TRUE))
}
