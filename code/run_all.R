#///////////////////////////////////////////////////////////////////////////////
#' MAIN SCRIPT: Run TreeScan Analysis Pipeline
#' This script orchestrates the entire TreeScan workflow
#' Code from Ramona Lall & Alison Levin-Rector, October 2025
#' Edited by Emily Javan, February 2026
#///////////////////////////////////////////////////////////////////////////////

## Load packages ---------------------------------------------------------------
# Install here package if not already installed
if (!require("here", quietly = TRUE)) {
  install.packages("here")
}
# Source package downloading & loading script
source(here::here("code", "load_packages.R"))

## Set parameters --------------------------------------------------------------
year <- 2026 # was 2025, but updating for new year
ph_dept <- "NYC-DHMH" # no other options currently

# set to FALSE if you'd like to disable the extra print statements & outputs
debug_mode <- TRUE

# Run synthetic data set for NYC-DHMH only
#  or use a date like "20250629" matching the date of file 
#  in "ED_count_data/ph_dept/Count_File_date.txt"
data_file = "synthetic"

## PART 1: Create Tree File ----------------------------------------------------
# Note: Will automatically download ICD-10 files from CMS if not present
#       Took ~41 minutes to run on M1 Macbook Pro

# Define expected output files from Step 1
tree_output <- here::here("input_data", "TreeScan_input_files", 
                          paste0("Final_Tree_File_", year, ".csv"))
tree_wide_output <- here::here("input_data", "TreeScan_input_files", "intermediate",
                               paste0("Tree_File_", year, "_wide_format.txt"))

# Check if both tree files already exist
if (file.exists(tree_output) && file.exists(tree_wide_output)) {
  message("✓ Tree files already exist, skipping PART 1")
} else {
  source(here::here("code", "1_Create_Tree_File.R"))
  message("→ Running PART 1: Create Tree File")
  create_tree_file(year = year, debug_mode = debug_mode)
}

## PART 2: Create Count File ---------------------------------------------------
# Note: Processes ED visit data to create count file for TreeScan analysis
#       Synthetic_Dataset.txt or ED_count_data_YYYYMMDD.txt

# Define expected output file from Step 2
count_output <- here::here("input_data", "TreeScan_input_files", "ED_count_data", ph_dept,
                           paste0("Count_File_", data_file, ".txt"))

# Check if count file already exists
if (file.exists(count_output)) {
  message("✓ Count file already exists, skipping PART 2")
} else {
  source(here::here("code", "2_Create_Count_File.R"))
  message("→ Running PART 2: Create Count File")
  create_count_file(
    year = year,
    debug_mode = debug_mode,
    ph_dept = ph_dept,
    data_file = data_file
  )
}

## PART 3: Run TreeScan --------------------------------------------------------
# TODO: Add after Step 2 is working



