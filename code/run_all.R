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

# Source function definitions
source(here::here("code", "1_Create_Tree_File.R"))

## Set parameters --------------------------------------------------------------
year <- 2026 # was 2025, but updating for new year
ph_dept <- "NYC-DHMH"

# set to FALSE if you'd like to disable the extra print statements & outputs
debug_mode <- TRUE

## PART 1: Create Tree File ----------------------------------------------------
# Note: Will automatically download ICD-10 files from CMS if not present
#       Took 40.8 minutes to run on M1 chip

# Define expected output file from Step 1
tree_output <- here::here("input_data", "TreeScan_input_files", 
                          paste0("Final_Tree_File_", year, ".csv"))

# Check if tree file already exists
if (file.exists(tree_output)) {
  message("✓ Tree file already exists, skipping PART 1")
} else {
  message("→ Running PART 1: Create Tree File")
  create_tree_file(year = year, debug_mode = debug_mode)
}

## PART 2: Create Count File ---------------------------------------------------
# TODO: Add after Step 1 is working

## PART 3: Run TreeScan --------------------------------------------------------
# TODO: Add after Step 2 is working



