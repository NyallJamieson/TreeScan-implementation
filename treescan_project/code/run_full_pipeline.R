# Full script that runs all the sub-scripts

# You need to setwd when in the "treescan_project" folder. eg:
setwd("~/TreeScan-implementation/treescan_project")

# We need to set where you store the folder in a treescan-friendly way
parent_dir <- normalizePath(getwd(), mustWork = TRUE)
parent_dir <- gsub("\\\\", "/", parent_dir)

# We need to set where you have downloaded the new treescan file
base_dir <- dirname(parent_dir)

# Is this your first time installing?
first_time <- FALSE

# If you're on a server, uploading treescan download unzipped automatically
# but still needs to install!
# If on server, set as true; otherwise, set as false
server <- FALSE

# Do you want to run on a subregion of your jurisdiction?
# e.g., TX_Tarrant in Texas?
subregion <- FALSE

# If subregion = TRUE, then what subregion? (this has TX_Travis as default)
which_subregion <- c("TX_Travis")
# This CAN include multiple regions: e.g., c("TX_Tarrant", "TX_Harris")

# How many processors do you want to use?
# select number below; 0 means use all available (which was the previous default)
number_processors <- 2

# Are you going to set this for batch or be more hands on?
# There are two options for lag selection for this pipeline
# 1) you run daily and on the first day of the month check lag curve yourself,
# then pick the lags you want to run for in this month
# 2) Check your lag independently of this pipeline, but run still run pipeline for
# your chosen lags
# Pick between 1 and 2 for your preference
# THIS IS INITIALLY SET TO 1
lag_choice <- 1

# If you choose approach 2, then do you want to reassess lags today?
reassess <- TRUE

# What date do you want to have as the end date of your analysis
# leave as Sys.Date() if you want to do treescan in real time
final_date <- Sys.Date()

# Is it a new month?
# If so then set to true.
# Also set to true if you want to re-assess the lag situation
new_month <- ifelse(file.exists(paste0(parent_dir, "/lag/plots/lag_curve_", format(as.Date(final_date), "%Y-%m"), ".png")), FALSE, TRUE)

# We need to install all required libraries, assuming you have none installed
if (isTRUE(first_time)){
  install.packages(c(
    "tidyverse",
    "devtools",
    "lubridate",
    "httr",
    "jsonlite",
    "readr",
    "rlang",
    "dplyr",
    "purrr",
    "sodium",
    "data.table",
    "openxlsx",
    "MMWRweek",
    "png",
    "svDialogs",
    "tcltk",
    "timeDate",
    "png"
  ))
}

# Need to install this separately 
# Feel free to skip updates
if (isTRUE(first_time)){
  devtools::install_github("cdcgov/Rnssp", force = TRUE)
}

# Run the script that locates treescan
source(paste0(parent_dir, "/code/0_locate_treescan.R"))

# Run the script that downloads the required data
source(paste0(parent_dir, "/code/1_download_data.R"))

# Run the script that subsets your download data if you requested
source(paste0(parent_dir, "/code/1.1_subset_downloaded_data.R"))

# Run the script that cleans the downloaded NSSP Essence data
source(paste0(parent_dir, "/code/2_clean_downloaded_data.R"))

if (lag_choice == 1){
  # Now source in the lag assessment script (fix)
  source(paste0(parent_dir, "/code/2.1_assess_lag.R"))
  
  # Now set which lags the model will use
  source(paste0(parent_dir, "/code/2.2_pick_lags.R"))
} else {
  # You need to pick your lags of choice
  # You should do this by running the ... R script to assess the lag
  if (reassess == TRUE){
    source(paste0(parent_dir, "/code/2.3_quick_lag_check.R"))
    # This code will produce you a plot
    # Choose from this plot what lags you want to consider
    # Remember: lag of 1 day means use data up until yesterday
    # lag of 2 days means use data up until the day before yesterday, etc
    
    # Ask for values
    input <- svDialogs::dlgInput("Look at your lag plot. Do you want to consider new lags? If so, then type below (separated by commas). Type 1 to mean you want to use data up until a day ago, etc.")$res
    
    # Get location of file to update initial_lags
    script_path <- paste0(parent_dir, "/code/run_full_pipeline.R")
    
    # Build replacement line
    new_line <- paste0("  initial_lags <- c(", input, ")")
    
    # Read script
    lines <- readLines(script_path)
    
    # Replace target line
    lines[119] <- new_line
    
    # Save script
    writeLines(lines, script_path)
    
    eval(parse(text = new_line))

    # We will set the initial lags to be 1 and 4
    initial_lags <- c(1, 4)
  }
  
  # This will update this accordingly after looking at the plot
}

# Now get metrics on whether a code is a data artifact
source(paste0(parent_dir, "/code/2.4_data_artifact_check.R"))

# If you are running not in real time, then you likely don't want to run on multiple lags

# Create the count file
source(paste0(parent_dir, "/code/3_create_count_file.R"))

# Update the parameter file
source(paste0(parent_dir, "/code/4_update_parameter_file.R"))

# Run treescan
source(paste0(parent_dir, "/code/5_run_treescan.R"))

# Run the linelist creation
source(paste0(parent_dir, "/code/6_create_signal_linelist.R"))

# Now generate the signal report
source(paste0(parent_dir, "/code/7_create_signal_report.R"))
