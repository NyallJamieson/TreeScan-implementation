# Full script that runs all the sub-scripts

# You need to setwd when in the "treescan_project" folder. eg:
setwd("~/TreeScan-implementation/treescan_project")

# We need to set where you store the folder in a treescan-friendly way
parent_dir <- normalizePath(getwd(), mustWork = TRUE)
parent_dir <- gsub("\\\\", "/", parent_dir)

# We need to set where you have downloaded the new treescan file
base_dir <- dirname(parent_dir)

# Is this your first time installing?
first_time <- TRUE

# If you're on a server, uploading treescan download unzipped automatically
# but still needs to install!

# If on server, set as true; otherwise, set as false
server <- FALSE

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
    "sodium"
  ))
}

# Need to install this separately 
# Feel free to skip updates
if (isTRUE(first_time)){
  devtools::install_github("cdcgov/Rnssp", force = TRUE)
}

# Run the script that installs treescan
source(paste0(parent_dir, "/code/0_install_treescan.R"))

# Run the script that downloads the required data
source(paste0(parent_dir, "/code/1_download_data.R"))

# Run the script that cleans the downloaded NSSP Essence data
source(paste0(parent_dir, "/code/2_clean_downloaded_data.R"))

# Create the count file
source(paste0(parent_dir, "/code/3_create_count_file.R"))

# Update the parameter file
source(paste0(parent_dir, "/code/4_update_parameter_file.R"))

# Run treescan
source(paste0(parent_dir, "/code/5_run_treescan.R"))
