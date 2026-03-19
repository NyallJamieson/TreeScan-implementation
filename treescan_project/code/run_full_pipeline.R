# Full script that runs all the sub-scripts

# We need to set where you store the folder
parent_dir <- "C:/Users/nj7786/Documents/treescan_project"

# We need to set where you have downloaded the new treescan file
base_dir <- "C:/Users/nj7786/Downloads/CLI"

# Is this your first time installing?
first_time <- TRUE

# Run the script that installs treescan
if (isTRUE(first_time)){
  source(paste0(parent_dir, "/code/0_install_treescan.R"))
}

# Run the script that downloads the required data
source(paste0(parent_dir, "/code/1_download_data.R"))

# Run the script that cleans the downloaded NSSP Essence data
source(paste0(parent_dir, "/code/2_clean_download_data.R"))

# Create the count file
source(paste0(parent_dir, "/code/3_create_count_file.R"))

# Update the parameter file
source(paste0(parent_dir, "/code/3.1_update_parameter_file.R"))

# Run treescan
source(paste0(parent_dir, "/code/4_run_treescan.R"))
