#////////////////////////////////////////////////////
#' Download and load packages needed to run code
#'
#////////////////////////////////////////////////////


# Install pacman if not already installed
if (!require("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

# Load packages (installing if needed)
pacman::p_load(
  xml2,       # Parse and manipulate XML/HTML documents
  tidyverse,  # Collection of data manipulation and visualization packages
  here,       # Construct file paths relative to project root
  rio,        # Import/export data in various formats
  icecream    # Enhanced debugging with ic() function
)
