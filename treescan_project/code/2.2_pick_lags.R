# Is it a new month? (we assess lag in new month)
# Set to true if you want to change (or re-assess) the lags you're using
initial_lags <- c(1,4)

# Location of this run_full_pipeline.R script
script_path <- paste0(parent_dir, "/code/2.2_pick_lags.R")

# Load required libraries
library("png")
library("svDialogs")

if (isTRUE(new_month) || isTRUE(first_time)){
  img <- readPNG(paste0(parent_dir, "/lag/plots/lag_curve_", year_month, ".png"))
  plot.new()
  rasterImage(img, 0, 0, 1, 1)
  
  # If we change the lag selection then we need to update this in the script
  line_x <- 3
  
  # Ask for values
  input <- dlgInput("Look at your lag plot. Do you want to consider new lags? If so, then type below (separated by commas). Type 1 to mean you want to use data up until a day ago, etc.")$res
  
  # Build replacement line
  new_line <- paste0("initial_lags <- c(", input, ")")
  
  # Read script
  lines <- readLines(script_path)
  
  # Replace target line
  lines[line_x] <- new_line
  
  # Save script
  writeLines(lines, script_path)
  
  eval(parse(text = new_line))
  
}
