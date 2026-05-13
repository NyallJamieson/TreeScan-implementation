update_prm_file <- function(
    lag,
    template_lag = 0,
    days_back = 90,
    todays_date = final_date,
    end_date = final_date - lag
) {
  end_date <- as.Date(end_date)
  start_date <- end_date - days_back
  
  fmt_date <- function(x) format(as.Date(x), "%Y/%m/%d")
  new_range <- paste0("[", fmt_date(start_date), ",", fmt_date(end_date), "]")
  
  to_prm_path <- function(...) {
    path <- do.call(file.path, list(...))
    path <- gsub("\\\\", "/", path)
    if (grepl("^//", list(...)[[1]])) {
      path <- sub("^/+", "//", path)
    }
    path
  }
  
  prm_out <- paste0(parent_dir, "/params/Parameter_File_lag", lag, ".prm")
  prm_in_existing <- prm_out
  prm_in_template <- paste0(parent_dir, "/params/Parameter_File_lag", template_lag, ".prm")
  
  # Use existing lag file if present; otherwise fall back to lag0 template
  prm_in <- if (file.exists(prm_in_existing)) prm_in_existing else prm_in_template
  
  if (!file.exists(prm_in)) {
    stop("Template file not found: ", prm_in)
  }
  
  lines <- readLines(prm_in, warn = FALSE)
  
  # If creating from lag0 template, replace lag0 with lagX everywhere
  if (!file.exists(prm_in_existing)) {
    lines <- gsub(
      paste0("lag", template_lag),
      paste0("lag", lag),
      lines,
      fixed = TRUE
    )
  }
  
  # Update date ranges
  lines <- sub(
    "^data-time-range=.*",
    paste0("data-time-range=", new_range),
    lines
  )
  
  lines <- sub(
    "^window-start-range=.*",
    paste0("window-start-range=", new_range),
    lines
  )
  
  lines <- sub(
    "^window-end-range=.*",
    paste0("window-end-range=", new_range),
    lines
  )
  
  replace_path_value <- function(lines, key, value) {
    sub(paste0("^", key, "=.*"), paste0(key, "=", value), lines)
  }
  
  lines <- replace_path_value(
    lines,
    "tree-filename",
    to_prm_path(parent_dir, "data", "Tree_File_2026.csv")
  )
  
  lines <- replace_path_value(
    lines,
    "count-filename",
    to_prm_path(
      parent_dir,
      "data",
      "analysis_count_files",
      paste0("Analysis_Count_File_", todays_date),
      paste0("lag", lag, ".txt")
    )
  )
  
  lines <- replace_path_value(
    lines,
    "results-filename",
    to_prm_path(
      parent_dir,
      "results",
      as.character(todays_date),
      paste0("Results_lag", lag, "_", todays_date, ".txt")
    )
  )
  
  lines <- replace_path_value(
    lines,
    "not-evaluated-nodes-file",
    to_prm_path(parent_dir, "data", "Do_not_evaluate_nodes.csv")
  )
  
  # Update number of processors
  lines <- sub(
    "^parallel-processes=.*",
    paste0("parallel-processes=", number_processors),
    lines
  )
  
  writeLines(lines, prm_out)
  
  invisible(list(
    prm_in = prm_in,
    prm_out = prm_out,
    start_date = start_date,
    end_date = end_date
  ))
}

# Also need to create the results file to save in
dir.create(paste0(parent_dir, "/results/", final_date), recursive = TRUE, showWarnings = FALSE)

for (LAG in initial_lags) {
  update_prm_file(lag = LAG)
}
