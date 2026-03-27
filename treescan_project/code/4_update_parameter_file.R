update_prm_file <- function(
    prm_in = file.path(parent_dir, "params", "Parameter_File.prm"),
    prm_out = file.path(parent_dir, "params", "Parameter_File.prm"),
    days_back = 90,
    end_date = END_DATE
) {
  end_date <- as.Date(end_date)
  start_date <- end_date - days_back
  
  fmt_date <- function(x) format(x, "%Y/%m/%d")
  new_range <- paste0("[", fmt_date(start_date), ",", fmt_date(end_date), "]")
  
  to_prm_path <- function(...) {
    normalizePath(file.path(...), winslash = "/", mustWork = FALSE)
  }
  
  lines <- readLines(prm_in, warn = FALSE)
  
  lines <- sub("^data-time-range=.*",
               paste0("data-time-range=", new_range),
               lines)
  
  lines <- sub("^window-start-range=.*",
               paste0("window-start-range=", new_range),
               lines)
  
  lines <- sub("^window-end-range=.*",
               paste0("window-end-range=", new_range),
               lines)
  
  replace_path_value <- function(lines, key, value) {
    sub(paste0("^", key, "=.*"), paste0(key, "=", value), lines)
  }
  
  lines <- replace_path_value(
    lines,
    "tree-filename",
    to_prm_path(parent_dir, "data", "Tree_File_20250629.csv")
  )
  
  lines <- replace_path_value(
    lines,
    "count-filename",
    to_prm_path(parent_dir, "data/analysis_count_files", paste0("Analysis_Count_File_", end_date, ".txt"))
  )
  
  lines <- replace_path_value(
    lines,
    "results-filename",
    to_prm_path(parent_dir, "results", paste0(END_DATE, "/Results_", END_DATE, ".txt"))
  )
  
  lines <- replace_path_value(
    lines,
    "not-evaluated-nodes-file",
    to_prm_path(parent_dir, "data", "Do_not_evaluate_nodes.csv")
  )
  
  writeLines(lines, prm_out)
  
  invisible(list(
    prm_in = prm_in,
    prm_out = prm_out,
    start_date = start_date,
    end_date = end_date
  ))
}

update_prm_file()

# Also need to create the results file to save in

dir.create(paste0("C:/Users/nj7786/Documents/treescan_project/results/", END_DATE))

