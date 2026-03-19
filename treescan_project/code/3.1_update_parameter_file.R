library(lubridate)

update_prm_file <- function(
    prm_in = file.path(parent_dir, "params", "Parameter_File.prm"),
    prm_out = file.path(parent_dir, "params", "Parameter_File.prm"),
    months_back = 15,
    end_date = Sys.Date()
) {
  
  end_date <- as.Date(end_date)
  start_date <- end_date %m-% months(months_back)
  
  fmt_date <- function(x) format(x, "%Y/%m/%d")
  new_range <- paste0("[", fmt_date(start_date), ",", fmt_date(end_date), "]")
  
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
  
  # Replace any full path beginning with parent_dir-like root in file entries
  # by rebuilding only the known path-bearing parameters
  replace_path_value <- function(lines, key, value) {
    sub(
      paste0("^", key, "=.*"),
      paste0(key, "=", value),
      lines
    )
  }
  
  lines <- replace_path_value(
    lines,
    "tree-filename",
    file.path(parent_dir, "data", "Tree_File_20250629.csv")
  )
  
  lines <- replace_path_value(
    lines,
    "count-filename",
    file.path(parent_dir, "data", "Analysis_Count_File.txt")
  )
  
  lines <- replace_path_value(
    lines,
    "results-filename",
    file.path(parent_dir, "Results_20250629.txt")
  )
  
  lines <- replace_path_value(
    lines,
    "not-evaluated-nodes-file",
    file.path(parent_dir, "data", "Do_not_evaluate_nodes.csv")
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