# Load in the data to assess lag
# Create output path
in_file <- file.path(
  parent_dir,
  "data",
  "datasets",
  paste0("dataset_", final_date, ".rds")
)

# Save data
data <- readRDS(file = in_file)

# What month-year are we in?
year_month <- format(final_date, "%Y-%m")

monthly_lags_assessed <- list.files(
  paste0(parent_dir, "/lag/curves"),
  pattern = "\\.rds$"
)

have_we_got_lag <- isTRUE(paste0("lag_curve_", year_month, ".rds") %in% monthly_lags_assessed)
have_we_got_databycode <- isTRUE(length(list.files(paste0(parent_dir, "/data_by_code"))) > 0)

if (!(isTRUE(have_we_got_lag) && isTRUE(have_we_got_databycode))){
  
  new_month <- TRUE
  
  df_all_for_lag <- readRDS(paste0(parent_dir, "/data/data for lag/data_for_lag.rds"))
  
  # Load required libraries
  library(dplyr)
  library(stringr)
  library(tidyr)
  library(purrr)
  library(lubridate)
  library(data.table)
  library(parallel)
  library(tibble)
  library(stringi)
  
  setDTthreads(0)
  
  .process_nssp_chunk_dt <- function(chunk_df) {
    if (!is.data.table(chunk_df)) setDT(chunk_df)
    
    # Keep only usable rows
    chunk_df <- chunk_df[
      !is.na(DischargeDiagnosisUpdates) &
        !is.na(DischargeDiagnosisMDTUpdates) &
        DischargeDiagnosisUpdates != "" &
        DischargeDiagnosisMDTUpdates != ""
    ]
    
    if (nrow(chunk_df) == 0L) {
      return(data.table(
        row_id = integer(),
        visit_time = as.POSIXct(character(), tz = "UTC"),
        event_id = integer(),
        update_time = as.POSIXct(character(), tz = "UTC"),
        code = character()
      ))
    }
    
    # ---- diagnosis events ----
    diag_list <- stri_split_fixed(chunk_df$DischargeDiagnosisUpdates, "|", omit_empty = FALSE)
    diag_n <- lengths(diag_list)
    
    diag_long <- data.table(
      row_id = rep.int(chunk_df$row_id, diag_n),
      visit_time = rep(chunk_df$visit_time, diag_n),
      diag_ev = unlist(diag_list, use.names = FALSE)
    )
    
    # Extract event_id and diagnosis payload
    diag_caps <- stri_match_first_regex(diag_long$diag_ev, "^\\{(\\d+)\\};;(.*)$")
    diag_tbl <- data.table(
      row_id = diag_long$row_id,
      visit_time = diag_long$visit_time,
      event_id = as.integer(diag_caps[, 2]),
      diag_str = diag_caps[, 3]
    )[!is.na(event_id)]
    
    rm(diag_long, diag_list, diag_caps)
    gc(FALSE)
    
    # ---- time events ----
    time_list <- stri_split_fixed(chunk_df$DischargeDiagnosisMDTUpdates, "|", omit_empty = FALSE)
    time_n <- lengths(time_list)
    
    time_long <- data.table(
      row_id = rep.int(chunk_df$row_id, time_n),
      time_ev = unlist(time_list, use.names = FALSE)
    )
    
    time_caps <- stri_match_first_regex(
      time_long$time_ev,
      "^\\{(\\d+)\\};(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})"
    )
    
    time_tbl <- data.table(
      row_id = time_long$row_id,
      event_id = as.integer(time_caps[, 2]),
      update_time = ymd_hms(time_caps[, 3], quiet = TRUE)
    )[!is.na(event_id)]
    
    rm(time_long, time_list, time_caps)
    gc(FALSE)
    
    # Join within row_id + event_id
    setkey(diag_tbl, row_id, event_id)
    setkey(time_tbl, row_id, event_id)
    ev_tbl <- diag_tbl[time_tbl, nomatch = 0L, allow.cartesian = TRUE]
    
    if (nrow(ev_tbl) == 0L) {
      return(data.table(
        row_id = integer(),
        visit_time = as.POSIXct(character(), tz = "UTC"),
        event_id = integer(),
        update_time = as.POSIXct(character(), tz = "UTC"),
        code = character()
      ))
    }
    
    # Extract ICD-like codes
    code_list <- stri_extract_all_regex(
      ev_tbl$diag_str,
      "[A-Z][0-9][0-9A-Z](?:\\.[0-9A-Z]{1,4})?"
    )
    code_n <- lengths(code_list)
    
    if (!any(code_n > 0L)) {
      return(data.table(
        row_id = integer(),
        visit_time = as.POSIXct(character(), tz = "UTC"),
        event_id = integer(),
        update_time = as.POSIXct(character(), tz = "UTC"),
        code = character()
      ))
    }
    
    out <- ev_tbl[rep.int(seq_len(nrow(ev_tbl)), code_n)]
    out[, code := unlist(code_list, use.names = FALSE)]
    out <- out[!is.na(code) & code != "", .(row_id, visit_time, event_id, update_time, code)]
    
    out
  }
  
  clean_nssp_updates_dt <- function(df, chunk_size = 100000L, cores = 1L) {
    dt <- as.data.table(df)
    
    dt <- dt[, .(
      row_id = seq_len(.N),
      visit_time = ymd_hms(C_Visit_Date_Time, quiet = TRUE),
      DischargeDiagnosisUpdates,
      DischargeDiagnosisMDTUpdates
    )]
    
    n <- nrow(dt)
    if (n == 0L) {
      return(data.table(
        row_id = integer(),
        visit_time = as.POSIXct(character(), tz = "UTC"),
        event_id = integer(),
        update_time = as.POSIXct(character(), tz = "UTC"),
        code = character()
      ))
    }
    
    starts <- seq.int(1L, n, by = chunk_size)
    ends <- pmin(starts + chunk_size - 1L, n)
    
    parts <- vector("list", length(starts))
    for (i in seq_along(starts)) {
      parts[[i]] <- .process_nssp_chunk_dt(dt[starts[i]:ends[i]])
    }
    
    rbindlist(parts, use.names = TRUE)
  }
  
  # Main cleaning
  df_long <- clean_nssp_updates_dt(
    df_all_for_lag,
    chunk_size = 100000L,
    cores = 1L
  )
  
  # First appearance per row_id + code
  setorder(df_long, row_id, code, update_time)
  first_appearance <- df_long[
    ,
    .(
      visit_time = visit_time[1L],
      first_time = min(update_time, na.rm = TRUE)
    ),
    by = .(row_id, code)
  ]
  
  first_appearance[
    ,
    delay_hours := as.numeric(difftime(first_time, visit_time, units = "hours"))
  ]
  
  first_appearance <- first_appearance[!is.na(delay_hours) & delay_hours >= 0]
  
  # Tree / ancestor mapping
  tree_year <- format(Sys.Date(), "%Y")
  tree_file <- paste0(parent_dir, "/data/Tree_File_", tree_year, "_wide_format.txt")
  icd_tree <- fread(tree_file)
  
  ancestor_map <- melt(
    icd_tree,
    id.vars = "Name",
    measure.vars = c("Level4", "Level5", "Level6", "Level7", "Level8"),
    value.name = "ancestor_code",
    variable.name = "level"
  )[
    !is.na(ancestor_code) & ancestor_code != "",
    .(Name, ancestor_code)
  ]
  
  ancestor_map <- unique(ancestor_map)
  
  # Expand each code to itself + ancestors
  self_map <- unique(first_appearance[, .(code = code, code_for_lag = code)])
  anc_map  <- unique(ancestor_map[, .(code = Name, code_for_lag = ancestor_code)])
  map_all  <- unique(rbindlist(list(self_map, anc_map), use.names = TRUE))
  
  map_all <- unique(
    rbindlist(list(self_map, anc_map), use.names = TRUE),
    by = c("code", "code_for_lag")
  )
  
  first_appearance_expanded <- map_all[
    first_appearance,
    on = "code",
    nomatch = 0L,
    allow.cartesian = TRUE
  ][
    ,
    .(row_id, code_for_lag, delay_hours)
  ]
  
  first_appearance_expanded <- unique(
    first_appearance_expanded,
    by = c("row_id", "code_for_lag", "delay_hours")
  )
  
  # Percentiles by code
  probs <- seq(0.01, 1, by = 0.01)
  pnames <- paste0("p", 1:100)
  
  delay_by_code <- first_appearance_expanded[
    ,
    as.list(setNames(quantile(delay_hours, probs = probs, na.rm = TRUE, names = FALSE), pnames)),
    by = .(code = code_for_lag)
  ]
  
  # Create directory to store delay by code
  dir.create(paste0(parent_dir, "/data_by_code"))
  saveRDS(delay_by_code, paste0(parent_dir, "/data_by_code/data_by_code.rds"))
  
  # Overall delay
  overall_delay <- first_appearance[
    ,
    as.list(setNames(quantile(delay_hours, probs = probs, na.rm = TRUE, names = FALSE), pnames))
  ]
  
  # Knee estimate
  x <- as.numeric(overall_delay[1, 1:95])
  y <- 1:95
  
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  
  fit <- smooth.spline(x, y, spar = 0.6)
  
  xx <- seq(min(x), max(x), length.out = 500)
  p0 <- predict(fit, xx, deriv = 0)
  p1 <- predict(fit, xx, deriv = 1)
  p2 <- predict(fit, xx, deriv = 2)
  
  curv <- abs(p2$y) / (1 + p1$y^2)^(3/2)
  knee_x <- xx[which.max(curv)]
  knee_y <- predict(fit, knee_x)$y
  
  optimal_minimal_lag <- ceiling(knee_x / 24)
  
  png(file.path(parent_dir, "lag/plots", paste0("lag_curve_", year_month, ".png")), width = 800, height = 600)
  x_days <- x / 24
  
  plot(x_days, y,
       xlab = "Delay (days)",
       ylab = "Percentage of codes diagnosed (%)",
       type = "l",
       ylim = c(0, 100),
       main = paste0("Lag curve ran on ", final_date, " for data 14-90 days ago"))
  
  # integer x values within the plotted range
  x_int <- seq(ceiling(min(x_days)), floor(max(x_days)), by = 1)
  
  # y-values of the curve at those integer x-values
  y_int <- approx(x_days, y, xout = x_int)$y
  
  # vertical lines: from x-axis up to curve
  segments(x0 = x_int, y0 = 0,
           x1 = x_int, y1 = y_int,
           col = "grey70", lty = 2)
  
  # horizontal lines: from y-axis to curve
  segments(x0 = 0, y0 = y_int,
           x1 = x_int, y1 = y_int,
           col = "grey70", lty = 2)
  
  # redraw curve on top
  lines(x_days, y, lwd = 2)
  
  # Add highlighted knee point
  points(knee_x / 24, knee_y, col = "red", pch = 19, cex = 1.5)
  dev.off()
  
  saveRDS(data.frame(x_days, y), file.path(parent_dir, "lag/curves", paste0("lag_curve_", year_month, ".rds")))

}
