# Full script that runs all the does a lag assessment

# Load required libraries
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(lubridate)
library(data.table)
library(parallel)
library(tibble)

# Find path
path <- paste0(parent_dir, "/raw_data") 

# Load data
files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)

# Join each chunked data together
df_all <- do.call(rbind, lapply(files, read.csv, stringsAsFactors = FALSE))

# Before we want to finish cleaning we need to assess the lag

# First we need to define some key dates
current_date <- Sys.Date()
current_date_minus_14 <- Sys.Date() - 14
current_date_minus_90 <- Sys.Date() - 90
dates_for_lag <- seq.Date(from = current_date_minus_90, to = current_date_minus_14, by = "day")

# Now subset data to time period we're testing for lag structure
df_all_for_lag <- df_all[which(as.Date(df_all$C_Visit_Date_Time) %in% dates_for_lag), ]

setDTthreads(0)

# Parse codes
parse_diag_events <- function(x) {
  ev <- str_split(x, fixed("|"))[[1]]
  
  tibble(
    event_id = as.integer(str_match(ev, "^\\{(\\d+)\\}")[, 2]),
    diag_str = str_match(ev, "^\\{\\d+\\};;(.*)$")[, 2]
  ) %>%
    filter(!is.na(event_id))
}

parse_time_events <- function(x) {
  ev <- str_split(x, fixed("|"))[[1]]
  
  tibble(
    event_id = as.integer(str_match(ev, "^\\{(\\d+)\\}")[, 2]),
    update_time = str_match(
      ev,
      "^\\{\\d+\\};(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})"
    )[, 2]
  ) %>%
    mutate(update_time = ymd_hms(update_time, quiet = TRUE)) %>%
    filter(!is.na(event_id))
}

# Fast chunk processor
.process_nssp_chunk <- function(chunk_df) {
  if (nrow(chunk_df) == 0L) {
    return(data.table(
      row_id = integer(),
      visit_time = as.POSIXct(character(), tz = "UTC"),
      event_id = integer(),
      update_time = as.POSIXct(character(), tz = "UTC"),
      code = character()
    ))
  }
  
  # Keep only rows that could possibly contribute output.
  # This does not change results; rows failing this would have returned empty anyway.
  chunk_df <- chunk_df %>%
    filter(
      !is.na(DischargeDiagnosisUpdates),
      !is.na(DischargeDiagnosisMDTUpdates),
      DischargeDiagnosisUpdates != "",
      DischargeDiagnosisMDTUpdates != ""
    )
  
  if (nrow(chunk_df) == 0L) {
    return(data.table(
      row_id = integer(),
      visit_time = as.POSIXct(character(), tz = "UTC"),
      event_id = integer(),
      update_time = as.POSIXct(character(), tz = "UTC"),
      code = character()
    ))
  }
  
  # -------------------------
  # Diagnosis side
  # Exactly equivalent to calling parse_diag_events() row by row,
  # but done in bulk for the chunk.
  # -------------------------
  diag_list <- str_split(chunk_df$DischargeDiagnosisUpdates, fixed("|"))
  diag_n <- lengths(diag_list)
  
  diag_long <- data.table(
    row_id = rep.int(chunk_df$row_id, diag_n),
    visit_time = rep(chunk_df$visit_time, diag_n),
    diag_ev = unlist(diag_list, use.names = FALSE)
  )
  
  diag_match_event <- str_match(diag_long$diag_ev, "^\\{(\\d+)\\}")
  diag_match_str   <- str_match(diag_long$diag_ev, "^\\{\\d+\\};;(.*)$")
  
  diag_tbl <- data.table(
    row_id = diag_long$row_id,
    visit_time = diag_long$visit_time,
    event_id = as.integer(diag_match_event[, 2]),
    diag_str = diag_match_str[, 2]
  )[!is.na(event_id)]
  
  # -------------------------
  # Time side
  # Exactly equivalent to calling parse_time_events() row by row,
  # but done in bulk for the chunk.
  # -------------------------
  time_list <- str_split(chunk_df$DischargeDiagnosisMDTUpdates, fixed("|"))
  time_n <- lengths(time_list)
  
  time_long <- data.table(
    row_id = rep.int(chunk_df$row_id, time_n),
    time_ev = unlist(time_list, use.names = FALSE)
  )
  
  time_match_event <- str_match(time_long$time_ev, "^\\{(\\d+)\\}")
  time_match_time  <- str_match(
    time_long$time_ev,
    "^\\{\\d+\\};(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})"
  )
  
  time_tbl <- data.table(
    row_id = time_long$row_id,
    event_id = as.integer(time_match_event[, 2]),
    update_time = ymd_hms(time_match_time[, 2], quiet = TRUE)
  )[!is.na(event_id)]
  
  # -------------------------
  # original joined by event_id inside each row
  # bulk equivalent is join by row_id + event_id
  # -------------------------
  ev_tbl <- merge(
    diag_tbl,
    time_tbl,
    by = c("row_id", "event_id"),
    all = FALSE,
    allow.cartesian = TRUE
  )
  
  if (nrow(ev_tbl) == 0L) {
    return(data.table(
      row_id = integer(),
      visit_time = as.POSIXct(character(), tz = "UTC"),
      event_id = integer(),
      update_time = as.POSIXct(character(), tz = "UTC"),
      code = character()
    ))
  }
  
  # -------------------------
  # str_extract_all(...), then unnest(code), then filter(!is.na(code), code != "")
  # -------------------------
  code_list <- str_extract_all(
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
  
  out <- out[
    !is.na(code) & code != "",
    .(row_id, visit_time, event_id, update_time, code)
  ]
  
  out[]
}

# Fast main cleaner
clean_nssp_updates <- function(df, chunk_size = 100000L, cores = 1L) {
  df_prepped <- df %>%
    mutate(
      row_id = row_number(),
      visit_time = ymd_hms(C_Visit_Date_Time, quiet = TRUE)
    ) %>%
    select(row_id, visit_time, DischargeDiagnosisUpdates, DischargeDiagnosisMDTUpdates)
  
  n <- nrow(df_prepped)
  if (n == 0L) {
    return(tibble(
      row_id = integer(),
      visit_time = as.POSIXct(character(), tz = "UTC"),
      event_id = integer(),
      update_time = as.POSIXct(character(), tz = "UTC"),
      code = character()
    ))
  }
  
  starts <- seq.int(1L, n, by = chunk_size)
  ends <- pmin(starts + chunk_size - 1L, n)
  idx <- Map(function(a, b) a:b, starts, ends)
  
  chunk_fun <- function(ii) {
    .process_nssp_chunk(df_prepped[ii, , drop = FALSE])
  }
  
  parts <- if (.Platform$OS.type != "windows" && cores > 1L) {
    mclapply(idx, chunk_fun, mc.cores = cores)
  } else {
    lapply(idx, chunk_fun)
  }
  
  as_tibble(rbindlist(parts, use.names = TRUE, fill = TRUE))
}

# Run main cleaning
df_long <- clean_nssp_updates(
  df_all_for_lag,
  chunk_size = 100000L,
  cores = max(1L, floor(detectCores() / 2))   # set to 1L on Windows if needed
)

# Find first appearance for each code for each patient
first_appearance <- df_long %>%
  group_by(row_id, code) %>%
  summarise(
    visit_time = first(visit_time),
    first_time = min(update_time, na.rm = TRUE),
    delay_hours = as.numeric(difftime(first_time, visit_time, units = "hours")),
    .groups = "drop"
  ) %>%
  filter(!is.na(delay_hours), delay_hours >= 0)

# Get time delay distributions for each specific ICD10 code
delay_by_code <- first_appearance %>%
  group_by(code) %>%
  summarise(
    percentiles = list(
      setNames(
        as.list(
          quantile(
            delay_hours,
            probs = seq(0.01, 1, by = 0.01),
            na.rm = TRUE,
            names = FALSE
          )
        ),
        paste0("p", 1:100)
      )
    ),
    .groups = "drop"
  ) %>%
  unnest_wider(percentiles)

# Overall time delay distribution
overall_delay <- first_appearance %>%
  summarise(
    percentiles = list(
      setNames(
        as.list(
          quantile(
            delay_hours,
            probs = seq(0.01, 1, by = 0.01),
            na.rm = TRUE,
            names = FALSE
          )
        ),
        paste0("p", 1:100)
      )
    )
  ) %>%
  unnest_wider(percentiles)

# Estimate the optimal time delay based on first elbow-like point
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

# plot(x, y, pch = 1)
# lines(p0$x, p0$y)
# points(knee_x, knee_y, col = "red", pch = 19, cex = 1.5)

x_days <- x / 24

plot(x_days, y,
     xlab = "Delay (days)",
     ylab = "Percentage of codes diagnosed (%)",
     type = "l",
     ylim = c(0, 100))

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

