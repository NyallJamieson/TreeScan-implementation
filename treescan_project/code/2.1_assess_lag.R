# Load required libraries
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(lubridate)

# Tidy ICD data
parse_diag_events <- function(x) {
  ev <- str_split(x, fixed("|"))[[1]]
  
  tibble(
    event_id = as.integer(str_match(ev, "^\\{(\\d+)\\}")[, 2]),
    diag_str = str_match(ev, "^\\{\\d+\\};;(.*)$")[, 2]
  ) %>%
    filter(!is.na(event_id))
}

# Time time data
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

# Gets code and timing for each update
clean_nssp_updates <- function(df) {
  
  df %>%
    mutate(
      row_id = row_number(),
      visit_time = ymd_hms(C_Visit_Date_Time, quiet = TRUE)
    ) %>%
    select(row_id, visit_time, DischargeDiagnosisUpdates, DischargeDiagnosisMDTUpdates) %>%
    pmap_dfr(function(row_id, visit_time, DischargeDiagnosisUpdates, DischargeDiagnosisMDTUpdates) {
      
      diag_tbl <- parse_diag_events(DischargeDiagnosisUpdates)
      time_tbl <- parse_time_events(DischargeDiagnosisMDTUpdates)
      
      ev_tbl <- inner_join(diag_tbl, time_tbl, by = "event_id")
      
      if (nrow(ev_tbl) == 0) {
        return(tibble())
      }
      
      ev_tbl %>%
        mutate(
          row_id = row_id,
          visit_time = visit_time,
          code = map(
            diag_str,
            ~ str_extract_all(
              .x,
              "[A-Z][0-9][0-9A-Z](?:\\.[0-9A-Z]{1,4})?"
            )[[1]]
          )
        ) %>%
        unnest(code) %>%
        filter(!is.na(code), code != "") %>%
        select(row_id, visit_time, event_id, update_time, code)
    })
}

# Now tidy the data
df_long <- clean_nssp_updates(df_all_for_lag)

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

# Now get overall time delay distribution
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

# Now estimate the optimal time delay based on first elbow-like point


x <- as.numeric(overall_delay[1, 1:95])
y <- 1:95

ord <- order(x)
x <- x[ord]
y <- y[ord]

# Fit smooth curve to it
fit <- smooth.spline(x, y, spar = 0.6)

# predict derivatives on a fine grid
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

optimal_minimal_lag <- ceiling(knee_x / 24)
