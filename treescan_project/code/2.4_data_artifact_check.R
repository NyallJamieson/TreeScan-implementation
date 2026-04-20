# Load back in the delay curve by code
delay_by_code <- readRDS(paste0(parent_dir, "/data_by_code/data_by_code.rds"))

monthly_lags_assessed2 <- list.files(
  paste0(parent_dir, "/lag/curves"),
  pattern = "\\.rds$"
)

# Try get in overall curve
curve <- readRDS(
  paste0(parent_dir, "/lag/curves/", monthly_lags_assessed2[length(monthly_lags_assessed2)])
)

library(dplyr)

# Turn pooled curve into one-row overall_delay object in HOURS
# curve$x_days = quantile times in days
# curve$y      = percentile numbers (1, 2, ..., likely 95)
overall_delay <- as.data.frame(
  setNames(
    as.list(curve$x_days * 24),
    paste0("p", curve$y)
  )
)

# Figure out which percentile columns actually exist in both objects
pcols_delay   <- grep("^p[0-9]+$", names(delay_by_code), value = TRUE)
pcols_overall <- grep("^p[0-9]+$", names(overall_delay), value = TRUE)

pnums_delay   <- as.integer(sub("^p", "", pcols_delay))
pnums_overall <- as.integer(sub("^p", "", pcols_overall))

common_pnums <- sort(intersect(pnums_delay, pnums_overall))
pcols <- paste0("p", common_pnums)

# These are the probabilities actually available
probs <- common_pnums / 100

# Get CDF of code's delay on a time grid
get_cdf_on_grid <- function(qvec, time_grid, probs) {
  keep <- !is.na(qvec) & !is.na(probs)
  
  qvec <- qvec[keep]
  probs_use <- probs[keep]
  
  if (length(qvec) == 0) {
    return(rep(NA_real_, length(time_grid)))
  }
  
  # Order just in case
  ord <- order(qvec, probs_use)
  qvec <- qvec[ord]
  probs_use <- probs_use[ord]
  
  # Anchor at time 0 with CDF 0
  x <- c(0, qvec)
  y <- c(0, probs_use)
  
  approx(
    x = x,
    y = y,
    xout = time_grid,
    method = "linear",
    rule = 2,
    ties = "ordered"
  )$y
}

# Get normalized AUC difference metric
auc_diff_score <- function(q_code, q_overall, probs, T = 48, by = 0.5) {
  time_grid <- seq(0, T, by = by)
  
  F_code <- get_cdf_on_grid(q_code, time_grid, probs)
  F_overall <- get_cdf_on_grid(q_overall, time_grid, probs)
  
  if (all(is.na(F_code)) || all(is.na(F_overall))) {
    return(NA_real_)
  }
  
  diff_vals <- F_code - F_overall
  
  # trapezoid rule
  auc <- sum(diff_vals[-1] + diff_vals[-length(diff_vals)], na.rm = TRUE) * by / 2
  
  # normalize by T
  auc / T
}

# Pull pooled quantiles
overall_q <- as.numeric(overall_delay[1, pcols, drop = TRUE])

# Compute artifact score for each code
artifact_scores <- delay_by_code %>%
  rowwise() %>%
  mutate(
    artifact_score = auc_diff_score(
      q_code = c_across(all_of(pcols)),
      q_overall = overall_q,
      probs = probs,
      T = 48,
      by = 0.5
    )
  ) %>%
  ungroup()

dir.create(paste0(parent_dir, "/data_artifact_assessment/"))
saveRDS(artifact_scores, paste0(parent_dir, "/data_artifact_assessment/artifact_scores.rds"))
