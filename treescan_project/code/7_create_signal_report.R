library(data.table)
library(stringi)
library(openxlsx)

# -----------------------------
# 1) Prepare today's signals
# -----------------------------
TS_Results_today <- TS_Results_today[, c("Node.Identifier", "Node.Name", "Recurrence.Interval", "Relative.Risk")]

# Fix node for dummy nodes
dummy_idx <- grepl("\\|", TS_Results_today$Node.Identifier)
TS_Results_today$Node.Identifier[dummy_idx] <- TS_Results_today$Node.Name[dummy_idx]

# Normalize types
TS_Results_today$Node.Identifier <- trimws(as.character(TS_Results_today$Node.Identifier))
TS_Results_today$Recurrence.Interval <- as.numeric(TS_Results_today$Recurrence.Interval)
TS_Results_today$Relative.Risk <- as.numeric(TS_Results_today$Relative.Risk)

# -----------------------------
# 2) Only pull prior 7 days
# -----------------------------
lookback_dates <- seq(as.Date(END_DATE) - 7, as.Date(END_DATE) - 1, by = "day")
lookback_str <- format(lookback_dates, "%Y-%m-%d")

results_dir <- file.path(parent_dir, "results")

# Build exact expected file paths instead of scanning everything
old_reports <- file.path(
  results_dir,
  lookback_str,
  paste0("Results_", lookback_str, ".csv")
)

# Keep only files that actually exist
old_reports <- old_reports[file.exists(old_reports)]

# Sort newest to oldest so RI_YYYYMMDD columns appear in sensible order
old_reports <- old_reports[order(old_reports, decreasing = TRUE)]

# -----------------------------
# 3) Merge prior days safely
# -----------------------------
for (file in old_reports) {
  temp <- tryCatch(read.csv(file, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(temp) || nrow(temp) == 0) next
  
  # Keep only rows with recurrence interval
  temp <- temp[!is.na(temp$Recurrence.Interval), , drop = FALSE]
  if (nrow(temp) == 0) next
  
  # Clean identifier
  temp$Node.Identifier <- stri_replace_all_fixed(as.character(temp$Node.Identifier), "\xa0", "")
  temp$Node.Identifier <- trimws(temp$Node.Identifier)
  
  # Fix dummy nodes in historical files too, if applicable
  if ("Node.Name" %in% names(temp)) {
    dummy_idx_old <- grepl("\\|", temp$Node.Identifier)
    temp$Node.Identifier[dummy_idx_old] <- temp$Node.Name[dummy_idx_old]
    temp$Node.Identifier <- trimws(temp$Node.Identifier)
  }
  
  # Restrict to nodes present today
  temp <- temp[temp$Node.Identifier %in% TS_Results_today$Node.Identifier, , drop = FALSE]
  if (nrow(temp) == 0) next
  
  # Keep only needed cols
  keep_cols <- c("Node.Identifier", "Time.Window.End", "Recurrence.Interval", "Relative.Risk")
  temp <- temp[, keep_cols[keep_cols %in% names(temp)], drop = FALSE]
  
  if (!all(c("Node.Identifier", "Time.Window.End", "Recurrence.Interval", "Relative.Risk") %in% names(temp))) next
  
  # Use file date instead of temp$Time.Window.End[1], which can be messy
  file_dt <- as.Date(sub("^Results_(\\d{4}-\\d{2}-\\d{2})\\.csv$", "\\1", basename(file)))
  
  names(temp)[names(temp) == "Recurrence.Interval"] <- paste0("RI_", format(file_dt, "%Y%m%d"))
  names(temp)[names(temp) == "Relative.Risk"] <- paste0("RR_", format(file_dt, "%Y%m%d"))
  
  temp <- temp[, c("Node.Identifier", paste0("RI_", format(file_dt, "%Y%m%d")), paste0("RR_", format(file_dt, "%Y%m%d"))), drop = FALSE]
  
  # Prevent duplicate node rows from exploding merge
  temp <- temp[!duplicated(temp$Node.Identifier), , drop = FALSE]
  
  TS_Results_today <- merge(
    TS_Results_today,
    temp,
    by = "Node.Identifier",
    all.x = TRUE,
    sort = FALSE
  )
}

# -----------------------------
# 4) Reorder columns cleanly
# -----------------------------
ri_cols <- grep("^RI_", names(TS_Results_today), value = TRUE)
rr_cols <- grep("^RR_", names(TS_Results_today), value = TRUE)

# Sort date columns newest -> oldest
ri_cols <- ri_cols[order(ri_cols, decreasing = TRUE)]
rr_cols <- rr_cols[order(rr_cols, decreasing = TRUE)]

TS_Results_today <- TS_Results_today[, c(
  "Node.Identifier",
  "Node.Name",
  "Recurrence.Interval",
  ri_cols,
  "Relative.Risk",
  rr_cols
)]

# -----------------------------
# 5) Assign trend
# -----------------------------
TS_Results_today$Trend <- NA_character_

assign_trend <- function(data_row, sigs_maxout = character(0)) {
  trend <- "5.Stable"
  
  node_id <- as.character(data_row$Node.Identifier)
  node1 <- grepl("^1\\-", node_id)
  
  today_ri <- suppressWarnings(as.numeric(data_row$Recurrence.Interval))
  today_rr <- suppressWarnings(as.numeric(data_row$Relative.Risk))
  
  ri_cols <- grep("^RI_", names(data_row), value = TRUE)
  rr_cols <- grep("^RR_", names(data_row), value = TRUE)
  
  ri_vals <- suppressWarnings(as.numeric(data_row[, ri_cols, drop = TRUE]))
  rr_vals <- suppressWarnings(as.numeric(data_row[, rr_cols, drop = TRUE]))
  
  yesterday_ri <- if (length(ri_vals) >= 1) ri_vals[1] else NA_real_
  yesterday_rr <- if (length(rr_vals) >= 1) rr_vals[1] else NA_real_
  
  non_missing_ri <- ri_vals[!is.na(ri_vals)]
  
  # Trend from history
  if (length(non_missing_ri) >= 4) {
    recent_data <- data.frame(
      RI = rev(non_missing_ri),
      day = seq_along(non_missing_ri)
    )
    
    fit <- lm(RI ~ day, data = recent_data)
    coefs <- summary(fit)$coefficients
    
    if (nrow(coefs) >= 2 && !is.na(coefs[2, 4])) {
      slope <- coefs[2, 1]
      p_value <- coefs[2, 4]
      
      if (slope > 0 && p_value < 0.05) {
        trend <- "2.Increasing"
      } else if (slope < 0 && p_value < 0.05) {
        trend <- "6.Decreasing"
      } else {
        trend <- "5.Stable"
      }
    }
  } else if (length(non_missing_ri) >= 2) {
    diffs <- diff(rev(non_missing_ri))
    
    if (all(diffs > 0)) {
      trend <- "2.Increasing"
    } else if (all(diffs < 0)) {
      trend <- "6.Decreasing"
    } else {
      trend <- "5.Stable"
    }
  }
  
  # New
  if (
    is.na(yesterday_ri) ||
    length(non_missing_ri) == 0 ||
    (node1 && !is.na(yesterday_ri) && yesterday_ri < 100) ||
    (!node1 && today_ri >= 365 &&
     (
       (!is.na(yesterday_ri) && yesterday_ri < 365) ||
       (!is.na(yesterday_rr) && yesterday_rr < 1.3)
     )
    )
  ) {
    trend <- "1.New"
  }
  
  # Maximum / Maximum-outlier
  if (!is.na(yesterday_ri) && today_ri == 100000 && yesterday_ri == 100000) {
    trend <- "4.Maximum"
  }
  
  if (!is.na(yesterday_ri) &&
      today_ri == 100000 &&
      yesterday_ri == 100000 &&
      node_id %in% sigs_maxout) {
    trend <- "3.Maximum-outlier"
  }
  
  trend
}

for (i in seq_len(nrow(TS_Results_today))) {
  TS_Results_today$Trend[i] <- assign_trend(TS_Results_today[i, , drop = FALSE], sigs_maxout = sigs_maxout)
}

# Put Trend first and sort
TS_Results_today <- TS_Results_today[order(TS_Results_today$Trend, TS_Results_today$Node.Identifier), ]
TS_Results_today <- TS_Results_today[, c("Trend", setdiff(names(TS_Results_today), "Trend"))]

# -----------------------------
# 6) Build a usable workbook
# -----------------------------
wb <- createWorkbook()
addWorksheet(wb, "Signals")

writeDataTable(wb, sheet = "Signals", x = TS_Results_today, tableStyle = "TableStyleMedium2")

# Freeze top row and first column
freezePane(wb, sheet = "Signals", firstRow = TRUE, firstCol = TRUE)

# Auto filter is handled by writeDataTable, but widths need fixing
setColWidths(wb, sheet = "Signals", cols = 1:ncol(TS_Results_today), widths = "auto")

# Styles
header_style <- createStyle(textDecoration = "bold", halign = "center", valign = "center")
num_style <- createStyle(numFmt = "0.00")
int_style <- createStyle(numFmt = "0")

addStyle(wb, "Signals", header_style, rows = 1, cols = 1:ncol(TS_Results_today), gridExpand = TRUE)

# Apply styles by column name
all_names <- names(TS_Results_today)

ri_idx <- which(grepl("^RI_|^Recurrence.Interval$", all_names))
rr_idx <- which(grepl("^RR_|^Relative.Risk$", all_names))

if (length(ri_idx) > 0) {
  addStyle(wb, "Signals", int_style, rows = 2:(nrow(TS_Results_today) + 1), cols = ri_idx, gridExpand = TRUE, stack = TRUE)
}
if (length(rr_idx) > 0) {
  addStyle(wb, "Signals", num_style, rows = 2:(nrow(TS_Results_today) + 1), cols = rr_idx, gridExpand = TRUE, stack = TRUE)
}

# Optional conditional formatting for Trend
trend_col <- which(names(TS_Results_today) == "Trend")
if (length(trend_col) == 1) {
  conditionalFormatting(wb, "Signals",
                        cols = trend_col, rows = 2:(nrow(TS_Results_today) + 1),
                        rule = '=="1.New"', style = createStyle(fontColour = "#006100", bgFill = "#C6EFCE")
  )
  conditionalFormatting(wb, "Signals",
                        cols = trend_col, rows = 2:(nrow(TS_Results_today) + 1),
                        rule = '=="2.Increasing"', style = createStyle(fontColour = "#9C6500", bgFill = "#FFEB9C")
  )
  conditionalFormatting(wb, "Signals",
                        cols = trend_col, rows = 2:(nrow(TS_Results_today) + 1),
                        rule = '=="3.Maximum-outlier"', style = createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE")
  )
  conditionalFormatting(wb, "Signals",
                        cols = trend_col, rows = 2:(nrow(TS_Results_today) + 1),
                        rule = '=="4.Maximum"', style = createStyle(fontColour = "#9C0006", bgFill = "#F4CCCC")
  )
}

# Save
out_file <- file.path(parent_dir, "signal_report", paste0("Signals_Report_", END_DATE, ".xlsx"))
saveWorkbook(wb, out_file, overwrite = TRUE)

message("Workbook saved to: ", out_file)