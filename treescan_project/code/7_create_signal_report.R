# Load required libraries
library(data.table)
library(stringi)
library(openxlsx)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)

delay_by_code <- readRDS(paste0(parent_dir, "/data_by_code/data_by_code.rds"))
artifact_scores <- readRDS(paste0(parent_dir, "/data_artifact_assessment/artifact_scores.rds"))
A <- readRDS(paste0(parent_dir, "/lag/curves/lag_curve_", format(final_date, "%Y-%m"), ".rds"))
DFW <- readRDS(paste0(parent_dir, "/data/data for lag/data_for_lag.rds"))

# Helper: safely close only the device opened for a PNG file
safe_dev_off <- function(dev_id) {
  if (!is.null(dev_id) && dev_id %in% dev.list()) {
    dev.set(dev_id)
    dev.off()
  }
}

if (length(unique(valid_nodes)) > 0) {
  
  # Loop over lags
  for (lag in initial_lags) {
    print(paste0("We are now assessing lag ", lag))
    
    # Common cause: dummy nodes that link different parts of the tree
    common_cause <- read.csv(paste0(parent_dir, "/data/common cause file final.csv"))
    common_cause <- common_cause[is.na(common_cause$X4) == FALSE, ]
    
    # Check if any signals are dummy nodes. If yes, add linked nodes to identifier, separated by "|".
    common_cause_codes <- TS_Results_today$Node.Identifier[
      grepl(
        paste(common_cause$X2, collapse = "|"),
        gsub("2\\-|1\\-|0\\-", "", TS_Results_today$Node.Identifier)
      )
    ]
    
    if (length(common_cause_codes) > 0) {
      for (i in seq_along(common_cause_codes)) {
        idx <- grepl(
          paste(gsub("2\\-", "2\\\\-", gsub("1\\-", "1\\\\-", common_cause_codes[i]))),
          TS_Results_today$Node.Identifier
        )
        
        TS_Results_today$Node.Name[idx] <- TS_Results_today$Node.Identifier[idx]
        
        list_codes <- common_cause$X1[
          grepl(paste(gsub("1\\-|2\\-", "", common_cause_codes[i])), common_cause$X2)
        ]
        
        idx2 <- grepl(paste(common_cause_codes[i]), TS_Results_today$Node.Identifier)
        TS_Results_today$Node.Identifier[idx2] <- paste0(
          c(TS_Results_today$Node.Name[idx2], list_codes),
          collapse = "|"
        )
      }
    }
    
    # For time trend
    Nodes <- TS_Results_today$Node.Identifier.after_dash <- sub(".*-", "", TS_Results_today$Node.Identifier)
    
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
    lookback_dates <- seq(as.Date(Sys.Date()) - 7, as.Date(Sys.Date()) - 1, by = "day")
    lookback_str <- format(lookback_dates, "%Y-%m-%d")
    
    results_dir <- file.path(parent_dir, "results")
    date_dirs <- file.path(results_dir, lookback_dates)
    
    old_reports <- unlist(
      lapply(date_dirs, function(dir) {
        list.files(
          path = dir,
          pattern = "\\.csv$",
          full.names = TRUE,
          ignore.case = TRUE
        )
      })
    )
    
    old_reports <- old_reports[file.exists(old_reports)]
    old_reports <- old_reports[order(old_reports, decreasing = TRUE)]
    
    # -----------------------------
    # 3) Merge prior days safely
    # -----------------------------
    for (file in old_reports) {
      temp <- tryCatch(read.csv(file, stringsAsFactors = FALSE), error = function(e) NULL)
      if (is.null(temp) || nrow(temp) == 0) next
      
      temp <- temp[!is.na(temp$Recurrence.Interval), , drop = FALSE]
      if (nrow(temp) == 0) next
      
      temp$Node.Identifier <- stri_replace_all_fixed(as.character(temp$Node.Identifier), "\xa0", "")
      temp$Node.Identifier <- trimws(temp$Node.Identifier)
      
      if ("Node.Name" %in% names(temp)) {
        dummy_idx_old <- grepl("\\|", temp$Node.Identifier)
        temp$Node.Identifier[dummy_idx_old] <- temp$Node.Name[dummy_idx_old]
        temp$Node.Identifier <- trimws(temp$Node.Identifier)
      }
      
      temp <- temp[temp$Node.Identifier %in% TS_Results_today$Node.Identifier, , drop = FALSE]
      if (nrow(temp) == 0) next
      
      keep_cols <- c("Node.Identifier", "Time.Window.End", "Recurrence.Interval", "Relative.Risk")
      temp <- temp[, keep_cols[keep_cols %in% names(temp)], drop = FALSE]
      
      if (!all(c("Node.Identifier", "Time.Window.End", "Recurrence.Interval", "Relative.Risk") %in% names(temp))) next
      
      file_dt <- sub(".*?(\\d{4}-\\d{2}-\\d{2}).*", "\\1", file)
      
      names(temp)[names(temp) == "Recurrence.Interval"] <- paste0("RI_", format(as.Date(file_dt), "%Y%m%d"))
      names(temp)[names(temp) == "Relative.Risk"] <- paste0("RR_", format(as.Date(file_dt), "%Y%m%d"))
      
      temp <- temp[, c(
        "Node.Identifier",
        paste0("RI_", format(as.Date(file_dt), "%Y%m%d")),
        paste0("RR_", format(as.Date(file_dt), "%Y%m%d"))
      ), drop = FALSE]
      
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
    if (nrow(TS_Results_today) > 0) {
      TS_Results_today$Trend <- NA_character_
    }
    
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
      TS_Results_today$Trend[i] <- assign_trend(
        TS_Results_today[i, , drop = FALSE],
        sigs_maxout = sigs_maxout
      )
    }
    
    if (nrow(TS_Results_today) > 0) {
      TS_Results_today <- TS_Results_today[order(TS_Results_today$Trend, TS_Results_today$Node.Identifier), ]
      TS_Results_today <- TS_Results_today[, c("Trend", setdiff(names(TS_Results_today), "Trend"))]
    }
    
    name <- paste0("TS_Results_Today_", lag)
    assign(name, TS_Results_today)
  }
  
  # Automatically grab all objects named TS_Results_Today_*
  df_names <- ls(pattern = "^TS_Results_Today_\\d+$")
  df_list <- mget(df_names)
  
  # Add lag number to each data frame and bind together
  all_data <- imap_dfr(df_list, ~{
    lag_num <- as.integer(str_extract(.y, "\\d+$"))
    .x %>%
      mutate(
        lag = lag_num,
        source_df = .y
      )
  })
  
  # For each Node.Identifier, keep the row from the smallest lag
  base_rows <- all_data %>%
    group_by(Node.Identifier) %>%
    arrange(lag, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup()
  
  # Build YES/NO presence columns for each lag
  presence_wide <- all_data %>%
    distinct(Node.Identifier, lag) %>%
    mutate(present = "YES") %>%
    pivot_wider(
      names_from = lag,
      values_from = present,
      names_prefix = "LAG ",
      values_fill = "NO"
    )
  
  positive_threshold <- 0.10
  negative_threshold <- -0.10
  
  artifact_flag <- all_data %>%
    distinct(Node.Identifier, lag) %>%
    group_by(Node.Identifier) %>%
    summarise(
      in_lag1 = any(lag == 1),
      in_other_lags = any(lag != 1),
      .groups = "drop"
    ) %>%
    mutate(
      code = sub("^[0-9]+-", "", Node.Identifier)
    ) %>%
    left_join(
      artifact_scores %>%
        select(code, artifact_score),
      by = "code"
    ) %>%
    mutate(
      `Data artifact warning` = if_else(
        !is.na(artifact_score) & artifact_score >= positive_threshold & in_lag1,
        "YES",
        "NO"
      ),
      `Masked in lag 1 warning` = if_else(
        !is.na(artifact_score) & artifact_score <= negative_threshold & !in_lag1 & in_other_lags,
        "YES",
        "NO"
      )
    ) %>%
    select(
      Node.Identifier,
      artifact_score,
      `Data artifact warning`,
      `Masked in lag 1 warning`
    )
  
  # Final output
  final_result <- base_rows %>%
    left_join(artifact_flag, by = "Node.Identifier") %>%
    left_join(presence_wide, by = "Node.Identifier") %>%
    arrange(Node.Identifier)
  
  TS_Results_today <- final_result
  
  # -----------------------------
  # 6) Build workbook: Signals sheet
  # -----------------------------
  wb <- createWorkbook()
  addWorksheet(wb, "Signals")
  
  TS_Results_today <- TS_Results_today %>%
    select(-`Data artifact warning`)
  
  TS_Results_today <- TS_Results_today %>%
    select(-`Masked in lag 1 warning`)
  
  writeDataTable(wb, sheet = "Signals", x = TS_Results_today, tableStyle = "TableStyleMedium2")
  freezePane(wb, sheet = "Signals", firstRow = TRUE, firstCol = TRUE)
  setColWidths(wb, sheet = "Signals", cols = 1:ncol(TS_Results_today), widths = "auto")
  
  header_style <- createStyle(textDecoration = "bold", halign = "center", valign = "center")
  num_style <- createStyle(numFmt = "0.00")
  int_style <- createStyle(numFmt = "0")
  
  addStyle(wb, "Signals", header_style, rows = 1, cols = 1:ncol(TS_Results_today), gridExpand = TRUE)
  
  all_names <- names(TS_Results_today)
  ri_idx <- which(grepl("^RI_|^Recurrence.Interval$", all_names))
  rr_idx <- which(grepl("^RR_|^Relative.Risk$", all_names))
  
  artifact_score_col <- which(names(TS_Results_today) == "artifact_score")
  
  if (length(artifact_score_col) == 1 && nrow(TS_Results_today) > 0) {
    conditionalFormatting(
      wb,
      sheet = "Signals",
      cols = artifact_score_col,
      rows = 2:(nrow(TS_Results_today) + 1),
      type = "colorScale",
      style = c("#006100", "#FFFFFF", "#9C0006"),
      rule = c(-1, 0, 1)
    )
  }
  
  if (length(ri_idx) > 0) {
    addStyle(wb, "Signals", int_style,
             rows = 2:(nrow(TS_Results_today) + 1),
             cols = ri_idx, gridExpand = TRUE, stack = TRUE)
  }
  
  if (length(rr_idx) > 0) {
    addStyle(wb, "Signals", num_style,
             rows = 2:(nrow(TS_Results_today) + 1),
             cols = rr_idx, gridExpand = TRUE, stack = TRUE)
  }
  
  trend_col <- which(names(TS_Results_today) == "Trend")
  if (length(trend_col) == 1) {
    conditionalFormatting(wb, "Signals",
                          cols = trend_col, rows = 2:(nrow(TS_Results_today) + 1),
                          rule = '=="1.New"',
                          style = createStyle(fontColour = "#006100", bgFill = "#C6EFCE"))
    conditionalFormatting(wb, "Signals",
                          cols = trend_col, rows = 2:(nrow(TS_Results_today) + 1),
                          rule = '=="2.Increasing"',
                          style = createStyle(fontColour = "#9C6500", bgFill = "#FFEB9C"))
    conditionalFormatting(wb, "Signals",
                          cols = trend_col, rows = 2:(nrow(TS_Results_today) + 1),
                          rule = '=="3.Maximum-outlier"',
                          style = createStyle(fontColour = "#9C0006", bgFill = "#FFC7CE"))
    conditionalFormatting(wb, "Signals",
                          cols = trend_col, rows = 2:(nrow(TS_Results_today) + 1),
                          rule = '=="4.Maximum"',
                          style = createStyle(fontColour = "#9C0006", bgFill = "#F4CCCC"))
  }
  
  # -----------------------------
  # 7) Add plots sheet
  # SAFER BATCH/TASK-SCHEDULER VERSION:
  #   - writes each plot to a real PNG file
  #   - closes the PNG device
  #   - inserts the finished image into Excel
  #   - avoids insertPlot(), which can capture blank devices in non-interactive jobs
  # -----------------------------
  addWorksheet(wb, "Plots")
  
  writeData(wb, "Plots", "Node plots", startRow = 1, startCol = 1)
  addStyle(wb, "Plots",
           createStyle(textDecoration = "bold", fontSize = 14),
           rows = 1, cols = 1)
  
  node_identifiers <- TS_Results_today$Node.Identifier
  node_codes <- sub(".*-", "", node_identifiers)
  
  # Layout: 2 plots per row block
  start_row <- 3
  start_col <- 1
  plot_width <- 6
  plot_height <- 4
  row_step <- 22
  col_step <- 10
  
  plot_num <- 0
  
  # Stable directory to hold plot files until the workbook is saved
  plot_dir <- file.path(parent_dir, "signal_report", "plot_pngs")
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Pull out ICD-like codes from the whole column once
  all_codes <- regmatches(
    DFW$DischargeDiagnosisUpdates,
    gregexpr("\\b[A-Z][0-9]{2}(?:\\.[0-9A-Z]+)?\\b", DFW$DischargeDiagnosisUpdates)
  )
  
  code_counts <- data.table(code = unlist(all_codes))[, .N, by = code][order(-N)]
  code_counts <- sum(code_counts$N)
  
  if (is.na(code_counts) || code_counts <= 0) {
    warning("code_counts is zero/NA; proportion plots may be skipped.")
  }
  
  for (k in seq_along(node_codes)) {
    print(k)
    
    i <- node_codes[k]
    node_label <- node_identifiers[k]
    
    which_row <- which(delay_by_code$code == i)
    if (length(which_row) == 0) next
    
    # If duplicate matches exist, use first one
    which_row <- which_row[1]
    
    xvals <- as.numeric(delay_by_code[which_row, 2:101])
    if (all(is.na(xvals))) next
    
    i_esc <- stringr::str_escape(i)
    
    num1 <- DFW[, count := str_count(
      DischargeDiagnosisUpdates,
      regex(paste0("(?<![A-Z0-9.])", i_esc, "(?![A-Z0-9.])"))
    )]
    
    num <- sum(num1$count, na.rm = TRUE)
    
    if (num == 0) {
      num <- sum(str_count(DFW$DischargeDiagnosis, paste0(";", i_esc)), na.rm = TRUE)
    }
    
    # Build lookup from actual xvals
    x_lookup <- xvals / 24
    y_lookup <- seq_along(xvals)
    
    get_y_for_x <- function(x_given) {
      approx(
        x = x_lookup,
        y = y_lookup,
        xout = x_given,
        rule = 2
      )$y
    }
    
    x_lookup2 <- A$x_days
    y_lookup2 <- A$y
    
    get_y_for_x2 <- function(x_given) {
      approx(
        x = x_lookup2,
        y = y_lookup2,
        xout = x_given,
        rule = 2
      )$y
    }
    
    result <- tryCatch(
      get_y_for_x(seq(0, 5, by = 0.01)),
      error = function(e) {
        message("Skipping ", i, ": ", conditionMessage(e))
        return(NULL)
      }
    )
    
    if (is.null(result)) next
    if (is.na(code_counts) || code_counts <= 0) next
    
    y1 <- get_y_for_x(seq(0, 5, by = 0.01))
    y2 <- get_y_for_x2(seq(0, 5, by = 0.01))
    frac <- 100 * (num * y1) / (code_counts * y2)
    
    # Guard against bad frac values that would make ylim fail
    if (all(is.na(frac)) || all(!is.finite(frac))) {
      message("Skipping ", i, ": frac is all NA/non-finite")
      next
    }
    
    frac[!is.finite(frac)] <- NA_real_
    
    plot_num <- plot_num + 1
    
    block_row <- start_row + ((plot_num - 1) %/% 2) * row_step
    block_col <- start_col + ((plot_num - 1) %% 2) * col_step
    
    writeData(wb, "Plots", node_label, startRow = block_row, startCol = block_col)
    addStyle(wb, "Plots",
             createStyle(textDecoration = "bold"),
             rows = block_row, cols = block_col)
    
    # Make a safe filename for this plot
    safe_code <- gsub("[^A-Za-z0-9_.-]", "_", i)
    plot_file <- file.path(plot_dir, paste0("plot_", sprintf("%04d", plot_num), "_", safe_code, ".png"))
    
    # Open a real PNG device. This is the key batch-safe change.
    png(
      filename = plot_file,
      width = plot_width,
      height = plot_height,
      units = "in",
      res = 150
    )
    dev_id <- dev.cur()
    
    tryCatch({
      par(mar = c(5, 5, 4, 6) + 0.1)
      
      # CDF plot
      plot(
        xvals / 24, 1:100,
        xlim = c(0, 5),
        pch = 1,
        main = paste0("Node ", node_label, " ESSENCE-upload delay distribution"),
        xlab = "Time in days",
        ylab = "Percentage of diagnoses reported after ED visit",
        type = "l",
        lwd = 2
      )
      
      lines(A$x_days, A$y, lty = 2, col = "grey", lwd = 2)
      
      x_frac <- seq(0, 5, length.out = length(frac))
      
      # Overlay frac with right y-axis
      par(new = TRUE)
      
      plot(
        x_frac, frac,
        type = "l",
        axes = FALSE,
        xlab = "",
        ylab = "",
        xlim = c(0, 5),
        ylim = range(frac, na.rm = TRUE),
        col = "red"
      )
      
      axis(4, col = "red")
      mtext("Proportion of diagnoses", side = 4, line = 3)
      
      legend(
        "bottomright",
        legend = c("ICD-specific code", "Pooled", "Proportion of volume"),
        lty = c(1, 2, 1),
        lwd = c(2, 2, 1),
        col = c("black", "grey", "red"),
        cex = 0.75,
        inset = 0.02,
        bg = "white"
      )
    }, error = function(e) {
      message("Plot failed for ", i, ": ", conditionMessage(e))
    }, finally = {
      safe_dev_off(dev_id)
    })
    
    # Only insert if the PNG was actually created
    if (file.exists(plot_file) && file.info(plot_file)$size > 0) {
      insertImage(
        wb,
        sheet = "Plots",
        file = plot_file,
        startRow = block_row + 1,
        startCol = block_col,
        width = plot_width,
        height = plot_height,
        units = "in"
      )
    } else {
      message("PNG was not created for ", i)
    }
  }
  
  setColWidths(wb, "Plots", cols = 1:20, widths = 14)
  
  # -----------------------------
  # 8) Save
  # -----------------------------
  out_file <- file.path(parent_dir, "signal_report", paste0("Signals_Report_", final_date, ".xlsx"))
  saveWorkbook(wb, out_file, overwrite = TRUE)
  
  message("Workbook saved to: ", out_file)
  
} else {
  print("You have no new signals")
}
