#  Load required libraries
library(openxlsx)

# For epiEngage: Create Signal Report - Ramona Lall (3/9/2026)
# Lists all clusters that meet our signal criteria and looks back for prior 7 days to check if signals are new or persisting
# Classify signals as: New, Increasing, Maximum-outlier, Maximum, Stable, Decreasing
TS_Results_today <- read.csv("C:\\Users\\nj7786\\Documents\\treescan_project\\Results_20250629.csv")
TS_Results_today <- TS_Results_today[, c("Node.Identifier","Node.Name","Recurrence.Interval","Relative.Risk")]

# Fix node for dummy nodes
TS_Results_today$Node.Identifier[grepl("\\|",TS_Results_today$Node.Identifier)]=TS_Results_today$Node.Name[grepl("\\|",TS_Results_today$Node.Identifier)]

# Pull in prior 7 days of results
dts=as.Date((end-7+1):(end-1))

# set working directory to folder with result files
setwd("C:\\Users\\nj7786\\Documents\\treescan_project\\")
old_reports=list.files(pattern =paste0("^", "Results_daily_v2_", ".*\\.csv$"), full.names = TRUE, recursive = TRUE)
old_reports=old_reports[grepl(paste0("Results_daily_v2_",format(dts,"%Y%m%d"),".csv",collapse="|"),old_reports)==T]

for (file in rev(old_reports)) 
{
  temp=read.csv(file)
  temp=temp[is.na(temp$Recurrence.Interval)==F,]
  temp$Node.Identifier=stri_replace_all_fixed(temp$Node.Identifier, "\xa0", "")
  temp=temp[temp$Node.Identifier %in% TS_Results_today$Node.Identifier,]
  temp=temp[,c("Node.Identifier","Time.Window.End","Recurrence.Interval","Relative.Risk")]
  names(temp)[3]=paste0("RI_",format(as.Date(unique(temp$Time.Window.End)),"%Y%m%d"))
  names(temp)[4]=paste0("RR_",format(as.Date(unique(temp$Time.Window.End)),"%Y%m%d"))
  temp=temp[,c(1,3,4)]
  if(nrow(temp)>0)
  {  TS_Results_today=merge(TS_Results_today,temp,by="Node.Identifier",all.x=T,sort=FALSE)}
}

ri_cols <- if (ncol(TS_Results_today) >= 5) seq(5, ncol(TS_Results_today), 2) else integer(0)
rr_cols <- if (ncol(TS_Results_today) >= 6) seq(6, ncol(TS_Results_today), 2) else integer(0)

TS_Results_today <- TS_Results_today[, c(1:3, ri_cols, 4, rr_cols)]

TS_Results_today$Trend=NA

assign_trend <- function(data) {
  trend <- NA
  node1 <- ifelse(grepl("1\\-", data$Node.Identifier), 1, 0)
  
  # today's values
  today_ri <- as.numeric(data$Recurrence.Interval)
  today_rr <- as.numeric(data$Relative.Risk)
  
  # historical RI/RR columns
  ri_idx <- grep("^RI_", colnames(data))
  rr_idx <- grep("^RR_", colnames(data))
  
  # yesterday = first historical day after reordering
  yesterday_ri <- if (length(ri_idx) >= 1) as.numeric(data[[ri_idx[1]]]) else NA
  yesterday_rr <- if (length(rr_idx) >= 1) as.numeric(data[[rr_idx[1]]]) else NA
  
  # RI series for trend checks: today + historical RI columns
  ri_vals <- c(today_ri, as.numeric(unlist(data[ri_idx])))
  ri_non_na <- ri_vals[!is.na(ri_vals)]
  
  # 4+ usable RI points: regression slope
  if (length(ri_non_na) >= 4) {
    recent_data <- data.frame(
      RI = rev(ri_non_na),
      day = seq_along(ri_non_na)
    )
    
    fit <- lm(RI ~ day, data = recent_data)
    coefs <- summary(fit)$coefficients
    
    if (nrow(coefs) >= 2) {
      slope <- coefs[2, 1]
      p_value <- coefs[2, 4]
      
      if (!is.na(p_value)) {
        if (slope > 0 && p_value < 0.05) {
          trend <- "2.Increasing"
        } else if (slope < 0 && p_value < 0.05) {
          trend <- "6.Decreasing"
        } else {
          trend <- "5.Stable"
        }
      }
    }
  }
  
  # 2-3 usable RI points: monotonic check
  if (length(ri_non_na) >= 2 && length(ri_non_na) <= 3) {
    diffs <- diff(rev(ri_non_na))
    if (all(diffs > 0)) {
      trend <- "2.Increasing"
    } else if (all(diffs < 0)) {
      trend <- "6.Decreasing"
    } else {
      trend <- "5.Stable"
    }
  }
  
  # New trend rule
  if (
    (node1 == 1 && !is.na(yesterday_ri) && yesterday_ri < 100) ||
    (node1 == 0 && today_ri >= 365 &&
     ((!is.na(yesterday_ri) && yesterday_ri < 365) ||
      (!is.na(yesterday_rr) && yesterday_rr < 1.3))) ||
    is.na(yesterday_ri) ||
    length(ri_idx) == 0 ||
    all(is.na(as.numeric(unlist(data[ri_idx]))))
  ) {
    trend <- "1.New"
  } else if (!is.na(yesterday_ri) && today_ri == 100000 && yesterday_ri == 100000) {
    trend <- "4.Maximum"
  }
  
  # Maximum-outlier rule
  if (!is.na(yesterday_ri) &&
      today_ri == 100000 &&
      yesterday_ri == 100000 &&
      data$Node.Identifier %in% sigs_maxout) {
    trend <- "3.Maximum-outlier"
  }
  
  return(trend)
}

for (i in seq_len(nrow(TS_Results_today))) {
  TS_Results_today$Trend[i] <- assign_trend(TS_Results_today[i, , drop = FALSE])
}
TS_Results_today=TS_Results_today[order(TS_Results_today$Trend),]
TS_Results_today <- TS_Results_today[c(ncol(TS_Results_today), 1:(ncol(TS_Results_today)-1))]

wb <- createWorkbook()

# Add data frames to sheets
addWorksheet(wb, "Signals")
writeData(wb, "Signals", TS_Results_today)
setColWidths(
  wb,
  sheet = "Signals",
  cols = 1,
  widths = c(19)  # must match number of cols
)

# Save workbook; Provide folder path
saveWorkbook(wb, paste0(getwd(),"/Signals_Report.xlsx"), overwrite = TRUE)

