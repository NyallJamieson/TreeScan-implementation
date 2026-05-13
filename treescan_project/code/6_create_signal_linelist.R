# For epiEngage: Create Signal Linelists - Ramona Lall (3/9/2026)

# Load required libraries
library("sf")
library("ggplot2")
library("dplyr")
library("scales")
library("dplyr")
library("readr")
library("lubridate")
library("MMWRweek")
library("stringi")
library("lubridate")
library("openxlsx")
library("stringr")

# Helper functions for if we get bad characters
normalize_text_utf8 <- function(x) {
  x <- as.character(x)
  
  # First pass: mark declared encoding as UTF-8 where possible
  x1 <- stringi::stri_enc_toutf8(x, is_unknown_8bit = TRUE, validate = TRUE)
  
  # Second pass: for anything still invalid, try common legacy encodings
  bad <- !is.na(x1) & !stringi::stri_enc_isutf8(x1)
  if (any(bad)) {
    x1[bad] <- iconv(x[bad], from = "latin1", to = "UTF-8", sub = "byte")
  }
  
  # Final cleanup: guarantee valid UTF-8 strings
  x1 <- iconv(x1, from = "", to = "UTF-8", sub = "byte")
  x1
}

normalize_for_tokens <- function(x) {
  x <- normalize_text_utf8(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- gsub("[[:punct:]]", " ", x, perl = TRUE)
  x <- toupper(x)
  x
}

# We want to keep track of all valid nodes
all_valid_nodes <- c()

Nodes <- c()

TS_Results_all <- data.frame(
  Cut.No. = integer(),
  Node.Identifier = character(),
  Node.Name = character(),
  Tree.Level = integer(),
  Node.Cases = integer(),
  Time.Window.Start = character(),
  Time.Window.End = character(),
  Cases.in.Window = integer(),
  Expected.Cases = numeric(),
  Relative.Risk = numeric(),
  Excess.Cases = numeric(),
  Test.Statistic = numeric(),
  P.value = numeric(),
  Recurrence.Interval = numeric(),
  Parent.Node = character(),
  Parent.Node.Name = character(),
  Branch.Order = integer(),
  stringsAsFactors = FALSE,
  check.names = FALSE
)
  
for (lag in initial_lags){
  
  # Get required nodes
  
  # Read in Results csv file (edit to match naming convention)
  TS_Results_today <- read.csv(paste0(parent_dir, "/results/", final_date, "/Results_lag", lag, "_", final_date, ".csv"))
  # Signal criteria
  TS_Results_today <- TS_Results_today[is.na(TS_Results_today$Recurrence.Interval) == F, ]
  TS_Results_today <- TS_Results_today[which(TS_Results_today$Relative.Risk>=1.3),]
  # Admit signals have a lower threshold
  TS_Results_today <- TS_Results_today[which((TS_Results_today$Recurrence.Interval >= 365)|(grepl("1\\-",TS_Results_today$Node.Identifier) & TS_Results_today$Recurrence.Interval>=100)),]
  TS_Results_today$Node.Identifier=stri_replace_all_fixed(TS_Results_today$Node.Identifier, "\xa0", "")
  
  TS_Results_all <- rbind(TS_Results_all, TS_Results_today)
  
  Nodes <- unique(append(Nodes, sub(".*-", "", TS_Results_today[,2])))
  print(Nodes)
}
  
TS_Results_today <- TS_Results_all

is_related_icd10 <- function(a, b) {
  # Remove dots so T83.511 and T83511 compare cleanly
  a_clean <- gsub("\\.", "", a)
  b_clean <- gsub("\\.", "", b)
  
  startsWith(a_clean, b_clean) || startsWith(b_clean, a_clean)
}

keep_strongest_branch_signals <- function(nodes) {
  kept <- character()
  
  for (node in nodes) {
    # only apply this logic to ICD-10-like codes
    is_icd <- grepl("^[A-Z][0-9]", node)
    
    if (!is_icd) {
      kept <- c(kept, node)
      next
    }
    
    already_represented <- any(vapply(
      kept,
      function(k) {
        grepl("^[A-Z][0-9]", k) && is_related_icd10(node, k)
      },
      logical(1)
    ))
    
    if (!already_represented) {
      kept <- c(kept, node)
    }
  }
  
  kept
}

filtered_nodes <- keep_strongest_branch_signals(Nodes)

TS_Results_today <- TS_Results_today %>%
  mutate(
    node_clean = str_remove(Node.Identifier, "^[0-9]+-")
  ) %>%
  filter(node_clean %in% filtered_nodes) %>%
  distinct(node_clean, .keep_all = TRUE)

# Reload just in case
load(file.path(parent_dir, "myProfile.rda"))
  
if (length(Nodes) > 0){
    
  # For time trend we need to download some background data
  source(paste0(parent_dir, "/code/6.1_download_background_for_interpretation.R"))
    
  # Now add to all_valid_nodes
  all_valid_nodes <- unique(valid_nodes)
  
}

# In 6.1 we check if no valid nodes after cleaning
# If so then we want to skip this lag and move to the next
# if (length(all_valid_nodes == 0)){
#   next
# }
    
# For time trend table pull in current year and additional 3 prior years
yr_list <- (as.numeric(format(final_date,"%Y"))-3) : as.numeric(format(final_date,"%Y"))

# Bit of tidying up before importing data
files_all <- list.files(paste0(parent_dir, "/data_for_interpretation"),
                        pattern = ".csv$",
                        full.names = TRUE
)

# Keep only files for the years we actually need
files <- files_all[
  grepl(
    paste0("(", paste(yr_list, collapse = "|"), ")"),
    basename(files_all)
  )
]

files <- sort(files)

message("Expected years: ", paste(yr_list, collapse = ", "))
message("Files selected:")
print(basename(files))

if (length(files) != length(yr_list)) {
  stop(
    "Expected ", length(yr_list), " files for years ",
    paste(yr_list, collapse = ", "),
    " but found ", length(files), ": ",
    paste(basename(files), collapse = ", ")
  )
}

year_dfs <- lapply(files, function(f) {
  read_csv(
    f,
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )
})

# However you import data; I use import_data_specdates function
archive_deduped <- rbind(year_dfs[[1]], year_dfs[[2]], year_dfs[[3]], year_dfs[[4]])
ed1 <- year_dfs[[1]]
ed1$date <- as.Date(ed1$C_Visit_Date_Time)
ed1 <- ed1[,c("date","Hospital","DischargeDiagnosis","DischargeDisposition")]
ed2 <- year_dfs[[2]]
ed2$date <- as.Date(ed2$C_Visit_Date_Time)
ed2 <- ed2[,c("date","Hospital","DischargeDiagnosis","DischargeDisposition")]
ed3 <- year_dfs[[3]]
ed3$date <- as.Date(ed3$C_Visit_Date_Time)
ed3 <- ed3[,c("date","Hospital","DischargeDiagnosis","DischargeDisposition")]
ed4 <- year_dfs[[4]]
ed4$date <- as.Date(ed4$C_Visit_Date_Time)
ed4 <- ed4[,c("date","Hospital","DischargeDiagnosis","DischargeDisposition")]

ed <- rbind(ed1,ed2,ed3,ed4)
    
ed$dispo <- "V" #visit
ed$dispo[ed$DischargeDisposition %in% c("09","9","09A","Adm","ADM","20","_Expired","Expired","22","23","24","25","26","27","28","29","40", "41", "42")] <- "A" #adverse

ed$diagnosiscode1 <- gsub("\\.", "", ed$DischargeDiagnosis)

ed <- cbind(MMWRweek::MMWRweek(ed$date), ed)

mmwrwks <- unique(MMWRweek::MMWRweek(unique(ed$date))[, - 3])

# For maps
library(sf)
library(dplyr)
library(tigris)

options(tigris_use_cache = TRUE)

# National ZCTA shapefile from Census via tigris
# cb = TRUE gives the lighter generalized file; set cb = FALSE if you want full detail
modzcta <- tigris::zctas(year = 2020, cb = TRUE, class = "sf")

# Keep the downstream join working by creating the same join key name
modzcta <- modzcta %>%
  mutate(modzcta = ZCTA5CE20)

# Create the same lookup structure your old code expected:
# zipcode + MODZCTA20
df <- modzcta %>%
  st_drop_geometry() %>%
  transmute(
    zipcode = ZCTA5CE20,
    MODZCTA20 = ZCTA5CE20
  )

# call in tree that has each code and has columns Level1--Level8
icd10.2026_treefilew_FINAL <- read.delim(paste0(parent_dir, "/data/Tree_File_2026_wide_format.txt"))

# for list of max signals that have outlier trend (need for Signal Report)
sigs_maxout <- character()

# An excel workbook which has a linelist for each signal and additional analyses
# The tab sheets include: 
# 1. line-level for Cluster - identifying incident vs non-incident [Include all variables of interest but at minimum for additional tabs see below fields needed]
# 2. line-level for Baseline (if it exists) - identifying incident vs non-incident
# 3. Frequency table of other co-diagnoses for cluster and baseline [diagnosiscode]
# 4. Chief complaint word frequency for cluster and baseline [chief complaint]
# 5. Multi-year weekly trend 
# 6. Maps for cluster and baseline [zipcode/MODZCTA]
# 7. Hospital graphic [hospital]
# 8. Demographic table comparing cluster to baseline [sex, age, race, ethnicity, boro]

# you need the original dataset "archive_deduped" (includes non-incident diagnoses) with all variables you want to see in line-level: "date","time","key","hospital","zipcode","patientid","sex","age","age_group","race","ethnicity","chiefcomplaint","admitreason","diagnosiscode","diagnosistext","diagnosiscode1","modeofarrival","travelhistory","dischargedisposition","dispo","dischargedate","triagenote"
# Of these, you must have: "date","key","hospital","zipcode","sex","age","age_group","race","ethnicity","chiefcomplaint","diagnosiscode1","dispo"
archive_deduped$date <- as.Date(archive_deduped$C_Visit_Date_Time)
archive_deduped$time <- format(
  as.POSIXct(archive_deduped$C_Visit_Date_Time, tz = "UTC"),
  "%H:%M:%S"
)

archive_deduped$chiefcomplaint <- normalize_text_utf8(archive_deduped$ChiefComplaintParsed)
archive_deduped$triagenote <- normalize_text_utf8(archive_deduped$TriageNotesOrig)
archive_deduped$diagnosistext <- normalize_text_utf8(archive_deduped$Diagnosis_Combo)
archive_deduped$admitreason <- normalize_text_utf8(archive_deduped$Admit_Reason_Code)

archive_deduped$key <- archive_deduped$C_Unique_Patient_ID
archive_deduped$hospital <- archive_deduped$HospitalName
archive_deduped$zipcode <- archive_deduped$Patient_Zip
archive_deduped$patientid <- archive_deduped$C_Unique_Patient_ID
archive_deduped$sex <- archive_deduped$Sex
archive_deduped$race <- archive_deduped$C_Race
archive_deduped$ethnicity <- archive_deduped$C_Ethnicity
# archive_deduped$chiefcomplaint <- archive_deduped$ChiefComplaintParsed
# archive_deduped$admitreason <- archive_deduped$Admit_Reason_Code
archive_deduped$diagnosiscode <- archive_deduped$DischargeDiagnosis
archive_deduped$diagnosiscode1 <- archive_deduped$diagnosiscode
# archive_deduped$diagnosistext <- archive_deduped$Diagnosis_Combo
archive_deduped$dischargedisposition <- archive_deduped$DischargeDisposition
archive_deduped$dispo <- ed$dispo
archive_deduped$modeofarrival <- archive_deduped$ModeOfArrival
archive_deduped$travelhistory <- archive_deduped$Travel_History
# archive_deduped$triagenote <- archive_deduped$TriageNotesOrig
archive_deduped$age <- archive_deduped$Age                                                                                                                            
archive_deduped$dischargedate <- archive_deduped$Discharge_Date_Time

# derive age_group from age using the bins expected later in the script
archive_deduped$age_group <- cut(
  as.numeric(archive_deduped$age),
  breaks = c(-Inf, 0, 4, 12, 17, 49, 64, 79, Inf),
  labels = c("<1", "1-4", "5-12", "13-17", "18-49", "50-64", "65-79", "80+"),
  right = TRUE
)

archive_deduped$age_group <- as.character(archive_deduped$age_group)

# Load in the count file
v2 <- read.csv(paste0(parent_dir, "/data/v2/", final_date, "/lag", lag, ".csv"))

# Common cause (these are for the dummy nodes that link different parts of the tree)
common_cause <- read.csv(paste0(parent_dir, "/data/common cause file final.csv"))
common_cause <- common_cause[is.na(common_cause$X4)==F,]

# check if any of the signals are for dummy node and if any, add the different linked nodes to the identifier value separated by "|"
common_cause_codes <- TS_Results_today$Node.Identifier[
  grepl(
    paste(common_cause$X2, collapse = "|"),
    gsub("2\\-|1\\-|0\\-", "", TS_Results_today$Node.Identifier)
  )
]

if (length(common_cause_codes) > 0) {
  for (j in seq_along(common_cause_codes)) {
    this_code <- common_cause_codes[j]
    
    hit_idx <- grepl(this_code, TS_Results_today$Node.Identifier, fixed = TRUE)
    
    TS_Results_today$Node.Name[hit_idx] <- TS_Results_today$Node.Identifier[hit_idx]
    
    list_codes <- common_cause$X1[
      grepl(gsub("1\\-|2\\-", "", this_code), common_cause$X2, fixed = TRUE)
    ]
    
    TS_Results_today$Node.Identifier[hit_idx] <- paste(
      unique(c(TS_Results_today$Node.Name[hit_idx], list_codes)),
      collapse = "|"
    )
  }
}

END_DATE <- final_date - lag

keep_cols <- c(
  "date","time","key","hospital","zipcode","patientid","sex","age","age_group",
  "race","ethnicity","chiefcomplaint","admitreason","diagnosiscode","diagnosistext",
  "diagnosiscode1","modeofarrival","travelhistory","dischargedisposition","dispo",
  "dischargedate","triagenote"
)

# archive_deduped match
dx_clean_archive <- gsub("\\.", "", archive_deduped$diagnosiscode1)

# v2 match
code_clean_v2 <- gsub("\\.", "", v2$code)

# ed match
dx_clean_ed <- gsub("\\.", "", ed$diagnosiscode1)

match_any_fixed <- function(x, codes) {
  codes <- codes[!is.na(codes) & codes != ""]
  if (length(codes) == 0) return(rep(FALSE, length(x)))
  
  out <- rep(FALSE, length(x))
  for (code in codes) {
    out <- out | grepl(code, x, fixed = TRUE)
  }
  out
}

archive_date <- as.Date(archive_deduped$date)
v2_date <- as.Date(v2$date)

match_date_archive <- !is.na(archive_date)
match_date_v2 <- !is.na(v2_date)

window_starts <- as.Date(TS_Results_today$Time.Window.Start)

# For cluster and baseline linelist if you want to determine which are incident vs non-incident, you will also need to use v2 (this is the study dataset where we only kept incident diagnoses)
# This has "date", "key", "dispo", "code" (in that order)
for(i in 1:length(valid_nodes))
{
  print(i)
  print(valid_nodes[i])
  
  node_codes <- valid_nodes[i]
  node_codes <- gsub("\\.", "", node_codes)
  
  ts_idx <- which(clean_node(TS_Results_today$Node.Identifier) == node_codes)[1]
  
  if (is.na(ts_idx)) {
    message("Skipping node not present in filtered TS_Results_today: ", node_codes)
    next
  }
  
  window_start <- as.Date(TS_Results_today$Time.Window.Start[ts_idx])
  
  match_dx_archive <- !is.na(dx_clean_archive) &
    match_any_fixed(dx_clean_archive, node_codes)
  
  match_dx_v2 <- !is.na(code_clean_v2) &
    match_any_fixed(code_clean_v2, node_codes)
  
  match_dx_ed <- !is.na(dx_clean_ed) &
    match_any_fixed(dx_clean_ed, node_codes)
  
  match_date_archive <- !is.na(archive_deduped$date)
  
  node_codes_clean <- gsub("\\.", "", node_codes)
  
  ed_keep <- ed[match_dx_ed, , drop = FALSE]
  
  if (nrow(ed_keep) == 0) next
  
  ed_keep$n <- 1
  
  
  temp <- archive_deduped[
    match_dx_archive & match_date_archive & archive_date >= window_start,
    keep_cols,
    drop = FALSE
  ]
  
  temp1 <- archive_deduped[
    match_dx_archive & match_date_archive & archive_date < window_start,
    keep_cols,
    drop = FALSE
  ]
  
  temp2 <- v2[
    match_dx_v2 & match_date_v2 & v2_date >= window_start,
    ,
    drop = FALSE
  ]
  
  temp21 <- v2[
    match_dx_v2 & match_date_v2 & v2_date < window_start,
    ,
    drop = FALSE
  ]
  
  # If baseline has records (i.e., not empty)
  if(nrow(temp1)>0)
  {
    # merge linelist for cluster between original and study dataset to identify incident vs non-incident
    if (nrow(temp2) > 0) {
      temp2$dxtype <- 0
      temp2$dxtype[temp2$dispo == "A"] <- 1
      
      temp$date <- as.Date(temp$date)
      temp2$date <- as.Date(temp2$date)
      
      temp2_small <- unique(temp2[, c("date", "key", "dxtype"), drop = FALSE])
      
      temp <- merge(temp, temp2_small, by = c("date", "key"), all.x = TRUE)
      temp$dxtype[is.na(temp$dxtype)] <- -1
    } else {
      temp$dxtype <- -1
    }
    
    # merge linelist for baseline between original and study dataset to identify incident vs non-incident
    if (nrow(temp21) > 0) {
      temp21$dxtype <- 0
      temp21$dxtype[temp21$dispo == "A"] <- 1
      
      temp1$date <- as.Date(temp1$date)
      temp21$date <- as.Date(temp21$date)
      
      temp21_small <- unique(temp21[, c("date", "key", "dxtype"), drop = FALSE])
      
      temp1 <- merge(temp1, temp21_small, by = c("date", "key"), all.x = TRUE)
      temp1$dxtype[is.na(temp1$dxtype)] <- -1
    } else {
      temp1$dxtype <- -1
    }
    
    # if 1- then only keep Adverse visits
    if (grepl("^1\\-", node_codes)) {
      temp    <- temp[temp$dispo == "A", , drop = FALSE]
      temp1   <- temp1[temp1$dispo == "A", , drop = FALSE]
      ed_keep <- ed_keep[ed_keep$dispo == "A", , drop = FALSE]
    }
    
    # order by date and time
    temp$time  <- format(strptime(temp$time,  "%H:%M:%S"), "%H:%M:%S")
    temp  <- temp[order(temp$date, temp$time), , drop = FALSE]
    
    # order by date and time
    temp1$time <- format(strptime(temp1$time, "%H:%M:%S"), "%H:%M:%S")
    temp1 <- temp1[order(temp1$date, temp1$time), , drop = FALSE]
    
    # For multi-year trend lines
    if (nrow(ed_keep) == 0) next
    
    ed_keep <- cbind(MMWRweek(ed_keep$date)[, -3, drop = FALSE], ed_keep)
    ed_keep <- aggregate(n ~ MMWRyear + MMWRweek, data = ed_keep, sum)
    ed_keep <- merge(mmwrwks, ed_keep, all.x = TRUE)
    ed_keep$n[is.na(ed_keep$n)] <- 0
    ed_keep <- ed_keep[ed_keep$MMWRyear %in% yr_list, , drop = FALSE]
    ed_keep <- ed_keep[order(ed_keep$MMWRyear, ed_keep$MMWRweek), , drop = FALSE]
    rownames(ed_keep) <- seq_len(nrow(ed_keep))
    
    if (nrow(ed_keep) < 2) next
    
    # check if it is an outlier trend for max signals
    if (weekdays(END_DATE) == "Saturday") {
      complete_week <- ed_keep$MMWRweek[nrow(ed_keep)]
    } else {
      complete_week <- ed_keep$MMWRweek[nrow(ed_keep) - 1]
    }
    
    if (nrow(ed_keep) >= 3) {
      idx_exclude <- c(nrow(ed_keep) - 1, nrow(ed_keep))
      base_idx <- which(ed_keep$MMWRweek[-idx_exclude] == complete_week)
      hlm_weeks <- unique(c(base_idx - 1, base_idx, base_idx + 1))
      hlm_weeks <- hlm_weeks[hlm_weeks >= 1 & hlm_weeks <= nrow(ed_keep)]
      hlm_data <- ed_keep[hlm_weeks, , drop = FALSE]
    } else {
      hlm_data <- ed_keep
    }
    
    hlm_data <- hlm_data[order(hlm_data$MMWRyear, hlm_data$MMWRweek), , drop = FALSE]
    
    current_vals <- ed_keep$n[
      ed_keep$MMWRyear == max(ed_keep$MMWRyear, na.rm = TRUE) &
        ed_keep$MMWRweek == complete_week
    ]
    
    if (length(current_vals) > 0 && nrow(hlm_data) > 0) {
      threshold <- mean(hlm_data$n, na.rm = TRUE) + 2 * sd(hlm_data$n, na.rm = TRUE)
      if (max(current_vals, na.rm = TRUE) > threshold) {
        sigs_maxout <- c(sigs_maxout, as.character(node_codes))
      }
    }
    
    code_trend <- tempfile(fileext = ".png")
    png(code_trend, width = 1000, height = 600, res = 150)
    par(mar = c(6, 4, 1, 2))  # Adjust bottom margin for label space
    
    plot(
      1:52, rep(0, 52),
      type = "n",
      xlim = c(1, 52),
      ylim = c(0, ceiling(max(ed_keep$n, na.rm = TRUE) * 1.1)),
      xlab = "Month",
      ylab = gsub("2\\-", "", paste(node_codes)),
      xaxt = "n"
    )
    axis(
      1,
      at = c(1, 5, 9, 14, 18, 22, 27, 31, 35, 40, 44, 48),
      labels = month.abb,
      cex.axis = 0.7,
      las = 2
    )
    
    # Trends where last point we note if partial or full week
    years_plot <- sort(unique(ed_keep$MMWRyear))
    cols_plot <- c("Blue", "Green", "Purple", "Red")[seq_along(years_plot)]
    lwds_plot <- c(1, 1, 1, 3)[seq_along(years_plot)]
    
    if (weekdays(END_DATE) == "Saturday") {
      for (k in seq_along(years_plot)) {
        yr <- years_plot[k]
        idx <- which(ed_keep$MMWRyear == yr)
        lines(ed_keep$MMWRweek[idx], ed_keep$n[idx], col = cols_plot[k], lwd = lwds_plot[k])
      }
      legend("topleft", lty = 1, col = cols_plot, legend = years_plot, bty = "n", ncol = length(years_plot))
    }
    
    if (weekdays(END_DATE) %in% c("Wednesday", "Thursday", "Friday")) {
      for (k in seq_along(years_plot)) {
        yr <- years_plot[k]
        idx <- which(ed_keep$MMWRyear == yr)
        lines(ed_keep$MMWRweek[idx], ed_keep$n[idx], col = cols_plot[k], lwd = lwds_plot[k])
      }
      
      # mark last point of most recent year as partial week
      latest_year <- max(years_plot, na.rm = TRUE)
      idx_latest <- which(ed_keep$MMWRyear == latest_year)
      
      if (length(idx_latest) > 0) {
        lastpt <- tail(idx_latest, 1)
        points(
          ed_keep$MMWRweek[lastpt],
          ed_keep$n[lastpt],
          pch = 21, bg = NA, col = "red", cex = 1, lwd = 1
        )
      }
      
      legend("topleft", lty = 1, col = cols_plot, legend = years_plot, bty = "n", ncol = length(years_plot))
      legend("topright", pch = 21, col = "red", legend = "partial week", cex = 0.5, bty = "n")
    }
    
    if (weekdays(END_DATE) %in% c("Sunday", "Monday", "Tuesday")) {
      ed_keep1 <- ed_keep[-nrow(ed_keep), , drop = FALSE]
      
      years_plot1 <- sort(unique(ed_keep1$MMWRyear))
      cols_plot1 <- c("Blue", "Green", "Purple", "Red")[seq_along(years_plot1)]
      lwds_plot1 <- c(1, 1, 1, 3)[seq_along(years_plot1)]
      
      for (k in seq_along(years_plot1)) {
        yr <- years_plot1[k]
        idx <- which(ed_keep1$MMWRyear == yr)
        lines(ed_keep1$MMWRweek[idx], ed_keep1$n[idx], col = cols_plot1[k], lwd = lwds_plot1[k])
      }
      
      legend("topleft", lty = 1, col = cols_plot1, legend = years_plot1, bty = "n", ncol = length(years_plot1))
    }
    
    dev.off()
    
    # Maps for cluster and baseline
    temp$zipcode <- substr(as.character(temp$zipcode), 1, 5)
    temp1$zipcode <- substr(as.character(temp1$zipcode), 1, 5)
    
    temp_z <- merge(temp, df[, c("zipcode", "MODZCTA20")], by = "zipcode", all.x = TRUE)
    temp_z <- temp_z[!is.na(temp_z$MODZCTA20) & temp_z$MODZCTA20 != "", , drop = FALSE]
    
    if (nrow(temp_z) > 0) {
      temp_z <- as.data.frame(table(temp_z$MODZCTA20), stringsAsFactors = FALSE)
      names(temp_z) <- c("modzcta", "Freq")
      temp_z$pct <- round(temp_z$Freq / sum(temp_z$Freq), 4)
    } else {
      temp_z <- data.frame(
        modzcta = character(),
        Freq = integer(),
        pct = numeric(),
        stringsAsFactors = FALSE
      )
    }
    
    temp1_z <- merge(temp1, df[, c("zipcode", "MODZCTA20")], by = "zipcode", all.x = TRUE)
    temp1_z <- temp1_z[!is.na(temp1_z$MODZCTA20) & temp1_z$MODZCTA20 != "", , drop = FALSE]
    
    if (nrow(temp1_z) > 0) {
      temp1_z <- as.data.frame(table(temp1_z$MODZCTA20), stringsAsFactors = FALSE)
      names(temp1_z) <- c("modzcta", "Freq")
      temp1_z$pct <- round(temp1_z$Freq / sum(temp1_z$Freq), 4)
    } else {
      temp1_z <- data.frame(
        modzcta = character(),
        Freq = integer(),
        pct = numeric(),
        stringsAsFactors = FALSE
      )
    }
    
    temp_z$modzcta <- as.character(temp_z$modzcta)
    temp1_z$modzcta <- as.character(temp1_z$modzcta)
    modzcta$modzcta <- as.character(modzcta$modzcta)
    
    # Use HospitalZip to define the map zoom area
    hospital_zip <- unique(na.omit(substr(as.character(temp$HospitalZip), 1, 5)))
    
    hospital_area <- modzcta %>%
      filter(modzcta %in% hospital_zip)
    
    if (nrow(hospital_area) > 0) {
      bb_hosp <- sf::st_bbox(hospital_area)
      
      # Add padding around the hospital ZIP so the plot is not too tightly cropped
      xpad <- as.numeric(bb_hosp["xmax"] - bb_hosp["xmin"]) * 2
      ypad <- as.numeric(bb_hosp["ymax"] - bb_hosp["ymin"]) * 2
      
      bb_hosp["xmin"] <- bb_hosp["xmin"] - xpad
      bb_hosp["xmax"] <- bb_hosp["xmax"] + xpad
      bb_hosp["ymin"] <- bb_hosp["ymin"] - ypad
      bb_hosp["ymax"] <- bb_hosp["ymax"] + ypad
    }
    
    modzcta1 <- left_join(modzcta, temp_z, by = "modzcta")
    modzcta_plot <- modzcta1[!is.na(modzcta1$pct), ]
    
    if (nrow(modzcta_plot) > 0) {
      bb <- sf::st_bbox(modzcta_plot)
      
      p <- ggplot(modzcta1) +
        geom_sf(aes(fill = pct), color = "white", size = 0.1) +
        scale_fill_viridis_c(
          option = "plasma",
          name = "Cluster",
          labels = label_number(accuracy = 0.001),
          na.value = "grey95"
        ) +
        coord_sf(
          xlim = if (exists("bb_hosp") && nrow(hospital_area) > 0) c(bb_hosp["xmin"], bb_hosp["xmax"]) else c(bb["xmin"], bb["xmax"]),
          ylim = if (exists("bb_hosp") && nrow(hospital_area) > 0) c(bb_hosp["ymin"], bb_hosp["ymax"]) else c(bb["ymin"], bb["ymax"]),
          expand = FALSE
        ) +
        theme_minimal() +
        labs(title = paste("MODZCTA Choropleth Map for", node_codes, "cluster"))
    } else {
      p <- ggplot(modzcta1) +
        geom_sf(aes(fill = pct), color = "white", size = 0.1) +
        scale_fill_viridis_c(
          option = "plasma",
          name = "Cluster",
          labels = label_number(accuracy = 0.001),
          na.value = "grey95"
        ) +
        theme_minimal() +
        labs(title = paste("MODZCTA Choropleth Map for", node_codes, "cluster"))
    }
    
    modzcta1 <- left_join(modzcta, temp1_z, by = "modzcta")
    modzcta_plot1 <- modzcta1[!is.na(modzcta1$pct), ]
    
    if (nrow(modzcta_plot1) > 0) {
      bb1 <- sf::st_bbox(modzcta_plot1)
      
      p1 <- ggplot(modzcta1) +
        geom_sf(aes(fill = pct), color = "white", size = 0.1) +
        scale_fill_viridis_c(
          option = "plasma",
          name = "Baseline",
          labels = label_number(accuracy = 0.001),
          na.value = "grey95"
        ) +
        coord_sf(
          xlim = c(bb1["xmin"], bb1["xmax"]),
          ylim = c(bb1["ymin"], bb1["ymax"]),
          expand = FALSE
        ) +
        theme_minimal() +
        labs(title = paste("MODZCTA Choropleth Map for", node_codes, "baseline"))
    } else {
      p1 <- ggplot(modzcta1) +
        geom_sf(aes(fill = pct), color = "white", size = 0.1) +
        scale_fill_viridis_c(
          option = "plasma",
          name = "Baseline",
          labels = label_number(accuracy = 0.001),
          na.value = "grey95"
        ) +
        theme_minimal() +
        labs(title = paste("MODZCTA Choropleth Map for", node_codes, "baseline"))
    }
    
    # Other co-diagnoses codes - creating frequency tables for cluster and baseline
    process <- function(x) {
      if (is.na(x) || x == "") return(character(0))
      tokens <- unlist(strsplit(gsub("\\|", "", x), ";"))
      tokens <- trimws(tokens)
      tokens <- tokens[tokens != ""]
      unique(substr(tokens, 1, 3))
    }
    
    temp3 <- temp1[temp1$dxtype != -1, "diagnosiscode1", drop = TRUE]
    if (length(temp3) == 0) temp3 <- temp1[, "diagnosiscode1", drop = TRUE]
    temp3 <- temp3[!is.na(temp3) & temp3 != ""]
    
    if (length(temp3) > 0) {
      result <- data.frame(
        dx = vapply(temp3, function(x) paste(process(x), collapse = " "), character(1)),
        stringsAsFactors = FALSE
      )
      
      dx_tokens <- unlist(strsplit(result$dx, " "))
      dx_tokens <- trimws(dx_tokens)
      dx_tokens <- dx_tokens[dx_tokens != ""]
      
      if (length(dx_tokens) > 0) {
        temp3b <- as.data.frame(table(dx_tokens), stringsAsFactors = FALSE)
        names(temp3b) <- c("dx", "Freq")
      } else {
        temp3b <- data.frame(dx = character(), Freq = integer(), stringsAsFactors = FALSE)
      }
    } else {
      temp3b <- data.frame(dx = character(), Freq = integer(), stringsAsFactors = FALSE)
    }
    
    temp3 <- temp1[temp1$dxtype != -1, "diagnosiscode1", drop = TRUE]
    if (length(temp3) == 0) temp3 <- temp1[, "diagnosiscode1", drop = TRUE]
    temp3 <- temp3[!is.na(temp3) & temp3 != ""]
    
    if (length(temp3) > 0) {
      result <- data.frame(
        dx = vapply(temp3, function(x) paste(process(x), collapse = " "), character(1)),
        stringsAsFactors = FALSE
      )
      
      dx_tokens <- unlist(strsplit(result$dx, " "))
      dx_tokens <- trimws(dx_tokens)
      dx_tokens <- dx_tokens[dx_tokens != ""]
      
      if (length(dx_tokens) > 0) {
        temp3b <- as.data.frame(table(dx_tokens), stringsAsFactors = FALSE)
        names(temp3b) <- c("dx", "Freq")
        temp3b$dx <- gsub("\\s+", "", temp3b$dx)
        
        temp3b <- merge(
          temp3b,
          icd10.2026_treefilew_FINAL[, c("Name1", "Desc")],
          by.x = "dx",
          by.y = "Name1",
          all.x = TRUE
        )
        
        temp3b <- temp3b[order(-temp3b$Freq), , drop = FALSE]
        names(temp3b)[1:2] <- c("Level4", "BaselineFreq")
        temp3b$BaselinePct <- round((temp3b$BaselineFreq * 100) / length(temp3), 1)
      } else {
        temp3b <- data.frame(
          Level4 = character(),
          BaselineFreq = integer(),
          Desc = character(),
          BaselinePct = numeric(),
          stringsAsFactors = FALSE
        )
      }
    } else {
      temp3b <- data.frame(
        Level4 = character(),
        BaselineFreq = integer(),
        Desc = character(),
        BaselinePct = numeric(),
        stringsAsFactors = FALSE
      )
    }
    
    temp3 <- temp[temp$dxtype != -1, "diagnosiscode1", drop = TRUE]
    if (length(temp3) == 0) temp3 <- temp[, "diagnosiscode1", drop = TRUE]
    temp3 <- temp3[!is.na(temp3) & temp3 != ""]
    
    if (length(temp3) > 0) {
      result <- data.frame(
        dx = vapply(temp3, function(x) paste(process(x), collapse = " "), character(1)),
        stringsAsFactors = FALSE
      )
      
      dx_tokens <- unlist(strsplit(result$dx, " "))
      dx_tokens <- trimws(dx_tokens)
      dx_tokens <- dx_tokens[dx_tokens != ""]
      
      if (length(dx_tokens) > 0) {
        temp3c <- as.data.frame(table(dx_tokens), stringsAsFactors = FALSE)
        names(temp3c) <- c("dx", "Freq")
        temp3c$dx <- gsub("\\s+", "", temp3c$dx)
        
        temp3c <- merge(
          temp3c,
          icd10.2026_treefilew_FINAL[, c("Name1", "Desc")],
          by.x = "dx",
          by.y = "Name1",
          all.x = TRUE
        )
        
        temp3c <- temp3c[order(-temp3c$Freq), , drop = FALSE]
        names(temp3c)[1:2] <- c("Level4", "ClusterFreq")
        temp3c$ClusterPct <- round((temp3c$ClusterFreq * 100) / length(temp3), 1)
      } else {
        temp3c <- data.frame(
          Level4 = character(),
          ClusterFreq = integer(),
          Desc = character(),
          ClusterPct = numeric(),
          stringsAsFactors = FALSE
        )
      }
    } else {
      temp3c <- data.frame(
        Level4 = character(),
        ClusterFreq = integer(),
        Desc = character(),
        ClusterPct = numeric(),
        stringsAsFactors = FALSE
      )
    }
    
  #   vals_cluster <- temp$chiefcomplaint[temp$dxtype != -1]
  #   if (length(vals_cluster) == 0) vals_cluster <- temp$chiefcomplaint
  #   vals_cluster <- vals_cluster[!is.na(vals_cluster) & trimws(vals_cluster) != ""]
    
  #   if (length(vals_cluster) == 0) {
    #   temp4 <- data.frame(
    #     CC_word = character(),
    #     ClusterFreq = integer(),
    #     ClusterPct = numeric(),
    #     stringsAsFactors = FALSE
    #   )
  #   } else {
    #   words_cluster <- toupper(unlist(lapply(
    #     strsplit(gsub("[[:punct:]]", " ", vals_cluster), " "),
    #     unique
    #   )))
      
    #   words_cluster <- trimws(words_cluster)
    #   words_cluster <- words_cluster[!is.na(words_cluster) & words_cluster != ""]
    #   words_cluster <- words_cluster[!words_cluster %in% c(
    #     "", "I10", "THE", "A", "AN", "OF", "AND", "TO", "IN", "FOR", "WITH",
    #     "ON", "IS", "WAS", "ARE", "BY", "AT", "FROM", "MY", "HIS", "HER",
    #     "HE", "SHE", "AS", "PER", "I", "HAS", "HAVE", "PT", "PATIENT", "STATES"
    #   )]
    #   words_cluster <- words_cluster[nchar(words_cluster) >= 2]
      
    #   if (length(words_cluster) == 0) {
    #     temp4 <- data.frame(
    #       CC_word = character(),
    #       ClusterFreq = integer(),
    #       ClusterPct = numeric(),
    #       stringsAsFactors = FALSE
    #     )
    #   } else {
    #     temp4 <- as.data.frame(table(words_cluster), stringsAsFactors = FALSE)
    #     names(temp4) <- c("CC_word", "ClusterFreq")
    #     temp4$ClusterPct <- round((temp4$ClusterFreq * 100) / length(vals_cluster), 1)
    #   }
    # }
    
    # Common stopword list for chief complaint words
    stop_words <- c(
      "", "I10", "THE", "A", "AN", "OF", "AND", "TO", "IN", "FOR", "WITH",
      "ON", "IS", "WAS", "ARE", "BY", "AT", "FROM", "MY", "HIS", "HER",
      "HE", "SHE", "AS", "PER", "I", "HAS", "HAVE", "PT", "PATIENT", "STATES"
    )
    
    # Helper: build chief complaint word table safely
    build_cc_table <- function(vals, freq_name, pct_name) {
      vals <- vals[!is.na(vals) & trimws(vals) != ""]
      
      if (length(vals) == 0) {
        out <- data.frame(
          CC_word = character(),
          stringsAsFactors = FALSE
        )
        out[[freq_name]] <- integer()
        out[[pct_name]] <- numeric()
        return(out)
      }
      
      words <- toupper(unlist(lapply(
        strsplit(gsub("[[:punct:]]", " ", vals), " "),
        unique
      )))
      
      words <- trimws(words)
      words <- words[!is.na(words) & words != ""]
      words <- words[!words %in% stop_words]
      words <- words[nchar(words) >= 2]
      
      if (length(words) == 0) {
        out <- data.frame(
          CC_word = character(),
          stringsAsFactors = FALSE
        )
        out[[freq_name]] <- integer()
        out[[pct_name]] <- numeric()
        return(out)
      }
      
      out <- as.data.frame(table(words), stringsAsFactors = FALSE)
      names(out) <- c("CC_word", freq_name)
      out[[pct_name]] <- round((out[[freq_name]] * 100) / length(vals), 1)
      out
    }
    
    build_cc_table <- function(vals, freq_name, pct_name) {
      vals <- vals[!is.na(vals)]
      vals <- normalize_for_tokens(vals)
      vals <- vals[trimws(vals) != ""]
      
      if (length(vals) == 0) {
        out <- data.frame(CC_word = character(), stringsAsFactors = FALSE)
        out[[freq_name]] <- integer()
        out[[pct_name]] <- numeric()
        return(out)
      }
      
      words <- toupper(unlist(lapply(
        strsplit(vals, " ", fixed = TRUE),
        unique
      )))
      
      words <- trimws(words)
      words <- words[!is.na(words) & words != ""]
      words <- words[!words %in% stop_words]
      words <- words[nchar(words) >= 2]
      
      if (length(words) == 0) {
        out <- data.frame(CC_word = character(), stringsAsFactors = FALSE)
        out[[freq_name]] <- integer()
        out[[pct_name]] <- numeric()
        return(out)
      }
      
      out <- as.data.frame(table(words), stringsAsFactors = FALSE)
      names(out) <- c("CC_word", freq_name)
      out[[pct_name]] <- round((out[[freq_name]] * 100) / length(vals), 1)
      out
    }
    
    # Cluster chief complaint words
    vals_cluster <- temp$chiefcomplaint[temp$dxtype != -1]
    if (length(vals_cluster) == 0) vals_cluster <- temp$chiefcomplaint
    temp4 <- build_cc_table(vals_cluster, "ClusterFreq", "ClusterPct")
    
    # Baseline chief complaint words
    vals_baseline <- temp1$chiefcomplaint[temp1$dxtype != -1]
    if (length(vals_baseline) == 0) vals_baseline <- temp1$chiefcomplaint
    temp4b <- build_cc_table(vals_baseline, "BaselineFreq", "BaselinePct")
    
    # Combined CC table always exists
    temp_words <- merge(temp4, temp4b, by = "CC_word", all = TRUE)
    if (nrow(temp_words) == 0) {
      temp_words <- data.frame(
        CC_word = character(),
        ClusterFreq = integer(),
        ClusterPct = numeric(),
        BaselineFreq = integer(),
        BaselinePct = numeric(),
        stringsAsFactors = FALSE
      )
    } else {
      if (!"ClusterFreq" %in% names(temp_words)) temp_words$ClusterFreq <- NA_integer_
      if (!"ClusterPct" %in% names(temp_words)) temp_words$ClusterPct <- NA_real_
      if (!"BaselineFreq" %in% names(temp_words)) temp_words$BaselineFreq <- NA_integer_
      if (!"BaselinePct" %in% names(temp_words)) temp_words$BaselinePct <- NA_real_
      
      temp_words <- temp_words[order(-temp_words$ClusterFreq, -temp_words$BaselineFreq), , drop = FALSE]
    }
    
    # Demographic tables for comparing Cluster and Baseline
    temp_d <- temp[temp$dxtype != -1, c("sex", "age_group", "race", "ethnicity", "zipcode"), drop = FALSE]
    if (nrow(temp_d) == 0) {
      temp_d <- temp[, c("sex", "age_group", "race", "ethnicity", "zipcode"), drop = FALSE]
    }
    if (nrow(temp_d) > 0) {
      temp_d$group <- "Cluster"
    } else {
      temp_d <- data.frame(
        sex = character(),
        age_group = character(),
        race = character(),
        ethnicity = character(),
        zipcode = character(),
        group = character(),
        stringsAsFactors = FALSE
      )
    }
    
    temp1_d <- temp1[temp1$dxtype != -1, c("sex", "age_group", "race", "ethnicity", "zipcode"), drop = FALSE]
    if (nrow(temp1_d) == 0) {
      temp1_d <- temp1[, c("sex", "age_group", "race", "ethnicity", "zipcode"), drop = FALSE]
    }
    if (nrow(temp1_d) > 0) {
      temp1_d$group <- "Baseline"
    } else {
      temp1_d <- data.frame(
        sex = character(),
        age_group = character(),
        race = character(),
        ethnicity = character(),
        zipcode = character(),
        group = character(),
        stringsAsFactors = FALSE
      )
    }
    
    combined <- rbind(temp_d, temp1_d)
    
    if (nrow(combined) == 0) {
      combined <- data.frame(
        sex = character(),
        age_group = character(),
        race = character(),
        ethnicity = character(),
        zipcode = character(),
        group = character(),
        stringsAsFactors = FALSE
      )
    }
    
    age_levels <- c("<1","1-4", "5-12", "13-17", "18-49", "50-64", "65-79", "80+")
    combined$age_group <- factor(combined$age_group, levels = age_levels)
    combined$sex <- toupper(as.character(combined$sex))
    
    combined$zipcode <- as.character(combined$zipcode)
    combined$zipcode <- trimws(combined$zipcode)
    combined$zipcode <- substr(combined$zipcode, 1, 5)
    
    combined$zip3 <- substr(combined$zipcode, 1, 3)
    combined$zip3[is.na(combined$zip3) | combined$zip3 == ""] <- "Unknown"
    
    combined$zip3 <- factor(combined$zip3)
    
    if (nrow(combined) > 0 && nlevels(combined$zip3) > 0) {
      ref_zip3 <- levels(combined$zip3)[1]
      combined$zip3 <- relevel(combined$zip3, ref = ref_zip3)
    }
    
    # List of demographic variables
    demographics <- c("sex", "age_group", "ethnicity", "race", "zip3_collapsed")
    
    # Set sparse ZIPs to 'Other'
    zip_counts <- table(combined$zip3)
    keep_zip3 <- names(zip_counts[zip_counts >= 10])
    
    combined$zip3_collapsed <- as.character(combined$zip3)
    combined$zip3_collapsed[is.na(combined$zip3_collapsed) | combined$zip3_collapsed == ""] <- "Unknown"
    combined$zip3_collapsed[!combined$zip3_collapsed %in% keep_zip3 & combined$zip3_collapsed != "Unknown"] <- "Other"
    combined$zip3_collapsed <- factor(combined$zip3_collapsed)
    
    # Set sparse ZIPs to 'Other'
    zip_counts <- table(combined$zip3)
    keep_zip3 <- names(zip_counts[zip_counts >= 10])
    
    combined$zip3_collapsed <- as.character(combined$zip3)
    combined$zip3_collapsed[is.na(combined$zip3_collapsed) | combined$zip3_collapsed == ""] <- "Unknown"
    combined$zip3_collapsed[!combined$zip3_collapsed %in% keep_zip3 & combined$zip3_collapsed != "Unknown"] <- "Other"
    combined$zip3_collapsed <- factor(combined$zip3_collapsed)
    
    # Use collapsed ZIPs in demographics
    demographics <- c("sex", "age_group", "ethnicity", "race", "zip3_collapsed")
    
    # Safe test function
    safe_test_p <- function(tbl) {
      tbl <- as.matrix(tbl)
      
      # remove zero-sum rows/columns
      tbl <- tbl[rowSums(tbl) > 0, colSums(tbl) > 0, drop = FALSE]
      
      # not enough data to test
      if (nrow(tbl) < 2 || ncol(tbl) < 2) {
        return(list(p.value = NA_real_, method = "Not enough data"))
      }
      
      # use Fisher for 2x2, otherwise chi-square / simulated chi-square
      if (all(dim(tbl) == c(2, 2))) {
        res <- fisher.test(tbl)
        return(list(p.value = res$p.value, method = res$method))
      }
      
      res <- suppressWarnings(chisq.test(tbl))
      
      if (any(is.na(res$expected)) || any(res$expected < 5)) {
        res <- suppressWarnings(chisq.test(tbl, simulate.p.value = TRUE, B = 10000))
        return(list(p.value = res$p.value, method = "Simulated Chi-squared Test"))
      } else {
        return(list(p.value = res$p.value, method = res$method))
      }
    }
    
    # Initialize results list
    results <- list()
    
    for (var in demographics) {
      if (!var %in% names(combined)) next
      if (!"group" %in% names(combined)) next
      
      x <- combined[[var]]
      g <- combined$group
      
      keep <- !is.na(x) & !is.na(g) &
        trimws(as.character(x)) != "" &
        trimws(as.character(g)) != ""
      
      if (sum(keep) == 0) next
      
      tab <- table(x[keep], g[keep])
      
      if (nrow(tab) == 0 || ncol(tab) == 0) next
      
      prop <- round(prop.table(tab, margin = 2) * 100, 2)
      combined_prop <- as.data.frame.matrix(prop)
      
      if (nrow(combined_prop) == 0) next
      
      combined_prop$Variable <- rownames(combined_prop)
      rownames(combined_prop) <- NULL
      
      test_res <- safe_test_p(tab)
      p_val <- test_res$p.value
      method <- test_res$method
      
      if (!"Baseline" %in% names(combined_prop)) combined_prop$Baseline <- NA_real_
      if (!"Cluster" %in% names(combined_prop)) combined_prop$Cluster <- NA_real_
      
      combined_prop$p_value <- ""
      combined_prop$p_value[1] <- ifelse(is.na(p_val), "", round(p_val, 3))
      
      combined_prop$method <- ""
      combined_prop$method[1] <- method
      
      combined_prop$Demographic <- var
      
      combined_prop <- combined_prop[, c("Demographic", "Variable", "Baseline", "Cluster", "p_value", "method")]
      results[[var]] <- combined_prop
    }
    
    if (length(results) > 0) {
      demographic_results <- do.call(rbind, results)
      rownames(demographic_results) <- NULL
      demog_table <- demographic_results
    } else {
      demog_table <- data.frame(
        Demographic = character(),
        Variable = character(),
        Baseline = numeric(),
        Cluster = numeric(),
        p_value = character(),
        method = character(),
        stringsAsFactors = FALSE
      )
    }
    
    demog_table <- demog_table[!demog_table$Variable %in% c("M", "NOT HISPANIC OR LATINO"), , drop = FALSE]
    rownames(demog_table) <- NULL
    
    if (nrow(demog_table) > 0) {
      demog_table$Baseline <- as.numeric(demog_table$Baseline)
      demog_table$Cluster <- as.numeric(demog_table$Cluster)
      demog_table$Diff <- round(demog_table$Cluster - demog_table$Baseline, 2)
      
      demog_table <- demog_table[, c("Demographic","Variable","Cluster","Baseline","Diff","p_value","method"), drop = FALSE]
      names(demog_table) <- c(
        "Demographic","Variable","ClusterPercent","BaselinePercent",
        "ClusterMinusBaselinePercent","ChiSq_Fis_Pval","Method"
      )
    }
    
    # Hospital Graph
    cluster_idx <- which(temp$dxtype != -1)
    if (length(cluster_idx) == 0) cluster_idx <- seq_len(nrow(temp))
    
    baseline_idx <- which(temp1$dxtype != -1)
    if (length(baseline_idx) == 0) baseline_idx <- seq_len(nrow(temp1))
    
    if (length(cluster_idx) > 0) {
      hosp_c <- data.frame(
        table(temp$hospital[cluster_idx]) / length(cluster_idx),
        stringsAsFactors = FALSE
      )
      names(hosp_c) <- c("hospital", "ClusterPct")
    } else {
      hosp_c <- data.frame(hospital = character(), ClusterPct = numeric(), stringsAsFactors = FALSE)
    }
    
    if (length(baseline_idx) > 0) {
      hosp_b <- data.frame(
        table(temp1$hospital[baseline_idx]) / length(baseline_idx),
        stringsAsFactors = FALSE
      )
      names(hosp_b) <- c("hospital", "BaselinePct")
    } else {
      hosp_b <- data.frame(hospital = character(), BaselinePct = numeric(), stringsAsFactors = FALSE)
    }
    
    hosp_cb <- merge(hosp_c, hosp_b, by = "hospital", all = TRUE)
    if (nrow(hosp_cb) == 0) {
      hosp_cb <- data.frame(
        hospital = character(),
        ClusterPct = numeric(),
        BaselinePct = numeric(),
        diff = numeric(),
        stringsAsFactors = FALSE
      )
    } else {
      hosp_cb$ClusterPct[is.na(hosp_cb$ClusterPct)] <- 0
      hosp_cb$BaselinePct[is.na(hosp_cb$BaselinePct)] <- 0
      hosp_cb$diff <- hosp_cb$ClusterPct - hosp_cb$BaselinePct
      hosp_cb <- hosp_cb[order(-hosp_cb$diff), , drop = FALSE]
      hosp_cb$hospital <- gsub("HOSPITAL", "HOSP", hosp_cb$hospital)
      hosp_cb$hospital <- gsub("MEDICAL", "MED", hosp_cb$hospital)
      hosp_cb$hospital <- gsub("CENTER", "CTR", hosp_cb$hospital)
    }
    
    hospital_barplot <- tempfile(fileext = ".png")
    png(hospital_barplot, width = 1000, height = 600, res = 150)
    par(mar = c(10, 4, 4, 2))
    
    if (nrow(hosp_cb) > 0) {
      barplot(
        hosp_cb$diff * 100,
        ylab = "%",
        names.arg = hosp_cb$hospital,
        las = 2,
        cex.names = 0.35,
        main = "Difference in percent of ED visits by hospital between cluster and baseline periods",
        cex.main = 0.8
      )
    } else {
      plot.new()
      title("Difference in percent of ED visits by hospital between cluster and baseline periods")
      text(0.5, 0.5, "No hospital data available")
    }
    dev.off()
    
    # Create excel with tab sheets
    wb <- createWorkbook()
    
    # Add data frames to sheets
    temp$NYCres <- ifelse(substr(as.character(temp$zipcode), 1, 5) %in% df$zipcode, "Y", "N")
    temp1$NYCres <- ifelse(substr(as.character(temp1$zipcode), 1, 5) %in% df$zipcode, "Y", "N")
    
    addWorksheet(wb, "ClusterLinelist")
    if (nrow(temp) > 0) {
      writeData(wb, "ClusterLinelist", temp[, c(1:6, ncol(temp), 7:16, 18:(ncol(temp)-1)), drop = FALSE])
    } else {
      writeData(wb, "ClusterLinelist", data.frame(Message = "No cluster linelist rows"))
    }
    
    # addWorksheet(wb, "BaselineLinelist")
    # if (nrow(temp1) > 0) {
    #   writeData(wb, "BaselineLinelist", temp1[, c(1:6, ncol(temp1), 7:16, 18:(ncol(temp1)-1)), drop = FALSE])
    # } else {
    #   writeData(wb, "BaselineLinelist", data.frame(Message = "No baseline linelist rows"))
    # }
    
    addWorksheet(wb, "Other_Codes")
    if (nrow(temp3c) > 0) {
      writeData(wb, "Other_Codes", temp3c[, c("Level4", "Desc", "ClusterFreq", "ClusterPct"), drop = FALSE], startRow = 1, startCol = 1)
    }
    if (nrow(temp3b) > 0) {
      writeData(wb, "Other_Codes", temp3b[, c("Level4", "Desc", "BaselineFreq", "BaselinePct"), drop = FALSE], startRow = 1, startCol = 6)
    }
    if (nrow(temp3b) == 0 && nrow(temp3c) == 0) {
      writeData(wb, "Other_Codes", data.frame(Message = "No co-diagnosis data available"))
    }
    
    addWorksheet(wb, "TopCC_words")
    if (nrow(temp_words) > 0) {
      writeData(wb, "TopCC_words", temp_words)
    } else {
      writeData(wb, "TopCC_words", data.frame(Message = "No chief complaint word data available"))
    }
    
    highlight_rows <- which(is.na(temp_words$BaselineFreq))
    highlight_rows <- highlight_rows[!is.na(highlight_rows)]
    highlight_rows <- head(highlight_rows, 30)
    
    yellow_style <- createStyle(fgFill = "yellow")
    
    for (j in highlight_rows) {
      addStyle(
        wb,
        sheet = "TopCC_words",
        style = yellow_style,
        rows = j + 1,
        cols = 1:2,
        gridExpand = TRUE,
        stack = TRUE
      )
    }
    
    # Weekly trends
    addWorksheet(wb, "Trends")
    insertImage(wb, sheet = "Trends", file = code_trend, startRow = 1, startCol = 1, width = 10, height = 6)
    
    # Maps
    addWorksheet(wb, "Maps")
    
    map_file <- tempfile(fileext = ".png")
    map1_file <- tempfile(fileext = ".png")
    ggsave(filename = map_file, plot = p, width = 6, height = 5, dpi = 300)
    ggsave(filename = map1_file, plot = p1, width = 6, height = 5, dpi = 300)
    
    insertImage(
      wb, sheet = "Maps", file = map_file,
      startRow = 1, startCol = 1, width = 6, height = 5, units = "in"
    )
    
    insertImage(
      wb, sheet = "Maps", file = map1_file,
      startRow = 1, startCol = 8, width = 6, height = 5, units = "in"
    )
    
    # Hospitals
    addWorksheet(wb, "Hospitals")
    insertImage(wb, sheet = "Hospitals", file = hospital_barplot, startRow = 1, startCol = 1, width = 10, height = 6)
    
    # Demographics
    addWorksheet(wb, "Demographics")
    if (nrow(demog_table) > 0) {
      writeData(wb, "Demographics", demog_table)
      
      highlight_rows <- which(demog_table$ChiSq_Fis_Pval != "" & as.numeric(demog_table$ChiSq_Fis_Pval) < 0.05)
      
      for (j in highlight_rows) {
        addStyle(
          wb,
          sheet = "Demographics",
          style = yellow_style,
          rows = j + 1,
          cols = 6,
          gridExpand = TRUE,
          stack = TRUE
        )
      }
      
      writeData(
        wb, "Demographics",
        "Missing and unknown race and ethnicity categories are excluded from chi-squared tests",
        startRow = nrow(demog_table) + 3, startCol = 1
      )
    } else {
      writeData(wb, "Demographics", data.frame(Message = "No demographic comparison data available"))
    }
    
    # Save workbook
    saveWorkbook(
      wb,
      paste0(
        parent_dir, "/signal_interpretation/", END_DATE, "_",
        gsub("\\|", "_", gsub("2\\-", "", node_codes)),
        ".xlsx"
      ),
      overwrite = TRUE
    )
  }  
  
  # If baseline is empty
  if(nrow(temp1)==0)
  {
    next
    # Merging original and study dataset to identify incident vs. non-incident
    if(nrow(temp2)>0)
    {
      temp2$dxtype=0
      temp2$dxtype[which(temp2$dispo=="A")]=1
      temp2=temp2[,-3]
      temp$date=as.Date(temp$date)
      temp2$date=as.Date(temp2$date)
      temp=merge(temp,temp2,by=c("date","key"),all.x=TRUE)
      temp=temp[,c(ncol(temp),1:(ncol(temp)-1))]
      temp$dxtype[is.na(temp$dxtype)==T]=-1
    }
    
    if(nrow(temp21)>0)
    {
      temp21$dxtype=0
      temp21$dxtype[which(temp21$dispo=="A")]=1
      temp21=temp21[,-3]
      temp1$date=as.Date(temp1$date)
      temp21$date=as.Date(temp21$date)
      temp1=merge(temp1,temp21,by=c("date","key"),all.x=TRUE)
      temp1=temp1[,c(ncol(temp1),1:(ncol(temp1)-1))]
      temp1$dxtype[is.na(temp1$dxtype)==T]=-1
    }
    
    # if 1- then only keep Adverse visits
    if(grepl("1\\-",node_codes)==T)
    {temp=temp[which(temp$dispo=="A"),]
    temp1=temp1[which(temp1$dispo=="A"),]
    ed_keep=ed_keep[which(ed_keep$dispo=="A"),]}
    
    # order by date and time
    temp$time=format(strptime(temp$time, "%H:%M"), "%H:%M")
    temp=temp[order(temp$date,temp$time),]
    
    # order by date and time
    temp1$time=format(strptime(temp1$time, "%H:%M"), "%H:%M")
    temp1=temp1[order(temp1$date,temp1$time),]
    
    #For multi-year trend lines
    ed_keep=cbind(MMWRweek(ed_keep$date)[,-3],ed_keep)
    ed_keep=aggregate(n~MMWRyear+MMWRweek,data=ed_keep,sum)
    ed_keep=merge(mmwrwks,ed_keep,all.x=TRUE)
    ed_keep$n[is.na(ed_keep$n)==T]=0
    ed_keep=ed_keep[ed_keep$MMWRyear %in% yr_list,]
    ed_keep=ed_keep[order(ed_keep$MMWRyear,ed_keep$MMWRweek),]
    rownames(ed_keep) <- seq_len(nrow(ed_keep))
    
    # Multi-year Weekly count graph
    # check is it an outlier trend for max signals
    complete_week=ifelse(weekdays(END_DATE)=="Saturday",ed_keep$MMWRweek[nrow(ed_keep)],ed_keep$MMWRweek[nrow(ed_keep)-1])
    
    hlm_weeks=which(ed_keep$MMWRweek[-c((nrow(ed_keep)-1):nrow(ed_keep))]==complete_week)
    hlm_weeks=c(hlm_weeks-1,hlm_weeks,hlm_weeks+1)
    hlm_weeks=hlm_weeks[hlm_weeks>=1]
    
    hlm_data=ed_keep[hlm_weeks,]
    hlm_data=hlm_data[order(hlm_data$MMWRyear,hlm_data$MMWRweek),]
    
    if(ed_keep$n[ed_keep$MMWRyear==max(ed_keep$MMWRyear) & ed_keep$MMWRweek==complete_week]>mean(hlm_data$n)+2*sd(hlm_data$n))
    {sigs_maxout <- c(sigs_maxout, as.character(node_codes))}
    
    code_trend <- tempfile(fileext = ".png")
    png(code_trend, width = 1000, height = 600, res = 150)
    par(mar = c(6, 4, 1, 2))  # Adjust bottom margin for label space
    
    plot(1:52,rep(0,52),type="n",xlim=c(1,52),ylim=c(0,ceiling(max(ed_keep$n)*1.1)),xlab="CDC_Week",ylab=gsub("2\\-","",paste(node_codes)),xaxt="n")
    axis(1,at=seq(1,52,2),labels=seq(1,52,2),cex.axis=0.6,las=2)
    
    # Trends where last point we note if partial or full week
    if(weekdays(END_DATE)=="Saturday")
    {for(k in 1:length(unique(ed_keep$MMWRyear)))
    {lines(ed_keep$MMWRweek[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])],ed_keep$n[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])],col=c("Blue","Green","Purple","Red")[k],lwd=c(1,1,1,3)[k])}
      legend("topleft",lty=1,col=c("Blue","Green","Purple","Red"),legend=unique(ed_keep$MMWRyear),bty="n",ncol=4)}
    
    if(weekdays(END_DATE)%in% c("Wednesday", "Thursday", "Friday"))
    {for(k in 1:length(unique(ed_keep$MMWRyear)))
    {lines(ed_keep$MMWRweek[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])],ed_keep$n[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])],col=c("Blue","Green","Purple","Red")[k],lwd=c(1,1,1,3)[k])}
      if(k==length(unique(ed_keep$MMWRyear))) 
      { lastpt=length(ed_keep$MMWRweek[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])])
      points(ed_keep$MMWRweek[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])][lastpt],ed_keep$n[which(ed_keep$MMWRyear==unique(ed_keep$MMWRyear)[k])][lastpt], pch = 21, bg = NA, col = "red", cex = 1, lwd = 1)
      legend("topleft",lty=1,col=c("Blue","Green","Purple","Red"),legend=unique(ed_keep$MMWRyear),bty="n",ncol=4)}
      lg=legend("topright",pch=21,col="Red",legend="partial week",cex=0.5,bty="n",plot=FALSE)
      legend(x = lg$rect$left, y = lg$rect$top*0.85,  # adjust 0.05 as needed
             pch = 21, col = "red", legend = "partial week", 
             cex = 0.5, bty = "n")}
    
    if(weekdays(END_DATE) %in% c("Sunday","Monday","Tuesday"))
    {ed_keep1=ed_keep[-length(ed_keep$MMWRyear),]
    for(k in 1:length(unique(ed_keep1$MMWRyear)))
    {lines(ed_keep1$MMWRweek[which(ed_keep1$MMWRyear==unique(ed_keep1$MMWRyear)[k])],ed_keep1$n[which(ed_keep1$MMWRyear==unique(ed_keep1$MMWRyear)[k])],col=c("Blue","Green","Purple","Red")[k],lwd=c(1,1,1,3)[k])}
    legend("topleft",lty=1,col=c("Blue","Green","Purple","Red"),legend=unique(ed_keep1$MMWRyear),bty="n",ncol=4)}
    dev.off()
    
    # Map for Cluster
    temp$zipcode=substr(temp$zipcode,1,5)
    temp_z=merge(temp,df[,c("zipcode","MODZCTA20")],by="zipcode",all.x=TRUE)
    temp_z=data.frame(table(temp_z$MODZCTA20))
    temp_z$pct=round(temp_z$Freq/sum(temp_z$Freq),4)
    names(temp_z)[1]="modzcta"
    
    modzcta1 <- left_join(modzcta, temp_z, by = "modzcta")
    modzcta_plot <- modzcta1[!is.na(modzcta1$pct), ]
    
    if (nrow(modzcta_plot) > 0) {
      bb <- sf::st_bbox(modzcta_plot)
      
      p <- ggplot(modzcta1) +
        geom_sf(aes(fill = pct), color = "white", size = 0.1) +
        scale_fill_viridis_c(
          option = "plasma",
          name = "Cluster",
          labels = label_number(accuracy = 0.001),
          na.value = "grey95"
        ) +
        coord_sf(
          xlim = c(bb["xmin"], bb["xmax"]),
          ylim = c(bb["ymin"], bb["ymax"]),
          expand = FALSE
        ) +
        theme_minimal() +
        labs(title = paste("MODZCTA Choropleth Map for", node_codes, "cluster"))
    } else {
      p <- ggplot(modzcta1) +
        geom_sf(aes(fill = pct), color = "white", size = 0.1) +
        scale_fill_viridis_c(
          option = "plasma",
          name = "Cluster",
          labels = label_number(accuracy = 0.001),
          na.value = "grey95"
        ) +
        theme_minimal() +
        labs(title = paste("MODZCTA Choropleth Map for", node_codes, "cluster"))
    }
    
    # Other co-diagnosis codes for cluster
    process <- function(x) {
      tokens <- unlist(strsplit(x, " "))
      tokens <- tokens[tokens != ""]  # remove empty strings
      unique_subs <- unique(substr(tokens, 1, 3))
      paste(unique_subs, collapse = " ")
    }
    
    temp3=gsub("\\|","",temp[which(temp$dxtype!=-1),"diagnosiscode1"])
    result <- data.frame(dx = sapply(temp3, process))
    temp3c=data.frame(table(dx=unlist(strsplit(result[,1], " "))))
    temp3c$dx=gsub("\\s+","",temp3c$dx)
    temp3c=merge(temp3c,icd10.2026_treefilew_FINAL[,c("Name1","Desc")],by.x="dx",by.y="Name1",all.x=T)
    temp3c=temp3c[order(-temp3c$Freq),]
    names(temp3c)[1:2]=c("Level4","ClusterFreq")
    temp3c$ClusterPct=round((temp3c$ClusterFreq*100)/length(temp3),1)
    
    # Top cc words for cluster
    temp4=as.data.frame(table(toupper(unlist(lapply(strsplit(gsub(x=temp$chiefcomplaint[which(temp$dxtype!=-1)],pattern="[[:punct:]]",replacement=" "),split=" "),unique)))))
    temp4=temp4[!temp4$Var1 %in% c("","THE","A","AN","OF","AND","TO","IN","FOR","WITH","ON","IS","WAS","ARE","BY","AT","FROM","MY","HIS","HER","HE","SHE","AS","PER","I","HAS","HAVE","PT","PATIENT","STATES"),]
    temp4=temp4[nchar(as.character(temp4$Var1))>=2,]
    names(temp4)=c("CC_word","ClusterFreq")
    temp4$ClusterPct=round((temp4$ClusterFreq*100)/length(temp$chiefcomplaint[which(temp$dxtype!=-1)]),1)
    
    temp_words=temp4
    temp_words=temp_words[order(-temp_words$ClusterFreq),]
    
    # Demographic tables
    temp_d=temp[which(temp$dxtype!=-1),c("sex","age_group", "race","ethnicity","zipcode")]
    temp_d$group <- "Cluster"
    combined=rbind(temp_d)
    age_levels <- c("<1","1-4", "5-12", "13-17", "18-49", "50-64", "65-79", "80+")
    combined$age_group<- factor(combined$age_group, levels = age_levels)
    combined$sex=toupper(combined$sex)
    combined$boro="Outside"
    combined$boro[grepl("\\b100|10128",combined$zipcode)]="Manhattan"
    combined$boro[grepl("\\b112",combined$zipcode)]="Brooklyn"
    combined$boro[grepl("\\b104",combined$zipcode)]="Bronx"
    combined$boro[grepl("\\b113|\\b114|\\b116|\\b111|\\b110",combined$zipcode)]="Queens"
    combined$boro[grepl("\\b103",combined$zipcode)]="StatenIs"
    ref_boro=ifelse(length(combined$boro[grepl("Manhattan",combined$boro)==T])>=1,"Manhattan",intersect(c("Brooklyn","Bronx","Queens","StatenIs"),unique(combined$boro))[1])
    combined$boro=relevel(factor(combined$boro),ref=ref_boro)
    combined1=combined[which(combined$race!="MISSUNK"),]
    combined1=combined1[which(combined1$ethnicity!="MISSUNK"),]
    # List of demographic variables
    demographics <- c("sex","age_group","ethnicity", "race","boro")
    
    # Initialize results list
    results <- list()
    
    for (var in demographics) {
      # Create contingency table
      tab <- table(combined1[[var]])
      
      # Calculate proportions by group
      prop <- round(prop.table(table(combined[[var]])) * 100,2)  # Column-wise percentages
      
      # Convert to data frame
      combined_prop <- as.data.frame(prop)
      combined_prop$Variable <- combined_prop$Var1
      rownames(combined_prop) <- NULL
      
      combined_prop$Demographic <- var
      combined_prop$Cluster <- combined_prop$Freq
      # Reorder columns
      combined_prop <- combined_prop[, c("Demographic","Variable", "Cluster")]
      
      # Append to results
      results[[var]] <- combined_prop
    }
    
    # Combine all results
    demog_table <- do.call(rbind, results)
    
    demog_table=demog_table[!demog_table$Variable %in% c("M","NOT HISPANIC OR LATINO"),]
    rownames(demog_table) <- NULL
    demog_table=demog_table[,c("Demographic","Variable","Cluster")]
    names(demog_table)=c("Demographic","Variable","ClusterPercent")
    
    # Hospital Graph for cluster percent
    hosp_c=data.frame(table(temp$hospital[which(temp$dxtype!=-1)])/nrow(temp[which(temp$dxtype!=-1),]))
    names(hosp_c)=c("hospital","ClusterPct")
    hosp_cb=hosp_c
    hosp_cb$ClusterPct[is.na(hosp_cb$ClusterPct)==T]=0
    hosp_cb=hosp_cb[order(-hosp_cb$ClusterPct),]
    hosp_cb$hospital=gsub("HOSPITAL","HOSP",hosp_cb$hospital)
    hosp_cb$hospital=gsub("MEDICAL","MED",hosp_cb$hospital)
    hosp_cb$hospital=gsub("CENTER","CTR",hosp_cb$hospital)
    hospital_barplot <- tempfile(fileext = ".png")
    
    # Increase margins to fit long labels
    png(hospital_barplot, width = 1000, height = 600, res = 150)
    par(mar = c(10, 4, 4, 2))  # Adjust bottom margin for label space
    
    barplot(hosp_cb$ClusterPct*100,ylab="%",names.arg=hosp_cb$hospital,las=2,cex.names=0.35,main="Percent of ED visits by hospital for cluster",cex.main=0.8)
    dev.off()
    
    install.packages("openxlsx")
    library("openxlsx")
    
    # Create excel with tab sheets
    wb <- createWorkbook()
    
    # Add data frames to sheets
    temp$NYCres=ifelse(substr(temp$zipcode,1,5) %in% df$zipcode,"Y","N")
    temp1$NYCres=ifelse(substr(temp1$zipcode,1,5) %in% df$zipcode,"Y","N")
    
    addWorksheet(wb, "ClusterLinelist")
    writeData(wb, "ClusterLinelist", temp[,c(1:6,ncol(temp),7:16,18:(ncol(temp)-1))])
    
    addWorksheet(wb, "BaselineLinelist")
    writeData(wb, "BaselineLinelist", temp1[,c(1:6,ncol(temp1),7:16,18:(ncol(temp1)-1))])
    
    addWorksheet(wb, "Other_Codes")
    writeData(wb, "Other_Codes", temp3c[,c("Level4","Desc","ClusterFreq", "ClusterPct")], startRow = 1, startCol = 1)
    
    addWorksheet(wb, "TopCC_words")
    writeData(wb, "TopCC_words", temp_words)
    
    # Weekly trends
    addWorksheet(wb, "Trends")
    insertImage(wb, sheet = "Trends", file = code_trend, startRow = 1, startCol = 1, width = 10, height = 6)
    
    addWorksheet(wb, "Maps")
    # Insert image into worksheet
    insertImage(wb, sheet = "Maps", file = ggsave("map.jpg", plot = p, width = 6, height = 5, dpi = 300), 
                startRow = 1, startCol = 1, width = 6, height = 5, units = "in")
    
    # Hospitals
    addWorksheet(wb, "Hospitals")
    insertImage(wb, sheet = "Hospitals", file = hospital_barplot, startRow = 1, startCol = 1, width = 10, height = 6)
    
    # demog table
    addWorksheet(wb, "Demographics")
    writeData(wb, "Demographics", demog_table)
    
    writeData(wb, "Demographics", "Missing and unknown race and ethnicity categories are excluded from chi-squared tests", startRow = nrow(demog_table) + 3, startCol = 1)
    
    # Save workbook; provide folder path
    saveWorkbook(wb, paste0(Folderpath,"/Linelist_",gsub("\\|", "_",gsub("2\\-","",node_codes)),".xlsx"), overwrite = TRUE)
  }    
  
}

