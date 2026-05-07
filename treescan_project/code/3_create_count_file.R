# Which lags do we want to get a count file for?
# This defaults to do it for lags of 1:5 days

for (LAG in initial_lags){
  print(paste0("Creating count file for lag ", LAG))
  
  end <- final_date - LAG

  ### CREATE COUNT FILE FOR ASYNDROMIC TREESCAN ANALYSIS
  ## Ramona Lall & Alison Levin-Rector
  ## October 2025
  
  # Start with synthetic dataset where each row is a unique key (person) and visit date,
  # and includes tab delimited diagnosis codes assigned on that visit and 
  # an indicator of severity: V=Visit, A=Admitted.
  # If using a 90-day study period, require 15 months of data so every day has a year lookback period
  # for defining incident diagnoses.
  
  library(lubridate)
  library(tidyr)
  library(dplyr)

  # set your working directory to point to "Preparing TreeScan Input Files" folder
  setwd(paste0(parent_dir, "/data"))
  
  year <- 2026
  
  # Define a function to select the most severe outcome
  outcome_severity <- function(outcome) {
    # "Admit" is more severe than "Visit"
    if ("A" %in% outcome) {
      return("A")
    }
    return("V")
  }
  
  # Pull in 15 months of ED visit data ending 8/20/2024
  sample_ED_data <- readRDS(
    file.path(
      parent_dir,
      "data",
      "datasets",
      paste0("dataset_", final_date, ".rds")
    )
  )
  
  library(data.table)
  library(stringr)
  
  setDT(sample_ED_data)
  
  # Convert date and clean diagnosis_codes
  sample_ED_data[, `:=`(
    date = as.Date(date),
    diagnosis_codes = str_squish(diagnosis_codes)
  )]
  
  # Split diagnosis_codes into long format faster than separate_longer_delim()
  split_codes <- strsplit(sample_ED_data$diagnosis_codes, " ", fixed = TRUE)
  n_codes <- lengths(split_codes)
  
  sample_ED_data_long <- sample_ED_data[
    rep.int(seq_len(.N), n_codes)
  ]
  
  sample_ED_data_long[, code := unlist(split_codes, use.names = FALSE)]
  sample_ED_data_long[, diagnosis_codes := NULL]
  
  # Clean and filter codes
  sample_ED_data_long[, code := gsub("\\s+", "", code)]
  sample_ED_data_long[, code := gsub(".", "", code, fixed = TRUE)]
  
  sample_ED_data_long <- sample_ED_data_long[
    !is.na(code) &
      code != "" &
      !grepl("^[0-9]", code) &
      grepl("\\d", code)
  ]
  
  # For repeat visits on same day, keep more severe outcome
  sample_ED_data_long[
    ,
    severity := outcome_severity(severity),
    by = .(date, key)
  ]
  
  # Remove duplicate rows
  sample_ED_data_long <- unique(sample_ED_data_long)
  
  ## Remove codes that are ineligible for this analysis
  
  ineligible_code_pattern <- paste(
    "U071|J09|J10|J11|\\bJ301\\b|\\bJ302\\b|\\bJ3089\\b|\\bJ309\\b|J45|T7840",
    "Z0|Z10|Z1152|Z12|Z13|Z14|Z15|Z17|Z18|Z19|Z21|Z28|Z30|Z3|Z4|Z50|Z51|Z52|Z53|Z55|Z56|Z62|Z63|Z64|Z66|Z67|Z68|Z76|Z78|Z8|Z90|Z91|Z92|Z93|Z94|Z95|Z96|Z97|Z98",
    "\\bC|\\bD0|\\bD1|\\bD2|\\bD3|\\bD4|\\bQ",
    sep = "|"
  )
  
  eligible_visit_codes <- sample_ED_data_long[
    !grepl(ineligible_code_pattern, code)
  ]
  
  ## Merge with tree file in wide format to get level2 and level3 parents for each code
  
  icd10_treefile_wide <- fread(
    file.path(parent_dir, "data", paste0("Tree_File_", year, "_wide_format.txt")),
    select = c("Name1", "Level2", "Level3")
  )
  
  ## Remove tree rows that would be removed after merge anyway
  icd10_treefile_wide <- icd10_treefile_wide[
    !is.na(Level2) & !is.na(Level3)
  ]
  
  eligible_visit_codes <- merge(
    eligible_visit_codes,
    icd10_treefile_wide,
    by.x = "code",
    by.y = "Name1",
    all = FALSE,
    sort = TRUE
  )
  
  setDT(eligible_visit_codes)
  
  # Transform again so each row is unique visit rather than unique visit-code
  all_visits <- eligible_visit_codes[
    ,
    .(
      Level3 = paste(Level3, collapse = ","),
      code   = paste(code, collapse = ",")
    ),
    by = .(date, key, severity)
  ]
  
  # Change Level 3 "-" to "_"
  all_visits[, Level3 := gsub("-", "_", Level3, fixed = TRUE)]
  
  ## Keep only incident diagnoses
  
  # Count number of visits per patient/key
  all_visits[, visit_count := .N, by = key]
  
  all_visits_single <- all_visits[
    visit_count == 1,
    .(date, key, severity, code, Level3)
  ]
  
  all_visits_multiple <- all_visits[
    visit_count >= 2,
    .(date, key, severity, code, Level3)
  ]
  
  # Optional: remove helper column from all_visits
  all_visits[, visit_count := NULL]
  
  # Define incident diagnosis for patients with multiple visits
  # Function to process each unique key
  process_patient <- function(patient_data) {
    
    # Calculate the interval matrix
    interval <- as.matrix(dist(patient_data$date))
    interval[upper.tri(interval, diag = TRUE)] <- NA
    interval <- ifelse(interval <= 365, 1, 0)
    
    # Update Level 3 for visits within 365 days
    for(i in 1:(nrow(interval)-1))
    {interval[,i]=ifelse(interval[,i]==1,gsub(","," ",patient_data$Level3[i]),NA)}
    
    # Create search strings from prior visits' Level 3 codes
    patient_data$search <- apply(interval, 1, function(x) {
      # If 'x' has no non-NA values, return NONE
      if (all(is.na(x))) {
        return("NONE")
      } else {
        paste0(unique(unlist(strsplit(na.omit(x), " "))), collapse = "|")
      }
    })
    
    # search = all non admit codes for patient within the past year
    patient_data$search2=NA
    # search2 = all admit codes for patient within the next year
    patient_data$search3=NA
    # searchf = both combined
    patient_data$searchf=NA
    
    # Do this for patients with at least one admit code
    if(nrow(patient_data[which(patient_data$severity=="A"),])>=1)
    {
      admits_only=patient_data[which(patient_data$severity=="A"),]
      
      interval <- as.matrix(dist(admits_only$date))
      interval[lower.tri(interval, diag = TRUE)] <- NA
      interval <- ifelse(interval <= 365, 1, 0)
      
      for(i in 1:nrow(admits_only))
      {
        if(sum(interval[,i],na.rm=T)==0|all(is.na(interval[,i])))
        {patient_data$search2[which(patient_data$severity=="A")[i]]="NONE"}  
        patient_data$search2[which(patient_data$date-patient_data$date[which(patient_data$severity=="A")][i]<0 & patient_data$date-patient_data$date[which(patient_data$severity=="A")][i]>=-365 & patient_data$severity=="V")]=paste0(unique(c(na.omit(unlist(strsplit(patient_data$search2[i],"\\|"))),unlist(strsplit(na.omit(patient_data$Level3[which(patient_data$severity=="A")][i]), ",")))),collapse="|")
      }
      
      if(nrow(admits_only)>=2)
      {
        for(i in 2:nrow(admits_only))
        {
          list_codes=paste0(admits_only$Level3[which((admits_only$date-admits_only$date[i]<0 & admits_only$date-admits_only$date[i]>=-365))],collapse=",")  
          patient_data$search3[which(patient_data$severity=="A")][i]=paste0(setdiff(unlist(strsplit(patient_data$Level3[which(patient_data$severity=="A")][i],",")),unlist(strsplit(list_codes,","))),collapse="|")
        }
      }
    }
    
    for(i in 1:nrow(patient_data))
    {
      
      patient_data$search[i]=ifelse(is.na(patient_data$search3[i])==F,gsub(patient_data$search3[i],"",patient_data$search[i]),patient_data$search[i])
      
      patient_data$searchf[i]=ifelse(is.na(patient_data$search2[i])==T,patient_data$search[i],
                                     ifelse(is.na(patient_data$search2[i])==F & patient_data$search2[i]!="NONE" & patient_data$search[i]=="NONE",patient_data$search2[i],
                                            ifelse(patient_data$search2[i]=="NONE","NONE",
                                                   paste0(c(patient_data$search[i],na.omit(patient_data$search2[i])),collapse = "|"))))
    }   
    return(patient_data)
  }
  
  process_patient_faster <- function(patient_data) {
    
    n <- nrow(patient_data)
    
    dates_num <- as.numeric(patient_data$date)
    severity  <- patient_data$severity
    level3    <- patient_data$Level3
    
    search  <- rep(NA_character_, n)
    search2 <- rep(NA_character_, n)
    search3 <- rep(NA_character_, n)
    searchf <- rep(NA_character_, n)
    
    level3_space <- gsub(",", " ", level3, fixed = TRUE)
    level3_split_space <- strsplit(level3_space, " ", fixed = TRUE)
    level3_split_comma <- strsplit(level3, ",", fixed = TRUE)
    
    ## search = prior Level3 codes within 365 days
    
    for (i in seq_len(n)) {
      
      if (i == 1L) {
        search[i] <- "NONE"
      } else {
        prior_idx <- which(
          seq_len(n) < i &
            abs(dates_num - dates_num[i]) <= 365
        )
        
        if (length(prior_idx) == 0L) {
          search[i] <- "NONE"
        } else {
          prior_codes <- unlist(level3_split_space[prior_idx], use.names = FALSE)
          prior_codes <- prior_codes[!is.na(prior_codes)]
          search[i] <- paste0(unique(prior_codes), collapse = "|")
        }
      }
    }
    
    admit_idx <- which(severity == "A")
    n_admit <- length(admit_idx)
    
    if (n_admit >= 1L) {
      
      admit_dates <- dates_num[admit_idx]
      
      for (i in seq_len(n_admit)) {
        
        this_admit_row  <- admit_idx[i]
        this_admit_date <- dates_num[this_admit_row]
        
        prior_admit_exists <- any(
          seq_len(n_admit) < i &
            abs(admit_dates - admit_dates[i]) <= 365
        )
        
        if (!prior_admit_exists) {
          search2[this_admit_row] <- "NONE"
        }
        
        prior_visit_idx <- which(
          dates_num - this_admit_date < 0 &
            dates_num - this_admit_date >= -365 &
            severity == "V"
        )
        
        if (length(prior_visit_idx) > 0L) {
          
          existing_codes <- unlist(strsplit(search2[i], "\\|"), use.names = FALSE)
          existing_codes <- existing_codes[!is.na(existing_codes)]
          
          new_codes <- level3_split_comma[[this_admit_row]]
          new_codes <- new_codes[!is.na(new_codes)]
          
          search2[prior_visit_idx] <- paste0(
            unique(c(existing_codes, new_codes)),
            collapse = "|"
          )
        }
      }
      
      if (n_admit >= 2L) {
        
        for (i in 2:n_admit) {
          
          this_admit_row <- admit_idx[i]
          this_admit_date <- dates_num[this_admit_row]
          
          prior_admit_rows <- admit_idx[
            admit_dates - this_admit_date < 0 &
              admit_dates - this_admit_date >= -365
          ]
          
          list_codes <- paste0(level3[prior_admit_rows], collapse = ",")
          
          search3[this_admit_row] <- paste0(
            setdiff(
              level3_split_comma[[this_admit_row]],
              unlist(strsplit(list_codes, ",", fixed = TRUE), use.names = FALSE)
            ),
            collapse = "|"
          )
        }
      }
    }
    
    for (i in seq_len(n)) {
      
      if (!is.na(search3[i])) {
        search[i] <- gsub(search3[i], "", search[i])
      }
      
      if (is.na(search2[i])) {
        searchf[i] <- search[i]
      } else if (!is.na(search2[i]) && search2[i] != "NONE" && search[i] == "NONE") {
        searchf[i] <- search2[i]
      } else if (search2[i] == "NONE") {
        searchf[i] <- "NONE"
      } else {
        searchf[i] <- paste0(c(search[i], na.omit(search2[i])), collapse = "|")
      }
    }
    
    patient_data$search  <- search
    patient_data$search2 <- search2
    patient_data$search3 <- search3
    patient_data$searchf <- searchf
    
    patient_data
  }
  
  setDT(all_visits_multiple)
  
  # Apply the faster function to each unique key and create search strings
  # search  = all non-admit codes for patient within the past year
  # search2 = all admit codes for patient within the next year
  # searchf = both combined
  all_visits_multiple_search <- all_visits_multiple[
    ,
    process_patient_faster(.SD),
    by = key
  ]
  
  # Use search string to find non-incident codes to remove
  # remove searchf codes from Level3 and substitute REMOVE for corresponding code
  remove_nonincident <- copy(all_visits_multiple_search)
  
  remove_nonincident[
    ,
    Level3 := mapply(
      function(Level3, searchf) {
        gsub(searchf, "REMOVE", Level3)
      },
      Level3,
      searchf,
      USE.NAMES = FALSE
    )
  ]
  
  remove_nonincident[
    ,
    code := mapply(
      function(code, Level3) {
        codes <- unlist(strsplit(code, ",", fixed = TRUE), use.names = FALSE)
        lvl3  <- unlist(strsplit(Level3, ",", fixed = TRUE), use.names = FALSE)
        
        codes[lvl3 == "REMOVE"] <- "REMOVE"
        
        paste0(codes, collapse = ",")
      },
      code,
      Level3,
      USE.NAMES = FALSE
    )
  ]
  
  setDT(all_visits_single)
  setDT(remove_nonincident)
  
  # Merge back with patients with 1 visit
  # every row is a unique patient-visit
  study_cohort <- rbindlist(
    list(
      all_visits_single[, .(date, key, severity, code)],
      remove_nonincident[, .(date, key, severity, code)]
    ),
    use.names = TRUE
  )
  
  # Keep 90-day study period
  study_data <- study_cohort[
    date >= end - 90 + 1 &
      date <= end
  ]
  
  # Make long dataset by splitting diagnosis codes
  split_codes <- strsplit(study_data$code, ",", fixed = TRUE)
  n_codes <- lengths(split_codes)
  
  study_data_long <- study_data[
    rep.int(seq_len(.N), n_codes)
  ]
  
  study_data_long[
    ,
    code := unlist(split_codes, use.names = FALSE)
  ]
  
  # Remove non-incident codes
  study_data_long <- study_data_long[
    code != "REMOVE"
  ]
  
  setDT(sample_ED_data)
  setDT(study_data_long)
  setDT(icd10_treefile_wide)
  
  # Keep unique
  # If multiple codes within same visit with same Level3, keep rarest
  
  # String of all codes in original sample dataset, deleting blanks
  code <- unlist(
    strsplit(sample_ED_data$diagnosis_codes, split = " ", fixed = TRUE),
    use.names = FALSE
  )
  
  code <- code[code != ""]
  code <- gsub(".", "", code, fixed = TRUE)
  
  # Frequency table for every code in the original sample dataset
  dx_freq_table <- as.data.table(table(code))
  setnames(dx_freq_table, c("code", "Freq"))
  
  # Merge so that we have a column that is the frequency of every code
  # in the original sample dataset
  study_data_long <- merge(
    study_data_long,
    dx_freq_table,
    by = "code",
    all.x = TRUE,
    sort = FALSE
  )
  
  # Add Level3
  study_data_long <- merge(
    study_data_long,
    icd10_treefile_wide[, .(Name1, Level3)],
    by.x = "code",
    by.y = "Name1",
    all.x = TRUE,
    sort = FALSE
  )
  
  # Sort in order of increasing frequency, so rarest is first
  setorder(study_data_long, key, date, Level3, Freq)
  
  # Create column n that counts the number of codes with the same Level3
  # during the same visit
  study_data_long[
    ,
    n := seq_len(.N),
    by = .(key, date, Level3)
  ]
  
  setDT(study_data_long)
  
  # Same as:
  # group_by(key, date, Level3) %>%
  # filter(n() >= 2 & Freq == first(Freq))
  keep_rarest <- study_data_long[
    ,
    .SD[.N >= 2L & Freq == Freq[1L]],
    by = .(key, date, Level3)
  ]
  
  # Same as:
  # mutate(n = sample(seq_along(Freq), size = n(), replace = FALSE)) %>%
  # filter(n == 1)
  tie_breaker <- keep_rarest[
    ,
    {
      tmp <- copy(.SD)
      tmp[, n := sample(seq_along(Freq), size = .N, replace = FALSE)]
      tmp[n == 1L]
    },
    by = .(key, date, Level3)
  ]
  
  # Same as:
  # group_by(key, date, Level3) %>%
  # filter(!(n() >= 2 & Freq == first(Freq))) %>%
  # filter(n == 1)
  no_repeats <- study_data_long[
    ,
    .SD[
      !(.N >= 2L & Freq == Freq[1L]) &
        n == 1L
    ],
    by = .(key, date, Level3)
  ]
  
  study_data_long <- rbindlist(
    list(no_repeats, tie_breaker),
    use.names = TRUE,
    fill = TRUE
  )
  
  v2 <- study_data_long[, c("date", "key", "severity", "code")]
  names(v2) <- c("date", "key", "dispo", "code")
  
  v2$date <- as.Date(v2$date)
  v2$key <- as.character(v2$key)
  v2$dispo <- as.character(v2$dispo)
  v2$code <- gsub("\\.", "", as.character(v2$code))
  
  # force exact column order expected downstream
  v2 <- v2[, c("date", "key", "dispo", "code")]
  
  dir.create(file.path(parent_dir, "data", "v2", final_date), recursive = TRUE, showWarnings = FALSE)
  
  write.csv(v2, paste0(parent_dir, "/data/v2/", final_date, "/lag", LAG, ".csv"), row.names = FALSE, quote = FALSE)

  ### CREATE INPUT FILE
  setDT(study_data_long)
  
  ## CREATE INPUT FILE
  # input file is unique visit date, codes and count of that combo
  
  input_file <- study_data_long[
    ,
    .(n = sum(n)),
    by = .(code, severity, date)
  ]
  
  # Add decimal
  input_file[
    nchar(code) >= 4L,
    code := paste0(
      substr(code, 1L, 3L),
      ".",
      substr(code, 4L, nchar(code))
    )
  ]
  
  # Add 0- and 1-
  input_file[
    ,
    code := fifelse(
      severity == "V",
      paste0("0-", code),
      paste0("1-", code)
    )
  ]
  
  # Keep and order final columns
  input_file <- input_file[
    ,
    .(
      date = format(as.Date(date), "%Y/%m/%d"),
      code = trimws(code),
      n = as.integer(n)
    )
  ]

  dir.create(paste0(parent_dir, "/data/analysis_count_files/Analysis_Count_File_", final_date), recursive = TRUE, showWarnings = FALSE)
  
  # Write a PURE ASCII tab-delimited file (no BOM, no UTF-16)
  write.table(
    input_file[, c("code","date","n")],
    file = paste0(parent_dir, "/data/analysis_count_files/Analysis_Count_File_", final_date, "/lag", LAG, ".txt"),
    sep = "\t",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE,
    fileEncoding = "ASCII"
  )
}
