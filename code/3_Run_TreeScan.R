#///////////////////////////////////////////////////////////////////////////////
### RUN TREESCAN ANALYSIS IN BATCH MODE
## Ramona Lall & Alison Levin-Rector
## October 2025

# Creates parameter file for TreeScan and executes analysis in batch mode.
# TreeScan documentation: https://www.treescan.org/
#///////////////////////////////////////////////////////////////////////////////

#///////////////////////////////////////////////////////////////////////////////
#' Run TreeScan Analysis in Batch Mode
#'
#' Creates a TreeScan parameter file and executes the analysis. The function
#' reads the count file to determine the date range, creates a parameter file
#' with all TreeScan settings, and runs TreeScan in batch mode.
#'
#' @param year Numeric. Year for tree file (e.g., 2025)
#' @param debug_mode Logical. If TRUE, prints debug information
#' @param ph_dept Character. Public health department name (e.g., "NYC-DHMH")
#' @param data_file Character. Either a date string (e.g., "20250629") or "synthetic"
#' @param treescan_path Character. Path to TreeScan executable. 
#'   Default: "/Applications/TreeScan.app/Contents/app/treescan" (macOS)
#'   Linux: "/opt/treescan.2.3/treescan64"
#' @return Path to the results file
#' @export
run_treescan <- function(
    year = 2026,
    debug_mode = FALSE,
    ph_dept = "NYC-DHMH",
    data_file = "synthetic",
    treescan_path = "/Applications/TreeScan.app/Contents/app/treescan") {
  
  # Start timing
  start_time <- Sys.time()
  
  if (debug_mode) message("Starting run_treescan...")
  if (debug_mode) message("Parameters: year=", year, ", ph_dept=", ph_dept, ", data_file=", data_file)
  
  # Define file paths using here()
  tree_file <- here("input_data", "TreeScan_input_files", 
                    paste0("Final_Tree_File_", year, ".csv"))
  
  count_file <- here("input_data", "TreeScan_input_files", "ED_count_data", ph_dept,
                     paste0("Count_File_", data_file, ".txt"))
  
  # Path to "Do not evaluate nodes" file
  nodes_file <- here("input_data", "TreeScan_input_files", "Do_not_evaluate_nodes.csv")
  
  output_dir <- here("output", "TreeScan_output_files", ph_dept)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Read count file to get date range and verify format
  if (debug_mode) message("Reading count file to determine date range: ", count_file)
  count_data <- read.delim(count_file)
  
  # Verify count file has correct column names and order
  # Standard format: date, code, n (TreeScan reads with field map 2,1,3)
  expected_cols <- c("date", "code", "n")
  if (!identical(names(count_data), expected_cols)) {
    stop("Count file has incorrect column format.\n",
         "  Expected: ", paste(expected_cols, collapse=", "), "\n",
         "  Found: ", paste(names(count_data), collapse=", "), "\n",
         "  File: ", count_file)
  }
  
  count_data <- count_data %>% mutate(date = ymd(date))
  
  date_range <- paste0("[", 
                       format(min(count_data$date), "%Y/%m/%d"), ",",
                       format(max(count_data$date), "%Y/%m/%d"), "]")
  
  if (debug_mode) {
    message("Analysis date range: ", date_range)
    message("Count file verified: ", nrow(count_data), " records with columns [", 
            paste(names(count_data), collapse=", "), "]")
  }
  
  # Define output file paths
  results_file <- file.path(output_dir, 
                            paste0("Results_", data_file, "_", 
                                   format(max(count_data$date), "%Y%m%d"), ".txt"))
  
  # Path to "Do not evaluate nodes" file
  nodes_file <- here("input_data", "TreeScan_input_files", "Do_not_evaluate_nodes.csv")
  
  # Path to parameter file - single file that gets updated for each run
  param_file <- here("input_data", "TreeScan_input_files", "Parameter_File.prm")
  
  if (file.exists(param_file)) {
    if (debug_mode) message("Updating existing parameter file: ", param_file)
    # Read and update the parameter file with run-specific paths
    # NOTE: count-filename-SourceFieldMap should be "2,1,3" for columns: date, code, n
    #       TreeScan field map is: [node position], [time position], [count position]
    param_lines <- readLines(param_file)
    param_lines <- gsub("^tree-filename=.*", paste0("tree-filename=", tree_file), param_lines)
    param_lines <- gsub("^count-filename=.*", paste0("count-filename=", count_file), param_lines)
    param_lines <- gsub("^data-time-range=.*", paste0("data-time-range=", date_range), param_lines)
    param_lines <- gsub("^results-filename=.*", paste0("results-filename=", results_file), param_lines)
    param_lines <- gsub("^window-start-range=.*", paste0("window-start-range=", date_range), param_lines)
    param_lines <- gsub("^window-end-range=.*", paste0("window-end-range=", date_range), param_lines)
    param_lines <- gsub("^not-evaluated-nodes-file=.*", paste0("not-evaluated-nodes-file=", nodes_file), param_lines)
    
    # Verify the field map is correct (should be "2,1,3" for date, code, n)
    field_map_line <- grep("^count-filename-SourceFieldMap=", param_lines, value = TRUE)
    if (length(field_map_line) > 0 && !grepl("2,3,1", field_map_line)) {
      warning("Count file field map may be incorrect. Expected '2,3,1' for columns [date, code, n]. ",
              "Found: ", field_map_line)
    }
    
    writeLines(param_lines, param_file)
  } else {
    # Create from scratch if parameter file doesn't exist
    if (debug_mode) message("Creating parameter file: ", param_file)
    create_treescan_params(
      treefile = tree_file,
      countfile = count_file,
      date_range = date_range,
      resultsfile = results_file,
      nodesfile = nodes_file,
      paramfile = param_file
    )
  }
  
  # Run TreeScan in batch mode
  if (!file.exists(treescan_path)) {
    stop("TreeScan executable not found at: ", treescan_path,
         "\nPlease install TreeScan or provide correct path with treescan_path argument")
  }
  
  message("→ Running TreeScan analysis...")
  if (debug_mode) message("Command: ", treescan_path, " ", param_file)
  
  system_result <- system(paste(treescan_path, param_file, sep = ' '))
  
  if (system_result != 0) {
    stop("TreeScan analysis failed with exit code ", system_result)
  }
  
  # Calculate and report elapsed time
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  message(sprintf("✓ TreeScan analysis completed in %.1f %s", 
                  as.numeric(elapsed_time), 
                  attr(elapsed_time, "units")))
  
  if (debug_mode) message("Results written to: ", results_file)
  
  return(invisible(results_file))
}

#///////////////////////////////////////////////////////////////////////////////
#' Create TreeScan Parameter File
#'
#' Internal function to generate a TreeScan parameter file with all settings
#' for asyndromic surveillance analysis. Only used if template doesn't exist.
#'
#' @param treefile Path to tree file
#' @param countfile Path to count file
#' @param date_range Date range string in format "[yyyy/mm/dd,yyyy/mm/dd]"
#' @param resultsfile Path for output results file
#' @param nodesfile Path to "do not evaluate nodes" file
#' @param paramfile Path for parameter file to create
#' @keywords internal
create_treescan_params <- function(treefile, countfile, date_range, resultsfile, nodesfile, paramfile) {
  
  prminput <- c(
    '[Analysis]',
    # Scan type (TREEONLY=0, TREETIME=1, TIMEONLY=2)
    "scan-type=1",
    # Probability model type (POISSON=0, BERNOULLI_TREE=1, UNIFORM=2, Not-Applicable=3)
    "probability-model=0",
    # Conditional type (UNCONDITIONAL=0, TOTALCASES=1, NODE=2, NODEANDTIME=3)
    "conditional-type=3",
    # Self control design - unconditional Bernoulli only (y/n)
    "self-control-design=n",
    # Case probability (integer/integer)
    "event-probability=1/2",
    # Variable case probability - unconditional Bernoulli only (y/n)
    "variable-case-probability=n",
    # Scan rate type (HIGHRATE=0, LOWRATE=1, HIGHORLOWRATE=2)
    "scan-rate-type=0",
    
    '[Input]',
    # Tree structure filename
    paste0("tree-filename=", treefile),
    # Source type (CSV=0)
    "tree-filename-SourceType=0",
    # Source field map (comma separated list of integers)
    "tree-filename-SourceFieldMap=3,4,1,2",
    # CSV source delimiter (leave empty for space or tab delimiter)
    "tree-filename-SourceDelimiter=,",
    # CSV source group character
    'tree-filename-SourceGrouper="',
    # CSV source skip initial lines (i.e. meta data)
    "tree-filename-SourceSkip=0",
    # CSV source first row column header
    "tree-filename-SourceFirstRowHeader=y",
    # Count data filename
    paste0("count-filename=", countfile),
    # Source type (CSV=0)
    "count-filename-SourceType=0",
    # Source field map (comma separated list of integers: node, time, count)
    # For file format [date, code, n], map is 2,3,1 (code is col 2, date is col 1, count is col 3)
    "count-filename-SourceFieldMap=2,3,1",
    # CSV source delimiter (leave empty for space or tab delimiter)
    'count-filename-SourceDelimiter=" "',
    # CSV source group character
    'count-filename-SourceGrouper="',
    # CSV source skip initial lines (i.e. meta data)
    "count-filename-SourceSkip=0",
    # CSV source first row column header
    "count-filename-SourceFirstRowHeader=y",
    # Control data filename
    "control-filename=",
    # Date precision type (NONE=0, GENERIC=1, YEAR=2, MONTH=3, DAY=4)
    "date-precision=4",
    # Data time ranges: [integer,integer] or [yyyy/mm/dd,yyyy/mm/dd]
    paste0("data-time-range=", date_range),
    
    '[Output]',
    # Results filename
    paste0("results-filename=", resultsfile),
    # Create HTML results (y/n)
    "results-html=y",
    # Create CSV results (y/n)
    "results-csv=y",
    # Create NCBI Asn results (y/n)
    "results-asn=n",
    # Create Newick File (y/n)
    "results-nwk=n",
    
    '[Advanced Input]',
    # Cuts filename
    "cut-filename=",
    # Default cuts type (SIMPLE=0, PAIRS=1, TRIPLETS=2, ORDINAL=3, COMBINATORIAL=4)
    "cut-type=0",
    # Allow data only on tree leaves (y/n)
    "data-only-on-leaves=n",
    # Ignore cases outside study period (y/n)
    "relaxed-study-data-period-checking=y",
    # Allow multi-parent nodes (y/n)
    "allow-multi-parent-nodes=y",
    # Allow multiple root nodes (y/n)
    "allow-multiple-roots=n",
    # Minimum censor time (2 <= x)
    "minimum-censor-time=50",
    # Minimum censor time percentage of study period (0 < x <= 100.0)
    "min-censor-percentage=10",
    # Apply risk window restriction due to censoring (y/n)
    "risk-window-restriction-censor=n",
    # Risk window alternative censor denominator (integer)
    "risk-window-alt-censor-denominator=2",
    
    '[Temporal Window]',
    # Maximum temporal size as percentage of data time range (0 < x <= 50.0)
    "maximum-window-percentage=50",
    # Maximum temporal size as fixed time length (integer)
    "maximum-window-fixed=28",
    # Maximum temporal size selection (PERCENTAGE_WINDOW=0, FIXED_LENGTH=1)
    "maximum-window-type=1",
    # Minimum temporal size as fixed time length (integer)
    "minimum-window-fixed=1",
    # Apply risk window restriction (y/n)
    "apply-risk-window-restriction=n",
    # Risk window percentage (0 < x <= 100.0)
    "risk-window-percentage=20",
    # Prospective analysis (y/n)
    "prospective-analysis=y",
    # Restrict temporal windows (y/n)
    "restricted-window-range=n",
    # Start data time range: [integer,integer] or [yyyy/mm/dd,yyyy/mm/dd]
    paste0("window-start-range=", date_range),
    # End data time range: [integer,integer] or [yyyy/mm/dd,yyyy/mm/dd]
    paste0("window-end-range=", date_range),
    
    '[Adjustments]',
    # Perform day of week adjustments (y/n)
    "perform-day-of-week-adjustments=y",
    # Apply exclusion time ranges (y/n)
    "apply-exclusion-data-ranges=n",
    # Exclusion time ranges (semi-colon separated list)
    "exclusion-data-ranges=",
    
    '[Inference]',
    # Number of simulation replications (0, 9, 999, n999)
    "monte-carlo-replications=99999",
    # Restrict tree levels evaluated (y/n)
    "restrict-tree-levels=y",
    # Tree levels excluded from evaluation (csv list, root level is 1)
    "excluded-tree-levels=1,2,3",
    # Restrict tree nodes evaluated (y/n)
    "restrict-evaluated-nodes=y",
    # Not evaluated tree nodes filename
    paste0("not-evaluated-nodes-file=", nodesfile),
    # Source type (CSV=0)
    "not-evaluated-nodes-file-SourceType=0",
    # Source field map (comma separated list of integers)
    "not-evaluated-nodes-file-SourceFieldMap=1",
    # CSV source delimiter (leave empty for space or tab delimiter)
    "not-evaluated-nodes-file-SourceDelimiter=,",
    # CSV source group character
    'not-evaluated-nodes-file-SourceGrouper="',
    # CSV source skip initial lines (i.e. meta data)
    "not-evaluated-nodes-file-SourceSkip=0",
    # CSV source first row column header
    "not-evaluated-nodes-file-SourceFirstRowHeader=y",
    # Randomization seed (integer)
    "randomization-seed=12345678",
    # Generate randomization seed (y/n)
    "random-randomization-seed=n",
    # Minimum number of cases in a node (integer)
    "minimum-node-cases=3",
    # P-value reporting type (STANDARD_PVALUE=0, TERMINATION_PVALUE=1)
    "pvalue-report-type=1",
    # Early termination threshold (> 0)
    "early-termination-threshold=5000",
    
    '[Sequential Scan]',
    # Perform sequential scan - time-only scan (y/n)
    "sequential-scan=n",
    # Sequential scan maximum cases for signal (integer)
    "sequential-maximum-signal=200",
    # Sequential scan - minimum cases to signal (integer)
    "sequential-minimum-signal=3",
    # Sequential scan filename
    "sequential-filename=",
    # Sequential alpha overall
    "sequential-alpha-overall=0.05",
    # Sequential alpha spending
    "sequential-alpha-spending=0.01",
    
    '[Power Evaluations]',
    # Perform power evaluations (y/n)
    "perform-power-evaluations=n",
    # Power evaluation type (0=Analysis And Power, 1=Only Power With Count, 2=Only Power With Defined Cases)
    "power-evaluation-type=0",
    # Critical values type (0=Monte Carlo, 1=User Specified Values)
    "critical-values-type=0",
    # Power evaluation critical value .05 (> 0)
    "critical-value-05=0",
    # Power evaluation critical value .01 (> 0)
    "critical-value-01=0",
    # Power evaluation critical value .001 (> 0)
    "critical-value-001=0",
    # Total cases in power evaluation (integer)
    "power-evaluation-totalcases=600",
    # Number of replications in power step (integer)
    "power-evaluation-replications=1000",
    # Power evaluation alternative hypothesis filename
    "alternative-hypothesis-filename=",
    # Power baseline probability (integer/integer)
    "baseline-probability=1/2",
    # Power z value (0 < z <= 0.01)
    "power-z=0.001",
    
    '[Miscellaneous Analysis]',
    # Frequency of prospective analyses type (0=Daily, 1=Weekly, 2=Monthly, 3=Quarterly, 4=Yearly)
    "prospective-frequency-type=0",
    # Frequency of prospective (integer)
    "prospective-frequency=1",
    
    '[Additional Output]',
    # Create LLR results (y/n)
    "results-llr=n",
    # Report critical values (y/n)
    "report-critical-values=y",
    # Report attributable risk (y/n)
    "report-attributable-risk=n",
    # Number of exposed attributable risk is based upon (positive integer)
    "attributable-risk-exposed=0",
    # Report parent cuts that match child cuts (y/n)
    "include-identical-parent-cuts=n",
    # Output temporal graph HTML file (y/n)
    "output-temporal-graph-html=y",
    # Temporal graph cluster reporting type (0=Only most likely, 1=X most likely, 2=Only significant)
    "temporal-graph-type=1",
    # Number of most likely clusters to report in temporal graph (positive integer)
    "temporal-graph-most-mlc=100",
    # Significant clusters p-value cutoff to report in temporal graph (0.000-1.000)
    "temporal-graph-significance-cutoff=0.05",
    
    '[Power Simulations]',
    # Input simulation data (y/n)
    "input-simulations=n",
    # Input simulation filename
    "input-simulations-file=",
    # Output simulation data (y/n)
    "output-simulations=n",
    # Output simulation filename
    "output-simulations-file=",
    
    '[Run Options]',
    # Number of parallel processes to execute (0=All Processors, x=At Most X Processors)
    "parallel-processes=0",
    
    '[System]',
    # Parameters version - do not modify
    "parameters-version=2.3.0"
  )
  
  write(prminput, paramfile)
}
