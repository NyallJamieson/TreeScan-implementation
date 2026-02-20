## TreeScan Implementation

Automated pipeline for creating TreeScan input files from ICD-10-CM codes and ED visit data.

### Quick Start

1. **Open the project**: Double-click `TreeScan-implementation.Rproj` to open the project in RStudio
2. **Open the main script**: Open `code/run_all.R` in RStudio
3. **Review and adjust parameters** (see details below)
4. **Run interactively**: Execute code line-by-line or section-by-section using:
   - **macOS**: `Cmd + Enter` (or click the "Run" button in the top-right of the script pane)
   - **Windows**: `Ctrl + Enter` (or click the "Run" button)
   - **Linux**: `Ctrl + Enter` (or click the "Run" button)

**Note**: Loading packages may take some time if you've never downloaded them before. If you will run on a Linux server, it may be necessary to work with your IT department to ensure all software can be downloaded prior to use.

### Understanding Parameters

Before running the pipeline, review the parameters at the top of `run_all.R`:

**Required Parameters:**
- **`year`**: The year of the ICD-10-CM code set to use (e.g., `2026`). Tree files for 2025 and 2026 are already created.
- **`ph_dept`**: Your public health department identifier (e.g., `"NYC-DHMH"`). 
  - ⚠️ **Important**: This must match the subdirectory name where your ED count data is stored
  - Your data should be in: `input_data/TreeScan_input_files/ED_count_data/[ph_dept]/`
  - Results will be written to: `output/TreeScan_output_files/[ph_dept]/`
- **`data_file`**: Either `"synthetic"` (for the example dataset) or a date string like `"20250629"` matching your data file
  - Must correspond to a file named `ED_count_data_[date].txt` or `Synthetic_Dataset.txt` in your `ph_dept` directory

**Optional Parameters:**
- **`debug_mode`**: Set to `TRUE` for detailed progress messages, `FALSE` for minimal output
- **`force_rerun_part1`**: Set to `TRUE` to force recreation of tree files (default: `FALSE`)
- **`force_rerun_part2`**: Set to `TRUE` to force reprocessing of count data (default: `FALSE`)
- **`force_rerun_part3`**: Set to `TRUE` to force re-running TreeScan analysis (default: `FALSE`)

**Example Setup for Your Department:**
```r
year <- 2026
ph_dept <- "YourDept"  # Must match your folder name in ED_count_data/
data_file <- "20250629"  # Must match ED_count_data_20250629.txt in your folder
debug_mode <- TRUE
```

The pipeline runs in three steps:
1. **Create tree files from ICD-10 hierarchy** - if they don't exist (auto-downloads files if needed). We've already created trees for 2025 and 2026, so this will only need to be run again towards the end of 2026.
2. **Process ED count data** - Takes pre-cleaned ED visit data (one row per person-visit with tab-delimited diagnosis codes), identifies incident diagnoses, and creates the final count file formatted for TreeScan. Example synthetic data is provided from NYC-DHMH.
3. **Run TreeScan analysis** - Executes TreeScan in batch mode and produces HTML, CSV, and TXT result files in the output directory.

**Force Re-run**: By default, each step checks if output files already exist and skips execution if they do. To force a re-run of any step (e.g., after changing parameters or fixing data issues), set the corresponding flag to `TRUE`:
```r
force_rerun_part1 <- TRUE  # Force re-create tree files
force_rerun_part2 <- TRUE  # Force re-process count data
force_rerun_part3 <- TRUE  # Force re-run TreeScan analysis
``` 

### Pipeline Steps

#### Step 1: Create Tree File
Automatically downloads ICD-10 files from CMS if not already present:
- `icd10cm_tabular_[year].xml` - Code hierarchy
- `icd10cm_codes_[year].txt` - Code descriptions

Then processes ICD-10 codes into TreeScan-compatible tree structure:
- Creates wide format tree file (used by Step 2 for code lookups)
- Adds missing codes from TXT file
- Appends supplemental nodes for common etiologies
- Outputs final tree to `input_data/TreeScan_input_files/Final_Tree_File_[year].csv`
- Outputs wide format to `input_data/TreeScan_input_files/intermediate/Tree_File_[year]_wide_format.txt`

#### Step 2: Process ED Count Data
Takes pre-cleaned ED visit data and processes it:
- **Input**: ED count data file with columns: `key` (person ID), `date`, `diagnosis_codes` (tab-delimited), `severity` (V=Visit, A=Admitted)
  - For synthetic data: `Synthetic_Dataset.txt`
  - For real data: `ED_count_data_[YYYYMMDD].txt`
- **Processing**:
  - Removes ineligible codes (seasonal, certain Z codes, neoplasms, congenital conditions)
  - Identifies incident diagnoses (codes not seen in previous 365 days)
  - Keeps rarest code when multiple codes map to same Level 3 category
  - Formats for TreeScan input with prefixes (0- for visits, 1- for admissions)
- **Output**: `Count_File_[data_file].txt` in the same directory as input

#### Step 3: Run TreeScan Analysis
Executes TreeScan in batch mode:
- **Input**: 
  - Tree file: `Final_Tree_File_[year].csv`
  - Count file: `Count_File_[data_file].txt`
  - Parameter template: `Parameter_File.prm` (updated in place with current run's file paths)
- **Processing**:
  - Updates `Parameter_File.prm` with current file paths and date ranges
  - All other parameter settings (Monte Carlo replications, tree levels, temporal windows, etc.) are preserved from the template
  - Runs TreeScan executable in batch mode
  - Default settings: 99,999 Monte Carlo replications, evaluates tree levels 4+, max 28-day temporal window, prospective analysis with day-of-week adjustments
- **Output**: Results files (HTML, CSV, TXT) in `output/TreeScan_output_files/[ph_dept]/`
  - `Results_[data_file]_[end_date].txt` - Main results
  - `Results_[data_file]_[end_date].html` - HTML report with temporal graphs
  - `Results_[data_file]_[end_date].csv` - CSV results for further analysis

**Note on Parameter File**: The `Parameter_File.prm` in `input_data/TreeScan_input_files/` is updated with each run to point to the correct input and output files. You can modify any parameter settings in this file (e.g., number of replications, temporal window size) and they will be preserved across runs - only file paths and date ranges are automatically updated.

### File Structure

```
TreeScan-implementation/
├── code/
│   ├── 0_Download_ICD10_Files.R     # Download CMS files
│   ├── 1_Create_Tree_File.R          # Create tree structure
│   ├── 2_Create_Count_File.R         # Process ED data
│   ├── 3_Run_TreeScan.R              # TODO
│   ├── load_packages.R               # Package management
│   └── run_all.R                     # Main orchestration script
├── input_data/
│   ├── ICD10_code_tree/              # Downloaded ICD-10 files
│   └── TreeScan_input_files/
│       ├── Final_Tree_File_[year].csv      # Final tree for TreeScan
│       ├── intermediate/
│       │   └── Tree_File_[year]_wide_format.txt  # Wide format for lookups
│       └── ED_count_data/
│           └── [ph_dept]/
│               ├── Synthetic_Dataset.txt          # Example data
│               ├── ED_count_data_[date].txt       # Pre-cleaned ED data
│               └── Count_File_[data].txt # Processed count file
└── output/
    └── TreeScan_output_files/        # TreeScan analysis results (TODO)
```

### Data Requirements

For your own department's data, prepare ED visit files with this format:
- **Columns**: `key` (person ID), `date` (YYYY-MM-DD), `diagnosis_codes` (tab-delimited ICD-10 codes), `severity` (V or A)
- **Filename**: `ED_count_data_[YYYYMMDD].txt` where date is the end date of your study period
- **Location**: `input_data/TreeScan_input_files/ED_count_data/[your_ph_dept]/`
- **Coverage**: 15 months of data (for 90-day study period with 365-day lookback)

### Software Requirements

- **R** (version 4.0+)
  - Packages: `xml2`, `tidyverse`, `lubridate`, `here`, `rio`, `pacman`, `purrr`
- **TreeScan™** (version 2.3+)
  - Download from: https://www.treescan.org/
  - Installation paths (used automatically by the pipeline):
    - **macOS**: `/Applications/TreeScan.app/Contents/app/treescan` (default)
    - **Linux**: `/opt/treescan.2.3/treescan64`
    - **Windows**: Typically `C:/Program Files/TreeScan/treescan.exe`
  - To use a custom installation path, set the `treescan_path` parameter when calling `run_treescan()`
- **Internet connection** (for initial ICD-10 file download)

**Important Note for Cross-Platform Users**: File paths in `Parameter_File.prm` use the operating system's native path format. If you're sharing this repository across different operating systems (e.g., developing on macOS but running on Linux), the parameter file will be automatically updated with the correct path format for your system when you run the pipeline.

### Credits

Original code by Ramona Lall & Alison Levin-Rector, October 2025  
Refactored by Emily Javan, February 2026







