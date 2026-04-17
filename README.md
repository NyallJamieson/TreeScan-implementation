## TreeScan for the World Cup

# 🌳 TreeScan Implementation

A full pipeline for running TreeScan-based analyses using R and the TreeScan software.

This project provides a structured workflow to prepare data, run TreeScan, and process results using an R-based pipeline.

---

## 📦 Installation

### 1. Download this Repository

- Click the green **Code** button on GitHub  
- Select **Download ZIP**  
- Extract the ZIP file  
- Locate the `treescan_project` subfolder  
- Move `treescan_project` to your desired working directory  

---

### 2. Install RStudio

Download and install RStudio:  
https://posit.co/download/rstudio-desktop/

---

### 3. Install TreeScan

Download TreeScan from:  
https://www.treescan.org/download_treescan.html  

⚠️ **Important setup details:**

- You must create an account before downloading  
- Choose version based on your environment:
  - **Windows** → if running locally  
  - **Linux** → if running on a server  
- Select the **NON-graphical version**
  - The standard (graphical) version may cause IT/access issues  

---

### 4. Place TreeScan in Project Folder

After downloading:

Move the TreeScan files into the correct subfolder inside `treescan_project`:

| Environment | Folder        |
|------------|--------------|
| Windows    | `TS_windows/` |
| Linux      | `TS_linux/`   |

---

## 🚀 Running the Pipeline

### 1. Open the Project in RStudio

- Launch RStudio  
- In the bottom-right file explorer:
  - Navigate to:
    `treescan_project/code/`
  - Open:
    `run_full_pipeline.R`

---

### 2. Configure the Script

Before running, update the following:

#### Set Working Directory

Update line 4 to match your local path:

```r
setwd("~/TreeScan-implementation/treescan_project")
```

Replace with wherever you saved `treescan_project`.

#### Set Execution Mode

Modify these variables depending on your setup:

```r
server <- FALSE      # Set to TRUE if running on a server
first_time <- TRUE   # Set to FALSE after first run
```

---

### 3. Run the Pipeline

- Run the script in RStudio  

The pipeline will:
- Execute TreeScan  
- Process outputs  
- Complete the full analysis workflow  

---

## ⚠️ Notes

- Ensure the correct TreeScan version is placed in the matching folder (`TS_windows` or `TS_linux`)  
- Using the **non-graphical version is required**  
- Incorrect working directory paths will cause errors  





