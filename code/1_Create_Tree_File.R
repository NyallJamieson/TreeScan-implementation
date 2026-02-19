## CREATE TREE FILES FOR ASYNDROMIC TREESCAN ANALYSES
## Ramona Lall & Alison Levin-Rector
## October 2025
#
## PART 1:
## READ IN XML FILES TO CREATE TREE FILES IN LONG AND WIDE FORMAT
#
## For PARTS 1 & 2: Download ICD-10 Files from https://www.cms.gov/medicare/coding-billing/icd-10-codes:
# 1) icd10cm_tabular_[year].xml from Code Tables, Tabular and Index ZIP file
# includes all codes except at Level 8
#
# 2) icd10cm_codes_[year].txt from Code Descriptions in Tabular Order ZIP file
# includes all codes from Level 5 to Level 8

#' Create Tree Files for TreeScan Analysis
#' 
#' @param year Numeric year for ICD-10-CM codes (default: 2026)
#' @param debug_mode Logical. If TRUE, enables verbose output with ic() statements 
#'   and saves/loads intermediate .rds files for faster debugging (default: FALSE)
#' @return Path to the final tree file created
create_tree_file <- function(year = 2026, debug_mode = FALSE) {
  
  # Start timer
  start_time <- Sys.time()
  
  # Load required packages
  suppressPackageStartupMessages({
    library(xml2)
    library(dplyr)
    library(stringr)
    library(here)
    library(rio)
    library(icecream)
  })
  
  # Configure icecream based on debug mode
  if (!debug_mode) {
    ic_disable()
  }
  
  # Create intermediate directory if in debug mode
  intermediate_dir <- here("input_data", "TreeScan_input_files", "intermediate")
  if (debug_mode && !dir.exists(intermediate_dir)) {
    dir.create(intermediate_dir, recursive = TRUE)
    message("✓ Created intermediate directory for debug files")
  }
  
  message("Creating tree files for year ", year, "...")
  
  # Check for required input files and download if missing
  xml_file <- here("input_data", "ICD10_code_tree", paste0("icd10cm_tabular_", year, ".xml"))
  txt_file <- here("input_data", "ICD10_code_tree", paste0("icd10cm_codes_", year, ".txt"))
  
  ic(xml_file)
  ic(txt_file)
  ic(file.exists(xml_file))
  ic(file.exists(txt_file))
  
  if (!file.exists(xml_file) || !file.exists(txt_file)) {
    message("→ Required ICD-10 files not found, downloading from CMS...")
    source(here("code", "0_Download_ICD10_Files.R"))
    download_icd10_files(year = year)
  }
  
  # Read in the XML file
  message("→ Reading XML file...")
  icd10_xml <- xml2::read_xml(xml_file)
  
  # Parse the XML into a list
  message("→ Parsing XML chapters...")
  huge_xml_dict <- xml_find_all(icd10_xml, "//ICD10CM.tabular/chapter")
  ic(length(huge_xml_dict))
  
  ## Create long tree from xml -------------------------------------------------
  message("→ Step 1: Initialize long format tree from XML...")
  # Initialize list to collect data
  tree_data <- list()
  # Add root manually
  tree_data[[length(tree_data) + 1]] <- c("Root", NA, 1, "ICD-10 Root")
  
  ## Create long tree from xml -------------------------------------------------
  message("→ Step 2: Creating long format tree from XML...")
  # Initialize list to collect data
  tree_data <- list()
  # Add root manually
  tree_data[[length(tree_data) + 1]] <- c("Root", NA, 1, "ICD-10 Root")
  
  # Iterate through each chapter
  for (item in huge_xml_dict) {
    desc <- xml_text(xml_find_first(item, "desc"))
    match <- regexpr("(.+)\\((.+)\\)", desc)
    
    if (attr(match, "match.length") > 0) {
      parent_name <- sub("^(.+)\\((.+)\\)$", "\\2", desc)
      full_desc <- sub("^(.+)\\((.+)\\)$", "\\1", desc)
      tree_data[[length(tree_data) + 1]] <- c(parent_name, "Root", 1, full_desc)
      
      sections <- xml_find_all(item, "section")
      
      for (entry in sections) {
        entry_desc <- xml_text(xml_find_first(entry, "desc"))
        entry_id <- xml_attr(entry, "id")
        match <- regexpr("(.+)\\(.+\\)", entry_desc)
        sec_desc <- sub("^(.+)\\((.+)\\)$", "\\1", entry_desc)
        
        if (attr(match, "match.length") > 0) {
          tree_data[[length(tree_data) + 1]] <- c(entry_id, parent_name, 1, sec_desc)
          
          diagnostics <- xml_find_all(entry, "diag")
          if (length(diagnostics) == 0) next
          
          for (diag in diagnostics) {
            diag_name <- xml_text(xml_find_first(diag, "name"))
            diag_desc <- xml_text(xml_find_first(diag, "desc"))
            tree_data[[length(tree_data) + 1]] <- c(diag_name, entry_id, 1, diag_desc)
            
            sub_diagnostics <- xml_find_all(diag, "diag")
            if (length(sub_diagnostics) == 0) next
            
            for (sub_diag in sub_diagnostics) {
              sub_diag_name <- xml_text(xml_find_first(sub_diag, "name"))
              sub_diag_desc <- xml_text(xml_find_first(sub_diag, "desc"))
              tree_data[[length(tree_data) + 1]] <- c(sub_diag_name, diag_name, 1, sub_diag_desc)
              
              sub_diagnostics3 <- xml_find_all(sub_diag, "diag")
              if (length(sub_diagnostics3) == 0) next
              
              for (sub_diag3 in sub_diagnostics3) {
                sub_diag3_name <- xml_text(xml_find_first(sub_diag3, "name"))
                sub_diag3_desc <- xml_text(xml_find_first(sub_diag3, "desc"))
                tree_data[[length(tree_data) + 1]] <- c(sub_diag3_name, sub_diag_name, 1, sub_diag3_desc)
                
                sub_diagnostics4 <- xml_find_all(sub_diag3, "diag")
                if (length(sub_diagnostics4) == 0) next
                
                for (sub_diag4 in sub_diagnostics4) {
                  sub_diag4_name <- xml_text(xml_find_first(sub_diag4, "name"))
                  sub_diag4_desc <- xml_text(xml_find_first(sub_diag4, "desc"))
                  tree_data[[length(tree_data) + 1]] <- c(sub_diag4_name, sub_diag3_name, 1, sub_diag4_desc)
                  
                  sub_diagnostics5 <- xml_find_all(sub_diag4, "diag")
                  if (length(sub_diagnostics5) == 0) next
                  
                  for (sub_diag5 in sub_diagnostics5) {
                    sub_diag5_name <- xml_text(xml_find_first(sub_diag5, "name"))
                    sub_diag5_desc <- xml_text(xml_find_first(sub_diag5, "desc"))
                    tree_data[[length(tree_data) + 1]] <- c(sub_diag5_name, sub_diag4_name, 1, sub_diag5_desc)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  # Convert list of rows to data frame
  tree_df <- as.data.frame(do.call(rbind, tree_data), stringsAsFactors = FALSE)
  colnames(tree_df) <- c("name", "parent", "level", "description")
  ic(nrow(tree_df))
  ic(ncol(tree_df))
  message("✓ Long format tree created with ", nrow(tree_df), " rows")
  
  
  ## Create wide tree from xml -------------------------------------------------
  message("→ Step 3: Creating wide format tree from XML...")
  # Initialize list to collect rows
  tree_data_wide <- list()
  # Define column names
  col_names <- c("Name", "Desc", paste0("Level", 1:8))
  
  # Iterate through each chapter
  for (item in huge_xml_dict) {
    desc <- xml_text(xml_find_first(item, "desc"))
    match <- regexpr("(.+)\\((.+)\\)", desc)
    
    if (attr(match, "match.length") > 0) {
      parent_name <- sub("^(.+)\\((.+)\\)$", "\\2", desc)
      
      # Iterate through each section in the chapter
      sections <- xml_find_all(item, "section")
      
      for (entry in sections) {
        entry_desc <- xml_text(xml_find_first(entry, "desc"))
        entry_id <- xml_attr(entry, "id")
        match <- regexpr("(.+)\\(.+\\)", entry_desc)
        sec_desc <- sub("^(.+)\\((.+)\\)$", "\\1", entry_desc)
        
        if (attr(match, "match.length") > 0) {
          
          diagnostics <- xml_find_all(entry, "diag")
          if (length(diagnostics) == 0) next
          
          for (diag in diagnostics) {
            diag_name <- xml_text(xml_find_first(diag, "name"))
            diag_desc <- xml_text(xml_find_first(diag, "desc"))
            tree_data_wide[[length(tree_data_wide) + 1]] <- c(
              diag_name, diag_desc, "Root", parent_name, entry_id, diag_name, "", "", "", ""
            )
            
            sub_diagnostics <- xml_find_all(diag, "diag")
            if (length(sub_diagnostics) == 0) next
            
            for (sub_diag in sub_diagnostics) {
              sub_diag_name <- xml_text(xml_find_first(sub_diag, "name"))
              sub_diag_desc <- xml_text(xml_find_first(sub_diag, "desc"))
              tree_data_wide[[length(tree_data_wide) + 1]] <- c(
                sub_diag_name, sub_diag_desc, "Root", parent_name, entry_id, diag_name, sub_diag_name, "", "", ""
              )
              
              sub_diagnostics3 <- xml_find_all(sub_diag, "diag")
              if (length(sub_diagnostics3) == 0) next
              
              for (sub_diag3 in sub_diagnostics3) {
                sub_diag3_name <- xml_text(xml_find_first(sub_diag3, "name"))
                sub_diag3_desc <- xml_text(xml_find_first(sub_diag3, "desc"))
                tree_data_wide[[length(tree_data_wide) + 1]] <- c(
                  sub_diag3_name, sub_diag3_desc, "Root", parent_name, entry_id, diag_name, sub_diag_name, sub_diag3_name, "", ""
                )
                
                sub_diagnostics4 <- xml_find_all(sub_diag3, "diag")
                if (length(sub_diagnostics4) == 0) next
                
                for (sub_diag4 in sub_diagnostics4) {
                  sub_diag4_name <- xml_text(xml_find_first(sub_diag4, "name"))
                  sub_diag4_desc <- xml_text(xml_find_first(sub_diag4, "desc"))
                  tree_data_wide[[length(tree_data_wide) + 1]] <- c(
                    sub_diag4_name, sub_diag4_desc, "Root", parent_name, entry_id, diag_name, sub_diag_name, sub_diag3_name, sub_diag4_name, ""
                  )
                  
                  sub_diagnostics5 <- xml_find_all(sub_diag4, "diag")
                  if (length(sub_diagnostics5) == 0) next
                  
                  for (sub_diag5 in sub_diagnostics5) {
                    sub_diag5_name <- xml_text(xml_find_first(sub_diag5, "name"))
                    sub_diag5_desc <- xml_text(xml_find_first(sub_diag5, "desc"))
                    tree_data_wide[[length(tree_data_wide) + 1]] <- c(
                      sub_diag5_name, sub_diag5_desc, "Root", parent_name, entry_id, diag_name, sub_diag_name, sub_diag3_name, sub_diag4_name, sub_diag5_name
                    )
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  # Convert list to data frame
  tree_df_wide <- as.data.frame(do.call(rbind, tree_data_wide), stringsAsFactors = FALSE)
  colnames(tree_df_wide) <- col_names
  ic(nrow(tree_df_wide))
  ic(ncol(tree_df_wide))
  message("✓ Wide format tree created with ", nrow(tree_df_wide), " rows")
  
  
  ## Step 4: ADD CODES FROM TXT FILE THAT ARE MISSING FROM XML -----------------
  message("→ Step 4: Reading TXT file and identifying missing codes...")
  
  # Read in txt file 
  icd10cm.codes <- read.delim( # rio::import was not behaving here...
    here("input_data", "ICD10_code_tree", paste0("icd10cm_codes_", year, ".txt")), 
    header = FALSE
  )
  icd10cm.codes$Name1=substr(icd10cm.codes$V1,1,7)
  icd10cm.codes$Name1=gsub(" ","",icd10cm.codes$Name1)
  ic(nrow(icd10cm.codes))
  
  # Read in tree file the wide format with Level1-Level8 for each code
  tree_df_wide$Name1=gsub("\\.","",tree_df_wide$Name)
  
  # Merge and identify codes not already in tree
  icd10_treefilew_f=merge(tree_df_wide,icd10cm.codes,by="Name1",all=TRUE)
  icd10_treefilew_f$added=ifelse(is.na(icd10_treefilew_f$Level2)==T,1,0)
  ic(nrow(icd10_treefilew_f))
  ic(sum(icd10_treefilew_f$added))
  message("→ Found ", sum(icd10_treefilew_f$added), " codes in TXT not in XML")
  
  # All codes not in current tree are in Level 8
  # Assign all Levels for codes identified as not in Tree
  message("→ Assigning hierarchy levels for missing codes...")
  for(i in 1:nrow(icd10_treefilew_f))
  {
    if(is.na(icd10_treefilew_f$Level2[i])==T)
    {
      code=paste0(substr(icd10_treefilew_f$Name1[i],1,3),".",substr(icd10_treefilew_f$Name1[i],4,nchar(icd10_treefilew_f$Name1[i])))
      code_len=nchar(code)
      icd10_treefilew_f[i,c("Level1","Level2","Level3","Level4","Level5","Level6","Level7","Level8")][code_len]=code
      for(n in (code_len-1):5)
      {
        if(substr(code,1,n) %in% icd10_treefilew_f[which(icd10_treefilew_f$added==0),c("Level1","Level2","Level3","Level4","Level5","Level6","Level7","Level8")][,n])
        {
          icd10_treefilew_f[i,c("Level1","Level2","Level3","Level4","Level5","Level6","Level7","Level8")][n]=substr(code,1,n)}
      }
      for(n in 4)
      {
        if(substr(code,1,3) %in% icd10_treefilew_f[which(icd10_treefilew_f$added==0),c("Level1","Level2","Level3","Level4","Level5","Level6","Level7","Level8")][,n])
        {
          icd10_treefilew_f[i,c("Level1","Level2","Level3","Level4","Level5","Level6","Level7","Level8")][n]=substr(code,1,3)
        }
      }
      # Add levels 1,2,3
      icd10_treefilew_f[i,c("Level1","Level2","Level3")]=unique(icd10_treefilew_f[which(icd10_treefilew_f$added==0 & icd10_treefilew_f$Level4==icd10_treefilew_f$Level4[i]),c("Level1","Level2","Level3")])  
    }
  }
  
  # Some Level8 have missing Level7; for those cases assign last available level
  icd10_treefilew_f$Level7[is.na(icd10_treefilew_f$Level8)==F & is.na(icd10_treefilew_f$Level7)==T]=icd10_treefilew_f$Level6[is.na(icd10_treefilew_f$Level8)==F & is.na(icd10_treefilew_f$Level7)==T]
  icd10_treefilew_f$Level7[is.na(icd10_treefilew_f$Level8)==F & is.na(icd10_treefilew_f$Level7)==T]=icd10_treefilew_f$Level5[is.na(icd10_treefilew_f$Level8)==F & is.na(icd10_treefilew_f$Level7)==T]
  icd10_treefilew_f$Level7[is.na(icd10_treefilew_f$Level8)==F & is.na(icd10_treefilew_f$Level7)==T]=icd10_treefilew_f$Level4[is.na(icd10_treefilew_f$Level8)==F & is.na(icd10_treefilew_f$Level7)==T]
  
  # Fill in Name and Desc of added codes
  icd10_treefilew_f$Name[which(nchar(icd10_treefilew_f$Name1)==3 & icd10_treefilew_f$added==1)]=icd10_treefilew_f$Name1[which(nchar(icd10_treefilew_f$Name1)==3 & icd10_treefilew_f$added==1)]
  icd10_treefilew_f$Name[which(nchar(icd10_treefilew_f$Name1)>=5 & icd10_treefilew_f$added==1)]=paste0(substr(icd10_treefilew_f$Name1[which(nchar(icd10_treefilew_f$Name1)>=5 & icd10_treefilew_f$added==1)],1,3),".",substr(icd10_treefilew_f$Name1[which(nchar(icd10_treefilew_f$Name1)>=5 & icd10_treefilew_f$added==1)],4,nchar(icd10_treefilew_f$Name1[which(nchar(icd10_treefilew_f$Name1)>=5 & icd10_treefilew_f$added==1)])))
  
  icd10_treefilew_f$Desc[which(icd10_treefilew_f$added==1)]=gsub("^[A-Z][0-9][0-9AB]\\.?[0-9A-TV-Z]{0,4} ","",icd10_treefilew_f$V1[which(icd10_treefilew_f$added==1)])
  
  # Fix Level3 where value is same as Level4
  icd10_treefilew_f <- icd10_treefilew_f %>%
    mutate(
      Level3 = if_else(Level3 == Level4, 
                       paste0(Level4, "-", Level4), 
                       Level3)
    )
  
  # Keep Level 7 and Level 8 for codes added to add to Final Tree (Long Format)
  message("→ Preparing codes to add to long format tree...")
  codes_to_add=unique(icd10_treefilew_f[which(icd10_treefilew_f$added==1),c("Level8","Level7","Desc")])
  ic(nrow(codes_to_add))
  
  # Add missing codes
  colnames(tree_df)[1:4] <- c("X1", "X2", "X3", "X4")
  
  if (nrow(codes_to_add) > 0) {
    # Only add codes if there are any to add
    codes_to_add$X3 <- "1"  # Use character "1" instead of integer
    codes_to_add=codes_to_add[,c("Level8","Level7","X3","Desc")]
    names(codes_to_add)=names(tree_df)
    # Ensure all columns are character
    codes_to_add <- codes_to_add |> mutate(across(everything(), as.character))
    icd10_treefile_added=rbind(tree_df,codes_to_add)
    ic(nrow(icd10_treefile_added))
    message("✓ Added ", nrow(codes_to_add), " missing codes from TXT file")
  } else {
    # No codes to add - all codes from TXT were already in XML
    icd10_treefile_added=tree_df
    ic(nrow(icd10_treefile_added))
    message("✓ No missing codes to add - all codes from TXT file were in XML")
  }
  
  # Fix Level3
  # subset codes where parent and child match
  message("→ Fixing Level3 where parent and child match...")
  temp=icd10_treefile_added$X1[which(icd10_treefile_added$X1==icd10_treefile_added$X2)]
  ic(length(temp))
  
  for(i in 1:length(temp))
  {
    icd10_treefile_added$X1[which(icd10_treefile_added$X1==temp[i])][1]=paste0(temp[i],"-",temp[i]) 
    icd10_treefile_added$X2[which(icd10_treefile_added$X1==temp[i])]=paste0(temp[i],"-",temp[i]) 
  }
  
  # Export wide Tree File, which is used in creating Count File
  message("→ Exporting wide format tree file...")
  wide_output_dir <- if (debug_mode) intermediate_dir else here("input_data", "TreeScan_input_files")
  rio::export(
    icd10_treefilew_f, 
    here(wide_output_dir, paste0("Tree_File_", year, "_wide_format.txt"))
  )
  message("✓ Wide format saved")
  
  # Export long Tree File, which is used to update tree files with annual ICD-10 code updates
  message("→ Exporting long format tree file...")
  long_output_dir <- if (debug_mode) intermediate_dir else here("input_data", "TreeScan_input_files")
  rio::export(
    icd10_treefile_added, 
    here(long_output_dir, paste0("Tree_File_", year, "_long_format.txt"))
  )
  message("✓ Long format saved")
  
  
  ## Step 5: CREATE FINAL TREE FILE FOR TREESCAN INPUT -------------------------
  message("→ Step 5: Creating final TreeScan input file...")
  # Append supplemental nodes and add in 0-, 1-, 2- prefixes
  
  # Check if intermediate file exists (only in debug mode)
  intermediate_file <- here(intermediate_dir, paste0("full_w_common_", year, ".rds"))
  
  if (debug_mode && file.exists(intermediate_file)) {
    message("→ Loading existing full_w_common from intermediate file...")
    full_w_common <- readRDS(intermediate_file)
    ic(nrow(full_w_common))
  } else {
    # Import supplemental nodes connecting ICD-10-CM code on different parts of tree that share a common etiology
    message("→ Importing supplemental nodes...")
    supplemental_nodes <- read_csv(
      here("input_data", "ICD10_code_tree", "Common_etiology_nodes.csv"),
      col_types = cols(X3 = col_character()), show_col_types=FALSE
    )
    ic(nrow(supplemental_nodes))
    
    # Append supplemental codes
    message("→ Appending supplemental codes...")
    full_w_common <- bind_rows(icd10_treefile_added, supplemental_nodes)
    ic(nrow(full_w_common))
  
    # Save intermediate file (only in debug mode)
    if (debug_mode) {
      saveRDS(full_w_common, intermediate_file)
      message("✓ Saved intermediate file: full_w_common")
    }
  }
  
  # Check if tree files exist (only in debug mode)
  trees_file <- here(intermediate_dir, paste0("trees_", year, ".rds"))
  
  if (debug_mode && file.exists(trees_file)) {
    message("→ Loading existing tree1-tree5 from intermediate file...")
    trees_list <- readRDS(trees_file)
    tree1 <- trees_list$tree1
    tree2 <- trees_list$tree2
    tree3 <- trees_list$tree3
    tree4 <- trees_list$tree4
    tree5 <- trees_list$tree5
    ic(nrow(tree1))
    ic(nrow(tree2))
  } else {
    # add in 0- and 1- prefixes for differentiating visits and admits
    message("→ Creating 5 tree versions with prefixes...")
    
    # Check structure before creating trees (only in debug mode)
    ic(colnames(full_w_common))
    ic(head(full_w_common, 3))
    
    tree1 <- full_w_common %>%
      transmute(
        child        = str_c("0-",X1),
        parent       = str_c("0-",X2),
        description  = str_c("0-",X4),
        distance_between = X3
      )
    ic(colnames(tree1))
    ic(nrow(tree1))
    
    tree2 <- full_w_common %>%
      transmute(
        child        = str_c("1-",X1),
        parent       = str_c("1-",X2),
        description  = str_c("1-",X4),
        distance_between = X3
      )
    ic(colnames(tree2))
    ic(nrow(tree2))
    
    tree3 <- full_w_common %>%
      transmute(
        child        = str_c("0-",X1),
        parent       = str_c("2-",X1),
        description  = str_c("0-",X4),
        distance_between = X3
      )
    ic(colnames(tree3))
    ic(nrow(tree3))
    
    tree4 <- full_w_common %>%
      transmute(
        child        = str_c("1-",X1),
        parent       = str_c("2-",X1),
        description  = str_c("1-",X4),
        distance_between = X3
      )
    ic(colnames(tree4))
    ic(nrow(tree4))
    
    tree5 <- full_w_common %>%
      transmute(
        child        = str_c("2-",X1),
        parent       = str_c("2-",X2),
        description  = str_c("2-",X4),
        distance_between = X3
      )
    ic(colnames(tree5))
    ic(nrow(tree5))
    
    # Save intermediate tree files (only in debug mode)
    if (debug_mode) {
      saveRDS(list(tree1=tree1, tree2=tree2, tree3=tree3, tree4=tree4, tree5=tree5), trees_file)
      message("✓ Saved intermediate file: tree1-tree5")
    }
  }
  
  message("→ Combining and cleaning final tree...")
  
  # Check all tree structures before combining (only in debug mode)
  if (debug_mode) {
    message("→ Verifying tree structures...")
    ic(colnames(tree1))
    ic(colnames(tree2))
    ic(colnames(tree3))
    ic(colnames(tree4))
    ic(colnames(tree5))
    
    # Check for parent column in each
    if (!"parent" %in% colnames(tree1)) stop("tree1 missing 'parent' column")
    if (!"parent" %in% colnames(tree2)) stop("tree2 missing 'parent' column")
    if (!"parent" %in% colnames(tree3)) stop("tree3 missing 'parent' column")
    if (!"parent" %in% colnames(tree4)) stop("tree4 missing 'parent' column")
    if (!"parent" %in% colnames(tree5)) stop("tree5 missing 'parent' column")
  }
  
  fulltree_final <- bind_rows(tree1, tree2, tree3, tree4, tree5) %>%
    # where parent not in ("0-","1-")
    dplyr::filter(!parent %in% c("0-", "1-")) %>%
    # keep child parent distance_between description + do same cleaning as SAS
    mutate(
      parent = if_else(parent == "2-", "", parent),
      description = if_else(description %in% c("0-", "1-", "2-"), "", description),
      parent = if_else(child %in% c("0-Root", "1-Root"), "2-Root", parent)
    ) %>%
    select(distance_between, description, child, parent)
  
  ic(nrow(fulltree_final))
  
  # Export final tree file
  message("→ Exporting final TreeScan tree file...")
  output_file <- here("input_data", "TreeScan_input_files", 
                      paste0("Final_Tree_File_", year, ".csv"))
  rio::export(fulltree_final, output_file)
  message("✓ Final tree file saved")
  
  # Calculate elapsed time
  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  if (elapsed < 60) {
    message(sprintf("✓ Tree files created successfully in %.1f seconds", elapsed))
  } else {
    message(sprintf("✓ Tree files created successfully in %.1f minutes", elapsed / 60))
  }
  
  return(invisible(output_file))
}
