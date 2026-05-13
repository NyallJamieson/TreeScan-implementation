if (isTRUE(subset)){
  dir.create(paste0(parent_dir, "/raw_data_subset"))
  
  subset_end_date <- as.Date(final_date)
  subset_start_date <- subset_end_date %m-% months(16)
  
  subset_target_dates <- seq.Date(subset_start_date, subset_end_date, by = "day")
  
  for (i in list.files(paste0(parent_dir, "/raw_data"), pattern = "\\.csv$", full.names = TRUE)){
    date <- as.Date(sub(".*NSSP_data_(\\d{4}-\\d{2}-\\d{2})_to_.*", "\\1", basename(i)))
    
    if (date %in% subset_target_dates){
      print(paste0("Reducing file for date ", date))
      
      A <- read.csv(i)
      B <- A[which(A$Region %in% which_subregion), ]
      
      i_subset <- sub("/raw_data/", "/raw_data_subset/", i)
      write.csv(B, i_subset, row.names = FALSE)
    }
  }
}
  

