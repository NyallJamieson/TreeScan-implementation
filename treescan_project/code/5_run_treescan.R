if (!isTRUE(server)){
  
  for (LAG in initial_lags){
    
    # Locate parameter file
    prm <- paste0(parent_dir, "/params/Parameter_File_lag", LAG, ".prm")
    
    # Run treescan
    out <- system2(
      treescan_bin,
      args = shQuote(prm),
      stdout = "",
      stderr = ""
    )
  }
  
} else {
  
  for (LAG in initial_lags){
    
    prm <- normalizePath(path.expand(paste0(parent_dir, "/params/Parameter_File_lag", LAG, ".prm")), mustWork = TRUE)
    
    print(treescan_bin)
    print(file.exists(treescan_bin))
    print(prm)
    print(file.exists(prm))
    
    Sys.chmod(treescan_bin, mode = "0755")
    file.info(treescan_bin)$mode
    
    # Run treescan
    out <- system2(
      treescan_bin,
      args = shQuote(prm),
      stdout = "",
      stderr = ""
    )
    
  }
}
