# create the directory
dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

# locate treescan
if (!isTRUE(server)){
  treescan_bin <- normalizePath(path.expand(paste0(parent_dir, "/TS_windows/treescan64.exe")), mustWork = TRUE)
} else {
  treescan_bin <- normalizePath(path.expand(paste0(parent_dir, "/TS_linux/treescan64")), mustWork = TRUE)
}

