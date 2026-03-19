# create the directory
dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

# unzip file
zip_file <- file.choose()
utils::unzip(zip_file, exdir = base_dir)

# get files within the directory
list.files(base_dir, recursive = TRUE)
files <- list.files(base_dir, recursive = TRUE, full.names = TRUE)

# name treescan executable
exe <- files[grepl("\\.exe$", files, ignore.case = TRUE)]

# locate treescan
treescan_bin <- paste0(base_dir, "/treescan64.exe")
