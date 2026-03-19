# Locate parameter file
prm <- "C:/Users/nj7786/Documents/treescan_project/params/Parameter_File.prm"

# Run treescan
out <- system2(
  treescan_bin,
  args = shQuote(prm),
  stdout = "",
  stderr = ""
)

cat(out, sep = "\n")