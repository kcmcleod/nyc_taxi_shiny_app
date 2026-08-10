# ==============================================================================
# Script to bundle Shiny app code for LLM upload
# ==============================================================================

bundle_shiny_code <- function(output_filename = paste0(
                                format(Sys.time(), "%Y%m%d:%H%M"), "_shiny_app_code_bundle.txt"
                              )) {
  # 1. Define the root directory (assuming script is run from project root)
  app_dir <- getwd()

  # 2. Find all relevant code files
  # This regex captures .R, .js, .css, .yml, and .yaml files
  all_files <- list.files(
    path = app_dir,
    pattern = "\\.(R|r|js|css|yml|yaml)$",
    recursive = TRUE,
    full.names = TRUE
  )

  # 3. Filter out directories that contain bloat or shouldn't be shared
  # e.g., git history, cache directories, or R project system files
  ignore_patterns <- c("/\\.git/", "/shared_app_cache/", "/\\.Rproj\\.user/", "/renv/")

  for (pattern in ignore_patterns) {
    all_files <- all_files[!grepl(pattern, all_files)]
  }

  # 4. Create and open the output text file
  message("Bundling ", length(all_files), " files into ", output_filename, "...")
  out_conn <- file(output_filename, "w")

  # 5. Loop through each file and append its contents
  for (file_path in all_files) {
    # Strip the leading "./" from the path for cleaner reading
    clean_path <- sub("^\\./", "", file_path)

    # Create a clear visual separator and header for the file name/path
    header <- c(
      "",
      "################################################################################",
      paste0("### FILE: ", clean_path),
      "################################################################################",
      ""
    )

    # Write the header
    writeLines(header, out_conn)

    # Read the file contents (suppressing warnings for incomplete final lines)
    content <- readLines(file_path, warn = FALSE)

    # Write the contents below the header
    writeLines(content, out_conn)
  }

  # 6. Close the connection
  close(out_conn)
  message("Done! You can now upload ", output_filename, ".")
}

# Execute the function
bundle_shiny_code()
