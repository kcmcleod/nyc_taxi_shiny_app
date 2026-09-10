library(covr)

# 1. Run your existing coverage logic
cov_results <- covr::file_coverage(
  source_files = core_sources,
  test_files = unit_tests
)

overall_pct <- covr::percent_coverage(cov_results)

# 2. Check if running inside GitHub Actions
step_summary_file <- Sys.getenv("GITHUB_STEP_SUMMARY")

if (nzchar(step_summary_file)) {
  # Build simple Markdown text
  status_icon <- ifelse(overall_pct >= 90, "🟢", "🔴")

  summary_lines <- c(
    "### 📊 R Shiny ETL Coverage Report",
    "",
    sprintf("status **Overall Code Coverage:** `%.2f%%` %s", overall_pct, status_icon),
    "",
    "| Check | Status |",
    "| :--- | :---: |",
    sprintf("| **Aggregations & Temporal Tiers (`T2-T4`)** | %s |", status_icon),
    sprintf("| **Lookup Joins & Enrichment (`fn_perform_joins`)** | %s |", status_icon),
    ""
  )

  # Append to the GitHub summary file
  cat(paste(summary_lines, collapse = "\n"), file = step_summary_file, append = TRUE)
}

# Print standard report to console as normal
covr::report(cov_results)
