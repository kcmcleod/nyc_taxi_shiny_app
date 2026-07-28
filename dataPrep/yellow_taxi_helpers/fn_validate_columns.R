# Helper function to check missing or extra columns for a given data frame
fn_validate_columns <- function(df, expected_cols, df_name) {

  # 1. Input validation
  if (!is.data.frame(df)) {
    log_error("... invalid input for {df_name}: expected a data frame or tibble.")
    return(FALSE)
  }
  
  if (missing(expected_cols) || is.null(expected_cols)) {
    log_error(".... expected columns vector is missing or null for {df_name}.")
    return(FALSE)
  }
  
  if (missing(df_name) || !is.character(df_name) || length(df_name) != 1) {
    df_name <- "Data frame"
  }
  
  # 2. look for missing/extra cols
  missing_cols <- setdiff(expected_cols, names(df))
  extra_cols <- setdiff(names(df), expected_cols)
  
  is_valid <- TRUE
  
  if (length(missing_cols) > 0) {
    log_error("......", df_name, " is missing: ", paste0(missing_cols, collapse = ", "))
    is_valid <- FALSE
  }
  
  if (length(extra_cols) > 0) {
    log_error("......", df_name, " has extra cols: ", paste0(extra_cols, collapse = ", "))
    is_valid <- FALSE
  }
  
  return(is_valid)
}