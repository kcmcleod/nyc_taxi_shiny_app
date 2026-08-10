# Helper function to handle join and validation safely
fn_perform_lookup <- function(df, lookup_df, join_by_cols, check_col_pre,
                              check_col_post, label, post_process_fn = NULL) {
  # data checks
  if (!is.data.frame(df)) {
    log_error("Invalid dataframe passed to perform_lookup for {label}.")
    stop("Input 'df' must be a data frame.")
  }

  # actual joins
  og_na_count <- sum(is.na(df[[check_col_pre]]))

  # Perform join
  df <- left_join(df, lookup_df, by = join_by_cols)

  # Optional post-processing (e.g., renaming or overriding columns)
  if (!is.null(post_process_fn)) {
    df <- post_process_fn(df)
  }

  new_na_count <- sum(is.na(df[[check_col_post]]))

  # Log results safely
  if (new_na_count == og_na_count) {
    log_info(".... no new NAs introduced in {label} data. We have: {og_na_count}")
  } else {
    log_error(".... new NAs introduced in {label} data. We have {og_na_count} vs {new_na_count}")
  }

  return(df)
}
