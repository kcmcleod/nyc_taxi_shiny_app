fn_test_lookup_join <- function(all_df_names, field_name) {
  
  if(! field_name %in% all_df_names) {
    log_error(".... ",  field_name, " join not complete")
    return(FALSE)
  }
  return(TRUE)
}