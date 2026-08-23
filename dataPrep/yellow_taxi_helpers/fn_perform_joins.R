fn_perform_joins <- function(df, zone_lookups, rate_lookups,
                             payment_lookups, vendor_lookups) {
  if (!is.data.frame(df) || nrow(df) == 0) {
    log_error(".... Input 'df' must be a non-empty data frame.")
    return(NULL)
  }

  enriched_df <- fn_perform_lookup(df, rate_lookups, "RatecodeID", "RatecodeID", "Rate", "rate") |>
    fn_perform_lookup(zone_lookups, c("PULocationID" = "LocationID"), "PULocationID", "PULocation", "PU",
      post_process_fn = function(d) dplyr::rename(d, PULocation = Borough)
    ) |>
    fn_perform_lookup(zone_lookups, c("DOLocationID" = "LocationID"), "DOLocationID", "DOLocation", "DO",
      post_process_fn = function(d) dplyr::rename(d, DOLocation = Borough)
    ) |>
    fn_perform_lookup(payment_lookups, "payment_type", "payment_type", "payment_type", "payment",
      post_process_fn = function(d) dplyr::mutate(d, payment_type = payment_class)
    ) |>
    dplyr::select(-c(payment_class)) |>
    fn_perform_lookup(vendor_lookups, "VendorID", "VendorID", "Vendor", "vendor")

  # Ensure expected columns exist after joins
  all_names <- names(enriched_df)
  expected_names <- c("PULocation", "DOLocation", "Rate", "Vendor")
  join_results <- sapply(expected_names, function(field) fn_test_lookup_join(all_names, field))

  if (any(!join_results)) {
    log_error("STOPPING: One or more lookup joins failed during enrichment.")
    stop("Lookup join verification failed", call. = FALSE)
  }

  return(enriched_df)
}
