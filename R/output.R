#' Summarize nowcast draws into quantiles
#'
#' @description
#' Takes nowcast output with multiple draws and summarizes into
#' prediction intervals per location and reference date in forecast hub format.
#'
#' @param nowcast_results A data frame as returned by [run_state_nowcasts()]
#'   or [run_single_nowcast()], containing columns `reference_date`, `location`,
#'   `pred_count`, and `draw`.
#' @param quantiles Numeric vector of quantiles to compute. Default uses
#'   forecast hub standard: `c(0.01, 0.025, seq(0.05, 0.95, by = 0.05),
#' 0.975, 0.99)`.
#'
#' @returns A data frame in forecast hub format with columns:
#'   - `reference_date`: The reference date
#'   - `location`: Geographic identifier
#'   - `output_type`: Always "quantile"
#'   - `output_type_id`: Quantile level as character (e.g., "0.5")
#'   - `value`: The prediction value
#'
#' @importFrom dplyr group_by reframe mutate arrange
#' @importFrom stats quantile
#' @export
#'
#' @examples
#' \dontrun{
#' results <- run_state_nowcasts(data, config)
#' summary <- summarize_nowcast(results)
#' }
summarize_nowcast <- function(
  nowcast_results,
  quantiles = c(0.01, 0.025, seq(0.05, 0.95, by = 0.05), 0.975, 0.99)
) {
  nowcast_results |>
    dplyr::group_by(.data$reference_date, .data$location) |>
    dplyr::reframe(
      output_type = "quantile",
      output_type_id = as.character(quantiles),
      value = quantile(.data$pred_count, probs = quantiles)
    ) |>
    dplyr::arrange(.data$reference_date, .data$location, .data$output_type_id)
}

#' Write nowcast results to partitioned parquet files
#'
#' @description
#' Writes summarized nowcast results to a partitioned parquet dataset,
#' partitioned by location. This enables efficient querying of results
#' for specific states using `arrow::open_dataset()`.
#'
#' @param nowcast_results A data frame as returned by [run_state_nowcasts()]
#'   or [run_single_nowcast()].
#' @param output_dir Character. Directory path for output. Will be created
#'   if it doesn't exist.
#' @param summarize Logical. If `TRUE`, summarize draws into
#'   quantiles before writing. If `FALSE` (default), write raw draws.
#' @param overwrite Logical. If `TRUE`, overwrite existing partitions.
#'   Default is `TRUE`.
#'
#' @returns Invisibly returns the path to the output directory.
#'
#' @importFrom cli cli_alert_success cli_alert_info cli_abort
#' @export
#'
#' @examples
#' \dontrun{
#' results <- run_state_nowcasts(data, config)
#'
#' # Write summarized results
#' write_nowcast_parquet(results, "output/nowcasts", summarize = TRUE)
#'
#' # Write raw draws
#' write_nowcast_parquet(results, "output/nowcasts_draws", summarize = FALSE)
#'
#' # Read back with arrow
#' library(arrow)
#' ds <- open_dataset("output/nowcasts")
#' ds |> filter(location == "ca") |> collect()
#' }
write_nowcast_parquet <- function(
  nowcast_results,
  output_dir,
  summarize = FALSE,
  overwrite = TRUE
) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg arrow} is required for parquet output.",
      i = "Install it with {.code install.packages('arrow')}"
    ))
  }

  # Summarize if requested
  if (summarize) {
    cli::cli_alert_info("Summarizing draws into quantiles")
    data_to_write <- summarize_nowcast(nowcast_results)
  } else {
    data_to_write <- nowcast_results
  }

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Write partitioned dataset
  arrow::write_dataset(
    data_to_write,
    path = output_dir,
    format = "parquet",
    partitioning = "location",
    existing_data_behavior = if (overwrite) "overwrite" else "error"
  )

  n_locations <- length(unique(data_to_write$location))
  cli::cli_alert_success(
    "Wrote partitioned parquet to {.path {output_dir}} ({n_locations} locations)"
  )

  invisible(output_dir)
}

#' Read nowcast results from partitioned parquet
#'
#' @description
#' Convenience function to read back nowcast results written by
#' [write_nowcast_parquet()].
#'
#' @param output_dir Character. Directory path containing parquet partitions.
#' @param locations Optional character vector of locations to read.
#'   If `NULL`, reads all locations.
#'
#' @returns A data frame of nowcast results.
#'
#' @importFrom cli cli_abort
#' @export
#'
#' @examples
#' \dontrun{
#' # Read all locations
#' results <- read_nowcast_parquet("output/nowcasts")
#'
#' # Read specific locations
#' results <- read_nowcast_parquet("output/nowcasts", locations = c("ca", "ny"))
#' }
read_nowcast_parquet <- function(output_dir, locations = NULL) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg arrow} is required for parquet output.",
      i = "Install it with {.code install.packages('arrow')}"
    ))
  }

  ds <- arrow::open_dataset(output_dir)

  if (!is.null(locations)) {
    ds <- ds |>
      dplyr::filter(.data$location %in% locations)
  }

  ds |>
    dplyr::collect()
}
