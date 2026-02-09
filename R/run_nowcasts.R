#' Run nowcasts for multiple states/locations
#'
#' @description
#' Orchestrates nowcast generation for multiple geographic locations,
#' with optional parallel processing. Wraps each location in error handling
#' to continue processing even if individual locations fail.
#'
#' This function handles the full pipeline from cumulative reporting data
#' (as returned by [fetch_reporting_data()]) to nowcast results, including
#' conversion to incremental counts internally.
#'
#' @param data A data frame of reporting data as returned by
#'   [fetch_reporting_data()], containing columns `reference_date`,
#'   `report_date`, `count`, `location`, and `signal`.
#' @param config A `nowcast_config` object to use as the default for all
#'   locations.
#' @param location_configs Optional named list of `nowcast_config` objects
#'   keyed by location code (e.g., `list(ca = config_ca, ny = config_ny)`).
#'   These override the default `config` for specific locations.
#' @param locations Character vector. Either `"all"` for all available
#'   locations in the data, or a character vector of specific location codes
#'   (e.g., `c("ca", "ny")`). Default is `"all"`.
#' @param nowcast_date Date. The date to use as the nowcast date. Defaults to
#'   the maximum report_date in the data.
#' @param cumulative Logical. If `TRUE` (the default), the input data is
#'   assumed to be cumulative counts (as returned by [fetch_reporting_data()])
#'   and will be converted to incremental counts using
#'   [cumulative_to_incremental()]. Set to `FALSE` if the data is already
#'   incremental.
#' @param parallel Logical. Whether to use parallel processing via
#'   `furrr::future_map()`. Default is `FALSE`.
#'
#' @returns A data frame with columns:
#'   - `reference_date`: The reference date being nowcasted
#'   - `location`: Geographic identifier
#'   - `pred_count`: Predicted count (one row per draw)
#'   - `draw`: Draw number
#'   - `output_type`: Type of output ("samples" or "point")
#'
#'   The returned data frame has an attribute `failed_locations` containing
#'   a character vector of any locations that failed to process.
#'
#' @importFrom purrr map list_rbind
#' @importFrom dplyr filter mutate
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#'   cli_alert_success cli_alert_warning cli_alert_info
#' @importFrom baselinenowcast as_reporting_triangle truncate_to_delay
#'   baselinenowcast
#' @export
#'
#' @examples
#' \dontrun{
#' # Fetch cumulative data for states
#' source <- delphi_epidata_source(target = "covid", geo_types = "state")
#' data <- fetch_reporting_data(source, reference_dates, report_dates)
#'
#' # Single config for all locations
#' config <- nowcast_config()
#' results <- run_state_nowcasts(data, config, locations = c("ca", "ny"))
#'
#' # Override config for specific locations
#' config <- nowcast_config(draws = 500)
#' location_configs <- list(
#'   ca = nowcast_config(max_delay = 5, draws = 500),
#'   ny = nowcast_config(max_delay = 7, uncertainty_model = "skellam", draws = 500)
#' )
#' results <- run_state_nowcasts(
#'   data, config,
#'   location_configs = location_configs,
#'   locations = c("ca", "ny", "tx")
#' )
#' # ca and ny use their specific configs, tx uses the default
#'
#' # Check for failures
#' attr(results, "failed_locations")
#' }
run_state_nowcasts <- function(
  data,
  config,
  location_configs = NULL,
  locations = "all",
  nowcast_date = NULL,
  cumulative = TRUE,
  parallel = FALSE
) {
  # Validate default config
  validate_nowcast_config(config)

  # Validate location-specific configs if provided
  if (!is.null(location_configs)) {
    if (!is.list(location_configs) || is.null(names(location_configs))) {
      cli::cli_abort(c(
        "Invalid {.arg location_configs} argument.",
        x = "Expected a named list of configs.",
        i = "Provide a named list like {.code list(ca = config_ca, ny = config_ny)}."
      ))
    }
    for (loc_name in names(location_configs)) {
      validate_nowcast_config(location_configs[[loc_name]])
    }
  }

  # Helper to get config for a location (override or default)
  get_config_for_loc <- function(loc) {
    if (!is.null(location_configs) && loc %in% names(location_configs)) {
      location_configs[[loc]]
    } else {
      config
    }
  }

  # Determine locations to process
  available_locations <- unique(data$location)

  if (length(locations) == 1 && locations == "all") {
    locs_to_process <- available_locations
  } else {
    locs_to_process <- locations
    missing <- setdiff(locations, available_locations)
    if (length(missing) > 0) {
      cli::cli_alert_warning(
        "Locations not found in data: {.val {missing}}"
      )
      locs_to_process <- intersect(locs_to_process, available_locations)
    }
  }

  if (length(locs_to_process) == 0) {
    cli::cli_abort("No valid locations to process.")
  }

  cli::cli_alert_info("Processing {length(locs_to_process)} location(s)")

  # Convert cumulative to incremental counts if needed
  if (cumulative) {
    incremental_data <- cumulative_to_incremental(
      data,
      group_cols = c("reference_date", "location", "signal")
    )
  } else {
    incremental_data <- data
  }

  # Determine nowcast date
  if (is.null(nowcast_date)) {
    nowcast_date <- max(data$report_date)
  }

  # Choose mapping function based on parallel setting
  if (parallel) {
    if (!requireNamespace("furrr", quietly = TRUE)) {
      cli::cli_abort(c(
        "Package {.pkg furrr} is required for parallel processing.",
        i = "Install it with {.code install.packages('furrr')}"
      ))
    }
    map_fn <- furrr::future_map
    cli::cli_alert_info("Using parallel processing via {.pkg furrr}")
  } else {
    map_fn <- purrr::map
  }

  # Process each location
  failed_locations <- character(0)

  results <- map_fn(
    locs_to_process,
    function(loc) {
      tryCatch(
        {
          loc_config <- get_config_for_loc(loc)
          run_single_nowcast(
            data = incremental_data,
            location = loc,
            config = loc_config,
            nowcast_date = nowcast_date,
            cumulative = FALSE # Already converted above
          )
        },
        error = function(e) {
          cli::cli_alert_warning(
            "Failed to process {.val {loc}}: {conditionMessage(e)}"
          )
          # Store failure but return NULL
          failed_locations <<- c(failed_locations, loc)
          NULL
        }
      )
    },
    .progress = !parallel
  ) |>
    purrr::list_rbind()

  # Attach failed locations as attribute
  attr(results, "failed_locations") <- failed_locations

  if (length(failed_locations) > 0) {
    cli::cli_alert_warning(
      "{length(failed_locations)} location(s) failed: {.val {failed_locations}}"
    )
  }

  n_successful <- length(locs_to_process) - length(failed_locations)
  cli::cli_alert_success(
    "Completed nowcasts for {n_successful} location(s)"
  )

  results
}

#' Run nowcast for a single location
#'
#' @description
#' Runs the baselinenowcast model for a single geographic location.
#' This is the inner function called by [run_state_nowcasts()] and can
#' also be used directly for Azure single-state entrypoint scripts.
#'
#' When called directly (not via [run_state_nowcasts()]), this function
#' converts cumulative counts to incremental counts internally.
#'
#' @param data A data frame of reporting data (cumulative or incremental).
#'   When called via [run_state_nowcasts()], data is already incremental.
#'   When called directly, cumulative data is converted automatically.
#' @param location Character. The location code to process.
#' @param config A `nowcast_config` object.
#' @param nowcast_date Date. The nowcast date.
#' @param cumulative Logical. If `TRUE` (default), converts cumulative counts

#'   to incremental. Set to `FALSE` if data is already incremental.
#'
#' @returns A data frame of nowcast results for the single location.
#'
#' @importFrom dplyr filter mutate
#' @importFrom baselinenowcast as_reporting_triangle truncate_to_delay
#'   baselinenowcast
#' @export
#'
#' @examples
#' \dontrun{
#' config <- nowcast_config(uncertainty_model = "normal")
#' # Pass cumulative data directly - conversion happens internally
#' result <- run_single_nowcast(data, "ca", config, as.Date("2025-01-04"))
#' }
run_single_nowcast <- function(
  data,
  location,
  config,
  nowcast_date,
  cumulative = TRUE
) {
  # Get uncertainty functions from config
  uncertainty_fns <- get_uncertainty_fns(config$uncertainty_model)

  # Filter to location
  loc_data <- data |>
    dplyr::filter(.data$location == .env$location) |>
    dplyr::filter(.data$report_date <= nowcast_date)

  if (nrow(loc_data) == 0) {
    cli::cli_abort("No data available for location {.val {location}}")
  }

  # Convert cumulative to incremental if needed
  if (cumulative) {
    loc_data <- cumulative_to_incremental(
      loc_data,
      group_cols = c("reference_date", "location", "signal")
    )
  }

  # Create reporting triangle
  reporting_triangle <- loc_data |>
    baselinenowcast::as_reporting_triangle(delays_unit = "weeks") |>
    baselinenowcast::truncate_to_delay(max_delay = config$max_delay)

  # Run nowcast
  nowcast_result <- baselinenowcast::baselinenowcast(
    reporting_triangle,
    scale_factor = config$scale_factor,
    prop_delay = config$prop_delay,
    draws = config$draws,
    uncertainty_model = uncertainty_fns$uncertainty_model,
    uncertainty_sampler = uncertainty_fns$uncertainty_sampler
  )

  # Add location column
  nowcast_result <- nowcast_result |>
    dplyr::mutate(location = location)

  nowcast_result
}
