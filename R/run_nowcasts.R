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
#' @param config A [nowcast_config()] or [gam_nowcast_config()] object to use
#'   as the default for all locations. Locations are fitted independently;
#'   use [nowcast()] directly for cross-location pooling.
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
#'   `purrr::in_parallel()`. Requires the `mirai` package. Default is `FALSE`.
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
#' @importFrom rlang .data .env
#' @importFrom cli cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom cli cli_alert_success cli_alert_warning cli_alert_info
#' @importFrom baselinenowcast as_reporting_triangle truncate_to_delay
#' @importFrom baselinenowcast baselinenowcast
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

  # Determine nowcast date
  if (is.null(nowcast_date)) {
    nowcast_date <- max(data$report_date)
  }

  # Workers must load the installed package, including its S3 methods.
  if (parallel) {
    rlang::check_installed(
      c("mirai", "carrier"),
      reason = "for parallel processing"
    )
    if (is.null(mirai::info())) {
      mirai::daemons(min(2L, length(locs_to_process)), dispatcher = TRUE)
      on.exit(mirai::daemons(0), add = TRUE)
    }
    paths <- .libPaths()
    mirai::everywhere(.libPaths(paths), paths = paths)
    cli::cli_alert_info("Using parallel processing via {.pkg mirai}")
  }

  # Define the worker function
  nowcast_one <- function(loc) {
    tryCatch(
      {
        loc_config <- get_config_for_loc(loc)
        run_single_nowcast(
          data = data,
          location = loc,
          config = loc_config,
          nowcast_date = nowcast_date,
          cumulative = cumulative
        )
      },
      error = function(e) {
        cli::cli_alert_warning(
          "Failed to process {.val {loc}}: {conditionMessage(e)}"
        )
        NULL
      }
    )
  }

  # Send explicit inputs to workers rather than a development namespace.
  if (parallel) {
    raw_results <- purrr::map(
      locs_to_process,
      purrr::in_parallel(
        function(loc) {
          loc_config <- if (
            !is.null(location_configs) && loc %in% names(location_configs)
          ) {
            location_configs[[loc]]
          } else {
            config
          }
          tryCatch(
            nowcastNHSN::run_single_nowcast(
              data,
              loc,
              loc_config,
              nowcast_date,
              cumulative = cumulative
            ),
            error = function(e) {
              structure(
                list(message = conditionMessage(e)),
                class = "nowcast_worker_error"
              )
            }
          )
        },
        config = config,
        location_configs = location_configs,
        data = data,
        nowcast_date = nowcast_date,
        cumulative = cumulative
      )
    )
    for (i in seq_along(raw_results)) {
      if (inherits(raw_results[[i]], "nowcast_worker_error")) {
        cli::cli_alert_warning(
          "Failed to process {.val {locs_to_process[i]}}: {raw_results[[i]]$message}"
        )
        raw_results[i] <- list(NULL)
      }
    }
  } else {
    raw_results <- purrr::map(locs_to_process, nowcast_one, .progress = TRUE)
  }

  # Identify failures by checking for NULLs
  failed_locations <- locs_to_process[purrr::map_lgl(raw_results, is.null)]
  successful <- Filter(Negate(is.null), raw_results)
  results <- if (length(successful)) {
    restore_nowcast_output(do.call(rbind, lapply(successful, as.data.frame)))
  } else {
    restore_nowcast_output(data.frame(
      pred_count = numeric(),
      reference_date = as.Date(character()),
      draw = integer(),
      nowcast = logical(),
      location = character()
    ))
  }

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
#' Runs the configured model for a single geographic location and signal.
#' This is the inner function called by [run_state_nowcasts()] and can
#' also be used directly for Azure single-state entrypoint scripts.
#'
#' This function prepares a reporting triangle and calls [nowcast()].
#' Each call fits one location independently; use [nowcast()] directly for
#' pooled or stratified models.
#'
#' @param data A data frame of reporting data (cumulative or incremental).
#'   Cumulative data is converted automatically when `cumulative = TRUE`.
#' @param location Character. The location code to process.
#' @param config A [nowcast_config()] or [gam_nowcast_config()] object.
#' @param nowcast_date Date. The nowcast date.
#' @param cumulative Logical. If `TRUE` (default), converts cumulative counts
#'   to incremental. Set to `FALSE` if data is already incremental.
#'
#' @returns A data frame of nowcast results for the single location.
#'
#' @importFrom dplyr filter mutate
#' @importFrom baselinenowcast as_reporting_triangle truncate_to_delay
#' @importFrom baselinenowcast baselinenowcast
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
  # Preserve legacy single-location calls using an unclassed baseline list.
  if (is.list(config) && identical(class(config), "list")) {
    class(config) <- c("nowcast_config", "list")
  }
  validate_nowcast_config(config)
  checkmate::assert_character(location, len = 1L, any.missing = FALSE)
  checkmate::assert_date(nowcast_date, len = 1L, any.missing = FALSE)
  validate_reporting_data(
    data,
    c(
      "reference_date",
      "report_date",
      "count",
      "location",
      "signal"
    )
  )
  loc_data <- data |>
    dplyr::filter(
      .data$location == .env$location,
      .data$report_date <= .env$nowcast_date
    )
  if (!nrow(loc_data)) {
    cli::cli_abort("No data available for location {.val {location}}")
  }
  if (anyNA(loc_data$signal) || length(unique(loc_data$signal)) != 1L) {
    cli::cli_abort("Location runners require exactly one signal per location.")
  }
  triangles <- as_reporting_triangles(
    loc_data,
    strata = "location",
    nowcast_date = nowcast_date,
    cumulative = cumulative
  )
  nowcast(triangles, config)
}
