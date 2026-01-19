#' Validate NHSN signal and geo_type parameters
#'
#' @description
#' Internal helper function to validate `signal` and `geo_type` parameters for
#' target nowcast NHSN data fetching functions. Note that the preliminary
#' signals are the targets for nowcasting in this project due to Wednesday
#' releases.
#'
#' @param signal Character string to validate against target NHSN signals
#' @param geo_type Character string to validate against valid geographic types
#'
#' @return NULL (called for side effects - throws error if invalid)
#' @keywords internal
#' @noRd
validate_nhsn_params <- function(signal, geo_type) {
  valid_signals <- c(
    "confirmed_admissions_covid_ew_prelim",
    "confirmed_admissions_flu_ew_prelim",
    "confirmed_admissions_rsv_ew_prelim"
  )

  if (!signal %in% valid_signals) {
    rlang::abort(
      paste0(
        "Not a target signal. Must be one of: ",
        paste(valid_signals, collapse = ", ")
      )
    )
  }

  valid_geo_types <- c("state", "hhs", "nation")
  if (!geo_type %in% valid_geo_types) {
    rlang::abort(
      paste0(
        "Invalid geo_type. Must be one of: ",
        paste(valid_geo_types, collapse = ", ")
      )
    )
  }

  invisible(NULL)
}


#' Fetch NHSN hospital admissions data
#'
#' @description
#' Fetches target NHSN (National Healthcare Safety Network) hospital respiratory
#' admissions data using the `epidatr` R client for the Delphi Epidata API. This
#' is just a closure around `epidatr::pub_covidcast()` with validation for the
#' target signals and parameters used in this nowcasting and checking that the
#' output is non-empty.
#' NB:
#' - For `time_values` and `issues`, you can pass an `epidatr::timeset` object
#' however this source only supports epiweeks (not dates).
#' - Output dates are Saturday week-endings, matching both MMWR epiweeks and
#' forecast hub conventions.
#'
#' @param signal Character string specifying the NHSN signal to fetch.
#'   Default is `"confirmed_admissions_covid_ew_prelim"`.
#' @param geo_type Character string specifying geographic resolution.
#'   Options: `"state"`, `"hhs"`, `"nation"`. Default is `"state"`.
#' @param geo_values Character vector of geography codes or `"*"` for all.
#'   Default is `"*"`.
#' @param time_values A timeset object (e.g., `epirange(202401, 202404)`)
#'   or `"*"` for all available dates. Default is `"*"`.
#' @param issues A timeset object for specific issue dates, or `"*"` for most
#'   recent. Default is `"*"`.
#' @param fetch_args List of fetch arguments passed to `epidatr::pub_covidcast()`.
#'   Defaults to `fetch_args_list(reference_week_day = 7)` to return Saturday
#'   dates matching MMWR epiweek and forecast hub conventions.
#' @param ... Additional arguments passed to `epidatr::pub_covidcast()`.
#' @return A tibble with columns including:
#'   - `geo_value`: Geographic identifier
#'   - `time_value`: Reference date (week-ending Saturday)
#'   - `value`: Number of confirmed admissions
#'   - `issue`: Issue date (week-ending Saturday of the epiweek when data was
#' published)
#'   - `lag`: Days between reference date and issue date
#'   - `signal`: Signal name
#'   - Additional metadata columns from `epidatr::pub_covidcast()`
#' @concept data_fetching
#' @export
fetch_nhsn_data <- function(
  signal = "confirmed_admissions_covid_ew_prelim",
  geo_type = "state",
  geo_values = "*",
  time_values = "*",
  issues = "*",
  fetch_args = epidatr::fetch_args_list(reference_week_day = 7),
  ...
) {
  # Input validation
  validate_nhsn_params(signal, geo_type)

  # Fetch data using epidatr
  nhsn_data <- epidatr::pub_covidcast(
    source = "nhsn",
    signals = signal,
    geo_type = geo_type,
    time_type = "week",
    time_values = time_values,
    geo_values = geo_values,
    issues = issues,
    fetch_args = fetch_args,
    ...
  )

  # Output validation
  if (nrow(nhsn_data) == 0) {
    rlang::warn("No data returned for the specified parameters")
  }
  nhsn_data
}


#' Fetch NHSN data formatted for reporting triangle construction
#'
#' @description
#' Wrapper around [fetch_nhsn_data()] that reshapes and converts the output
#' to a long format suitable for reporting triangle construction.
#' Uses `reference_week_day = 7` by default to return Saturday week-ending
#' dates, matching both MMWR epiweek and forecast hub conventions.
#'
#' @param signal Character string specifying the NHSN signal to fetch.
#'   Default is `"confirmed_admissions_covid_ew_prelim"`.
#'
#' @param geo_type Character string specifying geographic resolution.
#'   Options: `"state"`, `"hhs"`, `"nation"`. Default is `"state"`.
#'
#' @param geo_values Character vector of geography codes,
#'   or `"*"` for all geographies. Default is `"*"`.
#'
#' @param reference_dates A timeset object (e.g., `epirange(202401, 202404)`)
#'   or `"*"` for all available dates. Default is `"*"`.
#'
#' @param report_dates A timeset object for specific report dates, or `"*"`
#'   for most all. Default is `"*"`.
#'
#' @param ... Additional arguments passed to `epidatr::pub_covidcast()` via
#'   [fetch_nhsn_data()], such as `fetch_args` for controlling caching behavior.
#'
#' @return A data frame in long format suitable for reporting triangle construction
#'   with columns:
#'   - `reference_date`: Date of the event (epiweek-ending Saturday)
#'   - `report_date`: Date when data was reported (epiweek-ending Saturday)
#'   - `count`: Cumulative confirmed admissions reported as of that report date
#'   - `location`: Geographic identifier
#'   - `signal`: Signal name
#'
#' @concept data_fetching
#' The epidatr API returns cumulative counts (total reported as of each issue
#' date). Use [cumulative_to_incremental()] to convert to incremental counts
#' if needed for `baselinenowcast::as_reporting_triangle()`.
#'
#' @export
fetch_reporting_data_epidatr <- function(
  signal = "confirmed_admissions_covid_ew_prelim",
  geo_type = "state",
  geo_values = "*",
  reference_dates = "*",
  report_dates = "*",
  ...
) {
  results <- fetch_nhsn_data(
    signal = signal,
    geo_type = geo_type,
    geo_values = geo_values,
    time_values = reference_dates,
    issues = report_dates,
    ...
  )

  # Handle empty results - return empty data frame with expected columns
  if (nrow(results) == 0) {
    return(
      data.frame(
        reference_date = as.Date(character()),
        report_date = as.Date(character()),
        count = numeric(),
        location = character(),
        signal = character()
      )
    )
  }

  # Format for baselinenowcast
  results <- results |>
    dplyr::select(
      reference_date = "time_value",
      report_date = "issue",
      count = "value",
      location = "geo_value",
      "signal"
    ) |>
    dplyr::arrange(.data$reference_date, .data$report_date)

  results
}
