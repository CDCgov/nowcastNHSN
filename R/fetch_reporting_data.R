#' Convert dates to epirange format
#'
#' @param x Date vector or epirange object
#' @return epirange object
#' @noRd
as_epirange <- function(x) {
  if (inherits(x, "Date")) {
    validate_all_saturdays(x)
    x <- saturdays_to_epirange(x)
  }
  x
}

#' Create an epidata data source object
#'
#' @param signal Character, epidata signal name (e.g.,
#' "confirmed_admissions_covid_ew_prelim")
#' @param geo_types Character vector, geographic types to query (e.g., "state",
#' "nation")
#' @return A source object of class "epidata_source"
#' @export
epidata_source <- function(
  signal,
  geo_types = c("state", "nation")
) {
  structure(
    list(
      signal = signal,
      geo_types = geo_types
    ),
    class = c("epidata_source", "reporting_source")
  )
}

#' Fetch reporting data from a source object
#'
#' Generic function to fetch reporting triangle data using S3 method dispatch.
#'
#' @param source A source object created by [epidata_source()] or [github_source()]
#' @param reference_dates Date vector or epirange of reference dates
#' @param report_dates Date vector or epirange of report dates
#' @param locations Character vector of locations
#' @param ... Additional arguments passed to methods
#' @return data.frame with columns: reference_date, report_date, location, count, signal
#' @export
fetch_reporting_data <- function(
  source,
  reference_dates,
  report_dates,
  locations,
  ...
) {
  UseMethod("fetch_reporting_data")
}

#' Fetch reporting data from epidata
#'
#' @param source An epidata_source object
#' @param reference_dates Date vector or epirange of reference dates
#' @param report_dates Date vector or epirange of report dates
#' @param locations Character vector of locations ("*" for all)
#' @param ... Additional arguments (unused)
#' @return data.frame with reporting triangle data
#' @export
fetch_reporting_data.epidata_source <- function(
  source,
  reference_dates,
  report_dates,
  locations = "*",
  ...
) {
  # Convert to epirange if Dates are provided, otherwise use as-is
  report_dates <- as_epirange(report_dates)
  reference_dates <- as_epirange(reference_dates)

  purrr::map_dfr(source$geo_types, function(geo_type) {
    fetch_reporting_data_epidatr(
      signal = source$signal,
      geo_type = geo_type,
      geo_values = locations,
      reference_dates = reference_dates,
      report_dates = report_dates
    )
  })
}

#' Default method for unsupported source types
#' @export
fetch_reporting_data.default <- function(source, ...) {
  rlang::abort(sprintf(
    "Don't know how to fetch data from source of class '%s'",
    paste(class(source), collapse = "', '")
  ))
}
