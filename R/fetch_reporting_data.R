#' Convert dates to epirange format
#'
#' @param x Date vector, epirange object, or wildcard character ("*")
#' @return epirange object or wildcard
#' @noRd
to_epirange <- function(x) {
  # Pass through EpiRange and wildcard unchanged
  if (inherits(x, "EpiRange") || identical(x, "*")) {
    return(x)
  }

  # Convert Date vectors to epirange
  if (inherits(x, "Date")) {
    validate_all_saturdays(x)
    return(saturdays_to_epirange(x))
  }

  # Error for unsupported types
  rlang::abort(
    sprintf(
      "Invalid input type. Expected Date, EpiRange, or \"*\", got: %s",
      paste(class(x), collapse = ", ")
    )
  )
}

#' Create a data source object for the Delphi Epidata API
#'
#' @param target Character, disease target: "covid", "flu", or "rsv"
#' @param geo_types Character vector, geographic types to query (e.g., "state",
#' "nation")
#' @return A source object of class "delphi_epidata_source"
#' @concept data_sources
#' @export
delphi_epidata_source <- function(
  target = c("covid", "flu", "rsv"),
  geo_types = c("state", "nation")
) {
  # Validate and match target
  target <- rlang::arg_match(target)

  # Convert target to full signal name
  signal <- sprintf("confirmed_admissions_%s_ew_prelim", target)

  structure(
    list(
      signal = signal,
      geo_types = geo_types
    ),
    class = c("delphi_epidata_source", "reporting_source")
  )
}

#' Fetch reporting data from a source object
#'
#' Generic function to fetch reporting triangle data using S3 method dispatch.
#'
#' @param source A source object created by [delphi_epidata_source()] or
#'   [hub_data_source()]
#' @param reference_dates Date vector or epirange of reference dates
#' @param report_dates Date vector or epirange of report dates
#' @param locations Character vector of locations
#' @param ... Additional arguments passed to methods
#' @return data.frame with columns: reference_date, report_date, location, count, signal
#' @concept data_fetching
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

#' Fetch reporting data from Delphi Epidata
#'
#' @param source A delphi_epidata_source object
#' @param reference_dates Date vector or epirange of reference dates
#' @param report_dates Date vector or epirange of report dates
#' @param locations Character vector of locations ("*" for all)
#' @param ... Additional arguments (unused)
#' @return data.frame with reporting triangle data
#' @concept data_fetching
#' @export
fetch_reporting_data.delphi_epidata_source <- function(
  source,
  reference_dates,
  report_dates,
  locations = "*",
  ...
) {
  # Convert to epirange if Dates are provided, otherwise use as-is
  report_dates <- to_epirange(report_dates)
  reference_dates <- to_epirange(reference_dates)

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
#'
#' @param source A source object
#' @param ... Additional arguments (ignored)
#' @keywords internal
#' @export
fetch_reporting_data.default <- function(source, ...) {
  rlang::abort(sprintf(
    "Don't know how to fetch data from source of class '%s'",
    paste(class(source), collapse = "', '")
  ))
}
