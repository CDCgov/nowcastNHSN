#' Validate that all dates are Saturdays
#'
#' @description
#' Internal helper function to validate that all provided dates fall on
#' Saturday.
#' @noRd
validate_all_saturdays <- function(dates) {
  dates <- as.Date(dates)
  weekdays_check <- weekdays(dates)

  if (!all(weekdays_check == "Saturday")) {
    bad_dates <- dates[weekdays_check != "Saturday"]
    rlang::abort(sprintf(
      "All dates must be Saturdays. Got: %s",
      paste(format(bad_dates, "%Y-%m-%d (%A)"), collapse = ", ")
    ))
  }

  invisible(NULL)
}


#' Convert Saturday dates to `epidatr::epirange` with MMWR epiweeks
#'
#' @description
#' Converts a sequence of Saturday dates (forecast hub convention) to an
#' `epidatr::epirange` object (Saturday-ending weeks in YYYYWW format).
#'
#' @param dates A vector of dates. Must all be Saturdays.
#'
#' @return An `epidatr::epirange` object covering the epiweeks corresponding
#'   to the input dates.
#'
#' @details
#' The function:
#' 1. Validates that all dates are Saturdays
#' 2. Converts to MMWR epiweek format (YYYYWW) using lubridate
#' 3. Returns an `epidatr::epirange` spanning from the earliest to latest week
#'
#' **Why Saturday dates work directly?**
#' - Both forecast hub and MMWR/CDC epiweeks end on Saturday
#' - epidatr API requires epiweek format (YYYYWW)
#' @export
saturdays_to_epirange <- function(dates) {
  dates <- as.Date(dates)

  # Validate all dates are Saturdays
  validate_all_saturdays(dates)

  # Convert to MMWR epiweek format (YYYYWW) using lubridate
  epiweeks <- lubridate::epiyear(dates) * 100 + lubridate::epiweek(dates)

  # Create epirange from min to max
  epidatr::epirange(min(epiweeks), max(epiweeks))
}
