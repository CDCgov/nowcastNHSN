#' Validate that all dates are Saturdays
#'
#' @description
#' Internal helper function to validate that all provided dates fall on
#' Saturday as per the using the MMWR epiweek week-ending convention.
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


#' Validate reporting triangle data frame
#'
#' @description
#' Internal helper function to validate that a data frame has the required
#' columns for reporting triangle operations (reference_date, report_date,
#' count).
#' @param data A data frame to validate.
#' @param required_cols Character vector of required column names.
#' @noRd
validate_reporting_data <- function(
  data,
  required_cols = c("reference_date", "report_date", "count")
) {
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    rlang::abort(
      paste0(
        "Data must contain columns: ",
        paste(required_cols, collapse = ", "),
        ". Missing: ",
        paste(missing_cols, collapse = ", ")
      )
    )
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
#' @concept date_utils
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

#' Convert cumulative counts to incremental counts
#'
#' @description
#' Converts cumulative counts (total reported as of each report date) to
#' incremental counts (new cases reported at each report date). This is
#' necessary because some data sources (e.g., epidatr NHSN data) return
#' cumulative counts, but `baselinenowcast::as_reporting_triangle()` expects
#' incremental counts.
#'
#' @param data A data frame with columns `reference_date`, `report_date`, and
#'   `count`. The `count` column should contain cumulative counts.
#' @param group_cols Character vector of additional columns to group by when

#'   computing differences (e.g., "location", "signal"). Default is
#'   `c("reference_date", "location")`.
#'
#' @return A data frame with the same structure as input, but with `count`
#'   converted to incremental counts (difference from previous report date).
#'
#' @details
#' For each reference date (and optionally other grouping columns), the
#' function:
#' 1. Sorts by report date
#' 2. Computes the difference from the previous report date's count
#' 3. Uses 0 as the "previous" count for the first report date
#'
#' This assumes that the first report date for each reference date represents
#' the initial count (i.e., there was 0 before it).
#'
#' @concept data_processing
#' @export
#' @examples
#' # Example with cumulative data
#' cumulative_data <- data.frame(
#'   reference_date = as.Date(c("2024-01-06", "2024-01-06", "2024-01-06")),
#'   report_date = as.Date(c("2024-01-13", "2024-01-20", "2024-01-27")),
#'   count = c(100, 120, 125),  # cumulative
#'   location = "ca"
#' )
#'
#' incremental_data <- cumulative_to_incremental(cumulative_data)
#' # count is now: 100, 20, 5 (the differences)
cumulative_to_incremental <- function(
  data,
  group_cols = c("reference_date", "location")
) {
  # Validate required columns

  validate_reporting_data(data)

  # Ensure group_cols exist in data
  missing_cols <- setdiff(group_cols, names(data))
  if (length(missing_cols) > 0) {
    rlang::warn(
      paste0(
        "Group columns not found in data, ignoring: ",
        paste(missing_cols, collapse = ", ")
      )
    )
    group_cols <- intersect(group_cols, names(data))
  }

  # Convert cumulative to incremental
  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::mutate(
      count = .data$count -
        dplyr::lag(
          .data$count,
          default = 0,
          order_by = .data$report_date
        )
    ) |>
    dplyr::ungroup()
}
