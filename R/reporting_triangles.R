#' Collect reporting triangles with their series identifiers
#'
#' @param triangles A nonempty list of
#'   [baselinenowcast::reporting_triangle] objects containing incremental counts.
#' @param keys A data frame with one row per triangle and one column per series
#'   identifier, for example `location`, `signal`, or `age_group`. Rows must be
#'   unique and identifiers must not be missing. A zero-column, one-row data
#'   frame represents a single series without identifiers.
#' @param report_dates Effective observation dates, one per triangle.
#'   These are the latest available reports, before maximum-delay truncation.
#'   Dates must be Saturdays for triangles with weekly delays.
#'
#' @returns A `reporting_triangles` data frame with the key columns, a
#'   `report_date` column, and a `triangle` list-column. Series order follows
#'   the input order.
#' @details Delay units are read from each triangle. The built-in [nowcast()]
#'   methods currently require weekly delays. Observation dates are explicit
#'   because a truncated or complete triangle cannot always recover the date
#'   of the latest available report.
#' @export
reporting_triangles <- function(triangles, keys, report_dates) {
  checkmate::assert_list(triangles, min.len = 1L)
  checkmate::assert_data_frame(keys, nrows = length(triangles))
  validate_series_keys(keys)
  if (anyDuplicated(keys)) {
    cli::cli_abort("Series keys must uniquely identify each triangle.")
  }
  if (!ncol(keys) && nrow(keys) != 1L) {
    cli::cli_abort("Multiple triangles require series keys.")
  }
  checkmate::assert_date(
    report_dates,
    len = length(triangles),
    any.missing = FALSE
  )
  for (i in seq_along(triangles)) {
    validate_triangle_at_cutoff(triangles[[i]], report_dates[i])
  }
  new_reporting_triangles(triangles, keys, report_dates)
}

# Internal constructor: callers must validate inputs before assembly.
new_reporting_triangles <- function(triangles, keys, report_dates) {
  result <- as.data.frame(keys)
  result$report_date <- report_dates
  result$triangle <- triangles
  rownames(result) <- NULL
  class(result) <- c("reporting_triangles", "data.frame")
  result
}

#' Construct keyed reporting triangles from reporting data
#'
#' @param data Reporting data containing `reference_date`, `report_date`,
#'   `count`, and the columns in `strata`.
#' @param strata Character vector of columns identifying independent input
#'   series. Use `character(0)` for a single series without keys.
#' @param nowcast_date Requested vintage. Reports after this date are excluded.
#'   Defaults to the latest report date in `data`. Each series records its own
#'   latest available report on or before this vintage.
#' @param cumulative Whether counts are cumulative. If `TRUE`, include the
#'   initial cumulative report for each reference date and series.
#' @param delays_unit Unit of reporting delays, passed to
#'   [baselinenowcast::as_reporting_triangle()]. Defaults to `"weeks"` for NHSN;
#'   also accepts `"days"`, `"months"`, and `"years"`. The built-in [nowcast()]
#'   methods currently require weekly delays.
#' @returns A [reporting_triangles()] collection, without delay truncation or
#'   revision redistribution. Those operations are performed by [nowcast()].
#' @details With weekly delays, reference and report dates must be Saturdays.
#'   Duplicate cells, noninteger counts, and reports preceding their reference
#'   dates are errors.
#'   Missing observable incremental cells are filled with zero by
#'   [baselinenowcast::as_reporting_triangle()]; future cells remain missing.
#'   Missing cumulative snapshots are not inserted before differencing.
#' @export
as_reporting_triangles <- function(
  data,
  strata,
  nowcast_date = NULL,
  cumulative = TRUE,
  delays_unit = "weeks"
) {
  validate_reporting_data(data)
  checkmate::assert_character(strata, any.missing = FALSE, unique = TRUE)
  checkmate::assert_names(names(data), must.include = strata)
  validate_series_keys(as.data.frame(data[strata]))
  checkmate::assert_flag(cumulative)
  checkmate::assert_date(data$reference_date, any.missing = FALSE)
  checkmate::assert_date(data$report_date, any.missing = FALSE)
  if (!nrow(data)) {
    cli::cli_abort("No reporting data available.")
  }
  if (is.null(nowcast_date)) {
    nowcast_date <- max(data$report_date)
  }
  checkmate::assert_date(nowcast_date, len = 1L, any.missing = FALSE)
  data <- as.data.frame(data[
    data$report_date <= nowcast_date & data$reference_date <= nowcast_date,
    c("reference_date", "report_date", "count", strata),
    drop = FALSE
  ])
  if (!nrow(data)) {
    cli::cli_abort("No reporting data available on or before nowcast_date.")
  }
  if (identical(delays_unit, "weeks")) {
    validate_all_saturdays(data$reference_date)
    validate_all_saturdays(data$report_date)
  }
  if (any(data$report_date < data$reference_date)) {
    cli::cli_abort("Report dates cannot precede reference dates.")
  }
  checkmate::assert_numeric(data$count, any.missing = FALSE, finite = TRUE)
  checkmate::assert_integerish(data$count, any.missing = FALSE)
  if (anyDuplicated(data[c(strata, "reference_date", "report_date")])) {
    cli::cli_abort(
      "Duplicate reference_date/report_date cells within a series."
    )
  }
  if (cumulative) {
    if (any(data$count < 0)) {
      cli::cli_abort("Cumulative counts must be nonnegative.")
    }
    data <- cumulative_to_incremental(
      data,
      group_cols = c("reference_date", strata)
    )
  }
  groups <- series_groups(as.data.frame(data[strata]))
  triangles <- lapply(groups, function(rows) {
    baselinenowcast::as_reporting_triangle(
      data[rows, ],
      delays_unit = delays_unit
    )
  })
  keys <- as.data.frame(data[
    vapply(groups, `[`, integer(1), 1L),
    strata,
    drop = FALSE
  ])
  cutoffs <- as.Date(
    vapply(
      groups,
      function(rows) {
        as.numeric(max(data$report_date[rows]))
      },
      numeric(1)
    ),
    origin = "1970-01-01"
  )
  new_reporting_triangles(triangles, keys, cutoffs)
}

validate_series_keys <- function(keys) {
  reserved <- c(
    "triangle",
    "reference_date",
    "report_date",
    "count",
    "delay",
    "offset",
    "pred_count",
    "draw",
    "output_type",
    "output_type_id",
    "value",
    "nowcast",
    "origin",
    "origin_f",
    "lag",
    "lag_f",
    "season",
    "origin_s",
    "dow_reference_date",
    "dow_reference_date_factor"
  )
  checkmate::assert_names(names(keys), type = "unique")
  if (any(names(keys) %in% reserved | startsWith(names(keys), "."))) {
    cli::cli_abort(
      "Series keys cannot use reserved data or model column names."
    )
  }
  for (key in names(keys)) {
    x <- keys[[key]]
    if (
      !is.atomic(x) ||
        !is.null(dim(x)) ||
        anyNA(x) ||
        (is.numeric(x) && any(!is.finite(x)))
    ) {
      cli::cli_abort(
        "Series key {.val {key}} must contain nonmissing scalar values."
      )
    }
  }
  invisible(TRUE)
}

series_groups <- function(keys) {
  if (!ncol(keys)) {
    return(list(seq_len(nrow(keys))))
  }
  vctrs::vec_group_loc(keys)$loc
}

triangle_key_names <- function(data) {
  setdiff(names(data), c("triangle", "report_date"))
}

validate_triangle_at_cutoff <- function(triangle, cutoff) {
  baselinenowcast::assert_reporting_triangle(triangle)
  delays_unit <- baselinenowcast::get_delays_unit(triangle)
  dates <- baselinenowcast::get_reference_dates(triangle)
  if (delays_unit == "weeks") {
    validate_all_saturdays(dates)
    validate_all_saturdays(cutoff)
  }
  values <- as.matrix(triangle)
  observed <- values[!is.na(values)]
  checkmate::assert_numeric(observed, any.missing = FALSE, finite = TRUE)
  checkmate::assert_integerish(observed, any.missing = FALSE)
  max_observable_delay <- baselinenowcast::get_delays_from_dates(
    rep(cutoff, length(dates)),
    dates,
    delays_unit
  )
  observable <- outer(max_observable_delay, seq_len(ncol(values)) - 1L, `>=`)
  if (any(is.na(values) == observable) || any(dates > cutoff)) {
    cli::cli_abort(
      "Triangle missing cells must agree with its effective report date."
    )
  }
  invisible(TRUE)
}
