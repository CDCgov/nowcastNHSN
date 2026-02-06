#' Convert any date to the Saturday ending its MMWR epiweek
#'
#' @param dates A Date vector.
#' @return A Date vector of Saturdays.
#' @noRd
date_to_saturday <- function(dates) {
  # MMWR weeks run Sunday(0)-Saturday(6)
  # %w gives 0=Sunday, ..., 6=Saturday
  wday <- as.integer(format(dates, "%w"))
  dates + (6L - wday)
}

#' Convert FIPS state codes to lowercase two-letter abbreviations
#'
#' @param fips Character vector of FIPS codes (e.g., "06", "US").
#' @return Character vector of lowercase state abbreviations.
#' @noRd
fips_to_abbr <- function(fips) {
  lookup <- c(
    "US" = "us",
    "01" = "al",
    "02" = "ak",
    "04" = "az",
    "05" = "ar",
    "06" = "ca",
    "08" = "co",
    "09" = "ct",
    "10" = "de",
    "11" = "dc",
    "12" = "fl",
    "13" = "ga",
    "15" = "hi",
    "16" = "id",
    "17" = "il",
    "18" = "in",
    "19" = "ia",
    "20" = "ks",
    "21" = "ky",
    "22" = "la",
    "23" = "me",
    "24" = "md",
    "25" = "ma",
    "26" = "mi",
    "27" = "mn",
    "28" = "ms",
    "29" = "mo",
    "30" = "mt",
    "31" = "ne",
    "32" = "nv",
    "33" = "nh",
    "34" = "nj",
    "35" = "nm",
    "36" = "ny",
    "37" = "nc",
    "38" = "nd",
    "39" = "oh",
    "40" = "ok",
    "41" = "or",
    "42" = "pa",
    "44" = "ri",
    "45" = "sc",
    "46" = "sd",
    "47" = "tn",
    "48" = "tx",
    "49" = "ut",
    "50" = "vt",
    "51" = "va",
    "53" = "wa",
    "54" = "wv",
    "55" = "wi",
    "56" = "wy",
    "60" = "as",
    "66" = "gu",
    "69" = "mp",
    "72" = "pr",
    "78" = "vi"
  )
  result <- lookup[as.character(fips)]
  if (anyNA(result)) {
    unmatched <- unique(fips[is.na(result)])
    cli::cli_warn("Unmatched FIPS codes returned as NA: {.val {unmatched}}")
  }
  unname(result)
}

#' Filter hub data by reference dates, report dates, and locations
#'
#' @param data A data frame with columns reference_date, report_date, location.
#' @param reference_dates Date vector or "*" for no filter.
#' @param report_dates Date vector or "*" for no filter.
#' @param locations Character vector of locations or "*" for no filter.
#' @return Filtered data frame.
#' @noRd
filter_hub_data <- function(data, reference_dates, report_dates, locations) {
  for (arg in list(
    list(val = reference_dates, name = "reference_dates"),
    list(val = report_dates, name = "report_dates")
  )) {
    if (inherits(arg$val, "EpiRange")) {
      cli::cli_abort(c(
        "{.cls EpiRange} objects are not supported by {.fun hub_data_source}.",
        "i" = "Pass a {.cls Date} vector or {.val *} for {.arg {arg$name}}."
      ))
    }
  }

  if (!identical(reference_dates, "*")) {
    ref_range <- range(as.Date(reference_dates))
    data <- dplyr::filter(
      data,
      .data$reference_date >= ref_range[1],
      .data$reference_date <= ref_range[2]
    )
  }

  if (!identical(report_dates, "*")) {
    rep_range <- range(as.Date(report_dates))
    data <- dplyr::filter(
      data,
      .data$report_date >= rep_range[1],
      .data$report_date <= rep_range[2]
    )
  }

  if (!identical(locations, "*")) {
    data <- dplyr::filter(data, .data$location %in% locations)
  }

  data
}

#' Deduplicate hub data where multiple as_of dates map to the same report_date
#'
#' @param data A data frame with columns reference_date, as_of, report_date,
#'   location, count, signal.
#' @param dedup Character, `"latest"` or `"earliest"`.
#' @return Data frame with duplicates resolved and the `as_of` column removed.
#' @noRd
dedup_hub_data <- function(data, dedup = c("latest", "earliest")) {
  dedup <- rlang::arg_match(dedup)
  group_cols <- c("reference_date", "report_date", "location", "signal")

  dupes <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  n_dupes <- nrow(dupes)
  if (n_dupes > 0) {
    n_groups <- dupes |>
      dplyr::distinct(dplyr::across(dplyr::all_of(group_cols))) |>
      nrow()
    cli::cli_warn(c(
      "Multiple {.field as_of} dates mapped to the same
       {.field report_date} (Saturday) for {n_groups} group{?s}.",
      "i" = "Keeping the {dedup} observation per group
             ({n_dupes} duplicate row{?s} resolved)."
    ))
  }

  slice_fn <- if (dedup == "latest") dplyr::slice_tail else dplyr::slice_head

  data |>
    dplyr::arrange(
      .data$reference_date,
      .data$location,
      .data$report_date,
      .data$as_of
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    slice_fn(n = 1) |>
    dplyr::ungroup() |>
    dplyr::select(-"as_of")
}

#' Create a data source object for a forecast hub S3 bucket
#'
#' @param hub_name Character, the S3 bucket name (e.g.,
#'   "covid19-forecast-hub").
#' @param target Character, the target name to filter to in the hub data
#'   (e.g., "wk inc covid hosp" for NHSN COVID hospitalizations).
#' @return A source object of class "hub_data_source"
#' @concept data_sources
#' @export
hub_data_source <- function(
  hub_name = "covid19-forecast-hub",
  target = "wk inc covid hosp"
) {
  checkmate::assert_string(hub_name)
  checkmate::assert_string(target)

  structure(
    list(hub_name = hub_name, target = target),
    class = c("hub_data_source", "reporting_source")
  )
}

#' Fetch reporting data from a forecast hub S3 bucket
#'
#' @param source A hub_data_source object
#' @param reference_dates Date vector or "*" for all
#' @param report_dates Date vector or "*" for all
#' @param locations Character vector of locations ("*" for all). Use lowercase
#'   two-letter state abbreviations (e.g., "ca", "ny") or "us" for national.
#' @param dedup Character, how to handle multiple `as_of` dates that fall
#'   within the same MMWR epiweek. `"latest"` (default) keeps the most recent
#'   observation; `"earliest"` keeps the first. A warning is issued whenever
#'   duplicates are found.
#' @param ... Additional arguments (unused)
#' @return data.frame with reporting triangle data
#' @concept data_fetching
#' @export
fetch_reporting_data.hub_data_source <- function(
  source,
  reference_dates,
  report_dates,
  locations = "*",
  dedup = c("latest", "earliest"),
  ...
) {
  dedup <- rlang::arg_match(dedup)
  rlang::check_installed(
    c("hubData", "arrow"),
    reason = "to fetch data from forecast hub S3 buckets."
  )

  # Connect to hub S3 bucket

  con <- hubData::connect_target_timeseries(
    arrow::s3_bucket(source$hub_name),
    date_col = NULL,
    na = c("NA", ""),
    ignore_files = NULL
  )

  # Filter to target, then collect
  raw <- con |>
    dplyr::filter(.data$target == source$target) |>
    dplyr::collect()

  # Transform columns to match output schema
  result <- raw |>
    dplyr::transmute(
      reference_date = as.Date(.data$date),
      as_of = as.Date(.data$as_of),
      report_date = date_to_saturday(.data$as_of),
      location = fips_to_abbr(.data$location),
      count = .data$observation,
      signal = source$target
    ) |>
    dplyr::filter(!is.na(.data$location))

  # Multiple as_of dates can map to the same Saturday report_date.
  # Deduplicate, warning about how many duplicates were resolved.
  result <- dedup_hub_data(result, dedup)

  # Filter by reference_dates, report_dates, locations
  filter_hub_data(result, reference_dates, report_dates, locations)
}
