#' Create a data source object for cfa-dataops NHSN data
#'
#' @param target Character, disease target: `"covid"`, `"flu"`, or `"rsv"`.
#' @param prelim Logical. If `TRUE`, use the preliminary NHSN HRD dataset.
#' @param dataset_namespace Character. Optional cfa-dataops dataset namespace.
#'   Defaults to the cfa-stf NHSN HRD namespace implied by `prelim`.
#' @param stage Character. cfa-dataops dataset stage to download.
#' @param command Character. Name or path of the `dataops_save` command.
#' @param cache_dir Character. Optional directory used for downloaded
#'   cfa-dataops versions. A temporary directory is used when `NULL`.
#' @param force Logical. Passed through to `dataops_save --force`.
#' @param run_dataops_save Function used to invoke the `dataops_save` CLI.
#'   The default implementation shells out via [system2()]. It is called with
#'   two arguments: `command` (the value of the `command` constructor
#'   argument) and `args` (a character vector built from the other
#'   constructor arguments such as `dataset_namespace`, `stage`, and
#'   `force`). Override to swap in an alternative runner — for example, a
#'   different process-spawning mechanism, or a stub during testing.
#' @return A source object of class `"cfa_dataops_source"`.
#' @concept data_sources
#' @export
cfa_dataops_source <- function(
  target = c("covid", "flu", "rsv"),
  prelim = TRUE,
  dataset_namespace = NULL,
  stage = "load",
  command = "dataops_save",
  cache_dir = NULL,
  force = FALSE,
  run_dataops_save = NULL
) {
  target <- rlang::arg_match(target)

  checkmate::assert_flag(prelim)
  checkmate::assert_string(stage)
  checkmate::assert_string(command)
  checkmate::assert_flag(force)

  if (!is.null(dataset_namespace)) {
    checkmate::assert_string(dataset_namespace)
  }
  if (!is.null(cache_dir)) {
    checkmate::assert_string(cache_dir)
  }
  if (is.null(run_dataops_save)) {
    run_dataops_save <- default_run_dataops_save
  }
  checkmate::assert_function(run_dataops_save)

  count_col <- c(
    covid = "totalconfc19newadm",
    flu = "totalconfflunewadm",
    rsv = "totalconfrsvnewadm"
  )[[target]]

  if (is.null(dataset_namespace)) {
    dataset_namespace <- if (prelim) {
      "public.stf.nhsn_hrd_prelim"
    } else {
      "public.stf.nhsn_hrd"
    }
  }

  structure(
    list(
      target = target,
      prelim = prelim,
      dataset_namespace = dataset_namespace,
      stage = stage,
      command = command,
      cache_dir = cache_dir,
      force = force,
      run_dataops_save = run_dataops_save,
      count_col = count_col,
      signal = count_col
    ),
    class = c("cfa_dataops_source", "reporting_source")
  )
}

#' Fetch reporting data from cfa-dataops
#'
#' @param source A cfa_dataops_source object.
#' @param reference_dates Date vector or `"*"` for all.
#' @param report_dates Date vector or `"*"` for all.
#' @param locations Character vector of locations (`"*"` for all). Use
#'   lowercase two-letter state abbreviations (e.g., `"ca"`) or `"us"` for
#'   national data.
#' @param dedup Character, `"latest"` (default) or `"earliest"`. Controls how
#'   multiple cfa-dataops versions falling in the same MMWR epiweek are
#'   resolved.
#' @param ... Additional arguments passed to internal helpers.
#' @return data.frame with reporting triangle data.
#' @concept data_fetching
#' @export
fetch_reporting_data.cfa_dataops_source <- function(
  source,
  reference_dates = "*",
  report_dates = "*",
  locations = "*",
  dedup = c("latest", "earliest"),
  ...
) {
  dedup <- rlang::arg_match(dedup)
  validate_cfa_dataops_dates(reference_dates, "reference_dates")
  validate_cfa_dataops_dates(report_dates, "report_dates")

  download_dir <- cfa_dataops_download_dir(source)
  dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

  source$run_dataops_save(
    command = source$command,
    args = cfa_dataops_save_args(
      source = source,
      download_dir = download_dir,
      report_dates = report_dates
    )
  )

  data <- read_cfa_dataops_download(
    download_dir = download_dir,
    source = source,
    dedup = dedup
  )

  if (!identical(reference_dates, "*")) {
    data <- dplyr::filter(
      data,
      .data$reference_date %in% as.Date(reference_dates)
    )
  }
  if (!identical(report_dates, "*")) {
    data <- dplyr::filter(
      data,
      .data$report_date %in% as.Date(report_dates)
    )
  }
  if (!identical(locations, "*")) {
    data <- dplyr::filter(data, .data$location %in% locations)
  }

  data
}

#' @noRd
validate_cfa_dataops_dates <- function(x, name) {
  if (identical(x, "*")) {
    return(invisible(NULL))
  }

  if (!inherits(x, "Date")) {
    msg <- c(
      "Invalid {.arg {name}}: expected a {.cls Date} vector or {.val *}."
    )
    if (inherits(x, "EpiRange")) {
      msg <- c(
        msg,
        x = "{.cls EpiRange} objects are not supported by
             {.fun cfa_dataops_source}."
      )
    }
    cli::cli_abort(msg)
  }

  validate_all_saturdays(x)
  invisible(NULL)
}

#' @noRd
cfa_dataops_download_dir <- function(source) {
  if (!is.null(source$cache_dir)) {
    return(source$cache_dir)
  }

  tempfile("nowcastnhsn-cfa-dataops-")
}

#' @noRd
cfa_dataops_save_args <- function(source, download_dir, report_dates) {
  version <- cfa_dataops_version_query(report_dates)

  args <- c(
    source$dataset_namespace,
    download_dir,
    "--stage",
    source$stage,
    "--version",
    version,
    "--full_range"
  )

  if (isTRUE(source$force)) {
    args <- c(args, "--force")
  }

  args
}

#' @noRd
cfa_dataops_version_query <- function(report_dates) {
  if (identical(report_dates, "*")) {
    cli::cli_warn(c(
      "Downloading all matching cfa-dataops versions.",
      "i" = "This can be large; pass {.arg report_dates} to restrict the
             version range."
    ))
    return(">=0001-01-01")
  }

  start_date <- min(report_dates) - 6L
  end_date <- max(report_dates) + 1L
  sprintf(">=%s,<%s", format(start_date), format(end_date))
}

#' @noRd
default_run_dataops_save <- function(command, args) {
  command_path <- Sys.which(command)
  if (!nzchar(command_path)) {
    cli::cli_abort(c(
      "Cannot find the {.command {command}} command.",
      "i" = "Install and configure {.pkg cfa-dataops} in this Python
             environment, or pass a full path via {.arg command}.",
      "i" = "This data source is intended for environments with the relevant
             cfa-dataops catalog and Azure access."
    ))
  }

  output <- tryCatch(
    system2(command_path, args = shQuote(args), stdout = TRUE, stderr = TRUE),
    error = function(err) {
      cli::cli_abort(c(
        "{.command {command}} failed to start.",
        "x" = conditionMessage(err)
      ))
    }
  )

  status <- attr(output, "status", exact = TRUE)
  if (!is.null(status) && status != 0L) {
    cli::cli_abort(c(
      "{.command {command}} failed with exit status {status}.",
      "i" = "Check that the cfa-dataops catalog package is installed and
             Azure authentication is available from this machine.",
      "x" = paste(output, collapse = "\n")
    ))
  }

  invisible(output)
}

#' @noRd
read_cfa_dataops_download <- function(download_dir, source, dedup) {
  files <- list.files(
    download_dir,
    recursive = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )
  files <- files[file.info(files)$isdir %in% FALSE]
  files <- cfa_dataops_data_files(files)

  if (length(files) == 0L) {
    cli::cli_abort(c(
      "No cfa-dataops files were found after download.",
      "i" = "Expected versioned files under {.path {download_dir}}."
    ))
  }

  pieces <- purrr::map(
    files,
    read_cfa_dataops_file,
    source = source,
    download_dir = download_dir
  )
  result <- purrr::list_rbind(pieces)

  result <- dedup_cfa_dataops_data(result, dedup = dedup)

  dplyr::select(
    result,
    "reference_date",
    "report_date",
    "location",
    "count",
    "signal"
  )
}

#' @noRd
cfa_dataops_data_files <- function(files) {
  supported_ext <- c("csv", "parquet", "parq")
  sidecar_files <- c("metadata.json")

  ext <- tolower(tools::file_ext(files))
  basename_files <- basename(files)

  unsupported <- files[
    !ext %in% supported_ext &
      !basename_files %in% sidecar_files
  ]
  if (length(unsupported) > 0L) {
    unsupported_ext <- unique(tolower(tools::file_ext(unsupported)))
    cli::cli_abort(c(
      "Unsupported cfa-dataops file format{?s}: {.val {unsupported_ext}}.",
      "i" = "Only parquet and CSV downloads are supported."
    ))
  }

  files[ext %in% supported_ext]
}

#' @noRd
read_cfa_dataops_file <- function(file, source, download_dir) {
  version <- cfa_dataops_file_version(file, download_dir)
  version_date <- cfa_dataops_version_date(version)
  data <- read_cfa_dataops_table(file)

  required <- c("weekendingdate", "jurisdiction", source$count_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    cli::cli_abort(c(
      "Downloaded cfa-dataops data does not match the expected NHSN schema.",
      "x" = "Missing column{?s}: {.field {missing}}",
      "i" = "Dataset: {.val {source$dataset_namespace}}",
      "i" = "Stage: {.val {source$stage}}"
    ))
  }

  loc <- as.character(data[["jurisdiction"]])
  loc[loc == "USA"] <- "US"

  data.frame(
    reference_date = as.Date(data[["weekendingdate"]]),
    report_date = date_to_saturday(version_date),
    location = tolower(loc),
    count = as.numeric(data[[source$count_col]]),
    signal = source$signal,
    dataops_version = version,
    dataops_version_date = version_date,
    stringsAsFactors = FALSE
  )
}

#' @noRd
cfa_dataops_file_version <- function(file, download_dir) {
  root <- normalizePath(download_dir, winslash = "/", mustWork = TRUE)
  full <- normalizePath(file, winslash = "/", mustWork = TRUE)
  rel <- substring(full, nchar(root) + 2L)
  strsplit(rel, "/", fixed = TRUE)[[1]][1]
}

#' @noRd
cfa_dataops_version_date <- function(version) {
  version_date <- as.Date(substr(version, 1L, 10L), format = "%Y-%m-%d")
  if (is.na(version_date)) {
    cli::cli_abort(c(
      "Cannot infer a report date from cfa-dataops version {.val {version}}.",
      "i" = "Expected versions beginning with an ISO date, e.g.
             {.val 2025-04-24} or {.val 2025-04-24T12-00-00}."
    ))
  }

  version_date
}

#' @noRd
read_cfa_dataops_table <- function(file) {
  ext <- tolower(tools::file_ext(file))

  if (ext %in% c("parquet", "parq")) {
    rlang::check_installed(
      "arrow",
      reason = "to read cfa-dataops parquet downloads."
    )
    return(as.data.frame(arrow::read_parquet(file)))
  }

  utils::read.csv(file, stringsAsFactors = FALSE, check.names = FALSE)
}

#' @noRd
dedup_cfa_dataops_data <- function(data, dedup) {
  group_cols <- c("reference_date", "report_date", "location", "signal")

  dupes <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::ungroup()

  n_dupes <- nrow(dupes)
  if (n_dupes > 0L) {
    n_groups <- dupes |>
      dplyr::distinct(dplyr::across(dplyr::all_of(group_cols))) |>
      nrow()
    cli::cli_warn(c(
      "Multiple cfa-dataops versions mapped to the same report week for
       {n_groups} group{?s}.",
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
      .data$dataops_version_date,
      .data$dataops_version
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    slice_fn(n = 1) |>
    dplyr::ungroup()
}
