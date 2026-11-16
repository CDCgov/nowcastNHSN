#' Nowcast reporting triangles using a configured model
#'
#' @param data A weekly [baselinenowcast::reporting_triangle] or a keyed
#'   [reporting_triangles()] collection, containing incremental counts.
#' @param config A [nowcast_config()] or [gam_nowcast_config()] object.
#'   S3 dispatch uses this argument rather than the triangle class.
#' @param nowcast_date Effective observation date for a single triangle.
#'   Required for single triangles. For collections, leave `NULL` to use their
#'   stored dates; overriding those dates is not supported.
#' @param output_strata Keys to retain in the output. `NULL` retains all keys;
#'   `character(0)` aggregates all series. Aggregation sums aligned final draws
#'   and requires matching reference dates among contributing series.
#' @param ... Arguments for additional S3 methods. Built-in methods do not
#'   accept additional arguments.
#' @returns A `baselinenowcast_df` with `reference_date`, `pred_count`, `draw`,
#'   `output_type = "samples"`, `nowcast`, and the retained series keys.
#' @details Both methods truncate to the configuration's inclusive maximum
#'   delay. Complete dates are retained as repeated observed counts. The
#'   `nowcast` flag indicates missing cells in that configured triangle, even
#'   if the GAM's fitted delay support gives deterministic draws. A grouped
#'   call fails if any fit fails, preventing incomplete aggregate results.
#'
#'   Existing location pipelines can continue using [run_single_nowcast()] and
#'   [run_state_nowcasts()]. Pooled fitting is available directly through this
#'   interface using a collection and a GAM configuration's `fit_strata`.
#' @name nowcast
#' @order 1
#' @export
nowcast <- function(
  data,
  config = nowcast_config(),
  nowcast_date = NULL,
  output_strata = NULL,
  ...
) {
  UseMethod("nowcast", config)
}

#' @rdname nowcast
#' @order 4
#' @export
nowcast.default <- function(
  data,
  config = nowcast_config(),
  nowcast_date = NULL,
  output_strata = NULL,
  ...
) {
  cli::cli_abort(
    "No nowcast method for configuration class {.cls {class(config)}}."
  )
}

#' @rdname nowcast
#' @order 2
#' @export
nowcast.nowcast_config <- function(
  data,
  config = nowcast_config(),
  nowcast_date = NULL,
  output_strata = NULL,
  ...
) {
  rlang::check_dots_empty()
  validate_nowcast_config(config)
  data <- prepare_nowcast_input(data, config, nowcast_date, output_strata)
  uncertainty_fns <- get_uncertainty_fns(
    config$uncertainty_model,
    robust_sample = isTRUE(config$robust_sample)
  )
  results <- lapply(seq_len(nrow(data)), function(i) {
    with_fit_context(
      {
        result <- baselinenowcast::baselinenowcast(
          data$triangle[[i]],
          scale_factor = config$scale_factor,
          prop_delay = config$prop_delay,
          draws = config$draws,
          uncertainty_model = uncertainty_fns$uncertainty_model,
          uncertainty_sampler = uncertainty_fns$uncertainty_sampler
        )
        if (isTRUE(config$nonneg_pred)) {
          result$pred_count <- pmax(result$pred_count, 0)
        }
        result
      },
      data[i, triangle_key_names(data), drop = FALSE]
    )
  })
  finish_nowcast(results, data, output_strata)
}

prepare_nowcast_input <- function(data, config, nowcast_date, output_strata) {
  if (inherits(data, "reporting_triangle")) {
    checkmate::assert_date(nowcast_date, len = 1L, any.missing = FALSE)
    data <- reporting_triangles(
      list(data),
      data.frame(row.names = 1L),
      nowcast_date
    )
  } else if (inherits(data, "reporting_triangles")) {
    if (!is.null(nowcast_date)) {
      cli::cli_abort("Use the effective report dates stored in the collection.")
    }
    data <- reporting_triangles(
      data$triangle,
      data[triangle_key_names(data)],
      data$report_date
    )
  } else {
    cli::cli_abort(
      "Expected a reporting_triangle or reporting_triangles collection."
    )
  }
  units <- vapply(
    data$triangle,
    baselinenowcast::get_delays_unit,
    character(1)
  )
  if (any(units != "weeks")) {
    cli::cli_abort(
      "Built-in nowcast methods currently require weekly reporting triangles."
    )
  }
  keys <- triangle_key_names(data)
  if (!is.null(output_strata)) {
    checkmate::assert_subset(output_strata, keys)
    checkmate::assert_character(
      output_strata,
      any.missing = FALSE,
      unique = TRUE
    )
    for (group in series_groups(as.data.frame(data[output_strata]))) {
      dates <- lapply(
        data$triangle[group],
        baselinenowcast::get_reference_dates
      )
      if (!all(vapply(dates, identical, logical(1), dates[[1]]))) {
        cli::cli_abort(
          "Output aggregation requires matching reference-date coverage."
        )
      }
    }
  }
  data$triangle <- lapply(data$triangle, function(triangle) {
    baselinenowcast::truncate_to_delay(triangle, config$max_delay)
  })
  data
}

with_fit_context <- function(expr, keys, series_count = 1L) {
  tryCatch(expr, error = function(e) {
    label <- if (ncol(keys)) {
      paste(
        paste(names(keys), vapply(keys, as.character, character(1)), sep = "="),
        collapse = ", "
      )
    } else if (series_count > 1L) {
      paste("pooled fit with", series_count, "series")
    } else {
      "single series"
    }
    cli::cli_abort("Nowcast failed for {label}.", parent = e)
  })
}

finish_nowcast <- function(results, data, output_strata) {
  keys <- triangle_key_names(data)
  for (i in seq_along(results)) {
    results[[i]] <- as.data.frame(results[[i]])
    for (key in keys) {
      results[[i]][[key]] <- rep(data[[key]][i], nrow(results[[i]]))
    }
  }
  result <- do.call(rbind, results)
  if (!is.null(output_strata) && !setequal(output_strata, keys)) {
    result <- result |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(
        "reference_date",
        "draw",
        output_strata
      )))) |>
      dplyr::summarize(
        pred_count = sum(.data$pred_count),
        nowcast = any(.data$nowcast),
        .groups = "drop"
      )
  }
  restore_nowcast_output(result)
}

restore_nowcast_output <- function(result) {
  result <- as.data.frame(result)
  result <- result[order(result$reference_date, result$draw), , drop = FALSE]
  flags <- result$nowcast
  if (nrow(result)) {
    result <- baselinenowcast::new_baselinenowcast_df(
      result,
      output_type = "samples",
      nowcast_dates = as.Date(character())
    )
  } else {
    # The upstream constructor assigns a scalar and cannot construct zero rows.
    result$output_type <- character()
    class(result) <- c("baselinenowcast_df", "data.frame")
  }
  # Completeness can differ between series sharing the same reference date.
  result$nowcast <- flags
  standard <- c(
    "pred_count",
    "reference_date",
    "draw",
    "output_type",
    "nowcast"
  )
  result <- result[c(standard, setdiff(names(result), standard))]
  rownames(result) <- NULL
  baselinenowcast::assert_baselinenowcast_df(result)
  result
}
