# GAM fitting and sampling adapted from cdcent/nowcastRESPNET/R/gam.R and configs.R

#' @rdname nowcast
#' @order 3
#' @export
nowcast.gam_nowcast_config <- function(
  data,
  config = gam_nowcast_config(),
  nowcast_date = NULL,
  output_strata = NULL,
  ...
) {
  rlang::check_dots_empty()
  validate_nowcast_config(config)
  data <- prepare_nowcast_input(data, config, nowcast_date, output_strata)
  keys <- triangle_key_names(data)
  fit_keys <- config$fit_strata %||% keys
  checkmate::assert_subset(fit_keys, keys)
  if (!is.null(config$delay_prior)) {
    checkmate::assert_subset(
      setdiff(names(config$delay_prior), c("delay", "offset")),
      keys
    )
    # Check coverage even for complete or all-zero inputs that skip fitting.
    prior_grid <- expand.grid(
      .series = seq_len(nrow(data)),
      delay = 0:config$max_delay
    )
    add_delay_offset(add_series_keys(prior_grid, data), config)
  }

  results <- vector("list", nrow(data))
  for (group in series_groups(as.data.frame(data[fit_keys]))) {
    group_results <- with_fit_context(
      nowcast_gam_group(data, group, config),
      data[group[1], fit_keys, drop = FALSE],
      series_count = length(group)
    )
    results[group] <- group_results
  }
  finish_nowcast(results, data, output_strata)
}

nowcast_gam_group <- function(data, group, config) {
  triangles <- lapply(data$triangle[group], function(triangle) {
    totals <- rowSums(as.matrix(triangle), na.rm = TRUE)
    if (any(totals < 0)) {
      cli::cli_abort("Incremental counts imply negative observed totals.")
    }
    processed <- baselinenowcast::preprocess_negative_values(triangle)
    if (
      !isTRUE(all.equal(totals, rowSums(as.matrix(processed), na.rm = TRUE)))
    ) {
      cli::cli_abort(
        "Revision preprocessing failed to preserve observed totals."
      )
    }
    processed
  })
  results <- lapply(triangles, function(triangle) {
    dates <- baselinenowcast::get_reference_dates(triangle)
    incomplete <- apply(is.na(as.matrix(triangle)), 1L, any)
    result <- expand.grid(reference_date = dates, draw = seq_len(config$draws))
    result$pred_count <- rep(
      rowSums(as.matrix(triangle), na.rm = TRUE),
      config$draws
    )
    result$nowcast <- rep(incomplete, config$draws)
    result
  })
  if (!any(vapply(results, function(x) any(x$nowcast), logical(1)))) {
    return(results)
  }

  training <- do.call(
    rbind,
    lapply(seq_along(group), function(i) {
      long <- as.data.frame(triangles[[i]])
      long$.series <- group[i]
      long$.cutoff <- data$report_date[group[i]]
      long[long$reference_date > long$.cutoff - 7 * config$training_weeks, ]
    })
  )
  if (!nrow(training)) {
    cli::cli_abort("No observations in the GAM training window.")
  }
  positive_lags <- training$delay[training$count > 0]
  if (!length(positive_lags)) {
    if (
      any(vapply(
        results,
        function(x) {
          any(x$pred_count[x$nowcast] != 0)
        },
        logical(1)
      ))
    ) {
      cli::cli_abort(
        "No positive training counts support the incomplete dates."
      )
    }
    return(results)
  }
  supported_lag <- if (is.null(config$delay_prior)) {
    max(positive_lags)
  } else {
    config$max_delay
  }
  training <- training[training$delay <= supported_lag, ]
  prediction <- lapply(seq_along(group), function(i) {
    dates <- baselinenowcast::get_reference_dates(triangles[[i]])
    incomplete <- apply(is.na(as.matrix(triangles[[i]])), 1L, any)
    active <- incomplete &
      as.numeric(data$report_date[group[i]] - dates) < 7 * supported_lag
    if (!any(active)) {
      return(NULL)
    }
    grid <- expand.grid(reference_date = dates[active], delay = 0:supported_lag)
    grid$report_date <- grid$reference_date + 7 * grid$delay
    grid$.series <- group[i]
    grid$.cutoff <- data$report_date[group[i]]
    grid
  })
  prediction <- do.call(rbind, prediction)
  if (is.null(prediction) || !nrow(prediction)) {
    return(results)
  }

  require_gam_dependencies()
  training <- add_delay_offset(add_series_keys(training, data), config)
  prediction <- add_delay_offset(add_series_keys(prediction, data), config)
  features <- gam_feature_spec(training, triangle_key_names(data))
  training <- gam_features(training, features)
  prediction <- gam_features(prediction, features)
  fit <- fit_nowcast_gam(training, config, triangle_key_names(data))
  samples <- sample_nowcast_gam(fit, prediction, config)
  values <- validate_gam_samples(samples, prediction, config)

  if (config$sampling == "delay") {
    extra <- values |>
      dplyr::group_by(.data$.series, .data$reference_date, .data$draw) |>
      dplyr::summarize(
        probability = sum(.data$.value[.data$report_date <= .data$.cutoff]) /
          sum(.data$.value),
        .groups = "drop"
      )
    if (
      any(!is.finite(extra$probability)) ||
        any(extra$probability <= 0 | extra$probability > 1 + 1e-12)
    ) {
      cli::cli_abort(
        "GAM produced invalid reporting-completeness probabilities."
      )
    }
    extra$probability <- pmin(extra$probability, 1)
  } else {
    extra <- values |>
      dplyr::filter(.data$report_date > .data$.cutoff) |>
      dplyr::group_by(.data$.series, .data$reference_date, .data$draw) |>
      dplyr::summarize(unreported = sum(.data$.value), .groups = "drop")
  }

  for (i in seq_along(group)) {
    values_i <- extra[extra$.series == group[i], ]
    dates <- baselinenowcast::get_reference_dates(triangles[[i]])
    rows <- match(values_i$reference_date, dates) +
      (values_i$draw - 1L) * length(dates)
    unreported <- if (config$sampling == "delay") {
      observed <- results[[i]]$pred_count[rows]
      positive <- observed > 0
      counts <- numeric(length(observed))
      # Handle the zero-observation limit without an invalid size-zero draw.
      counts[positive] <- stats::rnbinom(
        sum(positive),
        size = observed[positive],
        prob = values_i$probability[positive]
      )
      counts
    } else {
      values_i$unreported
    }
    if (any(!is.finite(unreported))) {
      cli::cli_abort("GAM returned non-finite unreported-count draws.")
    }
    results[[i]]$pred_count[rows] <- results[[i]]$pred_count[rows] + unreported
  }
  results
}

require_gam_dependencies <- function() {
  rlang::check_installed(
    c("mgcv", "gratia"),
    version = c("1.9-5", "0.11.2"),
    reason = "for GAM nowcasting"
  )
}

add_series_keys <- function(long, data) {
  for (key in triangle_key_names(data)) {
    long[[key]] <- data[[key]][long$.series]
  }
  long
}

add_delay_offset <- function(long, config) {
  if (is.null(config$delay_prior)) {
    return(long)
  }
  prior <- config$delay_prior
  by <- setdiff(names(prior), "offset")
  result <- dplyr::left_join(long, prior, by = by, relationship = "many-to-one")
  if (nrow(result) != nrow(long) || anyNA(result$offset)) {
    cli::cli_abort(
      "Delay offsets must cover every series and configured delay."
    )
  }
  result$.delay_offset <- log(result$offset)
  result$offset <- NULL
  result
}

gam_season <- function(dates) {
  lubridate::year(dates) - as.integer(lubridate::month(dates) < 10L)
}

gam_feature_spec <- function(training, keys) {
  seasons <- gam_season(training$reference_date)
  seasonal_origin <- tapply(as.numeric(training$reference_date), seasons, min)
  list(
    origin = min(training$reference_date),
    seasonal_origin = seasonal_origin,
    origins = sort(unique(
      as.numeric(training$reference_date - min(training$reference_date)) + 1
    )),
    lags = sort(unique(training$delay * 7 + 1)),
    keys = lapply(training[keys], function(x) levels(factor(x)))
  )
}

gam_features <- function(data, spec) {
  data$origin <- as.numeric(data$reference_date - spec$origin) + 1
  data$origin_f <- factor(data$origin, levels = spec$origins)
  data$lag <- 7 * data$delay + 1
  data$lag_f <- factor(data$lag, levels = spec$lags)
  season <- as.character(gam_season(data$reference_date))
  data$season <- factor(season, levels = names(spec$seasonal_origin))
  data$origin_s <- as.numeric(data$reference_date) -
    spec$seasonal_origin[season] +
    1
  data$dow_reference_date <- lubridate::wday(
    data$reference_date,
    week_start = 1
  )
  data$dow_reference_date_factor <- factor(
    data$dow_reference_date,
    levels = 1:7
  )
  for (key in names(spec$keys)) {
    data[[key]] <- factor(data[[key]], levels = spec$keys[[key]])
  }
  data
}

fit_nowcast_gam <- function(training, config, keys) {
  formula <- config$formula
  if (is.null(formula)) {
    terms <- c(
      "s(origin, k = 3, bs = 'cr')",
      if (config$model == "gam_smooth") {
        "s(lag, k = 3, bs = 'cr')"
      } else {
        "s(lag_f, bs = 're')"
      }
    )
    formula <- stats::as.formula(paste("~", paste(terms, collapse = " + ")))
  }
  predictors <- c(
    "origin",
    "origin_f",
    "lag",
    "lag_f",
    "delay",
    "season",
    "origin_s",
    "dow_reference_date",
    "dow_reference_date_factor",
    "reference_date",
    "report_date",
    keys
  )
  missing <- setdiff(all.vars(formula), predictors)
  if (length(missing)) {
    cli::cli_abort("Unknown GAM formula predictors: {.val {missing}}.")
  }
  # Resolve mgcv smooth constructors while retaining user-defined functions.
  formula_env <- new.env(parent = environment(formula))
  for (name in c("s", "te", "ti", "t2")) {
    formula_env[[name]] <- getExportedValue("mgcv", name)
  }
  formula_env$offset <- stats::offset
  rhs <- formula[[2]]
  if (!is.null(config$delay_prior)) {
    rhs <- call("+", rhs, call("offset", as.name(".delay_offset")))
  }
  formula <- stats::as.formula(
    call("~", as.name("count"), rhs),
    env = formula_env
  )
  fit <- mgcv::gam(
    formula,
    data = training,
    family = "nb",
    na.action = stats::na.fail
  )
  if (!isTRUE(fit$converged) || any(!is.finite(stats::coef(fit)))) {
    cli::cli_abort("GAM fit did not converge to finite coefficients.")
  }
  fit
}

sample_nowcast_gam <- function(fit, prediction, config) {
  needed <- intersect(all.vars(stats::formula(fit)), names(prediction))
  if (anyNA(prediction[needed])) {
    cli::cli_abort(
      "Insufficient GAM training support for prediction dates or factors."
    )
  }
  sampler <- if (config$sampling == "delay") {
    gratia::fitted_samples
  } else {
    gratia::posterior_samples
  }
  # A single call per fitted model preserves joint coefficient draw identities.
  sampler(model = fit, data = prediction, n = config$draws, method = "mh")
}

validate_gam_samples <- function(samples, prediction, config) {
  value <- if (config$sampling == "delay") ".fitted" else ".response"
  required <- c(".row", ".draw", value)
  if (
    !all(required %in% names(samples)) ||
      nrow(samples) != nrow(prediction) * config$draws ||
      anyNA(samples[required]) ||
      any(!is.finite(samples[[value]])) ||
      any(samples[[value]] < 0) ||
      any(!samples$.row %in% seq_len(nrow(prediction))) ||
      any(!samples$.draw %in% seq_len(config$draws)) ||
      anyDuplicated(samples[c(".row", ".draw")])
  ) {
    cli::cli_abort("GAM sampler returned invalid or incomplete draws.")
  }
  result <- prediction[
    samples$.row,
    c(".series", "reference_date", "report_date", ".cutoff")
  ]
  result$draw <- as.integer(samples$.draw)
  result$.value <- samples[[value]]
  result
}
