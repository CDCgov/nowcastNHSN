#' Configure GAM nowcasting
#'
#' @param model Preset: `"gam_smooth"` uses a smooth delay term; `"gam_re"`
#'   uses a random-effect delay term. Both include a smooth reference-time term.
#' @param max_delay Maximum reporting delay in weeks, inclusive.
#' @param training_weeks Training window in weeks before each series' effective
#'   report date. Must exceed `max_delay`. The lower boundary is exclusive.
#' @param draws Number of predictive draws per reference date.
#' @param formula Optional one-sided GAM formula overriding the preset.
#'   Available predictors are `origin`, `origin_f`, `lag`, `lag_f`, `delay`,
#'   `season`, `origin_s`, `dow_reference_date`, `dow_reference_date_factor`,
#'   `reference_date`, `report_date`, and series keys (converted to factors for
#'   fitting). Preset and custom formulas are fitted without automatic
#'   simplification when data are sparse.
#' @param sampling `"delay"` samples fitted means, converts them to reporting
#'   completeness, and samples unreported counts. `"posterior"` samples future
#'   increments directly from the fitted GAM's posterior predictive distribution.
#' @param fit_strata Keys defining independent fitting groups. `NULL` fits each
#'   series independently; `character(0)` pools all series. A subset pools
#'   series within each combination of those keys. No stratum effects are added
#'   automatically to the formula.
#' @param delay_prior Optional data frame containing `delay` in weeks, positive
#'   finite `offset`, and optional series-key columns. Entries must be unique
#'   by delay and supplied keys and cover every series at delays `0:max_delay`.
#'   This is a fixed log offset, not an estimated prior distribution. Its use
#'   retains the full delay horizon instead of trimming to positive-count support.
#' @param location Optional location identifier, as in [nowcast_config()].
#' @param output_dir Optional output directory, as in [nowcast_config()].
#'
#' @returns A `gam_nowcast_config` inheriting from `nowcast_config`.
#' @details Fits negative-binomial GAMs using optional packages `mgcv` and
#'   `gratia`. Both sampling routes use joint Metropolis-Hastings coefficient
#'   draws within each fit. Negative revisions are redistributed backward,
#'   preserving observed totals within the delay horizon. Future downward
#'   revisions are not modeled.
#'
#'   Time features retain RESP-NET's day-based coordinates: `origin` is days
#'   since the earliest training reference date plus one, and `lag` is
#'   `7 * delay + 1`. Seasons begin October 1; `origin_s` is relative to the
#'   earliest training reference date within its season. Training coordinates
#'   and factor levels are reused for prediction.
#' @export
#' @examples
#' gam_nowcast_config()
#' gam_nowcast_config("gam_re", sampling = "posterior", draws = 100L)
#' gam_nowcast_config(
#'   formula = ~ s(origin, k = 3) + s(lag, k = 3) + s(location, bs = "re"),
#'   fit_strata = character(0)
#' )
gam_nowcast_config <- function(
  model = c("gam_smooth", "gam_re"),
  max_delay = 7L,
  training_weeks = 10L,
  draws = 1000L,
  formula = NULL,
  sampling = c("delay", "posterior"),
  fit_strata = NULL,
  delay_prior = NULL,
  location = NULL,
  output_dir = NULL
) {
  config <- structure(
    list(
      model = match.arg(model),
      max_delay = max_delay,
      training_weeks = training_weeks,
      draws = draws,
      formula = formula,
      sampling = match.arg(sampling),
      fit_strata = fit_strata,
      delay_prior = delay_prior,
      location = location,
      output_dir = output_dir
    ),
    class = c("gam_nowcast_config", "nowcast_config", "list")
  )
  validate_nowcast_config(config)
  config$max_delay <- as.integer(max_delay)
  config$training_weeks <- as.integer(training_weeks)
  config$draws <- as.integer(draws)
  config
}

#' @rdname validate_nowcast_config
#' @export
validate_nowcast_config.gam_nowcast_config <- function(config) {
  checkmate::assert_choice(config$model, c("gam_smooth", "gam_re"))
  checkmate::assert_int(config$max_delay, lower = 1L)
  checkmate::assert_int(config$training_weeks, lower = config$max_delay + 1L)
  checkmate::assert_int(config$draws, lower = 1L)
  checkmate::assert_choice(config$sampling, c("delay", "posterior"))
  checkmate::assert_character(
    config$fit_strata,
    any.missing = FALSE,
    unique = TRUE,
    null.ok = TRUE
  )
  checkmate::assert_character(
    config$location,
    len = 1L,
    any.missing = FALSE,
    null.ok = TRUE
  )
  checkmate::assert_character(
    config$output_dir,
    len = 1L,
    any.missing = FALSE,
    null.ok = TRUE
  )
  if (
    !is.null(config$formula) &&
      (!inherits(config$formula, "formula") || length(config$formula) != 2L)
  ) {
    cli::cli_abort(
      "GAM formula must be a one-sided formula, for example ~ s(lag)."
    )
  }
  prior <- config$delay_prior
  if (!is.null(prior)) {
    checkmate::assert_data_frame(prior, min.rows = 1L)
    checkmate::assert_names(names(prior), must.include = c("delay", "offset"))
    checkmate::assert_integerish(
      prior$delay,
      lower = 0,
      upper = config$max_delay,
      any.missing = FALSE
    )
    checkmate::assert_numeric(prior$offset, any.missing = FALSE, finite = TRUE)
    if (any(prior$offset <= 0)) {
      cli::cli_abort("Delay offsets must be strictly positive.")
    }
    keys <- setdiff(names(prior), c("delay", "offset"))
    validate_series_keys(prior[keys])
    if (anyDuplicated(prior[c("delay", keys)])) {
      cli::cli_abort("Delay offsets must be unique by delay and supplied keys.")
    }
  }
  invisible(TRUE)
}

#' @param x A GAM configuration.
#' @param ... Unused.
#' @rdname gam_nowcast_config
#' @export
print.gam_nowcast_config <- function(x, ...) {
  cli::cli_h1("GAM Nowcast Configuration")
  for (field in c(
    "model",
    "max_delay",
    "training_weeks",
    "draws",
    "sampling"
  )) {
    cli::cli_text("{field}: {.val {x[[field]]}}")
  }
  cli::cli_text("fit_strata: {.val {x$fit_strata %||% 'each series'}}")
  if (!is.null(x$formula)) {
    cli::cli_text("formula: {.val {paste(deparse(x$formula), collapse = ' ')}}")
  }
  cli::cli_text("delay_prior: {.val {!is.null(x$delay_prior)}}")
  invisible(x)
}
