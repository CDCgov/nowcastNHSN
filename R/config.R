#' Create a nowcast configuration object
#'
#' @description
#' Creates a validated configuration object for running state-level NHSN
#' nowcasts.
#'
#' @param max_delay Integer. Maximum delay (in weeks) to consider for the
#'   reporting triangle. Default is 7.
#' @param scale_factor Numeric. Multiplicative factor on max_delay for
#'   training volume in baselinenowcast. Default is 3.
#' @param prop_delay Numeric between 0 and 1. Proportion of training data
#'   used for delay estimation. Default is 0.5.
#' @param uncertainty_model Character. One of `"negative_binomial"` (default),
#'   `"normal"`, or `"skellam"`. Specifies the uncertainty model for
#'   probabilistic nowcasts.
#' @param draws Integer. Number of posterior draws for probabilistic nowcasts.
#'   Default is 1000.
#' @param location Character. A single location code (e.g., `"ca"`).
#'   Default is `NULL`, meaning no specific location is set.
#' @param output_dir Character. Directory path for partitioned parquet output.
#'   If `NULL`, results are returned but not written to disk. Default is `NULL`.
#'
#' @returns A list of class `nowcast_config` containing validated parameters.
#'
#' @importFrom checkmate assert_int assert_number assert_character
#'   assert_logical assert_choice makeAssertCollection reportAssertions
#' @importFrom cli cli_abort
#' @export
#'
#' @examples
#' # Default configuration
#' config <- nowcast_config()
#'
#' # Custom configuration for a single state
#' config <- nowcast_config(
#'   max_delay = 5,
#'   uncertainty_model = "skellam",
#'   location = "ca",
#'   output_dir = "output/nowcasts"
#' )
#'
#' # Configuration with fewer draws
#' config <- nowcast_config(
#'   draws = 500
#' )
nowcast_config <- function(
  max_delay = 7L,
  scale_factor = 3,
  prop_delay = 0.5,
  uncertainty_model = c("negative_binomial", "normal", "skellam"),
  draws = 1000L,
  location = NULL,
  output_dir = NULL
) {
  # Validate inputs

  uncertainty_model <- match.arg(uncertainty_model)

  coll <- checkmate::makeAssertCollection()
  checkmate::assert_int(max_delay, lower = 1L, add = coll)
  checkmate::assert_number(scale_factor, lower = 0, add = coll)
  checkmate::assert_number(prop_delay, lower = 0, upper = 1, add = coll)
  checkmate::assert_int(draws, lower = 1L, add = coll)
  if (!is.null(location)) {
    checkmate::assert_character(location, len = 1, add = coll)
  }
  if (!is.null(output_dir)) {
    checkmate::assert_character(output_dir, len = 1, add = coll)
  }
  checkmate::reportAssertions(coll)

  config <- list(
    max_delay = as.integer(max_delay),
    scale_factor = scale_factor,
    prop_delay = prop_delay,
    uncertainty_model = uncertainty_model,
    draws = as.integer(draws),
    location = location,
    output_dir = output_dir
  )

  class(config) <- c("nowcast_config", "list")

  config
}

#' Print method for nowcast_config
#'
#' @param x A `nowcast_config` object.
#' @param ... Additional arguments (ignored).
#' @returns Invisibly returns `x`.
#' @export
print.nowcast_config <- function(x, ...) {
  cli::cli_h1("Nowcast Configuration")
  cli::cli_ul()
  cli::cli_li("max_delay: {.val {x$max_delay}}")
  cli::cli_li("scale_factor: {.val {x$scale_factor}}")
  cli::cli_li("prop_delay: {.val {x$prop_delay}}")
  cli::cli_li("uncertainty_model: {.val {x$uncertainty_model}}")
  cli::cli_li("draws: {.val {x$draws}}")
  cli::cli_li("location: {.val {x$location %||% 'NULL (set at runtime)'}}")

  cli::cli_li("output_dir: {.val {x$output_dir %||% 'NULL (no file output)'}}")
  cli::cli_end()

  invisible(x)
}

#' Validate a nowcast_config object
#'
#' @param config A `nowcast_config` object to validate.
#' @returns `TRUE` invisibly if valid, otherwise throws an error.
#' @keywords internal
#' @noRd
validate_nowcast_config <- function(config) {
  if (!inherits(config, "nowcast_config")) {
    cli::cli_abort(c(
      "Invalid configuration object.",
      x = "Expected a {.cls nowcast_config} object.",
      i = "Use {.fn nowcast_config} to create a valid configuration."
    ))
  }

  required_fields <- c(
    "max_delay",
    "scale_factor",
    "prop_delay",
    "uncertainty_model",
    "draws"
  )

  missing_fields <- setdiff(required_fields, names(config))
  if (length(missing_fields) > 0) {
    cli::cli_abort(c(
      "Configuration is missing required fields.",
      x = "Missing: {.field {missing_fields}}"
    ))
  }

  invisible(TRUE)
}

#' Create a test configuration for quick pipeline validation
#'
#' @description
#' Creates a minimal configuration for quickly testing the nowcast pipeline.
#' Uses reduced draws, a single state, and fast settings to enable rapid
#' iteration during development.
#'
#' @param location Character. Single location code to test with.
#'   Default is `"ca"` (California).
#' @param draws Integer. Number of draws (reduced for speed).
#'   Default is `10`.
#' @param output_dir Character. Output directory. Default is `NULL`
#'   (no file output, just returns results).
#' @param ... Additional arguments passed to [nowcast_config()].
#'
#' @returns A `nowcast_config` object with test-friendly defaults.
#'
#' @export
#'
#' @examples
#' # Quick test config
#' config <- nowcast_config_test()
#' print(config)
#'
#' # Test with a different state
#' config <- nowcast_config_test(location = "ny")
#'
#' # Test with file output
#' config <- nowcast_config_test(output_dir = "test_output")
nowcast_config_test <- function(
  location = "ca",
  draws = 10L,
  output_dir = NULL,
  ...
) {
  cli::cli_alert_info(
    "Creating test config: {.val {location}}, {draws} draws"
  )

  nowcast_config(
    max_delay = 4L,
    scale_factor = 2,
    prop_delay = 0.5,
    uncertainty_model = "negative_binomial",
    draws = as.integer(draws),
    location = location,
    output_dir = output_dir,
    ...
  )
}
