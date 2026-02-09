# Tests for run_nowcasts.R
# Tests that wrapper functions produce equivalent results to direct baselinenowcast calls

# Load fixtures created from httptest-cached vignette data
# Run scripts/create_fixtures.R to regenerate these
get_test_fixtures <- function() {
  fixture_path <- testthat::test_path("fixtures")

  list(
    incremental_data = readRDS(file.path(fixture_path, "incremental_data.rds")),
    reporting_triangle = readRDS(file.path(
      fixture_path,
      "reporting_triangle_ca.rds"
    ))
  )
}

# Helper to run nowcast code quietly (suppresses baselinenowcast cli messages)
quietly <- function(expr) {
  suppressMessages(expr)
}

# ============================================================================
# nowcast_config tests
# ============================================================================

test_that("nowcast_config creates valid config object", {
  config <- nowcast_config()

  expect_s3_class(config, "nowcast_config")
  expect_equal(config$max_delay, 7)
  expect_equal(config$scale_factor, 3)
  expect_equal(config$prop_delay, 0.5)
  expect_equal(config$uncertainty_model, "negative_binomial")
  expect_equal(config$draws, 1000)
  expect_null(config$location)
  expect_null(config$output_dir)
})

test_that("nowcast_config validates parameters", {
  expect_error(nowcast_config(max_delay = 0), "max_delay")
  expect_error(nowcast_config(scale_factor = -1), "scale_factor")
  expect_error(nowcast_config(prop_delay = 1.5), "prop_delay")
  expect_error(nowcast_config(uncertainty_model = "invalid"))
  expect_error(nowcast_config(draws = 0), "draws")
})

test_that("nowcast_config_test creates test configuration", {
  config <- nowcast_config_test()

  expect_s3_class(config, "nowcast_config")
  expect_equal(config$max_delay, 4)
  expect_equal(config$draws, 10)
  expect_equal(config$location, "ca")
})

test_that("nowcast_config_test accepts custom location", {
  config <- nowcast_config_test(location = "ny", draws = 20)

  expect_equal(config$location, "ny")
  expect_equal(config$draws, 20)
})

# ============================================================================
# get_uncertainty_fns tests
# ============================================================================

test_that("get_uncertainty_fns returns correct functions for negative_binomial", {
  fns <- get_uncertainty_fns("negative_binomial")

  expect_type(fns, "list")
  expect_named(fns, c("uncertainty_model", "uncertainty_sampler"))
  expect_type(fns$uncertainty_model, "closure")
  expect_type(fns$uncertainty_sampler, "closure")
})

test_that("get_uncertainty_fns returns correct functions for normal", {
  fns <- get_uncertainty_fns("normal")

  expect_type(fns, "list")
  expect_named(fns, c("uncertainty_model", "uncertainty_sampler"))
  expect_type(fns$uncertainty_model, "closure")
  expect_type(fns$uncertainty_sampler, "closure")
})

test_that("get_uncertainty_fns returns correct functions for skellam", {
  fns <- get_uncertainty_fns("skellam")

  expect_type(fns, "list")
  expect_named(fns, c("uncertainty_model", "uncertainty_sampler"))
  expect_type(fns$uncertainty_model, "closure")
  expect_type(fns$uncertainty_sampler, "closure")
})

test_that("get_uncertainty_fns rejects invalid model names", {
  expect_error(get_uncertainty_fns("invalid"))
})

# ============================================================================
# run_single_nowcast equivalence tests (using fixtures)
# ============================================================================

test_that("run_single_nowcast produces equivalent results to direct baselinenowcast", {
  skip_if_not_installed("baselinenowcast")

  fixtures <- get_test_fixtures()
  reporting_triangle <- fixtures$reporting_triangle
  incremental_data <- fixtures$incremental_data

  nowcast_date <- max(incremental_data$reference_date[
    incremental_data$location == "ca"
  ])

  # Run direct baselinenowcast with the fixture triangle
  set.seed(42)
  direct_result <- quietly(baselinenowcast::baselinenowcast(
    reporting_triangle,
    scale_factor = 3,
    prop_delay = 0.5,
    draws = 50
  ))

  # Run via wrapper
  config <- nowcast_config(
    max_delay = 7,
    scale_factor = 3,
    prop_delay = 0.5,
    draws = 50
  )

  set.seed(42)
  wrapper_result <- quietly(run_single_nowcast(
    data = incremental_data,
    location = "ca",
    config = config,
    nowcast_date = nowcast_date,
    cumulative = FALSE # Fixture is already incremental
  ))

  # Compare results (wrapper adds location column)
  expect_equal(nrow(direct_result), nrow(wrapper_result))
  expect_equal(
    sort(unique(direct_result$reference_date)),
    sort(unique(wrapper_result$reference_date))
  )

  # Compare predicted counts (should be identical with same seed)
  direct_sorted <- direct_result[
    order(direct_result$reference_date, direct_result$draw),
  ]
  wrapper_sorted <- wrapper_result[
    order(wrapper_result$reference_date, wrapper_result$draw),
  ]
  expect_equal(direct_sorted$pred_count, wrapper_sorted$pred_count)
})

test_that("run_single_nowcast with normal uncertainty matches direct call", {
  skip_if_not_installed("baselinenowcast")

  fixtures <- get_test_fixtures()
  reporting_triangle <- fixtures$reporting_triangle
  incremental_data <- fixtures$incremental_data

  nowcast_date <- max(incremental_data$reference_date[
    incremental_data$location == "ca"
  ])

  # Direct call with normal uncertainty (matching getting-started.Rmd)
  set.seed(42)
  direct_result <- quietly(baselinenowcast::baselinenowcast(
    reporting_triangle,
    draws = 50,
    scale_factor = 3,
    prop_delay = 0.5,
    uncertainty_model = function(obs, pred) {
      baselinenowcast::fit_by_horizon(obs, pred, fit_model = fit_normal)
    },
    uncertainty_sampler = sample_normal
  ))

  # Via wrapper
  config <- nowcast_config(
    max_delay = 7,
    scale_factor = 3,
    prop_delay = 0.5,
    uncertainty_model = "normal",
    draws = 50
  )

  set.seed(42)
  wrapper_result <- quietly(run_single_nowcast(
    data = incremental_data,
    location = "ca",
    config = config,
    nowcast_date = nowcast_date,
    cumulative = FALSE # Fixture is already incremental
  ))

  direct_sorted <- direct_result[
    order(direct_result$reference_date, direct_result$draw),
  ]
  wrapper_sorted <- wrapper_result[
    order(wrapper_result$reference_date, wrapper_result$draw),
  ]
  expect_equal(direct_sorted$pred_count, wrapper_sorted$pred_count)
})

test_that("run_single_nowcast with skellam uncertainty matches direct call", {
  skip_if_not_installed("baselinenowcast")

  fixtures <- get_test_fixtures()
  reporting_triangle <- fixtures$reporting_triangle
  incremental_data <- fixtures$incremental_data

  nowcast_date <- max(incremental_data$reference_date[
    incremental_data$location == "ca"
  ])

  # Direct call with skellam uncertainty
  set.seed(42)
  direct_result <- quietly(baselinenowcast::baselinenowcast(
    reporting_triangle,
    draws = 50,
    scale_factor = 3,
    prop_delay = 0.5,
    uncertainty_model = function(obs, pred) {
      baselinenowcast::fit_by_horizon(obs, pred, fit_model = fit_skellam)
    },
    uncertainty_sampler = sample_skellam
  ))

  # Via wrapper
  config <- nowcast_config(
    max_delay = 7,
    scale_factor = 3,
    prop_delay = 0.5,
    uncertainty_model = "skellam",
    draws = 50
  )

  set.seed(42)
  wrapper_result <- quietly(run_single_nowcast(
    data = incremental_data,
    location = "ca",
    config = config,
    nowcast_date = nowcast_date,
    cumulative = FALSE # Fixture is already incremental
  ))

  direct_sorted <- direct_result[
    order(direct_result$reference_date, direct_result$draw),
  ]
  wrapper_sorted <- wrapper_result[
    order(wrapper_result$reference_date, wrapper_result$draw),
  ]
  expect_equal(direct_sorted$pred_count, wrapper_sorted$pred_count)
})

# ============================================================================
# run_state_nowcasts tests
# ============================================================================

test_that("run_state_nowcasts processes multiple locations", {
  skip_if_not_installed("baselinenowcast")

  fixtures <- get_test_fixtures()
  incremental_data <- fixtures$incremental_data

  # The fixture has both ca and ny
  config <- nowcast_config(
    max_delay = 7,
    draws = 10
  )

  result <- quietly(run_state_nowcasts(
    incremental_data,
    config,
    locations = c("ca", "ny"),
    cumulative = FALSE
  ))

  expect_s3_class(result, "data.frame")
  expect_true(all(c("ca", "ny") %in% result$location))
  expect_true("failed_locations" %in% names(attributes(result)))
})

test_that("run_state_nowcasts handles missing locations gracefully", {
  skip_if_not_installed("baselinenowcast")

  fixtures <- get_test_fixtures()
  incremental_data <- fixtures$incremental_data

  config <- nowcast_config(
    max_delay = 7,
    draws = 10
  )

  # Should alert about missing location (via cli) but still process available ones
  # cli_alert_warning produces a message, not a warning
  result <- quietly(run_state_nowcasts(
    incremental_data,
    config,
    locations = c("ca", "nonexistent"),
    cumulative = FALSE
  ))

  expect_true("ca" %in% result$location)
  expect_false("nonexistent" %in% result$location)
})

test_that("run_state_nowcasts accepts location_configs for overrides", {
  skip_if_not_installed("baselinenowcast")

  fixtures <- get_test_fixtures()
  incremental_data <- fixtures$incremental_data

  # Default config and location-specific overrides
  default_config <- nowcast_config(max_delay = 7, draws = 10)
  location_configs <- list(
    ny = nowcast_config(max_delay = 5, draws = 15, uncertainty_model = "normal")
  )

  result <- quietly(run_state_nowcasts(
    incremental_data,
    default_config,
    location_configs = location_configs,
    locations = c("ca", "ny"),
    cumulative = FALSE
  ))

  expect_s3_class(result, "data.frame")
  expect_true(all(c("ca", "ny") %in% result$location))

  # Verify different draws were used per location
  # ca uses default (10 draws), ny uses override (15 draws)
  ca_draws <- nrow(result[result$location == "ca", ]) /
    length(unique(result$reference_date[result$location == "ca"]))
  ny_draws <- nrow(result[result$location == "ny", ]) /
    length(unique(result$reference_date[result$location == "ny"]))

  expect_equal(ca_draws, 10)
  expect_equal(ny_draws, 15)
})

test_that("run_state_nowcasts uses default config when location_configs missing location", {
  skip_if_not_installed("baselinenowcast")

  fixtures <- get_test_fixtures()
  incremental_data <- fixtures$incremental_data

  # Only provide override for ca, ny should use default
  default_config <- nowcast_config(max_delay = 7, draws = 10)
  location_configs <- list(
    ca = nowcast_config(max_delay = 5, draws = 15)
  )

  # Should NOT error - ny uses the default config
  result <- quietly(run_state_nowcasts(
    incremental_data,
    default_config,
    location_configs = location_configs,
    locations = c("ca", "ny"),
    cumulative = FALSE
  ))

  expect_true(all(c("ca", "ny") %in% result$location))

  # ca uses override (15 draws), ny uses default (10 draws)
  ca_draws <- nrow(result[result$location == "ca", ]) /
    length(unique(result$reference_date[result$location == "ca"]))
  ny_draws <- nrow(result[result$location == "ny", ]) /
    length(unique(result$reference_date[result$location == "ny"]))

  expect_equal(ca_draws, 15)
  expect_equal(ny_draws, 10)
})

# ============================================================================
# summarize_nowcast tests
# ============================================================================

test_that("summarize_nowcast produces forecast hub format", {
  skip_if_not_installed("baselinenowcast")

  fixtures <- get_test_fixtures()
  incremental_data <- fixtures$incremental_data

  config <- nowcast_config(max_delay = 7, draws = 50)

  result <- quietly(run_single_nowcast(
    data = incremental_data,
    location = "ca",
    config = config,
    nowcast_date = max(incremental_data$reference_date[
      incremental_data$location == "ca"
    ]),
    cumulative = FALSE # Fixture is already incremental
  ))

  summary <- summarize_nowcast(result)

  # Check forecast hub format
  expect_true(all(
    c(
      "reference_date",
      "location",
      "output_type",
      "output_type_id",
      "value"
    ) %in%
      names(summary)
  ))

  expect_true(all(summary$output_type == "quantile"))

  # Should have 23 quantiles per location-date
  n_quantiles <- 23 # forecast hub standard
  n_dates <- length(unique(result$reference_date))
  expect_equal(nrow(summary), n_quantiles * n_dates)
})

test_that("summarize_nowcast quantiles are ordered correctly", {
  skip_if_not_installed("baselinenowcast")

  fixtures <- get_test_fixtures()
  incremental_data <- fixtures$incremental_data

  config <- nowcast_config(max_delay = 7, draws = 100)

  result <- quietly(run_single_nowcast(
    data = incremental_data,
    location = "ca",
    config = config,
    nowcast_date = max(incremental_data$reference_date[
      incremental_data$location == "ca"
    ]),
    cumulative = FALSE # Fixture is already incremental
  ))

  summary <- summarize_nowcast(result)

  # For each reference date, values should be monotonically increasing
  by_date <- split(summary, summary$reference_date)
  for (date_data in by_date) {
    sorted_by_quantile <- date_data[
      order(as.numeric(date_data$output_type_id)),
    ]
    expect_equal(sorted_by_quantile$value, sort(sorted_by_quantile$value))
  }
})
