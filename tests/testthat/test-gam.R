test_that("GAM configurations validate model-specific fields", {
  config <- gam_nowcast_config()
  expect_s3_class(config, "gam_nowcast_config")
  expect_s3_class(config, "nowcast_config")
  expect_true(validate_nowcast_config(config))
  expect_null(config$uncertainty_model)
  expect_message(print(config), "GAM Nowcast Configuration")
  expect_error(gam_nowcast_config(model = "unknown"))
  expect_error(gam_nowcast_config(sampling = "unknown"))
  expect_error(gam_nowcast_config(max_delay = 0))
  expect_error(gam_nowcast_config(training_weeks = 7))
  expect_error(gam_nowcast_config(draws = 0))
  expect_error(gam_nowcast_config(formula = count ~ lag), "one-sided")
  expect_error(gam_nowcast_config(formula = "~ lag"), "one-sided")
  expect_error(gam_nowcast_config(fit_strata = c("location", "location")))
  expect_error(gam_nowcast_config(location = NA_character_))
})

test_that("both GAM presets and sampling routes produce reproducible standard output", {
  skip_without_gam()
  data <- gam_reporting_data("ca")
  cutoff <- max(data$report_date)
  for (model in c("gam_smooth", "gam_re")) {
    for (sampling in c("delay", "posterior")) {
      config <- gam_nowcast_config(model, sampling = sampling, draws = 12)
      set.seed(31)
      actual <- suppressMessages(run_single_nowcast(data, "ca", config, cutoff))
      expect_s3_class(actual, "baselinenowcast_df")
      expect_named(
        actual,
        c(
          "pred_count",
          "reference_date",
          "draw",
          "output_type",
          "nowcast",
          "location"
        )
      )
      expect_silent(baselinenowcast::assert_baselinenowcast_df(actual))
      expect_equal(nrow(actual), length(unique(data$reference_date)) * 12)
      expect_true(all(is.finite(actual$pred_count) & actual$pred_count >= 0))
      expect_true(all(table(actual$reference_date) == 12))
      complete <- actual[!actual$nowcast, ]
      expect_true(all(vapply(
        split(complete$pred_count, complete$reference_date),
        function(x) {
          length(unique(x)) == 1L
        },
        logical(1)
      )))
      set.seed(31)
      again <- suppressMessages(run_single_nowcast(data, "ca", config, cutoff))
      expect_identical(actual, again)
    }
  }
})

test_that("GAM single-triangle, runner, and incremental routes agree", {
  skip_without_gam()
  data <- gam_reporting_data("ca")
  collection <- gam_triangles("ca")
  config <- gam_nowcast_config(draws = 8)
  set.seed(22)
  runner <- suppressMessages(run_single_nowcast(
    data,
    "ca",
    config,
    max(data$report_date)
  ))
  set.seed(22)
  single <- suppressMessages(nowcast(
    collection$triangle[[1]],
    config,
    collection$report_date
  ))
  expect_identical(single, runner[setdiff(names(runner), "location")])
  set.seed(22)
  increments <- suppressMessages(run_single_nowcast(
    cumulative_to_incremental(data),
    "ca",
    config,
    max(data$report_date) + 7,
    cumulative = FALSE
  ))
  expect_identical(runner, increments)
})

test_that("revision redistribution preserves observed totals at the inclusive horizon", {
  data <- gam_reporting_data("ca")
  cutoff <- max(data$report_date)
  ref <- cutoff - 14
  data$count[data$reference_date == ref] <- c(100, 85)
  collection <- suppressMessages(as_reporting_triangles(data, "location"))
  captured <- NULL
  local_mocked_bindings(
    sample_nowcast_gam = function(fit, prediction, config) {
      samples <- expand.grid(
        .row = seq_len(nrow(prediction)),
        .draw = seq_len(config$draws)
      )
      samples$.fitted <- 1
      samples
    },
    fit_nowcast_gam = function(training, config, keys) {
      captured <<- training
      list()
    },
    require_gam_dependencies = function() NULL
  )
  result <- suppressMessages(nowcast(collection, gam_nowcast_config(draws = 4)))
  expect_true(all(captured$count >= 0))
  expect_equal(sum(captured$count[captured$reference_date == ref]), 85)
  expect_true(all(result$pred_count[result$reference_date == ref] >= 85))
  expect_true(all(captured$delay <= 7))
  expect_true(all(captured$reference_date > cutoff - 70))
  expect_equal(captured$lag, captured$delay * 7 + 1)
})

test_that("complete and zero fitting groups do not require GAM dependencies", {
  local_mocked_bindings(require_gam_dependencies = function() {
    stop("unexpected fit")
  })
  data <- gam_reporting_data("ca")
  data$count <- 0
  zero <- suppressMessages(run_single_nowcast(
    data,
    "ca",
    gam_nowcast_config(draws = 3),
    max(data$report_date)
  ))
  expect_true(all(zero$pred_count == 0))
  data <- gam_reporting_data("ca")
  data <- data[data$reference_date <= max(data$report_date) - 49, ]
  complete <- suppressMessages(run_single_nowcast(
    data,
    "ca",
    gam_nowcast_config(draws = 3),
    max(data$report_date)
  ))
  expect_false(any(complete$nowcast))
  expect_true(all(table(complete$reference_date) == 3))
})

test_that("supported-delay trimming can give deterministic incomplete rows", {
  collection <- gam_triangles("ca")
  triangle <- collection$triangle[[1]]
  values <- as.matrix(triangle)
  values[, -1] <- ifelse(is.na(values[, -1]), NA, 0)
  collection$triangle[[1]] <- baselinenowcast::new_reporting_triangle(
    values,
    baselinenowcast::get_reference_dates(triangle),
    "weeks"
  )
  local_mocked_bindings(require_gam_dependencies = function() {
    stop("unexpected fit")
  })
  result <- suppressMessages(nowcast(collection, gam_nowcast_config(draws = 3)))
  expect_true(any(result$nowcast))
  expect_true(all(vapply(
    split(result$pred_count, result$reference_date),
    function(x) {
      length(unique(x)) == 1L
    },
    logical(1)
  )))
})

test_that("fit and sampler failures are visible with series context", {
  skip_without_gam()
  collection <- gam_triangles("ca")
  config <- gam_nowcast_config(draws = 3)
  local_mocked_bindings(sample_nowcast_gam = function(...) {
    data.frame(.row = 1L)
  })
  expect_error(suppressMessages(nowcast(collection, config)), "location=ca")
  expect_error(
    suppressMessages(nowcast(collection, config)),
    "invalid or incomplete"
  )
  expect_error(
    suppressMessages(nowcast(
      collection,
      gam_nowcast_config(formula = ~unknown)
    )),
    "Unknown GAM"
  )
  # A custom formula is not silently reduced to a different model.
  expect_error(
    suppressMessages(nowcast(
      collection,
      gam_nowcast_config(formula = ~ s(lag, k = 100))
    )),
    "fewer unique"
  )
  local_mocked_bindings(fit_nowcast_gam = function(...) stop("fit failure"))
  expect_error(suppressMessages(nowcast(collection, config)), "fit failure")
  failed <- suppressMessages(run_state_nowcasts(
    gam_reporting_data("ca"),
    config
  ))
  expect_equal(attr(failed, "failed_locations"), "ca")
  expect_equal(nrow(failed), 0)
})

test_that("sample validation rejects incomplete, invalid, or duplicated draws", {
  ref <- as.Date("2025-01-04")
  prediction <- data.frame(
    .series = 1L,
    reference_date = ref,
    report_date = ref + 0:2 * 7,
    .cutoff = ref + 7
  )
  config <- gam_nowcast_config(draws = 2)
  good <- expand.grid(.row = 1:3, .draw = 1:2)
  good$.fitted <- 1
  expect_equal(nrow(validate_gam_samples(good, prediction, config)), 6)
  for (bad_value in c(NA, Inf, -1)) {
    bad <- good
    bad$.fitted[1] <- bad_value
    expect_error(validate_gam_samples(bad, prediction, config), "invalid")
  }
  bad <- good
  bad$.row[1] <- 2
  expect_error(validate_gam_samples(bad, prediction, config), "invalid")
  expect_error(
    validate_gam_samples(good[-1, ], prediction, config),
    "incomplete"
  )
})

test_that("delay sampling uses the report cutoff and keeps coefficient draws aligned", {
  cutoff <- as.Date("2025-04-12")
  data <- expand.grid(reference_date = cutoff - 7 * 4:0, delay = 0:2)
  data$report_date <- data$reference_date + 7 * data$delay
  data$count <- 10
  data <- data[data$report_date <= cutoff, ]
  collection <- suppressMessages(as_reporting_triangles(
    data,
    character(),
    cumulative = FALSE
  ))
  local_mocked_bindings(
    require_gam_dependencies = function() NULL,
    fit_nowcast_gam = function(...) list(),
    sample_nowcast_gam = function(fit, prediction, config) {
      samples <- expand.grid(.row = seq_len(nrow(prediction)), .draw = 1:2)
      samples$.fitted <- ifelse(
        samples$.draw == 1,
        1,
        2^prediction$delay[samples$.row]
      )
      samples[rev(seq_len(nrow(samples))), ]
    }
  )
  config <- gam_nowcast_config(max_delay = 2, training_weeks = 4, draws = 2)
  set.seed(99)
  result <- suppressMessages(nowcast(collection, config))
  set.seed(99)
  observed <- c(20, 20, 10, 10)
  expected <- observed +
    stats::rnbinom(
      4,
      size = observed,
      prob = c(2 / 3, 3 / 7, 1 / 3, 1 / 7)
    )
  expect_equal(result$pred_count[result$nowcast], expected)
  expect_equal(
    unique(result$reference_date[result$nowcast]),
    c(cutoff - 7, cutoff)
  )
  local_mocked_bindings(sample_nowcast_gam = function(fit, prediction, config) {
    samples <- expand.grid(.row = seq_len(nrow(prediction)), .draw = 1:2)
    samples$.fitted <- 0
    samples
  })
  expect_error(
    suppressMessages(nowcast(collection, config)),
    "invalid reporting-completeness"
  )
})

test_that("GAM smooth fits recover a known multiplicative surface", {
  skip_without_gam()
  training <- expand.grid(origin = 7 * 0:4 + 1, lag = 7 * 0:4 + 1)
  training <- training[training$origin + training$lag <= 30, ]
  training$origin_f <- factor(training$origin)
  training$lag_f <- factor(training$lag)
  training$count <- 16 *
    4^((training$origin - 1) / 7) *
    2^(-(training$lag - 1) / 7)
  fit <- fit_nowcast_gam(training, gam_nowcast_config(), character())
  expect_equal(
    as.numeric(stats::predict(fit, type = "response")),
    training$count,
    tolerance = 1e-5
  )
  short <- training[training$origin <= 8 & training$lag <= 8, ]
  fit <- fit_nowcast_gam(short, gam_nowcast_config(), character())
  expect_setequal(
    all.vars(stats::formula(fit)),
    c("count", "origin_f", "lag_f")
  )
})

test_that("training and prediction reuse seasonal coordinates and factor levels", {
  dates <- as.Date(c("2024-09-21", "2024-09-28", "2024-10-05", "2024-10-12"))
  training <- data.frame(
    reference_date = dates,
    delay = 1:4,
    location = c("ca", "ca", "ny", "ny")
  )
  spec <- gam_feature_spec(training, "location")
  full <- gam_features(training, spec)
  subset <- gam_features(training[3:4, ], spec)
  expect_equal(subset$origin, full$origin[3:4])
  expect_equal(subset$origin_s, full$origin_s[3:4])
  expect_equal(as.numeric(full$origin_s), c(1, 8, 1, 8))
  expect_identical(levels(subset$location), levels(full$location))
})
