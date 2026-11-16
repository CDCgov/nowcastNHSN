test_that("independent fitting agrees with sequential single-series calls", {
  skip_without_gam()
  collection <- gam_triangles()
  config <- gam_nowcast_config("gam_re", draws = 8)
  set.seed(75)
  together <- suppressMessages(nowcast(collection, config))
  set.seed(75)
  separate <- lapply(seq_len(nrow(collection)), function(i) {
    one <- reporting_triangles(
      collection$triangle[i],
      data.frame(location = collection$location[i]),
      collection$report_date[i]
    )
    suppressMessages(nowcast(one, config))
  })
  expect_equal(
    sort_nowcast_rows(together),
    sort_nowcast_rows(do.call(rbind, separate))
  )
})

test_that("pooled formulas fit successfully and totals sum aligned final draws", {
  skip_without_gam()
  collection <- gam_triangles()
  config <- gam_nowcast_config(
    draws = 10,
    fit_strata = character(),
    formula = ~ s(origin, k = 3, bs = "cr") +
      s(lag, k = 3, bs = "cr") +
      s(location, bs = "re")
  )
  for (sampling in c("delay", "posterior")) {
    config$sampling <- sampling
    set.seed(114)
    series <- suppressMessages(nowcast(collection, config))
    set.seed(114)
    total <- suppressMessages(nowcast(
      collection,
      config,
      output_strata = character()
    ))
    expected <- series |>
      dplyr::group_by(.data$reference_date, .data$draw) |>
      dplyr::summarize(
        pred_count = sum(.data$pred_count),
        nowcast = any(.data$nowcast),
        .groups = "drop"
      )
    expect_equal(total$pred_count, expected$pred_count)
    expect_equal(total$nowcast, expected$nowcast)
    expect_silent(baselinenowcast::assert_baselinenowcast_df(series))
  }
})

test_that("fit strata control joint draws independently of output strata", {
  data <- gam_reporting_data()
  other <- data
  data$signal <- "first"
  other$signal <- "second"
  collection <- suppressMessages(as_reporting_triangles(
    rbind(data, other),
    c("location", "signal")
  ))
  calls <- list()
  local_mocked_bindings(
    require_gam_dependencies = function() NULL,
    fit_nowcast_gam = function(...) list(),
    sample_nowcast_gam = function(fit, prediction, config) {
      calls[[length(calls) + 1L]] <<- prediction
      samples <- expand.grid(
        .row = seq_len(nrow(prediction)),
        .draw = seq_len(config$draws)
      )
      # Distinct coefficient draws, shared by every row within a fitted model.
      samples$.response <- samples$.draw
      samples
    }
  )
  config <- gam_nowcast_config(
    draws = 3,
    sampling = "posterior",
    fit_strata = "signal"
  )
  series <- suppressMessages(nowcast(collection, config))
  expect_length(calls, 2)
  expect_true(all(vapply(
    calls,
    function(x) length(unique(x$location)) == 2,
    logical(1)
  )))
  expect_true(all(vapply(
    calls,
    function(x) length(unique(x$signal)) == 1,
    logical(1)
  )))
  total <- suppressMessages(nowcast(
    collection,
    config,
    output_strata = "location"
  ))
  expect_null(total$signal)
  expected <- series |>
    dplyr::group_by(.data$reference_date, .data$draw, .data$location) |>
    dplyr::summarize(pred_count = sum(.data$pred_count), .groups = "drop")
  expect_equal(total$pred_count, expected$pred_count)
  expect_error(nowcast(collection, gam_nowcast_config(fit_strata = "missing")))
  expect_error(nowcast(collection, config, output_strata = "missing"))
})

test_that("pooled predictions honor heterogeneous cutoffs and route-specific zeros", {
  data <- gam_reporting_data()
  cutoff <- max(data$report_date)
  data <- data[!(data$location == "ny" & data$report_date == cutoff), ]
  data$count[data$location == "ny"] <- 0
  collection <- suppressMessages(as_reporting_triangles(data, "location"))
  seen <- NULL
  local_mocked_bindings(
    require_gam_dependencies = function() NULL,
    fit_nowcast_gam = function(...) list(),
    sample_nowcast_gam = function(fit, prediction, config) {
      seen <<- prediction
      samples <- expand.grid(
        .row = seq_len(nrow(prediction)),
        .draw = seq_len(config$draws)
      )
      samples$.fitted <- samples$.response <- 1
      samples
    }
  )
  # Explicit offsets keep all delays, including those with no positive counts.
  offsets <- data.frame(delay = 0:7, offset = 1)
  config <- gam_nowcast_config(
    draws = 2,
    fit_strata = character(),
    delay_prior = offsets
  )
  delay <- suppressMessages(nowcast(collection, config))
  expect_true(all(delay$pred_count[delay$location == "ny"] == 0))
  expect_equal(unique(seen$.cutoff[seen$location == "ny"]), cutoff - 7)
  config$sampling <- "posterior"
  posterior <- suppressMessages(nowcast(collection, config))
  ny <- posterior[posterior$location == "ny", ]
  expected <- pmax(7 - as.numeric((cutoff - 7) - ny$reference_date) / 7, 0)
  expect_equal(ny$pred_count, expected)
  expect_true(any(ny$pred_count > 0))
  ref <- cutoff - 49
  expect_false(any(posterior$nowcast[
    posterior$location == "ca" & posterior$reference_date == ref
  ]))
  expect_true(all(posterior$nowcast[
    posterior$location == "ny" & posterior$reference_date == ref
  ]))
})

test_that("offsets are complete fixed predictors for training and prediction", {
  skip_without_gam()
  collection <- gam_triangles()
  prior <- expand.grid(
    delay = 0:7,
    location = c("ca", "ny"),
    stringsAsFactors = FALSE
  )
  prior$offset <- exp(-prior$delay / 2)
  config <- gam_nowcast_config(
    draws = 6,
    delay_prior = prior,
    fit_strata = character()
  )
  for (sampling in c("delay", "posterior")) {
    config$sampling <- sampling
    set.seed(46)
    result <- suppressMessages(nowcast(collection, config))
    expect_true(all(is.finite(result$pred_count)))
    expect_silent(baselinenowcast::assert_baselinenowcast_df(result))
  }
  training <- data.frame(delay = 0:7, location = "ca")
  joined <- add_delay_offset(training, config)
  expect_equal(joined$.delay_offset, -training$delay / 2)
  expect_null(joined$offset)
})

test_that("offset coverage and uniqueness are checked before fitting", {
  collection <- gam_triangles("ca")
  prior <- data.frame(delay = 0:7, offset = 1)
  expect_error(
    gam_nowcast_config(delay_prior = rbind(prior, prior[1, ])),
    "unique"
  )
  for (value in c(0, -1, NA, Inf)) {
    bad <- prior
    bad$offset[1] <- value
    expect_error(gam_nowcast_config(delay_prior = bad))
  }
  expect_error(gam_nowcast_config(
    delay_prior = transform(prior, delay = delay * 7)
  ))
  config <- gam_nowcast_config(delay_prior = prior[-1, ])
  expect_error(suppressMessages(nowcast(collection, config)), "cover every")
  bad <- prior
  bad$age_group <- "adult"
  expect_error(suppressMessages(nowcast(
    collection,
    gam_nowcast_config(delay_prior = bad)
  )))
  data <- gam_reporting_data("ca")
  data$count <- 0
  zero <- suppressMessages(as_reporting_triangles(data, "location"))
  expect_error(suppressMessages(nowcast(zero, config)), "cover every")
})

test_that("summaries and parquet output preserve all series keys", {
  collection <- gam_triangles()
  other <- collection
  collection$signal <- "first"
  other$signal <- "second"
  collection <- reporting_triangles(
    c(collection$triangle, other$triangle),
    rbind(collection[c("location", "signal")], other[c("location", "signal")]),
    c(collection$report_date, other$report_date)
  )
  set.seed(62)
  result <- suppressMessages(nowcast(collection, nowcast_config(draws = 5)))
  summary <- summarize_nowcast(result, quantiles = c(.1, .5, .9))
  expect_equal(nrow(summary), length(unique(result$reference_date)) * 4 * 3)
  expect_setequal(summary$signal, c("first", "second"))
  groups <- split(
    result,
    list(result$reference_date, result$location, result$signal)
  )
  expect_equal(sum(vapply(groups, nrow, integer(1))), nrow(result))
  single <- result[result$location == "ca" & result$signal == "first", ]
  expect_equal(
    summarize_nowcast(single, quantiles = .5)$value,
    as.numeric(tapply(single$pred_count, single$reference_date, stats::median))
  )
  skip_if_not_installed("arrow")
  path <- tempfile("grouped-nowcast-")
  withr::defer(unlink(path, recursive = TRUE))
  suppressMessages(write_nowcast_parquet(result, path))
  back <- read_nowcast_parquet(path)
  expect_equal(
    sort_nowcast_rows(result),
    sort_nowcast_rows(back[names(result)])
  )
  quantile_path <- tempfile("grouped-quantiles-")
  withr::defer(unlink(quantile_path, recursive = TRUE))
  suppressMessages(write_nowcast_parquet(
    result,
    quantile_path,
    summarize = TRUE
  ))
  expect_setequal(
    read_nowcast_parquet(quantile_path)$signal,
    c("first", "second")
  )
  expect_error(
    write_nowcast_parquet(result[setdiff(names(result), "location")], path),
    "location"
  )
})

test_that("mixed location configurations support GAMs and baseline models", {
  skip_without_gam()
  result <- suppressMessages(run_state_nowcasts(
    gam_reporting_data(),
    nowcast_config(draws = 5),
    location_configs = list(ny = gam_nowcast_config(draws = 6))
  ))
  expect_s3_class(result, "baselinenowcast_df")
  expect_length(attr(result, "failed_locations"), 0)
  expect_equal(length(unique(result$draw[result$location == "ca"])), 5)
  expect_equal(length(unique(result$draw[result$location == "ny"])), 6)
})

test_that("parallel workers load methods and release owned daemons on failure", {
  skip_without_gam()
  skip_if_not_installed("carrier")
  skip_if_not_installed("mirai")
  skip_if_not(
    identical(Sys.getenv("NOWCASTNHSN_PARALLEL_TESTS"), "true"),
    "Set NOWCASTNHSN_PARALLEL_TESTS=true with the current package installed."
  )
  expect_null(mirai::info())
  data <- gam_reporting_data()
  result <- suppressMessages(run_state_nowcasts(
    data,
    nowcast_config(draws = 5),
    location_configs = list(ny = gam_nowcast_config(draws = 6)),
    parallel = TRUE
  ))
  expect_length(attr(result, "failed_locations"), 0)
  expect_setequal(result$location, c("ca", "ny"))
  expect_equal(length(unique(result$draw[result$location == "ny"])), 6)
  expect_null(mirai::info())
  failed <- suppressMessages(run_state_nowcasts(
    data,
    nowcast_config(draws = 5),
    location_configs = list(ny = gam_nowcast_config(formula = ~unknown)),
    parallel = TRUE
  ))
  expect_equal(attr(failed, "failed_locations"), "ny")
  expect_null(mirai::info())
})
