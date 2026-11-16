test_that("keyed triangles preserve counts, dates, and key types", {
  data <- gam_reporting_data()
  data$location <- factor(data$location)
  collection <- suppressMessages(as_reporting_triangles(
    data,
    c("location", "signal")
  ))
  expect_s3_class(collection, "reporting_triangles")
  expect_equal(collection$location, factor(c("ca", "ny")))
  expect_equal(collection$report_date, rep(max(data$report_date), 2))
  expect_identical(
    collection,
    suppressMessages(as_reporting_triangles(
      data,
      c("location", "signal"),
      delays_unit = "weeks"
    ))
  )
  incremental <- cumulative_to_incremental(
    data,
    c("reference_date", "location", "signal")
  )
  from_increments <- suppressMessages(as_reporting_triangles(
    incremental,
    c("location", "signal"),
    cumulative = FALSE
  ))
  expect_identical(collection, from_increments)
  expect_true(all(vapply(
    collection$triangle,
    baselinenowcast::is_reporting_triangle,
    logical(1)
  )))
  expect_true(all(vapply(
    collection$triangle,
    function(x) anyNA(x),
    logical(1)
  )))
})

test_that("nonweekly triangles use their delay units to validate cutoffs", {
  for (unit in c("days", "months", "years")) {
    dates <- seq(as.Date("2023-01-02"), by = unit, length.out = 3)
    data <- data.frame(
      reference_date = dates[c(1, 1, 1, 2, 2, 3)],
      report_date = dates[c(1, 2, 3, 2, 3, 3)],
      count = c(10, 13, 15, 20, 24, 30),
      location = "ca"
    )
    collection <- suppressMessages(as_reporting_triangles(
      data,
      "location",
      delays_unit = unit
    ))
    triangle <- collection$triangle[[1]]
    expect_identical(baselinenowcast::get_delays_unit(triangle), unit)
    expect_equal(baselinenowcast::get_reference_dates(triangle), dates)
    expect_equal(as.numeric(triangle), c(10, 20, 30, 3, 4, NA, 2, NA, NA))
    expect_equal(collection$report_date, dates[3])
    expect_identical(
      reporting_triangles(
        collection$triangle,
        collection["location"],
        collection$report_date
      ),
      collection
    )
    expect_error(
      reporting_triangles(
        collection$triangle,
        collection["location"],
        seq(dates[3], by = unit, length.out = 2)[2]
      ),
      "effective report date"
    )
    for (config in list(nowcast_config(), gam_nowcast_config())) {
      expect_error(nowcast(collection, config), "require weekly")
      expect_error(
        nowcast(triangle, config, nowcast_date = dates[3]),
        "require weekly"
      )
    }
  }
})

test_that("vintages use each series' latest available report without leakage", {
  data <- gam_reporting_data()
  cutoff <- max(data$report_date)
  data <- data[!(data$location == "ny" & data$report_date == cutoff), ]
  expected <- suppressMessages(as_reporting_triangles(data, "location", cutoff))
  expect_equal(expected$report_date, c(cutoff, cutoff - 7))
  stale <- suppressMessages(as_reporting_triangles(
    data,
    "location",
    cutoff + 4
  ))
  expect_identical(expected, stale)
  future <- data[data$report_date == cutoff, ]
  future$report_date <- cutoff + 7
  future$count <- future$count + 10000
  actual <- suppressMessages(as_reporting_triangles(
    rbind(data, future),
    "location",
    cutoff
  ))
  expect_identical(actual, expected)
})

test_that("missing snapshots are differenced before observable gaps are filled", {
  ref <- as.Date("2025-01-04")
  data <- data.frame(
    reference_date = ref,
    report_date = ref + c(0, 14, 21),
    count = c(100, 130, 125)
  )
  triangle <- suppressMessages(as_reporting_triangles(
    data,
    character()
  ))$triangle[[1]]
  expect_equal(as.numeric(triangle), c(100, 0, 30, -5))
})

test_that("invalid cells, keys, and calendars fail clearly", {
  data <- gam_reporting_data("ca")
  expect_error(
    as_reporting_triangles(rbind(data, data[1, ]), "location"),
    "Duplicate"
  )
  bad <- data
  bad$count[1] <- -1
  expect_error(as_reporting_triangles(bad, "location"), "nonnegative")
  for (value in c(NA, Inf, 1.5)) {
    bad$count[1] <- value
    expect_error(as_reporting_triangles(bad, "location"))
  }
  bad <- data
  bad$report_date[1] <- bad$reference_date[1] - 7
  expect_error(as_reporting_triangles(bad, "location"), "precede")
  bad$report_date[1] <- bad$reference_date[1] + 1
  expect_error(as_reporting_triangles(bad, "location"), "Saturdays")
  expect_error(as_reporting_triangles(data, "count"), "reserved")
  expect_error(
    as_reporting_triangles(data, "location", delays_unit = "hours"),
    "delays_unit"
  )
  expect_error(
    as_reporting_triangles(data[FALSE, ], "location"),
    "No reporting data"
  )
  collection <- gam_triangles("ca")
  expect_error(
    reporting_triangles(
      rep(collection$triangle, 2),
      data.frame(location = c("ca", "ca")),
      rep(collection$report_date, 2)
    ),
    "uniquely"
  )
  expect_error(
    reporting_triangles(
      collection$triangle,
      data.frame(location = NA_character_),
      collection$report_date
    ),
    "nonmissing"
  )
  expect_error(
    reporting_triangles(
      collection$triangle,
      data.frame(location = "ca"),
      collection$report_date + 7
    ),
    "effective report date"
  )
})

test_that("single and collection baseline APIs preserve seeded baseline draws", {
  collection <- gam_triangles("ca")
  triangle <- collection$triangle[[1]]
  config <- nowcast_config(draws = 10)
  set.seed(41)
  direct <- suppressMessages(baselinenowcast::baselinenowcast(
    baselinenowcast::truncate_to_delay(triangle, 7),
    draws = 10
  ))
  set.seed(41)
  single <- suppressMessages(nowcast(triangle, config, collection$report_date))
  expect_equal(sort_nowcast_rows(single), sort_nowcast_rows(direct))
  set.seed(41)
  grouped <- suppressMessages(nowcast(collection, config))
  expect_identical(single, grouped[setdiff(names(grouped), "location")])
  expect_error(nowcast(triangle, config), "nowcast_date")
  expect_error(nowcast(collection, config, collection$report_date), "stored")
  expect_error(nowcast(collection, list()), "No nowcast method")
  expect_error(nowcast(collection, config, unused = TRUE), "must be empty")
  expect_error(
    nowcast(collection, nowcast_config(max_delay = 30)),
    "maximum delay"
  )
})

test_that("grouped baseline aggregation sums aligned draws and retains flags", {
  collection <- gam_triangles()
  config <- nowcast_config(draws = 10)
  set.seed(12)
  series <- suppressMessages(nowcast(collection, config))
  set.seed(12)
  total <- suppressMessages(nowcast(
    collection,
    config,
    output_strata = character()
  ))
  expected <- series |>
    dplyr::group_by(.data$reference_date, .data$draw) |>
    dplyr::summarize(pred_count = sum(.data$pred_count), .groups = "drop")
  expect_equal(total$pred_count, expected$pred_count)
  expect_s3_class(total, "baselinenowcast_df")
  expect_null(total$location)
  expect_silent(baselinenowcast::assert_baselinenowcast_df(total))
  incomplete <- collection
  incomplete$triangle[[2]] <- baselinenowcast::new_reporting_triangle(
    as.matrix(collection$triangle[[2]])[-1, ],
    baselinenowcast::get_reference_dates(collection$triangle[[2]])[-1],
    "weeks"
  )
  expect_error(
    nowcast(incomplete, config, output_strata = character()),
    "coverage"
  )
})

test_that("runner failures and legacy baseline configurations remain supported", {
  data <- gam_reporting_data("ca")
  config <- nowcast_config(draws = 5)
  set.seed(14)
  typed <- suppressMessages(run_single_nowcast(
    data,
    "ca",
    config,
    max(data$report_date)
  ))
  set.seed(14)
  untyped <- suppressMessages(run_single_nowcast(
    data,
    "ca",
    unclass(config),
    max(data$report_date)
  ))
  expect_identical(typed, untyped)
  other_signal <- data
  other_signal$signal <- "second_signal"
  expect_error(
    run_single_nowcast(
      rbind(data, other_signal),
      "ca",
      config,
      max(data$report_date)
    ),
    "exactly one signal"
  )
  failed <- suppressMessages(run_state_nowcasts(
    data,
    nowcast_config(max_delay = 30, draws = 5)
  ))
  expect_equal(nrow(failed), 0)
  expect_s3_class(failed, "baselinenowcast_df")
  expect_equal(attr(failed, "failed_locations"), "ca")
})
