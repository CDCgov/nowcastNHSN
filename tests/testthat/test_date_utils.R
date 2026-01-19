test_that("validate_all_saturdays accepts valid Saturdays", {
  saturdays <- seq(as.Date("2024-12-07"), as.Date("2024-12-28"), by = "week")
  expect_silent(validate_all_saturdays(saturdays))
})

test_that("validate_all_saturdays rejects mixed dates", {
  mixed <- c(as.Date("2024-12-07"), as.Date("2024-12-09"))
  expect_error(
    validate_all_saturdays(mixed),
    "All dates must be Saturdays"
  )
})

test_that("saturdays_to_epirange converts correctly", {
  saturdays <- seq(as.Date("2024-12-07"), as.Date("2024-12-28"), by = "week")
  result <- saturdays_to_epirange(saturdays)

  # Should be an EpiRange object (note capital R)
  expect_s3_class(result, "EpiRange")

  # Result is a vector with min and max weeks
  expect_length(result, 2)
  expect_true(result[1] >= 202449)
  expect_true(result[2] <= 202501)
})

# Tests for cumulative_to_incremental ----------------------------------------

# Shared test fixture for cumulative data
cumulative_fixture <- data.frame(
  reference_date = as.Date(c("2024-01-06", "2024-01-06", "2024-01-06")),
  report_date = as.Date(c("2024-01-13", "2024-01-20", "2024-01-27")),
  count = c(100, 120, 125),
  location = "ca"
)

test_that("cumulative_to_incremental converts cumulative to incremental", {
  result <- cumulative_to_incremental(cumulative_fixture)

  # First row: 100 - 0 = 100
  # Second row: 120 - 100 = 20
  # Third row: 125 - 120 = 5
  expect_equal(result$count, c(100, 20, 5))
})

test_that("cumulative_to_incremental handles multiple reference dates", {
  # Data with rows in scrambled order (not grouped by reference_date)
  multi_ref_data <- data.frame(
    reference_date = as.Date(c(
      "2024-01-13",
      "2024-01-06",
      "2024-01-13",
      "2024-01-06"
    )),
    report_date = as.Date(c(
      "2024-01-27",
      "2024-01-13",
      "2024-01-20",
      "2024-01-20"
    )),
    count = c(60, 100, 50, 120),
    location = "ca"
  )

  result <- cumulative_to_incremental(multi_ref_data)

  # Reference date 2024-01-06: 100, 20 (cumulative 100, 120)
  # Reference date 2024-01-13: 50, 10 (cumulative 50, 60)
  # Check by filtering to each reference_date
  ref_06 <- result[result$reference_date == as.Date("2024-01-06"), ]
  ref_13 <- result[result$reference_date == as.Date("2024-01-13"), ]
  expect_equal(ref_06$count[order(ref_06$report_date)], c(100, 20))
  expect_equal(ref_13$count[order(ref_13$report_date)], c(50, 10))
})

test_that("cumulative_to_incremental handles multiple locations", {
  # Data with interleaved locations (ca, ny, ca, ny) - not grouped
  multi_loc_data <- data.frame(
    reference_date = as.Date(c(
      "2024-01-06",
      "2024-01-06",
      "2024-01-06",
      "2024-01-06"
    )),
    report_date = as.Date(c(
      "2024-01-13",
      "2024-01-13",
      "2024-01-20",
      "2024-01-20"
    )),
    count = c(100, 50, 120, 55),
    location = c("ca", "ny", "ca", "ny")
  )

  result <- cumulative_to_incremental(multi_loc_data)

  # CA: 100, 20 (cumulative 100, 120)
  # NY: 50, 5 (cumulative 50, 55)
  ca_result <- result[result$location == "ca", ]
  ny_result <- result[result$location == "ny", ]
  expect_equal(ca_result$count[order(ca_result$report_date)], c(100, 20))
  expect_equal(ny_result$count[order(ny_result$report_date)], c(50, 5))
})

test_that("cumulative_to_incremental handles unordered report dates", {
  # Same data as fixture but with shuffled rows
  unordered_data <- cumulative_fixture[c(3, 1, 2), ]

  result <- cumulative_to_incremental(unordered_data)

  # order_by ensures correct differencing regardless of row order
  expect_equal(result$count[result$report_date == as.Date("2024-01-13")], 100)
  expect_equal(result$count[result$report_date == as.Date("2024-01-20")], 20)
  expect_equal(result$count[result$report_date == as.Date("2024-01-27")], 5)
})

test_that("cumulative_to_incremental errors on missing columns", {
  bad_data <- data.frame(
    reference_date = as.Date("2024-01-06"),
    report_date = as.Date("2024-01-13")
    # missing count
  )

  expect_error(
    cumulative_to_incremental(bad_data),
    "count"
  )
})

test_that("cumulative_to_incremental errors on missing group columns", {
  no_location_data <- cumulative_fixture[, c(
    "reference_date",
    "report_date",
    "count"
  )]

  expect_error(
    cumulative_to_incremental(no_location_data),
    "Group columns not found"
  )
})

test_that("cumulative_to_incremental preserves other columns", {
  data_with_signal <- cumulative_fixture
  data_with_signal$signal <- "covid"

  result <- cumulative_to_incremental(data_with_signal)

  expect_true("signal" %in% names(result))
  expect_equal(result$signal, rep("covid", 3))
  expect_equal(result$location, rep("ca", 3))
})
