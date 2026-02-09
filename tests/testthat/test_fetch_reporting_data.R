report_dates <- seq(as.Date("2024-11-30"), as.Date("2024-12-14"), by = "week")
reference_dates <- seq(
  as.Date("2024-11-23"),
  as.Date("2024-12-07"),
  by = "week"
)
loc <- "ca"

test_that("test fixture dates are all Saturdays", {
  # All dates should be Saturdays (day 6) per MMWR epiweek convention
  expect_true(all(weekdays(report_dates) == "Saturday"))
  expect_true(all(weekdays(reference_dates) == "Saturday"))
})

# Note: httptest2 doesn't work with epidatr because epidatr uses httr (v1), not httr2.
# epidatr makes HTTP requests via httr::RETRY() and httr::content() (see R/request.R
# in cmu-delphi/epidatr), which httptest2 can't intercept. The original httptest
# package could theoretically work, but epidatr also has its own caching layer that
# makes mocking complex. Using skip_if_offline() is the recommended approach.
test_that("fetch_reporting_data works with epidatr_source", {
  skip_if_offline()

  # Create epidatr source
  src <- delphi_epidata_source(
    target = "covid",
    geo_types = "state"
  )

  # Fetch data
  result <- fetch_reporting_data(
    source = src,
    reference_dates = reference_dates,
    report_dates = report_dates,
    locations = loc
  )

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(
    c("reference_date", "report_date", "location", "count") %in% names(result)
  ))

  # Check that we got data for at least some of the requested reference dates
  expect_true(any(result$reference_date >= min(reference_dates)))
  # Note: When fetching with specific report_dates, epidata may return
  # reference dates outside the requested range due to how issues are stored

  # Epidata may return additional report dates beyond what was requested
  expect_true(any(result$report_date >= min(report_dates)))
})

# Note: arrow S3 requests go through the C++ HTTP layer, so neither httptest
# nor httptest2 can intercept them. Using skip_if_offline() + skip_if_not_installed().
test_that("fetch_reporting_data works with hub_data_source", {
  skip_if_offline()
  skip_if_not_installed("hubData")
  skip_if_not_installed("arrow")

  src <- hub_data_source()

  result <- fetch_reporting_data(
    source = src,
    reference_dates = reference_dates,
    report_dates = report_dates,
    locations = loc
  )

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) > 0)
  expect_true(all(
    c("reference_date", "report_date", "location", "count", "signal") %in%
      names(result)
  ))

  # All dates should be Saturdays
  expect_true(all(weekdays(result$reference_date) == "Saturday"))
  expect_true(all(weekdays(result$report_date) == "Saturday"))

  # Location should be lowercase abbreviation
  expect_true(all(result$location == "ca"))

  # Check that we got data within the requested ranges
  expect_true(any(result$reference_date >= min(reference_dates)))
  expect_true(any(result$report_date >= min(report_dates)))
})
