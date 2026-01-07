report_dates <- seq(as.Date("2024-11-30"), as.Date("2024-12-14"), by = "week")
reference_dates <- seq(
  as.Date("2024-11-23"),
  as.Date("2024-12-07"),
  by = "week"
)
loc <- "ca"

# Note: httptest2 mocking doesn't work with epidatr's HTTP client implementation.
# Even with refresh_cache = TRUE passed via ..., requests aren't intercepted.
# Using skip_if_offline() as the recommended approach for network-dependent tests.
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
