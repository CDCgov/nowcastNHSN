report_dates <- seq(as.Date("2024-11-30"), as.Date("2024-12-14"), by = "week")
reference_dates <- seq(
  as.Date("2024-11-23"),
  as.Date("2024-12-07"),
  by = "week"
)
loc <- "ca"

test_that("fetch_reporting_data works with epidatr_source", {
  # Create epidatr source
  src <- epidata_source(
    signal = "confirmed_admissions_covid_ew_prelim",
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

  # Check date ranges
  expect_true(all(result$reference_date >= min(reference_dates)))
  expect_true(all(result$reference_date <= max(reference_dates)))
  # Epidata may return additional report dates beyond what was requested
  expect_true(all(result$report_date >= min(report_dates)))
})
