test_that("delphi_epidata_source creates correct class", {
  src <- delphi_epidata_source(target = "covid")

  expect_s3_class(src, "delphi_epidata_source")
  expect_s3_class(src, "reporting_source")
  expect_equal(src$signal, "confirmed_admissions_covid_ew_prelim")
})

test_that("hub_data_source creates correct class", {
  src <- hub_data_source()
  expect_s3_class(src, "hub_data_source")
  expect_s3_class(src, "reporting_source")
  expect_equal(src$hub_name, "covid19-forecast-hub")
  expect_equal(src$target, "wk inc covid hosp")
})

test_that("hub_data_source validates inputs", {
  expect_error(hub_data_source(hub_name = 123))
  expect_error(hub_data_source(target = NULL))
})

test_that("date_to_saturday returns correct Saturdays", {
  # Wednesday -> Saturday of same week #nolint
  expect_equal(
    date_to_saturday(as.Date("2024-01-03")),
    as.Date("2024-01-06")
  )
  # Sunday -> Saturday of same epiweek #nolint
  expect_equal(
    date_to_saturday(as.Date("2024-01-07")),
    as.Date("2024-01-13")
  )
  # Saturday -> itself #nolint
  expect_equal(
    date_to_saturday(as.Date("2024-01-06")),
    as.Date("2024-01-06")
  )
})

test_that("fips_to_abbr converts known codes", {
  expect_equal(fips_to_abbr("06"), "ca")
  expect_equal(fips_to_abbr("US"), "us")
  expect_equal(fips_to_abbr(c("36", "06")), c("ny", "ca"))
})

test_that("fips_to_abbr warns on unknown codes", {
  expect_warning(fips_to_abbr("99"), "Unmatched FIPS")
})

test_that("fetch_reporting_data.default errors on invalid source", {
  invalid_source <- structure(list(), class = "invalid_source")

  expect_error(
    fetch_reporting_data(invalid_source, NULL, NULL, NULL),
    "Don't know how to fetch data from source"
  )
})
