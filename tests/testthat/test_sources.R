test_that("delphi_epidata_source creates correct class", {
  src <- delphi_epidata_source(target = "covid")

  expect_s3_class(src, "delphi_epidata_source")
  expect_s3_class(src, "reporting_source")
  expect_equal(src$signal, "confirmed_admissions_covid_ew_prelim")
})

test_that("fetch_reporting_data.default errors on invalid source", {
  invalid_source <- structure(list(), class = "invalid_source")

  expect_error(
    fetch_reporting_data(invalid_source, NULL, NULL, NULL),
    "Don't know how to fetch data from source"
  )
})
