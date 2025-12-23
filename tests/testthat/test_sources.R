test_that("epidata_source creates correct class", {
  src <- epidata_source(signal = "test_signal")

  expect_s3_class(src, "epidata_source")
  expect_s3_class(src, "reporting_source")
  expect_equal(src$signal, "test_signal")
})

test_that("fetch_reporting_data.default errors on invalid source", {
  invalid_source <- structure(list(), class = "invalid_source")

  expect_error(
    fetch_reporting_data(invalid_source, NULL, NULL, NULL),
    "Don't know how to fetch data from source"
  )
})
