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
