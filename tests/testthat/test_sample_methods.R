# Tests for sample_methods.R

# ============================================================================
# sample_normal tests
# ============================================================================

test_that("sample_normal returns correct length", {
  pred <- c(3.2, 4.6, 5.1)
  variance <- 2.5

  samples <- sample_normal(pred, uncertainty_params = variance)

  expect_length(samples, length(pred))
})

test_that("sample_normal accepts single variance for all predictions", {
  pred <- c(1, 2, 3, 4, 5)
  variance <- 1.0

  # Should not error
  samples <- sample_normal(pred, uncertainty_params = variance)
  expect_length(samples, 5)
})

test_that("sample_normal accepts vector of variances", {
  pred <- c(1, 2, 3)
  variances <- c(0.5, 1.0, 1.5)

  samples <- sample_normal(pred, uncertainty_params = variances)
  expect_length(samples, 3)
})

test_that("sample_normal errors on length mismatch", {
  pred <- c(1, 2, 3)
  variances <- c(0.5, 1.0) # Wrong length

  expect_error(
    sample_normal(pred, uncertainty_params = variances),
    "recycle"
  )
})

test_that("sample_normal with zero variance returns predictions", {
  pred <- c(5, 10, 15)
  variance <- 0

  samples <- sample_normal(pred, uncertainty_params = variance)

  expect_equal(samples, pred)
})

test_that("sample_normal produces samples with correct mean (approx)", {
  set.seed(42)
  pred <- 100
  variance <- 4
  n_samples <- 10000

  # Generate many samples
  samples <- replicate(n_samples, sample_normal(pred, variance))

  # Mean should be close to pred
  expect_true(abs(mean(samples) - pred) < 0.5)
})

test_that("sample_normal produces samples with correct variance (approx)", {
  set.seed(42)
  pred <- 0
  variance <- 9
  n_samples <- 10000

  samples <- replicate(n_samples, sample_normal(pred, variance))

  # Variance should be close to specified variance
  expect_true(abs(var(samples) - variance) < 1)
})

# ============================================================================
# sample_skellam tests
# ============================================================================

test_that("sample_skellam returns correct length", {
  pred <- c(3.2, -1.5, 0.8)
  variance <- 10

  samples <- sample_skellam(pred, uncertainty_params = variance)

  expect_length(samples, length(pred))
})

test_that("sample_skellam returns integers", {
  pred <- c(3.2, -1.5, 0.8)
  variance <- 10

  samples <- sample_skellam(pred, uncertainty_params = variance)

  expect_true(all(samples == floor(samples)))
})

test_that("sample_skellam accepts single variance for all predictions", {
  pred <- c(1, 2, 3, 4, 5)
  variance <- 20

  samples <- sample_skellam(pred, uncertainty_params = variance)
  expect_length(samples, 5)
})

test_that("sample_skellam accepts vector of variances", {
  pred <- c(1, 2, 3)
  variances <- c(10, 15, 20)

  samples <- sample_skellam(pred, uncertainty_params = variances)
  expect_length(samples, 3)
})

test_that("sample_skellam errors on length mismatch", {
  pred <- c(1, 2, 3)
  variances <- c(10, 15) # Wrong length

  expect_error(
    sample_skellam(pred, uncertainty_params = variances),
    "recycle"
  )
})

test_that("sample_skellam errors when variance <= |pred|", {
  pred <- c(5)
  variance <- 4 # Must be > 5 for valid parameters

  expect_error(
    sample_skellam(pred, uncertainty_params = variance),
    "variance must be greater"
  )
})

test_that("sample_skellam handles negative predictions", {
  pred <- c(-5, -3, -1)
  variance <- 20

  samples <- sample_skellam(pred, uncertainty_params = variance)

  expect_length(samples, 3)
  expect_true(all(samples == floor(samples)))
})

test_that("sample_skellam produces samples with correct mean (approx)", {
  set.seed(42)
  pred <- 5
  variance <- 20 # lambda1 = 12.5, lambda2 = 7.5
  n_samples <- 10000

  samples <- replicate(n_samples, sample_skellam(pred, variance))

  # Mean should be close to pred
  expect_true(abs(mean(samples) - pred) < 0.5)
})

test_that("sample_skellam produces samples with correct variance (approx)", {
  set.seed(42)
  pred <- 0
  # When pred=0 and variance=20, lambda1 = lambda2 = 10
  variance <- 20
  n_samples <- 10000

  samples <- replicate(n_samples, sample_skellam(pred, variance))

  # Variance should be close to specified variance
  expect_true(abs(var(samples) - variance) < 2)
})

test_that("sample_skellam can produce negative samples", {
  set.seed(42)
  pred <- -5
  variance <- 20
  n_samples <- 100

  samples <- replicate(n_samples, sample_skellam(pred, variance))

  # With mean of -5, we should see many negative values
  expect_true(sum(samples < 0) > 0)
})
