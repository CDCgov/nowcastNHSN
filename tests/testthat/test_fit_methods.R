# Tests for fit_methods.R

# ============================================================================
# fit_normal tests
# ============================================================================

test_that("fit_normal returns correct MLE variance", {
  # When observations equal predictions, variance should be 0
  obs <- c(5, 10, 15)
  pred <- c(5, 10, 15)
  expect_equal(fit_normal(obs, pred), 0)

  # Known case: deviations of 1, 2, 3 -> MSE = (1 + 4 + 9) / 3 = 14/3
  obs <- c(1, 2, 3)
  pred <- c(0, 0, 0)
  expected_var <- mean((obs - pred)^2)
  expect_equal(fit_normal(obs, pred), expected_var)
})

test_that("fit_normal handles negative deviations", {
  obs <- c(-1, -2, -3)
  pred <- c(0, 0, 0)
  expected_var <- mean((obs - pred)^2)
  expect_equal(fit_normal(obs, pred), expected_var)
})

test_that("fit_normal returns NA for empty input", {
  expect_true(is.na(fit_normal(numeric(0), numeric(0))))
})

test_that("fit_normal handles NA values", {
  obs <- c(1, NA, 3)
  pred <- c(0, 0, 0)
  # Should only use valid pairs: (1, 0) and (3, 0)
  expected_var <- mean(c(1, 9))
  expect_equal(fit_normal(obs, pred), expected_var)
})

test_that("fit_normal returns NA when all values are NA", {
  obs <- c(NA, NA)
  pred <- c(1, 2)
  expect_true(is.na(fit_normal(obs, pred)))
})

# ============================================================================
# dskellam_log tests
# ============================================================================

test_that("dskellam_log returns finite log-probabilities for valid inputs", {
  # When lambda1 = lambda2, mean is 0
  log_prob <- dskellam_log(0, lambda1 = 5, lambda2 = 5)
  expect_true(is.finite(log_prob))
  expect_true(log_prob <= 0) # Log probabilities are <= 0

  # Skellam can produce negative values
  log_prob_neg <- dskellam_log(-2, lambda1 = 3, lambda2 = 5)
  expect_true(is.finite(log_prob_neg))
})

test_that("dskellam_log matches log of manual PMF computation", {
  # For numerically stable case with small lambdas, verify log-PMF matches
  # log of explicit Skellam PMF: exp(-(λ₁+λ₂)) * (λ₁/λ₂)^(x/2) * I_|x|(2*sqrt(λ₁*λ₂))
  x <- 1
  lambda1 <- 3
  lambda2 <- 2

  log_result <- dskellam_log(x, lambda1, lambda2)

  # Manual unlogged computation (stable for these small values)
  manual_pmf <- exp(-(lambda1 + lambda2)) *
    (lambda1 / lambda2)^(x / 2) *
    besselI(2 * sqrt(lambda1 * lambda2), nu = abs(x))

  expect_equal(log_result, log(manual_pmf), tolerance = 1e-10)
})

test_that("dskellam_log is vectorized", {
  x <- c(-2, 0, 2)
  log_probs <- dskellam_log(x, lambda1 = 5, lambda2 = 3)
  expect_length(log_probs, 3)
  expect_true(all(is.finite(log_probs)))
})

# ============================================================================
# fit_skellam tests
# ============================================================================

test_that("fit_skellam returns NA for empty input", {
  expect_true(is.na(fit_skellam(integer(0), numeric(0))))
})

test_that("fit_skellam method of moments matches fit_normal", {
  # With method = "mom", fit_skellam should give same result as fit_normal
  obs <- c(-2L, 1L, 3L, 0L, -1L)
  pred <- c(-1.5, 0.8, 2.5, 0.2, -0.5)

  var_skellam <- fit_skellam(obs, pred, method = "mom")
  var_normal <- fit_normal(obs, pred)

  expect_equal(var_skellam, var_normal)
})

test_that("fit_skellam MLE recovers true variance from simulated data", {
  # Generate Skellam samples with known parameters
  lambda1 <- 3
  lambda2 <- 1
  true_mean <- lambda1 - lambda2 # = 2
  true_variance <- lambda1 + lambda2 # = 4
  n <- 1000

  obs <- withr::with_seed(
    42,
    rpois(n, lambda1) - rpois(n, lambda2)
  )
  pred <- rep(true_mean, n)

  variance_mle_est <- fit_skellam(obs, pred, method = "mle")
  variance_mom_est <- fit_skellam(obs, pred, method = "mom")

  # Should recover variance close to 4

  expect_equal(variance_mle_est, true_variance, tolerance = 0.05)
  expect_equal(variance_mom_est, true_variance, tolerance = 0.05)
})

test_that("fit_skellam MLE returns positive variance", {
  set.seed(123)
  # Generate Skellam samples
  lambda1 <- 10
  lambda2 <- 5
  n <- 100
  obs <- rpois(n, lambda1) - rpois(n, lambda2)
  pred <- rep(lambda1 - lambda2, n) # True mean = 5

  variance <- fit_skellam(obs, pred, method = "mle")

  expect_true(variance > 0)
  # Variance should be close to true variance (lambda1 + lambda2 = 15)
  expect_true(variance > 5 && variance < 30)
})

test_that("fit_skellam handles NA values", {
  obs <- c(-2L, NA, 3L)
  pred <- c(-1.5, 0.8, 2.5)

  # Should not error
  variance <- fit_skellam(obs, pred, method = "mom")
  expect_true(is.finite(variance))

  # Should only use valid pairs
  expected_var <- mean(c((-2 - (-1.5))^2, (3 - 2.5)^2))
  expect_equal(variance, expected_var)
})

test_that("fit_skellam returns NA when all values are NA", {
  obs <- c(NA_integer_, NA_integer_)
  pred <- c(1, 2)
  expect_true(is.na(fit_skellam(obs, pred)))
})

test_that("fit_skellam requires integer observations", {
  obs <- c(1.5, 2.5, 3.5)
  pred <- c(1, 2, 3)
  expect_error(fit_skellam(obs, pred), "integerish")
})

test_that("fit_skellam MLE respects variance > |mu| constraint", {
  # With large means, variance must be larger
  obs <- c(10L, 12L, 8L)
  pred <- c(9, 11, 7) # All positive means around 9

  variance <- fit_skellam(obs, pred, method = "mle")

  # Variance must be greater than max(|pred|) for valid lambdas
  expect_true(variance > max(abs(pred)))
})

test_that("fit_skellam works with all negative observations", {
  obs <- c(-5L, -3L, -7L, -4L)
  pred <- c(-4.5, -2.8, -6.5, -3.8)

  variance <- fit_skellam(obs, pred, method = "mle")
  expect_true(is.finite(variance))
  expect_true(variance > 0)
})

test_that("fit_skellam produces valid lambda parameters", {
  obs <- c(-2L, 1L, 3L, 0L, -1L)
  pred <- c(-1.5, 0.8, 2.5, 0.2, -0.5)

  variance <- fit_skellam(obs, pred, method = "mle")

  # Check that lambdas are positive for the training means
  lambda1 <- 0.5 * (pred + variance)
  lambda2 <- 0.5 * (variance - pred)

  expect_true(all(lambda1 > 0))
  expect_true(all(lambda2 > 0))
})
