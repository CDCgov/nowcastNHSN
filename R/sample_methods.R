#' Sample from a normal distribution given predictions and variance
#'
#' @description
#' Generates samples from a normal distribution with the given predictions
#'   as means and the estimated variance, rounded to integers for use with
#'   count data. This is the sampling counterpart to [fit_normal()].
#'
#' @param pred Vector of predictions (means).
#' @param uncertainty_params Vector of variance parameters. If length 1,
#'   the same variance is used for all predictions.
#' @returns Integer vector of samples of the same length as `pred`.
#' @importFrom stats rnorm
#' @importFrom cli cli_abort
#' @importFrom vctrs vec_recycle
#' @family sample_distribution
#' @concept probabilistic_methods
#' @export
#' @examples
#' pred <- c(3.2, 4.6, 5.1)
#' variance <- 2.5
#' samples <- sample_normal(pred, uncertainty_params = variance)
#' samples
sample_normal <- function(pred, uncertainty_params) {
  # Recycle uncertainty_params to match pred length (only length-1 recycling)
  uncertainty_params <- vctrs::vec_recycle(
    uncertainty_params,
    size = length(pred),
    x_arg = "uncertainty_params"
  )

  # Sample from normal with mean = pred, sd = sqrt(variance)
  sampled_pred <- rnorm(
    n = length(pred),
    mean = pred,
    sd = sqrt(uncertainty_params)
  ) |>
    round()

  return(sampled_pred)
}

#' Sample from a Skellam distribution given predictions and variance
#'
#' @description
#' Generates samples from a Skellam distribution (difference of two Poissons)
#'   with the given predictions as means and the estimated variance.
#'   This is the sampling counterpart to [fit_skellam()].
#'
#'   The Skellam distribution is parameterized by λ₁ and λ₂ where:
#'   - Mean = λ₁ - λ₂ = pred
#'   - Variance = λ₁ + λ₂ = uncertainty_params
#'
#'   The individual parameters are recovered as:
#'   - λ₁ = (pred + variance) / 2
#'   - λ₂ = (variance - pred) / 2
#'
#' @param pred Vector of predictions (means). Can be negative.
#' @param uncertainty_params Vector of variance parameters. Must satisfy
#'   variance > |pred| for valid Poisson parameters. If length 1, the same
#'   variance is used for all predictions.
#' @param robust Logical. If `FALSE` (the default), invalid Skellam parameters
#'   (variance <= |pred|) raise an error. If `TRUE`, the variance is instead
#'   bumped up to `|pred| + 1e-6` for the offending entries so that both Poisson
#'   rates stay positive and sampling can proceed. This preserves the mean
#'   (`pred`) while widening the variance by the minimal amount needed for valid
#'   parameters. See the `robust_sample` option of [nowcast_config()].
#' @returns Integer vector of samples of the same length as `pred`.
#' @importFrom stats rpois
#' @importFrom cli cli_abort
#' @importFrom vctrs vec_recycle
#' @family sample_distribution
#' @concept probabilistic_methods
#' @export
#' @examples
#' pred <- c(3.2, -1.5, 0.8)
#' variance <- 10
#' samples <- sample_skellam(pred, uncertainty_params = variance)
#' samples
sample_skellam <- function(pred, uncertainty_params, robust = FALSE) {
  # Recycle uncertainty_params to match pred length (only length-1 recycling)
  uncertainty_params <- vctrs::vec_recycle(
    uncertainty_params,
    size = length(pred),
    x_arg = "uncertainty_params"
  )

  # In robust mode, widen the variance to the smallest value that keeps both
  # Poisson rates positive (variance > |pred|). This preserves the mean and
  # only adjusts entries that would otherwise be invalid.
  if (robust) {
    uncertainty_params <- pmax(uncertainty_params, abs(pred) + 1e-6)
  }

  # Compute Poisson parameters
  lambda1 <- 0.5 * (pred + uncertainty_params)
  lambda2 <- 0.5 * (uncertainty_params - pred)

  # Check validity
  if (any(lambda1 <= 0 | lambda2 <= 0)) {
    invalid_idx <- which(lambda1 <= 0 | lambda2 <= 0)
    cli_abort(c(
      "Invalid Skellam parameters: variance must be greater than |pred|",
      x = "Found {length(invalid_idx)} prediction(s) with invalid parameters",
      i = "Increase variance or check predictions at indices: {head(invalid_idx, 5)}" # nolint: line_length_linter
    ))
  }

  # Sample from Skellam: X ~ Poisson(λ₁) - Poisson(λ₂)
  sampled_pred <- rpois(length(pred), lambda1) - rpois(length(pred), lambda2)

  return(sampled_pred)
}
