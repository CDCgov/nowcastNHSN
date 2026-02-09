#' Check optimize() result for convergence
#'
#' @description
#' Utility function to validate that `stats::optimize()` found a valid minimum.
#' Checks that the objective value is finite (not Inf or NaN).
#'
#' @param opt_result The result from `stats::optimize()`.
#' @param context Optional character string describing what was being optimized,
#'   used in error messages.
#' @returns NULL invisibly if valid, otherwise throws an error.
#' @importFrom cli cli_abort
#' @keywords internal
#' @noRd
check_optimize_convergence <- function(opt_result, context = "optimization") {
  if (!is.finite(opt_result$objective)) {
    cli::cli_abort(c(
      "Optimization failed to converge.",
      x = "Could not find valid parameters for {context}.",
      i = "The objective function returned {.val {opt_result$objective}} at minimum."
    ))
  }
  invisible(NULL)
}


#' Fit a normal distribution with fixed mean to estimate variance
#'
#' @description
#' Takes in a vector of observations and a vector of fixed means (predictions)
#'   and returns the maximum likelihood estimate of the variance. With a fixed
#'   mean, the MLE for variance is simply the mean squared error.
#'
#' @param x Vector of observed values.
#' @param mu Vector of expected values (fixed means).
#' @returns The maximum likelihood estimate of the variance (sigma^2).
#' @family estimate_observation_error
#' @concept probabilistic_methods
#' @export
#' @examples
#' obs <- c(4, 8, 10)
#' pred <- c(3.1, 7.2, 11)
#' variance <- fit_normal(obs, pred)
#' variance
fit_normal <- function(x, mu) {
  if (length(x) == 0) {
    return(NA)
  }

  # Remove NA pairs

  valid <- !is.na(x) & !is.na(mu)
  x <- x[valid]
  mu <- mu[valid]

  if (length(x) == 0) {
    return(NA)
  }

  # MLE for variance with fixed mean is mean squared error
  variance <- mean((x - mu)^2)

  return(variance)
}

#' Skellam distribution log-probability mass function
#'
#' @description
#' Computes the log-PMF of the Skellam distribution, which is the distribution
#'   of the difference of two independent Poisson random variables.
#'
#' @param x Vector of integer observations (can be negative).
#' @param lambda1 Rate parameter for the first Poisson (must be positive).
#' @param lambda2 Rate parameter for the second Poisson (must be positive).
#' @returns Vector of log-probabilities.
#' @keywords internal
dskellam_log <- function(x, lambda1, lambda2) {
  # Skellam PMF: exp(-(λ₁+λ₂)) * (λ₁/λ₂)^(x/2) * I_|x|(2*sqrt(λ₁*λ₂))
  # Log form for numerical stability
  # Use expon.scaled = TRUE to avoid overflow in besselI for large arguments

  z <- 2 * sqrt(lambda1 * lambda2)
  log_pmf <- -(lambda1 + lambda2) +
    (x / 2) * log(lambda1 / lambda2) +
    log(besselI(z, nu = abs(x), expon.scaled = TRUE)) +
    z

  return(log_pmf)
}

#' Fit a Skellam distribution with fixed mean to estimate variance
#'
#' @description
#' Takes in a vector of integer observations and a vector of fixed means
#'   (predictions) and estimates the Skellam distribution variance parameter.
#'   The Skellam distribution models the difference of two Poisson random
#'   variables, making it suitable for count differences that can be negative.
#'
#'   With fixed mean μ = λ₁ - λ₂, the variance σ² = λ₁ + λ₂ is estimated via
#'   MLE or method of moments. The Poisson rate parameters can be recovered as:
#'   - λ₁ = (μ + σ²) / 2
#'   - λ₂ = (σ² - μ) / 2
#'
#'   For valid parameters, we require σ² > |μ|.
#'
#' @param x Vector of observed integer values (can be negative).
#' @param mu Vector of expected values (fixed means).
#' @param method Either "mle" for maximum likelihood estimation or "mom" for
#'   method of moments. Default is "mle".
#' @returns The estimated variance (σ² = λ₁ + λ₂).
#' @importFrom stats optimize
#' @importFrom checkmate assert_integerish
#' @family estimate_observation_error
#' @concept probabilistic_methods
#' @export
#' @examples
#' # Differences can be negative
#' obs <- c(-2, 1, 3, 0, -1)
#' pred <- c(-1.5, 0.8, 2.5, 0.2, -0.5)
#' variance <- fit_skellam(obs, pred)
#' variance
#'
#' # To sample from Skellam with a new mean prediction:
#' mu_new <- 5
#' lambda1 <- 0.5 * (mu_new + variance)
#' lambda2 <- 0.5 * (variance - mu_new)
#' # samples <- rpois(n, lambda1) - rpois(n, lambda2)
fit_skellam <- function(x, mu, method = "mle") {
  if (length(x) == 0) {
    return(NA)
  }

  # Check that all observations are integers
  assert_integerish(x)

  # Remove NA pairs
  valid <- !is.na(x) & !is.na(mu)
  x <- x[valid]
  mu <- mu[valid]

  if (length(x) == 0) {
    return(NA)
  }

  if (method == "mom") {
    # Method of moments: variance estimate
    variance <- mean((x - mu)^2)
  } else {
    # MLE approach
    # For each observation, we have fixed mean mu_i = lambda1_i - lambda2_i
    # We estimate a common variance sigma2 = lambda1 + lambda2
    # Constraint: sigma2 > max(|mu_i|) for all lambda values to be positive

    min_variance <- max(abs(mu)) + 0.01 # Slightly above to ensure positivity

    nllik <- function(sigma2) {
      lambda1 <- (mu + sigma2) / 2
      lambda2 <- (sigma2 - mu) / 2

      # Check validity
      if (any(lambda1 <= 0) || any(lambda2 <= 0)) {
        return(Inf)
      }

      nll <- -sum(dskellam_log(x, lambda1, lambda2), na.rm = TRUE)

      # Handle numerical issues
      if (!is.finite(nll)) {
        return(Inf)
      }

      return(nll)
    }

    # Start from method of moments estimate, bounded below by min_variance
    mom_est <- mean((x - mu)^2)
    upper_bound <- max(mom_est * 10, min_variance * 100)

    opt <- suppressWarnings(
      optimize(nllik, c(min_variance, upper_bound))
    )

    # Check convergence - suggest method of moments if MLE fails
    if (!is.finite(opt$objective)) {
      cli::cli_abort(c(
        "Skellam MLE optimization failed to converge.",
        i = "Try using {.code method = \"mom\"} for method of moments estimation.",
        i = "The objective function returned {.val {opt$objective}} at minimum."
      ))
    }

    variance <- opt$minimum
  }

  return(variance)
}

#' Get uncertainty model and sampler functions by name
#'
#' @description
#' Maps a named uncertainty model to the corresponding fit and sample functions
#' for use with `baselinenowcast::baselinenowcast()`.
#'
#' @param model_name Character. One of `"negative_binomial"`, `"normal"`, or
#'   `"skellam"`.
#'
#' @returns A named list with two elements:
#'   - `uncertainty_model`: A function for fitting uncertainty parameters
#'   - `uncertainty_sampler`: A function for sampling from the fitted model
#'
#' @importFrom baselinenowcast fit_by_horizon sample_nb
#' @importFrom cli cli_abort
#' @export
#'
#' @examples
#' # Get normal distribution functions
#' fns <- get_uncertainty_fns("normal")
#' fns$uncertainty_model
#' fns$uncertainty_sampler
#'
#' # Use with baselinenowcast
#' \dontrun{
#' fns <- get_uncertainty_fns("skellam")
#' nowcast <- baselinenowcast::baselinenowcast(
#'   reporting_triangle,
#'   uncertainty_model = fns$uncertainty_model,
#'   uncertainty_sampler = fns$uncertainty_sampler
#' )
#' }
get_uncertainty_fns <- function(model_name) {
  model_name <- match.arg(
    model_name,
    choices = c("negative_binomial", "normal", "skellam")
  )

  switch(
    model_name,
    negative_binomial = list(
      uncertainty_model = baselinenowcast::fit_by_horizon,
      uncertainty_sampler = baselinenowcast::sample_nb
    ),
    normal = list(
      uncertainty_model = function(obs, pred) {
        baselinenowcast::fit_by_horizon(obs, pred, fit_model = fit_normal)
      },
      uncertainty_sampler = sample_normal
    ),
    skellam = list(
      uncertainty_model = function(obs, pred) {
        baselinenowcast::fit_by_horizon(obs, pred, fit_model = fit_skellam)
      },
      uncertainty_sampler = sample_skellam
    )
  )
}
