#' Generate a sample dataset
#'
#' @param arg_n_obs the number of observations.
#' @param arg_n_factors the number of factors.
#' @param arg_r_factors the factor correlation between factors.
#' @param arg_lambda the factor loading level (0 or 1).
#' @param arg_error the population error level.
#' @param arg_skew the skewness level (0, 1, or 2).
#' @param arg_kurt the kurtosis level.
#'
#' @export
simulate_sample <- function(arg_n_obs, arg_n_factors, arg_r_factors, arg_lambda, arg_error, arg_skew, arg_kurt) {

  vec_n_items <- floor(runif(arg_n_factors) * 3) * 2 + 4
  phi <- matrix(0, arg_n_factors, arg_n_factors)

  if (arg_r_factors == 0) {
    r <- runif(sum(lower.tri(phi)), .0, .3)
  } else if (arg_r_factors == 1) {
    r <- runif(sum(lower.tri(phi)), .3, .6)
  }

  phi[lower.tri(phi)] <- r
  phi <- phi + t(phi)
  diag(phi) <- 1

  idx_to <- cumsum(vec_n_items)
  idx_fr <- c(0, idx_to[1:(arg_n_factors - 1)]) + 1
  arg_n_items   <- sum(vec_n_items)
  matrix_lambda <- matrix(0, arg_n_items, arg_n_factors)

  for (i in 1:arg_n_factors) {
    if (arg_lambda == 1) {
      matrix_lambda[idx_fr[i]:idx_to[i], i] <- runif(vec_n_items[i], .5, .7)
    }
    if (arg_lambda == 0) {
      matrix_lambda[idx_fr[i]:idx_to[i], i] <- runif(vec_n_items[i], .3, .5)
    }
  }

  population_cor_base <- matrix_lambda %*% phi %*% t(matrix_lambda)
  population_cor <- population_cor_base

  if (arg_error > 0) {
    matrix_lambda_minor <- matrix(
      runif(arg_n_items * arg_n_factors * 3, -1, 1),
      arg_n_items, arg_n_factors * 3
    )
    population_cor_minor <- matrix_lambda_minor %*% t(matrix_lambda_minor)
    weight_minor  <- arg_error / arg_n_factors
    population_cor <- population_cor_base + (population_cor_minor * weight_minor)
  }

  diag(population_cor) <- 1

  vec_skew <- rep(c(arg_skew, -arg_skew), arg_n_items / 2)
  vec_kurt <- rep(arg_kurt, arg_n_items)

  sample_data <- NNpatched(arg_n_obs, rep(0, arg_n_items), population_cor, vec_skew, vec_kurt)

  return(sample_data)

}
