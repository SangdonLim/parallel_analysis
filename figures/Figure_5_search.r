# Script for searching factor loadings for Figure 5

# SEARCH: K = 2 ----------------------------------------------------------------

rmsr <- function(z_loadings) {

  arg_loading_1 <- pnorm(z_loadings[1] / 1000)
  arg_loading_2 <- pnorm(z_loadings[2] / 1000)

  n_trials <- 1000

  # model 1: 2-factor correlated structure
  lambda_vec <- rep(.5, 4)
  lambda_mat <- kronecker(diag(1, 2), lambda_vec)
  factor_cor <- matrix(factor_cor_level, 2, 2)
  diag(factor_cor) <- 1
  population_cor <- lambda_mat %*% factor_cor %*% t(lambda_mat)
  diag(population_cor) <- 1

  sample_ev <- matrix(NA, n_trials, 8)
  set.seed(1)
  for (i in 1:n_trials) {
    sample_ev[i, ] <- eigen(cor(mvrnorm(100, rep(0, 8), population_cor)))$values
  }
  ev_model1_mean  <- apply(sample_ev, 2, mean)
  ev_model1_upper <- apply(sample_ev, 2, quantile, .975)
  ev_model1_lower <- apply(sample_ev, 2, quantile, .025)

  # model 2: 2-factor orthogonal structure
  lambda_vec <- rep(1, 4)
  lambda_mat <- kronecker(diag(1, 2), lambda_vec)
  lambda_mat[, 1] <- lambda_mat[, 1] * arg_loading_1
  lambda_mat[, 2] <- lambda_mat[, 2] * arg_loading_2
  factor_cor <- diag(1, 2)
  diag(factor_cor) <- 1
  population_cor <- lambda_mat %*% factor_cor %*% t(lambda_mat)
  diag(population_cor) <- 1

  sample_ev <- matrix(NA, n_trials, 8)
  set.seed(1)
  for (i in 1:n_trials) {
    sample_ev[i, ] <- eigen(cor(mvrnorm(100, rep(0, 8), population_cor)))$values
  }
  ev_model2_mean   <- apply(sample_ev, 2, mean)
  ev_model2_upper  <- apply(sample_ev, 2, quantile, .975)
  ev_model2_lower  <- apply(sample_ev, 2, quantile, .025)

  # compute difference
  rmsr_mean  <- sqrt(mean((ev_model1_mean   - ev_model2_mean ) ** 2))
  rmsr_upper <- sqrt(mean((ev_model1_upper  - ev_model2_upper) ** 2))
  rmsr_lower <- sqrt(mean((ev_model1_lower  - ev_model2_lower) ** 2))
  fit <- (rmsr_mean + rmsr_upper + rmsr_lower) * (1e+4)

  return(fit)

}

factor_cor_level <- .3
oldfit  <- 0
newfit  <- 1
z_loadings <- c(500, 500)

set.seed(1)
while (oldfit != newfit) {
  f <- optim(par = z_loadings, fn = rmsr, control = list(maxit = 20, REPORT = 1))
  z_loadings <- f$par
  oldfit     <- newfit
  newfit     <- f$value
  print(c(z_loadings, newfit))
  flush.console()
}
z_loadings <- sort(z_loadings, decreasing = TRUE)

loading_1 <- pnorm(z_loadings[1] / 1000) # .588
loading_2 <- pnorm(z_loadings[2] / 1000) # .409

# SEARCH: K = 3, 4 -------------------------------------------------------------

rmsr <- function(z_loadings) {

  arg_loading_1 <- pnorm(z_loadings[1] / 1000)
  arg_loading_2 <- pnorm(z_loadings[2] / 1000)
  arg_loading_3 <- pnorm(z_loadings[3] / 1000)

  n_trials <- 1000

  # 4-factor correlated structure
  lambda_vec    <- rep(loading_level, 3)
  lambda_mat    <- kronecker(diag(1, 4), lambda_vec)
  factor_cor         <- matrix(factor_cor_level, 4, 4)
  diag(factor_cor)   <- 1
  population_cor       <- lambda_mat %*% factor_cor %*% t(lambda_mat)
  diag(population_cor) <- 1

  sample_ev <- matrix(, n_trials, 12)
  set.seed(1)
  for(i in 1:n_trials) {
    sample_ev[i,] <- eigen(cor(mvrnorm(100, rep(0, 12), population_cor)))$values
  }
  ev_model1_mean  <- apply(sample_ev, 2, mean)
  ev_model1_upper <- apply(sample_ev, 2, quantile, .975)
  ev_model1_lower <- apply(sample_ev, 2, quantile, .025)

  # 3-factor correlated structure
  lambda_vec <- rep(1, 4)
  lambda_mat <- kronecker(diag(1, 3), lambda_vec)
  lambda_mat[, 1] <- lambda_mat[, 1] * arg_loading_1
  lambda_mat[, 2] <- lambda_mat[, 2] * arg_loading_2
  lambda_mat[, 3] <- lambda_mat[, 3] * arg_loading_3
  population_cor <- lambda_mat %*% t(lambda_mat)
  diag(population_cor) <- 1

  sample_ev <- matrix(, n_trials, 12)
  set.seed(1)
  for (i in 1:n_trials) {
    sample_ev[i, ] <- eigen(cor(mvrnorm(100, rep(0, 12), population_cor)))$values
  }
  ev_model2_mean  <- apply(sample_ev, 2, mean)
  ev_model2_upper <- apply(sample_ev, 2, quantile, .975)
  ev_model2_lower <- apply(sample_ev, 2, quantile, .025)

  # compute difference
  rmsr_mean  <- sqrt(mean(((ev_model1_mean  - ev_model2_mean ) ** 2)))
  rmsr_upper <- sqrt(mean(((ev_model1_upper - ev_model2_upper) ** 2)))
  rmsr_lower <- sqrt(mean(((ev_model1_lower - ev_model2_lower) ** 2)))
  fit <- (rmsr_mean + rmsr_upper + rmsr_lower) * (1e+4)

  return(fit)

}

factor_cor_level <- .3
loading_level    <- .4
oldfit <- 0
newfit <- 1
z_loadings <- c(500, 500, 500)

set.seed(1)
while(oldfit != newfit) {
  f <- optim(par = z_loadings, fn = rmsr, control = list(maxit = 20, REPORT = 1))
  z_loadings <- f$par
  oldfit     <- newfit
  newfit     <- f$value
  print(c(z_loadings, newfit))
  flush.console()
}
z_loadings <- sort(z_loadings, decreasing = TRUE)

loading_1 <- pnorm(z_loadings[1] / 1000) # .496
loading_2 <- pnorm(z_loadings[2] / 1000) # .299
loading_3 <- pnorm(z_loadings[3] / 1000) # .250
