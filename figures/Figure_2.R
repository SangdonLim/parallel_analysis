# Figure 2
# Effects of the number of items and factor loadings and factor correlation
# on the eigenvalues of a sample correlation matrix of the two-factor model
# ------------------------------------------------------------------------------

rstudioapi::restartSession()

library(MASS)
library(compiler)
library(devEMF)
enableJIT(3)

# Load all modules
for (f in list.files("modules")) {
  source(file.path("modules", f))
}

# Eigenvalues across factor loadings -------------------------------------------

n_trials <- 1000

# PA thresholds

set.seed(123456)

PA_ev_4 <- matrix(, n_trials, 8)
PA_ev_3 <- matrix(, n_trials, 7)

for (i in 1:n_trials) {
  PA_cor_4 <- cor(mvrnorm(100, rep(0, 8), diag(1, 8)))
  PA_cor_3 <- cor(mvrnorm(100, rep(0, 7), diag(1, 7)))
  PA_ev_4[i, ] <- eigen(PA_cor_4)$values
  PA_ev_3[i, ] <- eigen(PA_cor_3)$values
}

PA_full_95_4 <- apply(PA_ev_4, 2, quantile, prob = .95)
PA_full_95_3 <- apply(PA_ev_3, 2, quantile, prob = .95)

# Sample eigenvalues

set.seed(123456)

lambda_level <- seq(.4, .3, -.01)
sample_ev_mean_4 <-
sample_ev_mean_3 <-
PA_hitrate_4 <-
PA_hitrate_3 <-
  lambda_level * 0

for (i in seq_along(lambda_level)) {

  lambda_4 <- rep(.4, 4)
  lambda_4 <- kronecker(diag(1, 2), lambda_4)
  lambda_4[lambda_4[, 2] > 0, 2] <- lambda_level[i]
  lambda_3 <- lambda_4[1:7, ]

  population_cor_4 <- lambda_4 %*% t(lambda_4)
  population_cor_3 <- lambda_3 %*% t(lambda_3)
  diag(population_cor_4) <- 1
  diag(population_cor_3) <- 1

  sample_ev_4 <- sample_ev_3 <- rep(NA, n_trials)
  PA_hit_4    <- PA_hit_3    <- rep(NA, n_trials)

  for (ii in 1:n_trials) {
    ev_4 <- eigen(cor(mvrnorm(100, rep(0, 8), population_cor_4)))$values
    ev_3 <- eigen(cor(mvrnorm(100, rep(0, 7), population_cor_3)))$values
    PA_factors_4 <- vec_filter(ev_4, PA_full_95_4)
    PA_factors_3 <- vec_filter(ev_3, PA_full_95_3)
    PA_hit_4[ii] <- (PA_factors_4 == 2)
    PA_hit_3[ii] <- (PA_factors_3 == 2)
    sample_ev_4[ii] <- ev_4[2]
    sample_ev_3[ii] <- ev_3[2]
  }

  sample_ev_mean_4[i] <- mean(sample_ev_4)
  sample_ev_mean_3[i] <- mean(sample_ev_3)
  PA_hitrate_4[i] <- mean(PA_hit_4)
  PA_hitrate_3[i] <- mean(PA_hit_3)

  print(lambda_level[i])
  flush.console()

}

# Eigenvalues across factor correlation ----------------------------------------

set.seed(123456)

n_trials <- 1000

# PA thresholds

PA_ev <- matrix(, n_trials, 8)
for (i in 1:n_trials) {
  PA_ev[i, ] <- eigen(cor(mvrnorm(100, rep(0, 8), diag(1, 8))))$values
}

PA_full_95 <- apply(PA_ev, 2, quantile, prob = .95)

# Sample eigenvalues

set.seed(123456)

cor_level <- seq(0, .8 , .05)
sample_ev_mean_1 <-
sample_ev_mean_2 <-
PA_hitrate <-
  cor_level * 0

for (i in seq_along(cor_level)) {

  lambda <- rep(.4, 4)
  lambda <- kronecker(diag(1, 2), lambda)
  factor_cor <- matrix(cor_level[i], 2, 2)
  diag(factor_cor) <- 1
  population_cor <- lambda %*% factor_cor %*% t(lambda)
  diag(population_cor) <- 1

  sample_ev_1 <- sample_ev_2 <- rep(NA, 1000)
  PA_hit                     <- rep(NA, 1000)

  for (ii in 1:1000) {
    ev <- eigen(cor(mvrnorm(100, rep(0, 8), population_cor)))$values
    PA_factors <- vec_filter(ev, PA_full_95)
    PA_hit[ii] <- PA_factors == 2
    sample_ev_1[ii] <- ev[1]
    sample_ev_2[ii] <- ev[2]
  }

  sample_ev_mean_1[i] <- mean(sample_ev_1)
  sample_ev_mean_2[i] <- mean(sample_ev_2)
  PA_hitrate[i] <- mean(PA_hit)

  print(cor_level[i])
  flush.console()

}

# Plot -------------------------------------------------------------------------

for (img_type in c("eps", "emf", "svg")) {

  setEPS()
  if (img_type == "eps") {
    postscript("figures/Figure_2.eps", width = 6.8, height = 3)
  }
  if (img_type == "emf") {
    emf(
      "figures/Figure_2.emf", width = 6.8, height = 3,
      family = "Arial", coordDPI = 2540, emfPlus = FALSE
    )
  }
  if (img_type == "svg") {
    svg(
      "figures/Figure_2.svg", width = 6.8, height = 3
    )
  }

  par(
    mfrow = c(1, 2), mar = c(3, 3, 2, 1) + .1,
    ps = 8, mgp = c(2, 0.5, 0)
  )

  # Panel 1 --------------------------------------------------------------------

  plot(
    0, 0, type = "n", xlim = c(.4, .3), ylim = c(1.25, 1.5),
    xlab = "Factor loadings of the 2nd factor", ylab = "2nd eigenvalue",
    cex.lab = 1, cex.axis = 1, bty = "n"
  )

  mtext("(a)", side = 3, line = 1, cex = 1)

  points(lambda_level, sample_ev_mean_4, pch = 21, lwd = 1, cex = 1, bg = "white")
  points(lambda_level, sample_ev_mean_3, pch = 21, lwd = 1, cex = 1, bg = 'black')

  abline(h = PA_full_95_4[2], lty = 1, lwd = 1)
  abline(h = PA_full_95_3[2], lty = 2, lwd = 1)

  legend(
    x = .40, y = 1.5 + (0.25 / 30),
    legend = c("4 items for 2nd factor", "3 items for 2nd factor"),
    lty = c(1, 2),
    lwd = 1,
    pch = 21,
    cex = 1,
    box.lty = 0,
    pt.bg = c("white", "black"),
    y.intersp = .75, x.intersp = .5, seg.len = 1.5
  )

  box(lwd = 1)

  # Panel 2 --------------------------------------------------------------------

  plot(
    0, 0, xlim = c(0, 0.8), ylim = c(1, 2.5), type = "n",
    xlab = "Factor correlation", ylab = "Eigenvalue",
    cex.lab = 1, cex.axis = 1, bty = "n"
  )

  mtext("(b)", side = 3, line = 1, cex = 1)

  points(cor_level, sample_ev_mean_1, pch = 21, lwd = 1, cex = 1, bg = "white")
  points(cor_level, sample_ev_mean_2, pch = 21, lwd = 1, cex = 1, bg = "black")

  abline(h = PA_full_95[1], lwd = 1, lty = 1)
  abline(h = PA_full_95[2], lwd = 1, lty = 2)

  legend(
    x = .0, y = 2.5 + (1.5 / 30),
    legend = c("1st eigenvalue", "2nd eigenvalue"),
    lty = c(1,2),
    lwd = 1,
    pch = 21,
    cex = 1,
    box.lty = 0,
    pt.bg = c("white", "black"),
    y.intersp = .75, x.intersp = .5, seg.len = 1.5
  )

  box(lwd = 1)

  # Close ----------------------------------------------------------------------

  dev.off()

}
