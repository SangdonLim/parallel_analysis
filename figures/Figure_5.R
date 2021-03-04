# Figure 5
# Eigenvalue similarity between different factor structures
# ------------------------------------------------------------------------------

rstudioapi::restartSession()

library(MASS)
library(devEMF)

# K = 2 ------------------------------------------------------------------------

factor_cor_value <- .3
loading_f1_value <- .588
loading_f2_value <- .409

n_trials   <- 1000

# Model 1
lambda_vec <- rep(.5, 4)
lambda_mat <- kronecker(diag(1, 2), lambda_vec)
factor_cor <- matrix(factor_cor_value, 2, 2)
diag(factor_cor) <- 1
population_cor <- lambda_mat %*% factor_cor %*% t(lambda_mat)
diag(population_cor) <- 1

sample_ev <- matrix(NA, n_trials, 8)
set.seed(1)
for (i in 1:n_trials) {
  sample_ev[i, ] <- eigen(cor(mvrnorm(100, rep(0, 8), population_cor)))$values
}
ev_panel1_model1_mean  <- apply(sample_ev, 2, mean)
ev_panel1_model1_upper <- apply(sample_ev, 2, quantile, .75)
ev_panel1_model1_lower <- apply(sample_ev, 2, quantile, .25)

# Model 2
lambda_vec <- rep(1, 4)
lambda_mat <- kronecker(diag(1, 2), lambda_vec)
lambda_mat[, 1] <- lambda_mat[, 1] * loading_f1_value
lambda_mat[, 2] <- lambda_mat[, 2] * loading_f2_value
factor_cor <- diag(1, 2)
population_cor <- lambda_mat %*% factor_cor %*% t(lambda_mat)
diag(population_cor) <- 1

sample_ev <- matrix(NA, n_trials, 8)
set.seed(1)
for (i in 1:n_trials) {
  sample_ev[i, ] <- eigen(cor(mvrnorm(100, rep(0, 8), population_cor)))$values
}
ev_panel1_model2_mean  <- apply(sample_ev, 2, mean)
ev_panel1_model2_upper <- apply(sample_ev, 2, quantile, .75)
ev_panel1_model2_lower <- apply(sample_ev, 2, quantile, .25)

# K = 3, 4 ---------------------------------------------------------------------

factor_cor_value <- .3
loading_value    <- .4
loading_f1_value <- .496
loading_f2_value <- .299
loading_f3_value <- .250

n_trials <- 1000

# Model 1
lambda_vec <- rep(loading_value, 3)
lambda_mat <- kronecker(diag(1, 4), lambda_vec)
factor_cor <- matrix(factor_cor_value, 4, 4)
diag(factor_cor) <- 1
population_cor <- lambda_mat %*% factor_cor %*% t(lambda_mat)
diag(population_cor) <- 1

sample_ev <- matrix(NA, n_trials, 12)
set.seed(1)
for (i in 1:n_trials) {
  sample_ev[i, ] <- eigen(cor(mvrnorm(100, rep(0, 12), population_cor)))$values
}
ev_panel2_model1_mean  <- apply(sample_ev, 2, mean)
ev_panel2_model1_upper <- apply(sample_ev, 2, quantile, .75)
ev_panel2_model1_lower <- apply(sample_ev, 2, quantile, .25)

# Model 2
lambda_vec <- rep(1, 4)
lambda_mat <- kronecker(diag(1,3), lambda_vec)
lambda_mat[, 1] <- lambda_mat[, 1] * loading_f1_value
lambda_mat[, 2] <- lambda_mat[, 2] * loading_f2_value
lambda_mat[, 3] <- lambda_mat[, 3] * loading_f3_value

population_cor <- lambda_mat %*% t(lambda_mat)
diag(population_cor) <- 1

sample_ev <- matrix(NA, n_trials, 12)
set.seed(1)
for (i in 1:n_trials) {
  sample_ev[i, ] <- eigen(cor(mvrnorm(100, rep(0, 12), population_cor)))$values
}
ev_panel2_model2_mean  <- apply(sample_ev, 2, mean)
ev_panel2_model2_upper <- apply(sample_ev, 2, quantile, .75)
ev_panel2_model2_lower <- apply(sample_ev, 2, quantile, .25)

# Plot -------------------------------------------------------------------------

for (img_type in c("eps", "emf", "svg")) {

  setEPS()
  if (img_type == "eps") {
    postscript("figures/Figure_5.eps", width = 6.8, height = 3.6)
  }
  if (img_type == "emf") {
    emf(
      "figures/Figure_5.emf", width = 6.8, height = 3.6,
      family = "Arial", coordDPI = 2540, emfPlus = FALSE
    )
  }
  if (img_type == "svg") {
    svg(
      "figures/Figure_5.svg", width = 6.8, height = 3.6
    )
  }

  par(
    mfrow = c(1, 2),
    mar = c(2, 3, 2, 1) + .1,
    ps = 8,
    mgp = c(2, 0.5, 0)
  )

  # Panel 1 -------------------------------------------------------------------

  plot(
    0, 0, xlim = c(1, 8), ylim = c(0, 2.5), type = "n",
    ylab = "Eigenvalue", xlab = NA,
    cex.lab = 1, cex.axis = 1, bty = "n", yaxp = c(0, 2.5, 5)
  )

  lines(ev_panel1_model1_mean , lwd = 1, lty = 2, col = "black")
  lines(ev_panel1_model1_upper, lwd = 1, lty = 2, col = "black")
  lines(ev_panel1_model1_lower, lwd = 1, lty = 2, col = "black")
  lines(ev_panel1_model2_mean , lwd = 1, lty = 3, col = "black")
  lines(ev_panel1_model2_upper, lwd = 1, lty = 3, col = "black")
  lines(ev_panel1_model2_lower, lwd = 1, lty = 3, col = "black")
  points(ev_panel1_model1_mean, lwd = 1, pch = 21, cex = .75, bg = "black")
  points(ev_panel1_model2_mean, lwd = 1, pch = 21, cex = .75, bg = "white")

  txt1 <- expression(paste("Correlated factors (", italic("k"), " = 2)"))
  txt2 <- expression(paste("Orthogonal factors (", italic("k"), " = 2)"))

  legend(
    x = 1 + (8 - 1) / 3.5, y = 2.6,
    legend = c(txt1, txt2),
    lty = c(2, 3),
    pch = 21, pt.cex = .75, pt.bg = c("black", "white"),
    lwd = 1,
    box.lty = 0,
    cex = 1,
    y.intersp = .75, x.intersp = .5, seg.len = 1.5
  )

  mtext("(a)", side = 3, line = 1, cex = 1)

  box(lwd = 1)

  # Panel 2 -------------------------------------------------------------------

  plot(
    0, 0, xlim = c(1, 12), ylim = c(0, 2.5), type = "n",
    ylab = "Eigenvalue", xlab = NA,
    cex.lab = 1, cex.axis = 1, bty = "n", yaxp = c(0, 2.5, 5)
  )

  lines(ev_panel2_model1_mean , lwd = 1, lty = 2, col = "black")
  lines(ev_panel2_model1_upper, lwd = 1, lty = 2, col = "black")
  lines(ev_panel2_model1_lower, lwd = 1, lty = 2, col = "black")
  lines(ev_panel2_model2_mean , lwd = 1, lty = 3, col = "black")
  lines(ev_panel2_model2_upper, lwd = 1, lty = 3, col = "black")
  lines(ev_panel2_model2_lower, lwd = 1, lty = 3, col = "black")
  points(ev_panel2_model1_mean, lwd = 1, pch = 21, cex = .75, bg = "black")
  points(ev_panel2_model2_mean, lwd = 1, pch = 21, cex = .75, bg = "white")

  txt1 <- expression(paste("Correlated factors (", italic("k"), " = 4)"))
  txt2 <- expression(paste("Orthogonal factors (", italic("k"), " = 3)"))

  legend(
    x = 1 + (12 - 1) / 3.5, y = 2.6,
    legend = c(txt1, txt2),
    lty = c(2, 3), lwd = 1,
    pch = 21, pt.cex = .75, pt.bg = c("black", "white"),
    box.lty = 0,
    cex = 1,
    y.intersp = .75, x.intersp = .5, seg.len = 1.5
  )

  mtext("(b)", side = 3, line = 1, cex = 1)

  box(lwd = 1)

  # Close ---------------------------------------------------------------------

  dev.off()

}
