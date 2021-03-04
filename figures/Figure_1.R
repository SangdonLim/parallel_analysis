# Figure 1
# Graphical illustrations of
# the revised parallel analysis and the comparison data method
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
patch_functions()

# SAMPLE -----------------------------------------------------------------------

set.seed(123456)

lambda <- rep(.8, 8)
lambda <- kronecker(diag(1, 2), lambda)
lambda <- lambda[1:12, ]
lambda[, 2] <- lambda[, 2] / .8 * .5
factor_cor <- matrix(.5, 2, 2)
diag(factor_cor) <- 1
population_cor <- lambda %*% factor_cor %*% t(lambda)
diag(population_cor) <- 1

n_variables <- dim(lambda)[1]
sample_data <- mvrnorm(100, rep(0, n_variables), population_cor)
sample_cor <- cor(sample_data)

# TPA & RPA --------------------------------------------------------------------

TPA_cor       <- diag(1, n_variables)

sample_ev     <- eigen(sample_cor)$values
sample_lambda <- factanal(covmat = sample_cor, factors = 1, n.obs = 100)$loadings[]
RPA_cor       <- sample_lambda %*% t(sample_lambda)
diag(RPA_cor) <- 1

n_bootstrap_samples <- 1000

RPA_ev <-
TPA_ev <- matrix(NA, n_bootstrap_samples, n_variables)

for (i in 1:n_bootstrap_samples) {
  RPA_ev[i, ] <- eigen(cor(mvrnorm(100, rep(0, n_variables), RPA_cor)))$values
  TPA_ev[i, ] <- eigen(cor(mvrnorm(100, rep(0, n_variables), TPA_cor)))$values
}

breaks        <- seq(0.8, 1.7, 0.02)
RPA_histogram <- hist(RPA_ev[, 2], breaks = breaks)
RPA_full_95   <- quantile(RPA_ev[, 2], probs = .95)
TPA_histogram <- hist(TPA_ev[, 2], breaks = breaks)
TPA_full_95   <- quantile(TPA_ev[, 2], probs = .95)

# CD ---------------------------------------------------------------------------

set.seed(123456)

ev_rmsr    <- CDfixedseed(sample_data, 3)
F1_density <- density(ev_rmsr[, 1], bw = 0.02)
F2_density <- density(ev_rmsr[, 2], bw = 0.02)
F3_density <- density(ev_rmsr[, 3], bw = 0.02)

# Plot -------------------------------------------------------------------------

for (img_type in c("eps", "emf", "svg")) {

  setEPS()
  if (img_type == "eps") {
    postscript("figures/Figure_1.eps", width = 6.8, height = 3)
  }
  if (img_type == "emf") {
    emf(
      "figures/Figure_1.emf", width = 6.8, height = 3,
      family = "Arial", coordDPI = 2540, emfPlus = FALSE
    )
  }
  if (img_type == "svg") {
    svg(
      "figures/Figure_1.svg", width = 6.8, height = 3
    )
  }

  par(
    mfrow = c(1, 2), mar = c(3, 3, 2, 1) + .1,
    ps = 8, mgp = c(2, 0.5, 0)
  )

  # Panel 1 --------------------------------------------------------------------

  hist(
    TPA_ev[, 2], col = rgb(1, 1, 1),
    breaks = breaks, ylim = c(0, 200),
    lty = 0,
    main = "",
    xlab = "Second Eigenvalue",
    ylab = "Frequency",
    cex.lab = 1, cex.axis = 1,
    bty = "n", yaxs = "i", yaxp = c(0, 200, 4)
  )

  mtext("(a)", side = 3, line = 1, cex = 1)

  hist(TPA_ev[, 2], col = rgb(.8, .8, .8), breaks = breaks, lty = 0, add = TRUE)
  hist(RPA_ev[, 2], col = rgb(.4, .4, .4), breaks = breaks, lty = 0, add = TRUE)

  lines(x = rep(sample_ev[2], 2), y = c(0, 1000), lwd = 1)
  lines(x = rep(TPA_full_95, 2) , y = c(0, 1000), lwd = 1, lty = 3)
  lines(x = rep(RPA_full_95, 2) , y = c(0, 1000), lwd = 1, lty = 3)

  txt1 <- "RPA w/ 95th percentile"
  txt2 <- "PA w/ 95th percentile"
  txt3 <- expression(paste("Sample (", italic("k"), " = 2)"))
  legend(
    x = 0.8, y = 200,
    c(txt1, txt2, txt3),
    pt.bg = c(rgb(.4, .4, .4), rgb(.8, .8, .8)),
    pch = c(22, 22, NA), pt.cex = 1, pt.lwd = 1, cex = 1,
    lty = c(3, 3, 1), lwd = 1,
    bty = "o", bg = "white",
    box.lty = 0, box.lwd = 0, box.col = "white",
    y.intersp = .75, x.intersp = .5, seg.len = 1.5
  )

  box(lwd = 1)

  # Panel 2 --------------------------------------------------------------------

  plot(
    F1_density,
    main = "",
    xlab = "RMSR between Sample and Bootstrapped Eigenvalues",
    ylab = "Density",
    cex.lab = 1, cex.axis = 1,
    bty = "n", type = "n",
    ylim = c(0, 15), yaxs = "i", yaxp = c(0, 15, 3)
  )

  mtext("(b)", side = 3, line = 1, cex = 1)

  lines(F1_density, lwd = 1, lty = 1)
  lines(F2_density, lwd = 1, lty = 2)
  lines(F3_density, lwd = 1, lty = 3)

  txt1 <- expression(paste("RMSR w/ ", italic("k"), " = 1 bootstrap"))
  txt2 <- expression(paste("RMSR w/ ", italic("k"), " = 2 bootstrap"))
  txt3 <- expression(paste("RMSR w/ ", italic("k"), " = 3 bootstrap"))

  legend(
    "topleft",
    c(txt1, txt2, txt3),
    cex = 1, lty = c(1, 2, 3), lwd = 1,
    bty = "n", y.intersp = .75, x.intersp = .5, seg.len = 1.5
  )

  box(lwd = 1)

  # Close ----------------------------------------------------------------------

  dev.off()

}
