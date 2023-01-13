# Figure 3
# Percent correct of the selected PA procedures for the continuous variable
# ------------------------------------------------------------------------------

rstudioapi::restartSession()

library(reshape2)
library(devEMF)

# Load all modules
for (f in list.files("modules")) {
  source(file.path("modules", f))
}

results    <- prepare_results_data(bool = TRUE)
conditions <- get_conditions_matrix()
results    <- subset(
  results,
  n_factor != 1 & n_cats == 0
)

PC  <- aggregate(results, by = list(results[["idx_condition"]]), FUN = mean)
PC  <- PC[, -1]
idx <- colnames(PC) %in% get_DV_names()
PC[, idx] <- PC[, idx] * 100

arg_data  <- PC
arg_x_idx <- 1
x_levels  <- unique(arg_data[, arg_x_idx])
x_cells   <- length(x_levels)

cell_values      <- matrix(, 8, 3)
cell_values[, 1] <- c(0, 1, 0, 1, 0, 1, 0, 1)      # COR
cell_values[, 2] <- c(1, 1, 1, 1, 0, 0, 0, 0)      # LOAD
cell_values[, 3] <- c(0, 0, 1, 1, 0, 0, 1, 1) * .1 # ERROR

c_cells <- 4
r_cells <- 2

for (img_type in c("eps", "emf", "svg")) {

  setEPS()
  if (img_type == "eps") {
    postscript(
      "figures/Figure_3.eps", width = 6.8, height = 4.5,
      encoding = "ISOLatin1"
    )
  }
  if (img_type == "emf") {
    emf(
      "figures/Figure_3.emf", width = 6.8, height = 4.5,
      family = "Arial", coordDPI = 2540, emfPlus = FALSE
    )
  }
  if (img_type == "svg") {
    svg(
      "figures/Figure_3.svg", width = 6.8, height = 4.5
    )
  }

  par(
    mfrow = c(r_cells, c_cells),
    oma = c(3, 2, 2.5, 2), mar = c(.01, .01, .01, .01),
    cex = 1, las = 1, ps = 8, mgp = c(2, 0.5, 0)
  )

  for (cell_idx in 1:8) {

    tmp <- subset(
      arg_data,
      arg_data[["r_factor"]] == cell_values[cell_idx, 1] &
      arg_data[["w_lambda"]] == cell_values[cell_idx, 2] &
      arg_data[["w_error" ]] == cell_values[cell_idx, 3]
    )

    tmp <- aggregate(tmp, by = list(tmp[, arg_x_idx]), mean)[, -1]

    if (cell_idx %% 2 == 1) { par(mar = c(.01, 1, .01, .01)) }
    if (cell_idx %% 2 == 0) { par(mar = c(.01, .01, .01, 1)) }

    plot(
      tmp[, arg_x_idx], tmp[, arg_x_idx],
      ylim = c(0, 100), axes = FALSE, xlab = "", ylab = "", type = "n"
    )
    box()

    lty_cfg <- c(1, 2, 1, 1, 3)
    col_cfg <- c("black", "black", "black", "black", "black")
    bg_cfg  <- c("white", "white", "white", "black", "black")

    lines( tmp[, arg_x_idx], tmp[["T_fm"]], lty = lty_cfg[1], lwd = 1, col = col_cfg[1])
    lines( tmp[, arg_x_idx], tmp[["T_ru"]], lty = lty_cfg[2], lwd = 1, col = col_cfg[2])
    lines( tmp[, arg_x_idx], tmp[["R_fu"]], lty = lty_cfg[3], lwd = 1, col = col_cfg[3])
    points(tmp[, arg_x_idx], tmp[["R_fu"]], pch = 21, cex = .75, lwd = 1, bg = bg_cfg[3], col = col_cfg[3])
    lines( tmp[, arg_x_idx], tmp[["CD"]]  , lty = lty_cfg[4], lwd = 1, col = col_cfg[4])
    points(tmp[, arg_x_idx], tmp[["CD"]]  , pch = 21, cex = .75, lwd = 1, bg = bg_cfg[4], col = col_cfg[4])
    lines( tmp[, arg_x_idx], tmp[["T_pm"]], lty = lty_cfg[5], lwd = 1, col = col_cfg[5])

    if (cell_idx %% 2 == 1) {
      axis(2, at = seq(0, 100, 20), cex = 1, tck = -.05)
    }
    if (cell_idx %% 2 == 0) {
      axis(4, at = seq(0, 100, 20), cex = 1, tck = -.05, labels = FALSE)
    }
    if (cell_idx == 5 | cell_idx == 7) {
      axis(1, at = seq(100, 900, 200), tck = -.05)
    }

    if (cell_idx == 1) {
      legend(100, 55,
        legend = c(
          paste0("PA", "\uad", "PCA", "\uad", "m"),
          paste0("PA", "\uad", "PAF", "\uad", "95"),
          paste0("RPA", "\uad", "PCA", "\uad", "95"),
          paste0("CD"),
          paste0("PA", "\uad", "MRFA", "\uad", "m")
        ),
        pch = c(NA, NA, 21, 21, NA),
        pt.bg = bg_cfg, pt.cex = .75,
        lty = lty_cfg, lwd = 1,
        bty = "n",
        col = col_cfg,
        y.intersp = .75, x.intersp = .5, seg.len = 1.5
      )
    }
  }

  txt <- c(
    expression(~paste(".5" <= L) <= ".7"),
    expression(~paste(".3" <= L) <= ".5")
  )
  for (i in 1:r_cells) {
    mtext(
      txt[i], at = 1 - (i / r_cells) + (.5 / r_cells),
      line = 0, side = 4, outer = TRUE, las = 0, cex = 1
    )
  }

  txt <- c(
    expression(~paste(".0" <= R) <= ".3"),
    expression(~paste(".3" <= R) <= ".6"),
    expression(~paste(".0" <= R) <= ".3"),
    expression(~paste(".3" <= R) <= ".6")
  )
  mtext(txt[1], at = 0.138, line = .5, side = 3, outer = TRUE, las = 0, cex = 1)
  mtext(txt[2], at = 0.356, line = .5, side = 3, outer = TRUE, las = 0, cex = 1)
  mtext(txt[3], at = 0.638, line = .5, side = 3, outer = TRUE, las = 0, cex = 1)
  mtext(txt[4], at = 0.856, line = .5, side = 3, outer = TRUE, las = 0, cex = 1)

  txt <- c(
    "(a) Without Population Error",
    "(b) With Population Error"
  )
  panels <- 2
  for (i in 1:panels) {
    mtext(
      txt[i],
      at = (i / panels) - (.5 / panels),
      line = 1.5, side = 3, outer = TRUE, las = 0, cex = 1.25
    )
    mtext(
      "Sample Size",
      at = (i / panels) - (.5 / panels),
      line = 1.5, side = 1, outer = TRUE, las = 0, cex = 1.25
    )
  }

  mtext("Percent Correct", line = 1, side = 2, cex = 1.25, outer = TRUE, las = 0)

  dev.off()

}
