# TABLE 5
# Performance of the Selected PA Variants in the Multiple-Factor Model
# for the Continuous Variable
# TABLE 6
# Performance of the Selected PA Procedures in the Multiple-Factor Model
# for the Four-Category Ordinal Variable
# TABLE 7
# Performance of the Selected PA Variants in the Multiple-Factor Model
# for the Binary Variable
# ------------------------------------------------------------------------------

# Load all modules
for (f in list.files("modules")) {
  source(file.path("modules", f))
}

results <- prepare_results_data(bool = FALSE)

for (n_cats_level in c(0, 4, 2)) {

  table_data <- NULL

  for (DV in c("PC", "ME", "RMSE", "RANGE")) {

    d <- results
    d <- subset(d, n_factor > 1)
    d <- subset(d, d[["n_cats"]] == n_cats_level)
    idx_DVs <- which(names(d) %in% get_DV_names())

    if (DV == "PC")    d[, idx_DVs] =  d[, idx_DVs]     == d[["n_factor"]]
    if (DV == "ME")    d[, idx_DVs] =  d[, idx_DVs]     -  d[["n_factor"]]
    if (DV == "RMSE")  d[, idx_DVs] = (d[, idx_DVs]     -  d[["n_factor"]]) ** 2
    if (DV == "RANGE") d[, idx_DVs] = (d[, idx_DVs]     == d[["n_factor"]]) |
                                      (d[, idx_DVs] + 1 == d[["n_factor"]]) |
                                      (d[, idx_DVs] - 1 == d[["n_factor"]])

    o0 <- t(t(c(0, apply(d, 2, mean))))
    o1 <- t(aggregate(d, by = list(d[["w_error"]]  ), FUN = mean))
    o2 <- t(aggregate(d, by = list(d[["d_skew"]]   ), FUN = mean))
    o3 <- t(aggregate(d, by = list(d[["r_factor"]] ), FUN = mean))
    o4 <- t(aggregate(d, by = list(d[["w_lambda"]] ), FUN = mean))
    o4 <- o4[, 2:1]
    o5 <- t(aggregate(d, by = list(d[["n_factor"]] ), FUN = mean))
    o6 <- t(aggregate(d, by = list(d[["n_obs"]]    ), FUN = mean))

    o <- cbind(o0, o1, o2, o3, o4, o5, o6)

    idx_DVs <- rownames(o) %in% get_DV_names()
    header_rows <- o[1, , drop = FALSE]
    o <- o[idx_DVs, ]

    if (DV == "PC")    o <- o * 100
    if (DV == "RMSE")  o <- sqrt(o)
    if (DV == "RANGE") o <- o * 100

    if (DV == "PC")    o <- do_normal_rounding(o, 1)
    if (DV == "ME")    o <- do_normal_rounding(o, 2)
    if (DV == "RMSE")  o <- do_normal_rounding(o, 2)
    if (DV == "RANGE") o <- do_normal_rounding(o, 1)

    header_rows <- matrix("", 1, dim(o)[2])
    if (DV == "PC")    header_rows[2] <- "Percent correct"
    if (DV == "ME")    header_rows[2] <- "Mean error"
    if (DV == "RMSE")  header_rows[2] <- "Root mean squared error"
    if (DV == "RANGE") header_rows[2] <- "Percent correct (within +-1 range)"

    o <- rbind(header_rows, o)
    table_data <- rbind(table_data, o)

  }

  rownames(table_data) <- get_DV_full_names(rownames(table_data))
  header_rows <- matrix("", 2, 18)
  header_rows[1,  1]  <- "Overall"
  header_rows[1,  2]  <- "Population error"
  header_rows[1,  4]  <- "Nonsymmetry"
  header_rows[1,  7]  <- "Factor correlation"
  header_rows[1,  9]  <- "Factor loading"
  header_rows[1, 11]  <- "Number of factors"
  header_rows[1, 14]  <- "Sample size"
  header_rows[2,  2: 3] <- c("No", "Yes")
  header_rows[2,  4: 6] <- 0:2
  header_rows[2,  7: 8] <- c("Weak", "Strong")
  header_rows[2,  9:10] <- c("High", "Low")
  header_rows[2, 11:13] <- seq(2, 6, 2)
  header_rows[2, 14:18] <- seq(100, 900, 200)
  table_data <- rbind(header_rows, table_data)

  DVs_to_remove <- setdiff(get_DV_full_names(get_DV_names()), get_selected_DVs())
  table_data_subset <- table_data[-which(rownames(table_data) %in% DVs_to_remove), ]
  table_data_subset <- table_data_subset[, -1]
  table_data_subset <- table_data_subset[-c(27:34), ]

  if (n_cats_level == 0) {
    table_number <- 5
  }
  if (n_cats_level == 4) {
    table_number <- 6
  }
  if (n_cats_level == 2) {
    table_number <- 7
  }

  write.table(
    table_data,
    sprintf("tables/Table_%s_full.csv", table_number),
    sep = ",", col.names = FALSE
  )
  write.table(
    table_data_subset,
    sprintf("tables/Table_%s_simplified.csv", table_number),
    sep = ",", col.names = FALSE
  )

}
