# TABLE 3
# Overall Performance of the Selected PA Variants
# ------------------------------------------------------------------------------

library(reshape2)

# Load all modules
for (f in list.files("modules")) {
  source(file.path("modules", f))
}

results <- prepare_results_data()

# Create table data

table_data <- NULL

idx <- which(names(results) %in% get_DV_names())

for (DV in c("PC", "ME", "RMSE")) {
for (n_factor_level in 1:2) {
for (n_cats_level in 1:3) {

  d <- results
  if (n_cats_level == 1) { d <- subset(d, n_cats == 0) }
  if (n_cats_level == 2) { d <- subset(d, n_cats == 4) }
  if (n_cats_level == 3) { d <- subset(d, n_cats == 2) }
  if (n_factor_level == 1) { d <- subset(d, n_factor == 1) }
  if (n_factor_level == 2) { d <- subset(d, n_factor >  1) }

  if (dim(d)[1] > 0) {
    if (DV == "PC")   d[, idx] <-  d[, idx] == d[["n_factor"]]
    if (DV == "ME")   d[, idx] <-  d[, idx] -  d[["n_factor"]]
    if (DV == "RMSE") d[, idx] <- (d[, idx] -  d[["n_factor"]]) ** 2
    o <- t(t(c(apply(d, 2, mean))))
    if (DV == "RMSE")  o <- sqrt(o)
    if (DV == "PC")    o <- o * 100
    table_data <- cbind(table_data, o)
  }

}}}

idx <- which(rownames(table_data) %in% get_DV_names())
table_data <- table_data[idx, ]
rownames(table_data) <- get_DV_full_names(row.names(table_data))

table_data[,  1: 6] <- do_normal_rounding(table_data[,  1: 6], 1)
table_data[,  7:12] <- do_normal_rounding(table_data[,  7:12], 2)
table_data[, 13:18] <- do_normal_rounding(table_data[, 13:18], 2)

table_data_subset <- table_data[get_selected_DVs(), ]

header_rows <- matrix("", 3, 18)
header_rows[1, seq(1, 18, 6)] <- c(
  "Percentage correct",
  "Mean error",
  "RMSE"
)
header_rows[2, seq(1, 18, 6)] <- "K = 1"
header_rows[2, seq(4, 18, 6)] <- "K >= 2"
header_rows[3, ] <- rep(c("Cont.", "4-C", "2-C"), 6)

table_data        <- rbind(header_rows, table_data)
table_data_subset <- rbind(header_rows, table_data_subset)

write.table(table_data       , "tables/Table_3_full.csv"      , sep = ",", col.names = FALSE)
write.table(table_data_subset, "tables/Table_3_simplified.csv", sep = ",", col.names = FALSE)
