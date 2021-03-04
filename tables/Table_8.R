# TABLE 8
# F = -2 to +2
# (unpublished)

results <- prepare_results_data()

idx <- which(names(results) %in% get_DV_names())

table_data <- NULL

for (n_cats_level in c(0, 4, 2)) {
for (w_error_level in c(.0, .1)) {

  d <- results

  d <- subset(d, n_cats == n_cats_level)
  d <- subset(d, w_error == w_error_level)
  d <- subset(d, n_factor > 1)

  dd <- d
  dd[, idx] <- dd[, idx] <= dd[["n_factor"]] - 2
  v1 <- as.matrix(apply(dd, 2, mean))

  dd <- d
  dd[, idx] <- dd[, idx] == dd[["n_factor"]] - 1
  v2 <- as.matrix(apply(dd, 2, mean))

  dd <- d
  dd[, idx] <- dd[, idx] == dd[["n_factor"]]
  v3 <- as.matrix(apply(dd, 2, mean))

  dd <- d
  dd[, idx] <- dd[, idx] == dd[["n_factor"]] + 1
  v4 <- as.matrix(apply(dd, 2, mean))

  dd <- d
  dd[, idx] <- dd[, idx] == dd[["n_factor"]] + 2
  v5 <- as.matrix(apply(dd, 2, mean))

  w   <- cbind(v1, v2, v3, v4, v5)
  DVs <- rownames(w)[rownames(w) %in% get_DV_names()]
  w <- w[c(
    "w_error",
    "n_cats",
    DVs
  ), ]
  w[DVs, ] <- w[DVs, ] * 100

  table_data <- rbind(table_data, w)

}}

write.csv(table_data, "tables/Table_8.csv")
