# TABLE 4
# Effect Size of the Selected PA Variants
# in the Multiple-Factor Model by Measurement Scale
# ------------------------------------------------------------------------------

library(reshape2)

# Load all modules
for (f in list.files("modules")) {
  source(file.path("modules", f))
}

compute_effect_size <- function(x) {

  x$etasq <-
    x$`Sum Sq` /
    sum(x$`Sum Sq`)
  x$size <- ""
  x$size[
    x$etasq >= 0.01
  ] <- "SMALL"
  x$size[
    x$etasq >= 0.06
  ] <- "MEDIUM"
  x$size[
    x$etasq >= 0.14
  ] <- "LARGE"

  return(x)

}

results <- prepare_results_data(bool = TRUE)

# Simplified version

table_data <- NULL

for (n_cats_level in c(0, 4, 2)) {

  d <- results
  d <- subset(d, d[["n_factor"]] != 1)
  d <- subset(d, d[["n_cats"]] == n_cats_level)

  PC_data <- aggregate(d, by = list(d[["idx_condition"]]), FUN = mean)[, -1]

  idx1 <- which(colnames(d) %in% get_IV_names())
  idx2 <- which(colnames(d) %in% get_CV_names())
  d <- melt(PC_data, id.vars = names(d)[c(idx1, idx2)], variable.name = "method")
  d[["method"]] <- get_DV_full_names(as.character(d[["method"]]))

  d <- subset(d, d[["method"]] %in% get_selected_DVs())

  idx_IVs <- c(idx1, which(colnames(d) == "idx_condition"))
  for (IV in idx_IVs) {
    d[, IV] <- as.factor(d[, IV])
  }

  # Arcsine transformation
  d[["value"]] <- asin(sqrt(d[["value"]]))

  model_terms <- c(get_IV_names()[1:6])
  model <- formula(
    sprintf(
      "value ~ %s + n_obs:w_error + r_factor:w_lambda + r_factor:n_factor + Error(idx_condition)",
      paste0(model_terms, collapse = " + ")
    )
  )

  o <- NULL
  for (DV in get_selected_DVs()) {
    dd           <- subset(d, method == DV)
    fit          <- aov(model, dd)
    bb           <- summary(fit)[[1]][[1]]
    bb$`Sum Sq`  <- bb$`Sum Sq` / sum(bb$`Sum Sq`)
    bb           <- bb[, "Sum Sq", drop = FALSE]
    bb           <- as.matrix(bb)
    colnames(bb) <- DV
    o <- cbind(o, bb)
  }

  o <- t(o)
  colnames(o) <- trimws(colnames(o))

  o <- o[, c(
    "w_error",
    "d_skew",
    "r_factor",
    "w_lambda",
    "n_factor",
    "n_obs",
    "n_obs:w_error",
    "r_factor:w_lambda",
    "n_factor:r_factor"
  )]

  o <- rbind(o, Average = apply(o, 2, mean))
  o[o < .01] <- -1
  o <- do_normal_rounding(o, 2)

  header_row <- matrix("", 1, 9)
  if (n_cats_level == 0) header_row[1, 1] <- "Continuous"
  if (n_cats_level == 4) header_row[1, 1] <- "Four-Category"
  if (n_cats_level == 2) header_row[1, 1] <- "Binary"
  o <- rbind(header_row, o)

  table_data <- rbind(table_data, o)

}

idx_weak   <- as.numeric(table_data) == -1
table_data[idx_weak] <- ""

colnames(table_data) <- c("E", "S", "R", "L", "K", "N", "E*N", "R*L", "R*K")

table_data <- cbind(Procedure = rownames(table_data), table_data)

write.table(
  table_data, "tables/Table_4_simplified.csv", sep = ",",
  col.names = TRUE, row.names = FALSE
)

# Full version

for (n_methods in c(13, 7)) {
for (n_cats_level in c(0, 4, 2)) {

  d <- results
  d <- subset(d, d[["n_factor"]] != 1)
  d <- subset(d, d[["n_cats"]] == n_cats_level)

  PC_data <- aggregate(d, by = list(d[["idx_condition"]]), FUN = mean)[, -1]

  idx1 <- which(colnames(d) %in% get_IV_names())
  idx2 <- which(colnames(d) %in% get_CV_names())
  d <- melt(PC_data, id.vars = names(d)[c(idx1, idx2)], variable.name = "method")
  d[["method"]] <- get_DV_full_names(as.character(d[["method"]]))

  idx_IVs <- c(idx1, which(colnames(d) == "idx_condition"))
  for (IV in idx_IVs) {
    d[, IV] <- as.factor(d[, IV])
  }

  # Arcsine transformation
  d[["value"]] <- asin(sqrt(d[["value"]]))

  model_terms <- c(get_IV_names()[1:6], "method")
  model <- formula(
    sprintf(
      "value ~ (%s) ^ 3 + Error(idx_condition)",
      paste0(model_terms, collapse = " + ")
    )
  )

  if (n_methods == 13) {
    # remove EKC data
    d <- subset(d, !(method %in% c("EKC-0", "EKC-1")))
  }
  if (n_methods == 7) {
    d <- subset(d, method %in% get_selected_DVs())
  }

  fit <- aov(model, d)

  table_data_between <- summary(fit)[[1]][[1]]
  table_data_within <- summary(fit)[[2]][[1]]

  table_data_between <- compute_effect_size(table_data_between)
  table_data_within  <- compute_effect_size(table_data_within)

  table_data_between <- cbind(
    term = trimws(rownames(table_data_between)),
    table_data_between
  )
  table_data_within <- cbind(
    term = trimws(rownames(table_data_within)),
    table_data_within
  )
  table_data <- rbind(table_data_between, table_data_within)

  # run separate ANOVAs for each PA method

  for (DV in get_DV_full_names(get_DV_names()[1:13])) {
    table_data[DV] <- NA
  }

  model_terms <- c(get_IV_names()[1:6])
  model <- formula(
    sprintf(
      "value ~ (%s) ^ 3 + Error(idx_condition)",
      paste0(model_terms, collapse = " + ")
    )
  )

  for (DV in unique(d$method)) {

    dd <- subset(d, method == DV)
    fit <- aov(model, dd)
    dd <- summary(fit)[[1]][[1]]
    dd <- compute_effect_size(dd)
    etasq <- dd$etasq
    etasq[(length(etasq) + 1):nrow(table_data)] <- NA
    table_data[[DV]] <- etasq

  }

  table_data[is.na(table_data)] <- ""

  for (j in c(3:7, 9:21)) {
    table_data[, j] <- do_normal_rounding(as.numeric(table_data[, j]), 6)
  }

  write.table(
    table_data,
    sprintf(
      "tables/Table_4_full_%scats_%smethods.csv",
      n_cats_level, n_methods
    ),
    sep = ",",
    col.names = TRUE, row.names = FALSE
  )


}}
