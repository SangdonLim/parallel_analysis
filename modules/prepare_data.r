#' Prepare data
#'
#' Prepare results data for analysis.
#'
#' @export
prepare_results_data <- function(bool = FALSE) {

  results    <- read.csv("analysis/results.csv")
  conditions <- get_conditions_matrix()

  IVs     <- conditions[results[["idx_condition"]], ]
  results <- cbind(IVs, results)
  idx_DVs <- which(colnames(results) %in% get_DV_names())

  sum(results[["smoothed"]] == -1) # 42000, number of continuous datasets
  sum(results[["smoothed"]] == 0)  # 70655, # of datasets where smoothing was not performed
  sum(results[["smoothed"]] == 1)  # 13345, # of datasets where smoothing was performed  

  # apply maximum k
  max_n_factors <- get_max_n_factors(results[["n_items"]])
  for (DV in idx_DVs) {
    idx_cases <- which(results[, DV] == -1)
    # cases where the estimated k exceeded the maximum possible k
    results[idx_cases, DV] <- max_n_factors[idx_cases]
    # replace with the maximum possible k
  }

  # apply halting rule
  results[["R_rm"]] <- pmin(results[["R_rm"]], results[["r_halt"]])
  results[["R_ru"]] <- pmin(results[["R_ru"]], results[["r_halt"]])

  # convert to bool if requested
  if (bool) {
    results[, get_DV_names()] <-
    results[, get_DV_names()] == results[["n_factor"]]
  }

  # remove variables not needed for analysis
  results <- subset(
    results,
    select = -c(
      rng_seed,
      idx_trial,
      f_halt, r_halt,
      n_items, n_try, smoothed
    )
  )

  return(results)

}
