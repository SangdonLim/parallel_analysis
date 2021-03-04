# Conditions
get_conditions_matrix <- function() {

  IV1 <- c(100, 300, 500, 700, 900) # n_obs    # Sample size
  IV2 <- c(1, 2, 4, 6)              # n_factor # Number of factors
  IV3 <- c(0, 1)                    # r_factor # Factor correlation
  IV4 <- c(0, .1)                   # w_error  # Population Error
  IV5 <- c(0, 1)                    # d_skew   # Nonnormality
  IV6 <- c(0, 1)                    # w_lambda # Factor loading
  IV7 <- c(0, 2, 4)                 # n_cats   # Response scale (0 = continuous)
  IVs1 <- expand.grid(IV1, IV2, IV3, IV4, IV5, IV6, IV7)

  IV5 <- c(2)
  IVs2 <- expand.grid(IV1, IV2, IV3, IV4, IV5, IV6, IV7)

  IVs <- rbind(IVs1, IVs2)
  IVs <- cbind(IVs, 1:dim(IVs)[1])
  colnames(IVs) <- get_IV_names()

  idx <- which((IVs$n_factor == 1) & (IVs$r_factor == 1))
  IVs <- IVs[-idx, ]

  conditions <- IVs

  return(conditions)

}

# Constants
const_n_conditions <- function() {
  n.conditions <- dim(get_conditions_matrix())[1]
  return(n.conditions)
}
const_n_trials     <- function() {
  return(100)
}
