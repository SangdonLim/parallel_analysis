#' get reduced correlation matrix from a full correlation matrix
#'
#' @param arg_full A full correlation matrix.
#'
#' @export
reduce_pafa_smc <- function(arg_full) {
  smc_values <- 1 - (1 / diag(solve(arg_full)))
  arg_reduced <- arg_full
  diag(arg_reduced) <- smc_values
  return(arg_reduced)
}

#' get eigenvalues from a correlation matrix
#'
#' @param arg_full a full correlation matrix.
#'
#' @export
eig <- function(arg_full) {
  return(eigen(arg_full)$values)
}

#' get ECV values from a correlation matrix
#'
#' @param arg_full a full correlation matrix.
#'
#' @export
ecv <- function(arg_full) {
  tmp     <- eigen(arg_full)$values
  tmp_sum <- sum(tmp)
  return(tmp / tmp_sum)
}

#' get EKC thresholds from a correlation matrix
#'
#' @param arg_ev a vector containing observed eigenvalues.
#' @param arg_n_variables the number of variables.
#' @param arg_n_obs the sample size.
#'
#' @export
ekc <- function(arg_ev, arg_n_variables, arg_n_obs) {

  arg_gamma <- arg_n_variables / arg_n_obs
  ekc_thresholds <- rep(NA, arg_n_variables)
  tmp <- (1 + sqrt(arg_gamma)) ** 2

  for (j in 1:arg_n_variables) {
    num <- arg_n_variables - sum(arg_ev[0:(j - 1)])
    denom <- arg_n_variables - j + 1
    ekc_thresholds[j] <- (num / denom) * tmp
  }

  return(ekc_thresholds)

}

#' get estimated number of factors based on sample eigenvalues and threshold eigenvalues
#'
#' @param arg_signal_vec a vector containing observed eigenvalues.
#' @param arg_threshold_vec a vector containing expected eigenvalues.
#'
#' @export
vec_filter <- function(arg_signal_vec, arg_threshold_vec) {
  n_signals <- min(which(c((arg_signal_vec > arg_threshold_vec), FALSE) == FALSE)) - 1
  return(n_signals)
}

#' quantize a continuous-valued vector into a discrete-valued vector
#'
#' @param arg_float_vec a vector containing continuous values.
#' @param arg_tau_vec a vector defining cutpoints.
#'
#' @export
tau_knife <- function(arg_float_vec, arg_tau_vec) {
  arg_int_vec <- arg_float_vec * 0
  for (i in 1:length(arg_tau_vec)) {
    arg_int_vec <- arg_int_vec + as.integer(arg_float_vec > arg_tau_vec[i])
  }
  return(arg_int_vec)
}

#' quantize a continuous-valued matrix into a discrete-valued matrix
#'
#' @param arg_float_mat a matrix containing continuous values.
#' @param arg_tau_mat a matrix containing cutpoints for each variable.
#'
#' @export
tau_mesh <- function(arg_float_mat, arg_tau_mat) {

  n_variables <- dim(arg_float_mat)[2]
  arg_int_mat <- arg_float_mat * 0
  for (i in 1:n_variables) {
    arg_int_mat[, i] <- tau_knife(arg_float_mat[, i], arg_tau_mat[i, ])
  }

  return(arg_int_mat)

}

#' calculate maximum number of factors
#'
#' @param arg_n_variables the total number of variables in the dataset.
#'
#' @export
get_max_n_factors <- function(arg_n_variables) {
  max_n_factors <- floor(
    arg_n_variables
    - 0.5 * sqrt(8 * arg_n_variables + 1)
    + 0.5
  )
  return(max_n_factors)
}

#' read a file safely
#'
#' @param fname the file name to read.
#' @param d1 the number of expected rows.
#' @param d2 the number of expected columns.
#' @param ...
#'
#' @export
saferead <- function(fname, d1, d2, ...) {
  readagain <- TRUE
  while(readagain) {
    d <- try(read.table(fname, sep = ",", ...))
    if (length(d) == 1) {
      if (d[1] == 'Error in read.table(fname, sep = \",\") : no lines available in input\n') {
        d <- matrix(, 0, d2)
        break
      }
    }
    while (class(d) == "try-error") {
      Sys.sleep(5)
      d <- try(read.table(fname, sep = ",", ...))
    }
    if (is.na(d1)) {
      readagain <- !(dim(d)[2] == d2)
    } else if (is.na(d2)) {
      readagain <- !(dim(d)[1] == d1)
    } else {
      readagain <- !((dim(d)[1] == d1) && (dim(d)[2] == d2))
    }
    if (readagain) {
      Sys.sleep(5)
    }
  }
  return(d)
}

#' do "normal" rounding (round up from .5)
#'
#' @param x values to round.
#' @param digits digits to use.
#'
#' @examples
#' round(2.5)              # 2 (nearest even integer)
#' do_normal_rounding(2.5) # 3
#'
#' @export
do_normal_rounding <- function(x, digits) {
  x <- x + sign(x) * sqrt(.Machine$double.eps)
  x <- round(x, digits)
  return(x)
}
