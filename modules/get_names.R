#' return IV names
#'
#' @export
get_IV_names <- function() {
  tmp <- c(
    "n_obs",
    "n_factor",
    "r_factor",
    "w_error",
    "d_skew",
    "w_lambda",
    "n_cats",
    "rng_seed"
  )
  return(tmp)
}

#' return CV names
#'
#' @export
get_CV_names <- function() {
  tmp <- c(
    "idx_condition",
    "idx_trial"
  )
  return(tmp)
}

#' return DV names
#'
#' @export
get_DV_names <- function() {
  tmp <- c(
    "T_fm", "T_fu", "T_rm", "T_ru",
    "T_mm", "T_mu", "T_pm", "T_pu",
    "R_fm", "R_fu", "R_rm", "R_ru",
    "CD",
    "f_halt", "r_halt",
    "n_items",
    "n_try",
    "smoothed"
  )
  return(tmp)
}

#' return the number of IVs
#'
#' @export
get_IV_length <- function() {
  return(length(get_IV_names()))
}

#' return the number of CVs
#'
#' @export
get_CV_length <- function() {
  return(length(get_CV_names()))
}

#' return the number of DVs
#'
#' @export
get_DV_length <- function() {
  return(length(get_DV_names()))
}

#' return the index of IVs
#'
#' @param vec_names a vector containing IV names.
#'
#' @export
get_IV_idx <- function(vec_names) {
  return(vec_names %in% get_IV_names())
}

#' return the index of CVs
#'
#' @param vec_names a vector containing CV names.
#'
#' @export
get_CV_idx <- function(vec_names) {
  return(vec_names %in% get_CV_names())
}

#' return the index of DVs
#'
#' @param vec_names a vector containing DV names.
#'
#' @export
get_DV_idx <- function(vec_names) {
  return(vec_names %in% get_DV_names())
}

#' return full method names
#'
#' @param vec_names a vector containing DV names.
#'
#' @export
get_DV_full_names <- function(vec_names) {

  vec_names[which(vec_names == "T_fm")] <- "PA-PCA-m"
  vec_names[which(vec_names == "T_fu")] <- "PA-PCA-95"
  vec_names[which(vec_names == "T_rm")] <- "PA-PAF-m"
  vec_names[which(vec_names == "T_ru")] <- "PA-PAF-95"
  vec_names[which(vec_names == "T_mm")] <- "PA-MRFA-eg-m"  # eigenvalue-based
  vec_names[which(vec_names == "T_mu")] <- "PA-MRFA-eg-95" # eigenvalue-based
  vec_names[which(vec_names == "T_pm")] <- "PA-MRFA-m"     # ECV-based
  vec_names[which(vec_names == "T_pu")] <- "PA-MRFA-95"    # ECV-based
  vec_names[which(vec_names == "R_fm")] <- "RPA-PCA-m"
  vec_names[which(vec_names == "R_fu")] <- "RPA-PCA-95"
  vec_names[which(vec_names == "R_rm")] <- "RPA-PAF-m"
  vec_names[which(vec_names == "R_ru")] <- "RPA-PAF-95"
  vec_names[which(vec_names == "CD")]   <- "CD"

  return(vec_names)

}

#' return selected method names
#'
#' @export
get_selected_DVs <- function() {

  o <- c(
    "PA-PCA-m",
    "PA-PCA-95",
    "PA-PAF-95",
    "PA-MRFA-m",
    "RPA-PCA-95",
    "RPA-PAF-95",
    "CD"
  )
  return(o)

}
