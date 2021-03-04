# Main simulation script
# ------------------------------------------------------------------------------
# Note:
# Simulation results may be slightly different from stored results
# This is mainly due to floating-point differences in generated data
# ------------------------------------------------------------------------------

# Load all modules
for (f in list.files("modules")) {
  source(file.path("modules", f))
}
get_package("DA.MRFA", "1.1.2")

library(mnormt)
library(psych)
library(DA.MRFA)
library(RGenData)
library(mvnfast)
library(compiler)
library(progress)
library(parallel)
library(doParallel)

enableJIT(3)

# Prepare functions
patch_functions()

# Initialize parallel cluster
n_cores <- detectCores() - 2
cl <- makeCluster(n_cores)
registerDoParallel(cl)
message(sprintf(
  "Using %s threads", n_cores
))
clusterExport(cl, c(
  "NNpatched", "VMpatched",
  "CDpatched", "GDpatched",
  "mrfa", "rmvn", "FactorAnalysis", "polychoric")
)

# Initialize task list
conditions <- get_conditions_matrix()
task_list <- expand.grid(
  idx_condition = 1:dim(conditions)[1],
  idx_trial     = 1:100
)
n_tasks <- dim(task_list)[1]
tasks <- 1:n_tasks

# Begin main loop --------------------------------------------------------------

results <- foreach(
  task = tasks, .combine = rbind
) %dopar% {

  PA_loops <- 100

  idx_condition <- task_list$idx_condition[task]
  idx_trial     <- task_list$idx_trial[task]
  fn <- sprintf("results/%s_%s.csv", idx_condition, idx_trial)
  if (file.exists(fn)) {
    return(NULL)
  }

  # create IV variables and assign their values
  for (IV in get_IV_names()) {
    assign(IV, conditions[idx_condition, IV])
  }

  # ----------------------------------------------------------------------------
  # Generate a sample dataset
  # ----------------------------------------------------------------------------

  set.seed((rng_seed * 111111 + idx_trial) %% (2 ** 31))

  # data generation for continuous-valued data (n_cats = 0) --------------------

  if (n_cats == 0) {

    if (d_skew == 0) d_kurt <- 0
    if (d_skew == 1) d_kurt <- 1.5
    if (d_skew == 2) d_kurt <- 6

    n_try <- 0
    ok <- FALSE
    while (!ok) {
      n_try <- n_try + 1
      X_float <- try(simulate_sample(n_obs, n_factor, r_factor, w_lambda, w_error, d_skew, d_kurt))
      ok <- !inherits(X_float, "try-error")
    }

  }

  # data generation for discrete-valued data (n_cats = 2 or 4) -----------------

  if (n_cats > 0) {

    if (n_cats == 2 & d_skew == 0) tau <- c(-Inf, 0     , Inf)
    if (n_cats == 2 & d_skew == 1) tau <- c(-Inf, 0.5936, Inf)
    if (n_cats == 2 & d_skew == 2) tau <- c(-Inf, 1.0518, Inf)
    if (n_cats == 4 & d_skew == 0) tau <- c(-Inf,-1.5   , 0,      1.5   , Inf)
    if (n_cats == 4 & d_skew == 1) tau <- c(-Inf, 0.1678, 0.6873, 1.2513, Inf)
    if (n_cats == 4 & d_skew == 2) tau <- c(-Inf, 0.7515, 1.1341, 1.5980, Inf)

    n_try <- 0
    ok <- FALSE
    while (!ok) {
      n_try <- n_try + 1
      X_float <- try(simulate_sample(n_obs, n_factor, r_factor, w_lambda, w_error, 0, 0))
      ok <- !inherits(X_float, "try-error")
    }

    tau_mat <- matrix(tau, dim(X_float)[2], length(tau), byrow = TRUE)
    tau_mat[seq(2, dim(X_float)[2], 2), ] <-
    -tau_mat[seq(2, dim(X_float)[2], 2), ]

    X_int <- X_float * 0
    X_int <- tau_mesh(X_float, tau_mat)

  }

  # ----------------------------------------------------------------------------
  # Prepare sample data
  # ----------------------------------------------------------------------------

  set.seed((rng_seed * 123456 + idx_trial) %% (2 ** 31))

  results_IV <- conditions[idx_condition, get_IV_names()]
  n_obs   <- dim(X_float)[1]
  n_items <- dim(X_float)[2]

  results_CV <- data.frame(t(rep(-1, get_CV_length())))
  colnames(results_CV) <- get_CV_names()
  results_CV["idx_condition"] <- idx_condition
  results_CV["idx_trial"]     <- idx_trial

  results_DV <- data.frame(t(rep(-1, get_DV_length())))
  colnames(results_DV)  <- get_DV_names()
  results_DV["n_try"] <- n_try
  results_DV["n_items"] <- n_items

  # sample: get Pearson correlations
  if (n_cats == 0) {
    X_rf <- cor(X_float)
    results_DV["smoothed"] <- -1
  }

  # sample: get polychoric correlations
  if (n_cats >  0) {
    X_poly <- psych::polychoric(X_int, smooth = FALSE)
    X_tau <- X_poly[[2]]
    X_tau <- cbind(-Inf, X_tau, Inf)
    X_rf_presmooth <- X_poly[[1]]
    X_rf_postsmooth <- psych::cor.smooth(X_rf_presmooth)
    results_DV["smoothed"] <- !all(X_rf_presmooth == X_rf_postsmooth)
    # if identical then it means smoothing was not needed -> not smoothed
    # if not identical then it means smoothing was needed -> smoothed
    X_rf <- X_rf_postsmooth
  }

  # sample: get SMC-reduced correlations
  X_rr <- reduce_pafa_smc(X_rf)

  # sample: get MRFA-reduced correlations
  ok <- FALSE
  while (!ok) {
    X_rm <- try(mrfa(X_rf, dimensionality = n_items - 1, display = FALSE)$Matrix)
    ok <- !inherits(X_rm, "try-error")
  }

  # sample: get eigenvalues
  X_rf_ev  <- eig(X_rf)
  X_rr_ev  <- eig(X_rr)
  X_rm_ev  <- eig(X_rm)
  X_rp_ev  <- X_rm_ev / sum(X_rm_ev)

  results_DV["f_halt"] <- max(which(X_rf_ev > 1))
  results_DV["r_halt"] <- max(which(X_rr_ev > 0))

  # ----------------------------------------------------------------------------
  # Perform PA
  # ----------------------------------------------------------------------------

  set.seed((rng_seed * 234567 + idx_trial) %% (2 ** 31))

  PA_float <-
  PA_int   <-
  PA_rf    <-
  PA_rr    <-
  PA_rm    <- vector("list", PA_loops)

  # PA: make internal datasets
  for (i in 1:PA_loops) {
    PA_float[[i]] <- rmvn(n_obs, rep(0, n_items), diag(1, n_items))
  }

  # PA: get internal Pearson correlations
  if (n_cats == 0) {
    PA_rf <- lapply(PA_float, cor)
  }

  # PA: get internal polychoric correlations
  if (n_cats > 0) {
    for (i in 1:PA_loops) {
      PA_int[[i]] <- tau_mesh(PA_float[[i]], X_tau)
      PA_rf[[i]]  <- polychoric(PA_int[[i]], smooth = TRUE)[[1]]
      # Regenerate PA internal random data, if some variables have zero variance
      while (dim(PA_rf[[i]])[1] != n_items) {
        PA_float[[i]] <- rmvn(n_obs, rep(0, n_items), diag(1, n_items))
        PA_int[[i]] <- tau_mesh(PA_float[[i]], X_tau)
        PA_rf[[i]]  <- polychoric(PA_int[[i]], smooth = TRUE)[[1]]
      }
    }
  }

  # PA: get SMC-reduced internal correlations
  PA_rr <- lapply(PA_rf, reduce_pafa_smc)

  # PA: get MRFA-reduced internal correlations
  for (i in 1:PA_loops) {
    ok <- FALSE
    while (!ok) {
      PA_rm[[i]] <- try(
        mrfa(PA_rf[[i]], dimensionality = n_items - 1, display = FALSE)$Matrix
      )
      ok <- !inherits(PA_rm[[i]], "try-error")
    }
  }

  # PA: get eigenvalue distributions
  PA_rf_ev <- do.call(rbind, lapply(PA_rf, eig))
  PA_rr_ev <- do.call(rbind, lapply(PA_rr, eig))
  PA_rm_ev <- do.call(rbind, lapply(PA_rm, eig))
  PA_rp_ev <- do.call(rbind, lapply(PA_rm, ecv))

  # PA: get threshold eigenvalues
  PA_cut_rfm <- apply(PA_rf_ev, 2, mean)
  PA_cut_rrm <- apply(PA_rr_ev, 2, mean)
  PA_cut_rmm <- apply(PA_rm_ev, 2, mean)
  PA_cut_rpm <- apply(PA_rp_ev, 2, mean)
  PA_cut_rfu <- apply(PA_rf_ev, 2, quantile, probs = .95)
  PA_cut_rru <- apply(PA_rr_ev, 2, quantile, probs = .95)
  PA_cut_rmu <- apply(PA_rm_ev, 2, quantile, probs = .95)
  PA_cut_rpu <- apply(PA_rp_ev, 2, quantile, probs = .95)

  # PA: get estimated number of factors
  results_DV["T_fm"] <- vec_filter(X_rf_ev, PA_cut_rfm) # PA-PCA-m
  results_DV["T_rm"] <- vec_filter(X_rr_ev, PA_cut_rrm) # PA-PAF-m
  results_DV["T_mm"] <- vec_filter(X_rm_ev, PA_cut_rmm) # PA-MRFA-m
  results_DV["T_pm"] <- vec_filter(X_rp_ev, PA_cut_rpm) # PA-MRFA-ecv-m
  results_DV["T_fu"] <- vec_filter(X_rf_ev, PA_cut_rfu) # PA-PCA-95
  results_DV["T_ru"] <- vec_filter(X_rr_ev, PA_cut_rru) # PA-PAF-95
  results_DV["T_mu"] <- vec_filter(X_rm_ev, PA_cut_rmu) # PA-MRFA-95
  results_DV["T_pu"] <- vec_filter(X_rp_ev, PA_cut_rpu) # PA-MRFA-ecv-95

  # ----------------------------------------------------------------------------
  # Perform RPA
  # ----------------------------------------------------------------------------

  set.seed((rng_seed * 345678 + idx_trial) %% (2 ** 31))

  flags <- rep(FALSE, 4)
  max_n_factors <- get_max_n_factors(n_items)

  for (k in 0:max_n_factors) {

    RPA_float <-
    RPA_int   <-
    RPA_rf    <-
    RPA_rr    <- vector("list", PA_loops)

    if (k == 0) {

      RPA_rf <- PA_rf
      RPA_rr <- PA_rr

    }

    if (k > 0) {

      # Perform k-factor EFA and get factor loadings
      ok <- FALSE
      n_starts <- 1 # number of starting points in EFA
      while (!ok) {
        # this has a random component
        RPA_weights <- try(factanal(
          covmat = X_rf, n_obs = n_obs, factors = k, rotation = "none", nstart = n_starts)$loadings[])
        if (class(RPA_weights)[1] == "try-error") n_starts <- n_starts * 2
        if (class(RPA_weights)[1] == "matrix")    ok <- TRUE
        if (n_starts > 1024) break
      }
      if (n_starts > 1024) break

      # Reproduce the correlation matrix with factor loadings
      RPA_pop_rr <-
      RPA_pop_rf <- RPA_weights %*% t(RPA_weights)
      diag(RPA_pop_rf) <- 1

      # RPA: make internal datasets
      for (i in 1:PA_loops) {
        RPA_float[[i]] <- rmvn(n_obs, rep(0, n_items), RPA_pop_rf)
      }

      # RPA: get internal Pearson correlations
      if (n_cats == 0) {
        RPA_rf <- lapply(RPA_float, cor)
      }

      # RPA: get internal polychoric correlations
      if (n_cats >  0) {
        for (i in 1:PA_loops) {
          RPA_int[[i]] <- tau_mesh(RPA_float[[i]], X_tau)
          RPA_rf[[i]]  <- polychoric(RPA_int[[i]], smooth = TRUE)[[1]]
          while(dim(RPA_rf[[i]])[1] != n_items) {
            RPA_float[[i]] <- rmvn(n_obs, rep(0, n_items), RPA_pop_rf)
            RPA_int[[i]]   <- tau_mesh(RPA_float[[i]], X_tau)
            RPA_rf[[i]]    <- polychoric(RPA_int[[i]], smooth = TRUE)[[1]]
          }
        }
      }

      # RPA: get SMC-reduced internal correlations
      RPA_rr <- lapply(RPA_rf, reduce_pafa_smc)

    }

    # RPA: get eigenvalue distributions
    RPA_rf_ev <- do.call(rbind, lapply(RPA_rf, eig))
    RPA_rr_ev <- do.call(rbind, lapply(RPA_rr, eig))

    # RPA: get threshold eigenvalues
    RPA_cut_rfm <- apply(RPA_rf_ev, 2, mean)[k + 1]
    RPA_cut_rrm <- apply(RPA_rr_ev, 2, mean)[k + 1]
    RPA_cut_rfu <- apply(RPA_rf_ev, 2, quantile, probs = .95)[k + 1]
    RPA_cut_rru <- apply(RPA_rr_ev, 2, quantile, probs = .95)[k + 1]

    # RPA: get estimated number of factors
    if ((flags[1] == FALSE) && (X_rf_ev[k + 1] < RPA_cut_rfu)) {
      flags[1] <- TRUE
      results_DV["R_fu"] <- k
    }
    if ((flags[2] == FALSE) && (X_rr_ev[k + 1] < RPA_cut_rru)) {
      flags[2] <- TRUE
      results_DV["R_ru"] <- k
    }
    if ((flags[3] == FALSE) && (X_rf_ev[k + 1] < RPA_cut_rfm)) {
      flags[3] <- TRUE
      results_DV["R_fm"] <- k
    }
    if ((flags[4] == FALSE) && (X_rr_ev[k + 1] < RPA_cut_rrm)) {
      flags[4] <- TRUE
      results_DV["R_rm"] <- k
    }

    if (sum(flags) == 4) break

  }

  # ----------------------------------------------------------------------------
  # Perform CD
  # ----------------------------------------------------------------------------

  set.seed((rng_seed * 456789 + idx_trial) %% (2 ** 31))

  max_n_factors <- get_max_n_factors(n_items)

  if (n_cats == 0) {
    est_CD <- CDpatched(data = X_float, f.max = max_n_factors)
  }
  if (n_cats >  0) {
    est_CD <- CDpatched(data = X_int  , f.max = max_n_factors)
  }

  results_DV["CD"] <- est_CD

  # ----------------------------------------------------------------------------
  # Perform EKC
  # ----------------------------------------------------------------------------

  EKC0_ev <- ekc(X_rf_ev, n_items, n_obs)
  EKC1_ev <- pmax(EKC0_ev, 1)
  results_DV["ekc0"] <- vec_filter(X_rf_ev, EKC0_ev)
  results_DV["ekc1"] <- vec_filter(X_rf_ev, EKC1_ev)

  # ----------------------------------------------------------------------------
  # Save results
  # ----------------------------------------------------------------------------

  o  <- cbind(results_CV, results_DV)
  write.csv(o, fn, row.names = FALSE)

  NULL

}

stopCluster(cl)
