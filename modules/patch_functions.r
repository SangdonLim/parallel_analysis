#' Patch functions
#'
#' Create patched versions of 'semTools' 0.5.2 and 'RGenData' v1.0 with following changes:
#'
#' \code{NNpatched()} is based on \code{semTools::mvrnonnorm()}
#' - Change \code{MASS::mvrnorm()} to \code{mvnfast::rmvn()}
#' - Change \code{ValeMaurelli1983copied()} to \code{VMpatched()}
#'
#' \code{VMpatched()} is based on \code{semTools:::ValeMaurelli1983copied()}
#' - Change \code{MASS::mvrnorm()} to \code{mvnfast::rmvn()}
#'
#' \code{CDpatched()} is based on \code{RGenData::EFACompData()}
#' - Change \code{RGenData::GenDataPopulation} to \code{GDpatched()}
#'
#' \code{GDpatched()} is based on \code{RGenData::GenDataPopulation()}
#' - Remove \code{set.seed(seed)}
#'
#' @param clean if \code{TRUE} then delete the code afterwards.
#'
#' @export
patch_functions <- function(clean = TRUE) {

  o <- all(
    c("NNpatched", "VMpatched", "CDpatched", "GDpatched", "CDfixedseed", "GDfixedseed")
    %in% ls(envir = .GlobalEnv)
  )
  if (o) {
    return(TRUE)
  }

  get_package("semTools", "0.5-2")
  get_package("RGenData", "1.0")
  require(digest)

  # Patch semTools::mvrnonnorm() -----------------------------------------------

  cat(cli::rule(left = "Patching semTools::mvrnonnorm()"), fill = TRUE)

  # Load
  f <- eval(parse(text = "semTools::mvrnonnorm"))
  code <- capture.output(f)
  code <- code[1:34]
  check_hash(
    code,
    "684b9b098c69b4d1c982418d167fee9b",
    "* origin function semTools::mvrnonnorm()"
  )

  # Patch
  code[16] <- "        X <- mvnfast::rmvn(n = n, mu = mu, Sigma = Sigma)"
  code[26] <- "        Z <- VMpatched(n = n, COR = cov2cor(Sigma), skewness = skewness, kurtosis = kurtosis)"
  code <- code[-27]
  code <- c(
    "NNpatched <-", code
  )
  message("* patched function NNpatched(): created")
  check_hash(
    code,
    "223036b627241b788d407fdf2d4ac7dd",
    "* patched function NNpatched()"
  )

  # Source
  writeLines(code, "NNpatched.r")
  source("NNpatched.r")
  if (clean) {
    o <- remove_file_safely("NNpatched.r")
  }
  message("* patch complete")

  # Patch semTools:::ValeMaurelli1983copied() ----------------------------------

  cat(cli::rule(left = "Patching semTools:::ValeMaurelli1983copied()"), fill = TRUE)

  # Load
  f <- eval(parse(text = "semTools:::ValeMaurelli1983copied"))
  code <- capture.output(f)
  code <- code[1:99]
  check_hash(
    code,
    "11415e1e08e8ac2de40cbfd3928b9d0a",
    "* origin function semTools:::ValeMaurelli1983copied()"
  )

  # Patch
  code[93] <- "    X <- Z <- mvnfast::rmvn(n = n, mu = rep(0, nvar), sigma = ICOR)"
  code <- c(
    "VMpatched <-", code
  )
  message("* patched function VMpatched(): created")
  check_hash(
    code,
    "556ba327f30f8ab02662ef39fb6f7225",
    "* patched function VMpatched()"
  )

  # Source
  writeLines(code, "VMpatched.r")
  source("VMpatched.r")
  if (clean) {
    o <- remove_file_safely("VMpatched.r")
  }
  message("* patch complete")

  # Patch RGenData::EFACompData() ----------------------------------------------

  cat(cli::rule(left = "Patching RGenData::EFACompData()"), fill = TRUE)

  # Load
  f <- eval(parse(text = "RGenData::EFACompData"))
  code <- capture.output(f)
  code <- code[1:46]
  check_hash(
    code,
    "ac6f174980e610f145e2e44e59b75586",
    "* origin function RGenData::EFACompData()"
  )

  # Patch
  code[12] <- "        pop <- GDpatched(data, n.factors = f.cd, n.cases = n.pop, "
  code <- code[-c(34:45)]
  code[33] <- "    return(f.cd - 1)"
  code[16] <- "            while (v != n.variables) {"
  code[19] <- "                for (i in 1:n.variables) v <- v + as.integer(min(samp[, "
  code[20] <- "                  i]) != max(samp[, i]))"
  code <- c(
    code[1:18],
    "                v <- 0",
    code[19:34]
  )
  code <- c(
    "CDpatched <-", code
  )
  message("* patched function CDpatched(): created")
  check_hash(
    code,
    "65a34ce9ac5d72ccf74802d1009abc51",
    "* patched function CDpatched()"
  )

  # Source
  writeLines(code, "CDpatched.r")
  source("CDpatched.r")
  if (clean) {
    o <- remove_file_safely("CDpatched.r")
  }
  message("* patch complete")

  # Patch RGenData::GenDataPopulation() ----------------------------------------

  cat(cli::rule(left = "Patching RGenData::GenDataPopulation()"), fill = TRUE)

  # Load
  f <- eval(parse(text = "RGenData::GenDataPopulation"))
  code <- capture.output(f)
  code <- code[1:114]
  check_hash(
    code,
    "c8cf8cefaa59a4b11ec7b6ba41acda5d",
    "* origin function RGenData::GenDataPopulation()"
  )

  # Patch
  code[2] <- "    initial.multiplier = 1, corr.type = \"pearson\") "
  code <- code[-4]
  code <- c(
    "GDpatched <-", code
  )
  message("* patched function GDpatched(): created")
  check_hash(
    code,
    "e0ddfa6d5de25cba5b82fbc121193fc7",
    "* patched function GDpatched()"
  )

  # Source
  writeLines(code, "GDpatched.r")
  source("GDpatched.r")
  if (clean) {
    o <- remove_file_safely("GDpatched.r")
  }
  message("* patch complete")

  # Patch RGenData::EFACompData() (fixed seed version) -------------------------

  cat(cli::rule(left = "Patching RGenData::EFACompData() (fixed seed version)"), fill = TRUE)

  # Load
  f <- eval(parse(text = "RGenData::EFACompData"))
  code <- capture.output(f)
  code <- code[1:46]
  check_hash(
    code,
    "ac6f174980e610f145e2e44e59b75586",
    "* origin function RGenData::EFACompData()"
  )

  # Patch
  code[12] <- "        pop <- GDfixedseed(data, n.factors = f.cd, n.cases = n.pop, "
  code <- code[-c(34:45)]
  code[33] <- "    return(f.cd - 1)"
  code[16] <- "            while (v != n.variables) {"
  code[19] <- "                for (i in 1:n.variables) v <- v + as.integer(min(samp[, "
  code[20] <- "                  i]) != max(samp[, i]))"
  code <- c(
    code[1:18],
    "                v <- 0",
    code[19:34]
  )
  code[34] <- "    return(rmsr.eigs)"
  code <- c(
    "CDfixedseed <-", code
  )
  message("* patched function CDfixedseed(): created")
  check_hash(
    code,
    "3576819c4a04e336d8a4254437e61f28",
    "* patched function CDfixedseed()"
  )

  # Source
  writeLines(code, "CDfixedseed.r")
  source("CDfixedseed.r")
  if (clean) {
    o <- remove_file_safely("CDfixedseed.r")
  }
  message("* patch complete")

  # Patch RGenData::GenDataPopulation (fixed seed version) ---------------------

  cat(cli::rule(left = "Patching RGenData::GenDataPopulation() (fixed seed version)"), fill = TRUE)

  # Load
  f <- eval(parse(text = "RGenData::GenDataPopulation"))
  code <- capture.output(f)
  code <- code[1:114]
  check_hash(
    code,
    "c8cf8cefaa59a4b11ec7b6ba41acda5d",
    "* origin function RGenData::GenDataPopulation()"
  )

  # Patch
  code[2] <- "    initial.multiplier = 1, corr.type = \"pearson\") "
  code[4] <- "    set.seed(1)"
  code <- c(
    "GDfixedseed <-", code
  )
  message("* patched function GDfixedseed(): created")
  check_hash(
    code,
    "6ea08a3720a3a914af2face9dba8e9ed",
    "* patched function GDfixedseed()"
  )

  # Source
  writeLines(code, "GDfixedseed.r")
  source("GDfixedseed.r")
  if (clean) {
    o <- remove_file_safely("GDfixedseed.r")
  }
  message("* patch complete")

  # Load FA --------------------------------------------------------------------

  cat(cli::rule(left = "Loading RGenData::FactorAnalysis()"), fill = TRUE)

  # Load
  f <- eval(parse(text = "RGenData::FactorAnalysis"))
  code <- capture.output(f)
  code <- code[1:40]
  code <- c(
    "FactorAnalysis <-", code
  )
  check_hash(
    code,
    "fe79218a9a60f8e5f89affaa7dc83641",
    "* origin function RGenData::FactorAnalysis()"
  )

  # Source
  writeLines(code, "FA.r")
  source("FA.r")
  if (clean) {
    o <- remove_file_safely("FA.r")
  }
  message("* load complete")

  # Finalize -------------------------------------------------------------------

  cat(cli::rule(left = "OK"), fill = TRUE)

}

#' Remove a file safely
#'
#' Remove a file and make sure it is removed.
#'
#' @export
remove_file_safely <- function(f) {
  while(file.exists(f)) {
    o <- file.remove(f)
    Sys.sleep(1)
  }
  return(TRUE)
}

#' Check hash
#'
#' @export
check_hash <- function(x, expected_hash, fn_name) {
  hash_ok <- digest(x) == expected_hash
  if (!hash_ok) {
    stop(sprintf(
      "%s: hash mismatch", fn_name
    ))
  }
  message(sprintf(
    "%s: hash ok", fn_name
  ))
}
