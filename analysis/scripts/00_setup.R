###############################################################################
# 00_setup.R
# Package installation, path configuration, and shared utility functions
###############################################################################

# --- Load packages ---
# lmom and moments are not available via apt; their functionality is
# reimplemented below using e1071 and manual L-moment calculations.
# Load MASS before tidyverse so dplyr::select takes precedence
suppressPackageStartupMessages({
  library(MASS)
  library(readxl)
  library(tidyverse)
  library(fitdistrplus)
  library(e1071)
  library(scales)
  library(ragg)
  library(patchwork)
})
# Ensure dplyr::select is not masked by MASS::select
select <- dplyr::select

# --- Path configuration ---
# Detect project root (works whether sourced from scripts/ or from repo root)
if (exists("project_root") && !is.null(project_root)) {
  # Already set by caller
} else {
  # Try to find the repo root by looking for legal_instrument_data.xlsx
  candidate <- getwd()
  while (!file.exists(file.path(candidate, "legal_instrument_data.xlsx"))) {
    parent <- dirname(candidate)
    if (parent == candidate) stop("Cannot find repo root (legal_instrument_data.xlsx)")
    candidate <- parent
  }
  repo_root <- candidate
  project_root <- file.path(repo_root, "analysis")
}

data_file   <- file.path(dirname(project_root), "legal_instrument_data.xlsx")
tables_dir  <- file.path(project_root, "outputs", "tables")
figures_dir <- file.path(project_root, "outputs", "figures")

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

cat("Project root:", project_root, "\n")
cat("Data file:   ", data_file, "\n")

# --- Constants ---
ALL_YEARS <- 1967:2020
ALL_AREAS <- c(1, 3, 4, 6, 7, 8, 9, 10, 12, 15, 16, 17, 19, 20, 21, 23)
CORE_AREAS <- c(10, 15, 20)
PERIPHERAL_AREAS <- c(1, 3, 4, 6, 7, 8, 9, 12, 16, 17, 19, 21, 23)

AREA_LABELS <- c(
  "1"  = "Macroeconomics",
  "3"  = "Health",
  "4"  = "Agriculture",
  "6"  = "Education",
  "7"  = "Environment",
  "8"  = "Energy",
  "9"  = "Immigration",
  "10" = "Transportation",
  "12" = "Law & Crime",
  "15" = "Intra-ASEAN Trade",
  "16" = "Defense",
  "17" = "Technology",
  "19" = "International Affairs",
  "20" = "ASEAN Governance",
  "21" = "Public Lands",
  "23" = "Culture"
)

# Crisis windows (two-year)
CRISIS_YEARS <- c(1997, 1998, 2004, 2005, 2008, 2009, 2020)
# Milestone windows (two-year)
MILESTONE_YEARS <- c(1976, 1977, 1992, 1993, 2007, 2008, 2015, 2016)
# Zero-output years
ZERO_YEARS <- c(1968, 1970, 1971, 1973, 1974, 1984, 1990, 1993)

# One-year windows for sensitivity analysis
CRISIS_1YR <- c(1997, 2004, 2008, 2020)
MILESTONE_1YR <- c(1976, 1992, 2007, 2015)

# --- Skewness, Kurtosis, and L-moments (reimplemented) ---
# Using e1071 for skewness/kurtosis with explicit convention handling

#' Skewness (type 2 = SAS/SPSS convention, unbiased under normality)
calc_skewness <- function(x) {
  e1071::skewness(x, na.rm = TRUE, type = 2)
}

#' Raw kurtosis (benchmark = 3 for normal distribution)
#' e1071::kurtosis returns excess kurtosis (benchmark = 0), so add 3
calc_kurtosis_raw <- function(x) {
  e1071::kurtosis(x, na.rm = TRUE, type = 2) + 3
}

#' Excess kurtosis (benchmark = 0 for normal distribution)
calc_kurtosis_excess <- function(x) {
  e1071::kurtosis(x, na.rm = TRUE, type = 2)
}

#' Sample L-moments (manual implementation)
#' Computes the first 4 L-moments and L-moment ratios
#' Uses the probability-weighted moment (PWM) method
#' @param x Numeric vector
#' @return Named list: lambdas (L1, L2, L3, L4), ratios (tau3, tau4)
calc_lmoments <- function(x) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n < 4) return(list(lambdas = rep(NA, 4), ratios = c(tau3 = NA, tau4 = NA)))

  # Probability-weighted moments (PWMs) using unbiased estimators
  # b_r = (1/n) * sum_{i=r+1}^{n} C(i-1, r) / C(n-1, r) * x_i
  b0 <- mean(x)

  # b1 = (1/n) * sum_{i=2}^{n} ((i-1)/(n-1)) * x_i
  i_seq <- seq_along(x)
  b1 <- sum((i_seq - 1) / (n - 1) * x) / n

  # b2 = (1/n) * sum_{i=3}^{n} ((i-1)(i-2))/((n-1)(n-2)) * x_i
  b2 <- sum((i_seq - 1) * (i_seq - 2) / ((n - 1) * (n - 2)) * x) / n

  # b3 = (1/n) * sum_{i=4}^{n} ((i-1)(i-2)(i-3))/((n-1)(n-2)(n-3)) * x_i
  b3 <- sum((i_seq - 1) * (i_seq - 2) * (i_seq - 3) / ((n - 1) * (n - 2) * (n - 3)) * x) / n

  # L-moments from PWMs
  L1 <- b0
  L2 <- 2 * b1 - b0
  L3 <- 6 * b2 - 6 * b1 + b0
  L4 <- 20 * b3 - 30 * b2 + 12 * b1 - b0

  # L-moment ratios
  tau3 <- if (abs(L2) > 0) L3 / L2 else NA_real_
  tau4 <- if (abs(L2) > 0) L4 / L2 else NA_real_

  list(
    lambdas = c(L1 = L1, L2 = L2, L3 = L3, L4 = L4),
    ratios  = c(tau3 = tau3, tau4 = tau4)
  )
}

#' L-kurtosis (tau_4) convenience function
#' @param x Numeric vector
#' @return Scalar: L-kurtosis ratio (benchmark = 0.1226 for normal)
calc_lkurtosis <- function(x) {
  lm <- calc_lmoments(x)
  lm$ratios["tau4"]
}

# --- Shared utility functions ---

#' Compute percentage changes with zero-handling rules
#' @param counts Numeric vector of counts (one per year)
#' @param years Integer vector of corresponding years
#' @return tibble with year_from, year_to, y_prev, y_curr, pct_change, status
compute_pct_changes <- function(counts, years) {
  n <- length(counts)
  stopifnot(length(years) == n)

  tibble(
    year_from  = years[-n],
    year_to    = years[-1],
    y_prev     = counts[-n],
    y_curr     = counts[-1]
  ) %>%
    mutate(
      pct_change = case_when(
        y_prev == 0 & y_curr == 0  ~ 0,
        y_prev == 0 & y_curr > 0   ~ NA_real_,
        y_prev > 0  & y_curr == 0  ~ -1,
        TRUE                       ~ (y_curr - y_prev) / y_prev
      ),
      status = case_when(
        y_prev == 0 & y_curr == 0  ~ "zero_to_zero",
        y_prev == 0 & y_curr > 0   ~ "excluded_undefined",
        y_prev > 0  & y_curr == 0  ~ "decline_to_zero",
        TRUE                       ~ "normal"
      )
    )
}

#' Cosine similarity between two numeric vectors
cosine_similarity <- function(a, b) {
  stopifnot(length(a) == length(b))
  norm_a <- sqrt(sum(a^2))
  norm_b <- sqrt(sum(b^2))
  if (norm_a == 0 || norm_b == 0) return(NA_real_)
  sum(a * b) / (norm_a * norm_b)
}

#' Herfindahl-Hirschman Index
compute_hhi <- function(counts) {
  total <- sum(counts)
  if (total == 0) return(NA_real_)
  shares <- counts / total
  sum(shares^2)
}

#' Shannon entropy
compute_shannon <- function(counts) {
  total <- sum(counts)
  if (total == 0) return(NA_real_)
  props <- counts[counts > 0] / total
  -sum(props * log(props))
}

#' Longest consecutive gap (max run of zeros)
compute_longest_gap <- function(counts) {
  r <- rle(counts == 0)
  zero_runs <- r$lengths[r$values]
  if (length(zero_runs) == 0) return(0L)
  max(zero_runs)
}

# --- Location-scale t-distribution functions (for fitdistrplus) ---

#' Density of location-scale t
dlst <- function(x, mu = 0, sigma = 1, nu = 5, log = FALSE) {
  z <- (x - mu) / sigma
  val <- dt(z, df = nu, log = TRUE) - log(sigma)
  if (log) val else exp(val)
}

#' CDF of location-scale t
plst <- function(q, mu = 0, sigma = 1, nu = 5,
                 lower.tail = TRUE, log.p = FALSE) {
  pt((q - mu) / sigma, df = nu, lower.tail = lower.tail, log.p = log.p)
}

#' Quantile function of location-scale t
qlst <- function(p, mu = 0, sigma = 1, nu = 5,
                 lower.tail = TRUE, log.p = FALSE) {
  mu + sigma * qt(p, df = nu, lower.tail = lower.tail, log.p = log.p)
}

#' Random generation for location-scale t
rlst <- function(n, mu = 0, sigma = 1, nu = 5) {
  mu + sigma * rt(n, df = nu)
}

#' Fit location-scale t-distribution via MLE using fitdistrplus
#' Falls back to manual optim if fitdist fails
#' @param x Numeric vector
#' @param label Character label for warnings
#' @return list with mu, sigma, nu, se_mu, se_sigma, se_nu, loglik, aic, bic, method, note
fit_location_scale_t <- function(x, label = "data") {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 10) {
    return(list(
      mu = NA, sigma = NA, nu = NA,
      se_mu = NA, se_sigma = NA, se_nu = NA,
      loglik = NA, aic = NA, bic = NA,
      method = "none", note = paste("insufficient data (n =", n, ")")
    ))
  }

  # Multiple starting value sets
  start_list <- list(
    list(mu = median(x), sigma = mad(x),            nu = 3),
    list(mu = median(x), sigma = mad(x),            nu = 5),
    list(mu = mean(x),   sigma = sd(x),             nu = 10),
    list(mu = median(x), sigma = IQR(x) / 1.35,     nu = 30)
  )
  # Ensure sigma > 0
  start_list <- lapply(start_list, function(s) {
    s$sigma <- max(s$sigma, 0.01)
    s
  })

  best_fit <- NULL
  best_ll  <- -Inf

  for (s in start_list) {
    fit <- tryCatch({
      fitdist(
        data  = x,
        distr = "lst",
        start = s,
        lower = c(mu = -Inf, sigma = 1e-6, nu = 1.01),
        upper = c(mu = Inf,  sigma = Inf,   nu = 200),
        method = "mle"
      )
    }, error = function(e) NULL)

    if (!is.null(fit) && fit$loglik > best_ll) {
      best_ll  <- fit$loglik
      best_fit <- fit
    }
  }

  if (!is.null(best_fit)) {
    s <- summary(best_fit)
    return(list(
      mu = best_fit$estimate["mu"],
      sigma = best_fit$estimate["sigma"],
      nu = best_fit$estimate["nu"],
      se_mu = best_fit$sd["mu"],
      se_sigma = best_fit$sd["sigma"],
      se_nu = best_fit$sd["nu"],
      loglik = best_fit$loglik,
      aic = best_fit$aic,
      bic = best_fit$bic,
      method = "fitdist",
      note = "converged",
      fit_object = best_fit
    ))
  }

  # Fallback: manual optim
  neg_loglik <- function(par) {
    mu <- par[1]; sigma <- exp(par[2]); nu <- exp(par[3])
    z <- (x - mu) / sigma
    -sum(dt(z, df = nu, log = TRUE)) + n * log(sigma)
  }

  opt <- tryCatch({
    optim(
      par = c(median(x), log(max(mad(x), 0.01)), log(5)),
      fn = neg_loglik,
      method = "Nelder-Mead",
      hessian = TRUE,
      control = list(maxit = 10000)
    )
  }, error = function(e) NULL)

  if (!is.null(opt) && opt$convergence == 0) {
    mu_hat    <- opt$par[1]
    sigma_hat <- exp(opt$par[2])
    nu_hat    <- exp(opt$par[3])
    ll        <- -opt$value

    se <- tryCatch({
      vcov <- solve(opt$hessian)
      se_raw <- sqrt(pmax(diag(vcov), 0))
      c(se_raw[1], sigma_hat * se_raw[2], nu_hat * se_raw[3])
    }, error = function(e) rep(NA_real_, 3))

    return(list(
      mu = mu_hat, sigma = sigma_hat, nu = nu_hat,
      se_mu = se[1], se_sigma = se[2], se_nu = se[3],
      loglik = ll,
      aic = 2 * 3 - 2 * ll,
      bic = log(n) * 3 - 2 * ll,
      method = "optim_fallback",
      note = "converged via optim"
    ))
  }

  list(
    mu = NA, sigma = NA, nu = NA,
    se_mu = NA, se_sigma = NA, se_nu = NA,
    loglik = NA, aic = NA, bic = NA,
    method = "none",
    note = paste("fitting failed for", label)
  )
}

#' Verify an output file exists and is non-empty
verify_output <- function(filepath) {
  if (!file.exists(filepath)) stop(paste("Missing output:", filepath))
  if (file.size(filepath) == 0) stop(paste("Empty output:", filepath))
  cat("  OK:", filepath, "(", file.size(filepath), "bytes)\n")
}

cat("\n00_setup.R loaded successfully.\n")
