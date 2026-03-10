###############################################################################
# 00_setup.R
# Package loading, path configuration, constants, and shared utility functions
###############################################################################

# --- Load packages ---
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
  library(knitr)
  library(kableExtra)
})
select <- dplyr::select

# --- Path configuration ---
if (exists("project_root") && !is.null(project_root)) {
  # Already set by caller
} else {
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

# Full codebook (22 topics; topics 2, 5, 13, 14, 18 have zero instruments)
CODEBOOK <- c(
  "1"  = "Macroeconomics",
  "2"  = "Civil Rights",
  "3"  = "Health",
  "4"  = "Agriculture",
  "5"  = "Labor",
  "6"  = "Education",
  "7"  = "Environment",
  "8"  = "Energy",
  "9"  = "Immigration",
  "10" = "Transportation",
  "12" = "Law & Crime",
  "13" = "Social Welfare",
  "14" = "Housing",
  "15" = "Intra-ASEAN Trade",
  "16" = "Defense",
  "17" = "Technology",
  "18" = "Foreign Trade",
  "19" = "International Affairs",
  "20" = "ASEAN Governance",
  "21" = "Public Lands",
  "23" = "Culture"
)

# Active area labels (16 areas with instruments)
AREA_LABELS <- CODEBOOK[as.character(ALL_AREAS)]

# Crisis windows (two-year): 2008 classified as milestone, not crisis
# Note: 2007-08 overlap (Charter + GFC onset) resolved as milestone
#        since the Charter signing preceded the GFC.
CRISIS_YEARS    <- c(1997, 1998, 2004, 2005, 2009, 2020)
MILESTONE_YEARS <- c(1976, 1977, 1992, 1993, 2007, 2008, 2015, 2016)
ZERO_YEARS      <- c(1968, 1970, 1971, 1973, 1974, 1984, 1990, 1993)

# One-year windows for sensitivity analysis
CRISIS_1YR    <- c(1997, 2004, 2009, 2020)
MILESTONE_1YR <- c(1976, 1992, 2007, 2015)

# --- Skewness, Kurtosis, L-moments (reimplemented) ---
# Using e1071; moments and lmom packages are not available

calc_skewness <- function(x) {
  e1071::skewness(x, na.rm = TRUE, type = 2)
}

calc_kurtosis_raw <- function(x) {
  e1071::kurtosis(x, na.rm = TRUE, type = 2) + 3
}

calc_kurtosis_excess <- function(x) {
  e1071::kurtosis(x, na.rm = TRUE, type = 2)
}

calc_lmoments <- function(x) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n < 4) return(list(lambdas = rep(NA, 4), ratios = c(tau3 = NA, tau4 = NA)))

  b0 <- mean(x)
  i_seq <- seq_along(x)
  b1 <- sum((i_seq - 1) / (n - 1) * x) / n
  b2 <- sum((i_seq - 1) * (i_seq - 2) / ((n - 1) * (n - 2)) * x) / n
  b3 <- sum((i_seq - 1) * (i_seq - 2) * (i_seq - 3) /
              ((n - 1) * (n - 2) * (n - 3)) * x) / n

  L1 <- b0
  L2 <- 2 * b1 - b0
  L3 <- 6 * b2 - 6 * b1 + b0
  L4 <- 20 * b3 - 30 * b2 + 12 * b1 - b0

  tau3 <- if (abs(L2) > 0) L3 / L2 else NA_real_
  tau4 <- if (abs(L2) > 0) L4 / L2 else NA_real_

  list(lambdas = c(L1 = L1, L2 = L2, L3 = L3, L4 = L4),
       ratios  = c(tau3 = tau3, tau4 = tau4))
}

calc_lkurtosis <- function(x) {
  lm <- calc_lmoments(x)
  lm$ratios["tau4"]
}

# --- Shared utility functions ---

compute_pct_changes <- function(counts, years) {
  n <- length(counts)
  stopifnot(length(years) == n)
  tibble(
    year_from = years[-n],
    year_to   = years[-1],
    y_prev    = counts[-n],
    y_curr    = counts[-1]
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

cosine_similarity <- function(a, b) {
  stopifnot(length(a) == length(b))
  norm_a <- sqrt(sum(a^2))
  norm_b <- sqrt(sum(b^2))
  if (norm_a == 0 || norm_b == 0) return(NA_real_)
  sum(a * b) / (norm_a * norm_b)
}

compute_hhi <- function(counts) {
  total <- sum(counts)
  if (total == 0) return(NA_real_)
  shares <- counts / total
  sum(shares^2)
}

compute_shannon <- function(counts) {
  total <- sum(counts)
  if (total == 0) return(NA_real_)
  props <- counts[counts > 0] / total
  -sum(props * log(props))
}

compute_longest_gap <- function(counts) {
  r <- rle(counts == 0)
  zero_runs <- r$lengths[r$values]
  if (length(zero_runs) == 0) return(0L)
  max(zero_runs)
}

# --- Location-scale t-distribution functions ---

dlst <- function(x, mu = 0, sigma = 1, nu = 5, log = FALSE) {
  z <- (x - mu) / sigma
  val <- dt(z, df = nu, log = TRUE) - log(sigma)
  if (log) val else exp(val)
}

plst <- function(q, mu = 0, sigma = 1, nu = 5,
                 lower.tail = TRUE, log.p = FALSE) {
  pt((q - mu) / sigma, df = nu, lower.tail = lower.tail, log.p = log.p)
}

qlst <- function(p, mu = 0, sigma = 1, nu = 5,
                 lower.tail = TRUE, log.p = FALSE) {
  mu + sigma * qt(p, df = nu, lower.tail = lower.tail, log.p = log.p)
}

rlst <- function(n, mu = 0, sigma = 1, nu = 5) {
  mu + sigma * rt(n, df = nu)
}

compute_theo_var <- function(sigma, nu) {
  if (is.na(sigma) || is.na(nu)) return(NA_real_)
  if (nu <= 2) return(Inf)
  sigma^2 * nu / (nu - 2)
}

empty_tfit <- function(note = "failed") {
  list(
    mu = NA, sigma = NA, sigma_sq = NA, nu = NA, theo_var = NA,
    se_mu = NA, se_sigma = NA, se_nu = NA,
    loglik = NA, aic = NA, bic = NA,
    method = "none", note = note
  )
}

fit_location_scale_t <- function(x, label = "data") {
  x <- x[!is.na(x)]
  n <- length(x)

  if (n < 10) {
    return(empty_tfit(paste("insufficient data (n =", n, ")")))
  }

  # Multiple starting value sets
  start_list <- list(
    list(mu = median(x), sigma = mad(x),        nu = 3),
    list(mu = median(x), sigma = mad(x),        nu = 5),
    list(mu = mean(x),   sigma = sd(x),         nu = 10),
    list(mu = median(x), sigma = IQR(x) / 1.35, nu = 30)
  )
  start_list <- lapply(start_list, function(s) {
    s$sigma <- max(s$sigma, 0.01)
    s
  })

  best_fit <- NULL
  best_ll  <- -Inf

  for (s in start_list) {
    fit <- tryCatch({
      fitdist(
        data  = x, distr = "lst", start = s,
        lower = c(mu = -Inf, sigma = 1e-6, nu = 1.01),
        upper = c(mu = Inf,  sigma = Inf,  nu = 200),
        method = "mle"
      )
    }, error = function(e) NULL)

    if (!is.null(fit) && fit$loglik > best_ll) {
      best_ll  <- fit$loglik
      best_fit <- fit
    }
  }

  mu_hat <- sigma_hat <- nu_hat <- NA_real_
  se_mu <- se_sigma <- se_nu <- NA_real_
  ll <- aic_val <- bic_val <- NA_real_
  method <- "none"
  fit_note <- "failed"

  if (!is.null(best_fit)) {
    mu_hat    <- unname(best_fit$estimate["mu"])
    sigma_hat <- unname(best_fit$estimate["sigma"])
    nu_hat    <- unname(best_fit$estimate["nu"])
    se_mu     <- unname(best_fit$sd["mu"])
    se_sigma  <- unname(best_fit$sd["sigma"])
    se_nu     <- unname(best_fit$sd["nu"])
    ll        <- best_fit$loglik
    aic_val   <- best_fit$aic
    bic_val   <- best_fit$bic
    method    <- "fitdist"
    fit_note  <- "converged"
  } else {
    # Fallback: manual optim
    neg_loglik <- function(par) {
      mu <- par[1]; sigma <- exp(par[2]); nu <- exp(par[3])
      z <- (x - mu) / sigma
      -sum(dt(z, df = nu, log = TRUE)) + n * log(sigma)
    }
    opt <- tryCatch({
      optim(par = c(median(x), log(max(mad(x), 0.01)), log(5)),
            fn = neg_loglik, method = "Nelder-Mead",
            hessian = TRUE, control = list(maxit = 10000))
    }, error = function(e) NULL)

    if (!is.null(opt) && opt$convergence == 0) {
      mu_hat    <- opt$par[1]
      sigma_hat <- exp(opt$par[2])
      nu_hat    <- exp(opt$par[3])
      ll        <- -opt$value
      aic_val   <- 2 * 3 - 2 * ll
      bic_val   <- log(n) * 3 - 2 * ll
      method    <- "optim_fallback"
      fit_note  <- "converged via optim"

      se_vec <- tryCatch({
        vcov <- solve(opt$hessian)
        se_raw <- sqrt(pmax(diag(vcov), 0))
        c(se_raw[1], sigma_hat * se_raw[2], nu_hat * se_raw[3])
      }, error = function(e) rep(NA_real_, 3))
      se_mu <- se_vec[1]; se_sigma <- se_vec[2]; se_nu <- se_vec[3]
    }
  }

  if (is.na(mu_hat)) {
    return(empty_tfit(paste("fitting failed for", label)))
  }

  sigma_sq <- sigma_hat^2
  theo_var <- compute_theo_var(sigma_hat, nu_hat)

  # Jittering sensitivity check for degenerate fits
  jitter_note <- ""
  if (sigma_hat < 1e-4) {
    cat(sprintf("    %s: degenerate fit (sigma=%.2e). Trying jittered data...\n",
                label, sigma_hat))
    x_jittered <- x + runif(n, -0.001, 0.001)
    jfit <- tryCatch({
      fitdist(data = x_jittered, distr = "lst",
              start = list(mu = median(x_jittered),
                           sigma = max(sd(x_jittered), 0.01), nu = 5),
              lower = c(mu = -Inf, sigma = 1e-6, nu = 1.01),
              upper = c(mu = Inf,  sigma = Inf,  nu = 200),
              method = "mle")
    }, error = function(e) NULL)
    if (!is.null(jfit)) {
      j_sigma <- unname(jfit$estimate["sigma"])
      j_nu    <- unname(jfit$estimate["nu"])
      jitter_note <- sprintf("; jittered: sigma=%.4f, nu=%.2f", j_sigma, j_nu)
      cat(sprintf("    Jittered result: sigma=%.4f, nu=%.2f\n", j_sigma, j_nu))
    } else {
      jitter_note <- "; jittering also degenerate"
    }
  }

  list(
    mu = mu_hat, sigma = sigma_hat, sigma_sq = sigma_sq,
    nu = nu_hat, theo_var = theo_var,
    se_mu = se_mu, se_sigma = se_sigma, se_nu = se_nu,
    loglik = ll, aic = aic_val, bic = bic_val,
    method = method, note = paste0(fit_note, jitter_note)
  )
}

verify_output <- function(filepath) {
  if (!file.exists(filepath)) stop(paste("Missing output:", filepath))
  if (file.size(filepath) == 0) stop(paste("Empty output:", filepath))
  cat("  OK:", filepath, "(", file.size(filepath), "bytes)\n")
}

# --- Publication theme (grayscale, journal-ready) ---
# Per Kastellec (2025), Healy (2019): theme_bw, serif, larger text sizes

theme_pub <- function(base_size = 14) {
  theme_bw(base_size = base_size, base_family = "serif") %+replace%
    theme(
      text = element_text(colour = "black", family = "serif"),
      axis.text = element_text(size = 14, colour = "black"),
      axis.title = element_text(size = 18),
      strip.text = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 12),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_blank()
    )
}

save_figure <- function(plot, name, width = 6.5, height = 4.5) {
  png_path <- file.path(figures_dir, paste0(name, ".png"))
  pdf_path <- file.path(figures_dir, paste0(name, ".pdf"))
  ggsave(png_path, plot, width = width, height = height, dpi = 600,
         device = agg_png)
  ggsave(pdf_path, plot, width = width, height = height,
         device = cairo_pdf)
  verify_output(png_path)
  verify_output(pdf_path)
}

# --- Publication table helper (kableExtra) ---
# Produces HTML + LaTeX PDF outputs for each table
# Style: booktabs, serif font, compact, horizontal rules only

save_table <- function(tbl, name, caption = NULL, footnote = NULL,
                       col_names = NULL, align = NULL,
                       group_rows = NULL, pack_rows = NULL) {
  html_path  <- file.path(tables_dir, paste0(name, ".html"))
  latex_path <- file.path(tables_dir, paste0(name, ".tex"))

  # Column names default to existing names
  if (is.null(col_names)) col_names <- names(tbl)

  kb <- kbl(tbl, format = "html", booktabs = TRUE,
            caption = caption, col.names = col_names, align = align,
            escape = FALSE) %>%
    kable_styling(
      bootstrap_options = c("striped", "condensed"),
      full_width = FALSE, font_size = 13,
      html_font = '"Times New Roman", Times, serif'
    )

  # Apply row grouping if specified
  if (!is.null(pack_rows)) {
    for (pr in pack_rows) {
      kb <- kb %>% pack_rows(pr$label, pr$start, pr$end,
                             bold = TRUE, italic = FALSE)
    }
  }

  if (!is.null(footnote)) {
    kb <- kb %>% footnote(general = footnote, general_title = "Note: ",
                          footnote_as_chunk = TRUE)
  }

  save_kable(kb, file = html_path, self_contained = TRUE)
  verify_output(html_path)

  # LaTeX version
  kb_tex <- kbl(tbl, format = "latex", booktabs = TRUE,
                caption = caption, col.names = col_names, align = align,
                escape = FALSE) %>%
    kable_styling(latex_options = c("hold_position", "scale_down"),
                  font_size = 10)

  if (!is.null(pack_rows)) {
    for (pr in pack_rows) {
      kb_tex <- kb_tex %>% pack_rows(pr$label, pr$start, pr$end,
                                     bold = TRUE, italic = FALSE)
    }
  }

  if (!is.null(footnote)) {
    kb_tex <- kb_tex %>% footnote(general = footnote, general_title = "Note: ",
                                  footnote_as_chunk = TRUE, threeparttable = TRUE)
  }

  save_kable(kb_tex, file = latex_path, self_contained = TRUE)
  verify_output(latex_path)
}

cat("\n00_setup.R loaded successfully.\n")
