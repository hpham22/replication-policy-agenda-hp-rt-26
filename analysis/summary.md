# ASEAN Policy Agenda Analysis: Summary of Results

Generated: 2026-03-04 07:16:42

## Data

- **Valid instruments:** 244
- **Policy areas:** 16
- **Years:** 1967-2020 (54 years, 46 with output)
- **Zero-output years:** 1968, 1970, 1971, 1973, 1974, 1984, 1990, 1993

## Parameter Choices

- **Percentage changes:** Area-level, pooled across all 16 areas and 53 year-pairs
- **Zero-handling rules:**
  - 0 -> 0: assign 0
  - 0 -> >0: exclude (undefined)
  - >0 -> 0: assign -1 (100% decrease)
  - >0 -> >0: standard formula
- **Kurtosis convention:** Raw kurtosis (benchmark = 3)
- **Core areas:** 10 (Transportation), 15 (Intra-ASEAN Trade), 20 (ASEAN Governance)
- **2008 classification:** Included in both crisis and milestone categories
- **Crisis windows (2-year):** 1997-98, 2004-05, 2008-09, 2020 (1-year only)
- **Milestone windows (2-year):** 1976-77, 1992-93, 2007-08, 2015-16

## H1a: Aggregate Punctuated Equilibrium

- **N valid percentage changes:** 772
- **Mean:** -0.0651
- **SD:** 0.5910
- **Skewness:** 9.6239
- **Raw kurtosis:** 170.2610 (benchmark = 3)
- **Excess kurtosis:** 167.2610
- **L-kurtosis (tau_4):** 0.6463 (benchmark = 0.1226)
- **Shapiro-Wilk W:** 0.299039, p = 5.57199626122569e-47
- **t-fit mu:** 0.0000
- **t-fit sigma:** 0.0000 [95% CI: 0.0000, 0.0000]
- **t-fit sigma^2:** 0.0000
- **t-fit nu:** 1.0100 [95% CI: 0.9823, 1.9161]
- **t-fit Theo. variance:** Inf
- **Interpretation:** infinite variance (extreme punctuation)

## H1b: Core-Periphery Decomposition

### Aggregate (n = 772)
- Kurtosis: 170.2610 | L-kurtosis: 0.6463
- Shapiro-Wilk: W = 0.299039, p = 5.57e-47
- t-fit: mu = 0.000000, sigma = 0.000001, sigma^2 = 0.000000, nu = 1.0100 (converged)
- sigma 95% CI: [0.000000, 0.000000] | nu 95% CI: [0.9993, 1.8839]
- Theo. variance: Inf | infinite variance (extreme punctuation)

### Core (n = 132)
- Kurtosis: 42.4905 | L-kurtosis: 0.4199
- Shapiro-Wilk: W = 0.503274, p = 3.06e-19
- t-fit: mu = 0.000000, sigma = 0.000001, sigma^2 = 0.000000, nu = 1.0164 (converged)
- sigma 95% CI: [0.000000, 0.515095] | nu 95% CI: [0.2975, 3.9178]
- Theo. variance: Inf | infinite variance (extreme punctuation)

### Peripheral (n = 640)
- Kurtosis: 11.2176 | L-kurtosis: 0.6516
- Shapiro-Wilk: W = 0.292318, p = 7.15e-44
- t-fit: mu = 0.000000, sigma = 0.000001, sigma^2 = 0.000000, nu = 1.0100 (converged; bootstrap unreliable)
- sigma 95% CI: [NA, NA] | nu 95% CI: [NA, NA]
- Theo. variance: Inf | infinite variance (extreme punctuation)

### Variance Decomposition
- Total variance: 0.3492
- Within-core: 83.5% of total
- Within-peripheral: 16.8% of total
- Between-group: -0.3% of total

## H2: Crisis vs. Milestone Change Patterns

### Year-Type Comparison (2-year windows)

**crisis** (n = 7 years):
- Mean output: 11.3 (SD = 7.9)
- Mean active areas: 4.3
- Mean HHI: 0.4121
- Mean peripheral share: 17.8%
- Mean entropy: 1.0872

**milestone** (n = 7 years):
- Mean output: 4.7 (SD = 4.5)
- Mean active areas: 2.9
- Mean HHI: 0.5897
- Mean peripheral share: 7.6%
- Mean entropy: 0.7298

**normal** (n = 33 years):
- Mean output: 4.1 (SD = 2.7)
- Mean active areas: 2.5
- Mean HHI: 0.5797
- Mean peripheral share: 30.7%
- Mean entropy: 0.7173

## T-Distribution Decomposition (Fernandez-i-Marin et al.)

The location-scale t-distribution separates two theoretically distinct dimensions:
- **sigma^2 (scale^2):** captures **incrementalism** — higher sigma^2 means larger routine changes
- **nu (degrees of freedom):** captures **punctuation** — lower nu means heavier tails, more extreme punctuations

A system can be highly incremental (low sigma^2) AND highly punctuated (low nu) simultaneously.
Kurtosis alone conflates these two dimensions; the t-distribution separates them.

### Unified Comparison (6 groups)

**Aggregate** (n = 772):
- sigma = 0.000001, sigma^2 = 0.000000, nu = 1.0100
- sigma 95% CI: [0.000000, 0.000000] | nu 95% CI: [0.9993, 1.8839]
- Theo. variance: Inf | L-kurtosis: 0.6463
- Interpretation: infinite variance (extreme punctuation) (converged)

**Core** (n = 132):
- sigma = 0.000001, sigma^2 = 0.000000, nu = 1.0164
- sigma 95% CI: [0.000000, 0.515095] | nu 95% CI: [0.2975, 3.9178]
- Theo. variance: Inf | L-kurtosis: 0.4199
- Interpretation: infinite variance (extreme punctuation) (converged)

**Peripheral** (n = 640):
- sigma = 0.000001, sigma^2 = 0.000000, nu = 1.0100
- sigma 95% CI: [NA, NA] | nu 95% CI: [NA, NA]
- Theo. variance: Inf | L-kurtosis: 0.6516
- Interpretation: infinite variance (extreme punctuation) (converged; bootstrap unreliable)

**Crisis years** (n = 95):
- sigma = 0.000001, sigma^2 = 0.000000, nu = 1.0100
- sigma 95% CI: [0.000000, 0.000000] | nu 95% CI: [0.6191, 1.7564]
- Theo. variance: Inf | L-kurtosis: 0.6597
- Interpretation: infinite variance (extreme punctuation) (converged)

**Milestone years** (n = 116):
- sigma = 0.000001, sigma^2 = 0.000000, nu = 1.0100
- sigma 95% CI: [0.000000, 0.000000] | nu 95% CI: [0.6176, 2.4249]
- Theo. variance: Inf | L-kurtosis: 0.3960
- Interpretation: infinite variance (extreme punctuation) (converged)

**Normal years** (n = 465):
- sigma = 0.000001, sigma^2 = 0.000000, nu = 1.0100
- sigma 95% CI: [0.000000, 0.000000] | nu 95% CI: [0.8936, 2.5019]
- Theo. variance: Inf | L-kurtosis: 0.6076
- Interpretation: infinite variance (extreme punctuation) (converged)

## Caveats and Notes

- **2008 overlap:** Year 2008 appears in both crisis (GFC, 2008-09) and milestone
  (Charter, 2007-08) windows. It is counted in both groups for the aggregate comparison.
- **2020 truncation:** COVID-19 window is one year only; no post-event comparison possible.
- **1993 zero-output:** Within the AFTA milestone window (1992-93), 1993 had zero instruments.
  This may reflect the pre-1995 pattern of sparse output rather than a milestone dynamic.
- **2004-05 tsunami:** Trade integration output in 2004 (Framework Agreement annexes) is
  not tsunami-related; disaster response (AADMER) appears in 2005.
- **T-distribution fitting:** The MLE fits may produce degenerate values (sigma near 0, nu at the
  lower bound of 1.01) when data contains large point masses (e.g., 83% zeros in pooled data).
  The continuous t-distribution cannot adequately model data with extreme point masses.
  The kurtosis and L-kurtosis provide complementary distributional evidence. Degenerate fits
  for peripheral areas honestly reflect the extreme bimodal {0, -1} pattern in peripheral attention.

