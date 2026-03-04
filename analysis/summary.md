# ASEAN Policy Agenda Analysis: Summary of Results

Generated: 2026-03-04 08:25:04

## Data

- **Total instruments:** 245 (1 unclassifiable, 244 area-assignable)
- **Policy areas:** 16 active (of 22 in codebook)
- **Years:** 1967<U+2013>2020 (54 years, 46 with output)
- **Zero-output years:** 1968, 1970, 1971, 1973, 1974, 1984, 1990, 1993
- **Codebook verification:** floor(Sub/100) == Main for all rows

## Parameter Choices

- **Percentage changes:** Area-level, pooled across 16 areas and 53 year-pairs
- **Zero-handling rules:**
  - 0 -> 0: assign 0 (included in full distribution, excluded in restricted)
  - 0 -> >0: exclude (undefined)
  - >0 -> 0: assign -1 (100% decrease)
  - >0 -> >0: standard formula
- **Full distribution:** N = 772 (incl. 642 zero-to-zero transitions)
- **Restricted distribution:** N = 130 (excl. zero-to-zero)
- **Core areas:** 10 (Transportation), 15 (Intra-ASEAN Trade), 20 (ASEAN Governance)
- **2008 classification:** Milestone only (Charter preceded GFC; overlap flagged)
- **Crisis windows:** 1997-98, 2004-05, 2009, 2020
- **Milestone windows:** 1976-77, 1992-93, 2007-08, 2015-16

## H1a: Aggregate Punctuated Equilibrium

### Full Distribution
- **N:** 772
- **Mean:** -0.0651, **Median:** 0.0000, **SD:** 0.5910
- **Raw kurtosis:** 170.2610 (benchmark = 3)
- **L-kurtosis:** 0.6463 (benchmark = 0.1226)
- **Shapiro-Wilk:** W = 0.299039, p = 5.5720e-47
- **t-fit:** mu = 0.0000 (NA), sigma = 0.0000 (NA), nu = 1.0100 (NA)
- **Note:** converged; jittered: sigma=0.0005, nu=1.01

### Restricted Distribution (excl. 0-to-0)
- **N:** 130
- **Mean:** -0.3863, **SD:** 1.4008
- **Raw kurtosis:** 37.9179
- **L-kurtosis:** 0.3721
- **t-fit:** mu = -1.0000 (NA), sigma = 0.0000 (NA), nu = 1.0100 (NA)
- **Note:** converged; jittered: sigma=0.0014, nu=1.01

## H1b: Core-Periphery Decomposition

### Distributional Comparison (Table 3)

**N:** Agg = 772 | Core = 132 | Periph = 640
**Mean:** Agg = -0.0651 | Core = -0.0093 | Periph = -0.0766
**SD:** Agg = 0.5910 | Core = 1.3062 | Periph = 0.2661
**Skewness:** Agg = 9.6239 | Core = 5.3206 | Periph = -3.1925
**Raw Kurtosis (benchmark=3):** Agg = 170.2610 | Core = 42.4905 | Periph = 11.2176
**L-kurtosis (tau_4, benchmark=0.123):** Agg = 0.6463 | Core = 0.4199 | Periph = 0.6516
**Shapiro-Wilk W:** Agg = 0.299039 | Core = 0.503274 | Periph = 0.292318
**Shapiro-Wilk p:** Agg = 5.5720e-47 | Core = 3.0612e-19 | Periph = 7.1496e-44
**t-fit: mu (SE):** Agg = 0.000000 (NA) | Core = 0.000000 (NA) | Periph = 0.000000 (NA)
**t-fit: sigma (SE):** Agg = 0.000001 (NA) | Core = 0.000001 (NA) | Periph = 0.000001 (NA)
**t-fit: sigma^2:** Agg = 0.000000 | Core = 0.000000 | Periph = 0.000000
**t-fit: nu (SE):** Agg = 1.0100 (NA) | Core = 1.0164 (NA) | Periph = 1.0100 (NA)
**t-fit: Theo. variance:** Agg = Inf | Core = Inf | Periph = Inf
**t-fit: AIC:** Agg = -13419.59 | Core = 90.78 | Periph = -13494.65
**t-fit: Note:** Agg = converged; jittered: sigma=0.0005, nu=1.01 | Core = converged; jittered: sigma=0.0023, nu=1.01 | Periph = converged; jittered: sigma=0.0005, nu=1.01

### Variance Decomposition
- Total variance: 0.3492
- Within-core: 83.5% of total
- Within-peripheral: 16.8% of total
- Between-group: -0.3% of total

## H2: Crisis vs. Milestone Change Patterns

### Year-Type Comparison

**crisis** (n = 6): mean output = 12.8 (SD = 7.4), active areas = 4.7, HHI = 0.3974, entropy = 1.1529
**milestone** (n = 7): mean output = 4.7 (SD = 4.5), active areas = 2.9, HHI = 0.5897, entropy = 0.7298
**normal** (n = 33): mean output = 4.1 (SD = 2.7), active areas = 2.5, HHI = 0.5797, entropy = 0.7173
**overall** (n = 46): mean output = 5.3 (SD = 4.8), active areas = 2.9, HHI = 0.5574, entropy = 0.7760

## Caveats and Notes

- **2008 overlap:** 2007-08 is both the Charter milestone and the onset of the GFC.
  Classified as milestone since the Charter signing preceded the GFC.
  The GFC crisis window is 2009 only.
- **2020 truncation:** COVID-19 window is one year only; no post-event comparison.
- **1993 zero-output:** Within the AFTA milestone window, 1993 had zero instruments.
- **2004-05 tsunami:** Trade output in 2004 is not tsunami-related; AADMER in 2005.
- **T-distribution fitting:** Degenerate fits (sigma near 0) reflect the data's
  discrete structure (point masses at 0 and -1). The restricted distribution
  (excluding 0-to-0) provides more meaningful continuous distribution fits.
- **Observation count:** The full distribution has 772 observations (16 areas x 53
  year-pairs, minus 76 excluded). The restricted distribution (130 obs) better
  reflects the subset with actual policy changes.

