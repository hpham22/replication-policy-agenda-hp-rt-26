# ASEAN Policy Agenda Analysis: Summary of Results

Generated: 2026-03-04 08:54:41

## Data

- **Total instruments:** 245 (1 unclassifiable, 244 area-assignable)
- **Policy areas:** 16 active (of 22 in codebook)
- **Years:** 1967<U+2013>2020 (54 years, 46 with output)
- **Zero-output years:** 1968, 1970, 1971, 1973, 1974, 1984, 1990, 1993
- **Codebook verification:** floor(Sub/100) == Main for all rows

## Parameter Choices

- **Primary distribution:** Restricted (excl. 0-to-0), N = 130
- **Full distribution (robustness):** N = 772 (incl. 642 zero-to-zero transitions)
- **Percentage changes:** Area-level, pooled across 16 areas and 53 year-pairs
- **Zero-handling rules:**
  - 0 -> 0: assign 0 (excluded from primary; included in robustness)
  - 0 -> >0: exclude (undefined)
  - >0 -> 0: assign -1 (100% decrease)
  - >0 -> >0: standard formula
- **Core areas:** 10 (Transportation), 15 (Intra-ASEAN Trade), 20 (ASEAN Governance)
- **2008 classification:** Milestone only (Charter preceded GFC; overlap flagged)
- **Crisis windows:** 1997-98, 2004-05, 2009, 2020
- **Milestone windows:** 1976-77, 1992-93, 2007-08, 2015-16

## H1a: Aggregate Punctuated Equilibrium

### Primary Distribution (Restricted, excl. 0-to-0)
- **N:** 130
- **Mean:** -0.3863, **Median:** -1.0000, **SD:** 1.4008
- **Raw kurtosis:** 37.9179 (benchmark = 3)
- **L-kurtosis:** 0.3721 (benchmark = 0.1226)
- **Shapiro-Wilk:** W = 0.465710, p = 9.6292e-20
- **t-fit:** mu = -1.0000 (NA), sigma = 0.0000 (NA), nu = 1.0100 (NA)
- **Note:** converged; jittered: sigma=0.0015, nu=1.01

### Full Distribution (Robustness)
- **N:** 772
- **Mean:** -0.0651, **SD:** 0.5910
- **Raw kurtosis:** 170.2610
- **L-kurtosis:** 0.6463
- **t-fit:** mu = 0.0000 (NA), sigma = 0.0000 (NA), nu = 1.0100 (NA)
- **Note:** converged; jittered: sigma=0.0007, nu=1.01

## H1b: Core-Periphery Decomposition

### Distributional Comparison (Table 3, restricted distribution)

**N:** Agg = 130 | Core = 77 | Periph = 53
**Mean:** Agg = -0.3863 | Core = -0.0159 | Periph = -0.9245
**SD:** Agg = 1.4008 | Core = 1.7149 | Periph = 0.2667
**Skewness:** Agg = 5.0857 | Core = 4.1097 | Periph = 3.3087
**Raw Kurtosis (benchmark=3):** Agg = 37.9179 | Core = 25.4703 | Periph = 12.2970
**L-kurtosis (tau_4, benchmark=0.123):** Agg = 0.3721 | Core = 0.3265 | Periph = 0.7176
**Shapiro-Wilk W:** Agg = 0.465710 | Core = 0.562698 | Periph = 0.292082
**Shapiro-Wilk p:** Agg = 9.6292e-20 | Core = 8.5914e-14 | Periph = 1.4838e-14
**t-fit: mu (SE):** Agg = -1.000000 (NA) | Core = -0.597350 (NA) | Periph = -1.000000 (NA)
**t-fit: sigma (SE):** Agg = 0.000001 (NA) | Core = 0.474967 (NA) | Periph = 0.000001 (NA)
**t-fit: sigma^2:** Agg = 0.000000 | Core = 0.225594 | Periph = 0.000000
**t-fit: nu (SE):** Agg = 1.0100 (NA) | Core = 1.5044 (NA) | Periph = 1.0100 (NA)
**t-fit: Theo. variance:** Agg = Inf | Core = Inf | Periph = Inf
**t-fit: AIC:** Agg = -248.05 | Core = 222.04 | Periph = -1115.23
**t-fit: Note:** Agg = converged; jittered: sigma=0.0014, nu=1.01 | Core = converged | Periph = converged; jittered: sigma=0.0005, nu=1.01

### Variance Decomposition
- Total variance: 1.9622
- Within-core: 88.8% of total
- Within-peripheral: 1.5% of total
- Between-group: 9.7% of total

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

