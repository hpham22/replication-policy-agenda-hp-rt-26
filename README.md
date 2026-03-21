# Replication Materials

This repository contains replication codes and dataset for "Policy Agenda Dynamics in ASEAN: Punctuated Equilibrium and the Effects of Crises and Milestones."

**Authors:** Hung Pham, The University of Hong Kong

**Contact:** For inquiries regarding findings, please contact hpham@hku.hk

**R packages:** MASS, readxl, tidyverse, fitdistrplus, e1071, scales, patchwork

## Data

- `data/` -- Dataset and codebook (to be uploaded)

## Scripts

Scripts should be run in order, or use `run_all.R` to execute the full pipeline:

| Script | Description |
|--------|-------------|
| `00_setup.R` | Packages, paths, constants, utility functions |
| `00_data_preparation.R` | Data loading, cleaning, matrix construction |
| `01_fig1_attention_distribution.R` | Figure 1: Policy area attention shares |
| `02_fig2_annual_timeline.R` | Figure 2: Annual output timeline (1967-2020) |
| `03_table1_crisis_milestone.R` | Table 1: Crisis and milestone classifications |
| `04_h1a_aggregate.R` | Table 2, Figure 3: Aggregate distributional statistics |
| `05_h1b_decomposition.R` | Tables 3-4: Core-periphery decomposition |
| `06_h2_crisis_milestone.R` | Tables 5-6, Figure 4: Crisis/milestone analysis |
| `07_appendix_b_robustness.R` | Tables B1-B6, Figure B1: Robustness checks |
| `run_all.R` | Master runner: executes full pipeline |

## Results

All outputs are saved to `results/`:

- Figures as PDF
- Tables as CSV
