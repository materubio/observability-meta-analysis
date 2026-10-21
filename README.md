# Coding sample: effect sizes for a meta-analysis on social image

This repository contains the R code I wrote as a research assistant for a meta-analysis on social image concerns and prosocial behavior in Dec 2025. The code computes standardized effect sizes (Pearson's r and Fisher's z, with 95% confidence intervals) for an economics sample and a psychology sample, and plots them.

## Files

- `coding_sample.R`: the three original scripts (`effects_sizes_MR.R`, `effects_sizes_csv.R` and `graphs.R`) shown one after the other in a single file.
- `effects_sizes_MR.R`: the first script on its own, which `graphs.R` calls by name.
- `effect_sizes_data.csv`: the estimates of the economics sample, read by `effects_sizes_csv.R`.

## How to run

Open R in this folder and run `source("coding_sample.R")`. The code uses the packages `dplyr`, `tibble`, `readr`, `ggplot2` and `osfr`. The psychology sample is downloaded from OSF when the script runs, so it needs an internet connection.
