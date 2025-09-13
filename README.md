---
title: "Pairwise Two-sample Kolmogorov–Smirnov Test"
output: html_document
---

# Pairwise Two-sample Kolmogorov–Smirnov Test (R Package)

This package provides functions to perform **pairwise two-sample Kolmogorov–Smirnov (KS) tests** for multiple numeric datasets in R, with **S3 object-oriented methods** (`print`, `summary`, `plot`).

---

## Features

- Compare **2–10 numeric datasets**.
- Each dataset must have **at least 10 observations** and **no NA values**.
- Compute **pairwise D-statistics and p-values**.
- Optionally include **base R `ks.test()` results**.
- Visualize datasets via **ECDF plots**, with optional annotation of maximum D-statistic.
- Fully documented with examples.

---

## Installation
```{r installation, eval=FALSE}
# Install devtools if needed
install.packages("devtools")

# Load devtools
library(devtools)

# Install the package from the local directory
install("path/to/your/package")

# Or, to load all functions without installing
load_all("path/to/your/package")

# Generate documentation
document()
```

## Usage Examples

# 1. Create Example Data

```{r}
set.seed(1)
a <- rnorm(100)
b <- rnorm(100, 0.5)
c <- rnorm(100, 1)

datasets <- list(A = a, B = b, C = c)
```
# 2. Run KS Test

```{r}
res <- ks_test(datasets)
res
```
