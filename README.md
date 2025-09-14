# Comparison of Multiple Numeric Samples Using KS Test

This package provides functions to perform pairwise two-sample Kolmogorov–Smirnov (KS) tests for multiple numeric datasets in R, with S3 object-oriented methods (`print`, `summary`, `plot`).

## Features

- Compare 2–10 numeric datasets.
- Each dataset must have at least 10 observations and no NA values.
- Compute pairwise D-statistics and p-values.
- Optionally include base R `ks.test()` results.
- Visualize datasets via ECDF plots, with optional annotation of maximum D-statistic.
- Fully documented with examples.

**Note**: For better and clearer visualization in plots, using a maximum of 5 datasets is recommended.


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

### 1. Create Example Data

```{r}
set.seed(1)
a <- rnorm(100)
b <- rnorm(100, 0.5)
c <- rnorm(100, 1)

datasets <- list(A = a, B = b, C = c)
```

### 2. Run KS Test

```{r}
res <- ks_test(datasets)
res
```

### 3. Print Results

```{r}
print(res)
```

### 4. Summary

```{r}
# Summary of KS test result
summary(res)

# Summary including base R ks.test results
summary(res, base_ks = TRUE)
```

### Interpretation

- **Reject null hypothesis** if p-value < 0.05  
  > The two samples come from different distributions.

- **Fail to reject null hypothesis** if p-value ≥ 0.05  
  > The two samples come from the same distribution.


### 5. Plot ECDFs

```{r}
# Plot ECDFs
plot(res)

# Plot ECDFs and annotate maximum D-statistic for each pair
plot(res, show_pairwise_D = TRUE)
```

See [example.R](./example.R) for usage demonstrations.

## References

- [Kolmogorov–Smirnov test](https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test)
- stats::ks.test in base R
