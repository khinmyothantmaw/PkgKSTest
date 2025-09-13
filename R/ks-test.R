#' Pairwise Two-sample Kolmogorov–Smirnov Test
#'
#' Perform pairwise two-sample Kolmogorov–Smirnov (KS) tests to compare whether
#' multiple numeric datasets come from the same distribution.
#'
#' @param data_list A named list of numeric vectors. Minimum 2 datasets, maximum 10.
#' Each vector must have at least 10 observations and contain no NAs.
#'
#' @details
#' The Kolmogorov–Smirnov test statistic \eqn{D} is defined as the maximum absolute
#' difference between the empirical cumulative distribution functions (ECDFs) of two samples.
#' The null hypothesis is that the two samples are drawn from the same distribution.
#'
#' This function performs **pairwise comparisons** for all datasets in `data_list`.
#'
#' @return An object of class \code{"ks_test"} containing:
#' \item{results}{A data.frame with columns \code{Sample1}, \code{Sample2}, \code{D.Statistic}, \code{p.value},
#' and \code{conclusion}}
#' \item{data_list}{The original list of datasets.}
#' \item{method}{Description of the test.}
#'
#' @examples
#' set.seed(1)
#' a <- rnorm(100)
#' b <- rnorm(100, 0.5)
#' c <- rnorm(100, 1)
#'
#' res <- ks_test(list(A = a, B = b, C = c))
#'
#' # Print results
#' print(res)
#'
#' # Show summary of Kolmogorov–Smirnov test results
#' summary(res)
#'
#' # Show summary including base R ks.test results
#' summary(res, base_ks = TRUE)
#'
#' # Plot ECDFs
#' plot(res)
#'
#' # Plot ECDFs and annotate maximum D-statistic for each pair
#' plot(res, show_pairwise_D = TRUE)
#' @export
ks_test <- function(data_list) {
    n_terms = 100

    # Input checks
    if (!is.list(data_list) || length(data_list) < 2) {
        stop("At least 2 datasets must be provided as a list of numeric vectors.")
    }
    if (length(data_list) > 10) {
        stop("A maximum of 10 datasets can be compared at once.")
    }

    for (nm in names(data_list)) {
        x <- data_list[[nm]]
        if (!is.numeric(x)) stop(sprintf("Dataset '%s' is not numeric. All datasets must be numeric vectors.", nm))
        if (length(x) < 10) stop(sprintf("Dataset '%s' has fewer than 10 observations. Each dataset must have at least 10.", nm))
        if (anyNA(x)) stop(sprintf("Dataset '%s' contains NA values. Please remove or impute missing values.", nm))
    }

    n <- length(data_list)
    pairs <- t(combn(n, 2))  # all pairwise combinations
    results <- data.frame(
        sample1 = character(0),
        sample2 = character(0),
        D = numeric(0),
        p.value = numeric(0),
        n1 = numeric(0),
        n2 = numeric(0),
        stringsAsFactors = FALSE
    )

    for (k in seq_len(nrow(pairs))) {
        i <- pairs[k, 1]
        j <- pairs[k, 2]
        x <- sort(data_list[[i]])
        y <- sort(data_list[[j]])
        nx <- length(x)
        ny <- length(y)

        # Compute D statistic
        i1 <- j1 <- 1
        cdf_x <- cdf_y <- 0
        D <- 0
        while (i1 <= nx && j1 <= ny) {
            if (x[i1] <= y[j1]) { cdf_x <- i1 / nx; i1 <- i1 + 1 }
            else { cdf_y <- j1 / ny; j1 <- j1 + 1 }
            D <- max(D, abs(cdf_x - cdf_y))
        }
        while (i1 <= nx) { cdf_x <- i1 / nx; i1 <- i1 + 1; D <- max(D, abs(cdf_x - cdf_y)) }
        while (j1 <= ny) { cdf_y <- j1 / ny; j1 <- j1 + 1; D <- max(D, abs(cdf_x - cdf_y)) }

        # Effective sample size
        n_eff <- (nx * ny) / (nx + ny)
        lambda <- (sqrt(n_eff) + 0.12 + 0.11 / sqrt(n_eff)) * D

        # Asymptotic p-value
        pval <- 0
        for (t in 1:n_terms) {
            pval <- pval + (-1)^(t-1) * exp(-2 * t^2 * lambda^2)
        }
        pval <- min(max(2 * pval, 0), 1)

        results <- rbind(results, data.frame(
            Sample1 = names(data_list)[i],
            Sample2 = names(data_list)[j],
            D.Statistic = D,
            p.value = pval,
            n1 = nx,
            n2 = ny,
            stringsAsFactors = FALSE
        ))
    }

    out <- list(
        results = results,
        data_list = data_list,
        method = "Pairwise Two-sample Kolmogorov–Smirnov test"
    )

    class(out) <- "ks_test"
    return(out)
}

#' @export
print.ks_test <- function(x, ...) {
    cat(x$method, "\n\n")
    print(x$results)
    invisible(x)
}

#' Summary Method
#' @export
summary.ks_test <- function(object, base_ks = FALSE, ...) {

    res <- object$results
    res$Conclusion <- ifelse(res$p.value < 0.05, "Reject H0 (The two samples come from different distributions)", "Fail to reject H0 (The two samples come from the same distribution)")

    if (base_ks) {
        # Compute base R ks.test for each pair
        base_results <- lapply(seq_len(nrow(res)), function(i) {
            xdat <- object$data_list[[res$Sample1[i]]]
            ydat <- object$data_list[[res$Sample2[i]]]
            ks.test(xdat, ydat, alternative = "two.sided")
        })
        res$base_ks_D.Statistic <- sapply(base_results, function(z) z$statistic)
        res$base_ks_p.value <- sapply(base_results, function(z) z$p.value)
    } else {
        res$base_ks_D.Statistic <- NA
        res$base_ks_p.value <- NA
    }

    out <- list(
        results = res,
        method = object$method,
        n_datasets = length(object$data_list),
        dataset_names = names(object$data_list),
        base_ks = base_ks
    )
    class(out) <- "summary.ks_test"
    return(out)
}

# Print method for summary
#' @export
print.summary.ks_test <- function(x, ...) {
    cat("Pairwise Two-sample Kolmogorov–Smirnov Test Summary\n")
    cat("Number of datasets:", x$n_datasets, "\n")
    cat("Datasets:", paste(x$dataset_names, collapse = ", "), "\n\n")

    df <- x$results
    if (x$base_ks) {
        print(df[, c("Sample1", "Sample2", "D.Statistic", "p.value", "base_ks_D.Statistic", "base_ks_p.value", "Conclusion")],
              row.names = FALSE)
    } else {
        print(df[, c("Sample1", "Sample2", "D.Statistic", "p.value", "Conclusion")], row.names = FALSE)
    }
    invisible(x)
}

#' @export
plot.ks_test <- function(x, show_pairwise_D = FALSE, ...) {
    if (!inherits(x, "ks_test")) stop("Object must be of class 'ks_test'")

    data_list <- x$data_list
    n <- length(data_list)

    if (n == 0) stop("No datasets to plot")
    if (n > 10) stop("Maximum 10 datasets allowed for plotting")

    # Assign colors automatically
    colors <- grDevices::rainbow(n)

    # Determine overall range for x-axis
    all_values <- unlist(data_list)
    rng <- range(all_values, finite = TRUE)

    # Plot first ECDF
    plot(ecdf(data_list[[1]]), col = colors[1], lwd = 2,
         xlim = rng, ylim = c(0, 1), xlab = "Value", ylab = "ECDF",
         main = "Empirical CDFs of Datasets", ...)

    # Add remaining ECDFs
    if (n > 1) {
        for (i in 2:n) {
            lines(ecdf(data_list[[i]]), col = colors[i], lwd = 2)
        }
    }

    # Add legend
    legend("bottomright", legend = names(data_list), col = colors, lwd = 2, bty = "n")

    # Optional: show pairwise D statistics
    if (show_pairwise_D && n > 1) {
        pairs <- combn(n, 2)
        for (k in 1:ncol(pairs)) {
            i <- pairs[1, k]
            j <- pairs[2, k]
            Fx <- ecdf(data_list[[i]])
            Fy <- ecdf(data_list[[j]])
            pooled <- sort(unique(c(data_list[[i]], data_list[[j]])))
            diffs <- abs(Fx(pooled) - Fy(pooled))
            D <- max(diffs)
            t_star <- pooled[which.max(diffs)]

            # Draw vertical segment at D
            segments(t_star, Fx(t_star), t_star, Fy(t_star),
                     col = "darkgreen", lwd = 2, lty = 2)
            # Annotate D value
            text(t_star, mean(c(Fx(t_star), Fy(t_star))),
                 labels = paste0("D=", round(D, 3)),
                 pos = 3, col = "darkgreen", cex = 0.8)
        }
    }
}
