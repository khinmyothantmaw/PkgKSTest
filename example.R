# Example usage of ks_test package

# Load package (after installation)
library(PkgKSTest)

set.seed(123)

# -------------------------------
# 1. Basic Two-sample Example
# -------------------------------
x <- rnorm(100, mean = 0)
y <- rnorm(100, mean = 1)

res1 <- ks_test(list(X = x, Y = y))
print(res1)
summary(res1)
plot(res1, show_pairwise_D = TRUE) #

# -------------------------------
# 2. Multiple Datasets Example
# -------------------------------
a <- rexp(200)
b <- rexp(80, 0.5)
c <- rexp(150, 1.2)

res2 <- ks_test(list(A = a, B = b, C = c))
summary(res2, base_ks = TRUE)   # include Base R reference
plot(res2)

d <- rnorm(50, 0.5, 1.5)
e <- rgamma(80, shape = 1)
g <- rgamma(100, shape = 0, rate = 1.1)
h <- rpois(20, lambda = 3)
i <- rnorm(120, 1, 2)

res3 <- ks_test(list(A = a, B = b, C = c, D = d, E = e, G = g, H = h, I = i, X = x, Y = y))
summary(res3)
plot(res3)

# -------------------------------
# 3. Failure Cases
# -------------------------------

## Case 3.1: Dataset with < 10 observations
ks_test(list(Small = rnorm(5), Normal = rnorm(100)))


## Case 3.2: Dataset with NAs
bad_data <- rnorm(50)
bad_data[10] <- NA
ks_test(list(Good = rnorm(100), Bad = bad_data))


## Case 3.3: More than 10 datasets
too_many <- lapply(1:11, function(i) rnorm(50))
names(too_many) <- paste0("D", 1:11)
ks_test(too_many)


# -------------------------------
# 4. Identical Distributions
# -------------------------------
set.seed(10)
z1 <- rnorm(100, 0, 1)
set.seed(20)
z2 <- rnorm(100, 0, 1)

res4 <- ks_test(list(Z1 = z1, Z2 = z2))
summary(res4)
plot(res4, show_pairwise_D = TRUE)
