library(extraDistr)
# ------------------------------
# 1. Set seed
# ------------------------------
set.seed(123)

# ------------------------------
# 2. Choose parameters
# ------------------------------
n <- 25 
beta0 <- 0
beta1 <- 1
sigma2 <- 5 

# ------------------------------
# 3. Generate X values using Xi=-1+0.2*(i-1)
# ------------------------------
X <- -1 + (0:(n-1))*0.2

# ------------------------------
# Generate symmetric non-normal errors
# Laplace (0, b) has variance 2*b^2
# ------------------------------
b <- sqrt(sigma2/2) 
eps <- rlaplace(n,0,b)

# errors are from mixture of normal
# mixture normal errors
eps <- ifelse(runif(n) < 0.5,
              rnorm(n, -2, 1),
              rnorm(n,  2, 1))

Y <- beta0 + beta1*X + eps

Xmat <- cbind(1, X)

# ------------------------------
# OLS estimator, Fitted values and Residuals
# ------------------------------
beta_hat <- solve(t(Xmat)%*%Xmat) %*% t(Xmat) %*% Y

Yhat <- Xmat %*% beta_hat

e <- Y - Yhat

# ------------------------------
# Estimate sigma^2
# ------------------------------
k <- 1  # number of predictors (excluding intercept)
s2 <- as.numeric(t(e)%*%e/(n-k-1))
s <- sqrt(s2)

# ------------------------------
# estimated key quantities
# ------------------------------
beta_hat
sigma2   # true variance
s2       # estimated variance

# ------------------------------
# Hat matrix
# ------------------------------
H <- Xmat %*% solve(t(Xmat)%*%Xmat) %*% t(Xmat)
h <- diag(H)

# ------------------------------
# Standardized residuals  and ordered studentized residuals
# ------------------------------
std_res <- e / (s*sqrt(1-h))

std_res_sorted <- sort(std_res)


#------------------------------------------
# Normal probability plot
# ------------------------------
# Theoretical quantiles
# ------------------------------
i <- 1:n
p1 <- (i - 3/8)/(n + 1/4)
gamma <- qnorm(p1)

# ------------------------------
#  Normal probability plot
# ------------------------------
plot(gamma, std_res_sorted,
     xlab="Theoretical Normal Quantiles",
     ylab="Ordered Standardized Residuals",
     main="Normal Probability Plot",
     pch=19)

abline(lm(std_res_sorted ~ gamma), col="red", lwd=2)


# Q-Q plot 
# theoretical probabilities
i <- 1:n
p2 <- (i - 0.5)/n

# theoretical normal quantiles
q <- qnorm(p2)

# Q-Q plot manually
plot(q, std_res_sorted,
     xlab="Theoretical Quantiles",
     ylab="Ordered Standardized Residuals",
     main="Manual Q-Q Plot",
     pch=19)

abline(lm(std_res_sorted ~ q), col="green", lwd=2)



# Kolmogorov-Smirnov test for normality 
# sorted residuals 
std_res_sorted
# Empirical CDF values 
i <- 1:n
Fn <- i/n
# Theoretical CDF values at sorted residuals 
F0 <- pnorm(std_res_sorted)
# KS statistic
Di <- abs(Fn - F0)
D <- max(Di)
#Table 
# create table
KS_table <- data.frame(
  i = i,
  std_res_sorted = std_res_sorted,
  Fn = Fn,
  F0 = F0,
  Di = Di
)

KS_table 
D
# Dcritical value for alpha=0.05
Dcrit <- 1.36/sqrt(n)
Dcrit
if(D > Dcrit){
  print("Reject H0: Normality is rejected")
} else {
  print("Do not reject H0: Data is consistent with normality")
}



# Shapiro Wilk test for normality 
shapiro_test <- shapiro.test(std_res)
shapiro_test

# Mannually compute  
set.seed(123)

M <- 20000
Z <- matrix(rnorm(M*n), nrow=M)
Z_sorted <- t(apply(Z,1,sort))

gamma_vec <- colMeans(Z_sorted)
V_mat <- cov(Z_sorted)
b <- solve(V_mat) %*% gamma_vec
a <- b / sqrt(as.numeric(t(b) %*% b))
a <- as.vector(a)


s <- sqrt(sum((std_res-mean(std_res))^2))
W <- (sum(a * std_res_sorted)) / s 
W 




# Shapiro–Francia(SF) Test
# Correlation between ordered residuals and expected normal order statistics
std_res_sorted <- sort(std_res)
i <- 1:n
p1 <- (i - 3/8)/(n + 1/4)
gamma <- qnorm(p1)

cor_test <- cor(std_res_sorted, gamma)
cor_test
W_SF <- cor_test^2
W_SF


















#--------------------------Heteroscedasticity---------------------

# data generation
set.seed(123)
n <- 50
beta0 <- 1
beta1 <- 2
sigma2 <- 5
sigma <- sqrt(sigma2)

# regressors
X <- 1 + (0:(n-1))*0.2

# heteroscedastic errors using espi_i=sigma*X_i*z_i such that Var(eps_i)=sigma^2*X_i^2
z <- rnorm(n)
eps <- sigma * X * z

# response
Y <- beta0 + beta1*X + eps

Xmat <- cbind(1, X)

# ------------------------------
# OLS estimator, Fitted values and Residuals
# ------------------------------
beta_hat <- solve(t(Xmat)%*%Xmat) %*% t(Xmat) %*% Y

Yhat <- Xmat %*% beta_hat

e <- Y - Yhat

# ------------------------------
# estimated key quantities
# ------------------------------
beta_hat

# ------------------------------
# Hat matrix
# ------------------------------
H <- Xmat %*% solve(t(Xmat)%*%Xmat) %*% t(Xmat)
h <- diag(H)


# ------------------------------
# Standardized residuals  and ordered studentized residuals
# ------------------------------
std_res <- e / (s*sqrt(1-h))

plot(Yhat, e,
     xlab="Fitted values",
     ylab="Residuals",
     main="Residuals vs Fitted")

abline(h=0,col="red")

plot(Yhat, abs(e),
     xlab="Fitted values",
     ylab="|Residuals|",
     main="Absolute Residual Plot")

plot(Yhat, e^2,
     xlab="Fitted values",
     ylab="Squared Residuals",
     main="Squared Residual Plot")



# Spearman rank correlation test between fitted values and absolute residuals
cor_test <- cor(Yhat, abs(e), method="spearman")
cor_test

cor_test_manual <- cor(rank(Yhat), rank(abs(e)))
cor_test_manual

rho<- cor_test

t_stat <- rho * sqrt((n-2)/(1 - rho^2))
t_stat

p_value <- 2 * (1 - pt(abs(t_stat), df = n-2))
p_value




# White test for heteroscedasticity
# step 1: regress squared residuals on fitted values and their squares
Y2 <- e^2
X_white <- cbind(1, Yhat, Yhat^2)
White_model <- lm(Y2~X_white -1)  # -1 to exclude intercept since it's already included in X_white

# step 2: compute R^2 and test statistic from this regression
R2_white <- summary(White_model)$r.squared
LM_stat <- n*R2_white
LM_stat

pvalue_white <- 1 - pchisq(LM_stat, df=2)   # df= 2 since we have 2 regressors Yhat and Yhat^2
pvalue_white
if(pvalue_white < 0.05 ){
  cat("Reject H0: Heteroscedasticity is present\n")
} else {
  cat("Do not reject H0: No evidence of heteroscedasticity\n")
}


# Let we are interested in estimating a'beta  where a=(4,5) then Find estimate of a'beta 
# using OLS and GLS  and compare their variances 
a <- c(4,5) 
Omega <- diag(X^2)

# OLS estimator 
a_beta_OLS <- a %*% solve(t(Xmat)%*%Xmat) %*% t(Xmat) %*% Y
a_beta_OLS

# Variance of a'beta_OLS
Var_a_beta_OLS <- t(a) %*% solve(t(Xmat)%*%Xmat) %*%
  t(Xmat)%*%Omega%*%Xmat %*%
  solve(t(Xmat)%*%Xmat) %*% a

Var_a_beta_OLS



# GLS estimator

a_beta_GLS <- t(a) %*% solve(t(Xmat)%*%solve(Omega)%*%Xmat) %*%
  t(Xmat)%*%solve(Omega)%*%Y

a_beta_GLS
# Variance of a'beta_GLS

Var_a_beta_GLS <- t(a) %*% solve(t(Xmat)%*%solve(Omega)%*%Xmat) %*% a
Var_a_beta_GLS


# Values 
beta <- c(beta0, beta1)

a_beta_true <- a %*% beta

a_beta_true
a_beta_OLS
a_beta_GLS
Var_a_beta_OLS
Var_a_beta_GLS











#--------------Autocorrelation Test-------------------
library(lmtest)
set.seed(123)

n <- 50

beta0 <- 1
beta1 <- 2

rho <- 0.7
sigma <- 1

# regressors
# regressors
X <- 1 + (0:(n-1))*0.2

# generate AR(1) errors
u <- rnorm(n,0,sigma)

eps <- numeric(n)
eps[1] <- u[1]

for(i in 2:n){
  eps[i] <- rho*eps[i-1] + u[i]
}

# generate Y
Y <- beta0 + beta1*X + eps
Xmat <- cbind(1, X)
# OLS estimator
beta_hat <- solve(t(Xmat)%*%Xmat) %*% t(Xmat) %*% Y
Yhat <- Xmat %*% beta_hat
e <- Y-Yhat


plot(e,type="l",
     main="Residuals over time",
     ylab="Residuals",
     xlab="Time")

model <- lm(Y ~ X)
dwtest(model)


# Durbin watson test statistics
DW_stat <- sum(diff(e)^2)/sum(e^2)
DW_stat
