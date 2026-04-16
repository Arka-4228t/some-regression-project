# Create the data frame
X8 <- c(35.3,29.7,30.8,58.8,61.4,71.3,74.4,76.7,70.7,57.5,
        46.4,28.9,28.1,39.1,46.8,48.5,59.3,70.0,70.0,74.5,
        72.1,58.1,44.6,33.4,28.6)

X6 <- c(20,20,23,20,21,22,11,23,21,20,
        20,21,21,19,23,20,22,22,11,23,
        20,21,20,20,22)

Y <- c(10.98,11.13,12.51,8.40,9.27,8.73,6.36,8.50,7.82,9.14,
       8.24,12.19,11.88,9.57,10.94,9.58,10.09,8.11,6.83,8.88,
       7.68,8.47,8.86,10.36,11.08)

data <- data.frame(Y, X6, X8)

# Fit model
model_X6 <- lm(Y ~ X6, data = data)

# Summary (coefficients, R^2, t-tests)
summary(model_X6)

# ANOVA table
anova(model_X6)

plot(X6, Y,
     main = "Y vs X6 with fitted line",
     xlab = "X6", ylab = "Y")
abline(model_X6, col = "blue")


res_X6 <- residuals(model_X6)
fitted_X6 <- fitted(model_X6)

plot(X6, res_X6,
     main = "Residuals vs X6",
     xlab = "X6", ylab = "Residuals")
abline(h = 0, lty = 2)

plot(fitted_X6, res_X6,
     main = "Residuals vs Fitted (Y~X6)",
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, lty = 2)



qqnorm(res_X6)
qqline(res_X6, col = "blue")



# Fit model
model_X8 <- lm(Y ~ X8, data = data)

# Summary
summary(model_X8)

# ANOVA table
anova(model_X8)

plot(X8, Y,
     main = "Y vs X8 with fitted line",
     xlab = "X8", ylab = "Y")
abline(model_X8, col = "red")


res_X8 <- residuals(model_X8)
fitted_X8 <- fitted(model_X8)


plot(X8, res_X8,
     main = "Residuals vs X8",
     xlab = "X8", ylab = "Residuals")
abline(h = 0, lty = 2)


plot(fitted_X8, res_X8,
     main = "Residuals vs Fitted (Y~X8)",
     xlab = "Fitted values", ylab = "Residuals")
abline(h = 0, lty = 2)


qqnorm(res_X8)
qqline(res_X8, col = "red")



# Fit multiple regression
model_full <- lm(Y ~ X6 + X8, data = data)

# Summary
summary(model_full)

# ANOVA table
anova(model_full)

summary(model_X6)$r.squared
summary(model_X8)$r.squared
summary(model_full)$r.squared

res_full <- residuals(model_full)
qqnorm(res_full)
qqline(res_full, col = "green")



#------------------END-------------------------------------------











#---------------END---------------------------
# Step 1: residuals of Y on X8
res_Y_X8 <- residuals(lm(Y ~ X8, data = data))

# Step 2: residuals of X6 on X8
res_X6_X8 <- residuals(lm(X6 ~ X8, data = data))

# Step 3: plot
plot(res_X6_X8, res_Y_X8,
     main = "Partial Regression Plot for X6",
     xlab = "Residuals of X6 | X8",
     ylab = "Residuals of Y | X8")

abline(lm(res_Y_X8 ~ res_X6_X8), col = "blue")


# Step 1: residuals of Y on X6
res_Y_X6 <- residuals(lm(Y ~ X6, data = data))

# Step 2: residuals of X8 on X6
res_X8_X6 <- residuals(lm(X8 ~ X6, data = data))

# Step 3: plot
plot(res_X8_X6, res_Y_X6,
     main = "Partial Regression Plot for X8",
     xlab = "Residuals of X8 | X6",
     ylab = "Residuals of Y | X6")

abline(lm(res_Y_X6 ~ res_X8_X6), col = "red")
