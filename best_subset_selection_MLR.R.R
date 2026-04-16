
install.packages(c("MASS","car","lmtest"))
install.packages("car", dependencies = TRUE)
# Load libraries
library(MASS)      #contains Hald cement dataset
library(carData)      #For VIF (multicollinearity check)
library(lmtest)
library(car)
# DATA LOADING
data(cement, package = "MASS")

# View data
print(cement)
n <- nrow(cement)

##### ALL POSSIBLE MODELS #####

##----Model 0 (No Predictor – Intercept Only)-----##
model0 <- lm(y ~ 1, data = cement)

summary(model0)
deviance(model0)
MSE0 <- deviance(model0)/(n - 0 - 1)

##----Models with One Predictor (4 Models)----##
model1 <- lm(y ~ x1, data = cement)
summary(model1)
deviance(model1)
MSE1 <- deviance(model1)/(n - 1 - 1)

# Adjusted R2
summary(model1)$adj.r.squared

#################################
model2 <- lm(y ~ x2, data = cement)
summary(model2)
deviance(model2)
MSE2 <- deviance(model2)/(n - 1 - 1)

summary(model2)$adj.r.squared

#################################
model3 <- lm(y ~ x3, data = cement)
summary(model3)
deviance(model3)
MSE3 <- deviance(model3)/(n - 1 - 1)

summary(model3)$adj.r.squared

#################################
model4 <- lm(y ~ x4, data = cement)
summary(model4)
deviance(model4)
MSE4 <- deviance(model4)/(n - 1 - 1)
summary(model4)$adj.r.squared

##----Models with Two Predictors (6 Models)----##

model5 <- lm(y ~ x1 + x2, data = cement)
summary(model5)
deviance(model5)
MSE5 <- deviance(model5)/(n - 2 - 1)

summary(model5)$adj.r.squared

vif(model5)                       # multicollinearity

##################################
model6 <- lm(y ~ x1 + x3, data = cement)
summary(model6)
deviance(model6)
MSE6 <- deviance(model6)/(n - 2 - 1)
summary(model6)$adj.r.squared

vif(model6)

##################################
model7 <- lm(y ~ x1 + x4, data = cement)
summary(model7)
deviance(model7)
MSE7 <- deviance(model7)/(n - 2 - 1)
summary(model7)$adj.r.squared

vif(model7)

##################################
model8 <- lm(y ~ x2 + x3, data = cement)
summary(model8)
deviance(model8)
MSE8 <- deviance(model8)/(n - 2 - 1)
summary(model8)$adj.r.squared

vif(model8)

##################################
model9 <- lm(y ~ x2 + x4, data = cement)
summary(model9)
deviance(model9)
MSE9 <- deviance(model9)/(n - 2 - 1)
summary(model9)$adj.r.squared

vif(model9)

##################################
model10 <- lm(y ~ x3 + x4, data = cement)
summary(model10)
deviance(model10)
MSE10 <- deviance(model10)/(n - 2 - 1)
summary(model10)$adj.r.squared

vif(model10)

##################################
##-------Models with Three Predictors (4 Models)--------##

model11 <- lm(y ~ x1 + x2 + x3, data = cement)
summary(model11)
deviance(model11)
MSE11 <- deviance(model11)/(n - 3 - 1)
summary(model11)$adj.r.squared

vif(model11)

####################################
model12 <- lm(y ~ x1 + x2 + x4, data = cement)
summary(model12)
deviance(model2)
MSE12 <- deviance(model12)/(n - 3 - 1)
summary(model12)$adj.r.squared

vif(model12)

##################################
model13 <- lm(y ~ x1 + x3 + x4, data = cement)
summary(model13)
deviance(model3)
MSE13 <- deviance(model13)/(n - 3 - 1)
summary(model13)$adj.r.squared

vif(model13)

###################################
model14 <- lm(y ~ x2 + x3 + x4, data = cement)
summary(model14)
deviance(model4)
MSE14 <- deviance(model14)/(n - 3 - 1)
summary(model14)$adj.r.squared

vif(model14)

####################################

##-----------Full Model (All Predictors)-----------##
model15 <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
summary(model15)
deviance(model5)
MSE15 <- deviance(model15)/(n - 4 - 1)

sigma2_hat <- summary(model15)$sigma^2
sigma2_hat

summary(model15)$adj.r.squared

vif(model15)

#######################################################
####-----PLOT R_2 VS p-------------####

R2 <- c(
  summary(model1)$r.squared,
  summary(model2)$r.squared,
  summary(model3)$r.squared,
  summary(model4)$r.squared,
  
  summary(model5)$r.squared,
  summary(model6)$r.squared,
  summary(model7)$r.squared,
  summary(model8)$r.squared,
  summary(model9)$r.squared,
  summary(model10)$r.squared,
  
  summary(model11)$r.squared,
  summary(model12)$r.squared,
  summary(model13)$r.squared,
  summary(model14)$r.squared,
  
  summary(model15)$r.squared
)
R2

####---DEFINE NUMBER OF PREDICTORS p---####

p <- c(
  1,1,1,1,
  2,2,2,2,2,2,
  3,3,3,3,
  4
)
p
####---COMPUTE Rp_2----####
Rp2 <- tapply(R2, p, max)
Rp2

####---plot Rp_2 vs p----####


# Rp^2 values (already computed earlier)
Rp2 <- tapply(R2, p, max)

## p-values for plot ##

p<- 1:4

# monotone spline interpolation
smooth_curve <- splinefun(p, Rp2, method = "monoH.FC")

# create smooth grid
p_smooth <- seq(1,4,length=100)
Rp2_smooth <- smooth_curve(p_smooth)

# plot
plot(p_smooth, Rp2_smooth,
     type="l",
     lwd=2,
     xlab="p",
     ylab=expression(R[p]^2),
     main=expression("Plot of " * R[p]^2 * " versus p"),
     xaxt="n")

axis(1, at=1:4)
points(p, Rp2, pch=19)

#####------PLOT MSRes_p vs p---------#####

rss <- c(
  deviance(model1), deviance(model2), deviance(model3), deviance(model4),
  deviance(model5), deviance(model6), deviance(model7), deviance(model8), deviance(model9), deviance(model10),
  deviance(model11), deviance(model12), deviance(model13), deviance(model14),
  deviance(model15)
)

p <- c(
  1,1,1,1,
  2,2,2,2,2,2,
  3,3,3,3,
  4
)

RSSp <- tapply(rss,p,min)

###---COMPUTE MSRes----###


MSRes <- RSSp/(n - (1:4) - 1)
MSRes

smooth <- splinefun(1:4, MSRes, method="monoH.FC")

p_smooth <- seq(1,4,length=100)
MS_smooth <- smooth(p_smooth)

plot(p_smooth,MS_smooth,
     type="l",
     lwd=2,
     xlab="p",
     ylab=expression(MS[Res](p)),
     main=expression("Plot of "*MS[Res](p)*" versus p"),
     xaxt="n")

axis(1,at=1:4)

points(1:4,MSRes,pch=19)

######------Compute C_p ---------#####

Cp0 <- deviance(model0)/sigma2_hat - n + 2*(0+1)

###--- p = 1 ----###


Cp1 <- deviance(model1)/sigma2_hat - n + 2*(1+1)

Cp2 <- deviance(model2)/sigma2_hat - n + 2*(1+1)

Cp3 <- deviance(model3)/sigma2_hat - n + 2*(1+1)

Cp4 <- deviance(model4)/sigma2_hat - n + 2*(1+1)

###---- p = 2 ----###


Cp5 <- deviance(model5)/sigma2_hat - n + 2*(2+1)

Cp6 <- deviance(model6)/sigma2_hat - n + 2*(2+1)

Cp7 <- deviance(model7)/sigma2_hat - n + 2*(2+1)

Cp8 <- deviance(model8)/sigma2_hat - n + 2*(2+1)

Cp9 <- deviance(model9)/sigma2_hat - n + 2*(2+1)

Cp10 <- deviance(model10)/sigma2_hat - n + 2*(2+1)

###----- p = 3 -----###


Cp11 <- deviance(model11)/sigma2_hat - n + 2*(3+1)

Cp12 <- deviance(model12)/sigma2_hat - n + 2*(3+1)

Cp13 <- deviance(model13)/sigma2_hat - n + 2*(3+1)

Cp14 <- deviance(model14)/sigma2_hat - n + 2*(3+1)

###----- p = 4 -----###

Cp15 <- deviance(model15)/sigma2_hat - n + 2*(4+1)

###----SUMMARY TABLE----###

result <- data.frame(
  
  # Number of regressors (including intercept)
  p = c(1,2,2,2,2,3,3,3,3,3,3,4,4,4,4,5),
  
  # Regressors
  Regressors = c(
    "None",
    "x1","x2","x3","x4",
    "x1,x2","x1,x3","x1,x4","x2,x3","x2,x4","x3,x4",
    "x1,x2,x3","x1,x2,x4","x1,x3,x4","x2,x3,x4",
    "x1,x2,x3,x4"
  ),
  
  # SSRes(p)
  SSRes = c(
    2715.7635,
    1265.6867, 906.3363, 1939.4005, 883.8669,
    57.9045, 1227.0721, 74.7621, 415.4427, 868.8801, 175.7380,
    48.1106, 47.9727, 50.8361, 73.8145,
    47.8636
  ),
  
  # R^2
  R2 = c(
    0,
    0.53395, 0.66627, 0.28587, 0.67459,
    0.97868, 0.54817, 0.97247, 0.84703, 0.68006, 0.93529,
    0.98228, 0.98234, 0.98128, 0.97282,
    0.98238
  ),
  
  # Adjusted R^2
  Adj_R2 = c(
    0,
    0.49158, 0.63593, 0.22095, 0.64495,
    0.97441, 0.45780, 0.96697, 0.81644, 0.61007, 0.92235,
    0.97638, 0.97645, 0.97504, 0.96376,
    0.97356
  ),
  
  # MSRes(p)
  MSRes = c(
    226.3136,
    115.0624, 82.3942, 176.3092, 80.3515,
    5.7904, 122.7073, 7.4762, 41.5443, 86.8880, 17.5738,
    5.3456, 5.3303, 5.6485, 8.2017,
    5.9829
  ),
  
  # Cp
  Cp = c(
    442.92,
    202.55, 142.49, 315.16, 138.73,
    2.68, 198.10, 5.50, 62.44, 138.23, 22.37,
    3.04, 3.02, 3.50, 7.34,
    5.00
  )
  
)

print(result)

# -------------------------
# Data (from table)
# -------------------------

# Number of parameters (including intercept)
p <- c(
  1,                # intercept only
  2,2,2,2,          # 1 predictor
  3,3,3,3,3,3,      # 2 predictors
  4,4,4,4,          # 3 predictors
  5                 # full model
)

# R^2 values
R2 <- c(0,
  0.53395, 0.66627, 0.28587, 0.67459,
  0.97868, 0.54817, 0.97247, 0.84703, 0.68006, 0.93529,
  0.98228, 0.98234, 0.98128, 0.97282,
  0.98238
)


##### R_p ^2 vs p ######
# -------------------------
# Colors (group-wise)
# -------------------------
col_vec <- c(
  "black",               # p=1
  rep("red",4),          # p=2
  rep("blue",6),         # p=3
  rep("darkgreen",4),    # p=4
  "purple"               # p=5
)

# -------------------------
# Plot
# -------------------------
plot(p, R2,
     pch=19,
     col=col_vec,
     xlim=c(0.5,7.5),
     ylim=c(0.25,1.02),
     xlab="Number of parameters (p)",
     ylab=expression(R[p]^2),
     main=expression("Plot of " * R[p]^2 * " versus p"),
     xaxt="n")

axis(1, at=2:5)
box()

###################################################################


############ MSE vs p PLOT #########################3


# Number of parameters (including intercept)
p <- c(
  1,                # intercept only
  2,2,2,2,          # 1 predictor
  3,3,3,3,3,3,      # 2 predictors
  4,4,4,4,          # 3 predictors
  5                 # full model
)

# MSRes values
MSRes <- c(
  226.3136,
  115.0624, 82.3942, 176.3092, 80.3515,
  5.7904, 122.7073, 7.4762, 41.5443, 86.8880, 17.5738,
  5.3456, 5.3303, 5.6485, 8.2017,
  5.9829
)

# -------------------------
# Colors (group-wise clarity)
# -------------------------
col_vec <- c(
  "black",               # p=1
  rep("red",4),          # p=2
  rep("blue",6),         # p=3
  rep("darkgreen",4),    # p=4
  "purple"               # p=5
)

# -------------------------
# Slight jitter to avoid overlap
# -------------------------
set.seed(1)
p_plot <- jitter(p, amount=0.05)

# -------------------------
# Plot
# -------------------------
plot(p_plot, MSRes,
     pch=19,
     col=col_vec,
     xlim=c(0.5,7.5),
     ylim=c(0,230),
     xlab="Number of parameters (p)",
     ylab=expression(MS[Res](p)),
     main=expression("Plot of " * MS[Res](p) * " versus p"),
     xaxt="n")

axis(1, at=1:5)
box()



