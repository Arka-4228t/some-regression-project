# -------------------------
# Load data
# -------------------------
data(cement)
n <- nrow(cement)

# -------------------------
# Full model (for sigma^2)
# -------------------------
full_model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
sigma2_hat <- summary(full_model)$sigma^2   # MSE of full model

# -------------------------
# Initialize
# -------------------------
remaining <- c("x1","x2","x3","x4")
selected <- c()

# Threshold F value (we can adjust)
F_in <- 4

step <- 1

# -------------------------
# Forward Selection Loop
# -------------------------
while(length(remaining) > 0){
  
  cat("\n-------------------------\n")
  cat("Step", step, "\n")
  
  F_values <- c()
  
  # Fit current model
  if(length(selected) == 0){
    current_model <- lm(y ~ 1, data = cement)
  } else {
    current_formula <- as.formula(
      paste("y ~", paste(selected, collapse = "+"))
    )
    current_model <- lm(current_formula, data = cement)
  }
  
  RSS_current <- deviance(current_model)
  
  # Try adding each remaining variable
  for(var in remaining){
    
    new_vars <- c(selected, var)
    new_formula <- as.formula(
      paste("y ~", paste(new_vars, collapse = "+"))
    )
    
    new_model <- lm(new_formula, data = cement)
    RSS_new <- deviance(new_model)
    
    # Partial F-statistic
    SSR_added <- RSS_current - RSS_new
    F_val <- SSR_added / sigma2_hat
    
    F_values[var] <- F_val
  }
  
  print(F_values)
  
  # Select variable with max F
  best_var <- names(which.max(F_values))
  best_F <- max(F_values)
  
  cat("Selected variable:", best_var, "\n")
  cat("F value:", best_F, "\n")
  
  # Check stopping condition
  if(best_F < F_in){
    cat("Stopping: No variable meets F_in criterion\n")
    break
  }
  
  # Update lists
  selected <- c(selected, best_var)
  remaining <- setdiff(remaining, best_var)
  
  cat("Model now:", paste(selected, collapse = ", "), "\n")
  
  step <- step + 1
}

# -------------------------
# Final Model
# -------------------------
cat("\nFinal Selected Model:\n")
final_formula <- as.formula(
  paste("y ~", paste(selected, collapse = "+"))
)

final_model <- lm(final_formula, data = cement)
summary(final_model)

#####-----BACKWARD ELIMINATION-------#####


# -------------------------
# Full model
# -------------------------
full_model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)

# Estimate sigma^2
sigma2_hat <- summary(full_model)$sigma^2

# -------------------------
# Initialize
# -------------------------
current_vars <- c("x1","x2","x3","x4")

# Threshold (you can adjust)
F_out <- 4

step <- 1

# -------------------------
# Backward Elimination Loop
# -------------------------
while(length(current_vars) > 0){
  
  cat("\n-------------------------\n")
  cat("Step", step, "\n")
  
  # Fit current model
  current_formula <- as.formula(
    paste("y ~", paste(current_vars, collapse = "+"))
  )
  
  current_model <- lm(current_formula, data = cement)
  RSS_full <- deviance(current_model)
  
  F_values <- c()
  
  # Try removing each variable
  for(var in current_vars){
    
    reduced_vars <- setdiff(current_vars, var)
    
    # Reduced model
    if(length(reduced_vars) == 0){
      reduced_model <- lm(y ~ 1, data = cement)
    } else {
      reduced_formula <- as.formula(
        paste("y ~", paste(reduced_vars, collapse = "+"))
      )
      reduced_model <- lm(reduced_formula, data = cement)
    }
    
    RSS_reduced <- deviance(reduced_model)
    
    # Partial F
    SSR_loss <- RSS_reduced - RSS_full
    F_val <- SSR_loss / sigma2_hat
    
    F_values[var] <- F_val
  }
  
  print(F_values)
  
  # Find smallest F
  worst_var <- names(which.min(F_values))
  min_F <- min(F_values)
  
  cat("Variable to remove:", worst_var, "\n")
  cat("F value:", min_F, "\n")
  
  # Check stopping rule
  if(min_F >= F_out){
    cat("Stopping: all variables significant\n")
    break
  }
  
  # Remove variable
  current_vars <- setdiff(current_vars, worst_var)
  
  cat("Model now:", paste(current_vars, collapse = ", "), "\n")
  
  step <- step + 1
}

# -------------------------
# Final Model
# -------------------------
cat("\nFinal Model:\n")
final_formula <- as.formula(
  paste("y ~", paste(current_vars, collapse = "+"))
)

final_model <- lm(final_formula, data = cement)
summary(final_model)

########----STEPWISE REGRESSION---------##########

# -------------------------
# Full model (for sigma^2)
# -------------------------
full_model <- lm(y ~ x1 + x2 + x3 + x4, data = cement)
sigma2_hat <- summary(full_model)$sigma^2

# -------------------------
# Initialize
# -------------------------
remaining <- c("x1","x2","x3","x4")
selected <- c()

# Thresholds
F_in <- 4
F_out <- 3

step <- 1

# -------------------------
# Stepwise Loop
# -------------------------
repeat{
  
  cat("\n=========================\n")
  cat("Step", step, "\n")
  
  # -------------------------
  # FORWARD STEP
  # -------------------------
  F_forward <- c()
  
  if(length(selected) == 0){
    current_model <- lm(y ~ 1, data = cement)
  } else {
    current_model <- lm(
      as.formula(paste("y ~", paste(selected, collapse="+"))),
      data = cement)
  }
  
  RSS_current <- deviance(current_model)
  
  for(var in remaining){
    
    new_vars <- c(selected, var)
    
    new_model <- lm(
      as.formula(paste("y ~", paste(new_vars, collapse="+"))),
      data = cement)
    
    RSS_new <- deviance(new_model)
    
    SSR_added <- RSS_current - RSS_new
    F_val <- SSR_added / sigma2_hat
    
    F_forward[var] <- F_val
  }
  
  print(F_forward)
  
  if(length(F_forward) > 0){
    best_var <- names(which.max(F_forward))
    best_F <- max(F_forward)
    
    if(best_F > F_in){
      selected <- c(selected, best_var)
      remaining <- setdiff(remaining, best_var)
      
      cat("Added:", best_var, "\n")
    }
  }
  
  # -------------------------
  # BACKWARD STEP
  # -------------------------
  if(length(selected) > 0){
    
    F_backward <- c()
    
    current_model <- lm(
      as.formula(paste("y ~", paste(selected, collapse="+"))),
      data = cement)
    
    RSS_full <- deviance(current_model)
    
    for(var in selected){
      
      reduced_vars <- setdiff(selected, var)
      
      if(length(reduced_vars) == 0){
        reduced_model <- lm(y ~ 1, data = cement)
      } else {
        reduced_model <- lm(
          as.formula(paste("y ~", paste(reduced_vars, collapse="+"))),
          data = cement)
      }
      
      RSS_reduced <- deviance(reduced_model)
      
      SSR_loss <- RSS_reduced - RSS_full
      F_val <- SSR_loss / sigma2_hat
      
      F_backward[var] <- F_val
    }
    
    print(F_backward)
    
    worst_var <- names(which.min(F_backward))
    min_F <- min(F_backward)
    
    if(min_F < F_out){
      selected <- setdiff(selected, worst_var)
      remaining <- union(remaining, worst_var)
      
      cat("Removed:", worst_var, "\n")
    }
  }
  
  # -------------------------
  # Stop condition
  # -------------------------
  if((length(F_forward)==0 || max(F_forward) <= F_in) &&
     (length(selected)==0 || min(F_backward) >= F_out)){
    cat("Stopping condition reached\n")
    break
  }
  
  step <- step + 1
}

# -------------------------
# Final Model
# -------------------------
cat("\nFinal Model:\n")
final_model <- lm(
  as.formula(paste("y ~", paste(selected, collapse="+"))),
  data = cement)

summary(final_model)