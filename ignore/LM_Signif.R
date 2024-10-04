model <- lm(es ~ Value, data = OR_comp)
model_summary = summary(model)
f_statistic <- model_summary$fstatistic
p_value <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)

if (p_value < 0.05) {
  print("The model is significant.")
} else {
  print("The model is not significant.")
}


#---------------------
# List of models to evaluate
models <- list(
  lm1 = lm(es ~ Value, data = OR_comp),
  lm2 = lm(es ~ Correlation, data = Beta_comp),
  lm3 = lm(es ~ Correlation, data = B_comp),
  lm4 = lm(es ~ Correlation, data = R_comp),
  lm5 = lm(es ~ Value, data = Ratios_comp),
  lm6 = lm(es ~ Value, data = z_comp),
  lm7 = lm(es ~ Value, data = F_comp),
  lm8 = lm(es ~ Value, data = Est_comp),
  lm9 = lm(es ~ Correlation, data = p_comp),
  lm10 = lm(es ~ Value, data = Rsquare_comp)
)

# Function to check significance
check_significance <- function(model) {
  model_summary <- summary(model)
  f_statistic <- model_summary$fstatistic
  p_value <- pf(f_statistic[1], f_statistic[2], f_statistic[3], lower.tail = FALSE)
  
  cat("F-statistic:", f_statistic[1], "\n")
  cat("p-value:", p_value, "\n")
  
  if (p_value < 0.05) {
    cat("The model is significant.\n\n")
  } else {
    cat("The model is not significant.\n\n")
  }
}

# Loop through each model and check significance
for (i in 1:length(models)) {
  cat("Model", i, ":\n")
  check_significance(models[[i]])
}
