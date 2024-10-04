es_categories


# summary tables ----------
effect_sizes = es_categories #%>% filter(Environmental_condition=="Temperature")
tmp = effect_sizes %>% group_by(es_cat) %>% summarise(total_n = sum(n))
tmp$prop = tmp$total_n/sum(tmp$total_n)
tmp_comb = tmp %>% mutate(effect_type = ifelse(es_cat == 'No effect', 'No effect', 'Non-zero effect')) %>%
  group_by(effect_type) %>%
  summarise(total_n = sum(total_n))
tmp_comb$prop = tmp_comb$total_n/sum(tmp_comb$total_n)

tmp_comb
tmp

round(tmp_comb$prop,3)*100

paste0(round(es_categories$prop[es_categories$es_cat=="No effect" & es_categories$Environmental_condition=="Precipitation"],3)*100,"%")

# Assuming your data is in a dataframe named 'es_categories'

# Summarize the total count for 'No effect' and 'Non-zero effect'
combined_effects <- es_categories %>%
  mutate(effect_type = ifelse(es_cat == 'No effect', 'No effect', 'Non-zero effect')) %>%
  group_by(effect_type) %>%
  summarise(total_n = sum(n))

print(combined_effects)

# Set the observed counts
observed <- combined_effects$total_n

# Assuming equal distribution for the expected counts
expected <- rep(sum(observed) / 2, 2)

# Perform chi-squared test
chi_test <- chisq.test(observed, p = expected / sum(observed))

# View results
print(chi_test)

#
# Function to perform chi-square tests on sampled data
perform_chi_tests_no_effect <- function(data, n_samples, percentage) {
  
  # Create a results dataframe to store chi-square statistics and p-values
  results <- data.frame(i = 1:n_samples)
  
  # Loop through the number of samples
  for (i in 1:n_samples) {
    
    # Sample a percentage of the data with replacement
    smp <- data[sample(1:nrow(data), size = round(nrow(data) * percentage), replace = TRUE),]
    
    # Group the data into two categories: No effect and Non-zero effect
    smp$Effect_category <- ifelse(smp$es_cat == "No effect", "No effect", "Non-zero effect")
    
    # Create contingency table (counts for each effect category)
    chi_test_data <- xtabs(n ~ Effect_category, data = smp)
    
    # Perform chi-square test on the sample
    chi_test <- chisq.test(chi_test_data)
    
    # Store chi-square statistic and p-value in the results dataframe
    results$chi_stat[i] <- chi_test$statistic
    results$p_value[i] <- chi_test$p.value
  }
  
  return(results)
}

# Example usage:
# Assuming your data is stored in a dataframe named 'es_data'
# and you want to perform 1000 chi-square tests on 80% of the data each time:
results <- perform_chi_tests_no_effect(es_categories, n_samples = 1000, percentage = 0.8)

# Print the results
head(results)


##--------------
# Function to perform chi-square tests on sampled data, sampling based on proportions
perform_chi_tests_combined <- function(data, n_samples, percentage) {
  
  # Create a results dataframe to store chi-square statistics and p-values
  results <- data.frame(i = 1:n_samples)
  
  # Extract the total number of observations in each category
  total_no_effect <- data$total_n[data$effect_type == "No effect"]
  total_non_zero <- data$total_n[data$effect_type == "Non-zero effect"]
  
  # Total count
  total_n <- total_no_effect + total_non_zero
  
  # Loop through the number of samples
  for (i in 1:n_samples) {
    
    # Sample 80% of the counts by randomly sampling proportions and then multiplying by the total counts
    prop_no_effect <- sum(sample(c(0, 1), size = total_no_effect, replace = TRUE, prob = c(1 - percentage, percentage)))
    prop_non_zero <- sum(sample(c(0, 1), size = total_non_zero, replace = TRUE, prob = c(1 - percentage, percentage)))
    
    # Create a contingency table for the sampled data
    smp <- data.frame(
      effect_type = c("No effect", "Non-zero effect"),
      total_n = c(prop_no_effect, prop_non_zero)
    )
    print(smp)
    # Create contingency table (counts for each effect category)
    chi_test_data <- xtabs(total_n ~ effect_type, data = smp)
    
    # Perform chi-square test on the sample
    chi_test <- chisq.test(chi_test_data)
    
    # Store chi-square statistic and p-value in the results dataframe
    results$chi_stat[i] <- chi_test$statistic
    results$p_value[i] <- chi_test$p.value
  }
  
  return(results)
}

# Example usage:
# Assuming your data is stored in a dataframe named 'combined_effects'
# and you want to perform 1000 chi-square tests on 80% of the data each time:
results <- perform_chi_tests_combined(combined_effects, n_samples = 50, percentage = 0.8)

# Print the results
mean(results$p_value)
head(results)


#-0------------------

# Create expanded dataframe based on total_n counts
expand_combined_effects <- function(data) {
  # Create an empty dataframe to store expanded rows
  expanded_df <- data.frame(effect_type = character(), stringsAsFactors = FALSE)
  
  # Loop through each effect type and expand rows based on total_n
  for (i in 1:nrow(data)) {
    expanded_rows <- data.frame(effect_type = rep(data$effect_type[i], data$total_n[i]), stringsAsFactors = FALSE)
    expanded_df <- rbind(expanded_df, expanded_rows)
  }
  
  return(expanded_df)
}

# Function to perform chi-square tests on sampled data
perform_chi_tests_expanded <- function(data, n_samples, percentage) {
  results <- data.frame(i = 1:n_samples)
  
  # Expand the data based on total counts
  expanded_df <- expand_combined_effects(data)
  expanded_df$effect_type = as.factor(expanded_df$effect_type)
  expanded_df$Dummy = 1
  
  for (i in 1:n_samples) {
    # Sample a random percentage of the expanded data
   smp <- expanded_df[sample(1:nrow(expanded_df), size = round(nrow(expanded_df) * percentage), replace = FALSE),]
   #smp <- expanded_df[sample(1:nrow(expanded_df), size = 1000, replace = TRUE),]
    #print(smp)
    # Create contingency table
    chi_test_data <- xtabs(data = smp, Dummy ~ effect_type)
    #print(chi_test_data)
    # Perform chi-square test
    chi_test <- chisq.test(chi_test_data)
    
    # Store chi-square statistic and p-value
    results$chi_stat[i] <- chi_test$statistic
    results$p_value[i] <- chi_test$p.value
  }
  
  return(results)
}

# Example usage
# Assuming your data is stored in a dataframe named 'combined_effects'
results <- perform_chi_tests_expanded(combined_effects, n_samples = 1000, percentage = 0.8)
mean(results$p_value)
# Check the first few results
head(results)

#Chi Square boot -----------------
# Load the boot package
library(boot)

# Define the chi-squared statistic function for bootstrapping
chi_squared_function <- function(data, indices) {
  # Sample the data (resample with replacement using indices)
  sampled_data <- data[indices, , drop = FALSE]  # Ensure sampled_data is a data frame
  #print(sampled_data)
  # Create a contingency table for the sampled data
  contingency_table <- table(sampled_data$effect_type)
  #print(contingency_table)
  print(sum(contingency_table))
  # Perform the chi-squared test on the resampled data
  chi_test <- chisq.test(contingency_table)
  
  # Return the chi-squared statistic
  return(chi_test$statistic)
}

# Create a sample dataset for the bootstrap test
# Replace this with your actual dataset
expanded_df <- data.frame(
  effect_type = c(rep("No effect", 236), rep("Non-zero effect", 299))
)

# Perform the bootstrap using the boot function
set.seed(123)  # For reproducibility
boot_results <- boot(data = expanded_df, statistic = chi_squared_function, R = 1000)

# View the bootstrap results
print(boot_results)

# Bias-corrected and accelerated method
boot_ci_bca <- boot.ci(boot_results, type = "bca")
print(boot_ci_bca)

# Extract the bootstrapped chi-square statistics
boot_statistics <- boot_results$t

# Original Chi-square statistic
original_statistic <- boot_results$t0

# Calculate the proportion of bootstrapped statistics that are greater than or equal to the original
p_value <- mean(boot_statistics >= original_statistic)

# Print the p-value
cat("Bootstrap-based p-value:", p_value, "\n")

# Get confidence intervals for the bootstrapped chi-squared statistic
# Percentile method
boot_ci_perc <- boot.ci(boot_results, type = "perc")
print(boot_ci_perc)

# Basic method
boot_ci_basic <- boot.ci(boot_results, type = "basic")
print(boot_ci_basic)

# Normal method
boot_ci_normal <- boot.ci(boot_results, type = "norm")
print(boot_ci_normal)

# Bias-corrected and accelerated method
boot_ci_bca <- boot.ci(boot_results, type = "bca")
print(boot_ci_bca)


# Plot histogram of bootstrap statistics
hist(boot_results$t, main = "Bootstrap Distribution of Chi-Squared Statistic", xlab = "Chi-Squared Statistic", breaks = 30)

# Plot density plot for more smooth distribution
plot(density(boot_results$t), main = "Bootstrap Density of Chi-Squared Statistic", xlab = "Chi-Squared Statistic")

#-----------
install.packages("boot")
library(boot)
combined_effects <- data.frame(
  effect_type = c("No effect", "Non-zero effect"),
  total_n = c(236, 299)
)
# Create an expanded dataset based on the counts
expanded_df <- data.frame(
  effect_type = rep(c("No effect", "Non-zero effect"), times = c(236, 299))
)

# Define the proportion function
proportion_function <- function(data, indices) {
  # Resample the data based on the indices
  sampled_data <- data[indices, , drop = FALSE]  # Ensure data remains a data frame
  # Calculate the proportion of "No effect"
  prop_no_effect <- mean(sampled_data[["effect_type"]] == "No effect")
  return(prop_no_effect)
}
# Run bootstrapping
set.seed(123)  # For reproducibility
boot_results <- boot(data = expanded_df, 
                     statistic = proportion_function, 
                     R = 1000)  # 1000 bootstrap samples
# View bootstrapped results
print(boot_results)

# Get confidence intervals
boot.ci(boot_results, type = "perc")  # Percentile method for CIs

#Permutation ---------------------------------------------------
# Permutation test for comparing proportions (No effect vs Non-zero effect)
set.seed(123)

# Function to calculate the chi-squared statistic on permuted data
perm_test_function <- function(data) {
  # Shuffle (permute) the effect_type labels
  permuted_data <- data
  permuted_data$effect_type <- sample(permuted_data$effect_type)
  
  # Perform the chi-squared test on permuted data
  chi_test_perm <- chisq.test(table(permuted_data$effect_type))
  return(chi_test_perm$statistic)
}

# Original chi-squared test on the observed data
original_test <- chisq.test(table(expanded_df$effect_type))
original_stat <- original_test$statistic

# Number of permutations
n_permutations <- 1000

# Perform permutation test
perm_results <- replicate(n_permutations, perm_test_function(expanded_df))

# Calculate the p-value as the proportion of permuted statistics greater than or equal to the observed
perm_p_value <- mean(perm_results >= original_stat)

# Print the results
cat("Original Chi-squared statistic:", original_stat, "\n")
cat("Permutation-based p-value:", perm_p_value, "\n")


# Boot permut -----------
# Load necessary library
library(boot)

# Define your observed data (contingency table)
# Here we have 236 'No effect' and 299 'Non-zero effect' occurrences
observed_counts <- c(rep("No effect", 236), rep("Non-zero effect", 299))

# Define a function to calculate the Chi-square statistic from the permuted data
perm_chi_squared_test <- function(data, indices) {
  # Permute the observed data
  permuted_data <- data[indices]
  
  # Rebuild the contingency table from permuted data
  permuted_table <- table(permuted_data)
  
  # Ensure both categories are present (for safety)
  if (length(permuted_table) == 2) {
    # Calculate the Chi-Square statistic for the permuted table
    chisq.test(permuted_table)$statistic
  } else {
    return(NA)  # Return NA if one category is missing in the permutation
  }
}

# Run the bootstrapping with permutations
set.seed(123)  # for reproducibility

# Bootstrapping with permutation using 1000 iterations
boot_results <- boot(data = observed_counts, statistic = perm_chi_squared_test, R = 1000, sim = "permutation")

# Output the bootstrap results
print(boot_results)

# Get confidence intervals for the bootstrapped Chi-squared statistic
boot_ci <- boot.ci(boot_results, type = "perc")
print(boot_ci)