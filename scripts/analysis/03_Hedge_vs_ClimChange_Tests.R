# Hedges g vs Climate change tests

# 0. Load libraries -----,=

library(tidyverse)
library(here)
library(knitr)
library(magrittr)
library(kableExtra)  
library(ggplot2)     
library(dplyr)
library(tidyr)
library(reshape2)
library(gridExtra)

# First need to run the `climate_variables.R` script 
# and get the `es_climvars_proportions.csv` dataframe. 
# 1. Load data ------
# Load the dataframe with calculated climate data and dominant climate change categories.

dat = read.csv(here('data', 'es_climvars_proportions_new.csv'))

# Ensure 'es_cat' is a factor with ordered levels
dat <- dat %>%
  mutate(es_cat = factor(es_cat,
                         levels = c('Negative effect (g < -0.2)', 'Positive effect (g > 0.2)'),
                         ordered = TRUE))

dat %<>% filter(es_cat!="No effect")

# Subset the data based on Environmental_condition
dat_temp <- dat %>%
  filter(Environmental_condition == "Temperature" & !is.na(es_cat))

dat_prec <- dat %>%
  filter(Environmental_condition == "Precipitation" & !is.na(es_cat))

# 2. Set up functions ------
# Function to determine the appropriate statistical test based on the initial dataset
determine_initial_test <- function(data, effect_cat, bin_var, all_effect_cats) {
  # Create the initial contingency table
  cont_table <- table(data[[effect_cat]], data[[bin_var]])
  
  # Ensure all effect size categories are represented
  cont_table <- cont_table[all_effect_cats, , drop = FALSE]
  cont_table[is.na(cont_table)] <- 0
  
  # Perform Chi-Square Test to check assumptions
  chi_test <- tryCatch(chisq.test(cont_table), error = function(e) NULL)
  
  if (!is.null(chi_test) && all(chi_test$expected >= 5)) {
    return("Chi-Square Test")
  } else {
    return("Fisher's Exact Test")
  }
}


# Updated function to perform statistical tests with bootstrapping using a single test type
perform_stat_tests_subset_bootstrap_single_test <- function(data, effect_cat, agreement_var, bin_var, climate_type,
                                                            num_iterations = 1000,
                                                            sample_size = 0.8,
                                                            sampling_method = "bootstrap",
                                                            seed = 123) {
  
  # Initialize vectors to store results
  p_values <- numeric(num_iterations)
  test_stats <- numeric(num_iterations)
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Define all possible effect size categories (excluding "No effect")
  all_effect_cats <- c("Negative effect (g < -0.2)", "Positive effect (g > 0.2)")
  
  # Determine the appropriate test based on the initial dataset
  test_type <- determine_initial_test(data, effect_cat, bin_var, all_effect_cats)
  
  # Inform the user about the chosen test
  #cat(paste("Using", test_type, "for all bootstrapped iterations."))
  
  for (i in 1:num_iterations) {
    # Sample the data based on the selected method
    if (sampling_method == "bootstrap") {
      sample_data <- data %>% sample_frac(size = 1, replace = TRUE)  # Bootstrap: Sampling with replacement
    } else if (sampling_method == "random") {
      sample_data <- data %>% sample_frac(size = sample_size, replace = FALSE)  # Random subsample: e.g., 80% without replacement
    } else {
      stop("Invalid sampling method. Choose 'bootstrap' or 'random'.")
    }
    
    # Create contingency table
    cont_table <- table(sample_data[[effect_cat]], sample_data[[bin_var]])
    
    # Ensure all effect size categories are represented in the table
    cont_table <- cont_table[all_effect_cats, , drop = FALSE]
    cont_table[is.na(cont_table)] <- 0  # Replace NA with 0
    
    # Perform the chosen statistical test
    if (test_type == "Chi-Square Test") {
      test_result <- tryCatch(chisq.test(cont_table), error = function(e) NULL)
      if (!is.null(test_result)) {
        #print(test_result)
        p_val <- test_result$p.value
        test_stat <- test_result$statistic
      } else {
        p_val <- NA
        test_stat <- NA
      }
    } else if (test_type == "Fisher's Exact Test") {
      test_result <- tryCatch(fisher.test(cont_table), error = function(e) NULL)
      if (!is.null(test_result)) {
        #print(test_result)
        p_val <- test_result$p.value
        test_stat <- NA  # Fisher's Exact Test does not provide a Chi-Square statistic
      } else {
        p_val <- NA
        test_stat <- NA
      }
    }
    
    # Store results
    p_values[i] <- p_val
    test_stats[i] <- test_stat
  }
  
  # Summarize the results
  summary_results <- list(
    p_values = p_values,
    test_stats = test_stats,
    test_type = test_type,
    climate_type = climate_type
  )
  
  # Print summary
  cat("--------------------------\n")
  cat(paste("Climate Type:", climate_type, "| Agreement Variable:", agreement_var, "\n"))
  cat("----------------------------\n")
  
  #cat("\nStatistical Test Summary over", num_iterations, "iterations:\n")
  if (test_type == "Chi-Square Test") {
    cat("Chi-Square Tests performed:", num_iterations, "\n")
  } else {
    cat("Fisher's Exact Tests performed:", num_iterations, "\n")
  }
  
  cat("\nP-values Summary:\n")
  cat(sprintf("Mean p-value: %.4f\n", mean(p_values, na.rm = TRUE)))
  cat(sprintf("Median p-value: %.4f\n", median(p_values, na.rm = TRUE)))
  cat(sprintf("Proportion p < 0.05: %.2f%%\n", mean(p_values < 0.05, na.rm = TRUE) * 100))
  
  if (test_type == "Chi-Square Test") {
    cat("\nTest Statistics Summary (Chi-Square):\n")
    cat(sprintf("Mean Chi-Square: %.4f\n", mean(test_stats, na.rm = TRUE)))
    cat(sprintf("Median Chi-Square: %.4f\n", median(test_stats, na.rm = TRUE)))
  }
  
  return(summary_results)
}

summarize_bootstrap_results <- function(summary_results) {
  # Extract components
  p_values <- summary_results$p_values
  test_stats <- summary_results$test_stats
  test_type <- summary_results$test_type
  climate_type <- summary_results$climate_type
  
  # Initialize summary list
  summary_list <- list()
  
  # Include the type of test and climate type in the summary
  summary_list$test_type <- test_type
  summary_list$climate_type <- climate_type
  
  # Calculate Mean p-value
  mean_p <- mean(p_values, na.rm = TRUE)
  summary_list$mean_pval <- mean_p
  
  # Calculate 95% CI for p-values using t.test
  p_ci <- t.test(p_values, conf.level = 0.95)$conf.int
  summary_list$pval_CI_L <- p_ci[1]
  summary_list$pval_CI_U <- p_ci[2]
  
  # Calculate Proportion of p < 0.05
  prop_sig <- mean(p_values < 0.05, na.rm = TRUE) * 100  # in percentage
  summary_list$prop_sigif <- prop_sig
  
  # Handle Chi-Square or Fisher's Exact based on test type
  if (test_type == "Chi-Square Test") {
    # Remove NA values from test statistics
    valid_test_stats <- test_stats[!is.na(test_stats)]
    
    # Calculate Mean test statistic for Chi-Square Test
    mean_stat <- mean(valid_test_stats, na.rm = TRUE)
    summary_list$mean_test_stat <- mean_stat
    
    # Calculate 95% CI for test statistics using t.test
    stat_ci <- t.test(valid_test_stats, conf.level = 0.95)$conf.int
    summary_list$test_stat_CI_L <- stat_ci[1]
    summary_list$test_stat_CI_U <- stat_ci[2]
  } else {
    # For Fisher's Exact Test (or other tests without test statistics), skip this part
    summary_list$mean_test_stat <- NA
    summary_list$test_stat_CI_L <- NA
    summary_list$test_stat_CI_U <- NA
  }
  
  # Return the summary list
  return(summary_list)
}

# Apply the function to temperature agreement variables
all_effect_cats <- c("Negative effect (g < -0.2)", "Positive effect (g > 0.2)")

# 3. Run chi-square/fisher tests. -------

temp_results_2_boot <- perform_stat_tests_subset_bootstrap_single_test(
  data = dat_temp,
  effect_cat = "es_cat",
  agreement_var = "bio1_most_frequent_2",
  bin_var = "bio1_most_frequent_2",
  climate_type = "Temperature (2ºC change)",
  num_iterations = 1000,
  sample_size = 0.8,
  sampling_method = "bootstrap",  # or "random"
  seed = 123
)

temp_results_15_boot <- perform_stat_tests_subset_bootstrap_single_test(
  data = dat_temp,
  effect_cat = "es_cat",
  agreement_var = "bio1_most_frequent_15",
  bin_var = "bio1_most_frequent_15",
  climate_type = "Temperature (1.5ºC change)",
  num_iterations = 1000,
  sample_size = 0.8,
  sampling_method = "bootstrap",  # or "random"
  seed = 123
)
temp_results_1_boot <- perform_stat_tests_subset_bootstrap_single_test(
  data = dat_temp,
  effect_cat = "es_cat",
  agreement_var = "bio1_most_frequent_1",
  bin_var = "bio1_most_frequent_1",
  climate_type = "Temperature (1ºC change)",
  num_iterations = 1000,
  sample_size = 0.8,
  sampling_method = "bootstrap",  # or "random"
  seed = 123
)

temp_results_05_boot <- perform_stat_tests_subset_bootstrap_single_test(
  data = dat_temp,
  effect_cat = "es_cat",
  agreement_var = "bio1_most_frequent_05",
  bin_var = "bio1_most_frequent_05",
  climate_type = "Temperature (0.5ºC change)",
  num_iterations = 1000,
  sample_size = 0.8,
  sampling_method = "bootstrap",  # or "random"
  seed = 123
)

precip_results_100_boot <- perform_stat_tests_subset_bootstrap_single_test(
  data = dat_prec,
  effect_cat = "es_cat",
  agreement_var = "bio12_most_frequent_100cat",
  bin_var = "bio12_most_frequent_100cat",
  climate_type = "Precipitation (+- 100mm)",
  num_iterations = 1000,
  sample_size = 0.8,
  sampling_method = "bootstrap",  # or "random"
  seed = 123
)

precip_results_50_boot <- perform_stat_tests_subset_bootstrap_single_test(
  data = dat_prec,
  effect_cat = "es_cat",
  agreement_var = "bio12_most_frequent_50cat",
  bin_var = "bio12_most_frequent_50cat",
  climate_type = "Precipitation (+- 50mm)",
  num_iterations = 1000,
  sample_size = 0.8,
  sampling_method = "bootstrap",  # or "random"
  seed = 123
)

# Apply the function to precipitation agreement variables
precip_results_25_boot <- perform_stat_tests_subset_bootstrap_single_test(
  data = dat_prec,
  effect_cat = "es_cat",
  agreement_var = "bio12_most_frequent_25cat",
  bin_var = "bio12_most_frequent_25cat",
  climate_type = "Precipitation (+- 25mm)",
  num_iterations = 1000,
  sample_size = 0.8,
  sampling_method = "bootstrap",  # or "random"
  seed = 123
)

# 4. Summarise results.------------

# Make a list of results
results_list <- list(
  temp_05 = summarize_bootstrap_results(temp_results_05_boot),
  temp_1 = summarize_bootstrap_results(temp_results_1_boot),
  temp_15 = summarize_bootstrap_results(temp_results_15_boot),
  temp_2 = summarize_bootstrap_results(temp_results_2_boot),
  precip_25 = summarize_bootstrap_results(precip_results_25_boot),
  precip_50 = summarize_bootstrap_results(precip_results_50_boot),
  precip_100 = summarize_bootstrap_results(precip_results_100_boot)
)

# Convert each list into a data frame with an additional column indicating the name
dat_results <- do.call(rbind, lapply(names(results_list), function(name) {
  result <- results_list[[name]]
  data.frame(list_name = name, result, stringsAsFactors = FALSE)
}))

# Assuming you have your results data frame `dat_results`
dat_results <- dat_results %>%
  mutate(across(where(is.numeric), round, digits = 2))

dat_results$test_type[dat_results$test_type=="Chi-Square Test"] = "Chi-Square"
dat_results$test_type[dat_results$test_type=="Fisher's Exact Test"] = "Fisher's Exact"

# Edit column names
names(dat_results) = c("list_name", "TestType", "ClimVar", "MeanPVal", "PVal.CI.Lo", "PVal.CI.Up", "PropPValSignif", "MeanStat", "StatCI.Lo","StatCI.Up")
#write_csv(dat_results, here('outputs','tables_new','TableS8_climvars_hedge_tests.csv'))
