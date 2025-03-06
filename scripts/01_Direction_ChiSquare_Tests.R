# Chi-square Analysis: Climate Sensitivity Direction Reported & Groups Removed
# Author: Artur Trebski

# ----------------------------------------------------------------|
# Section 1: Load Necessary Libraries and Data ----
# ----------------------------------------------------------------|

# Load required libraries
library(dplyr)
library(tidyr)
library(stringr)
library(tidyverse)

# Load files

df = read.csv(here::here("data","dataset_final.csv"))
df = unique(df) #ensure all rows are unique

# Change vector names
df = df%>%
  mutate(Transmission_type = ifelse(Transmission_type == 'HVH', 'Vectored',
                                    'Non-vectored'))

# Add parasite group
df = df%>%
  mutate(Pathogen = ifelse(Pathogen == 'P', 'Parasite',
                           ifelse(Pathogen == 'V', 'Virus',
                                  'Bacteria')))

# ----------------------------------------------------------------|
# Section 2: Define Helper Functions ----
# ----------------------------------------------------------------|

## 2.1 Function to Perform Chi-square Tests on Sampled Data
perform_chi_tests <- function(data, n_samples, percentage) {
  # Initialize results data frame
  results <- data.frame(i = 1:n_samples)
  
  # Ensure 'Direction' is a factor
  data$Direction <- as.factor(data$Direction)
  
  # Loop through each sample
  for (i in 1:n_samples) {
    # Sample a percentage of the data with replacement
    smp <- data %>%
      sample_frac(size = percentage, replace = TRUE)
    
    # Perform chi-square test
    chi_test <- chisq.test(xtabs(Dummy ~ Direction, data = smp))
    
    # Extract statistics based on Environmental_condition
    env_conditions <- unique(data$Environmental_condition)
    
    if (length(env_conditions) == 3 ||
        env_conditions %in% c("Humidity", "Temperature", "Precipitation")) {
      results$chi_all[i] <- chi_test$statistic
      results$p_all[i] <- chi_test$p.value 
    }
  }
  
  return(results)
}

## 2.2 Function to Calculate Summary Statistics (P-values and Chi-square)
calculate_means <- function(results) {
  # Calculate mean P-values and Chi-square statistics
  means <- c(
    p_all = mean(results$p_all, na.rm = TRUE),
    chi_all = mean(results$chi_all, na.rm = TRUE),
    chi_ci_lower_all = t.test(results$chi_all, conf.level = 0.95)$conf.int[1],
    chi_ci_upper_all = t.test(results$chi_all, conf.level = 0.95)$conf.int[2]
  )
  return(means)
}

## 2.3 Main Analysis Function: Remove Top Groups and Perform Chi-square Tests
chisq_remove_groups <- function(df, grouping_factor, percentage = 0.8, n_samples = 1000) {
  
  # Filter the dataset for valid entries
  df_filtered <- df %>%
    filter(Direction != "") %>%
    filter(P.value_general != "") %>%
    filter(P.value_general != ">0.05")
  
  # Initialize list to store means
  means_df <- list()
  
  # Check if any groups should be removed
  if (grouping_factor != "none_removed") {
    # Summarize groups and identify top 3
    group_summary <- df_filtered %>%
      group_by(across(all_of(grouping_factor))) %>%
      summarise(N = n_distinct(Reference_ID), .groups = 'drop') %>%
      mutate(P = N / sum(N)) %>%
      arrange(desc(N))
    
    print(group_summary[1:3, 1])
    print(group_summary[1:3, ])
    # Extract top 3 groups
    top_groups <- group_summary %>%
      slice_head(n = 3) %>%
      pull(!!sym(grouping_factor)) %>%
      na.omit()
    
    # Iterate over each top group to remove and perform tests
    for (group_rm in top_groups) {
      tmp <- df_filtered %>%
        filter(across(all_of(grouping_factor)) != group_rm)
      
      # Ensure each Environmental_condition has more than 10 records
      if (all(table(tmp$Environmental_condition) > 10)) {
        results <- perform_chi_tests(tmp, n_samples, percentage)
        means <- calculate_means(results)
        means_df[[as.character(group_rm)]] <- means
      }
    }
  } else {
    # Perform tests without removing any groups
    if (all(table(df_filtered$Environmental_condition) >= 10)) {
      results <- perform_chi_tests(df_filtered, n_samples, percentage)
      means <- calculate_means(results)
      means_df[["none_removed"]] <- means
    }
  }
  
  # Separate Chi-square statistics and P-values
  if (grouping_factor != "none_removed") {
    means_chi <- lapply(means_df, function(x) x[grep("chi", names(x))])
    means_pval <- lapply(means_df, function(x) x[!grepl("chi", names(x))])
  } else {
    means_chi <- list()
    means_chi[[grouping_factor]] <- means_df[[grouping_factor]][grep("chi", names(means_df[[grouping_factor]]))]
    
    means_pval <- list()
    means_pval[[grouping_factor]] <- means_df[[grouping_factor]][!grepl("chi", names(means_df[[grouping_factor]]))]
  }
  
  # Transform P-values to significance levels
  levels_means_pval <- as.data.frame(lapply(means_pval, function(x) {
    ifelse(x < 0.01, "**",
           ifelse(x < 0.05, "*", ">0.05"))
  }))
  
  means_pval <- as.data.frame(means_pval)
  rownames(levels_means_pval) <- rownames(means_pval)
  means_chi <- as.data.frame(means_chi)
  
  return(list(
    means_pval = means_pval,
    levels_means_pval = levels_means_pval,
    means_chi = means_chi
  ))
}

# ----------------------------------------------------------------|
# Section 3: Main Analysis Workflow ----
# ----------------------------------------------------------------|

# 3.1 Set Analysis Parameters
percentage <- 0.8  # Percentage of data to sample
n_samples <- 1000   # Number of samples for chi-square tests
set.seed(123)       # Set seed for reproducibility

# 3.2 Initialize List to Store Results
list_of_dfs <- list()

# 3.3 Iterate Over Each Environmental Condition
for (env_condition in c("All", "Temperature", "Precipitation", "Humidity")) {
  
  # 3.3.1 Subset the Data Based on Environmental Condition
  if (env_condition != "All") {
    df_subset <- df %>%
      filter(Environmental_condition == env_condition)
  } else {
    df_subset <- df
  }
  
  # 3.3.2 Perform Chi-square Tests Without Removing Any Groups
  none_removed <- chisq_remove_groups(
    df = df_subset,
    grouping_factor = "none_removed",
    percentage = percentage,
    n_samples = n_samples
  )
  
  # 3.3.3 Perform Chi-square Tests by Removing Specific Grouping Factors
  disease_removed <- chisq_remove_groups(
    df = df_subset,
    grouping_factor = "General_Disease",
    percentage = percentage,
    n_samples = n_samples
  )
  
  country_removed <- chisq_remove_groups(
    df = df_subset,
    grouping_factor = "Country",
    percentage = percentage,
    n_samples = n_samples
  )
  
  princ_reservoir_removed <- chisq_remove_groups(
    df = df_subset,
    grouping_factor = "Principal_reservoir",
    percentage = percentage,
    n_samples = n_samples
  )
  
  stat_method_removed <- chisq_remove_groups(
    df = df_subset,
    grouping_factor = "Statistical_method",
    percentage = percentage,
    n_samples = n_samples
  )
  
  pathogen_group_removed <- chisq_remove_groups(
    df = df_subset,
    grouping_factor = "Pathogen",
    percentage = percentage,
    n_samples = n_samples
  )
  
  transmission_type_removed <- chisq_remove_groups(
    df = df_subset,
    grouping_factor = "Transmission_type",
    percentage = percentage,
    n_samples = n_samples
  )
  
  # 3.3.4 Combine P-values, Significance Levels, and Chi-square Statistics
  combined_pvals <- bind_cols(
    none_removed$means_pval,
    disease_removed$means_pval,
    country_removed$means_pval,
    princ_reservoir_removed$means_pval,
    pathogen_group_removed$means_pval,
    stat_method_removed$means_pval,
    transmission_type_removed$means_pval
  )
  
  combined_levels <- bind_cols(
    none_removed$levels_means_pval,
    disease_removed$levels_means_pval,
    country_removed$levels_means_pval,
    princ_reservoir_removed$levels_means_pval,
    pathogen_group_removed$levels_means_pval,
    stat_method_removed$levels_means_pval,
    transmission_type_removed$levels_means_pval
  )
  
  combined_chi <- bind_cols(
    none_removed$means_chi,
    disease_removed$means_chi,
    country_removed$means_chi,
    princ_reservoir_removed$means_chi,
    pathogen_group_removed$means_chi,
    stat_method_removed$means_chi,
    transmission_type_removed$means_chi
  )
  
  # 3.3.5 Transform Chi-square Statistics to Long Format
  chi_transformed <- combined_chi %>%
    rownames_to_column(var = "Metric") %>%
    pivot_longer(cols = -Metric, names_to = "Group", values_to = "Value")
  
  chi_wide <- chi_transformed %>%
    pivot_wider(names_from = Metric, values_from = Value)
  
  # 3.3.6 Transform P-values to Long Format
  pval_transformed <- combined_pvals %>%
    rownames_to_column(var = "Metric") %>%
    pivot_longer(cols = -Metric, names_to = "Group", values_to = "Value")
  
  pvals_wide <- pval_transformed %>%
    pivot_wider(names_from = Metric, values_from = Value)
  
  # 3.3.7 Transform P-value Levels to Long Format
  pval_levels_transformed <- combined_levels %>%
    rownames_to_column(var = "Metric") %>%
    pivot_longer(cols = -Metric, names_to = "Group", values_to = "Value")
  
  pvals_levels_wide <- pval_levels_transformed %>%
    pivot_wider(names_from = Metric, values_from = Value)
  
  names(pvals_levels_wide) = c("Group","P_level")
  
  # 3.3.8 Join All Results into a Single Data Frame
  full_results <- chi_wide %>%
    left_join(pvals_wide, by = "Group") %>%
    left_join(pvals_levels_wide, by = "Group")
  
  # 3.3.9 Rename Columns for Clarity
  rename_columns <- function(df) {
    df %>%
      rename_with(~ str_replace(., "chi_all", "Chi_Square")) %>%
      rename_with(~ str_replace(., "chi_ci_lower_.*", "Chi_Lower_CI")) %>%
      rename_with(~ str_replace(., "chi_ci_upper_.*", "Chi_Upper_CI")) %>%
      rename_with(~ str_replace(., "p_all", "P_value"))
  }
  
  full_results <- rename_columns(full_results)
  
  # 3.3.10 Assign the Results to Corresponding Environmental Condition
  if (env_condition == "All") {
    df_all_env <- full_results
  } else if (env_condition == "Temperature") {
    df_temp <- full_results
  } else if (env_condition == "Humidity") {
    df_hum <- full_results
  } else if (env_condition == "Precipitation") {
    df_prec <- full_results
  }
}

# ----------------------------------------------------------------|
# Section 4: Compile and Export Results ----
# ----------------------------------------------------------------|

# 4.1 Compile All Results into a List
list_of_dfs <- list(
  Overall = df_all_env,
  Humidity = df_hum,
  Temperature = df_temp,
  Precipitation = df_prec
)

# 4.2 Display the List of Data Frames
print(list_of_dfs)

# 4.3 Export Overall Results to CSV
# write.csv(
#   as.data.frame(list_of_dfs[["Overall"]]),
#   'outputs/tables/TableS3_ChiSquare.csv',
#   row.names = FALSE
# )

# 4.4 Export Additional Environmental Condition Results (Uncomment as Needed)
# write.csv(as.data.frame(list_of_dfs[["Temperature"]]), 'outputs/tables/chisq_results_temperature2.csv', row.names = FALSE)
# write.csv(as.data.frame(list_of_dfs[["Humidity"]]), 'outputs/tables/chisq_results_humidity2.csv', row.names = FALSE)
# write.csv(as.data.frame(list_of_dfs[["Precipitation"]]), 'outputs/tables/chisq_results_precipitation2.csv', row.names = FALSE)
