# Anderson-Darling and Kolmogorov-Smirnov Tests
# Code to be sourced into results_text.Rmd file
# 15 July 2024

# Load data 
library(dplyr)
library(here)
library(magrittr)

df = read.csv(here::here("data","dataset_final.csv"))
df = unique(df)
set.seed(123)

# Change vector names
df = df%>%
  mutate(Transmission_type = ifelse(Transmission_type == 'HVH', 'Vectored', 'Non-vectored'))

# add parasite group
df = df%>%
  mutate(Pathogen = ifelse(Pathogen == 'P', 'Parasite',
                           ifelse(Pathogen == 'V', 'Virus',
                                  'Bacteria')))

df %<>% filter(!is.na(es))

# Determining how category distributions differ from full dataset ----
### Defining statistical test 
ad2_stat <- function(x, y) {
  
  # Sample sizes
  n <- length(x)
  
  m <- length(y)
  
  # Pooled sample and pooled ecdf
  z <- c(x, y)
  z <- z[-which.max(z)] # Exclude the largest point
  H <- rank(z) / (n + m)
  
  # Statistic computation via ecdf()
  (n * m / (n + m)^2) * sum((ecdf(x)(z) - ecdf(y)(z))^2 / ((1 - H) * H))
  
}

# A homogeneity test using the Anderson-Darling statistic
perm_comp_test <- function(x, y, B = 1e3) {
  
  # Sizes of x and y
  n <- length(x)
  m <- length(y)
  
  # Test statistic function. Requires TWO arguments, one being the original
  # data (X_1, ..., X_n, Y_1, ..., Y_m) and the other containing the random
  # index for permuting the sample
  Tn <- function(data, perm_index) {
    
    # Permute sample by perm_index
    data <- data[perm_index]
    
    # Split into two samples
    x <- data[1:n]
    y <- data[(n + 1):(n + m)]
    
    # Test statistic -- MODIFY DEPENDING ON THE PROBLEM
    ad2_stat(x = x, y = y)
    
  }
  
  # Perform permutation resampling with the aid of boot::boot
  Tn_star <- boot::boot(data = c(x, y), statistic = Tn,
                        sim = "permutation", R = B)
  
  # Test information -- MODIFY DEPENDING ON THE PROBLEM
  method <- "Permutation-based Anderson-Darling test of homogeneity"
  alternative <- "any alternative to homogeneity"
  
  # p-value: modify if rejection does not happen for large values of the
  # test statistic
  pvalue <- mean(Tn_star$t > Tn_star$t0)
  
  # Construct an "htest" result
  result <- list(statistic = c("stat" = Tn_star$t0), p.value = pvalue,
                 statistic_perm = drop(Tn_star$t),
                 B = B, alternative = alternative, method = method,
                 data.name = deparse(substitute(x)))
  class(result) <- "htest"
  
  # Return "htest"
  return(result)
  
}

#Defining categories and parameters to use in procedure.
categories = list(Transmission_type = c('Vectored', 'Non-vectored'), 
                  Principal_reservoir = c('Rodents', 'Mammals (multispecies)', 
                                          'Livestock', 'Birds'), 
                  Pathogen = c('Virus', 'Bacteria'), 
                  vector = c('Mosquito', 'Tick', 'Mite'),
                  Country = c('China','Iran','USA','Argentina','India'),
                  Disease = c('Haemorrhagic fever with renal syndrome', 
                              'Brucellosis', 'Scrub typhus',
                              'Leptospirosis', 'Japanese encephalitis'))


### AD Test: WITHOUT SUBSET REMOVAL ----------------------------------------------
# Create named lists for results

pvals  = vector("list")
test_stats = vector("list")
studies_in_a_group = vector("list")
effects_in_a_group = vector("list")
studies_in_environmental_condition = vector("list")
effects_in_environmental_condition = vector("list")

for(condition in unique(df$Environmental_condition)){
  # Separate out climate factor
  temp = df %>%
    subset(Linear_Nonlinear == 'Linear') %>%
    subset(Environmental_condition == condition)
  
  for(category in names(categories)){
    for(group in categories[[category]]){
      # Separate out category  
      group_df = temp %>%
        filter(.data[[category]] == group)
      
      ad = perm_comp_test(temp$es, group_df$es)
      
      key = paste(condition, category, group, sep='.')
      pvals[[key]] = ad$p.value
      test_stats[[key]] = ad$statistic
      
      studies_in_a_group[[key]] = length(unique(group_df$Reference_ID))
      effects_in_a_group[[key]] = length(group_df$es)
      
      studies_in_environmental_condition[[key]] = length(unique(temp$Reference_ID))
      effects_in_environmental_condition[[key]] = length(temp$es)
    }
  }
}

# Create Dataframe for AD tests WITHOUT subset removal
results_df = data.frame(id = names(pvals),
                        p_val = unlist(pvals),
                        stat = unlist(test_stats),
                        studies_in_a_group = unlist(studies_in_a_group),
                        effects_in_a_group = unlist(effects_in_a_group),
                        studies_in_environmental_condition = unlist(studies_in_environmental_condition),
                        effects_in_environmental_condition = unlist(effects_in_environmental_condition))

### AD Test: WITH SUBSET REMOVAL ----------------------------------------------


# Create named lists for results with subset removal
pvals_removed  = vector("list")
test_stats_removed = vector("list")


for(condition in unique(df$Environmental_condition)){
  # Separate out climate factor
  temp = df %>%
    subset(Linear_Nonlinear == 'Linear') %>%
    subset(Environmental_condition == condition)
  
  for(category in names(categories)){
    for(group in categories[[category]]){
      # Separate out category  
      group_df = temp %>%
        filter(.data[[category]] == group)
      
      # Filter out group for random sample  
      temp_filtered = temp %>%
        filter(.data[[category]] != group)
      
      ad = perm_comp_test(temp_filtered$es, group_df$es)
      
      key = paste(condition, category, group, sep='.')
      pvals_removed[[key]] = ad$p.value
      test_stats_removed[[key]] = ad$statistic
      
      # Save confidence intervals for subset removal
    }
  }
}

# Create Dataframe for AD tests WITH subset removal
results_df2 = data.frame(id = names(pvals_removed),
                         p_val_removed = unlist(pvals_removed),
                         stat_removed = unlist(test_stats_removed))

# Merge the two results dataframes
results_df = left_join(results_df, results_df2, by = "id")

# Split the id column into separate columns for Environmental_condition, Category, and Group
results_df = results_df %>%
  tidyr::separate(id, into = c("Environmental_condition", "Category", "Group"), sep = "\\.")

rownames(results_df) = NULL
#write.csv(results_df, here('ignore','outputs','tables','TableS467_ADTests_SampleSizes_withCI.csv'), row.names = F)

results_df

# -----------------------------------------------------------------------------------|
# Correction for multiple testing - Bonferroni - 1 correction per climate factor -----
# -----------------------------------------------------------------------------------|
# **Filtering by Environmental Condition**
results_df %<>% filter(effects_in_a_group >= 20)

# Split by environmental condition and apply corrections
env_conditions <- c("Temperature", "Humidity", "Precipitation")

results_list <- lapply(env_conditions, function(condition) {
  subset_df <- results_df %>% filter(Environmental_condition == condition)
  
  # Apply BH corrections for both p-value sets
  subset_df$p_val_corrected <- p.adjust(subset_df$p_val, method = "BH")
  subset_df$p_val_removed_corrected <- p.adjust(subset_df$p_val_removed, method = "BH")
  
  # Add significance levels
  subset_df <- subset_df %>%
    mutate(
      Signif_level_corrected = case_when(
        p_val_corrected < 0.001 ~ '***',
        p_val_corrected < 0.01 ~ '**',
        p_val_corrected < 0.05 ~ '*',
        TRUE ~ ''
      ),
      Signif_level_corrected_removed = case_when(
        p_val_removed_corrected < 0.001 ~ '***',
        p_val_removed_corrected < 0.01 ~ '**',
        p_val_removed_corrected < 0.05 ~ '*',
        TRUE ~ ''
      )
    )
  
  return(subset_df)
})

# Combine all results
results_df_corrected <- bind_rows(results_list)

# Reorder columns
results_df_corrected %<>% relocate(
  p_val_corrected, Signif_level_corrected, 
  stat, p_val_removed, p_val_removed_corrected, 
  Signif_level_corrected_removed, stat_removed, 
  .after = p_val
)

# write.csv(results_df_corrected, here('ignore','outputs','tables','TableS467_ADTests_MultipleTestingCorrected.csv'), row.names = F)

# -----------------------------------------------------------------------------------|
# KS Tests: Comparison of Vector and Non-vector borne disease ------------------

es_summaries = df %>%
  subset(Linear_Nonlinear == 'Linear')%>%
  group_by(Transmission_type, Environmental_condition)%>%
  summarize(mn = mean(es), sd = sd(es), n=n())

#**Testing vectored vs non-vectored**

# create named list for results
pvals_ks  = vector("list")
test_stats_ks = vector('list')

for(condition in unique(df$Environmental_condition)){
  # separate out climate factor
  vec = df%>%
    subset(Linear_Nonlinear == 'Linear')%>%
    subset(Environmental_condition == condition)%>%
    subset(Transmission_type == 'Vectored')
  
  nonvec = df%>%
    subset(Linear_Nonlinear == 'Linear')%>%
    subset(Environmental_condition == condition)%>%
    subset(Transmission_type == 'Non-vectored')
  
  
  #ad = ad_test(vec$es, nonvec$es, nboots=10000)
  ks = ks.test(nonvec$es, vec$es, alternative = 'greater')
  pvals_ks[[condition]] = ks$p.value
  test_stats_ks[[condition]] = ks$statistic
}

# Combine the lists into a dataframe
ks_results <- data.frame(
  Variable = names(pvals_ks),
  P_Value = unlist(pvals_ks),
  Test_Statistic = unlist(test_stats_ks)
)

# Merge the data based on environmental condition
merged_data <- es_summaries %>%
  left_join(ks_results, by = c("Environmental_condition" = "Variable"))

# Relocate env. condition as first column
merged_data %<>% relocate(Environmental_condition, .before = Transmission_type)

#write.csv(merged_data, here('ignore','outputs','tables','TableS5_KS_Tests_Full.csv'), row.names = F)
#write.csv(ks_results, here('outputs', 'tables', 'TableS5_KS_Tests.csv'), row.names = FALSE)
