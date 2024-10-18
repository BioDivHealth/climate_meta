# Anderson-Darling and Kolmogorov-Smirnov Tests
# Code to be sourced into results_text.Rmd file
# 15 July 2024

# Load data 
df = read.csv(here("data","dataset_final.csv"))
df = unique(df)
set.seed(123)

# Change vector names
df = df%>%
  mutate(Transmission_type = ifelse(Transmission_type == 'HVH', 'Vectored', 'Non-vectored'))

# Change parasite names 

# add parasite group
df = df%>%
  mutate(Pathogen = ifelse(Pathogen == 'P', 'Parasite',
                           ifelse(Pathogen == 'V', 'Virus',
                                  'Bacteria')))

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
                  Principal_reservoir = c('Rodents', 'Mammals (multispecies)', 'Livestock', 'Birds'), 
                  Pathogen = c('Virus', 'Bacteria'), 
                  vector = c('Mosquito', 'Tick', 'Mite'),
                  Country = c('China','Iran','USA','Argentina','India'), #added temporarily
                  Disease = c('Haemorrhagic fever with renal syndrome', 'Brucellosis', 'Scrub typhus',
                              'Leptospirosis')
)

#AD Test: without subset removal ----
  

# create named list for results
pvals  = vector("list")
test_stats = vector('list')

for(condition in unique(df$Environmental_condition)){
  # separate out climate factor
  temp = df%>%
    subset(Linear_Nonlinear == 'Linear')%>%
    subset(Environmental_condition == condition)
  
  for(category in names(categories)){
    for(group in categories[[category]]){
      # separate out category  
      group_df = temp%>%
        filter(.data[[category]] == group)
      
      ad = perm_comp_test(temp$es, group_df$es)
      
      pvals[[paste(condition, category, group, sep='.')]] = ad$p.value
      test_stats[[paste(condition, category, group, sep='.')]] = ad$statistic
    }
  }
}


#**Create Dataframe**
  
results_df = data.frame(id = names(pvals),
                        p_val = unlist(pvals),
                        stat = unlist(test_stats))

#AD Test with subset removal----
  
# create named list for results
pvals  = vector("list")
test_stats = vector('list')

for(condition in unique(df$Environmental_condition)){
  # separate out climate factor
  temp = df%>%
    subset(Linear_Nonlinear == 'Linear')%>%
    subset(Environmental_condition == condition)
  
  for(category in names(categories)){
    for(group in categories[[category]]){
      # separate out category  
      group_df = temp%>%
        filter(.data[[category]] == group)
      
      # filter out group for random sample
      temp_filtered = temp%>%
        filter(.data[[category]] != group)
      
      ad = perm_comp_test(temp_filtered$es, group_df$es)
      
      pvals[[paste(condition, category, group, sep='.')]] = ad$p.value
      test_stats[[paste(condition, category, group, sep='.')]] = ad$statistic
      
    }
  }
}


#**Combine and save dataframes**
  
results_df2 = data.frame(id = names(pvals),
                         p_val_removed = unlist(pvals),
                         stat_removed = unlist(test_stats))

results_df = left_join(results_df, results_df2)

results_df = results_df%>%
  separate_wider_delim(cols = id,
                       delim = ".",
                       names = c("Environmental_condition", "Category", "Group"))

rownames(results_df) = NULL
#write.csv(results_df, here('outputs','tables','TableS456_ADTests.csv'), row.names = F)

results_df 

# Comparison of Vector and Non-vector borne disease ----

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

# Print the dataframe
print(ks_results)
#write.csv(ks_results, here('outputs', 'tables', 'KS_Tests.csv'), row.names = FALSE)
