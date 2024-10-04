

library(dplyr)

# Function to perform chi-square tests on sampled data
perform_chi_tests <- function(data, n_samples, percentage) {
  results <- data.frame(i = 1:n_samples)
  data$Direction = as.factor(data$Direction)
  for (i in 1:n_samples) {
    #sample % of data
    smp <- data[sample(1:nrow(data), size = round(nrow(data) * percentage), replace = TRUE),]
    #smp$Direction = as.factor(smp$Direction)
    #perform chi-tests
    #print(table(smp$Direction))
    chi_test_all <- chisq.test(xtabs(data = smp, Dummy ~ Direction))

    if(length(unique(data$Environmental_condition))==3){
    #extract statistics and p-values
    results$chi_all[i] <- chi_test_all$statistic
    results$p_all[i] <- chi_test_all$p.value
    } else {
    if(unique(data$Environmental_condition)=="Humidity"){
      results$chi_all[i] <- chi_test_all$statistic
      results$p_all[i] <- chi_test_all$p.value
    }
    if(unique(data$Environmental_condition)=="Temperature"){
      results$chi_all[i] <- chi_test_all$statistic
      results$p_all[i] <- chi_test_all$p.value
    }
  if(unique(data$Environmental_condition)=="Precipitation"){
    results$chi_all[i] <- chi_test_all$statistic
    results$p_all[i] <- chi_test_all$p.value
  }
 }
}
  
  return(results)
}

# Function to calculate summary statistics (P-values)
calculate_means <- function(results) {
  means <- c(
    p_all = mean(results$p_all, na.rm = TRUE),
    chi_all = mean(results$chi_all, na.rm = TRUE),
    chi_ci_lower_all = t.test(results$chi_all, conf.level = 0.95)$conf.int[1], #Chi Ci Lower
    chi_ci_upper_all = t.test(results$chi_all, conf.level = 0.95)$conf.int[2] #Chi Ci Upper
  )
  return(means)
}

# Main analysis function (removing 1 group from the 3 biggest groups at a time)
chisq_remove_groups <- function(df, grouping_factor, percentage = percentage, n_samples = 1000) {
  
  # none_removed doesnt remove any groups and performs analysis for full dataset
  # and 3 climatic subsets
  if(grouping_factor!="none_removed"){
    group_summary <- df %>%
      group_by(across(all_of(grouping_factor))) %>%
      summarise(N = n_distinct(Reference_ID)) %>%
      mutate(P = N / sum(N)) %>%
      arrange(desc(N))
    print(group_summary)[1:3,1]
    #identify top 3 groups per count
    top_groups <- as.vector(unlist(group_summary[1:3, 1]))
    top_groups = top_groups[!is.na(top_groups)]}
  
  means_df <- list()
  
  if(grouping_factor!="none_removed"){
    for (group_rm in top_groups) {
      tmp <- df %>%
        filter(Direction != "") %>%
        filter(P.value_general != "") %>%
        filter(P.value_general != ">0.05") %>%
        filter(across(all_of(grouping_factor)) != group_rm)
      #check if all env. categories have mroe than 10 records
      if (all(table(tmp$Environmental_condition) > 10)) {
        results <- perform_chi_tests(tmp, n_samples, percentage)
        means <- calculate_means(results)
        means_df[[group_rm]] <- means
      }
    }
  } else {
    tmp <- df %>%
      filter(Direction != "") %>%
      filter(P.value_general != "") %>%
      filter(P.value_general != ">0.05")
    
    if (all(table(tmp$Environmental_condition) >= 10)) {
      results <- perform_chi_tests(tmp, n_samples, percentage)
      means <- calculate_means(results)
      means_df[[grouping_factor]] <- means
    }
  }
  
  if(grouping_factor!="none_removed"){
    means_chi = lapply(means_df, function(sublist) { sublist[grep("chi", names(sublist))]})
    means_pval = lapply(means_df, function(sublist) { sublist[!grepl("chi", names(sublist))]})} 
  else {
    means_chi = list()
    means_chi[[grouping_factor]] = means_df[[grouping_factor]][grep("chi",names(means_df[[grouping_factor]]))]
    means_pval = list()
    means_pval[[grouping_factor]] = means_df[[grouping_factor]][!grepl("chi",names(means_df[[grouping_factor]]))]
  }
  
  #Transform P-values to simplified significance levels
  levels_means_pval <- as.data.frame(lapply(means_pval, function(x) {
    ifelse(x < 0.01, "**", ifelse(x < 0.05, "*", ">0.05"))
  }))
  means_pval = as.data.frame(means_pval)
  rownames(levels_means_pval) <- rownames(means_pval)
  means_chi = as.data.frame(means_chi)
  
  return(list(means_pval = means_pval, levels_means_pval = levels_means_pval,
              means_chi = means_chi))
  
}