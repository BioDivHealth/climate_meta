# standard error calculation

# Function to calculate the standard error of Hedges' g
calculate_se_hedges_g <- function(g, N) {
  # Assume equal group sizes
  n1 <- N / 2
  n2 <- N / 2
  
  # Calculate the standard error using the formula
  term1 <- N / (n1 * n2)
  term2 <- (g^2) / (2 * (N - 3))
  
  se_g_squared <- term1 + term2
  se_g <- sqrt(se_g_squared)
  
  return(se_g)
}

#se_g <- calculate_se_hedges_g(g, N)
#print(paste("The standard error of Hedges' g is:", se_g))

df = read.csv('data/dataset_final.csv')
df$se_new = NA


df$se_new <- mapply(calculate_se_hedges_g, df$es, df$N.value)
View(df %>% select(es, se, se_new, N.value))
