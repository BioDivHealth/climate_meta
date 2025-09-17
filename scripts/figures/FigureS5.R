#Dependencies

library(patchwork)
library(cowplot)
library(tidyverse)
library(magrittr)
library(here)

df = read.csv(here::here("data","dataset_final_g.csv"))
df = unique(df) #ensure all rows are unique
df %<>% filter(!is.na(es) & !is.na(Value))

# 1. Individual plots ----
## Odds ratio -----------------------------------------
OR_comp = df %>% filter(Type=="Odds ratio")
OR_comp_scatter <- ggplot(data = OR_comp, aes(x = Value, y = es)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "#54D8B1FF") +
  geom_point(aes(color = "Observation", shape = "Observation"), size = 3, alpha = 0.5) + 
  xlab("Odds Ratio") +
  ylab("Effect size (g)") +
  theme_classic()

# Assess outliers using residuals
OR_comp$es <- unlist(OR_comp$es)
model <- lm(es ~ Value, data = OR_comp)
residuals <- model$residuals
outliers <- OR_comp[abs(residuals) > 2 * sd(residuals), ]

# Add outliers to scatter plot
OR_comp_scatter <- OR_comp_scatter + 
  geom_point(data = outliers, 
             aes(x = Value, y = es, color = "Outlier", shape = "Outlier"), 
             size = 3.5) +  # Red dot for outliers
  geom_text(data = outliers, 
            aes(x = Value, y = es, label = Data_ID), 
            position = position_jitter(width = 0.6, height = 0.2), 
            alpha = 0.9, 
            size = 3.5) +  # Add labels only for outliers
  scale_color_manual(name = "", values = c("Observation" = "#175149FF", "Outlier" = "#E54E21FF")) +
  scale_shape_manual(name = "", values = c("Observation" = 16, "Outlier" = 16)) +
  
  # Increase font sizes for legend and axis labels
  theme(legend.position = "right",
        legend.text = element_text(size = 12),  # Increase legend font size
        axis.text.x = element_text(size = 12),  # Increase x-axis text size
        axis.text.y = element_text(size = 12),  # Increase y-axis text size
        axis.title.x = element_text(size = 14), # Increase x-axis title size
        axis.title.y = element_text(size = 14)) # Increase y-axis title size


## Beta coefficient standardised -----------------------------------------
Beta_comp = df %>% filter(Type=="Beta coefficient")
Beta_comp_scatter <- ggplot(data = Beta_comp, aes(x = Value, y = es)) +
    geom_line() +  # Add line
    geom_smooth(method = "lm", se = FALSE, color = "#54D8B1FF") + 
    geom_point(aes(color = "Observation", shape = "Observation"), size = 3, alpha = 0.5) +  # Add main data points
    #xlab("Standardised beta coefficient (β)") +
    labs(x = expression("Standardised  "*beta*" coefficient"))+
    ylab("Effect size (g)") +
    theme_classic()
  
  # Assess outliers using residuals
  model <- lm(es ~ Value, data = Beta_comp)
  residuals <- model$residuals
  outliers <- Beta_comp[abs(residuals) > 2 * sd(residuals), ]
  
  # Add outliers to scatter plot
  Beta_comp_scatter <- Beta_comp_scatter + 
    geom_point(data = outliers, 
               aes(x = Value, y = es, color = "Outlier", shape = "Outlier"), 
               size = 3.5) +  # Add slightly larger points for outliers
    geom_text(data = outliers, 
              aes(x = Value, y = es, label = Data_ID), 
              position = position_jitter(width = 0.6, height = 0.2), 
              alpha = 0.9, 
              size = 3.5) +  # Add text labels only for outliers
    scale_color_manual(name = "", 
                       values = c("Observation" = "#175149FF", "Outlier" = "#E54E21FF")) +  # Custom colors
    scale_shape_manual(name = "", 
                       values = c("Observation" = 16, "Outlier" = 16)) +  # Custom shapes
    theme(legend.position = "right",
          legend.text = element_text(size = 12),  # Increase legend text size
          axis.text.x = element_text(size = 12),  # Increase x-axis text size
          axis.text.y = element_text(size = 12),  # Increase y-axis text size
          axis.title.x = element_text(size = 14), # Increase x-axis title size
          axis.title.y = element_text(size = 14)) # Increase y-axis title size
  
  # Display scatter plot
  print(Beta_comp_scatter)

## Unstandardized Correlation coefficient -----------------------------------------
  correlation_methods <- c("Cross-correlation analysis", "Simple correlation")
  
  B_comp = df %>%
    filter(Type %in% c("Unstandardised coefficient", "Correlation coefficient"),
           !Statistical_method %in% correlation_methods) %>% 
    filter(!grepl("Spearman", ifelse(is.na(Statistical_method), "", Statistical_method), ignore.case = TRUE),
           !grepl("Pearson",  ifelse(is.na(Statistical_method), "", Statistical_method), ignore.case = TRUE))
    
  # Scatter plot showing the relationship between correlation coefficient and effect size
  B_comp_scatter <- ggplot(data = B_comp, aes(x = Value, y = es)) +
    geom_line() +  # Add line
    geom_smooth(method = "lm", se = FALSE, color = "#54D8B1FF") +  # Add smooth line with custom color
    geom_point(aes(color = "Observation", shape = "Observation"), size = 3, alpha = 0.5) +  # Add main data points
    xlab("Unstandardised B coefficient") +
    ylab("Effect size (g)") +
    theme_classic()
  
  # Assess outliers using residuals
  model <- lm(es ~ Value, data = B_comp)
  residuals <- model$residuals
  outliers <- B_comp[abs(residuals) > 2 * sd(residuals), ]
  
  # Add outliers to scatter plot
  B_comp_scatter <- B_comp_scatter + 
    geom_point(data = outliers, 
               aes(x = Value, y = es, color = "Outlier", shape = "Outlier"), 
               size = 3.5) +  # Add slightly larger points for outliers
    geom_text(data = outliers, 
              aes(x = Value, y = es, label = Data_ID), 
              position = position_jitter(width = 0.3, height = 0.2), 
              alpha = 0.9, 
              size = 3.5) +  # Add text labels only for outliers
    scale_color_manual(name = "", 
                       values = c("Observation" = "#175149FF", "Outlier" = "#E54E21FF")) +  # Custom colors
    scale_shape_manual(name = "", 
                       values = c("Observation" = 16, "Outlier" = 16)) +  # Custom shapes
    theme(legend.position = "right",
          legend.text = element_text(size = 12),  # Increase legend text size
          axis.text.x = element_text(size = 12),  # Increase x-axis text size
          axis.text.y = element_text(size = 12),  # Increase y-axis text size
          axis.title.x = element_text(size = 14), # Increase x-axis title size
          axis.title.y = element_text(size = 14)) # Increase y-axis title size
  
  # Display scatter plot
  print(B_comp_scatter)
  

  
B_corr = df %>%
    filter(Type == "Correlation coefficient",
           Statistical_method %in% correlation_methods) %>% 
    filter(!grepl("Spearman", ifelse(is.na(Statistical_method), "", Statistical_method), ignore.case = TRUE),
         !grepl("Pearson",  ifelse(is.na(Statistical_method), "", Statistical_method), ignore.case = TRUE))
  
# Scatter plot showing the relationship between correlation coefficient and effect size
B_corr_scatter <- ggplot(data = B_corr, aes(x = Value, y = es)) +
  geom_line() +  # Add line
  geom_smooth(method = "lm", se = FALSE, color = "#54D8B1FF") +  # Add smooth line with custom color
  geom_point(aes(color = "Observation", shape = "Observation"), size = 3, alpha = 0.5) +  # Add main data points
  xlab("Correlation coefficient") +
  ylab("Effect size (g)") +
  theme_classic()

# Assess outliers using residuals
model <- lm(es ~ Value, data = B_corr)
residuals <- model$residuals
outliers <- B_corr[abs(residuals) > 2 * sd(residuals), ]

# Add outliers to scatter plot
B_corr_scatter <- B_corr_scatter + 
  geom_point(data = outliers, 
             aes(x = Value, y = es, color = "Outlier", shape = "Outlier"), 
             size = 3.5) +  # Add slightly larger points for outliers
  geom_text(data = outliers, 
            aes(x = Value, y = es, label = as.character(Data_ID)), 
            position = position_jitter(width = 0.6, height = 0.2), 
            alpha = 0.9, 
            size = 3.5) +  # Add text labels only for outliers
  scale_color_manual(name = "", 
                     values = c("Observation" = "#175149FF", "Outlier" = "#E54E21FF")) +  # Custom colors
  scale_shape_manual(name = "", 
                     values = c("Observation" = 16, "Outlier" = 16)) +  # Custom shapes
  theme(legend.position = "right",
        legend.text = element_text(size = 12),  # Increase legend text size
        axis.text.x = element_text(size = 12),  # Increase x-axis text size
        axis.text.y = element_text(size = 12),  # Increase y-axis text size
        axis.title.x = element_text(size = 14), # Increase x-axis title size
        axis.title.y = element_text(size = 14)) # Increase y-axis title size

# Display scatter plot
print(B_corr_scatter)  
  
  
  
## Spearman Pearson Corr -----------------------------------------
R_comp = df %>% filter(Statistical_method %in% c("Spearman rank correlation", "Pearson correlation"))
  
  # Scatter plot showing the relationship between correlation coefficient and effect size
  R_comp_scatter <- ggplot(data = R_comp, aes(x = Value, y = es)) +
    geom_line() +  # Add line
    geom_smooth(method = "lm", se = FALSE, color = "#54D8B1FF") +  # Add smooth line with custom color
    geom_point(aes(color = "Observation", shape = "Observation"), size = 3, alpha = 0.5) +  # Add main data points
    xlab("Correlation coefficient (r)") +
    ylab("Effect size (g)") +
    theme_classic()
  
  # Assess outliers using residuals
  model <- lm(es ~ Value, data = R_comp)
  residuals <- model$residuals
  outliers <- R_comp[abs(residuals) > 2 * sd(residuals), ]
  
  # Add outliers to scatter plot
  R_comp_scatter <- R_comp_scatter + 
    geom_point(data = outliers, 
               aes(x = Value, y = es, color = "Outlier", shape = "Outlier"), 
               size = 3.5) +  # Add slightly larger points for outliers
    geom_text(data = outliers, 
              aes(x = Value, y = es, label = Data_ID), 
              position = position_jitter(width = 0.1, height = 0.5), 
              alpha = 0.9, 
              size = 3.5) +  # Add text labels only for outliers
    scale_color_manual(name = "", 
                       values = c("Observation" = "#175149FF", "Outlier" = "#E54E21FF")) +  # Custom colors
    scale_shape_manual(name = "", 
                       values = c("Observation" = 16, "Outlier" = 16)) +  # Custom shapes
    theme(legend.position = "right",
          legend.text = element_text(size = 12),  # Increase legend text size
          axis.text.x = element_text(size = 12),  # Increase x-axis text size
          axis.text.y = element_text(size = 12),  # Increase y-axis text size
          axis.title.x = element_text(size = 14), # Increase x-axis title size
          axis.title.y = element_text(size = 14)) # Increase y-axis title size
  
  # Display scatter plot
  print(R_comp_scatter)

## Ratios -----------------------------------------
Ratios_comp = df %>% filter(Type %in% c("Relative risk", "Risk ratio", "Incidence rate ratio", "Prevalence ratio", "Hospitalisation rate ratio"))
  Ratios_comp_scatter <- ggplot(data = Ratios_comp, aes(x = Value, y = es)) +
    geom_line() +  # Add line
    geom_smooth(method = "lm", se = FALSE, color = "#54D8B1FF") +  # Add smooth line with custom color
    geom_point(aes(color = "Observation", shape = "Observation"), size = 3, alpha = 0.5) +  # Add main data points
    xlab("Ratios") +
    ylab("Effect size (g)") +
    theme_classic()
  
  # Assess outliers using residuals
  model <- lm(es ~ Value, data = Ratios_comp)
  residuals <- model$residuals
  outliers <- Ratios_comp[abs(residuals) > 2 * sd(residuals), ]
  
  # Add outliers to scatter plot
  Ratios_comp_scatter <- Ratios_comp_scatter + 
    geom_point(data = outliers, 
               aes(x = Value, y = es, color = "Outlier", shape = "Outlier"), 
               size = 3.5) +  # Add slightly larger points for outliers
    geom_text(data = outliers, 
              aes(x = Value, y = es, label = Data_ID), 
              position = position_jitter(width = 0.6, height = 0.2), 
              alpha = 0.9, 
              size = 3.5) +  # Add text labels only for outliers
    scale_color_manual(name = "", 
                       values = c("Observation" = "#175149FF", "Outlier" = "#E54E21FF")) +  # Custom colors
    scale_shape_manual(name = "", 
                       values = c("Observation" = 16, "Outlier" = 16)) +  # Custom shapes
    theme(legend.position = "right",
          legend.text = element_text(size = 12),  # Increase legend text size
          axis.text.x = element_text(size = 12),  # Increase x-axis text size
          axis.text.y = element_text(size = 12),  # Increase y-axis text size
          axis.title.x = element_text(size = 14), # Increase x-axis title size
          axis.title.y = element_text(size = 14)) # Increase y-axis title size
  
  # Display scatter plot with outliers
  print(Ratios_comp_scatter)

  
## Z-scores -----------------------------------------
z_comp = df %>% filter(Type=="Z-score")
  z_comp_scatter <- ggplot(data = z_comp, aes(x = Value, y = es)) +
    geom_line() +  # Add line
    geom_smooth(method = "lm", se = FALSE, color = "#54D8B1FF") +  # Add smooth line with custom color
    geom_point(aes(color = "Observation", shape = "Observation"), size = 3, alpha = 0.5) +  # Add main data points
    xlab("Z-score") +
    ylab("Effect size (g)") +
    theme_classic()
  
  # Assess outliers using residuals
  model <- lm(es ~ Value, data = z_comp)
  residuals <- model$residuals
  outliers <- z_comp[abs(residuals) > 2 * sd(residuals), ]
  
  # Add outliers to scatter plot
  z_comp_scatter <- z_comp_scatter + 
    geom_point(data = outliers, 
               aes(x = Value, y = es, color = "Outlier", shape = "Outlier"), 
               size = 3.5) +  # Add slightly larger points for outliers
    geom_text(data = outliers, 
              aes(x = Value, y = es, label = Data_ID), 
              position = position_jitter(width = 0.6, height = 0.2), 
              alpha = 0.9, 
              size = 3.5) +  # Add text labels only for outliers
    scale_color_manual(name = "", 
                       values = c("Observation" = "#175149FF", "Outlier" = "#E54E21FF")) +  # Custom colors
    scale_shape_manual(name = "", 
                       values = c("Observation" = 16, "Outlier" = 16)) +  # Custom shapes
    theme(legend.position = "right",
          legend.text = element_text(size = 12),  # Increase legend text size
          axis.text.x = element_text(size = 12),  # Increase x-axis text size
          axis.text.y = element_text(size = 12),  # Increase y-axis text size
          axis.title.x = element_text(size = 14), # Increase x-axis title size
          axis.title.y = element_text(size = 14)) # Increase y-axis title size
  
  # Display scatter plot with outliers
  print(z_comp_scatter)

## F-stat -----------------------------------------
F_comp = df %>% filter(Type=="F-statistic")
  F_comp_scatter <- ggplot(data = F_comp, aes(x = Value, y = es)) +
    geom_line() +  # Add line
    geom_smooth(method = "lm", se = FALSE, color = "#54D8B1FF") +  # Add smooth line with custom color
    geom_point(aes(color = "Observation", shape = "Observation"), size = 3, alpha = 0.5) +  # Add main data points
    xlab("F-statistic") +
    ylab("Effect size (g)") +
    theme_classic() +
    scale_color_manual(name = "", 
                       values = c("Observation" = "#175149FF")) +  # Custom color
    scale_shape_manual(name = "", 
                       values = c("Observation" = 16)) +  # Custom shape
    theme(legend.position = "right",
          legend.text = element_text(size = 12),  # Increase legend text size
          axis.text.x = element_text(size = 12),  # Increase x-axis text size
          axis.text.y = element_text(size = 12),  # Increase y-axis text size
          axis.title.x = element_text(size = 14), # Increase x-axis title size
          axis.title.y = element_text(size = 14)) # Increase y-axis title size
  
  # Display scatter plot
  print(F_comp_scatter)

  
## Estimate -----------------------------------------
Est_comp = df %>% filter(Type=="Estimate")
  Est_comp_scatter <- ggplot(data = Est_comp, aes(x = Value, y = es)) +
    geom_smooth(method = "lm", se = FALSE, color = "#54D8B1FF") +  # Add smooth line with custom color
    geom_line(alpha = 0.3) +  # Add line
    geom_point(aes(color = "Observation", shape = "Observation"), size = 3, alpha = 0.5) +  # Add main data points
    xlab("Estimate") +
    ylab("Effect size (g)") +
    theme_classic() +
    scale_color_manual(name = "", 
                       values = c("Observation" = "#175149FF")) +  # Custom color
    scale_shape_manual(name = "", 
                       values = c("Observation" = 16)) +  # Custom shape
    theme(legend.position = "right",
          legend.text = element_text(size = 12),  # Increase legend text size
          axis.text.x = element_text(size = 12),  # Increase x-axis text size
          axis.text.y = element_text(size = 12),  # Increase y-axis text size
          axis.title.x = element_text(size = 14), # Increase x-axis title size
          axis.title.y = element_text(size = 14)) # Increase y-axis title size
  
  # Display scatter plot
  print(Est_comp_scatter)

## Percent change -----------------------------------------
p_comp = df %>% filter(Type=="Percent change")
  # Scatter plot showing the relationship between correlation coefficient and effect size
  p_comp_scatter <- ggplot(data = p_comp, aes(x = Value, y = es)) +
    geom_smooth(method = "lm", se = FALSE, color = "#54D8B1FF") +  # Add smooth line with custom color
    geom_line(alpha = 0.3) +  # Add line
    geom_point(aes(color = "Observation", shape = "Observation"), size = 3, alpha = 0.5) +  # Add main data points
    xlab("Percentage change") +
    ylab("Effect size (g)") +
    theme_classic()
  
  # Assess outliers using residuals
  model <- lm(es ~ Value, data = p_comp)
  residuals <- model$residuals
  outliers <- p_comp[abs(residuals) > 2 * sd(residuals), ]
  
  # Add outliers to scatter plot
  p_comp_scatter <- p_comp_scatter + 
    geom_point(data = outliers, 
               aes(x = Value, y = es, color = "Outlier", shape = "Outlier"), 
               size = 3.5) +  # Add slightly larger points for outliers
    geom_text(data = outliers, 
              aes(x = Value, y = es, label = Data_ID), 
              position = position_jitter(width = 0, height = 0.025), 
              alpha = 0.9, 
              size = 3.5) +  # Add text labels only for outliers
    scale_color_manual(name = "", 
                       values = c("Observation" = "#175149FF", "Outlier" = "#E54E21FF")) +  # Custom colors
    scale_shape_manual(name = "", 
                       values = c("Observation" = 16, "Outlier" = 16)) +  # Custom shapes
    theme(legend.position = "right",
          legend.text = element_text(size = 12),  # Increase legend text size
          axis.text.x = element_text(size = 12),  # Increase x-axis text size
          axis.text.y = element_text(size = 12),  # Increase y-axis text size
          axis.title.x = element_text(size = 14), # Increase x-axis title size
          axis.title.y = element_text(size = 14)) # Increase y-axis title size
  
  # Display scatter plot
  print(p_comp_scatter)

## R-squared -----------------------------------------
Rsquare_comp = df %>% filter(Type=="R square")
Rsquare_comp_scatter <- ggplot(data = Rsquare_comp, aes(x = Value, y = es)) +
    geom_line() +  # Add line
    geom_smooth(method = "lm", se = FALSE, color = "#54D8B1FF") +  # Add smooth line with custom color
    geom_point(aes(color = "Observation", shape = "Observation"), size = 3, alpha = 0.5) +  # Add main data points
    xlab("R-squared") +
    ylab("Effect size (g)") +
    theme_classic()
  
  # Assess outliers using residuals
  model <- lm(es ~ Value, data = Rsquare_comp)
  residuals <- model$residuals
  outliers <- Rsquare_comp[abs(residuals) > 2 * sd(residuals), ]
  
  # Add outliers to scatter plot
  Rsquare_comp_scatter <- Rsquare_comp_scatter + 
    geom_point(data = outliers, 
               aes(x = Value, y = es, color = "Outlier", shape = "Outlier"), 
               size = 3.5) +  # Add slightly larger points for outliers
    geom_text(data = outliers, 
              aes(x = Value, y = es, label = Data_ID), 
              position = position_jitter(width = 0.6, height = 0.2), 
              alpha = 0.9, 
              size = 3.5) +  # Add text labels only for outliers
    scale_color_manual(name = "", 
                       values = c("Observation" = "#175149FF", "Outlier" = "#E54E21FF")) +  # Custom colors
    scale_shape_manual(name = "", 
                       values = c("Observation" = 16, "Outlier" = 16)) +  # Custom shapes
    theme(legend.position = "right",
          legend.text = element_text(size = 12),  # Increase legend text size
          axis.text.x = element_text(size = 12),  # Increase x-axis text size
          axis.text.y = element_text(size = 12),  # Increase y-axis text size
          axis.title.x = element_text(size = 14), # Increase x-axis title size
          axis.title.y = element_text(size = 14)) # Increase y-axis title size
  
  # Display scatter plot
  print(Rsquare_comp_scatter)
  
  
# Hedge's g from t-stat -----------------------------------------  
# Scatter plot showing the relationship between Z-score and effect size
  Hedge_t = df %>% filter(Type=="T-value")
  Hedge_t_scatter <- ggplot(data = Hedge_t, aes(x = Value, y = es)) +
    geom_line() +  # Add line
    geom_smooth(method = "lm", se = FALSE, color = "#54D8B1FF") +  # Add smooth line with custom color
    geom_point(aes(color = "Observation", shape = "Observation"), size = 3, alpha = 0.5) +  # Add main data points
    xlab("T") +
    ylab("Effect size (g)") +
    theme_classic()
  
  # Assess outliers using residuals
  model <- lm(es ~ Value, data = Hedge_t)
  residuals <- model$residuals
  outliers <- Hedge_t[abs(residuals) > 2 * sd(residuals), ]
  
  # Add outliers to scatter plot
  Hedge_t_scatter <- Hedge_t_scatter + 
    geom_point(data = outliers, 
               aes(x = Value, y = es, color = "Outlier", shape = "Outlier"), 
               size = 3.5) +  # Add slightly larger points for outliers
    geom_text(data = outliers, 
              aes(x = Value, y = es, label = Data_ID), 
              position = position_jitter(width = 0.6, height = 0.2), 
              alpha = 0.9, 
              size = 3.5) +  # Add text labels only for outliers
    scale_color_manual(name = "", 
                       values = c("Observation" = "#175149FF", "Outlier" = "#E54E21FF")) +  # Custom colors
    scale_shape_manual(name = "", 
                       values = c("Observation" = 16, "Outlier" = 16)) +  # Custom shapes
    theme(legend.position = "right",
          legend.text = element_text(size = 12),  # Increase legend text size
          axis.text.x = element_text(size = 12),  # Increase x-axis text size
          axis.text.y = element_text(size = 12),  # Increase y-axis text size
          axis.title.x = element_text(size = 14), # Increase x-axis title size
          axis.title.y = element_text(size = 14)) # Increase y-axis title size
  
  # Display scatter plot with outliers
  print(Hedge_t_scatter)  
  
  

  
  
# 2. Calculate lms -----------------------------------------
# new
  models <- list(
    lm1 = lm(es ~ Value, data = OR_comp),
    lm2 = lm(es ~ Value, data = Beta_comp),
    lm3 = lm(es ~ Value, data = B_comp),
    lm4 = lm(es ~ Value, data = R_comp),
    lm5 = lm(es ~ Value, data = Ratios_comp),
    lm6 = lm(es ~ Value, data = z_comp),
    lm7 = lm(es ~ Value, data = F_comp),
    lm8 = lm(es ~ Value, data = Est_comp),
    lm9 = lm(es ~ Value, data = p_comp),
    lm10 = lm(es ~ Value, data = Rsquare_comp),
    lm11 = lm(es ~ Value, data = Hedge_t),
    lm12 = lm(es ~ Value, data = B_corr))
  
  # Function to check significance
  check_significance <- function(model) {
    p_value <- summary(model)$coefficients[2, 4]  # Extract the p-value for the predictor
    if (p_value < 0.001) {
      return("***")
    } else if (p_value < 0.01) {
      return("**")
    } else if (p_value < 0.05) {
      return("*")
    } else {
      return("")
    }
  }
  
  model_significance <- lapply(models, check_significance)
  
  # 3. Combine plots -------------
vjust_value = 4
custom_size = 7
custom_alpha = 0.8
custom_color = "#AF4E24FF"

vjust_value = 4
custom_size = 7
custom_alpha = 0.8
custom_color = "#AF4E24FF"
#"#C52E19FF", "#AC9765FF", "#54D8B1FF", "#B67C3BFF", "#175149FF", "#AF4E24FF"
OR_comp_scatter <- OR_comp_scatter + 
  annotate("text", x = Inf, y = Inf, label = model_significance$lm1, vjust = vjust_value, hjust = 1.1, size = custom_size, color = custom_color, alpha = custom_alpha)

Rsquare_comp_scatter <- Rsquare_comp_scatter + 
  annotate("text", x = Inf, y = Inf, label = model_significance$lm10, vjust = vjust_value, hjust = 1.1, size = custom_size, color = custom_color, alpha = custom_alpha)

Beta_comp_scatter <- Beta_comp_scatter + 
  annotate("text", x = Inf, y = Inf, label = model_significance$lm2, vjust = vjust_value, hjust = 1.1, size = custom_size, color = custom_color, alpha = custom_alpha)

B_comp_scatter <- B_comp_scatter + 
  annotate("text", x = Inf, y = Inf, label = model_significance$lm3, vjust = vjust_value, hjust = 1.1, size = custom_size, color = custom_color, alpha = custom_alpha)

R_comp_scatter <- R_comp_scatter + 
  annotate("text", x = Inf, y = Inf, label = model_significance$lm4, vjust = vjust_value, hjust = 1.1, size = custom_size, color = custom_color, alpha = custom_alpha)

Ratios_comp_scatter <- Ratios_comp_scatter + 
  annotate("text", x = Inf, y = Inf, label = model_significance$lm5, vjust = vjust_value, hjust = 1.1, size = custom_size, color = custom_color, alpha = custom_alpha)

z_comp_scatter <- z_comp_scatter + 
  annotate("text", x = Inf, y = Inf, label = model_significance$lm6, vjust = vjust_value, hjust = 1.1, size = custom_size, color = custom_color, alpha = custom_alpha)

F_comp_scatter <- F_comp_scatter + 
  annotate("text", x = Inf, y = Inf, label = model_significance$lm7, vjust = vjust_value, hjust = 1.1, size = custom_size, color = custom_color, alpha = custom_alpha)

Est_comp_scatter <- Est_comp_scatter + 
  annotate("text", x = Inf, y = Inf, label = model_significance$lm8, vjust = vjust_value, hjust = 1.1, size = custom_size, color = custom_color, alpha = custom_alpha)

p_comp_scatter <- p_comp_scatter + 
  annotate("text", x = Inf, y = Inf, label = model_significance$lm9, vjust = vjust_value, hjust = 1.1, size = custom_size, color = custom_color, alpha = custom_alpha)

Hedge_t_scatter <- Hedge_t_scatter +
  annotate("text", x = Inf, y = Inf, label = model_significance$lm11, vjust = vjust_value, hjust = 1.1, size = custom_size, color = custom_color, alpha = custom_alpha)

B_corr_scatter <- B_corr_scatter +
  annotate("text", x = Inf, y = Inf, label = model_significance$lm12, vjust = vjust_value, hjust = 1.1, size = custom_size, color = custom_color, alpha = custom_alpha)



# Create the combined plot without legends
combined_plot <- (OR_comp_scatter + theme(legend.position = "none")) + 
  (Rsquare_comp_scatter + theme(legend.position = "none",
                                axis.title.y = element_blank())) + 
  (Beta_comp_scatter + theme(legend.position = "none")) + 
  (B_comp_scatter + theme(legend.position = "none",
                          axis.title.y = element_blank())) + 
  (R_comp_scatter + theme(legend.position = "none")) + 
  (Ratios_comp_scatter + theme(legend.position = "none",
                               axis.title.y = element_blank())) + 
  (z_comp_scatter + theme(legend.position = "none")) + 
  (Hedge_t_scatter + theme(legend.position = "none",
                           axis.title.y = element_blank())) + 
  (Est_comp_scatter + theme(legend.position = "none")) + 
  (p_comp_scatter + theme(legend.position = "none",
                          axis.title.y = element_blank())) + 
  (B_corr_scatter + theme(legend.position = "none")) + 
  (F_comp_scatter + theme(legend.position = "none",
                          axis.title.y = element_blank())) +
  plot_layout(ncol = 2, nrow = 6)

# Extract the legend from one of the plots
legend_plot <- get_legend(p_comp_scatter)

# Combine the combined plot and the legend
plot_s5 <- plot_grid(
  combined_plot,                # The main combined plot
  legend_plot,                  # The extracted legend
  ncol = 1,                     # Stack them vertically
  rel_heights = c(0.95, 0.06)   # Allocate more space to the plots, less to the legend
)

# Display the final plot
print(plot_s5)

ggsave(here::here('outputs','final_figs_new','FigureS5.pdf'), plot_s5, device = "pdf",
       width = 5.5, height = 6.7, units = "in", dpi = 600, scale = 1.9)

# Save the final plot
#ggsave(here::here('outputs','FigureS5_new.png'), plot_s5, width = 5.5, height = 6.7, units = "in", dpi = 300, scale = 1.7)
