# =========== GENERATE FIGURE S2 ==========

# Libraries -------------------------
library(patchwork)
library(ggplot2)
library(dplyr)
library(here)
library(ggpubr)
library(magrittr)
# ALL DATA -------------------------- 
df$P.value_specific = as.numeric(df$P.value_specific)

## Data filtering and preparation-----
s2_method = "pearson"
stats_label = ifelse(s2_method=="spearman","Spearman's ρ:", "Pearson's r:")
smooth_method = "lm"

## ES vs IF ----------
pub1 = df %>% filter(!is.na(es) & !is.na(oa_2yr_mean_citedness))
pub1 %<>% dplyr::select(es, oa_2yr_mean_citedness, Environmental_condition)
dat_pub1 = pub1
#dat_pub1$es = abs(dat_pub1$es)

# Choose colours
colours_journals = c("#3C5488FF","#E64B35FF", "#175149")

# Scatter plot with regression line
p1 <- ggplot(dat_pub1, aes(x = oa_2yr_mean_citedness, y = abs(es))) +
  geom_point(color = colours_journals[2], alpha = 0.5) +
  geom_smooth(method = smooth_method, color = colours_journals[3], se = TRUE, alpha = 0.17) +
  #ggtitle("Effect Size vs Journal Impact Factor") +
  xlab("OpenAlex 2-year mean citedness") +
  ylab("Effect Size (Hedge's g)") +
  scale_x_continuous(breaks = seq(0, max(dat_pub1$oa_2yr_mean_citedness), by = 2.5))+
  theme_minimal()

# Calculate Pearson correlation coefficient
correlation1 <- cor.test(abs(dat_pub1$es), dat_pub1$oa_2yr_mean_citedness, method = s2_method, exact = FALSE)

# Annotate the scatter plot with the correlation coefficient

p1 <- p1 + annotate("text", x = max(dat_pub1$oa_2yr_mean_citedness) * 0.65, y = max(abs(dat_pub1$es)) * 0.93,
                    label = paste(stats_label, round(correlation1$estimate, 2),
                                  "\np-value:", format(correlation1$p.value, digits = 2)),
                    color = "black", size = 3.8, hjust = 0, fontface = "bold")
p1 = p1 + labs(tag = "A")+
  theme(plot.tag = element_text(face = "bold",size = 14),
        plot.tag.position = c(0.0,1),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text=element_text(size=12))  # Bold y-axis text)

p1 = p1 + coord_cartesian(ylim = c(0, max(abs(dat_pub1$es))))
p1
#------------------------------------------------------------------------------|

## p-value VS IF ------
# Data filtering and preparation for the second plot
pub2 = df %>% filter(!is.na(P.value_specific) & !is.na(oa_2yr_mean_citedness))
pub2 %<>% dplyr::select(P.value_specific, oa_2yr_mean_citedness, Environmental_condition)
dat_pub2 = pub2
dat_pub2$P.value_specific = abs(dat_pub2$P.value_specific)

# Scatter plot with regression line
p2 <- ggplot(dat_pub2, aes(x = oa_2yr_mean_citedness, y = P.value_specific)) +
  geom_point(color = colours_journals[1], alpha = 0.5) +
  geom_smooth(method = smooth_method, color = colours_journals[3], se = TRUE, alpha=0.17) +
  #ggtitle("Reported p-value vs Journal Impact Factor") +
  xlab("OpenAlex 2-year mean citedness") +
  ylab("p-value reported") +
  theme_minimal()
p2 = p2 + coord_cartesian(ylim = c(0, max(dat_pub2$P.value_specific)))

# Calculate Pearson correlation coefficient
correlation2 <- cor.test(dat_pub2$oa_2yr_mean_citedness, dat_pub2$P.value_specific, method = s2_method, exact = FALSE)

# Annotate the scatter plot with the correlation coefficient
p2 <- p2 + annotate("text", x = max(dat_pub2$oa_2yr_mean_citedness) * 0.62, y = max(dat_pub2$P.value_specific) * 0.93,
                    label = paste(stats_label, round(correlation2$estimate, 2),
                                  "\np-value:", format(correlation2$p.value, digits = 2)),
                    color = "black", size = 3.8, hjust = 0, fontface = "bold")

p2 = p2 + labs(tag = "B")+
  theme(plot.tag = element_text(face = "bold",size = 14),
        plot.tag.position = c(0,1),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text=element_text(size=12))
#p2 = p2 + coord_cartesian((ylim = c(0, max(dat_pub2$P.value_specific))))
p2

# Outliers removed -------------------------------------------------------------

## ES VS IF ---------------------------------------------------------------------

# Prepare data
pub1 = df %>% filter(!is.na(es) & !is.na(oa_2yr_mean_citedness))
pub1 %<>% dplyr::select(es, oa_2yr_mean_citedness, Environmental_condition)
dat_pub1 = pub1
#dat_pub1$es = abs(dat_pub1$es)
dat_pub1$es = (dat_pub1$es)
#dat_pub1 %<>% filter(Environmental_condition!="Humidity")

# Perform Shapiro-Wilk test for normality ----|
shapiro_test_es <- shapiro.test(dat_pub1$es)
shapiro_test_impact <- shapiro.test((dat_pub1$oa_2yr_mean_citedness))

# Print the Shapiro-Wilk test results
print(shapiro_test_es)
print(shapiro_test_impact)

### Check outliers  -----
#### Impact Factors  ----
# Calculate IQR
Q1 <- quantile(dat_pub1$oa_2yr_mean_citedness, 0.25)
Q3 <- quantile(dat_pub1$oa_2yr_mean_citedness, 0.75)
IQR <- Q3 - Q1

# Check outliers using IQR
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
iqr_outliers_1 <- dat_pub1 %>% filter(oa_2yr_mean_citedness < lower_bound |
                                      oa_2yr_mean_citedness > upper_bound
)
# Print the number of outliers
cat("Number of outliers detected by IQR method:", nrow(iqr_outliers_1), "\n")

# Print the outliers detected
cat("Outliers detected by IQR method:\n")
print(unique(iqr_outliers_1$oa_2yr_mean_citedness))

#### Effect Sizes  ----
# Calculate IQR
Q1 <- quantile(dat_pub1$es, 0.25)
Q3 <- quantile(dat_pub1$es, 0.75)
IQR <- Q3 - Q1

# Check outliers using IQR
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
iqr_outliers_2 <- dat_pub1 %>% filter(es < lower_bound | es > upper_bound
)

# Print the number of outliers
cat("Number of outliers detected by IQR method:", nrow(iqr_outliers_2), "\n")

# Print the outliers detected
cat("Outliers detected by IQR method:\n")
print(unique(iqr_outliers_2$es))

# Remove outliers
dat_pub1 %<>% filter(!es %in% unique(iqr_outliers_2$es))
#dat_pub1 %<>% filter(!oa_2yr_mean_citedness %in% unique(iqr_outliers_1$oa_2yr_mean_citedness))
#### Calculate correlation coefficient ------------------------------

dat_pub1 %<>% select(es, oa_2yr_mean_citedness)
dat_pub1 = unique(dat_pub1)

correlation1_rm <- cor.test(abs(dat_pub1$es), 
                         (dat_pub1$oa_2yr_mean_citedness), 
                         method = s2_method, 
                         exact = TRUE)

print(correlation1_rm)

# Plot 
dat_pub1$es = abs(dat_pub1$es)

# Scatter plot with regression line
p1_rm <- ggplot(dat_pub1, aes(x = oa_2yr_mean_citedness, y = abs(es))) +
  geom_point(color = colours_journals[2], alpha = 0.5) +
  geom_smooth(method = smooth_method, color = colours_journals[3], se = TRUE, alpha = 0.17) +
  #ggtitle("Effect Size vs Journal Impact Factor") +
  xlab("OpenAlex 2-year mean citedness") +
  ylab("Effect Size (Hedge's g)") +
  scale_x_continuous(breaks = seq(0, max(dat_pub1$oa_2yr_mean_citedness), by = 2.5))+
  theme_minimal()

# Annotate the scatter plot with the correlation coefficient
p1_rm <- p1_rm + annotate("text", x = max(dat_pub1$oa_2yr_mean_citedness) * 0.65, y = max(dat_pub1$es) * 0.93,
                    label = paste(stats_label, round(correlation1_rm$estimate, 2),
                                  "\np-value:", format(correlation1_rm$p.value, digits = 2)),
                    color = "black", size = 3.8, hjust = 0, fontface = "bold")

p1_rm = p1_rm + labs(tag = "C")+
  theme(plot.tag = element_text(face = "bold",size = 14),
        plot.tag.position = c(0.0,1),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text=element_text(size=12))  # Bold y-axis text)
p1_rm = p1_rm + coord_cartesian(ylim = c(0, max(dat_pub1$es)))
p1_rm


## p-value VS IF ----------------------------------------------------------------
## Prepare data
pub2 = df %>% filter(!is.na(P.value_specific) & !is.na(oa_2yr_mean_citedness))
pub2 %<>% dplyr::select(P.value_specific, oa_2yr_mean_citedness, Environmental_condition)
dat_pub2 = pub2
dat_pub2$P.value_specific = abs(dat_pub2$P.value_specific)

# Perform Shapiro-Wilk test for normality
shapiro_test_es <- shapiro.test((dat_pub2$P.value_specific))
shapiro_test_impact <- shapiro.test(dat_pub2$oa_2yr_mean_citedness)

# Print the Shapiro-Wilk test results
print(shapiro_test_es)
print(shapiro_test_impact)

### Check outliers ---------------------------------------------
#### IF ! ----
# Calculate IQR
Q1 <- quantile(dat_pub2$oa_2yr_mean_citedness, 0.25)
Q3 <- quantile(dat_pub2$oa_2yr_mean_citedness, 0.75)
IQR <- Q3 - Q1

# Check outliers using IQR
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
iqr_outliers_2 <- dat_pub2 %>% filter(oa_2yr_mean_citedness < lower_bound | oa_2yr_mean_citedness > upper_bound)

# Print the number of outliers
cat("Number of outliers detected by IQR method:", nrow(iqr_outliers_2), "\n")

# Print the outliers detected
cat("Outliers detected by IQR method:\n")
print(unique(iqr_outliers_2$oa_2yr_mean_citedness))



#### P.value_specific ! -----
# Calculate IQR
Q1 <- quantile(dat_pub2$P.value_specific, 0.25)
Q3 <- quantile(dat_pub2$P.value_specific, 0.75)
IQR <- Q3 - Q1

# Check outliers using IQR
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
iqr_outliers_1 <- dat_pub2 %>% filter(P.value_specific < lower_bound | P.value_specific > upper_bound)

# Print the number of outliers
cat("Number of outliers detected by IQR method:", nrow(iqr_outliers_1), "\n")

# Print the outliers detected
cat("Outliers detected by IQR method:\n")
print(unique(iqr_outliers_1$P.value_specific))

dat_pub2 %<>% filter(!P.value_specific %in% unique(iqr_outliers_1$P.value_specific))
#dat_pub2 %<>% filter(!oa_2yr_mean_citedness %in% unique(iqr_outliers_1$oa_2yr_mean_citedness))

#### Calculate Spearman's rank correlation -----
correlation2_rm <- cor.test(dat_pub2$oa_2yr_mean_citedness, dat_pub2$P.value_specific,
                          method = s2_method, exact = FALSE)


# Scatter plot with regression line
p2_rm <- ggplot(dat_pub2, aes(x = oa_2yr_mean_citedness, y = P.value_specific)) +
  geom_point(color = colours_journals[1], alpha = 0.5) +
  geom_smooth(method = smooth_method, color = colours_journals[3], se = TRUE, alpha=0.17) +
  scale_x_continuous(breaks = seq(0, max(dat_pub2$oa_2yr_mean_citedness), by = 2.5))+
  #ggtitle("Reported p-value vs Journal Impact Factor") +
  xlab("OpenAlex 2-year mean citedness") +
  ylab("p-value reported") +
  theme_minimal()


# Annotate the scatter plot with the correlation coefficient
p2_rm <- p2_rm + annotate("text", x = max(dat_pub2$oa_2yr_mean_citedness) * 0.62, y = max(dat_pub2$P.value_specific) * 0.93,
                    label = paste(stats_label, round(correlation2_rm$estimate, 2),
                                  "\np-value:", format(correlation2_rm$p.value, digits = 2)),
                    color = "black", size = 3.8, hjust = 0, fontface = "bold")

p2_rm = p2_rm + labs(tag = "D")+
  theme(plot.tag = element_text(face = "bold",size = 14),
        plot.tag.position = c(0,1),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text=element_text(size=12))
p2_rm = p2_rm + coord_cartesian(ylim = c(0, max(dat_pub2$P.value_specific)))
p2_rm

# Combine plots
p3 = ggarrange(p1, p2, ncol = 2, nrow = 1)
p3 = p3  + theme(plot.margin = unit(c(0.5,0,0,0.2),"cm"))

p3_rm = ggarrange(p1_rm, p2_rm, ncol = 2, nrow = 1)
p3_rm = p3_rm  + theme(plot.margin = unit(c(0.5,0,0,0.2),"cm"))

print(p3)
print(p3_rm)




# Outliers removed on both axes -------------------------------------------------

## ES VS IF (both axes) --------------------------------------------------------

# Prepare fresh data
pub1_both = df %>% filter(!is.na(es) & !is.na(oa_2yr_mean_citedness))
pub1_both %<>% dplyr::select(es, oa_2yr_mean_citedness, Environmental_condition)
dat_pub1_both = pub1_both

# IQR bounds for both variables
Q1_es  <- quantile(dat_pub1_both$es, 0.25)
Q3_es  <- quantile(dat_pub1_both$es, 0.75)
IQR_es <- Q3_es - Q1_es
lower_es <- Q1_es - 1.5 * IQR_es
upper_es <- Q3_es + 1.5 * IQR_es

Q1_if  <- quantile(dat_pub1_both$oa_2yr_mean_citedness, 0.25)
Q3_if  <- quantile(dat_pub1_both$oa_2yr_mean_citedness, 0.75)
IQR_if <- Q3_if - Q1_if
lower_if <- Q1_if - 1.5 * IQR_if
upper_if <- Q3_if + 1.5 * IQR_if

# Filter rows within both-variable IQR ranges
dat_pub1_both %<>% filter(
  es >= lower_es & es <= upper_es &
  oa_2yr_mean_citedness >= lower_if & oa_2yr_mean_citedness <= upper_if
)

# Correlation
correlation1_both <- cor.test(abs(dat_pub1_both$es),
                              dat_pub1_both$oa_2yr_mean_citedness,
                              method = s2_method,
                              exact = FALSE)

# Plot
dat_pub1_both$es = abs(dat_pub1_both$es)
p1_both <- ggplot(dat_pub1_both, aes(x = oa_2yr_mean_citedness, y = es)) +
  geom_point(color = colours_journals[2], alpha = 0.5) +
  geom_smooth(method = smooth_method, color = colours_journals[3], se = TRUE, alpha = 0.17) +
  xlab("OpenAlex 2-year mean citedness") +
  ylab("Effect Size (Hedge's g)") +
  scale_x_continuous(breaks = seq(0, max(dat_pub1_both$oa_2yr_mean_citedness), by = 2.5))+
  theme_minimal()

p1_both <- p1_both + annotate("text", x = max(dat_pub1_both$oa_2yr_mean_citedness) * 0.65, y = max(dat_pub1_both$es) * 0.93,
                    label = paste(stats_label, round(correlation1_both$estimate, 2),
                                  "\np-value:", format(correlation1_both$p.value, digits = 2)),
                    color = "black", size = 3.8, hjust = 0, fontface = "bold")

p1_both = p1_both + labs(tag = "C")+
  theme(plot.tag = element_text(face = "bold",size = 14),
        plot.tag.position = c(0.0,1),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text=element_text(size=12))
p1_both = p1_both + coord_cartesian(ylim = c(0, max(dat_pub1_both$es)))


## p-value VS IF (both axes) ---------------------------------------------------

# Prepare fresh data
pub2_both = df %>% filter(!is.na(P.value_specific) & !is.na(oa_2yr_mean_citedness))
pub2_both %<>% dplyr::select(P.value_specific, oa_2yr_mean_citedness, Environmental_condition)
dat_pub2_both = pub2_both
dat_pub2_both$P.value_specific = abs(dat_pub2_both$P.value_specific)

# IQR bounds for both variables
Q1_p  <- quantile(dat_pub2_both$P.value_specific, 0.25)
Q3_p  <- quantile(dat_pub2_both$P.value_specific, 0.75)
IQR_p <- Q3_p - Q1_p
lower_p <- Q1_p - 1.5 * IQR_p
upper_p <- Q3_p + 1.5 * IQR_p

Q1_if2  <- quantile(dat_pub2_both$oa_2yr_mean_citedness, 0.25)
Q3_if2  <- quantile(dat_pub2_both$oa_2yr_mean_citedness, 0.75)
IQR_if2 <- Q3_if2 - Q1_if2
lower_if2 <- Q1_if2 - 1.5 * IQR_if2
upper_if2 <- Q3_if2 + 1.5 * IQR_if2

# Filter rows within both-variable IQR ranges
dat_pub2_both %<>% filter(
  P.value_specific >= lower_p & P.value_specific <= upper_p &
  oa_2yr_mean_citedness >= lower_if2 & oa_2yr_mean_citedness <= upper_if2
)

# Correlation
correlation2_both <- cor.test(dat_pub2_both$oa_2yr_mean_citedness,
                              dat_pub2_both$P.value_specific,
                              method = s2_method,
                              exact = FALSE)

# Plot
p2_both <- ggplot(dat_pub2_both, aes(x = oa_2yr_mean_citedness, y = P.value_specific)) +
  geom_point(color = colours_journals[1], alpha = 0.5) +
  geom_smooth(method = smooth_method, color = colours_journals[3], se = TRUE, alpha=0.17) +
  scale_x_continuous(breaks = seq(0, max(dat_pub2_both$oa_2yr_mean_citedness), by = 2.5))+
  xlab("OpenAlex 2-year mean citedness") +
  ylab("p-value reported") +
  theme_minimal()

p2_both <- p2_both + annotate("text", x = max(dat_pub2_both$oa_2yr_mean_citedness) * 0.62, y = max(dat_pub2_both$P.value_specific) * 0.93,
                    label = paste(stats_label, round(correlation2_both$estimate, 2),
                                  "\np-value:", format(correlation2_both$p.value, digits = 2)),
                    color = "black", size = 3.8, hjust = 0, fontface = "bold")

p2_both = p2_both + labs(tag = "D")+
  theme(plot.tag = element_text(face = "bold",size = 14),
        plot.tag.position = c(0,1),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text=element_text(size=12))
p2_both = p2_both + coord_cartesian(ylim = c(0, max(dat_pub2_both$P.value_specific)))

# Combine and print
p3_both = ggarrange(p1_both, p2_both, ncol = 2, nrow = 1)
p3_both = p3_both + theme(plot.margin = unit(c(0.5,0,0,0.2),"cm"))

print(p3_both)
