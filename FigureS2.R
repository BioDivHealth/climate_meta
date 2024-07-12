# Code for Figure S2 ----------------

# Libraries -------------------------
library(patchwork)
library(ggplot2)
library(dplyr)

# ALL DATA -------------------------- 
## Data filtering and preparation-----
s2_method = "pearson"
stats_label = ifelse(s2_method=="spearman","Spearman's ρ:", "Pearson's r:")
smooth_method = "lm"

## ES vs IF ----------
pub1 = df %>% filter(!is.na(es) & !is.na(Journal_5yr_Impact))
pub1 %<>% dplyr::select(es, Journal_5yr_Impact, Environmental_condition)
dat_pub1 = pub1
#dat_pub1$es = abs(dat_pub1$es)

# Choose colours
colours_journals = c("#3C5488FF","#E64B35FF", "#175149")

# Scatter plot with regression line
p1 <- ggplot(dat_pub1, aes(x = Journal_5yr_Impact, y = abs(es))) +
  geom_point(color = colours_journals[2], alpha = 0.5) +
  geom_smooth(method = smooth_method, color = colours_journals[3], se = TRUE, alpha = 0.17) +
  #ggtitle("Effect Size vs Journal Impact Factor") +
  xlab("Journal 5-year Impact Factor") +
  ylab("Effect Size (Hedge's g)") +
  scale_x_continuous(breaks = seq(0, max(dat_pub1$Journal_5yr_Impact), by = 2.5))+
  theme_minimal()

# Calculate Pearson correlation coefficient
correlation1 <- cor.test(abs(dat_pub1$es), dat_pub1$Journal_5yr_Impact, method = s2_method, exact = FALSE)

# Annotate the scatter plot with the correlation coefficient

p1 <- p1 + annotate("text", x = max(dat_pub1$Journal_5yr_Impact) * 0.65, y = max(abs(dat_pub1$es)) * 0.93,
                    label = paste(stats_label, round(correlation1$estimate, 2),
                                  "\np-value:", format(correlation1$p.value, digits = 2)),
                    color = "black", size = 3.8, hjust = 0, fontface = "bold")
p1 = p1 + labs(tag = "A")+
  theme(plot.tag = element_text(face = "bold",size = 14),
        plot.tag.position = c(0.0,1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text=element_text(size=12))  # Bold y-axis text)

p1 = p1 + coord_cartesian(ylim = c(0, max(abs(dat_pub1$es))))
p1
#------------------------------------------------------------------------------|

## p-value VS IF ------
# Data filtering and preparation for the second plot
pub2 = df %>% filter(!is.na(P.value_specific) & !is.na(Journal_5yr_Impact))
pub2 %<>% dplyr::select(P.value_specific, Journal_5yr_Impact, Environmental_condition)
dat_pub2 = pub2
dat_pub2$P.value_specific = abs(dat_pub2$P.value_specific)

# Scatter plot with regression line
p2 <- ggplot(dat_pub2, aes(x = Journal_5yr_Impact, y = P.value_specific)) +
  geom_point(color = colours_journals[1], alpha = 0.5) +
  geom_smooth(method = smooth_method, color = colours_journals[3], se = TRUE, alpha=0.17) +
  #ggtitle("Reported p-value vs Journal Impact Factor") +
  xlab("Journal 5-year Impact Factor") +
  ylab("p-value reported") +
  theme_minimal()
p2 = p2 + coord_cartesian(ylim = c(0, max(dat_pub2$P.value_specific)))

# Calculate Pearson correlation coefficient
correlation2 <- cor.test(dat_pub2$Journal_5yr_Impact, dat_pub2$P.value_specific, method = s2_method, exact = FALSE)

# Annotate the scatter plot with the correlation coefficient
p2 <- p2 + annotate("text", x = max(dat_pub2$Journal_5yr_Impact) * 0.62, y = max(dat_pub2$P.value_specific) * 0.93,
                    label = paste(stats_label, round(correlation2$estimate, 2),
                                  "\np-value:", format(correlation2$p.value, digits = 2)),
                    color = "black", size = 3.8, hjust = 0, fontface = "bold")

p2 = p2 + labs(tag = "B")+
  theme(plot.tag = element_text(face = "bold",size = 14),
        plot.tag.position = c(0,1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text=element_text(size=12))
#p2 = p2 + coord_cartesian((ylim = c(0, max(dat_pub2$P.value_specific))))
p2

# Outliers removed -------------------------------------------------------------

## ES VS IF ---------------------------------------------------------------------

# Prepare data
pub1 = df %>% filter(!is.na(es) & !is.na(Journal_5yr_Impact))
pub1 %<>% dplyr::select(es, Journal_5yr_Impact, Environmental_condition)
dat_pub1 = pub1
#dat_pub1$es = abs(dat_pub1$es)
dat_pub1$es = (dat_pub1$es)
#dat_pub1 %<>% filter(Environmental_condition!="Humidity")

# Perform Shapiro-Wilk test for normality ----|
shapiro_test_es <- shapiro.test(dat_pub1$es)
shapiro_test_impact <- shapiro.test((dat_pub1$Journal_5yr_Impact))

# Print the Shapiro-Wilk test results
print(shapiro_test_es)
print(shapiro_test_impact)

### Check outliers  -----
#### Impact Factors  ----
# Calculate IQR
Q1 <- quantile(dat_pub1$Journal_5yr_Impact, 0.25)
Q3 <- quantile(dat_pub1$Journal_5yr_Impact, 0.75)
IQR <- Q3 - Q1

# Check outliers using IQR
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
iqr_outliers_1 <- dat_pub1 %>% filter(Journal_5yr_Impact < lower_bound |
                                      Journal_5yr_Impact > upper_bound
)
# Print the number of outliers
cat("Number of outliers detected by IQR method:", nrow(iqr_outliers), "\n")

# Print the outliers detected
cat("Outliers detected by IQR method:\n")
print(unique(iqr_outliers_1$Journal_5yr_Impact))

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
cat("Number of outliers detected by IQR method:", nrow(iqr_outliers), "\n")

# Print the outliers detected
cat("Outliers detected by IQR method:\n")
print(unique(iqr_outliers_2$es))

# Remove outliers
dat_pub1 %<>% filter(!es %in% unique(iqr_outliers_2$es))
#dat_pub1 %<>% filter(!Journal_5yr_Impact %in% unique(iqr_outliers_1$Journal_5yr_Impact))
#### Calculate correlation coefficient ------------------------------

dat_pub1 %<>% select(es, Journal_5yr_Impact)
dat_pub1 = unique(dat_pub1)

correlation1 <- cor.test(abs(dat_pub1$es), 
                         (dat_pub1$Journal_5yr_Impact), 
                         method = s2_method, 
                         exact = TRUE)

print(correlation1)

# Plot 
dat_pub1$es = abs(dat_pub1$es)
# Scatter plot with regression line
p1_rm <- ggplot(dat_pub1, aes(x = Journal_5yr_Impact, y = abs(es))) +
  geom_point(color = colours_journals[2], alpha = 0.5) +
  geom_smooth(method = smooth_method, color = colours_journals[3], se = TRUE, alpha = 0.17) +
  #ggtitle("Effect Size vs Journal Impact Factor") +
  xlab("Journal 5-year Impact Factor") +
  ylab("Effect Size (Hedge's g)") +
  scale_x_continuous(breaks = seq(0, max(dat_pub1$Journal_5yr_Impact), by = 2.5))+
  theme_minimal()

# Annotate the scatter plot with the correlation coefficient
p1_rm <- p1_rm + annotate("text", x = max(dat_pub1$Journal_5yr_Impact) * 0.65, y = max(dat_pub1$es) * 0.93,
                    label = paste(stats_label, round(correlation1$estimate, 2),
                                  "\np-value:", format(correlation1$p.value, digits = 2)),
                    color = "black", size = 3.8, hjust = 0, fontface = "bold")

p1_rm = p1_rm + labs(tag = "C")+
  theme(plot.tag = element_text(face = "bold",size = 14),
        plot.tag.position = c(0.0,1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text=element_text(size=12))  # Bold y-axis text)
p1_rm = p1_rm + coord_cartesian(ylim = c(0, max(dat_pub1$es)))
p1_rm


## p-value VS IF ----------------------------------------------------------------
## Prepare data
pub2 = df %>% filter(!is.na(P.value_specific) & !is.na(Journal_5yr_Impact))
pub2 %<>% dplyr::select(P.value_specific, Journal_5yr_Impact, Environmental_condition)
dat_pub2 = pub2
dat_pub2$P.value_specific = abs(dat_pub2$P.value_specific)

# Perform Shapiro-Wilk test for normality
shapiro_test_es <- shapiro.test((dat_pub2$P.value_specific))
shapiro_test_impact <- shapiro.test(dat_pub2$Journal_5yr_Impact)

# Print the Shapiro-Wilk test results
print(shapiro_test_es)
print(shapiro_test_impact)

### Check outliers ---------------------------------------------
#### IF ! ----
# Calculate IQR
Q1 <- quantile(dat_pub2$Journal_5yr_Impact, 0.25)
Q3 <- quantile(dat_pub2$Journal_5yr_Impact, 0.75)
IQR <- Q3 - Q1

# Check outliers using IQR
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
iqr_outliers_2 <- dat_pub2 %>% filter(Journal_5yr_Impact < lower_bound | Journal_5yr_Impact > upper_bound)

# Print the number of outliers
cat("Number of outliers detected by IQR method:", nrow(iqr_outliers2), "\n")

# Print the outliers detected
cat("Outliers detected by IQR method:\n")
print(unique(iqr_outliers2$Journal_5yr_Impact))



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
cat("Number of outliers detected by IQR method:", nrow(iqr_outliers2), "\n")

# Print the outliers detected
cat("Outliers detected by IQR method:\n")
print(unique(iqr_outliers2$P.value_specific))

dat_pub2 %<>% filter(!P.value_specific %in% unique(iqr_outliers_1$P.value_specific))
#dat_pub2 %<>% filter(!Journal_5yr_Impact %in% unique(iqr_outliers_1$Journal_5yr_Impact))

#### Calculate Spearman's rank correlation -----
correlation2 <- cor.test(dat_pub2$Journal_5yr_Impact, dat_pub2$P.value_specific,
                          method = s2_method, exact = FALSE)


# Scatter plot with regression line
p2_rm <- ggplot(dat_pub2, aes(x = Journal_5yr_Impact, y = P.value_specific)) +
  geom_point(color = colours_journals[1], alpha = 0.5) +
  geom_smooth(method = smooth_method, color = colours_journals[3], se = TRUE, alpha=0.17) +
  scale_x_continuous(breaks = seq(0, max(dat_pub2$Journal_5yr_Impact), by = 2.5))+
  #ggtitle("Reported p-value vs Journal Impact Factor") +
  xlab("Journal 5-year Impact Factor") +
  ylab("p-value reported") +
  theme_minimal()


# Annotate the scatter plot with the correlation coefficient
p2_rm <- p2_rm + annotate("text", x = max(dat_pub2$Journal_5yr_Impact) * 0.62, y = max(dat_pub2$P.value_specific) * 0.93,
                    label = paste(stats_label, round(correlation2$estimate, 2),
                                  "\np-value:", format(correlation2$p.value, digits = 2)),
                    color = "black", size = 3.8, hjust = 0, fontface = "bold")

p2_rm = p2_rm + labs(tag = "D")+
  theme(plot.tag = element_text(face = "bold",size = 14),
        plot.tag.position = c(0,1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text=element_text(size=12))
p2_rm = p2_rm + coord_cartesian(ylim = c(0, max(dat_pub2$P.value_specific)))
p2_rm

