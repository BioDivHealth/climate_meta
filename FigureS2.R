# Code for Figure S2 ----------------

# Libraries -------------------------
library(patchwork)
library(ggplot2)
library(dplyr)

# Data filtering and preparation-----
s2_method = "spearman"
stats_label = ifelse(s2_method=="spearman","Spearman’s rho:", "Pearson r")
smooth_method = "loess"

pub1 = df %>% filter(!is.na(es) & !is.na(Journal_5yr_Impact))
pub1 %<>% dplyr::select(es, Journal_5yr_Impact, Environmental_condition)
dat_pub1 = pub1
dat_pub1$es = abs(dat_pub1$es)

# Choose colours
colours_journals = c("#3C5488FF","#E64B35FF", "#175149")

# Scatter plot with regression line
p1 <- ggplot(dat_pub1, aes(x = Journal_5yr_Impact, y = es)) +
  geom_point(color = colours_journals[2], alpha = 0.5) +
  geom_smooth(method = smooth_method, color = colours_journals[3], se = TRUE, alpha = 0.17) +
  #ggtitle("Effect Size vs Journal Impact Factor") +
  xlab("Journal 5-year Impact Factor") +
  ylab("Effect Size (Hedge's g)") +
  scale_x_continuous(breaks = seq(0, max(dat_pub1$Journal_5yr_Impact), by = 2.5))+
  theme_minimal()

# Calculate Pearson correlation coefficient
correlation1 <- cor.test(dat_pub1$es, dat_pub1$Journal_5yr_Impact, method = s2_method, exact = FALSE)

# Annotate the scatter plot with the correlation coefficient
p1 <- p1 + annotate("text", x = max(dat_pub1$Journal_5yr_Impact) * 0.6, y = max(dat_pub1$es) * 0.9,
                    label = paste(stats_label, round(correlation1$estimate, 2),
                                  "\nP-value:", format(correlation1$p.value, digits = 2)),
                    color = "black", size = 4, hjust = 0)
p1 = p1 + labs(tag = "A")+
  theme(plot.tag = element_text(face = "bold",size = 14),
        plot.tag.position = c(0.0,1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text=element_text(size=12))  # Bold y-axis text)
p1
#------------------------------------------------------------------------------

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

# Calculate Pearson correlation coefficient
correlation2 <- cor.test(dat_pub2$Journal_5yr_Impact, dat_pub2$P.value_specific, method = s2_method, exact = FALSE)

# Annotate the scatter plot with the correlation coefficient
p2 <- p2 + annotate("text", x = max(dat_pub2$Journal_5yr_Impact) * 0.6, y = max(dat_pub2$P.value_specific) * 0.9,
                    label = paste(stats_label, round(correlation2$estimate, 2),
                                  "\nP-value:", format(correlation2$p.value, digits = 2)),
                    color = "black", size = 4, hjust = 0)

p2 = p2 + labs(tag = "B")+
  theme(plot.tag = element_text(face = "bold",size = 14),
        plot.tag.position = c(0,1),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text=element_text(size=12))
p2