# =========== GENERATE FIGURE S4 ==========

# Libraries
library(ggplot2)
library(dplyr)
library(cowplot)

# Filter and unify P-values
df = read.csv(here::here("data","dataset_final.csv"))
df = unique(df) #ensure all rows are unique

tmp = df %>% filter(P.value_general != "")

# Filter specific P-values for the plots
tmp1 = tmp %>% filter(!is.na(P.value_specific)) %>% filter(P.value_specific <= 0.07 & P.value_specific >= 0.03)
tmp2 = tmp %>% filter(!is.na(P.value_specific))

# Create data frames for the plots
pv1 = as.data.frame(tmp1$P.value_specific)
pv2 = as.data.frame(tmp2$P.value_specific)
colnames(pv1) <- "P_value_specific"
colnames(pv2) <- "P_value_specific"

# Create the zoomed-in plot (pl1)
pl1 = ggplot(pv1, aes(x = P_value_specific, y = after_stat(count))) + 
  geom_histogram(colour = 1, fill = "#3C5488FF",
                 binwidth = 0.005, boundary = 0, alpha = 0.5) +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "#E64B35FF") +
  labs(x = "p-value", y = "Count", title = "Histogram of p-vals around 0.05") +
  theme_linedraw()+
  theme(axis.text.y = element_text(size = 8),  # Bold y-axis text
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        axis.text.x = element_text(size = 8),
        title = element_text(size=8))+  # Bold x-axis title)
  xlim(0.03, 0.07)

# Create the full range plot (pl2)
pl2 = ggplot(pv2, aes(x = P_value_specific, y = after_stat(count))) + 
  geom_histogram(colour = 1, fill = "#3C5488FF", binwidth = 0.025, boundary = 0, alpha = 0.8) +
  geom_vline(xintercept = 0.05, linetype = "dashed", color = "#E64B35FF") +
  labs(x = "p-value", y = "Count", title = "Histogram of p-values") +
  theme_minimal() +
  xlim(0, 1)   # Adjust the x-axis limit based on your data range

combined_plot_s4 <- ggdraw() +
  draw_plot(pl2) +
  draw_plot(pl1, x = 0.43, y = 0.34, width = 0.49, height = 0.53) +
  draw_plot_label(label = "Zoomed-in view", x = 0.55, y = 0.925, size = 9)

print(combined_plot_s4)
