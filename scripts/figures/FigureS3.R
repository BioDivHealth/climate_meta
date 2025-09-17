# =========== GENERATE FIGURE S3 ==========

# Libraries -------------------------
library(patchwork)
library(ggplot2)
library(dplyr)

df = read.csv(here::here("data","dataset_final_g.csv"))
df = unique(df) #ensure all rows are unique

# Define the function to create the funnel plot
create_funnel_plot <- function(tmp, point_color, title_plot, subplot_tag, positions) {
  mean_es   <- mean(tmp$es, na.rm = TRUE)
  max_se    <- max(tmp$se, na.rm = TRUE)
  max_abs_es<- max(abs(tmp$es), na.rm = TRUE)
  
  # make x-limits symmetric around 0 as before
  xlim_range <- c(-max_abs_es, max_abs_es)
  
  region <- data.frame(
    x = c(mean_es - 1.96*max_se, mean_es + 1.96*max_se, mean_es),
    y = c(max_se, max_se, 0)
  )
  
  ggplot(tmp, aes(es, se)) +
    geom_polygon(data = region, aes(x, y), fill = point_color, alpha = 0.1, inherit.aes = FALSE) +
    geom_point(shape = 1, color = point_color) +
    geom_vline(xintercept = mean_es, linetype = "dashed") +
    coord_cartesian(xlim = xlim_range) +     # <-- instead of scale_x_continuous(limits=…)
    scale_y_reverse() +
    theme_minimal() +
    labs(tag = subplot_tag, title = title_plot) +
    theme(plot.tag = element_text(face = "bold", size = 13),
          plot.tag.position = positions)
}

f1 <- df %>% filter(!is.na(se) & !is.na(es))
funnel_plot_1 <- create_funnel_plot(f1, "forestgreen", "Overall dataset",
                                    subplot_tag="A", positions = c(0.01,1.05)) + ylab("Standard Error") + xlab("")

f2 <- df %>% filter(!is.na(se) & !is.na(es)) %>% filter(Environmental_condition == "Temperature")
funnel_plot_2 <- create_funnel_plot(f2, "#352A87", "Temperature",
                                    subplot_tag="B", positions = c(0.01,1.05)) + xlab("") + ylab("")

f3 <- df %>% filter(!is.na(se) & !is.na(es)) %>% filter(Environmental_condition == "Precipitation")
funnel_plot_3 <- create_funnel_plot(f3, "#0FAEB8", "Precipitation" ,
                                    subplot_tag="C", positions = c(0.01,1.05)) + xlab("Effect Size (Hedge's g)") + ylab("Standard Error")

f4 <- df %>% filter(!is.na(se) & !is.na(es)) %>% filter(Environmental_condition == "Humidity")
funnel_plot_4 <- create_funnel_plot(f4, "#FCC237", "Humidity",
                                    subplot_tag="D", 
                                    positions = c(0.01,1.05)) + 
  xlab("Effect Size (Hedge's g)") + ylab("") + geom_point(shape = 1, color = "#FAA209")   # Scatter plot of effect sizes vs. standard errors

s3 = ggarrange(funnel_plot_1, funnel_plot_2, funnel_plot_3, funnel_plot_4, nrow = 2, ncol = 2)
s3 = s3 + theme(plot.margin = unit(c(0.7,0,0,0.3),"cm"))

print(s3)
ggsave(s3, file=here('outputs','final_figs_new','FigureS3.pdf'), device="pdf", units="in", width=7.5, height=5, dpi=1000, scale=1.2)
