# Code for Figure 3 of Gourlay et al

# dependencies:
library(ggplot2)
library(ggdist)
library(dplyr)
library(patchwork)

# -------------------- Load data --------------------

# Load effect size data
df = read.csv('~/Projects/climate_meta/data/lewis_dataset/Effect_sizes_2023-11-17.csv')

# Change vector names
df = df%>%
  mutate(vectype = ifelse(type == 'HVH', 'Vectored', 'Non-vectored'))

df$vectype = factor(df$vectype, levels=c('Non-vectored', 'Vectored'))

# Order environmental factors
df$Environmental_condition = factor(df$Environmental_condition, 
                                    levels=c('Temperature', 
                                             'Precipitation', 
                                             'Humidity'))

# Add number scaling for if crosses 0
df = df%>%
  mutate(crosszero = ifelse((es-se)*(es+se) > 0, 2, 1))

df = df%>%
  mutate(crosszero = ifelse(is.na(crosszero), -1, crosszero))

# -------------------- Rank effect size --------------------

df = df%>%
  group_by(Environmental_condition, vectype)%>%
  #arrange(es)%>%
  mutate(esrank=rank(es, ties.method="random"))


# Offset vectored non-vectored 
df = df%>%
  mutate(esrank = ifelse(vectype == 'Non-vectored', esrank+20, esrank))

out = df%>%
  subset(Environmental_condition == 'Humidity')%>%
  select(c(es, esrank, se, crosszero))

# -------------------- Remove outliers --------------------
# Get Expected Values:
# Set the number of studies and observations
n = n_distinct(df$study)
k = n_distinct(df$Reference_ID, na.rm = T)

# Calculate the sampling variance
v = (n - 1) / (n - k)

# Calculate the expected range of effect sizes
upperexp = round(qnorm(0.025, lower.tail = FALSE) * sqrt(v), 3)
lowerexp = round(qnorm(0.975, lower.tail = FALSE) * sqrt(v), 3)

# max effect size
max_effect_size <- 4*sd(df$es)



# -------------------- Plot theme --------------------
plot_theme =   theme(
  #strip.background = element_blank(),
  panel.background = element_rect(color="black", fill=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line.x = element_line(),
)

colors = c('#0060FF', '#FF0060')
#colors = c('#00FFFF', '#FF0000')

# -------------------- Figure code --------------------

### Part A ### 
p1 = df%>% 
  ggplot(aes(x = es, fill=vectype)) +
  #geom_density_ridges(aes(y=type), scale=5, alpha=0.5) +
  geom_density(alpha=0.6)+
  geom_vline(xintercept = 0, linetype = 2, size = 0.75)+
  scale_fill_manual(values=colors)+
  facet_grid(Environmental_condition~., scales="free")+
  labs(x="Hedges G Effect Size", y = "Density")+
  xlim(-2.5, 2.5)+
  scale_y_continuous(expand = c(0,0))+
  #scale_x_continuous(expand = c(0,0))+
  plot_theme+
  theme(legend.position = "none",
        axis.line.y = element_line(),
        #strip.text = element_blank(),
        strip.text = element_text(size=12),
        strip.background = element_rect(fill=NA, color="black"))



### Part B ### 
p2 = df%>%
  ggplot(aes(x=es, y=esrank, col=vectype))+
  geom_point(alpha=0.4, shape=16,  size=3)+
  geom_vline(xintercept = 0, linetype = 2, size = 0.75)+
  geom_errorbarh(aes(xmin = es -se, xmax = es + se, color=vectype), height=0, show.legend = F)+
  scale_color_manual(values=colors, name = "Transmission type")+
  # scale_shape_manual(values = c(1,19, 9), name = "Error crosses zero",
  #                    labels =c('FALSE', 'TRUE', 'Missing data for SE'))+
  scale_alpha(range=c(0.2,0.4))+
  facet_grid(Environmental_condition~., scales="free")+
  labs(x="Hedges G Effect Size", y = "")+
  xlim(-2.5, 2.5)+
  plot_theme+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.key=element_rect(fill="white"),
        strip.text = element_text(size=12),
        strip.background = element_rect(fill=NA, color="black"))

fig = p1 + p2 + plot_layout(guides="collect")
ggsave(filename = 'outputs/Figure3.png', plot=fig)



df%>%
  labs(y=NULL, x= "effect size")+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.y = element_blank())+
  labs(colour=NULL)

df%>%
  ggplot(aes(x = es)) +
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black", alpha = 0.5, position = position_nudge(0.022)) +
  theme_void() +
  theme(plot.margin = margin(0, 0, 5, 0)) +
  geom_vline(xintercept = 0, linetype = 2, size = 0.75) 


combined_plot <- histogram / figure4 +
  plot_layout(guides = "collect",heights = c(0.5,3))

combined_plot


fig4_temp_og<-zooALL_effect_size_filtered %>% filter(zooALL_effect_size_filtered$Environmental_condition=="Temperature") #temperature only
fig4_temp<-fig4_temp_og %>% #temperature only with standard error values
  filter(!is.na(se))



figure4_temp<-figure4_temp+theme_cowplot()+
  labs(y=NULL, x= "effect size")+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.y = element_blank())+
  labs(colour=NULL)


df%>% 
  ggplot(aes(x = es, fill=type)) +
  stat_halfeye(binwidth = 0.2) +
  theme(plot.margin = margin(0, 0, 5, 0)) +
  geom_vline(xintercept = 0, linetype = 2, size = 0.75)+
  facet_grid(Environmental_condition~.)



combined_plot_temp <- histogram_temp / figure4_temp +
  plot_layout(guides = "collect",heights = c(0.5,3))

combined_plot_temp