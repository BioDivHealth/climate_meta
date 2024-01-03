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

# -------------------- Plot theme --------------------
plot_theme =   theme(
  #strip.background = element_blank(),
  panel.background = element_rect(color="black", fill=NA),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line.x = element_line()
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


# Create and save figure
fig = p1 + p2 + plot_layout(guides="collect")
#ggsave(filename = 'outputs/Figure3.png', plot=fig)


# -------------------- Stats test --------------------
# separate into bins
# group effect sizes
df = df%>%
  mutate(es_cat = ifelse(abs(es) < 0.2, '', #if CI contains 0
                         ifelse(abs(es) < 0.5, 'small (0.2 < g < 0.5)', # otherwise negative
                                ifelse(abs(es) < 0.8, 'medium (0.5 < g < 0.8)',
                                       'large (g > 0.8)'))) # or positive
  )      

# Determine cut points based on effect size
cut_points = c(-Inf, -0.2, 0.2, Inf)

# cut up effect size
df$es_bin = cut(df$es, breaks = cut_points, labels=c('neg', 'none', 'pos'))

# -------------------- Do the tests --------------------
# subset data and create table for the chi-square test

### Temperature ###
df_sub = df%>%
  filter(Environmental_condition == 'Temperature')

df_sub$es_bin = droplevels(df_sub$es_bin)

contingency_table <- table(df_sub$es_bin, df_sub$type)

# Step 5: Perform a chi-square test for differences between groups within each bin
temp_chisq<- chisq.test(contingency_table)
resid(temp_chisq)

# Display the result
print(temp_chisq)

### Precipitation ### 
df_sub = df%>%
  filter(Environmental_condition == 'Precipitation')

df_sub$es_bin = droplevels(df_sub$es_bin)

contingency_table <- table(df_sub$es_bin, df_sub$type)

#Perform a chi-square test for differences between groups within each bin
precip_chisq <- chisq.test(contingency_table)
resid(precip_chisq)

# Display the result
print(precip_chisq)

### Humidity ### 
df_sub = df%>%
  filter(Environmental_condition == 'Humidity')

df_sub$es_bin = droplevels(df_sub$es_bin)

contingency_table <- table(df_sub$es_bin, df_sub$type)

#Perform a chi-square test for differences between groups within each bin
humidity_chisq <- chisq.test(contingency_table)
resid(humidity_chisq)

# Display the result
print(humidity_chisq)

### Save a table of results ###
results = data.frame(group = c('Temperature', 'Precipitation', 'Humidity'),
                     chi_sq = c(temp_chisq$statistic, precip_chisq$statistic, humidity_chisq$statistic),
                     pval = c(temp_chisq$p.value, precip_chisq$p.value, humidity_chisq$statistic)
                     )

write.csv(results, '~/Projects/climate_meta/outputs/tables/ChiSq_EnviroCats_Results.csv', row.names = F)

