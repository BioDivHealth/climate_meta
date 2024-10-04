# Code for Figure 4 of Gourlay et al

# dependencies:
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggridges)

df4 = df %>% filter(!is.na(es))
#df4$General_Disease[df4$General_Disease=="Haemorrhagic fever with renal syndrome"] = "HFRS"

#temperature
temp = df4 %>% filter(Environmental_condition=="Temperature")
points_temp = temp %>%
  group_by(General_Disease) %>%
  summarise(count = n(), mean_es = mean(es, na.rm = TRUE)) %>%
  filter(count > 10)  # Filter diseases with more than 10 data points
# Merge the data back with the original data
temp <- merge(temp, points_temp, by = "General_Disease")

#precipitation
prec = df4 %>% filter(Environmental_condition=="Precipitation")
points_prec = prec %>%
  group_by(General_Disease) %>%
  summarise(count = n(), mean_es = mean(es, na.rm = TRUE)) %>%
  filter(count > 10)  # Filter diseases with more than 10 data points
# Merge the data back with the original data
prec <- merge(prec, points_prec, by = "General_Disease")

#humidity
hum = df4 %>% filter(Environmental_condition=="Humidity")
points_hum = hum %>%
  group_by(General_Disease) %>%
  summarise(count = n(), mean_es = mean(es, na.rm = TRUE)) %>%
  filter(count > 10)  # Filter diseases with more than 10 data points
# Merge the data back with the original data
hum <- merge(hum, points_hum, by = "General_Disease")

data = rbind(temp, prec, hum)

# Colors  ---------------------------------------------------------

colors = c("#a80003", "#a3391f", "#9c5439", "#469e97", "#00a8a5")

# Figure Function ------------------------------------------------

createFig4  = function(df4, colors){
  
  df4$Environmental_condition = factor(df4$Environmental_condition,
                                       levels=c('Temperature',
                                                'Precipitation',
                                                'Humidity'))
  
  # Add number scaling for if crosses 0
  df4 = df4%>%
    mutate(crosszero = ifelse((es-se)*(es+se) > 0, 2, 1))
  
  df4 = df4%>%
    mutate(crosszero = ifelse(is.na(crosszero), -1, crosszero))
  
  # -------------------- Rank effect size --------------------
  # Calculate the counts for each disease
  custom_order <- c("Hantaviral diseases", 
                    "Brucellosis", 
                    "Leptospirosis",
                    "Arboviral diseases",
                    "Rickettsioses")
  
  df4$General_Disease <- factor(df4$General_Disease, levels = custom_order)
  
  df4 = df4%>%
    group_by(Environmental_condition, General_Disease)%>%
    arrange(es) %>%
    mutate(esrank=rank(es, ties.method="random"))
  
  
  # Offset vectored non-vectored 
  # Define the offset values for each disease
  disease_offsets <- c("Hantaviral diseases" = 125,
                       "Brucellosis" = 100,
                       "Leptospirosis" = 75,
                       "Arboviral diseases" = 25,
                       "Rickettsioses" = 0)
  
  # # Apply the offsets to the dataset
  # df4 <- df4 %>%
  #   mutate(esrank = esrank + ifelse(General_Disease %in% names(disease_offsets), 
  #                                   disease_offsets[General_Disease], 
  #                                   0))
  # Apply the offsets to the dataset
  df4 <- df4 %>%
    mutate(esrank = esrank + ifelse(General_Disease %in% names(disease_offsets), 
                                    disease_offsets[General_Disease], 
                                    0) + 
             ifelse(Environmental_condition == "Precipitation", 3, 
                    ifelse(Environmental_condition == "Humidity", 12, 0)))
  # # out = df4%>%
  #   subset(Environmental_condition == 'Humidity')%>%
  #   select(c(es, esrank, se, crosszero))
  
  
  # -------------------- Plot theme --------------------
  plot_theme =   theme(
    #strip.background = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    axis.line.x = element_line(),
    legend.position = "none"
    
  )
  
  
  # -------------------- Figure code --------------------
  ### Part A ### 
  p1 =df4%>% 
    subset(Linear_Nonlinear == 'Linear')%>%
    ggplot(aes(x = es, y=General_Disease, fill=General_Disease, color=General_Disease)) +
    #geom_density_ridges(aes(y=type), scale=5, alpha=0.5) +
    geom_density_ridges(scale=2.5)+
    geom_vline(xintercept = 0, linetype = 2, size = 0.8)+
    scale_fill_manual(values=alpha(colors, 0.7))+
    scale_color_manual(values=colors)+
    facet_grid(~Environmental_condition)+
    labs(x="Hedges g Effect Size", y="")+
    xlim(-2.5, 2.5)+
    #scale_y_continuous(expand = c(0,0))+
    #scale_x_continuous(expand = c(0,0))+
    plot_theme+
    labs(tag = "A")+
    theme(axis.title.x=element_text(vjust=-2),
          axis.ticks.y=element_blank(),
          plot.tag.position = c(0.055, 0.95),
          plot.tag = element_text(face = "bold", size = 14),
          strip.text = ggtext::element_markdown(size=14, hjust=0.5, margin=margin(b=10), face="bold"))
  
  custom_order_labs <- c("Hantaviral diseases", 
                         "Brucellosis", 
                         "Leptospirosis",
                         "Arboviral diseases",
                         "Rickettsioses")
  
  ann_text = data.frame(es=c(-2.5, -2.5, -2.5, -2.5, -2.5), esrank = c(124, 98, 65, 25, 2), lab = custom_order_labs,
                        General_Disease=custom_order,
                        Environmental_condition=factor('Temperature', levels=c('Temperature', 'Precipitation', 'Humidity')))
  
  ### Part B ### 
  p2 = df4%>%
    subset(Linear_Nonlinear == 'Linear')%>%
    ggplot(aes(x=es, y=esrank, col=General_Disease))+
    geom_point(shape=16,  size=3, alpha=0.8)+
    geom_vline(xintercept = 0, linetype = 2, size = 0.75)+
    geom_errorbarh(aes(xmin = es -se, xmax = es + se, color=General_Disease), height=0, show.legend = F)+
    scale_color_manual(values=colors, name = "Transmission Type")+
    # scale_shape_manual(values = c(1,19, 9), name = "Error crosses zero",
    #                    labels =c('FALSE', 'TRUE', 'Missing data for SE'))+
    #scale_alpha(range=c(0.2,0.4))+
    geom_text(data=ann_text, aes(label=lab), color="black", size=3, vjust = -0.5, hjust = 0, alpha = 0.6)+
    facet_grid(~Environmental_condition)+
    labs(x="", y = "Effect size rank")+
    scale_x_continuous(position="top", limits=c(-2.5, 2.5))+
    scale_y_reverse()+
    plot_theme+
    labs(tag = "B")+
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text = element_blank(),
          plot.tag.position = c(0.05, 0.95),
          strip.background = element_blank(),
          plot.tag = element_text(face = "bold", size = 14),
          axis.title.y=element_text(vjust=-10))
  
  
  
  # Create and save figure
  fig = p1 / plot_spacer() / p2 + plot_layout(heights=c(4, -0.1, 5))
  
  return(fig)
}

f4 = createFig4(data, colors)
f4
ggsave(f4, file=here('outputs','Figure4_General.png'), device="png", units="in", width=8, height=6, dpi=300, scale=1.25)

