# =========== GENERATE FIGURE 3 ==========

# dependencies:
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggridges)
library(magrittr)

# -------------------- Load data --------------------
df = read.csv(here::here("data","dataset_final.csv"))
df %<>% filter(!is.na(es))

# change vector names
df = df%>%
  mutate(Transmission_type = ifelse(Transmission_type == 'HVH', 'Vectored', 'Non-vectored'))

df$Transmission_type = factor(df$Transmission_type, levels=c('Non-vectored','Vectored'))

df = df%>%
  mutate(Pathogen = ifelse(Pathogen == 'P', 'Parasite',
                           ifelse(Pathogen == 'V', 'Virus',
                                  'Bacteria')))

createFig3  = function(df, colors){
# Load effect size data
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
    group_by(Environmental_condition, Transmission_type)%>%
    #arrange(es)%>%
    mutate(esrank=rank(es, ties.method="random"))
  
  
  # Offset vectored non-vectored 
  df = df%>%
    mutate(esrank = ifelse(Transmission_type == 'Vectored', esrank+100, esrank))
  
  out = df%>%
    subset(Environmental_condition == 'Humidity')%>%
    select(c(es, esrank, se, crosszero))
  
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
  p1 =df%>% 
    subset(Linear_Nonlinear == 'Linear')%>%
    ggplot(aes(x = es, y=Transmission_type, fill=Transmission_type, color=Transmission_type)) +
    #geom_density_ridges(aes(y=type), scale=5, alpha=0.5) +
    geom_density_ridges(scale=10)+
    geom_vline(xintercept = 0, linetype = 2, size = 0.8)+
    scale_fill_manual(values=alpha(colors, 0.7))+
    scale_color_manual(values=colors)+
    facet_grid(~Environmental_condition)+
    labs(x="Hedges' *g* Effect Size", y="")+
    xlim(-2.5, 2.5)+
    #scale_y_continuous(expand = c(0,0))+
    #scale_x_continuous(expand = c(0,0))+
    plot_theme+
    labs(tag = "A")+
    theme(#axis.title.x=element_text(vjust=-2),
          axis.title.x = ggtext::element_markdown(margin = margin(t = 10)),
          axis.ticks.y=element_blank(),
          plot.tag.position = c(0.055, 0.95),
          plot.tag = element_text(face = "bold", size = 14),
          strip.text = ggtext::element_markdown(size=14, hjust=0.5, margin=margin(b=10), face="bold"))
  
  
  ann_text = data.frame(es=c(-2.5, -2.5), esrank = c(-10, 90), lab = c('Non-vectored', 'Vectored'),
                        Transmission_type=c('Vectored', 'Non-vectored'),
                         Environmental_condition=factor('Temperature', levels=c('Temperature', 'Precipitation', 'Humidity')))
  
  ### Part B ### 
  p2 = df%>%
    subset(Linear_Nonlinear == 'Linear')%>%
    ggplot(aes(x=es, y=esrank, col=Transmission_type))+
    geom_point(shape=16,  size=3, alpha=0.8)+
    geom_vline(xintercept = 0, linetype = 2, size = 0.75)+
    geom_errorbarh(aes(xmin = es -se, xmax = es + se, color=Transmission_type), height=0, show.legend = F)+
    scale_color_manual(values=colors, name = "Transmission Type")+
    # scale_shape_manual(values = c(1,19, 9), name = "Error crosses zero",
    #                    labels =c('FALSE', 'TRUE', 'Missing data for SE'))+
    #scale_alpha(range=c(0.2,0.4))+
    geom_text(data=ann_text, aes(label=lab), color="black", size=3, hjust=0)+
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
  fig = p1 / plot_spacer() / p2 + plot_layout(heights=c(4, -0.1, 4))
  
  return(fig)
}

colors = c('#A80003',"#00A8A5")
f3 = createFig3(df, colors)
f3
#ggsave(f3, file=here('outputs','Figure3.png'), device="png", units="in", width=8, height=6, dpi=300, scale=1.25)
