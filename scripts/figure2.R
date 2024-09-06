# load packages 
library(ggplot2)
library(dplyr)
library(patchwork)

# Create a function to add images to facet labels
# create labeller for pictograms
# picture_labeller <- function(value, pic_first=TRUE) {
#   image =  paste0('~/Projects/climate_meta/images/', value, '.png')
#   
#   if(pic_first){
#     label_with_image = paste0("<img src='", image, "' height='8' width='8' />&emsp;&emsp;", value)
#   }
#   else{
#     label_with_image = paste0(value,  "&emsp;&emsp;&emsp;<img src='", image, "' height='15' width='15' />")
#   }
#   
#   
#   return(label_with_image)
# }

# group effect sizes
df = df%>%
  mutate(es_cat = ifelse(es < -0.2, 'Negative effect (g < -0.2)', #if CI contains 0
                         ifelse(abs(es) < 0.2, 'No effect', # otherwise negative
                                'Positive effect (g > 0.2)')) # or positive
  )

createFig2  = function(df, stats, colors){
  # -------------------- Format data --------------------
  # exclude non-eligible studies
  df = df%>%
    subset(Linear_Nonlinear == 'Linear')
  
  # stats dataframe
  stats = stats%>%
    mutate(significance = ifelse(p_val < 0.001, '***', ifelse(p_val < 0.01, '**',
                                                              ifelse(p_val < 0.05, '*', ''))))
  
  ### Groups to include
  hosts = c('Rodents', 'Mammals (multispecies)', 'Livestock', 'Birds')
  parasites = c('Virus', 'Bacteria')
  vectors = c('Mosquito', 'Tick', 'Mite')
  
  
  ### Order for factors
  df$Environmental_condition = factor(df$Environmental_condition, 
                                      levels=c('Temperature', 
                                               'Precipitation', 
                                               'Humidity'))
  
  stats$Environmental_condition = factor(stats$Environmental_condition, 
                                      levels=c('Temperature', 
                                               'Precipitation', 
                                               'Humidity'))
  
  # -------------------- Figure Parameters --------------------
  ### Themes for plots
  topplot =   theme(
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    legend.position = 'none',
    strip.background = element_rect(fill="white"),
    axis.title.y = element_text(face="bold")
  )
  
  midplot = theme(
    strip.background = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.text.y = ggtext::element_markdown()
  )
  
  botplot =   theme(
    strip.background = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.x = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.text.y = ggtext::element_markdown()
  )

  # -------------------- Figure Code  --------------------

  # 1. All together
  p1 = df%>%
    count(Environmental_condition, es_cat)%>%
    group_by(Environmental_condition)%>%
    mutate(prop = n /sum(n), tot = sum(n))%>%
    ggplot(aes(x=prop, y="Full Dataset"))+
    geom_col(aes(fill=es_cat), position= position_fill(reverse=TRUE), width=0.75)+
    geom_label(data = . %>% slice_head(n=1),
              aes(x=0.02, label=paste0('n = ', tot)), 
              hjust=0,
              size=3,
              label.size=0,
              alpha=0.5
    )+
    topplot+
    facet_grid(.~Environmental_condition)+
    scale_fill_manual(values=colors)+
    scale_x_continuous(expand=c(0,0),limits=c(0,1), breaks=seq(0, 1, by=0.25), labels=c(0, 0.25, 0.5, 0.75, 1))+
    labs(y="", x="")+
    theme(strip.text = ggtext::element_markdown(size=14, hjust=0.5, margin=margin(b=0), face="bold"),
          axis.text.y =  element_text(angle=0, size=12, vjust=0.5, hjust=0, face="bold"))
  
# Create horizontal line
  data = data.frame(x = 1, y = 1)
  hl = ggplot(data, aes(x, y)) +
    geom_hline(yintercept = 1) +  # Add a horizontal line at y = 1
    theme_void()
  
  
# 2. Transmission type
  pv = stats%>%
   subset(Category == 'Transmission_type')%>%
   rename(Transmission_type = Group)

  p2 = df%>%
    count(Transmission_type, Environmental_condition, es_cat)%>%
    group_by(Transmission_type, Environmental_condition)%>%
    mutate(prop = n /sum(n), tot = sum(n))%>%
    ggplot(aes(x=prop, y=Transmission_type))+
    geom_col(aes(fill=es_cat), position= position_fill(reverse=TRUE), width=0.75)+
    geom_label(data = . %>% slice_head(n=1),
              aes(x=0.02, label=paste0('n = ', tot)), 
              vjust=0.5,
              hjust=0,
              label.size=0,
              alpha=0.5,
              size=3
    )+
    geom_text(data=pv, aes(x=.02, y=Transmission_type, label=significance),
              fontface="bold",
              vjust=0,
              hjust=0,
              size=8,
              alpha = 0.8)+
    facet_grid(.~Environmental_condition)+
    midplot+
    scale_fill_manual(values=colors)+
    scale_x_continuous(expand=c(0,0),limits=c(0,1), breaks=seq(0, 1, by=0.25), labels=c(0, 0.25, 0.5, 0.75, 1))+
    labs(y= "Transmission \n Type", x="")+
    theme(panel.spacing = unit(1, "lines"))
  
  
  # 3. Parasite type
  pv = stats%>%
    subset(Category == 'Pathogen')%>%
    rename(Pathogen = Group)
  
  p3 = df%>%
    subset(Pathogen %in% parasites)%>%
    count(Pathogen, Environmental_condition, es_cat)%>%
    group_by(Pathogen, Environmental_condition)%>%
    mutate(prop = n /sum(n), tot = sum(n))%>%
    ggplot(aes(x=prop, y=Pathogen))+
    geom_col(aes(fill=es_cat), position= position_fill(reverse=TRUE), width=0.75)+
    geom_label(data = . %>% slice_head(n=1),
              aes(x=0.02, label=paste0('n = ', tot)), 
              vjust=0.5,
              hjust=0,
              label.size=0,
              alpha=0.5,
              size=3
    )+
    geom_text(data=pv, aes(x=0.02, y=Pathogen, label=significance),
              fontface="bold",
              vjust=0,
              hjust=0,
              size=8,
              alpha = 0.8
              )+
    facet_grid(.~Environmental_condition)+
    midplot+
    scale_fill_manual(name="Hedges *g* Effect Size", values=colors)+
    theme(legend.position = "right")+
    scale_x_continuous(expand=c(0,0),limits=c(0,1), breaks=seq(0, 1, by=0.25), labels=c(0, 0.25, 0.5, 0.75, 1))+
    #scale_y_discrete(labels=picture_labeller(rev(parasites)))+
    theme(axis.text.y = ggtext::element_markdown(),
          legend.title = ggtext::element_markdown())+
    labs(y = "Pathogen \n type", x="")+
    theme(panel.spacing = unit(1, "lines"))
  

  # 4. Vector group
  # get pvals
  pv = stats%>%
    subset(Category == 'vector')%>%
    rename(vector = Group)
  
  p4 = df%>%
    subset(vector %in% vectors)%>%
    mutate(vector = factor(vector, levels=rev(vectors)))%>%
    count(vector, Environmental_condition, es_cat)%>%
    group_by(vector, Environmental_condition)%>%
    mutate(prop = n /sum(n), tot = sum(n))%>%
    ggplot(aes(x=prop, y=vector))+
    geom_col(aes(fill=es_cat), position= position_fill(reverse=TRUE), width=0.75)+
    geom_label(data = . %>% slice_head(n=1),
              aes(x=0.02, label=paste0('n = ', tot)), 
              vjust=0.5,
              hjust=0,
              size=3,
              alpha=0.5,
              label.size=0
    )+
    geom_text(data=pv, aes(x=0.02, y=vector, label=significance),
              fontface="bold",
              vjust=0,
              hjust=0,
              size=8,
              alpha = 0.8)+
    facet_grid(.~Environmental_condition)+
    midplot+
    scale_x_continuous(expand=c(0,0),limits=c(0,1), breaks=seq(0, 1, by=0.25), labels=c(0, 0.25, 0.5, 0.75, 1))+
    scale_fill_manual(values=colors)+
    #scale_y_discrete(labels=picture_labeller(rev(vectors)))+
    labs(y= "Vector \n group", x="")+
    theme(panel.spacing = unit(1, "lines"),
          axis.text.y = ggtext::element_markdown())

  # 2. Reservoir hosts
  pv = stats%>%
    subset(Category == 'Principal_reservoir')%>%
    rename(Principal_reservoir = Group)
  
  p5 = df%>%
    subset(Principal_reservoir %in% hosts)%>%
    mutate(Principal_reservoir = factor(Principal_reservoir, levels=rev(hosts)))%>%
    count(Principal_reservoir, Environmental_condition, es_cat)%>%
    group_by(Principal_reservoir, Environmental_condition)%>%
    mutate(prop = n /sum(n), tot = sum(n))%>%
    ggplot(aes(x=prop, y=Principal_reservoir))+
    geom_col(aes(fill=es_cat), position= position_fill(reverse=TRUE), width=0.75)+
    geom_label( data = . %>% slice_head(n=1), 
              aes(x=0.02, label=paste0('n = ', tot)), 
              vjust=0.5,
              hjust=0,
              size=3,
              alpha=0.5,
              label.size=0
    )+
    geom_text(data=pv, aes(x=.02, y=Principal_reservoir, label=significance),
              fontface="bold",
              size=8,
              alpha = 0.8,
              hjust=0,
              vjust=0.1)+
    facet_grid(.~Environmental_condition)+
    botplot+
    scale_fill_manual(values=colors)+
    theme(strip.background = element_blank())+
    scale_x_continuous(expand=c(0,0),limits=c(0,1), breaks=seq(0, 1, by=0.25), labels=c(0, 0.25, 0.5, 0.75, 1))+
    #scale_x_continuous(expand = expansion(mult = c(0, .15)))+
    labs(y = 'Principal \n reservoir', x="Proportion of measures")+
    theme(panel.spacing = unit(1, "lines"))
  
  fig = p1+hl+p2+p3+p4+p5+plot_layout(ncol=1,
                                   heights=c(2,0.25, 2,2,3,3),
                                   guides="collect")
  
  
  return(fig)
}

# -------------------- Create Figure  --------------------

# colors = c( "#29A1AB" , '#fee8c8', '#AB3329')
# arch palette"#88a0dc" "#381a61" "#7c4b73" "#ed968c" "#ab3329" "#e78429" "#f9d14a"

#createFig2(df, results_df, colors)

