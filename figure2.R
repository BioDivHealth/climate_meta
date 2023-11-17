# load packages 
library(ggplot2)
library(dplyr)
library(patchwork)

# -------------------- Load data --------------------

# effect size data
df = read.csv('~/Projects/climate_meta/data/lewis_dataset/Effect_sizes_2023-10-04.csv')

# add Rory's annotations for reservoir and disease group
df = df %>%
  left_join(read.csv("~/Projects/climate_meta/data/rory_annotations/dz_harm.csv"))

# add in modeling method
df = df%>%
  left_join(read.csv("~/Projects/climate_meta/data/rory_annotations/method_harm.csv"))

# exclude non-eligible studies
df = df%>%
  subset(Include == 'Y')%>%
  subset(Linear_Nonlinear == 'Linear')

# change vector names
df = df%>%
  mutate(type2 = ifelse(type == 'HVH', 'vectored', 'non-vectored'))

# group effect sizes
df = df%>%
  mutate(es_cat = ifelse(abs(es) < 0.2, 'no effect (g < 0.2)', #if CI contains 0
                         ifelse(abs(es) < 0.5, 'small (0.2 < g < 0.5)', # otherwise negative
                                ifelse(abs(es) < 0.8, 'medium (0.5 < g < 0.8)',
                                       'large (g > 0.8)'))) # or positive
  )      
df$es_cat = factor(df$es_cat, levels=c('no effect (g < 0.2)', 'small (0.2 < g < 0.5)', 'medium (0.5 < g < 0.8)', 'large (g > 0.8)'))
xtextsize = 10
wdth = 9

# -------------------- Figure Parameters --------------------
### Color Scheme
#colors = as.vector(MetBrewer::met.brewer("Archambault"))
colors = c( '#fee8c8', '#fdbb84', '#e34a33', '#7f0000')

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
  axis.text.y = ggtext::element_markdown()
  #strip.text = element_text(size=12, hjust=0.4),
  #axis.text.y = element_text(size = xtextsize),
  #axis.title.y = element_text(size=11),
  #legend.title = element_text(size=10)
)

midplot = theme(
  strip.background = element_blank(),
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  strip.text.x = element_blank(),
  axis.ticks.y = element_blank(),
  axis.text.y = element_text(margin = margin(b = -20)),
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
  axis.text.y = element_text(margin = margin(b = -20)),
  legend.position = "none",
  axis.line.y = element_blank(),
  axis.line.x = element_line(color = "black"),
  axis.text.y = ggtext::element_markdown()
)

### Order for factors
df$Environmental_condition = factor(df$Environmental_condition, 
                                    levels=c('Temperature', 
                                             'Precipitation', 
                                             'Humidity'))

### Groups to include
dz = c('Hantaviral diseases', 'Arboviral diseases', 'Leptospirosis', 
       'Rickettsioses', 'Brucellosis', 'Echinococoses')
hosts = c('Rodents', 'Mammals (multispecies)', 'Livestock', 'Birds')
parasites = c('Virus', 'Bacteria')
df$parasite_group = stringr::str_to_title(df$parasite_group)
vectors = c('Mosquito', 'Tick', 'Mite')
method = c('GL(M)M', 'Correlation', '(S)ARIMA')


# Create a function to add images to facet labels
# create labeller for pictograms
picture_labeller <- function(value, pic_first=TRUE) {
  image =  paste0('~/Projects/climate_meta/images/', value, '.png')
  
  if(pic_first){
    label_with_image = paste0("<img src='", image, "' height='8' width='8' />&emsp;&emsp;", value)
  }
  else{
    label_with_image = paste0(value,  "&emsp;&emsp;&emsp;<img src='", image, "' height='15' width='15' />")
  }
  
  
  return(label_with_image)
}

figtype = 'prop'

# -------------------- FIGURE CODE: PROPORTIONS --------------------

if(figtype == 'prop'){
# 1. Disease groups
  p1 = df%>%
    subset(Disease2 %in% dz)%>%
    mutate(Disease2 = factor(Disease2, levels=rev(dz)))%>%
    count(Disease2, Environmental_condition, es_cat)%>%
    group_by(Disease2, Environmental_condition)%>%
    mutate(prop = n /sum(n), tot = sum(n))%>%
    ggplot(aes(x=prop, y=Disease2))+
    geom_col(aes(fill=es_cat), position= position_fill(reverse=TRUE), width=0.75)+
    geom_text(data = . %>% slice_head(n=1),
              aes(x=1.02, label=paste0('n = ', tot)), size=3,
              hjust=0
    )+
    topplot+
    facet_grid(.~Environmental_condition, labeller=labeller(Environmental_condition = custom_labeller))+
    scale_fill_manual(values=colors)+
    scale_x_continuous(expand = expansion(mult = c(0, .15)))+
    labs(y='Disease Group', x="")+
    #theme(panel.spacing.x = unit(1, "lines"))+
    theme(strip.text = ggtext::element_markdown(size=12, hjust=0.4, margin=margin(b=0)))
    
  
  # 2. Reservoir hosts
  p2 = df%>%
    subset(Principal_reservoir %in% hosts)%>%
    mutate(Principal_reservoir = factor(Principal_reservoir, levels=rev(hosts)))%>%
    count(Principal_reservoir, Environmental_condition, es_cat)%>%
    group_by(Principal_reservoir, Environmental_condition)%>%
    mutate(prop = n /sum(n), tot = sum(n))%>%
    ggplot(aes(x=prop, y=Principal_reservoir))+
    geom_col(aes(fill=es_cat), position= position_fill(reverse=TRUE), width=0.75)+
    geom_text(data = . %>% slice_head(n=1),
              aes(x=1.02, label=paste0('n = ', tot)), size=3,
              hjust=0
    )+
    facet_grid(.~Environmental_condition)+
    midplot+
    scale_fill_manual(values=colors)+
    theme(strip.background = element_blank())+
    scale_x_continuous(expand = expansion(mult = c(0, .15)))+
    labs(y = 'Principal \n reservoir', x="")+
    theme(panel.spacing = unit(1, "lines"))
  
  # 3. Parasite type
  p3 = df%>%
    subset(parasite_group %in% parasites)%>%
    count(parasite_group, Environmental_condition, es_cat)%>%
    group_by(parasite_group, Environmental_condition)%>%
    mutate(prop = n /sum(n), tot = sum(n))%>%
    ggplot(aes(x=prop, y=parasite_group))+
    geom_col(aes(fill=es_cat), position= position_fill(reverse=TRUE), width=0.75)+
    geom_text(data = . %>% slice_head(n=1),
              aes(x=1.02, label=paste0('n = ', tot)), size=3,
              hjust=0
    )+
    facet_grid(.~Environmental_condition)+
    midplot+
    scale_fill_manual(name="Hedge's G Effect Size", values=colors)+
    theme(legend.position = "right")+
    scale_x_continuous(expand = expansion(mult = c(0, .15)))+
    scale_y_discrete(labels=picture_labeller(rev(parasites)))+
    theme(axis.text.y = ggtext::element_markdown())+
    labs(y = "Parasite \n group", x="")+
    theme(panel.spacing = unit(1, "lines"))
  
  # Transmission type
  p4 = df%>%
    count(type2, Environmental_condition, es_cat)%>%
    group_by(type2, Environmental_condition)%>%
    mutate(prop = n /sum(n), tot = sum(n))%>%
    ggplot(aes(x=prop, y=type2))+
    geom_col(aes(fill=es_cat), position= position_fill(reverse=TRUE), width=0.75)+
    geom_text(data = . %>% slice_head(n=1),
              aes(x=1.02, label=paste0('n = ', tot)), size=3, 
              hjust=0
    )+
    facet_grid(.~Environmental_condition)+
    midplot+
    scale_fill_manual(values=colors)+
    scale_x_continuous(expand = expansion(mult = c(0, .15)))+
    labs(y= "Transmission \n Type", x="")+
    theme(panel.spacing = unit(1, "lines"))
  
  
  # 5. Vector group
  out = df%>%
    subset(vector %in% vectors)%>%
    mutate(vector = factor(vector, levels=rev(vectors)))%>%
    count(vector, Environmental_condition, es_cat)%>%
    group_by(vector, Environmental_condition)%>%
    mutate(prop = n /sum(n), tot = sum(n))%>%
    ggplot(aes(x=prop, y=vector))+
    geom_col(aes(fill=es_cat), position= position_fill(reverse=TRUE), width=0.75)+
    geom_text(data = . %>% slice_head(n=1),
              aes(x=1.02, label=paste0('n = ', tot)), size=3,
              hjust=0
    )+
    facet_grid(.~Environmental_condition)+
    botplot+
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.15)), breaks=seq(0, 1, by=0.25), labels=c(0, 0.25, 0.5, 0.75, 1))+
    scale_fill_manual(values=colors)+
    scale_y_discrete(labels=picture_labeller(rev(vectors)))+
    labs(y= "Vector \n group", x="Proportion of studies")+
    theme(panel.spacing = unit(1, "lines"),
          axis.text.y = ggtext::element_markdown(size=8, valign=0.5, halign=0.5, align_heights = TRUE, align_widths=TRUE,
                                                 padding=unit(c(10,0,0,0), 'pt')))

  
  fig = p1+p2+p3+p4+p5+plot_layout(ncol=1,
                                   heights=c(5,4,2,2,4),
                                   guides="collect")
  
  
  #ggsave(paste0('outputs/Figure3_prop.jpg'), fig, 
  #              width=1400, height=800, units="px", dpi=72)
}

