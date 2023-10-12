# load packages 
library(ggplot2)
library(dplyr)

# -------------------- Load data --------------------

# effect size data
df = read.csv('data/lewis_dataset/Effect_sizes_2023-10-04.csv')

# add Rory's annotations for reservoir and disease group
df = df %>%
  left_join(read.csv("data/rory_annotations/dz_harm.csv"))

# add in modeling method
df = df%>%
  left_join(read.csv("data/rory_annotations/method_harm.csv"))

# exclude non-eligible studies
df = df%>%
  subset(Include == 'Y')%>%
  subset(Linear_Nonlinear == 'Linear')

# change vector names
df = df%>%
  mutate(type2 = ifelse(type == 'HVH', 'vectored', 'non-vectored'))


# -------------------- Select Figure version --------------------
v = 3
versions = c('CI', 'PaperVals', 'Cats')

if(v == 1){
  df = df%>%
    mutate(es_cat = ifelse(ci.hi*ci.lo < 0, '0', #if CI contains 0
                           ifelse(es < 0, '-', # otherwise negative
                                  '+')) # or positive
    )
  df$es_cat = factor(df$es_cat, levels=c("-", "0", "+"))
  xtextsize = 14
  wdth = 7
}

if(v == 2){
  df = df%>%
    mutate(es_cat = ifelse(Lower*Upper < 0, '0', #if CI contains 0
                           ifelse(es < 0, '-', # otherwise negative
                                  '+')) # or positive
    )  
  df$es_cat = factor(df$es_cat, levels=c("-", "0", "+"))
  xtextsize = 14
  wdth = 7
}

if(v == 3){
  df = df%>%
    mutate(es_cat = ifelse(abs(es) < 0.2, 'none', #if CI contains 0
                           ifelse(abs(es) < 0.5, 'small', # otherwise negative
                                  ifelse(abs(es) < 0.8, 'med',
                                         'large'))) # or positive
    )      
  df$es_cat = factor(df$es_cat, levels=c('none', 'small', 'med', 'large'))
  xtextsize = 8
  wdth = 9
}
# -------------------- Figure Parameters --------------------
### Color Scheme
colors = as.vector(MetBrewer::met.brewer("Archambault"))

### Themes for plots
topplot =   theme(
  panel.background = element_blank(),
  panel.grid = element_blank(),
  axis.ticks.x = element_blank(),
  axis.text.x = element_blank(),
  axis.line.y = element_line(color="black"),
  strip.background = element_rect(fill="white"),
  strip.text = element_text(size=12),
  legend.title = element_text(size=10)
)

midplot = theme(
  strip.background = element_blank(),
  strip.text.x = element_blank(),
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.ticks.x = element_blank(),
  axis.text.x = element_blank(),
  axis.line.y = element_line(color = "black"),
  legend.title = element_text(size=10)
)

botplot =   theme(
  strip.background = element_blank(),
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  strip.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.line.x = element_blank(),
  axis.text.x = element_text(size = xtextsize, vjust=-0.2),
  axis.title.x = element_text(vjust=-1, size=11),
  axis.line.y = element_line(color = "black"),
  legend.title = element_text(size=10)
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
parasites = c('Bacteria', 'Virus', 'Protozoan', 'Flatworm')
df$parasite_group = stringr::str_to_title(df$parasite_group)
method = c('GL(M)M', 'Correlation', '(S)ARIMA')


# -------------------- FIGURE CODE --------------------
# 1. Disease groups
p1 = df%>%
  #subset(!is.na(es_cat))%>%
  subset(Disease2 %in% dz)%>%
  ggplot()+
  geom_histogram(aes(es_cat, fill=Disease2), stat="count", 
                 position="stack")+
  facet_grid(.~Environmental_condition)+
  topplot+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(name = "Disease group", values=colors)+
  labs(x="", y="Number of studies")

# 2. Reservoir hosts
p2 = df%>%
  #subset(!is.na(es_cat))%>%
  subset(Principal_reservoir %in% hosts)%>%
  ggplot()+
  geom_histogram(aes(es_cat, fill=Principal_reservoir), stat="count", 
                 position="stack")+
  facet_grid(.~Environmental_condition)+
  midplot+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(name = "Principal reservoir", values=colors)+
  labs(x= "", y="Number of studies")

# 3. Parasite type
p3 = df%>%
  #subset(!is.na(es_cat))%>%
  subset(parasite_group %in% parasites)%>%
  ggplot()+
  geom_histogram(aes(es_cat, fill=parasite_group), stat="count", 
                 position="stack")+
  facet_grid(.~Environmental_condition)+
  botplot+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(name="Parasite group", values=colors)+
  labs(x = "Direction of effect", y="Number of studies")

# Transmission type
p4 = df%>%
  subset(!is.na(es_cat))%>%
  #subset(parasite_group %in% parasites)%>%
  ggplot()+
  geom_histogram(aes(es_cat, fill=type2), stat="count", 
                 position="stack")+
  facet_grid(.~Environmental_condition)+
  topplot+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(name="Transmission type", values=colors)+
  labs(x= "", y="Number of studies")

# 5. Vector group
p5 = df%>%
  subset(!is.na(es_cat))%>%
  subset(vector != "")%>%
  ggplot()+
  geom_histogram(aes(es_cat, fill=vector), stat="count", 
                 position="stack")+
  facet_grid(.~Environmental_condition)+
  midplot+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(name="Primary vector", values=colors)+
  labs(x= "", y="Number of studies")

# 6. Modelling method
p6 = df%>%
  #subset(!is.na(es_cat))%>%
  subset(Method2 %in% method)%>%
  ggplot()+
  geom_histogram(aes(es_cat, fill=Method2), stat="count", 
                 position="stack")+
  facet_grid(.~Environmental_condition)+
  botplot+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_manual(name='Modelling method', values=colors)+
  labs(x = "Direction of effect", y="Number of studies")


fig = egg::ggarrange(p1, p2, p3, p4, p5, p6, nrow=3, ncol=2, byrow=F)
ggsave(paste0('outputs/Figure3_', versions[v], '.jpg'), fig, 
              width=12, height=8, units="in", dpi=300)
