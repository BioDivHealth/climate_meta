# =========== GENERATE FIGURE 1 ==========


# ------------- setup -------------

# deps
library(pacman)
p_load(ggplot2, dplyr, magrittr, sf, patchwork, cowplot, here, MetBrewer, pals)

# read Lewis' database of published estimates
lw = read.csv(here('data','lewis_dataset','lewis_data.csv'))
lw = unique(lw)

df = read.csv(here('data','dataset_final_g.csv'))
df$Linear_Nonlinear[df$Linear_Nonlinear=="Nonlinear"] = "Non-linear"
ll = df %>% rename("Disease2" = "General_Disease",
                   "P_V_B" = "Pathogen",
                   "Method2" = "General_Stats_Method",
                   "Response2" = "General_response")

#ll %<>% filter(Principal_reservoir=="Rodents")

# -----------  colours scheme ------------

colors = as.vector(MetBrewer::met.brewer("Archambault", n=9))

dz_col = colors[2]
host_col = colors[4]
risk_col = colors[6]
meth_col = colors[5]

# define all your “global” sizes in one place
pt_size    <- 1    # was 3
seg_size   <- 0.5  # was default 0.5
sf_size    <- 0.1  # was 0.4 (map border)
bar_width  <- 0.5  # your desired bar width

# teach ggplot to use those as defaults
update_geom_defaults("point",   list(size  = pt_size   ))
update_geom_defaults("segment", list(linewidth  = seg_size  ))
update_geom_defaults("sf",      list(size  = sf_size   ))


# ---------- plots ------------

# 1. disease group
# ll = ll %>%
#   dplyr::left_join(
#     read.csv(here('data', 'rory_annotations', 'dz_harm.csv'))
#   )

p0 = ll %>%
  dplyr::filter(Disease2 != "Multiple") %>%
  dplyr::group_by(Reference_ID, Disease2) %>%
  dplyr::summarise(N = length(Disease2)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Disease2) %>%
  dplyr::summarise(N = n_distinct(Reference_ID)) %>%
  dplyr::arrange(desc(N)) %>%
  dplyr::mutate(Disease2 = factor(Disease2, levels=rev(Disease2), ordered=TRUE)) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend=N, y=Disease2, yend=Disease2), color=dz_col, show.legend = FALSE) + 
  geom_point(aes(N, Disease2), color=dz_col) + 
  theme_classic() +
  ylab("Disease or disease group") +
  labs(tag = "B")+
  xlab("Number of studies")+
  theme(plot.tag = element_text(face = "bold", size = 6),
        plot.tag.position = c(0.021, 1))



# 2. principal reservoir

p00 = ll %>%
  dplyr::filter(Disease2 != "Multiple") %>%
  dplyr::group_by(Reference_ID, Principal_reservoir) %>%
  dplyr::summarise(N = length(Principal_reservoir)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Principal_reservoir) %>%
  dplyr::summarise(N = n_distinct(Reference_ID)) %>%
  dplyr::arrange(desc(N)) %>%
  dplyr::mutate(Principal_reservoir = factor(Principal_reservoir, levels=rev(Principal_reservoir), ordered=TRUE)) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend=N, y=Principal_reservoir, yend=Principal_reservoir), color=host_col, show.legend = FALSE) + 
  geom_point(aes(N, Principal_reservoir), color=host_col) + 
  theme_classic() +
  ylab("Principal reservoir") +
  labs(tag = "D")+
  xlab("Number of studies")+
  theme(plot.tag = element_text(face = "bold", size = 6),
        plot.tag.position = c(0, 1))


# 3. response variables (hazard metric)

# harm = read.csv(here('data', 'rory_annotations', 'datatype_harm.csv'))
# harm %<>% rename("Response2" = "Reponse2")
# 
# cats = ll %>%
#   dplyr::left_join(
#     harm
#   ) %>%
#   dplyr::select(Reference_ID, Response2) %>%
#   distinct()

cats = ll %>% select(Reference_ID, Response2) %>% distinct()

p1 = table(cats$Response2) %>%
  as.data.frame %>%
  dplyr::rename(Response=1, N=2) %>%
  dplyr::mutate(
    Response = as.vector(Response),
    Response = replace(Response, Response == "Human case locations", "Human case\nlocations"),
    Response = replace(Response, Response == "Human disease incidence", "Human disease\nincidence"),
    Response = replace(Response, Response == "Human outbreak occurrence", "Human outbreak\noccurrence"),
    Response = replace(Response, Response == "Human seroprevalence", "Human seroprevalence")
  ) %>%
  dplyr::filter(Response != "Host occurrence") %>%
  dplyr::arrange(desc(N)) %>%
  dplyr::mutate(Response = factor(Response, levels=rev(Response), ordered=TRUE)) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend=N, y=Response, yend=Response), color=risk_col, show.legend = FALSE) + 
  geom_point(aes(N, Response), color=risk_col) + 
  theme_classic() +
  xlab("Number of studies") + ylab("Risk/hazard metric") +
  labs(tag = "C")+
  theme(axis.title.x = element_blank(),
        plot.tag = element_text(face = "bold", size = 6),
        plot.tag.position = c(0, 1))


# 4. pathogen type
pvb = ll %>%
  dplyr::select(Reference_ID, P_V_B) %>%
  distinct() 

p3 = table(pvb$P_V_B) %>% 
  as.data.frame() %>%
  dplyr::rename(Type=1, N=2) %>%
  dplyr::mutate(
    Type = as.vector(Type),
    Type = replace(Type, Type == "P", "Parasite"),
    Type = replace(Type, Type == "V", "Virus"),
    Type = replace(Type, Type == "B", "Bacteria/rickettsia")
  ) %>%
  dplyr::arrange(desc(N)) %>%
  dplyr::mutate(Type = factor(Type, levels=rev(Type), ordered=TRUE)) %>%
  ggplot() + 
  geom_segment(aes(x = 0, xend=N, y=Type, yend=Type), color=host_col, show.legend = FALSE) + 
  geom_point(aes(N, Type), color=host_col) + 
  theme_classic() +
  xlab("Number of studies") + ylab("Pathogen") +
  labs(tag = "E")+
  theme(axis.title.x = element_blank(),
        plot.tag = element_text(face = "bold", size = 6),
        plot.tag.position = c(-0.03, 0.98))

# 5. modelling method

# ll = ll %>%
#   left_join(
#     read.csv(here('data', 'rory_annotations', 'method_harm.csv'))
#   )

meth = ll %>%
  dplyr::select(Reference_ID, Method2) %>%
  distinct() 

p4 = table(meth$Method2) %>% 
  as.data.frame() %>%
  dplyr::rename(Method2=1, N=2) %>%
  dplyr::arrange(desc(N)) %>%
  dplyr::mutate(Method2 = factor(Method2, levels=rev(Method2), ordered=TRUE)) %>%
  ggplot() + 
  geom_segment(aes(x = 0, xend=N, y=Method2, yend=Method2), color=meth_col, show.legend = FALSE) + 
  geom_point(aes(N, Method2), color=meth_col) + 
  theme_classic() +
  labs(tag = "G")+
  xlab("Number of studies") + ylab("Modelling method")+
  theme(plot.tag = element_text(face = "bold", size = 6),
        plot.tag.position = c(-0.03, 0.98))

# 6. linear/nonlinear function fitted

nl = ll %>%
  dplyr::select(Reference_ID, Linear_Nonlinear) %>%
  distinct() 

p5 = table(nl$Linear_Nonlinear) %>% 
  as.data.frame() %>%
  dplyr::rename(LN=1, N=2) %>%
  dplyr::arrange(desc(N)) %>%
  dplyr::mutate(LN = factor(LN, levels=rev(LN), ordered=TRUE)) %>%
  ggplot() + 
  geom_segment(aes(x = 0, xend=N, y=LN, yend=LN), color=meth_col, show.legend = FALSE) + 
  geom_point(aes(N, LN), color=meth_col) + 
  theme_classic() +
  xlab("Number of studies") + ylab("Function") +
  labs(tag = "F")+
  theme(axis.title.x = element_blank(),
        plot.tag = element_text(face = "bold", size = 9),
        plot.tag.position = c(-0.03, 1.13))

# ----------- map of locations ---------

# map
ne = sf::st_read(here('data', 'shapefiles', 'world-administrative-boundaries.shp'))
robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
ne2 = sf::st_transform(ne, robinson)

# reads in extra CSV of country centroid geolocations for ones where finer scale not available
locs_extra = read.csv(here('data', 'rory_annotations', 'geoloc_harm.csv'))

ll2 = lw %>%
  dplyr::select(-Longitude, -Latitude_) %>%
   dplyr::left_join(
     locs_extra
   ) %>%
  dplyr::mutate(
    Longitude = as.numeric(Longitude), Latitude_ = as.numeric(Latitude_)
  ) %>%
  dplyr::filter(!is.na(Longitude) & !is.na(Latitude_)) %>%
  sf::st_as_sf(coords=c("Longitude", "Latitude_"), crs=sf::st_crs(ne)) %>%
  sf::st_transform(robinson) %>%
  dplyr::mutate(
    Var = factor(Environmental_condition, levels=c("Temperature", "Precipitation", "Humidity"), ordered=TRUE)
  )

# data for the bar plot
climvars = lw %>%
  select(Reference_ID, Environmental_condition)%>%
  distinct() %>% 
  count(Environmental_condition) %>%  
  group_by(Environmental_condition) %>% 
  mutate(prop = n/length(unique(df$lw)))


## Map new --------------
ll %<>% dplyr::mutate(
  Longitude = as.numeric(Longitude), Latitude_ = as.numeric(Latitude_)
) 
no_coord = ll %>% dplyr::mutate(
  Longitude = as.numeric(Longitude), Latitude_ = as.numeric(Latitude_)
)  %>% filter(is.na(Longitude) | is.na(Latitude_))

l4 = lw %>%
  dplyr::select(-Longitude, -Latitude_) %>%
  dplyr::left_join(
    locs_extra
  ) %>%
  dplyr::mutate(
    Longitude = as.numeric(Longitude), Latitude_ = as.numeric(Latitude_))

for (i in no_coord$Data_ID){
  if (i %in% l4$Data_ID){
    if((unique(is.na(no_coord$Longitude[no_coord$Data_ID==i])) | unique(is.na(no_coord$Latitude_[no_coord$Data_ID==i])))){
      print(i)
      if(unique(!is.na(l4$Latitude_)[l4$Data_ID==i]) & unique(!is.na(l4$Longitude)[l4$Data_ID==i])){
        no_coord$Latitude_[no_coord$Data_ID==i] = l4$Latitude_[l4$Data_ID==i]
        no_coord$Longitude[no_coord$Data_ID==i] = l4$Longitude[l4$Data_ID==i]
      }
    }
  }
}

coord = ll %>% filter(!(is.na(Longitude) | is.na(Latitude_)))
new_coord = rbind(coord, no_coord)

### Prepare data for the map
new_coord %<>% dplyr::mutate(Longitude = as.numeric(Longitude), Latitude_ = as.numeric(Latitude_)) %>%
  dplyr::filter(!is.na(Longitude) & !is.na(Latitude_)) %>%
  sf::st_as_sf(coords=c("Longitude", "Latitude_"), crs=sf::st_crs(ne)) %>%
  sf::st_transform(robinson) %>%
  dplyr::mutate(
    Var = factor(Environmental_condition, levels=c("Temperature", "Precipitation", "Humidity"), ordered=TRUE)
  )

# Create the map
map_new <- ggplot() + 
  geom_sf(data = ne2, fill = "grey90", color = "grey70", linewidth = 0.05) + 
  theme_void() + 
  geom_sf(data = sf::st_jitter(new_coord, factor = 0.003),size = 1, aes(color = Var), alpha = 0.8) + 
  scale_color_manual(values = pals::parula(n = 8)[c(1, 4, 7)], name = "Climate metric type") + 
  labs(tag = "A")+
  theme(
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8, color = "grey15"), 
    legend.position = "none",  # Remove the legend to make space for the bar plot
    plot.tag.position = c(0.015, 0.9),
    plot.tag = element_text(face = "bold", size = 6)
  )

# data for the bar plot new
climvars_new = ll %>%
  select(Reference_ID, Environmental_condition)%>%
  distinct() %>% 
  count(Environmental_condition) %>%  
  group_by(Environmental_condition) %>% 
  mutate(prop = n/length(unique(df$lw)))

# Create the bar plot new
bar_plot_new <- ggplot(climvars_new, aes(x = n, y = Environmental_condition, fill = Environmental_condition)) +
  geom_col() +
  scale_fill_manual(values = c("Humidity" = "#FCC237", "Precipitation" = "#0FAEB8", "Temperature" = "#352A87")) +  # Custom colors
  labs(x = "Number of studies", y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",  # Remove legend
    axis.text.y = element_text(size = 5),  # Bold y-axis text
    axis.text.x = element_text(size = 5),  # X-axis text size
    axis.title.x = element_text(size = 5),  # Bold x-axis title
    plot.tag.position = c(0.195, 1.2),
    plot.tag = element_text(face = "bold", size = 6))


combined_plot_new <- ggdraw() +
  draw_plot(map_new, 0, 0, 1, 1) +
  draw_plot(bar_plot_new, x = 0.03, y = 0.05, width = 0.23, height = 0.25)


# design for barplots
design = "
  123569
  123579
  124589
"
comb = p0 + plot_spacer() + p1 + p00 + plot_spacer() + p3 + p5 + p4 + plot_spacer() + 
  plot_layout(guides = "collect", 
              nrow=3, ncol=5, 
              widths=c(1.1, 0.03,0.8,0.07, 0.7, 0.2), 
              heights=c(0.25, 0.2, 0.55),
              design=design)

# design for full plot
design2 = "
  11
  #2
"

Fig1_new = combined_plot_new + (comb) + plot_layout(nrow=2, heights=c(1, 0.8), widths=c(0, 1), design=design2)

print(Fig1_new)

Fig1_smalltext <- Fig1_new & 
  theme(
    text          = element_text(size = 5.3),  # base text
    axis.title    = element_text(size = 5.3),
    axis.text     = element_text(size = 5),
    axis.title.x = element_text(size = 5.3),
    axis.title.y = element_text(size = 5.3),
    legend.title  = element_text(size = 5.5),
    legend.text   = element_text(size = 5.5),
    plot.tag      = element_text(face = "bold", size = 6),
    plot.margin = unit(c(-0.1, 0, 0.05, 0.05),
                        "inches")
  )



# 3️⃣ now save
ggsave(
  Fig1_smalltext,
  filename = here('outputs','final_figs_new','Figure1.pdf'),
  device   = "pdf",
  units    = "cm",
  width    = 10,
  height   = 10,
  dpi      = 600,
  scale = 1.3
)


