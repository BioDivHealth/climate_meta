# =========== GENERATE FIGURE 1 ==========


# ------------- setup -------------

# wd
PATH = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(PATH)

# deps
library(ggplot2); library(dplyr); library(magrittr); library(sf); library(patchwork)

# read Lewis' database of published estimates
ll = read.csv("./data/lewis_dataset/lewis_data.csv")



# -----------  colours scheme ------------

colors = as.vector(MetBrewer::met.brewer("Archambault", n=9))

dz_col = colors[2]
host_col = colors[4]
risk_col = colors[6]
meth_col = colors[5]




# ---------- plots ------------

# 1. disease group
ll = ll %>%
  dplyr::left_join(
    read.csv("dz_harm.csv")
  )

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
  geom_point(aes(N, Disease2), color=dz_col, size=3) + 
  theme_classic() +
  ylab("Disease or disease group") +
  xlab("Number of studies")


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
  geom_point(aes(N, Principal_reservoir), color=host_col, size=3) + 
  theme_classic() +
  ylab("Principal reservoir") +
  xlab("Number of studies")


# 3. response variables (hazard metric)

cats = ll %>%
  dplyr::left_join(
    read.csv("./datatype_harm.csv")
  ) %>%
  dplyr::select(Reference_ID, Reponse2) %>%
  distinct()

p1 = table(cats$Reponse2) %>%
  as.data.frame %>%
  dplyr::rename(Response=1, N=2) %>%
  dplyr::mutate(
    Response = as.vector(Response),
    Response = replace(Response, Response == "Human case locations", "Human case\nlocations"),
    Response = replace(Response, Response == "Human disease incidence", "Human disease\nincidence"),
    Response = replace(Response, Response == "Human outbreak occurrence", "Human outbreak\noccurrence"),
    Response = replace(Response, Response == "Human seroprevalence", "Human\nseroprevalence")
  ) %>%
  dplyr::filter(Response != "Host occurrence") %>%
  dplyr::arrange(desc(N)) %>%
  dplyr::mutate(Response = factor(Response, levels=rev(Response), ordered=TRUE)) %>%
  ggplot() +
  geom_segment(aes(x = 0, xend=N, y=Response, yend=Response), color=risk_col, show.legend = FALSE) + 
  geom_point(aes(N, Response), color=risk_col, size=3) + 
  theme_classic() +
  xlab("Number of studies") + ylab("Risk/hazard metric") +
  theme(axis.title.x = element_blank())


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
    Type = replace(Type, Type == "B", "Bacteria/\nrickettsia")
  ) %>%
  dplyr::arrange(desc(N)) %>%
  dplyr::mutate(Type = factor(Type, levels=rev(Type), ordered=TRUE)) %>%
  ggplot() + 
  geom_segment(aes(x = 0, xend=N, y=Type, yend=Type), color=host_col, show.legend = FALSE) + 
  geom_point(aes(N, Type), color=host_col, size=3) + 
  theme_classic() +
  xlab("Number of studies") + ylab("Pathogen type") +
  theme(axis.title.x = element_blank())

# 5. modelling method

ll = ll %>%
  left_join(
    read.csv("method_harm.csv")
  )
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
  geom_point(aes(N, Method2), color=meth_col, size=3) + 
  theme_classic() +
  xlab("Number of studies") + ylab("Modelling method")

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
  geom_point(aes(N, LN), color=meth_col,size=3) + 
  theme_classic() +
  xlab("Number of studies") + ylab("Function") +
  theme(axis.title.x = element_blank())



# ----------- map of locations ---------

# map
ne = sf::st_read("./world-administrative-boundaries.shp")
robinson = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
ne2 = sf::st_transform(ne, robinson)

# reads in extra CSV of country centroid geolocations for ones where finer scale not available

ll2 = ll %>%
  dplyr::select(-Longitude, -Latitude_) %>%
  dplyr::left_join(
    read.csv("./geoloc_harm.csv")
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

# map
map = ggplot() + 
  geom_sf(data=ne2, fill="grey90", color="grey70", size=0.4) +
  theme_void() +
  geom_sf(data=sf::st_jitter(ll2, .5), size=1.2, aes(color=Var), alpha=0.8) +
  scale_color_manual(values=pals::parula(n=8)[c(1, 4, 7)], name="Climate metric type") + 
  theme(
    legend.title = element_text(size=11),
    legend.text = element_text(size=10, color="grey15"), 
    legend.position=c(0.18, 0.2)
  )
  

# combine into multipanel using patchwork

# design for barplots
design = "
  124
  125
  136
"
comb = p0 + p1 + p00 + p3 + p5 + p4  + plot_layout(guides = "collect", 
                                                   nrow=3, ncol=3, 
                                                   widths=c(1.2, 1, 1), 
                                                   heights=c(0.25, 0.2, 0.55),
                                                   design=design)

# design for full plot
design2 = "
  11
  #2
"
comb2 = map + (comb) + plot_layout(nrow=2, heights=c(1, 0.8), widths=c(0.1, 1), design=design2)

# save
ggsave(comb2, file="Figure1.jpg", device="jpg", units="in", width=10, height=9, dpi=600, scale=0.92)




