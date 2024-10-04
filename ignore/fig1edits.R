ll %<>% dplyr::mutate(
  Longitude = as.numeric(Longitude), Latitude_ = as.numeric(Latitude_)
) 

#Orig Lewis + Extra coord -- keeping Latitude and Longitude
l4 = lw %>%
  dplyr::select(-Longitude, -Latitude_) %>%
  dplyr::left_join(
    locs_extra
  ) %>%
  dplyr::mutate(
    Longitude = as.numeric(Longitude), Latitude_ = as.numeric(Latitude_))
  # ) %>%
  # dplyr::filter(!is.na(Longitude) & !is.na(Latitude_)) %>%
  # sf::st_as_sf(coords=c("Longitude", "Latitude_"), crs=sf::st_crs(ne)) %>%
  # sf::st_transform(robinson) %>%
  # dplyr::mutate(
  #   Var = factor(Environmental_condition, levels=c("Temperature", "Precipitation", "Humidity"), ordered=TRUE)
  # )

no_coord = ll %>% filter(is.na(Longitude) | is.na(Latitude_))

for (i in no_coord$Data_ID){
  #if (i %in% ll2$Data_ID){
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

new_coord %<>% dplyr::mutate(Longitude = as.numeric(Longitude), Latitude_ = as.numeric(Latitude_)) %>%
dplyr::filter(!is.na(Longitude) & !is.na(Latitude_)) %>%
sf::st_as_sf(coords=c("Longitude", "Latitude_"), crs=sf::st_crs(ne)) %>%
sf::st_transform(robinson) %>%
dplyr::mutate(
  Var = factor(Environmental_condition, levels=c("Temperature", "Precipitation", "Humidity"), ordered=TRUE)
)

# Create the map
map <- ggplot() + 
  geom_sf(data = ne2, fill = "grey90", color = "grey70", size = 0.4) + 
  theme_void() + 
  geom_sf(data = sf::st_jitter(new_coord, factor = 0.003), size = 1.2, aes(color = Var), alpha = 0.8) + 
  scale_color_manual(values = pals::parula(n = 8)[c(1, 4, 7)], name = "Climate metric type") + 
  labs(tag = "A")+
  theme(
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10, color = "grey15"), 
    legend.position = "none",  # Remove the legend to make space for the bar plot
    plot.tag.position = c(0.05, 0.9),
    plot.tag = element_text(face = "bold", size = 12)
  )






test = lw %>%
  #dplyr::select(-Longitude, -Latitude_) %>%
  dplyr::mutate(
    Longitude = as.numeric(Longitude), Latitude_ = as.numeric(Latitude_)
  ) %>%
  dplyr::filter(!is.na(Longitude) & !is.na(Latitude_)) %>%
  sf::st_as_sf(coords=c("Longitude", "Latitude_"), crs=sf::st_crs(ne)) %>%
  sf::st_transform(robinson)

  #dplyr::mutate(
  #  Var = factor(Environmental_condition, levels=c("Temperature", "Precipitation", "Humidity"), ordered=TRUE)
  #)

#-------------------
# Map with only good data

#Orig Lewis + Extra coord -- keeping Latitude and Longitude
l_map = ll %>%
  dplyr::mutate(
    Longitude = as.numeric(Longitude), Latitude_ = as.numeric(Latitude_)) %>%
dplyr::filter(!is.na(Longitude) & !is.na(Latitude_)) %>%
sf::st_as_sf(coords=c("Longitude", "Latitude_"), crs=sf::st_crs(ne)) %>%
sf::st_transform(robinson) %>%
dplyr::mutate(
  Var = factor(Environmental_condition, levels=c("Temperature", "Precipitation", "Humidity"), ordered=TRUE)
)
map2 <- ggplot() + 
  geom_sf(data = ne2, fill = "grey90", color = "grey70", size = 0.4) + 
  theme_void() + 
  geom_sf(data = sf::st_jitter(l_map, factor = 0.003), size = 1.2, aes(color = Var), alpha = 0.8) + 
  scale_color_manual(values = pals::parula(n = 8)[c(1, 4, 7)], name = "Climate metric type") + 
  labs(tag = "A")+
  theme(
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 10, color = "grey15"), 
    legend.position = "none",  # Remove the legend to make space for the bar plot
    plot.tag.position = c(0.05, 0.9),
    plot.tag = element_text(face = "bold", size = 12)
  )

