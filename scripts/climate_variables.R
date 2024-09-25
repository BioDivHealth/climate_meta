# dependencies
library(tidyverse)
library(here)
library(sp)
library(terra)

# load datasets
df = read.csv(here('data','dataset_final.csv'))
coords =  read.csv(here('data','data_final_coords.csv'))

# change names of columns describign coordinates
coords %<>% rename("X" = "Longitude",
                   "Y" = "Latitude_")

# Perform a left join to add X and Y columns from coords to df
df_joined <- df %>%
  left_join(coords %>% select(Reference_ID, Data_ID, Environmental_condition,
                              Country, Location, Continent, X, Y), 
            by = c("Reference_ID", "Data_ID", "Environmental_condition",
                   "Country", "Location", "Continent"))

# keep only unique rows
df_joined = unique(df_joined)

# save file with X Y coordinates columns
write.csv(df_joined, here('data','data_with_coords.csv'), row.names = FALSE)

dat <- read.csv(here('data','data_with_coords.csv'))
dat %<>% filter(!is.na(X) & !is.na(Y))

xy <- sp::SpatialPointsDataFrame(matrix(c(dat$X, dat$Y), ncol = 2), dat)

path = ("/Users/arturtrebski/chelsa_cmip6/1981-2010/CHELSA_bio1_1981-2010_V.2.1.tif")

# Load the raster file
raster_file <- rast(path)

# Convert the sp::SpatialPointsDataFrame to terra format
xy_terra <- vect(xy)
crs(xy_terra) <- crs(raster_file)
# Extract values at xy points
#extracted_values <- extract(raster_file, xy_terra)


# List all .tif files from the directory recursively
folder_path <- "/Users/arturtrebski/chelsa_cmip6"
tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)

# Initialize a list to store extracted values for each file
extracted_data <- list()

# Loop through each .tif file and extract data
for (i in seq_along(tif_files)) {
  # Load the raster file
  raster_file <- rast(tif_files[i])
  
  # Extract values at xy points
  extracted_values <- extract(raster_file, xy_terra)
  
  # Store the extracted values in the list, and optionally add the file name as a column
  extracted_data[[i]] <- data.frame(file = tif_files[i], values = extracted_values[, 2])
}

# Combine all extracted data into a single dataframe
extracted_df <- do.call(cbind, extracted_data)



# # 5. Load reference climate data ---------------------------------------------
# 
# bioclim_layers = c(3, 5, 8, 11, 12, 14, 18)
# layers_nr = c(3, 5, 8, 11, 12, 14, 18)
# 
# # Read from storage 
# bio_layers <- list.files(climate_path, pattern = 'tif')
# bio_layers <- bio_layers[layers_nr]
# 
# bio_layers1 <- paste(climate_path, bio_layers, sep = "/")
# bio_layers2 <- lapply(bio_layers1, raster::raster)
# 
# #stack bio layers and then crop them
# bio_stack1 = raster::stack(bio_layers2)
# bio_stack_crop1 = raster::crop(bio_stack1, ext_occ)
# 
# #Specify layer to which models will be projected
# env_crop = bio_stack_crop1
# 
# ## 5.1 Load future climate data -------------------------------------------------
# 
# #create paths to each folder
# climate_path_future<-c(paste(base_dir_in,'CHELSA_future/2041-2060/rcp45/HadGEM2-CC/',sep=''),
#                        paste(base_dir_in,'CHELSA_future/2041-2060/rcp85/HadGEM2-CC/',sep=''),
#                        paste(base_dir_in,'CHELSA_future/2061-2080/rcp45/HadGEM2-CC/',sep=''),
#                        paste(base_dir_in,'CHELSA_future/2061-2080/rcp85/HadGEM2-CC/',sep='')
# )
# #create names of scenarios
# climate_scen_name<-c('rcp45_HadGEM2-CC_2041-2060','rcp85_HadGEM2-CC_2041-2060',
#                      'rcp45_HadGEM2-CC_2061-2080','rcp85_HadGEM2-CC_2061-2080')
# 
# env_crop_future <- list()
# 
# #load future data & crop
# for (fut in 1:length(climate_scen_name)) {
#   bio_layers_future <- list.files(climate_path_future[[fut]], pattern = 'tif')
#   bio_layers1_future <- paste0(climate_path_future[[fut]], bio_layers_future) 
#   bio_layers1_future <- bio_layers1_future[layers_nr]
#   bio_layers2_future <- lapply(bio_layers1_future,raster)
#   env_layers_future <- raster::stack(bio_layers2_future)
#   env_crop_future[[fut]] <- raster::crop(env_layers_future, ext_occ)
#   names(env_crop_future[[fut]]) <-c('CHELSA_bio10_03','CHELSA_bio10_05',
#                                     'CHELSA_bio10_08','CHELSA_bio10_11',
#                                     'CHELSA_bio10_12','CHELSA_bio10_14',
#                                     'CHELSA_bio10_18')
# }
# list.files("/Users/arturtrebski/chelsa_cmip6/", all.files = TRUE, recursive = TRUE)
# #Extract values of bioclimatic variables at presence/background/pseudoabsence locations
# 
# ras_extract <- 
#   function(sp_name, 
#            in_dir, 
#            out_dir, 
#            raster_in) {
#     
#     df <- vroom::vroom(paste0(in_dir, "/", sp_name, ".csv"), delim = ",")
#     xy <- sp::SpatialPointsDataFrame(matrix(c(df$x, df$y), ncol = 2), df)
#     ras_ext <- raster::extract(raster_in, xy)
#     pres_ext <- data.frame(df, ras_ext)
#     pres_ext <- pres_ext[complete.cases(pres_ext),]
#     write.csv(x = pres_ext,
#               file = paste0(out_dir, "/", sp_name, ".csv"),
#               row.names = FALSE)
#     print(sp_name)
#     
#   }
# 
# #Assign crop for callibration layers for each species 
# ext_occ_sp <- raster::extent(ext_xy(species,i))
# env_crop_sp <- raster::crop(bio_stack1, ext_occ_sp)
# 
# #Extract data for presence
# ras_extract(species$sp_name[i],paste0(base_dir_in,"/points/rarefied/"),
#             paste0(base_dir_in,"/environmental/presence/"),env_crop_sp)
# 
# 
# 
# list.files("~/chelsa_cmip6")
# 
