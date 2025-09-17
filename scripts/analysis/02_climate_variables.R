# Dependencies ------------------------------------------------------------
library(tidyverse)  
library(dplyr)
library(here)       
library(sp)         
library(terra)    
library(magrittr)
library(rlang)

# Set Up Data --------------------------------------------------------------
# Load datasets
dat <- read.csv(here('data', 'dataset_final_g.csv'))
dat %<>% select(-X, -Y) 
lw = read.csv(here('data','lewis_dataset','lewis_data.csv'))
lw = unique(lw)

# dat$X = as.numeric(dat$Longitude)
# dat$Y = as.numeric(dat$Latitude_)

# reads in extra CSV of country centroid geolocations for ones where finer scale not available
locs_extra = read.csv(here('data', 'rory_annotations', 'geoloc_harm.csv'))

ll = dat

# ADD COORDINATEs --------------------------------------------------------

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

l4 = unique(l4)
dim(l4)

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

new_coord$X = new_coord$Longitude
new_coord$Y = new_coord$Latitude_

dat = dat %>% left_join(new_coord %>% select(row_unique, X, Y), by="row_unique")
# ------------------------------------------------------------------------------

View(dat %>% select(Reference_ID,Latitude_, Longitude, X, Y, measure))

# Filter out rows with missing coordinates
dat %<>% filter(!is.na(X) & !is.na(Y))

# Create a SpatialPointsDataFrame from the coordinates
xy <- sp::SpatialPointsDataFrame(
  coords = matrix(c(dat$X, dat$Y), ncol = 2),
  data = dat
)

# Define the path to the raster file
path <- "/Volumes/LaCie/pnas_climate_paper/1981-2010/bio/CHELSA_bio1_1981-2010_V.2.1.tif"

# Load the raster file
raster_file <- rast(path)

# Convert the SpatialPointsDataFrame to terra's vect format
xy_terra <- vect(xy)

# Assign the coordinate reference system (CRS) from the raster to the points
crs(xy_terra) <- crs(raster_file)

# Extract Climate Data -----------------------------------------------------
# Define the folder containing .tif files
folder_path <- "/Volumes/LaCie/pnas_climate_paper"

# List all .tif files in the directory and its subdirectories
tif_files <- list.files(
  path = folder_path,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)

# Initialize a list to store extracted values for each file, including row_unique
extracted_data <- list()

# Add the row_unique column as the first element to retain it in the final data
extracted_data[[1]] <- dat$row_unique

# Loop through each .tif file and extract data at the specified points
for (i in seq_along(tif_files)) {
  # Load the current raster file
  raster_file <- rast(tif_files[i])
  
  # Extract raster values at the spatial points
  extracted_values <- terra::extract(raster_file, xy_terra)
  
  # Store only the extracted values (second column) without file names
  extracted_data[[i + 1]] <- extracted_values[, 2]
}

# Combine all extracted values into a single dataframe
extracted_df <- as.data.frame(do.call(cbind, extracted_data))

# Assign column names: first column as "row_unique" and others as raster file names
colnames(extracted_df) <- c("row_unique", basename(tif_files))

# Clean the raster file names for clarity
clean_names <- basename(tif_files) %>%
  # Remove version and file extension
  gsub("_V.2.1\\.tif$", "", .) %>%
  # Remove 'CHELSA_' prefix
  gsub("^CHELSA_", "", .) %>%
  # Simplify model names by removing specific suffixes
  gsub("-esm[0-9a-zA-Z-]*", "", .) %>%
  gsub("-cm6a-lr", "", .) %>%
  gsub("-0-ll", "", .) %>%
  # Replace underscores with dots for consistency
  gsub("_", ".", .)

# Assign the cleaned names to the dataframe columns, excluding the first column
colnames(extracted_df)[-1] <- clean_names

# Optionally, merge the extracted climate data with the original dataset
dat %<>% left_join(extracted_df, by = "row_unique") %>%
  relocate(row_unique, .before = Reference_ID)

# Save the dataset with climate variables (optional)
write.csv(dat, here('data', 'dataset_with_climatologies_new.csv'), row.names = FALSE)

# Calculate Differences Between Baseline and Future ---------------------------
# Load the dataset with climatologies
extracted_df <- read.csv(here('data', 'dataset_with_climatologies_new.csv'))

# Identify the names of all bio1 and bio12 future columns using regex
future_bio1_cols <- grep("bio1\\.2041\\.2070", names(extracted_df), value = TRUE)
future_bio12_cols <- grep("bio12\\.2041\\.2070", names(extracted_df), value = TRUE)

# Function to calculate differences between future and baseline values
calculate_diff <- function(future_cols, baseline_col, data) {
  for (future_col in future_cols) {
    # Ensure both future and baseline columns are numeric
    data[[future_col]] <- as.numeric(data[[future_col]])
    data[[baseline_col]] <- as.numeric(data[[baseline_col]])
    
    # Create a new column name by replacing "2041.2070" with "diff"
    new_col_name <- gsub("2041\\.2070", "diff", future_col)
    
    # Calculate the difference and create the new column
    data <- data %>%
      mutate(!!new_col_name := !!sym(future_col) - !!sym(baseline_col))
    
    # Display a message confirming the creation of the new column
    message(paste("Created new column:", new_col_name))
  }
  
  return(data)  # Return the updated dataframe
}

# Calculate differences for bio1 variables
diff <- calculate_diff(
  future_cols = future_bio1_cols,
  baseline_col = "bio1.1981.2010",
  data = extracted_df
)

# Calculate differences for bio12 variables
diff <- calculate_diff(
  future_cols = future_bio12_cols,
  baseline_col = "bio12.1981.2010",
  data = diff
)

# Verify the number of columns to ensure correctness
if (dim(diff)[2] == 119) {
  print("Appropriate number of columns present.")
  calculated_differences <- diff
}


# Classify Differences ------------------------------------------------------

## Positive/Negative Proportions -------------------------------------------
# Identify the bio1.diff and bio12.diff columns
bio1_diff_cols <- grep("bio1\\.diff", names(calculated_differences), value = TRUE)
bio12_diff_cols <- grep("bio12\\.diff", names(calculated_differences), value = TRUE)

# Calculate proportions of positive and negative changes for bio1 and bio12
calculated_differences <- calculated_differences %>%
  rowwise() %>%
  mutate(
    # bio1 proportions
    bio1.positive.prop = sum(c_across(all_of(bio1_diff_cols)) > 0),
    bio1.negative.prop = sum(c_across(all_of(bio1_diff_cols)) < 0),
    
    # bio12 proportions
    bio12.positive.prop = sum(c_across(all_of(bio12_diff_cols)) >= 0),
    bio12.negative.prop = sum(c_across(all_of(bio12_diff_cols)) < 0)
  ) %>%
  ungroup()

# Display the first few rows of the updated dataframe
head(calculated_differences)

## Specific Ranges Classification ------------------------------------------
# Identify the bio1.diff and bio12.diff columns again (if necessary)
bio1_diff_cols <- grep("bio1\\.diff", names(calculated_differences), value = TRUE)
bio12_diff_cols <- grep("bio12\\.diff", names(calculated_differences), value = TRUE)

# Create new columns for specific thresholds in bio1 and bio12
calculated_differences <- calculated_differences %>%
  rowwise() %>%
  mutate(
    # bio1 specific thresholds
    bio1.greater_than_2 = sum(c_across(all_of(bio1_diff_cols)) > 2),
    bio1.greater_than_15 = sum(c_across(all_of(bio1_diff_cols)) > 1.5),
    bio1.greater_than_1 = sum(c_across(all_of(bio1_diff_cols)) > 1),
    bio1.greater_than_05 = sum(c_across(all_of(bio1_diff_cols)) > 0.5),
    bio1.less_than_05 = sum(c_across(all_of(bio1_diff_cols)) <= 0.5),
    bio1.less_than_1 = sum(c_across(all_of(bio1_diff_cols)) <= 1),
    bio1.less_than_15 = sum(c_across(all_of(bio1_diff_cols)) <= 1.5),
    bio1.less_than_2 = sum(c_across(all_of(bio1_diff_cols)) <= 2),
    
    # bio12 specific thresholds
    bio12.greater_than_100 = sum(c_across(all_of(bio12_diff_cols)) > 100),
    bio12.greater_than_50 = sum(c_across(all_of(bio12_diff_cols)) > 50),
    bio12.greater_than_25 = sum(c_across(all_of(bio12_diff_cols)) > 25),
    bio12.less_than_minus_100 = sum(c_across(all_of(bio12_diff_cols)) < -100),
    bio12.less_than_minus_50 = sum(c_across(all_of(bio12_diff_cols)) < -50),
    bio12.less_than_minus_25 = sum(c_across(all_of(bio12_diff_cols)) < -25),
    bio12.between_minus_50_and_50 = sum(
      c_across(all_of(bio12_diff_cols)) >= -50 & 
        c_across(all_of(bio12_diff_cols)) <= 50
    ),
    bio12.between_minus_100_and_100 = sum(
      c_across(all_of(bio12_diff_cols)) >= -100 & 
        c_across(all_of(bio12_diff_cols)) <= 100
    ),
    bio12.between_minus_25_and_25 = sum(
      c_across(all_of(bio12_diff_cols)) >= -25 & 
        c_across(all_of(bio12_diff_cols)) <= 25
    )
  ) %>%
  ungroup()

# Identify Dominant Change per Row ----------------------------------------
# For bio1 thresholds
calculated_differences <- calculated_differences %>%
  rowwise() %>%
  mutate(
    # Determine the most frequent category for bio1 > 2 vs. < 2
    bio1_most_frequent_2 = case_when(
      bio1.less_than_2 == max(bio1.greater_than_2, bio1.less_than_2) ~ "bio1 < 2",
      bio1.greater_than_2 == max(bio1.greater_than_2, bio1.less_than_2) ~ "bio1 > 2"
    )
  )

calculated_differences <- calculated_differences %>%
  rowwise() %>%
  mutate(
    # Determine the most frequent category for bio1 > 1.5 vs. < 1.5
    bio1_most_frequent_15 = case_when(
      bio1.less_than_15 == max(bio1.greater_than_15, bio1.less_than_15) ~ "bio1 < 1.5",
      bio1.greater_than_15 == max(bio1.greater_than_15, bio1.less_than_15) ~ "bio1 > 1.5"
    )
  )

calculated_differences <- calculated_differences %>%
  rowwise() %>%
  mutate(
    # Determine the most frequent category for bio1 > 1 vs. < 1
    bio1_most_frequent_1 = case_when(
      bio1.less_than_1 == max(bio1.greater_than_1, bio1.less_than_1) ~ "bio1 < 1",
      bio1.greater_than_1 == max(bio1.greater_than_1, bio1.less_than_1) ~ "bio1 > 1"
    )
  )

calculated_differences <- calculated_differences %>%
  rowwise() %>%
  mutate(
    # Determine the most frequent category for bio1 > 0.5 vs. < 0.5
    bio1_most_frequent_05 = case_when(
      bio1.less_than_05 == max(bio1.less_than_05, bio1.greater_than_05) ~ "bio1 < 0.5",
      bio1.greater_than_05 == max(bio1.less_than_05, bio1.greater_than_05) ~ "bio1 > 0.5"
    )
  )

# For bio12 thresholds
calculated_differences <- calculated_differences %>%
  rowwise() %>%
  mutate(
    # Determine the most frequent category for bio12 > 100, between -100 and 100, or < -100
    bio12_most_frequent_100cat = case_when(
      bio12.greater_than_100 == max(
        bio12.greater_than_100, 
        bio12.between_minus_100_and_100, 
        bio12.less_than_minus_100
      ) ~ "bio12 > 100",
      bio12.between_minus_100_and_100 == max(
        bio12.greater_than_100, 
        bio12.between_minus_100_and_100, 
        bio12.less_than_minus_100
      ) ~ "-100 < bio12 < 100",
      bio12.less_than_minus_100 == max(
        bio12.greater_than_100,
        bio12.between_minus_100_and_100,
        bio12.less_than_minus_100
      ) ~ "bio12 < -100"
    )
  )

calculated_differences <- calculated_differences %>%
  rowwise() %>%
  mutate(
    # Determine the most frequent category for bio12 > 50, between -50 and 50, or < -50
    bio12_most_frequent_50cat = case_when(
      bio12.greater_than_50 == max(
        bio12.greater_than_50, 
        bio12.between_minus_50_and_50, 
        bio12.less_than_minus_50
      ) ~ "bio12 > 50",
      bio12.between_minus_50_and_50 == max(
        bio12.greater_than_50, 
        bio12.between_minus_50_and_50,
        bio12.less_than_minus_50
      ) ~ "-50 < bio12 < 50",
      bio12.less_than_minus_50 == max(
        bio12.greater_than_50,
        bio12.between_minus_50_and_50,
        bio12.less_than_minus_50
      ) ~ "bio12 < -50"
    )
  )

calculated_differences <- calculated_differences %>%
  rowwise() %>%
  mutate(
    # Determine the most frequent category for bio12 > 25, between -25 and 25, or < -25
    bio12_most_frequent_25cat = case_when(
      bio12.greater_than_25 == max(
        bio12.greater_than_25, 
        bio12.between_minus_25_and_25, 
        bio12.less_than_minus_25
      ) ~ "bio12 > 25",
      bio12.between_minus_25_and_25 == max(
        bio12.greater_than_25, 
        bio12.between_minus_25_and_25,
        bio12.less_than_minus_25
      ) ~ "-25 < bio12 < 25",
      bio12.less_than_minus_25 == max(
        bio12.greater_than_25,
        bio12.between_minus_25_and_25,
        bio12.less_than_minus_25
      ) ~ "bio12 < -25"
    )
  )

# Classify Effects ----------------------------------------------------------
# Add a categorical effect based on the 'es' column
calculated_differences %<>%
  mutate(
    es_cat = case_when(
      es < -0.2 ~ 'Negative effect (g < -0.2)',
      abs(es) < 0.2 ~ 'No effect',
      abs(es) >= 0.2 ~ 'Positive effect (g > 0.2)'
    )
  )

# Save the Final Dataset ----------------------------------------------------
# Export the calculated differences with classifications to a CSV file
write.csv(calculated_differences, here('data', 'es_climvars_proportions_new.csv'), row.names = FALSE)
