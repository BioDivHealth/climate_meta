# dependencies
library(tidyverse)
library(here)
library(sp)
library(terra)

# Set up data -------------------------------------------
# load datasets
df = read.csv(here('data','dataset_final.csv'))
coords =  read.csv(here('data','data_final_coords.csv'))

# Combine datasets  -------------------------------------------
# change names of columns describign coordinates
coords %<>% rename("X" = "Longitude",
                   "Y" = "Latitude_")

# Perform a left join to add X and Y columns from coords to df
df_joined <- df %>%
  left_join(coords %>% select(X, Y, row_unique), 
            by = c("row_unique"))

# keep only unique rows
df_joined = unique(df_joined)

# save file with X Y coordinates columns
# write.csv(df_joined, here('data','data_with_coords.csv'), row.names = FALSE)

dat <- read.csv(here('data','data_with_coords.csv'))
dat %<>% filter(!is.na(X) & !is.na(Y))

xy <- sp::SpatialPointsDataFrame(matrix(c(dat$X, dat$Y), ncol = 2), dat)

path = ("/Users/arturtrebski/chelsa_cmip6/1981-2010/CHELSA_bio1_1981-2010_V.2.1.tif")

# Load the raster file
raster_file <- rast(path)

# Convert the sp::SpatialPointsDataFrame to terra format
xy_terra <- vect(xy)
crs(xy_terra) <- crs(raster_file)


# Extract climate data  -------------------------------------------
# List all .tif files from the directory recursively
folder_path <- "/Users/arturtrebski/chelsa_cmip6"
tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)

# Initialize a list to store extracted values for each file, including row_unique
extracted_data <- list()

# Add the row_unique column as the first "extracted" dataset, so it stays in the final data
extracted_data[[1]] <- dat$row_unique  # row_unique column to be included

# Loop through each .tif file and extract data
for (i in seq_along(tif_files)) {
  # Load the raster file
  raster_file <- rast(tif_files[i])
  
  # Extract values at xy points
  extracted_values <- extract(raster_file, xy_terra)
  
  # Store only the extracted values, without file names
  extracted_data[[i + 1]] <- extracted_values[, 2]  # Second column contains the extracted data
}

# Combine all extracted values into a single dataframe
extracted_df <- as.data.frame(do.call(cbind, extracted_data))

# Assign the row_unique column name and then the raster file names
colnames(extracted_df) <- c("row_unique", basename(tif_files))  # Add row_unique as the first column

# Now assign the file names as column names
file_names <- basename(tif_files)  # Get the file names without full path

# Clean the file names by removing ".tif" and other redundant parts
clean_names <- gsub("_V.2.1.tif", "", file_names)       # Remove version and file extension
clean_names <- gsub("CHELSA_", "", clean_names)         # Remove 'CHELSA_' prefix

# Simplify the model names (keep only the first part, like gfdl, ipsl, etc.)
clean_names <- gsub("-esm[0-9a-zA-Z-]*", "", clean_names)  # Remove parts like "-esm4", "-esm1-2-hr"
clean_names <- gsub("-cm6a-lr", "", clean_names)           # Remove "-cm6a-lr" from ipsl models
clean_names <- gsub("-0-ll", "", clean_names)              # Remove "-0-ll" from ukesm models

# Replace underscores with dots for clarity
clean_names <- gsub("_", ".", clean_names)
# Assign the cleaned names as the column names
colnames(extracted_df)[-1] <- clean_names

# Optionally, combine extracted_df with the original df if necessary
dat %<>% left_join(extracted_df, by = "row_unique")
dat %<>% relocate(row_unique, .before = Reference_ID)

# Save the dataset with climate variables
#write.csv(dat, here('data','dataset_with_climatologies.csv'), row.names = FALSE)

# Calculate differences between baseline and future ---------------------------
extracted_df = read.csv(here('data','dataset_with_climatologies.csv'))

# Get the names of all bio1 and bio12 future columns using regex
future_bio1_cols <- grep("bio1\\.2041.2070", names(extracted_df), value = TRUE)
future_bio12_cols <- grep("bio12\\.2041.2070", names(extracted_df), value = TRUE)

calculate_diff <- function(future_cols, baseline_col, extracted_df) {
  # Loop through each future column and compute the difference from the baseline
  for (future_col in future_cols) {
    # Ensure both the future and baseline columns are numeric
    extracted_df[[future_col]] <- as.numeric(extracted_df[[future_col]])
    extracted_df[[baseline_col]] <- as.numeric(extracted_df[[baseline_col]])
    
    # Generate a new column name by replacing "2041-2070" with "diff" in the future column name
    new_col_name <- gsub("2041.2070", "diff", future_col)
    
    # Use mutate to create the new column
    extracted_df <- extracted_df %>%
      mutate(!!new_col_name := !!sym(future_col) - !!sym(baseline_col))
    
    # Print a message to confirm the new column creation
    message(paste("Created new column:", new_col_name))
  }
  
  return(extracted_df)  # Return the modified dataframe
}

# Calculate differences for bio1
diff = calculate_diff(future_bio1_cols, "bio1.1981.2010", extracted_df=extracted_df)

# Calculate differences for bio12
diff = calculate_diff(future_bio12_cols, "bio12.1981.2010", diff)

if(dim(diff)[2]==119){
  print("Good amount of columns")
  calculated_differences = diff
}

names(calculated_differences)

# Means and SDs for bio1 -----------------------------------------
# Select only the relevant bio1 columns
bio1_columns <- grep("bio1.diff.*", names(calculated_differences), value = TRUE)

# Separate the bio1 columns for each SSP scenario
bio1_ssp126_cols <- grep("ssp126", bio1_columns, value = TRUE)
bio1_ssp370_cols <- grep("ssp370", bio1_columns, value = TRUE)
bio1_ssp585_cols <- grep("ssp585", bio1_columns, value = TRUE)

# Create a new dataset with row_unique and the calculated means and standard deviations
bio1_summary <- calculated_differences %>%
  select(row_unique, all_of(bio1_ssp126_cols), all_of(bio1_ssp370_cols), all_of(bio1_ssp585_cols)) %>%
  mutate(
    bio1_ssp126_mean = rowMeans(select(., all_of(bio1_ssp126_cols)), na.rm = TRUE),
    bio1_ssp126_sd = apply(select(., all_of(bio1_ssp126_cols)), 1, sd, na.rm = TRUE),
    
    bio1_ssp370_mean = rowMeans(select(., all_of(bio1_ssp370_cols)), na.rm = TRUE),
    bio1_ssp370_sd = apply(select(., all_of(bio1_ssp370_cols)), 1, sd, na.rm = TRUE),
    
    bio1_ssp585_mean = rowMeans(select(., all_of(bio1_ssp585_cols)), na.rm = TRUE),
    bio1_ssp585_sd = apply(select(., all_of(bio1_ssp585_cols)), 1, sd, na.rm = TRUE)
  ) %>%
  select(row_unique, bio1_ssp126_mean, bio1_ssp126_sd, bio1_ssp370_mean, bio1_ssp370_sd, bio1_ssp585_mean, bio1_ssp585_sd)

# View the first few rows of the summary dataset
head(bio1_summary)

# Optionally, save the bio1_summary dataset
write.csv(bio1_summary, here('data','bio1_summary.csv'), row.names = FALSE)

# Means and SDs for bio12 -----------------------------------------
# Select only the relevant bio12 columns
bio12_columns <- grep("bio12.diff.*", names(calculated_differences), value = TRUE)

# Separate the bio12 columns for each SSP scenario
bio12_ssp126_cols <- grep("ssp126", bio12_columns, value = TRUE)
bio12_ssp370_cols <- grep("ssp370", bio12_columns, value = TRUE)
bio12_ssp585_cols <- grep("ssp585", bio12_columns, value = TRUE)

# Create a new dataset with row_unique and the calculated means and standard deviations
bio12_summary <- calculated_differences %>%
  select(row_unique, all_of(bio12_ssp126_cols), all_of(bio12_ssp370_cols), all_of(bio12_ssp585_cols)) %>%
  mutate(
    bio12_ssp126_mean = rowMeans(select(., all_of(bio12_ssp126_cols)), na.rm = TRUE),
    bio12_ssp126_sd = apply(select(., all_of(bio12_ssp126_cols)), 1, sd, na.rm = TRUE),
    
    bio12_ssp370_mean = rowMeans(select(., all_of(bio12_ssp370_cols)), na.rm = TRUE),
    bio12_ssp370_sd = apply(select(., all_of(bio12_ssp370_cols)), 1, sd, na.rm = TRUE),
    
    bio12_ssp585_mean = rowMeans(select(., all_of(bio12_ssp585_cols)), na.rm = TRUE),
    bio12_ssp585_sd = apply(select(., all_of(bio12_ssp585_cols)), 1, sd, na.rm = TRUE)
  ) %>%
  select(row_unique, bio12_ssp126_mean, bio12_ssp126_sd, bio12_ssp370_mean, bio12_ssp370_sd, bio12_ssp585_mean, bio12_ssp585_sd)

# View the first few rows of the summary dataset
head(bio12_summary)

# Optionally, save the bio12_summary dataset
# write.csv(bio12_summary, here('data','bio12_summary.csv'), row.names = FALSE)

# Check for normality ----------------------------------------------------------

# Identify all bio1 and bio12 columns
bio_cols <- grep("bio1.diff|bio12.diff", names(calculated_differences), value = TRUE)

# Step 3: Visual Inspection for Each Column (Histograms and Q-Q Plots with Unique Values)
for (col in bio_cols) {
  # Select unique values for the column
  col_unique <- unique(calculated_differences[[col]])
  
  # Plot histogram for unique values
  hist(col_unique, main=paste("Histogram of", col), xlab=col, breaks=20)
  
  # Plot Q-Q plot for unique values
  qqnorm(col_unique, main=paste("Q-Q Plot of", col))
  qqline(col_unique, col="red")  # Add a reference line
  
  # Pause between plots for visual inspection
  readline(prompt = "Press [Enter] to continue to the next plot...")
}

  ## Combined plots ---------------
### Bio 1 
library(ggplot2)
library(gridExtra)

# Identify bio1 and bio12 columns
bio1_cols <- grep("bio1.diff", names(calculated_differences), value = TRUE)
bio12_cols <- grep("bio12.diff", names(calculated_differences), value = TRUE)

# Create lists to store ggplot objects
bio1_hist_list <- list()

# Loop through bio1 columns to create histograms
for (col in bio1_cols) {
  col_unique <- unique(calculated_differences[[col]])
  p <- ggplot(data.frame(x = col_unique), aes(x)) +
    geom_histogram(bins = 10, fill = "blue", alpha = 0.6) +
    ggtitle(paste("Histogram of", col)) +
    theme_minimal()
  bio1_hist_list <- c(bio1_hist_list, list(p))
}


# Get the range of the data across all bio1 columns for consistent x-axis limits
bio1_range <- range(unlist(lapply(bio1_cols, function(col) unique(calculated_differences[[col]]))), na.rm = TRUE)

# Create list to store bio1 histograms
bio1_hist_list <- list()

# Loop through bio1 columns to create histograms with consistent x-axis limits and bin width
for (col in bio1_cols) {
  col_unique <- unique(calculated_differences[[col]])
  p <- ggplot(data.frame(x = col_unique), aes(x)) +
    geom_histogram(bins = 20, fill = "blue", alpha = 0.6) +
    ggtitle(paste("Histogram of", col)) +
    xlim(bio1_range) +  # Fix the x-axis limits across all histograms
    theme_minimal()
  bio1_hist_list <- c(bio1_hist_list, list(p))
}

# Arrange all histograms in a grid
grid.arrange(grobs = bio1_hist_list, ncol = 3)

# Arrange all histograms in a grid
grid.arrange(grobs = bio1_hist_list, ncol = 3)

bio1_qq_list <- list()

# Loop through bio1 columns to create Q-Q plots
for (col in bio1_cols) {
  col_unique <- unique(calculated_differences[[col]])
  p <- ggplot(data.frame(sample = col_unique), aes(sample = sample)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    ggtitle(paste("Q-Q Plot of", col)) +
    theme_minimal()
  bio1_qq_list <- c(bio1_qq_list, list(p))
}

# Arrange all Q-Q plots in a grid
grid.arrange(grobs = bio1_qq_list, ncol = 3)

bio12_hist_list <- list()

# Loop through bio12 columns to create histograms
for (col in bio12_cols) {
  col_unique <- unique(calculated_differences[[col]])
  p <- ggplot(data.frame(x = col_unique), aes(x)) +
    geom_histogram(bins = 20, fill = "blue", alpha = 0.6) +
    ggtitle(paste("Histogram of", col)) +
    theme_minimal()
  bio12_hist_list <- c(bio12_hist_list, list(p))
}

# Arrange all histograms in a grid
grid.arrange(grobs = bio12_hist_list, ncol = 3)


bio12_qq_list <- list()

# Loop through bio12 columns to create Q-Q plots
for (col in bio12_cols) {
  col_unique <- unique(calculated_differences[[col]])
  p <- ggplot(data.frame(sample = col_unique), aes(sample = sample)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    ggtitle(paste("Q-Q Plot of", col)) +
    theme_minimal()
  bio12_qq_list <- c(bio12_qq_list, list(p))
}

# Arrange all Q-Q plots in a grid
grid.arrange(grobs = bio12_qq_list, ncol = 3)





  ## Tests --------------------------------------------------
# Store results in a dataframe
normality_results <- data.frame(Column = character(), P_Value = numeric(), stringsAsFactors = FALSE)

# Loop through each bio column and perform Shapiro-Wilk test
for (col in bio_cols) {
  print(col)
  col_unique <- unique(calculated_differences[[col]])
  # Perform Shapiro-Wilk test
  test_result <- shapiro.test(col_unique)
  
  # Store the results
  normality_results <- rbind(normality_results, data.frame(Column = col, P_Value = test_result$p.value))
}

# View the normality results
print(normality_results)
normality_results$P_Value>0.05
# Save the results if needed
# write.csv(normality_results, here('data', 'normality_results.csv'), row.names = FALSE)

# Classify differences ---------------------------

## Positive/negative -------
# Step 1: Identify the bio1.diff and bio12.diff columns
bio1_diff_cols <- grep("bio1.diff", names(calculated_differences), value = TRUE)
bio12_diff_cols <- grep("bio12.diff", names(calculated_differences), value = TRUE)

# Step 2: Create new columns for positive and negative proportions
calculated_differences <- calculated_differences %>%
  rowwise() %>%
  mutate(
    # bio1 proportions
    bio1.positive.prop = sum(c_across(all_of(bio1_diff_cols)) > 0),# / length(bio1_diff_cols),
    bio1.negative.prop = sum(c_across(all_of(bio1_diff_cols)) < 0),# / length(bio1_diff_cols),
    
    # bio12 proportions
    bio12.positive.prop = sum(c_across(all_of(bio12_diff_cols)) >= 0),# / length(bio12_diff_cols),
    bio12.negative.prop = sum(c_across(all_of(bio12_diff_cols)) < 0),# / length(bio12_diff_cols)
  ) %>%
  ungroup()

# Step 3: View the updated dataframe
head(calculated_differences)

## Specific ranges -----
# Step 1: Identify the bio1.diff and bio12.diff columns
bio1_diff_cols <- grep("bio1.diff", names(calculated_differences), value = TRUE)
bio12_diff_cols <- grep("bio12.diff", names(calculated_differences), value = TRUE)

# Step 2: Create new columns for deeper thresholds
calculated_differences <- calculated_differences %>%
  rowwise() %>%
  mutate(
    # bio1 specific thresholds
    bio1.greater_than_1 = sum(c_across(all_of(bio1_diff_cols)) >= 1), #/ length(bio1_diff_cols),
    bio1.less_than_1 = sum(c_across(all_of(bio1_diff_cols)) < 1),# / length(bio1_diff_cols),
    
    # bio12 specific thresholds
    bio12.greater_than_100 = sum(c_across(all_of(bio12_diff_cols)) > 100), # / length(bio12_diff_cols),
    bio12.between_minus_100_and_100 = sum(c_across(all_of(bio12_diff_cols)) > -100 & c_across(all_of(bio12_diff_cols)) < 100), #/ length(bio12_diff_cols),
    bio12.less_than_minus_100 = sum(c_across(all_of(bio12_diff_cols)) < -100)# / length(bio12_diff_cols)
  ) %>%
  ungroup()

# Step 3: View the updated dataframe with the new columns
head(calculated_differences)

.tmp = calculated_differences %>% select(row_unique, Data_ID, Reference_ID, Environmental_condition, bio1.1981.2010, bio12.1981.2010, bio1.positive.prop, bio1.negative.prop,bio1.greater_than_1, bio1.less_than_1, bio12.positive.prop, bio12.negative.prop, bio12.greater_than_100, bio12.between_minus_100_and_100, bio12.less_than_minus_100)

View(.tmp)

#write.csv(.tmp, here('data','climatology_proportions.csv'), row.names = FALSE)

# group effect sizes
df = calculated_differences %>%
  mutate(es_cat = ifelse(es < -0.2, 'Negative effect (g < -0.2)', #if CI contains 0
                         ifelse(abs(es) < 0.2, 'No effect', # otherwise negative
                                'Positive effect (g > 0.2)')) # or positive
  ) %>% filter(!is.na(es))

write.csv(df, here('data', 'es_climvars_proportions.csv'), row.names = FALSE)

# Temperature climate sensitivity
df_temp = df %>% filter(Environmental_condition=="Temperature" & !is.na(es))
cat("Nr of SSP x Climate model combinations (out of 15) predicting >1ºC increase")
table(df_temp$es_cat, df_temp$bio1.greater_than_1)
table(df_temp$es_cat, df_temp$bio1.less_than_1)

# Precipitation climate sensitivity
df_prec = df %>% filter(Environmental_condition=="Precipitation" & !is.na(es))
cat("Nr of SSP x Climate model combinations (out of 15) predicting \n>100mm increase in annual rainfall versus categories of effect\nsizes of precipitation climate sensitivity")
table(df_prec$es_cat, df_prec$bio12.greater_than_100)
table(df_prec$es_cat, df_prec$bio12.less_than_minus_100)
table(df_prec$es_cat, df_prec$bio12.between_minus_100_and_100)

