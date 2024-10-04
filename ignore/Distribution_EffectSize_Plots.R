# load dataset
df = read.csv(here('data','dataset_final.csv'))

# change vector names
df = df%>%
  mutate(Transmission_type = ifelse(Transmission_type == 'HVH', 'Vectored', 'Non-vectored'))

# add parasite group
df = df%>%
  mutate(Pathogen = ifelse(Pathogen == 'P', 'Parasite',
                           ifelse(Pathogen == 'V', 'Virus',
                                  'Bacteria')))
# keep only unique rows
df = unique(df)

table(df$Disease)

# Subsetting data for HFRS (or other diseases with sufficient effect sizes)
hfrs_data <- df[df$Disease == "Haemorrhagic fever with renal syndrome",]
hfrs_data %<>% filter(!is.na(es))
# Summary statistics for Hedge's g (effect sizes)
summary(hfrs_data$es)
sd(hfrs_data$es)  # Standard deviation for effect sizes

# Create a boxplot or violin plot for HFRS effect sizes
library(ggplot2)
ggplot(hfrs_data, aes(x = Disease, y = es)) +
  geom_violin(fill = "skyblue") +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(title = "Distribution of Effect Sizes for HFRS", y = "Hedge's g (Effect Size)", x = "HFRS")

# Calculate I² for heterogeneity (requires metafor package)
library(metafor)
res <- rma(yi = es, data = hfrs_data)
print(res$I2)  # Heterogeneity index


#combined plto --------------
##Violin Plots ---------------
# Read the data from your CSV file
# Replace 'your_file.csv' with the path to your CSV file
data <- df %>% filter(!is.na(es)) %>% filter(Environmental_condition=="Precipitation")

# Ensure the columns are named correctly: 'Disease' and 'es'
# Filter diseases with more than 10 data points
filtered_data <- data %>% filter(!is.na(es)) %>% 
  group_by(Disease) %>%
  filter(n() > 10)

# Create the violin plot
ggplot(filtered_data, aes(x = Disease, y = es)) +
  geom_violin(fill = "skyblue", scale = "width", trim = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.5, color = "darkblue") +
  labs(title = "Distribution of Effect Sizes (Hedge's g) for Diseases with >10 Data Points",
       x = "Disease", y = "Hedge's g (Effect Size)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Boxplots --------------------------------------------------
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(viridis)

# Read the data from your CSV file
# Replace 'your_file.csv' with the path to your CSV file
data <- df %>% filter(!is.na(es))# %>% filter(Environmental_condition=="Humidity")

# Group and summarize the data to count the number of data points for each disease
data_summary <- data %>%
  group_by(Disease) %>%
  summarise(count = n(), mean_es = mean(es, na.rm = TRUE)) %>%
  filter(count > 10)  # Filter diseases with more than 10 data points

# Merge the data back with the original data
filtered_data <- merge(data, data_summary, by = "Disease")

# Create a new label for each disease with the number of data points
filtered_data$Disease <- paste0(filtered_data$Disease, " (n=", filtered_data$count, ")")

# Order the diseases by the number of data points
filtered_data$Disease <- reorder(filtered_data$Disease, filtered_data$count)

# Create the boxplot with the custom color palette
ggplot(filtered_data, aes(x = Disease, y = es, fill = Disease)) +
  geom_violin(outlier.shape = NA) +  # Avoid plotting outliers
  geom_jitter(width = 0.1, alpha = 0.5, color = "darkblue") +
  scale_fill_viridis(discrete = TRUE, alpha = 0.4) +
  labs(title = "Distribution of Effect Sizes (Hedge's g) for Diseases with >10 Data Points (FULL DATASET:)",
       x = "", y = "Hedge's g (Effect Size)") +
  theme_minimal() +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.x = element_blank(),  # Remove x-axis labels
        axis.ticks.x = element_blank(),  # Remove x-axis ticks
        panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 11))  # Remove x-axis grid lines)
