# =========== GENERATE FIGURE S6 ==========

#Artur Trebski
#10 Oct 2024

# Libraries ------
library(tidyverse)
library(here)
library(knitr)
library(magrittr)
library(kableExtra)  
library(ggplot2)
library(patchwork)

# Data setup -------
# Read in the data
tmp = read.csv(here('data', 'es_climvars_proportions_new.csv'))

# Ensure 'es_cat' is a factor with ordered levels
tmp <- tmp %>%
  mutate(es_cat = factor(es_cat,
                         levels = c('Negative effect (g < -0.2)', 'Positive effect (g > 0.2)'),
                         ordered = TRUE))

base_font = 4

tmp %<>% filter(es_cat!="No effect")

# Subset the data based on Environmental_condition
df_temp <- tmp %>%
  filter(Environmental_condition == "Temperature" & !is.na(es_cat))

df_prec <- tmp %>%
  filter(Environmental_condition == "Precipitation" & !is.na(es_cat))

# Rename values
# Modify the bio_dif column values directly in df_temp and df_prec
# Temperature categories modifications directly on df_temp
# Modify for > 2ºC and < 2ºC
df_temp$bio1_most_frequent_2[df_temp$bio1_most_frequent_2 == "bio1 > 2"] <- "> +2ºC"
df_temp$bio1_most_frequent_2[df_temp$bio1_most_frequent_2 == "bio1 < 2"] <- "< +2ºC"

# Modify for > 1.5ºC and < 1.5ºC
df_temp$bio1_most_frequent_15[df_temp$bio1_most_frequent_15 == "bio1 > 1.5"] <- "> +1.5ºC"
df_temp$bio1_most_frequent_15[df_temp$bio1_most_frequent_15 == "bio1 < 1.5"] <- "< +1.5ºC"

# Modify for > 1ºC and < 1ºC
df_temp$bio1_most_frequent_1[df_temp$bio1_most_frequent_1 == "bio1 > 1"] <- "> +1ºC"
df_temp$bio1_most_frequent_1[df_temp$bio1_most_frequent_1 == "bio1 < 1"] <- "< +1ºC"

# Modify for > 0.5ºC and < 0.5ºC
df_temp$bio1_most_frequent_05[df_temp$bio1_most_frequent_05 == "bio1 > 0.5"] <- "> +0.5ºC"
df_temp$bio1_most_frequent_05[df_temp$bio1_most_frequent_05 == "bio1 < 0.5"] <- "< +0.5ºC"

# Precipitation categories modifications directly on df_prec
# Modify for < -100mm and > +100mm
df_prec$bio12_most_frequent_100cat[df_prec$bio12_most_frequent_100cat == "bio12 < -100"] <- "< -100mm"
df_prec$bio12_most_frequent_100cat[df_prec$bio12_most_frequent_100cat == "bio12 > 100"] <- "> +100mm"
df_prec$bio12_most_frequent_100cat[df_prec$bio12_most_frequent_100cat == "-100 < bio12 < 100"] <- "±100mm Range"

# Modify for < -50mm and > +50mm
df_prec$bio12_most_frequent_50cat[df_prec$bio12_most_frequent_50cat == "bio12 < -50"] <- "< -50mm"
df_prec$bio12_most_frequent_50cat[df_prec$bio12_most_frequent_50cat == "bio12 > 50"] <- "> +50mm"
df_prec$bio12_most_frequent_50cat[df_prec$bio12_most_frequent_50cat == "-50 < bio12 < 50"] <- "±50mm Range"

# Modify for < -25mm and > +25mm
df_prec$bio12_most_frequent_25cat[df_prec$bio12_most_frequent_25cat == "bio12 < -25"] <- "< -25mm"
df_prec$bio12_most_frequent_25cat[df_prec$bio12_most_frequent_25cat == "bio12 > 25"] <- "> +25mm"
df_prec$bio12_most_frequent_25cat[df_prec$bio12_most_frequent_25cat == "-25 < bio12 < 25"] <- "±25mm Range"

df_prec %<>% dplyr::select(row_unique, es_cat, bio12_most_frequent_100cat, bio12_most_frequent_50cat, bio12_most_frequent_25cat)
df_temp %<>% dplyr::select(row_unique, es_cat, bio1_most_frequent_05, bio1_most_frequent_1, bio1_most_frequent_15, bio1_most_frequent_2)

#write.csv(df_prec, "df_prec.csv", row.names = FALSE)
#write.csv(df_temp, "df_temp.csv", row.names = FALSE)
levels(df_temp$bio1_most_frequent_1) = c("< +1ºC","> +1ºC")
levels(df_temp$bio1_most_frequent_15) = c("< +1.5ºC","> +1.5ºC")
levels(df_temp$bio1_most_frequent_2) = c("< +2ºC","> +2ºC")

# BIO1 - Create custom facet heatmap function ------------------------

draw_facet_heatmap <- function(data, column_names, titles, low_color = "ivory2", high_color = "tomato3", mid_color = "white", output_file = NULL) {
  # Combine data from multiple columns into one data frame for faceting
  heat_data_list <- lapply(seq_along(column_names), function(i) {
    column <- column_names[i]
    heat_data <- table(data[[column]], data$es_cat)
    heat_data_df <- as.data.frame(as.table(heat_data))
    colnames(heat_data_df) <- c("Temperature_Category", "Effect_Size", "Count")
    heat_data_df$Category_Type <- titles[i]
    heat_data_df <- heat_data_df[heat_data_df$Temperature_Category %in% levels(data[[column]]), ]  # Filter to include only the relevant temperature categories
    return(heat_data_df)
  })
  
  combined_heat_data <- do.call(rbind, heat_data_list)
  
  # Add missing <1ºC category for the relevant facet
  combined_heat_data$Temperature_Category <- as.character(combined_heat_data$Temperature_Category)
  
  if (!"< +1ºC" %in% combined_heat_data$Temperature_Category) {
    
    missing_row <- data.frame(Temperature_Category = "< +1ºC",
                              Effect_Size = rep(unique(combined_heat_data$Effect_Size), each = 2),
                              Count = 0,
                              Category_Type = titles[3])
    combined_heat_data <- rbind(combined_heat_data, missing_row)
  }
  combined_heat_data$Temperature_Category <- factor(combined_heat_data$Temperature_Category,
                                                    levels = c("< +1ºC", "> +1ºC",
                                                               "< +1.5ºC", "> +1.5ºC",
                                                               "< +2ºC", "> +2ºC"))
  
  combined_heat_data$Category_Type <- factor(combined_heat_data$Category_Type, levels = rev(titles))
  
  # Fill in missing combinations with Count = 0 for each facet separately
  combined_heat_data <- do.call(rbind, lapply(split(combined_heat_data,
                                                    combined_heat_data$Category_Type), function(tmp) {
    all_combinations <- expand.grid(
      Temperature_Category = unique(tmp$Temperature_Category),
      Effect_Size = unique(tmp$Effect_Size),
      Category_Type = unique(tmp$Category_Type)
    )
    tmp <- merge(all_combinations, tmp, by = c("Temperature_Category",
                                             "Effect_Size",
                                             "Category_Type"), all.x = TRUE)
    tmp$Count[is.na(tmp$Count)] <- 0
    return(tmp)
  }))
  
  # Create the heatmap using facet_wrap
  p <- ggplot(combined_heat_data, aes(x = Temperature_Category, y = Effect_Size, fill = Count)) +
    geom_tile(color = "ivory4") +
    geom_text(aes(label = Count), color = "black", size = 1.5, alpha = 0.9) +  # Add text labels for the count values
    scale_fill_gradientn(colors = c(mid_color, low_color, high_color),
                         values = scales::rescale(c(0, 1, max(combined_heat_data$Count))),
                         limits = c(0, max(combined_heat_data$Count)),
                         na.value = "white") +
    theme_minimal() +
    # theme(axis.text.x = element_text(angle = 30, hjust = 0.6, size = base_font, vjust = 0.9),
    #       strip.text = element_text(size = base_font, face = "bold"),
    #       axis.text.y = element_text(angle = 0, size = base_font, face = "bold"),
    #       axis.title.y = element_text(size = base_font),
    #       axis.title.x = element_text(size = base_font)) +
    labs(x = "Temperature change categories", y = "Effect Size", fill = "Count") +
    facet_wrap(~ Category_Type, ncol = 3, scales = "free_x")
  
  # Save the plot if output_file is specified
  if (!is.null(output_file)) {
    ggsave(output_file, plot = p, width = 15, height = 5, units = "in")
    print(p)
  } else {
    return(p)
  }
}

column_names_temp <- c("bio1_most_frequent_2", "bio1_most_frequent_15", "bio1_most_frequent_1")
titles_temp <- c("2ºC Threshold",
            "1.5ºC Threshold",
            "1ºC Threshold")

# BIO12 facet heatmap function ---------

# Create custom facet heatmap function for precipitation data
draw_facet_heatmap_prec <- function(data, column_names, titles, low_color = "ivory2", high_color = "dodgerblue4", mid_color = "white", include_mid_range = TRUE, output_file = NULL) {
  # Combine data from multiple columns into one data frame for faceting
  heat_data_list <- lapply(seq_along(column_names), function(i) {
    column <- column_names[i]
    heat_data <- table(data[[column]], data$es_cat)
    heat_data_df <- as.data.frame((heat_data))
    colnames(heat_data_df) <- c("Precipitation_Category", "Effect_Size", "Count")
    heat_data_df$Category_Type <- titles[i]
    return(heat_data_df)
  })
  
  combined_heat_data <- do.call(rbind, heat_data_list)
  
  # Add missing categories for the relevant facets
  combined_heat_data$Precipitation_Category <- as.character(combined_heat_data$Precipitation_Category)
  
  # Set factor levels based on include_mid_range argument
  if (include_mid_range) {
    combined_heat_data$Precipitation_Category <- factor(combined_heat_data$Precipitation_Category,
                                                        levels = c( "< -100mm","±100mm Range","> +100mm",
                                                                   "< -50mm","±50mm Range","> +50mm",
                                                                   "< -25mm","±25mm Range","> +25mm"))
  } else {
    combined_heat_data <- combined_heat_data[!combined_heat_data$Precipitation_Category %in% c("±100mm Range", "±50mm Range", "±25mm Range"), ]
    combined_heat_data$Precipitation_Category <- factor(combined_heat_data$Precipitation_Category,
                                                        levels = c("< -100mm","> +100mm", 
                                                                   "< -50mm","> +50mm", 
                                                                   "< -25mm","> +25mm"))
  }
  
  combined_heat_data$Category_Type <- factor(combined_heat_data$Category_Type, levels = rev(titles))
  
  # Fill in missing combinations with Count = 0 for each facet separately
  combined_heat_data <- do.call(rbind, lapply(split(combined_heat_data,
                                                    combined_heat_data$Category_Type), function(tmp) {
                                                      all_combinations <- expand.grid(
                                                        Precipitation_Category = unique(tmp$Precipitation_Category),
                                                        Effect_Size = unique(tmp$Effect_Size),
                                                        Category_Type = unique(tmp$Category_Type)
                                                      )
                                                      tmp <- merge(all_combinations, tmp, by = c("Precipitation_Category",
                                                                                               "Effect_Size",
                                                                                               "Category_Type"), all.x = TRUE)
                                                      tmp$Count[is.na(tmp$Count)] <- 0
                                                      return(tmp)
                                                    }))
  
  # Create the heatmap using facet_wrap
  p <- ggplot(combined_heat_data, aes(x = Precipitation_Category, y = Effect_Size, fill = Count)) +
    geom_tile(color = "ivory4") +
    geom_text(aes(label = Count), color = "black", size = 1.5, alpha = 1) +  # Add text labels for the count values
    scale_fill_gradientn(colors = c(mid_color, low_color, high_color),
                         values = scales::rescale(c(0, 1, max(combined_heat_data$Count))),
                         limits = c(0, max(combined_heat_data$Count)),
                         na.value = "white") +
    theme_minimal() +
    # theme(axis.text.x = element_text(angle = 30, hjust = 0.6, size = base_font, vjust = 0.9),
    #       strip.text = element_text(size = base_font, face = "bold"),
    #       axis.text.y = element_text(angle = 0, size = base_font, face = "bold"),
    #       axis.title.y = element_text(size = base_font),
    #       axis.title.x = element_text(size = base_font)) +
    labs(x = "Precipitation change categories", y = "Effect Size", fill = "Count") +
    facet_wrap(~ Category_Type, ncol = 3, scales = "free_x")
  
  # Save the plot if output_file is specified
  if (!is.null(output_file)) {
    ggsave(output_file, plot = p, width = 15, height = 5, units = "in")
    print(p)
  } else {
    return(p)
  }
}

column_names_prec <- c("bio12_most_frequent_100cat", "bio12_most_frequent_50cat", "bio12_most_frequent_25cat")
titles_prec <- c("100mm Threshold",
            "50mm Threshold",
            "25mm Threshold")

# Combined plot for supplementary figure ----------------------------------------

plot_temp = draw_facet_heatmap(df_temp, column_names_temp, titles_temp)
plot_temp = plot_temp + guides(fill = guide_colorbar(
  title = "Count", title.position = "top",
  barwidth = grid::unit(0.25, "cm"), barheight = grid::unit(1.5, "cm"),
  ticks = TRUE, label.position = "right")) + theme(axis.text.x = element_text(angle = 30, hjust = 0.6, size = base_font, vjust = 0.9),
                                                        strip.text = element_text(size = base_font+1, face = "bold"),
                                                        axis.text.y = element_text(angle = 0, size = base_font+1, face = "bold"),
                                                        axis.title.y = element_text(size = base_font+1),
                                                        axis.title.x = element_text(size = base_font+1),
                                                        legend.title = element_text(size = 3, face = "bold"),
                                                        legend.text  = element_text(size = 3))

plot_temp
plot_prec = draw_facet_heatmap_prec(df_prec, column_names_prec, titles_prec)
plot_prec = plot_prec + guides(fill = guide_colorbar(
  title = "Count", title.position = "top",
  barwidth = grid::unit(0.25, "cm"), barheight = grid::unit(1.5, "cm"),
  ticks = TRUE, label.position = "right"))  + theme(axis.text.x = element_text(angle = 30, hjust = 0.6, size = base_font, vjust = 0.9),
                                                    strip.text = element_text(size = base_font+1, face = "bold"),
                                                    axis.text.y = element_text(angle = 0, size = base_font+1, face = "bold"),
                                                    axis.title.y = element_text(size = base_font+1),
                                                    axis.title.x = element_text(size = base_font+1),
                                                    legend.title = element_text(size = 3, face = "bold"),
                                                    legend.text  = element_text(size = 3))


library(gridExtra)

combined_plot_S6 <- plot_temp /  plot_prec + 
  plot_layout(nrow = 2, heights = c(1, 1)) #& theme(legend.position = "right", legend.direction = "vertical",
                                                                    #         legend.title = element_text(size = 5, face = "bold"),
                                                                    #         legend.text  = element_text(size = 5))  # Add spacer in the middle and adjust heights

print(combined_plot_S6)
ggsave(combined_plot_S6, file=here('outputs', 'final_figs_new', 'Figure_4_new.pdf'), device="pdf", units="cm", width=10, height=7, dpi=1000, scale=1)
