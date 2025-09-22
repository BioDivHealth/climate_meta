# =========== GENERATE COMBINED FIGURE (Fig. 3 + Fig. 4) ==========

# dependencies:
library(ggplot2)
library(dplyr)
library(patchwork)
library(ggridges)
library(here)

# -------------------- Load & prepare data --------------------
df <- read.csv(here::here("data", "dataset_final_g.csv"))
df <- df %>% filter(!is.na(es))

# Ensure consistent facet order: Temperature, Precipitation, Humidity
df$Environmental_condition <- factor(
  df$Environmental_condition,
  levels = c("Temperature", "Precipitation", "Humidity")
)

# Harmonise transmission names
# df <- df %>%
#   mutate(Transmission_type = ifelse(Transmission_type == "HVH", "Vectored", "Non-vectored"))
# 
# df$Transmission_type <- factor(df$Transmission_type, levels = c("Non-vectored", "Vectored"))

# Shorten disease label used in figures
df$Disease[df$Disease == "Haemorrhagic fever with renal syndrome"] <- "HFRS"

# Keep only diseases with > 10 data points per environmental factor (as in Fig_4_fin)
df4 <- df

temp <- df4 %>% filter(Environmental_condition == "Temperature")
points_temp <- temp %>% group_by(Disease) %>% summarise(count = n(), .groups = "drop") %>% filter(count > 15)
temp <- merge(temp, points_temp, by = "Disease")

prec <- df4 %>% filter(Environmental_condition == "Precipitation")
points_prec <- prec %>% group_by(Disease) %>% summarise(count = n(), .groups = "drop") %>% filter(count > 15)
prec <- merge(prec, points_prec, by = "Disease")

hum <- df4 %>% filter(Environmental_condition == "Humidity")
points_hum <- hum %>% group_by(Disease) %>% summarise(count = n(), .groups = "drop") %>% filter(count > 15)
hum <- merge(hum, points_hum, by = "Disease")

df_disease <- rbind(temp, prec, hum)

# Apply the same ordering to the combined disease dataset
df_disease$Environmental_condition <- factor(
  df_disease$Environmental_condition,
  levels = c("Temperature", "Precipitation", "Humidity")
)


# -------------------- Palettes --------------------
cols_transmission <- c("#A80003", "#00A8A5")
cols_disease <- c("#a80003", "#a3391f", "#9c5439", "#718b7b", "#609589", "#469e97", "#00a8a5")


# -------------------- Helper transforms --------------------
# make_rank_transmission <- function(data) {
#   data %>%
#     mutate(Environmental_condition = factor(
#       Environmental_condition,
#       levels = c("Temperature", "Precipitation", "Humidity")
#     )) %>%
#     group_by(Environmental_condition, Transmission_type) %>%
#     mutate(esrank = rank(es, ties.method = "random")) %>%
#     ungroup() %>%
#     mutate(esrank = ifelse(Transmission_type == "Non-vectored", esrank + 100, esrank))
# }

# Fixed-height bands for transmission (Vectored vs Non-vectored)
make_rank_transmission <- function(data, band_height = 20, gap = 5) {
  env_shift <- function(ec) ifelse(ec == "Precipitation", 5,
                                   ifelse(ec == "Humidity", 12, 0))
  
  data %>%
    mutate(
      Environmental_condition = factor(
        Environmental_condition,
        levels = c("Temperature", "Precipitation", "Humidity")
      ),
      # keep colour mapping consistent: Non-vectored (red), Vectored (teal)
      Transmission_type = factor(Transmission_type,
                                 levels = c("Non-vectored", "Vectored")),
      # band order: Vectored on top, Non-vectored below (y is reversed later)
      band_id = ifelse(Transmission_type == "Non-vectored", 1, 0),
      y0 = band_id * (band_height + gap)
    ) %>%
    group_by(Environmental_condition, Transmission_type) %>%
    arrange(es, .by_group = TRUE) %>%
    mutate(
      r   = rank(es, ties.method = "random"),
      n   = n(),
      r01 = ifelse(n > 1, (r - 1) / (n - 1), 0.5),   # 0..1 within the band
      esrank = y0 + r01 * band_height
    ) %>%
    ungroup() %>%
    mutate(esrank = esrank + env_shift(Environmental_condition))
}

make_rank_disease <- function(data, band_height = 20, gap = 8) {
  order <- rev(c("HFRS","Brucellosis","Leptospirosis","Scrub typhus",
             "Japanese encephalitis","Tick-borne encephalitis","West Nile Virus"))
  
  env_shift <- function(ec) ifelse(ec == "Precipitation", 5,
                                   ifelse(ec == "Humidity", 12, 0))
  
  data %>%
    mutate(
      Environmental_condition = factor(Environmental_condition,
                                       levels = c("Temperature","Precipitation","Humidity")),
      Disease = factor(Disease, levels = order),
      band_id = as.integer(Disease) - 1,                 # 0..6
      y0 = band_id * (band_height + gap)                 # band start
    ) %>%
    group_by(Environmental_condition, Disease) %>%
    arrange(es, .by_group = TRUE) %>%
    mutate(
      r  = rank(es, ties.method = "random"),
      n  = n(),
      r01 = ifelse(n > 1, (r - 1) / (n - 1), 0.5),       # 0..1 in the band
      esrank = y0 + r01 * band_height                    # scaled rank inside band
    ) %>%
    ungroup() %>%
    mutate(esrank = esrank + env_shift(Environmental_condition))
}

# make_rank_disease <- function(data) {
#   custom_order <- c(
#     "HFRS",
#     "Brucellosis",
#     "Leptospirosis",
#     "Scrub typhus",
#     "Japanese encephalitis",
#     "Tick-borne encephalitis",
#     "West Nile Virus"
#   )
# 
#   disease_offsets <- c(
#     "HFRS" = 140,
#     "Brucellosis" = 115,
#     "Leptospirosis" = 90,
#     "Scrub typhus" = 70,
#     "Japanese encephalitis" = 50,
#     "Tick-borne encephalitis" = 24,
#     "West Nile Virus" = 0
#   )
# 
#   data %>%
#     mutate(Environmental_condition = factor(
#       Environmental_condition,
#       levels = c("Temperature", "Precipitation", "Humidity")
#     )) %>%
#     mutate(Disease = factor(Disease, levels = custom_order)) %>%
#     group_by(Environmental_condition, Disease) %>%
#     arrange(es, .by_group = TRUE) %>%
#     mutate(esrank = rank(es, ties.method = "random")) %>%
#     ungroup() %>%
#     mutate(
#       esrank = esrank + ifelse(Disease %in% names(disease_offsets), disease_offsets[Disease], 0) +
#         ifelse(Environmental_condition == "Precipitation", 5,
#                ifelse(Environmental_condition == "Humidity", 12, 0))
#     )
# }


# -------------------- Plot builders --------------------
build_A1_ridges_transmission <- function(dd) {
  dd %>%
    filter(Linear_Nonlinear == "Linear") %>%
    ggplot(aes(x = es, y = Transmission_type, fill = Transmission_type, color = Transmission_type)) +
    geom_density_ridges(scale = 6, linewidth = 0.1) +
    geom_vline(xintercept = 0, linetype = 2) +
    scale_fill_manual(values = scales::alpha(cols_transmission, 0.7)) +
    scale_color_manual(values = cols_transmission) +
    facet_grid(~Environmental_condition) +
    labs(x = "", y = "") +
    xlim(-3, 3) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      axis.line.x = element_blank(),
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
      plot.tag.position = c(0.055, 0.95),
      plot.tag = element_text(face = "bold"),
      strip.text = ggtext::element_markdown(size = 5.5, hjust = 0.5, margin = margin(b = 1), face = "bold")
    ) +
    labs(tag = "A")
}

build_A2_ridges_disease <- function(dd) {
  
  ord <- c("HFRS","Brucellosis","Leptospirosis","Scrub typhus",
           "Japanese encephalitis","Tick-borne encephalitis","West Nile Virus")
  dd %>%
    filter(Linear_Nonlinear == "Linear") %>%
    mutate(Disease = factor(Disease, levels = ord)) %>%   # WNV last -> top
    ggplot(aes(x = es, y = Disease, fill = Disease, color = Disease)) +
    geom_density_ridges(scale = 2.5, linewidth = 0.1) +
    geom_vline(xintercept = 0, linetype = 2) +
    scale_fill_manual(values = scales::alpha(cols_disease, 0.7)) +
    scale_color_manual(values = cols_disease) +
    facet_grid(~Environmental_condition) +
    labs(x = "Hedges' *g* Effect Size", y = "") +
    xlim(-3, 3) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      axis.line.x = element_line(),
      legend.position = "none",
      axis.ticks.y = element_blank(),
      plot.tag.position = c(0.055, 0.95),
      plot.tag = element_text(face = "bold"),
      strip.text = element_blank(),
      axis.title.x = ggtext::element_markdown(margin = margin(t = 2))
    ) + labs(tag = "B")
}


build_B1_snake_transmission <- function(dd, band_height = 20, gap = 5) {
  band_map <- data.frame(
    Transmission_type = factor(c("Non-vectored", "Vectored"),
                               levels = c("Non-vectored", "Vectored")),
    band_id = c(1, 0)
  ) |>
    mutate(
      y0  = band_id * (band_height + gap),
      esrank = y0 + band_height/2 - gap/2,   # label position within band
      es  = -2.5,                          # left margin x-position for labels
      Environmental_condition = factor("Temperature",
                                       levels = c("Temperature","Precipitation","Humidity")),
      lab = as.character(Transmission_type)
    )
  
  dd %>%
    filter(Linear_Nonlinear == "Linear") %>%
    ggplot(aes(x = es, y = esrank, col = Transmission_type)) +
    geom_point(shape = 16, alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_errorbarh(aes(xmin = es - se, xmax = es + se, color = Transmission_type),
                   height = 0, show.legend = FALSE, size = 0.20, alpha = 0.3) +
    scale_color_manual(values = cols_transmission, name = "Transmission Type") +
    # ---- FIXED: give x and y explicitly ----
    geom_text(data = band_map,
            aes(x = es, y = esrank, label = lab),
            color = "black", hjust = 0, alpha = 0.5, inherit.aes = FALSE, size = 1.6) +
    facet_grid(~Environmental_condition) +
    labs(x = "", y = "") +
    scale_x_continuous(position = "bottom", limits = c(-2.5, 2.5)) +
    scale_y_reverse() +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      axis.line.x = element_blank(),
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.text = element_blank(),
      plot.tag.position = c(0.055, 0.95),
      plot.tag = element_text(face = "bold"),
      axis.title.y = element_text(vjust = -10)
    ) +
    labs(tag = "C")
}

build_B2_snake_disease <- function(dd, band_height = 20, gap = 8) {
  ord <- levels(dd$Disease)
  ann_text <- data.frame(
    es = -2.5,
    Disease = ord,
    Environmental_condition = factor("Temperature",
                                     levels = c("Temperature","Precipitation","Humidity")),
    esrank = ((seq_along(ord) - 1) * (band_height + gap)) + band_height - gap/2,
    lab = ord
  )
  
  dd %>%
    filter(Linear_Nonlinear == "Linear") %>%
    ggplot(aes(x = es, y = esrank, col = Disease)) +
    geom_point(shape = 16, alpha = 0.5) +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_errorbarh(aes(xmin = es - se, xmax = es + se, color = Disease),
                   height = 0, show.legend = FALSE, size = 0.25, alpha = 0.3) +
    scale_color_manual(values = rev(cols_disease)) +
    geom_text(data = ann_text, aes(label = lab, col = Disease),
              vjust = -0.5, hjust = 0, alpha = 0.9, size = 1.6) +
    facet_grid(~Environmental_condition) +
    labs(x = "Hedges' *g* Effect Size", y = "Effect size rank") +
    scale_x_continuous(position = "bottom", limits = c(-2.5, 2.5)) +
    scale_y_reverse() +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      strip.background = element_blank(),
      axis.line.x = element_line(),
      legend.position = "none",
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      strip.text = element_blank(),
      plot.tag.position = c(0.055, 0.95),
      plot.tag = element_text(face = "bold"),
      axis.title.y = element_text(vjust = -35, hjust = 0.9),
      axis.title.x = ggtext::element_markdown(margin = margin(t = 2))
    ) + labs(tag = "D")
}

# ─────────────────────────────────────────────────────────
# 1. GLOBAL THEME  (fonts & spacing in points/mm)
# ─────────────────────────────────────────────────────────
font_base <- 8.5

theme_set(
  theme_minimal(base_size = font_base) +
    theme(
      axis.title       = element_text(size = font_base - 1),
      axis.text        = element_text(size = font_base - 2.5),
      axis.title.y = element_text(size = font_base - 2),
      axis.title.x = element_text(size = font_base - 2),
      strip.text       = element_text(size = font_base - 1, face = "bold"),
      plot.tag         = element_text(size = font_base - 1, face = "bold"),
      panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks.x     = element_line(size = 0.25),
      axis.ticks       = element_line(size = 0.25),
      axis.ticks.length= unit(0.5, "mm"),
      plot.margin      = margin(1, 1, 1, 1, unit = "mm")
    )
)

# Global geom defaults similar to the originals
update_geom_defaults("point",     list(size = 0.6))
update_geom_defaults("text",      list(size = 1.25))
update_geom_defaults("line",      list(size = 0.1))
update_geom_defaults("vline",     list(linewidth = 0.2))
update_geom_defaults("segment",   list(size = 0.2))
update_geom_defaults("errorbarh", list(size = 0.03))


# ─────────────────────────────────────────────────────────
# 2. Build subplots
# ─────────────────────────────────────────────────────────
df_tx_rank <- make_rank_transmission(df)
df_dz_rank <- make_rank_disease(df_disease)

subplot_A1 <- build_A1_ridges_transmission(df)
subplot_A2 <- build_A2_ridges_disease(df_disease)

subplot_B1 <- build_B1_snake_transmission(df_tx_rank)
subplot_B2 <- build_B2_snake_disease(df_dz_rank)

block_A <- patchwork::wrap_plots(
       subplot_A1, subplot_A2,
       ncol = 1,
       heights = c(0.6, 1))
 
block_B <- patchwork::wrap_plots(
         subplot_B1, subplot_B2,
         ncol = 1,
         heights = c(0.5, 1))
 
   # Combine the two blocks (two rows); make B a bit taller if you want
figure_combined <- patchwork::wrap_plots(
         block_A, block_B,
         ncol = 1,
         heights = c(1, 1.3)
     )

figure_combined

ggsave(
   figure_combined,
   file   = here::here("outputs", "final_figs_new", "Figure3_Combined2.pdf"),
   device = "pdf",
   units  = "cm",
   width  = 13,
   height = 10,
   dpi    = 600,
   scale  = 1.3
 )


