library(ggrepel)  
library(scales)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(rlang)
library(lubridate)

# --- PREPARAZIONE DATI ------------------------------------------------------

# Calcolo la media dei tocchi totali per giocatore
df_touch_avg <- df_skill %>%
  mutate(touches_total = touch_L + touch_R) %>%
  group_by(player) %>%
  summarise(touches_total_avg = mean(touches_total, na.rm = TRUE), .groups = "drop")

df_physicalreport <- df_physicalreport %>%
  mutate(
    physical_index_raw =(
      scales::rescale(z5_min, to = c(0, 1), from = range(z5_min, na.rm = T)) * 0.2 + 
        scales::rescale(z6_min, to = c(0, 1), from = range(z6_min, na.rm = T)) * 0.2 + 
        scales::rescale(sprint_dist, to = c(0, 1), from = range(sprint_dist, na.rm = T)) * 0.1 + 
        scales::rescale(workrate, to = c(0, 1), from = range(workrate, na.rm = T))* 0.5
    ), 
    physical_index = rescale(physical_index_raw, to = c(0, 1))
  )

# Unisco parte tecnica (agg) e fisica (df_physicalreport) + tocchi medi
df_scatter <- agg %>%
  left_join(df_physicalreport %>% select(player, physical_index), by = "player") %>%
  left_join(df_touch_avg, by = "player") %>%
  filter(category != "Goalkeepers" & !player %in% c("C. Barzi", "G. D'Errico", "F. Cerioli"))

# --- INDICE DI QUALITÀ TECNICA ----------------------------------------------

# Costruiamo un indice composito (0–100)
df_scatter <- df_scatter %>%
  mutate(
    one_touch_share = share_1T,  # % di possesso in tocchi di prima
    quality_index_raw = (
      scales::rescale(releases_per_min, to = c(0,1), from = range(releases_per_min, na.rm=TRUE)) * 0.4 +
        scales::rescale(release_vel_avg,  to = c(0,1), from = range(one_touch_share, na.rm=TRUE))  * 0.4 +
        scales::rescale(touches_total_avg, to = c(0, 1), from = range(touches_total_avg, na.rm = T)) *0.2
    ),
    quality_index = rescale(quality_index_raw, to = c(0, 1))
  )

# --- MEDIE PER LINEE TRATTEGGIATE -------------------------------------------

mean_physical <- mean(df_scatter$physical_index, na.rm = TRUE)
mean_quality  <- mean(df_scatter$quality_index, na.rm = TRUE)


# --- SCATTERPLOT ------------------------------------------------------------
scatter_phy_tec <- ggplot(df_scatter, aes(x = physical_index, y = quality_index)) +
  geom_point(
    aes(fill = category),
    shape = 21,       # 🔹 Cerchio con bordo
    color = "white",  # 🔹 Bordo bianco
    size = 5,         # 🔹 Pallini più grandi
    alpha = 0.9,
    stroke = 0.7      # 🔹 Spessore del bordo
  ) +
  geom_text(
    aes(label = player),
    vjust = -1,
    color = "white",
    size = 3,
    alpha = 0.8
  ) +
  geom_vline(xintercept = mean_physical, linetype = "dashed", color = "gray60", size = 1.1) +
  geom_hline(yintercept = mean_quality,  linetype = "dashed", color = "gray60", size = 1.1) +
  
  scale_fill_manual(values = c(
    "Defenders" = "#FFFFFF",     # bianco
    "Midfielders" = "#FF2E2E",   # rosso
    "Forwards" = "gray30"        # grigio scuro
  )) +
  
  guides(
    fill = guide_legend(
      override.aes = list(color = "white")  # legenda con bordo bianco
    )
  ) +
  
  labs(
    title = "📈 Confronto tra qualità fisica e tecnica",
    subtitle = "Giocatori di movimento – confronto tra volumi fisici e qualità tecnica",
    x = "Indice Qualità Fisica (0-1)",
    y = "Indice Qualità Tecnica (0–1)",
    fill = "Ruolo"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#111111", color = NA),
    panel.background = element_rect(fill = "#111111", color = NA),
    legend.background = element_rect(fill = "#111111", color = NA),
    panel.grid.major = element_line(color = "#333333"),
    panel.grid.minor = element_line(color = "#222222"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    plot.title = element_text(color = "#FF2E2E", face = "bold", size = 16),
    plot.subtitle = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  )
