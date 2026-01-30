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
  filter(player %in% player_list) %>%
  mutate(touches_total = touch_L + touch_R) %>%
  group_by(player) %>%
  summarise(touches_total_avg = mean(touches_total, na.rm = TRUE), .groups = "drop")

df_physicalreport <- df_physicalreport %>%
  filter(player %in% player_list) %>%
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
  filter(category != "Goalkeepers" & player %in% player_list)

# --- INDICE DI QUALITÀ TECNICA ----------------------------------------------

# Costruiamo un indice composito (0–100)
df_scatter <- df_scatter %>%
  mutate(
    one_touch_share = share_1T,  # % di possesso in tocchi di prima
    quality_index_raw = (
      scales::rescale(releases_per_min, to = c(0,1), from = range(releases_per_min, na.rm=TRUE)) * 0.4 +
        scales::rescale(release_vel_avg,  to = c(0,1), from = range(release_vel_avg, na.rm=TRUE))  * 0.4 +
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
    color = "black",  # 🔹 Bordo bianco
    size = 5,         # 🔹 Pallini più grandi
    alpha = 0.9,
    stroke = 0.7      # 🔹 Spessore del bordo
  ) +
  geom_text(
    aes(label = player),
    vjust = -1,
    color = "black",
    size = 4,
    alpha = 0.8
  ) +
  geom_vline(xintercept = mean_physical, linetype = "dashed", color = "gray60", size = 1.1) +
  geom_hline(yintercept = mean_quality,  linetype = "dashed", color = "gray60", size = 1.1) +
  
  scale_fill_manual(values = c(
    "Defenders" = "black",     # bianco
    "Midfielders" = "#FF2E2E",   # rosso
    "Forwards" = "gray30"        # grigio scuro
  )) +
  
  guides(
    fill = guide_legend(
      override.aes = list(color = "black")  # legenda con bordo bianco
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
  theme_crema_pro()

scatter_phy_tec


plot_phy_tec_player <- function(player_name) {
  
  df_player <- df_scatter %>% dplyr::filter(player == player_name)
  
  if (nrow(df_player) == 0) {
    stop("Giocatore non trovato in df_scatter (controlla spelling/nome).")
  }
  
  ggplot2::ggplot(df_player, ggplot2::aes(x = physical_index, y = quality_index)) +
    ggplot2::geom_point(
      ggplot2::aes(fill = category),
      shape = 21,
      color = "black",
      size = 6,
      alpha = 0.95,
      stroke = 0.8
    ) +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = player),
      color = "black",
      size = 5,
      box.padding = 0.4,
      point.padding = 0.3,
      max.overlaps = Inf
    ) +
    ggplot2::geom_vline(xintercept = mean_physical, linetype = "dashed", color = "gray60", linewidth = 1.1) +
    ggplot2::geom_hline(yintercept = mean_quality,  linetype = "dashed", color = "gray60", linewidth = 1.1) +
    ggplot2::scale_fill_manual(values = c(
      "Defenders" = "black",
      "Midfielders" = "#FF2E2E",
      "Forwards" = "gray30"
    )) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(color = "black"))) +
    ggplot2::labs(
      title = "📈 Confronto tra qualità fisica e tecnica",
      subtitle = paste0("Giocatore: ", player_name, " — linee tratteggiate = medie squadra"),
      x = "Indice Qualità Fisica (0-1)",
      y = "Indice Qualità Tecnica (0–1)",
      fill = "Ruolo"
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    theme_crema_pro() +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.3), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.3), expand = c(0, 0))
  
}


build_df_scatter_phy_tec <- function(ctx_skill,
                                     df_physicalreport,
                                     player_list,
                                     exclude_gk = TRUE) {
  
  # ---- 1) tocchi medi da ctx$df_skill (non da df_skill globale) ----
  df_touch_avg <- ctx_skill$df_skill %>%
    filter(player %in% player_list) %>%
    mutate(touches_total = touch_L + touch_R) %>%
    group_by(player) %>%
    summarise(touches_total_avg = mean(touches_total, na.rm = TRUE), .groups = "drop")
  
  # ---- 2) indice fisico (come fai tu) ----
  df_phys <- df_physicalreport %>%
    filter(player %in% player_list) %>%
    mutate(
      physical_index_raw = (
        scales::rescale(z5_min,      to = c(0,1), from = range(z5_min, na.rm = TRUE)) * 0.2 +
          scales::rescale(z6_min,      to = c(0,1), from = range(z6_min, na.rm = TRUE)) * 0.2 +
          scales::rescale(sprint_dist, to = c(0,1), from = range(sprint_dist, na.rm = TRUE)) * 0.1 +
          scales::rescale(workrate,    to = c(0,1), from = range(workrate, na.rm = TRUE)) * 0.5
      ),
      physical_index = scales::rescale(physical_index_raw, to = c(0,1))
    ) %>%
    select(player, physical_index)
  
  # ---- 3) base tecnica: usa ctx$agg_all (è il tuo “agg”) ----
  df_scatter <- ctx_skill$agg_all %>%
    left_join(df_phys, by = "player") %>%
    left_join(df_touch_avg, by = "player") %>%
    filter(player %in% player_list) %>%
    { if (exclude_gk) filter(., category != "Goalkeepers") else . }
  
  # ---- 4) indice qualità tecnica (come fai tu, ma su df_scatter) ----
  df_scatter <- df_scatter %>%
    mutate(
      quality_index_raw = (
        scales::rescale(releases_per_min,   to = c(0,1), from = range(releases_per_min, na.rm = TRUE)) * 0.4 +
          scales::rescale(release_vel_avg,    to = c(0,1), from = range(release_vel_avg,  na.rm = TRUE)) * 0.4 +
          scales::rescale(touches_total_avg,  to = c(0,1), from = range(touches_total_avg,na.rm = TRUE)) * 0.2
      ),
      quality_index = scales::rescale(quality_index_raw, to = c(0,1))
    )
  
  # ---- 5) medie squadra (per linee tratteggiate) ----
  mean_physical <- mean(df_scatter$physical_index, na.rm = TRUE)
  mean_quality  <- mean(df_scatter$quality_index,  na.rm = TRUE)
  
  list(
    df_scatter = df_scatter,
    mean_physical = mean_physical,
    mean_quality  = mean_quality
  )
}

prova <- prep_physical_context(df_ft, players = players_ft)
prova2 <- prova$df_physicalreport

out_scatter_session <- build_df_scatter_phy_tec(
  ctx_skill = ctx_session,
  df_physicalreport = prova2,
  player_list = players_ft
)

df_scatter    <- out_scatter_session$df_scatter
mean_physical <- out_scatter_session$mean_physical
mean_quality  <- out_scatter_session$mean_quality
