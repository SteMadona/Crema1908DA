df_scatter <- agg %>%
  left_join(df_physicalreport %>% select(player, physical_index), by = "player") %>%
  left_join(df_touch_avg, by = "player") %>%
  filter(category != "Goalkeepers" & !player %in% c("C. Barzi", "G. D'Errico", "F. Cerioli"))


df_scatter_ruoli <- agg %>%
  left_join(df_passes, by = "player") %>%
  left_join(df_physicalreport, by = "player") %>%
  filter(category.x != "Goalkeepers" & !player %in% c("C. Barzi", "G. D'Errico", "F. Cerioli"))


# ==============================================================================
# DIFENSORI 
# ==============================================================================

df_scatter_dif <- df_scatter_ruoli %>%
  select(player, category.x, pass_perc, regains, HID, workrate) %>%
  mutate(
    dif_idx_raw = (
      scales::rescale(regains, to = c(0, 1), from = range(regains, na.rm = T)) *0.6 +
        scales::rescale(pass_perc, to = c(0, 1), from = range(pass_perc, na.rm = T)) * 0.4
      ), 
    dif_idx = rescale(dif_idx_raw, to = c(0, 1)), 
    phy_idx_raw = (
      scales::rescale(HID, to = c(0,1), from = range(HID, na.rm = T)) * 0.5 + 
        scales::rescale(workrate, to = c(0, 1), from = range(workrate, na.rm = T)) * 0.5
    ), 
    phy_idx = rescale(phy_idx_raw, to = c(0, 1))
      
  ) %>%
  filter(category.x == "Defenders")


mean_physical_dif <- mean(df_scatter_dif$phy_idx, na.rm = TRUE)
mean_quality_dif  <- mean(df_scatter_dif$dif_idx, na.rm = TRUE)

scatter_dif <- ggplot(df_scatter_dif, aes(x = dif_idx, y = phy_idx)) +
  geom_point(
    shape = 21,       # 🔹 Cerchio con bordo
    color = "white",  # 🔹 Bordo bianco
    fill = "#FF2E2E",
    size = 5,         # 🔹 Pallini più grandi
    alpha = 0.9,
    stroke = 0.7      # 🔹 Spessore del bordo
  ) +
  geom_text(
    aes(label = player),
    vjust = -1,
    color = "white",
    size = 4,
    alpha = 0.8
  ) +
  geom_vline(xintercept = mean_quality_dif, linetype = "dashed", color = "gray60", size = 1.1) +
  geom_hline(yintercept = mean_physical_dif,  linetype = "dashed", color = "gray60", size = 1.1) +
  
  guides(
    fill = guide_legend(
      override.aes = list(color = "white")  # legenda con bordo bianco
    )
  ) +
  
  labs(
    title = "📈 Confronto doti difensive e fisiche",
    subtitle = "Difensori: recuperi e passaggi vs HID e workrate",
    x = "Indice Qualità Difensiva (0-1)",
    y = "Indice Qualità Fisica (0–1)"
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


# ==============================================================================
# CENTROCAMPISTI 
# ==============================================================================



df_scatter_cc <- df_scatter_ruoli %>%
  select(player, category.x, pass_perc, workrate, HID, touch_per_min) %>%
  mutate(
    quality_idx_raw = (
      scales::rescale(pass_perc, to = c(0,1), from = range(pass_perc, na.rm = T))*0.6 + 
        scales::rescale(touch_per_min, to = c(0, 1), from = range(touch_per_min, na.rm = T))*0.4
    ),
    qual_idx = rescale(quality_idx_raw, to = c(0, 1)), 
    phy_idx_raw = (
      scales::rescale(HID, to = c(0, 1), from = range(HID, na.rm = T))*0.4 + 
        scales::rescale(workrate, to = c(0, 1), from = range(workrate, na.rm = T))*0.6
    ), 
    phy_idx = rescale(phy_idx_raw, to = c(0, 1))
  ) %>%
  filter(category.x == "Midfielders")

mean_physical_cc <- mean(df_scatter_cc$phy_idx, na.rm = TRUE)
mean_quality_cc  <- mean(df_scatter_cc$qual_idx, na.rm = TRUE)

scatter_cc <- ggplot(df_scatter_cc, aes(x = qual_idx, y = phy_idx)) +
  geom_point(
    shape = 21,       # 🔹 Cerchio con bordo
    color = "white",  # 🔹 Bordo bianco
    fill = "#FF2E2E",
    size = 5,         # 🔹 Pallini più grandi
    alpha = 0.9,
    stroke = 0.7      # 🔹 Spessore del bordo
  ) +
  geom_text(
    aes(label = player),
    vjust = -1,
    color = "white",
    size = 4,
    alpha = 0.8
  ) +
  geom_vline(xintercept = mean_quality_cc, linetype = "dashed", color = "gray60", size = 1.1) +
  geom_hline(yintercept = mean_physical_cc,  linetype = "dashed", color = "gray60", size = 1.1) +
  
  guides(
    fill = guide_legend(
      override.aes = list(color = "white")  # legenda con bordo bianco
    )
  ) +
  
  labs(
    title = "📈 Confronto doti tecniche e fisiche",
    subtitle = "Centrocampisti: tocchi e %passaggi vs HID e workrate",
    x = "Indice Qualità Tecnica (0-1)",
    y = "Indice Qualità Fisica (0–1)"
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



# ==============================================================================
# ATTACCANTI 
# ==============================================================================


df_scatter_att <- df_scatter_ruoli %>%
  select(player, category.x, release_vel_avg, sprint_dist, touch_L, 
         touch_R, touches_total, topspeed) %>%
  mutate(foot_balance = 1 - (touch_R / touches_total), 
         qual_idx_raw = (
           scales::rescale(release_vel_avg, to = c(0, 1), from = range(release_vel_avg, na.rm = T))*0.7 +
             scales::rescale(foot_balance, to = c(0, 1), from = range(foot_balance, na.rm = T))*0.3
         ), 
         qual_idx = rescale(qual_idx_raw, to = c(0, 1)), 
         phy_idx_raw = (
           scales::rescale(sprint_dist, to = c(0, 1), from = range(sprint_dist, na.rm = T))*0.6 + 
             scales::rescale(topspeed, to = c(0, 1), frmo = range(topspeed, na.rm = T))*0.4
         ), 
         phy_idx = rescale(phy_idx_raw, to = c(0, 1))
      ) %>%
  filter(category.x == "Forwards")

mean_physical_att <- mean(df_scatter_att$phy_idx, na.rm = TRUE)
mean_quality_att <- mean(df_scatter_att$qual_idx, na.rm = TRUE)

scatter_att <- ggplot(df_scatter_att, aes(x = qual_idx, y = phy_idx)) +
  geom_point(
    shape = 21,       # 🔹 Cerchio con bordo
    color = "white",  # 🔹 Bordo bianco
    fill = "#FF2E2E",
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
  geom_vline(xintercept = mean_quality_att, linetype = "dashed", color = "gray60", size = 1.1) +
  geom_hline(yintercept = mean_physical_att,  linetype = "dashed", color = "gray60", size = 1.1) +
  
  guides(
    fill = guide_legend(
      override.aes = list(color = "white")  # legenda con bordo bianco
    )
  ) +
  
  labs(
    title = "📈 Confronto doti tecniche e fisiche",
    subtitle = "Attaccanti: Media velocità rilascio e balance sx/dx vs topspeed e sprint distance",
    x = "Indice Qualità Tecnica (0-1)",
    y = "Indice Qualità Fisica (0–1)"
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




