library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(rlang)

df <- read_excel("../data/CremaFT_1910.xlsx")


physical_data <- cbind(
  df$`Phase Duration (min)`, 
  df$`Player Name`,
  df$Tag, 
  df$`Position Category`, 
  df$Position, 
  df$`Top Speed (m/s)`, 
  df$`Distance Covered (m)`, 
  df$`Work Rate (m/min)`, 
  df$`Speed Zone - Distance Covered (m) Zone 1 [0-1.5(m/s)]`, 
  df$`Speed Zone - Distance Covered (m) Zone 2 [1.5-2(m/s)]`, 
  df$`Speed Zone - Distance Covered (m) Zone 3 [2-3(m/s)]`, 
  df$`Speed Zone - Distance Covered (m) Zone 4 [3-4(m/s)]`, 
  df$`Speed Zone - Distance Covered (m) Zone 5 [4-5.5(m/s)]`, 
  df$`Speed Zone - Distance Covered (m) Zone 6 [5.5-7(m/s)]`, 
  df$`Speed Zone - Distance Covered (m) Zone 7 [> 7(m/s)]`, 
  df$`Horizontal Acc Zones - Distance Covered (m) Zone 1 [0-0.4(m/s^2)]`, 
  df$`Horizontal Acc Zones - Distance Covered (m) Zone 2 [0.4-0.9(m/s^2)]`,
  df$`Horizontal Acc Zones - Distance Covered (m) Zone 3 [0.9-1.5(m/s^2)]`, 
  df$`Horizontal Acc Zones - Distance Covered (m) Zone 4 [1.5-2(m/s^2)]`, 
  df$`Horizontal Acc Zones - Distance Covered (m) Zone 5 [2-3(m/s^2)]`, 
  df$`Horizontal Acc Zones - Distance Covered (m) Zone 6 [> 3(m/s^2)]`, 
  df$`Horizontal Decl Zones - Distance Covered (m) Zone 1 [0-0.6(m/s^2)]`, 
  df$`Horizontal Decl Zones - Distance Covered (m) Zone 2 [0.6-1.1(m/s^2)]`, 
  df$`Horizontal Decl Zones - Distance Covered (m) Zone 3 [1.1-1.6(m/s^2)]`, 
  df$`Horizontal Decl Zones - Distance Covered (m) Zone 4 [1.6-2.2(m/s^2)]`, 
  df$`Horizontal Decl Zones - Distance Covered (m) Zone 5 [2.2-3(m/s^2)]`, 
  df$`Horizontal Decl Zones - Distance Covered (m) Zone 6 [> 3(m/s^2)]`
)

physical_data <- as.data.frame(physical_data)

colnames(physical_data) <- c(
  "duration", "player", "Tag", "category", "position",
  "TopSpeed", "DistanceTot", "WorkRate",
  "SpeedZ1_m", "SpeedZ2_m", "SpeedZ3_m", "SpeedZ4_m", "SpeedZ5_m", "SpeedZ6_m", "SpeedZ7_m",
  "AccZ1_m", "AccZ2_m", "AccZ3_m", "AccZ4_m", "AccZ5_m", "AccZ6_m",
  "DecZ1_m", "DecZ2_m", "DecZ3_m", "DecZ4_m", "DecZ5_m", "DecZ6_m"
)

physical_data <- physical_data %>% 
  filter(Tag == "Full Session") %>% 
  mutate(
    across(c(starts_with("SpeedZ"), starts_with("AccZ"), starts_with("DecZ"),
             "duration","DistanceTot","TopSpeed","WorkRate"), 
           ~ suppressWarnings(as.numeric(.)))
  )


# =============================
# SPRINT DIST vs TOP SPEED
# =============================
df_topspeed <- physical_data %>%
  group_by(player) %>%
  summarise(
    sprint_dist = sum(SpeedZ7_m, na.rm = TRUE),
    TopSpeed = max(TopSpeed, na.rm = TRUE) * 3.6,  # m/s -> km/h
    .groups = "drop"
  ) %>%
  left_join(physical_data %>% select(player, category) %>% distinct(), by = "player") %>%
  filter(category != "Goalkeepers")

ggplot(data = df_topspeed, aes(x = sprint_dist, y = TopSpeed, label = player)) +
  geom_point(aes(color = category), alpha = 0.9, size = 3, na.rm = TRUE) + 
  geom_text(vjust = -1, size = 3, color = "white", alpha = 0.8) + 
  geom_hline(yintercept = mean(df_topspeed$TopSpeed, na.rm = TRUE),
             linetype = "dashed", color = "gray50", size = 1.1) + 
  geom_vline(xintercept = mean(df_topspeed$sprint_dist, na.rm = TRUE),
             linetype = "dashed", color = "gray50", size = 1.1) + 
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
    plot.title = element_text(color = "white", face = "bold", size = 16),
    plot.subtitle = element_text(color = "gray80"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  ) + 
  labs(
    x = "Sprint Distance (m)", 
    y = "Top Speed (km/h)", 
    title = "Sprint Distance vs Top Speed", 
    color = "Category"
  )

# =============================
# INDICI / REPORT
# =============================
df_physicalreport <- physical_data %>%
  group_by(player) %>%
  summarise(
    HID = mean((SpeedZ5_m + SpeedZ6_m) / duration, na.rm = TRUE),
    z1_min = mean(SpeedZ1_m / duration, na.rm = TRUE), 
    z2_min = mean(SpeedZ2_m / duration, na.rm = TRUE), 
    z3_min = mean(SpeedZ3_m / duration, na.rm = TRUE), 
    z4_min = mean(SpeedZ4_m / duration, na.rm = TRUE), 
    z5_min = mean(SpeedZ5_m / duration, na.rm = TRUE), 
    z6_min = mean(SpeedZ6_m / duration, na.rm = TRUE), 
    sprint_dist = sum(SpeedZ7_m, na.rm = TRUE) / pmax(n(), 1), 
    topspeed = max(TopSpeed, na.rm = TRUE) * 3.6,  # m/s -> km/h
    workrate = mean(WorkRate, na.rm = TRUE),
    
    # pesi crescenti per Acc/Dec (1..6)
    acc_index = {
      acc_means <- colMeans(select(cur_data_all(), starts_with("AccZ")), na.rm = TRUE)
      sum((1:6) * acc_means)
    },
    dec_index = {
      dec_means <- colMeans(select(cur_data_all(), starts_with("DecZ")), na.rm = TRUE)
      sum((1:6) * dec_means)
    },
    .groups = "drop"
  ) %>%
  left_join(physical_data %>% select(player, category) %>% distinct(), by = "player") %>%
  mutate(
    acc_index = scales::rescale(acc_index, to = c(0,100)),
    dec_index = scales::rescale(dec_index, to = c(0,100))
  )

# Escludo portieri per medie
df_no_gk <- df_physicalreport %>% filter(category != "Goalkeepers")

# Medie di squadra e ruolo
team_avg <- df_no_gk %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  mutate(player = "Media Squadra", category = "All")

role_avg <- df_no_gk %>%
  group_by(category) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
  mutate(player = paste("Media", category))

# Unisco
df_extended <- bind_rows(df_physicalreport, team_avg, role_avg)

# PIVOT — **HID incluso**
df_long <- df_extended %>%
  pivot_longer(
    cols = c(z1_min:z6_min, HID, sprint_dist, topspeed, workrate, acc_index, dec_index),
    names_to = "statistica",
    values_to = "valore"
  )

# =============================
# FUNZIONE REPORT
# =============================
report_fisico <- function(giocatore) {
  ruolo <- df_physicalreport %>% filter(player == giocatore) %>% pull(category) %>% unique()
  media_ruolo_label <- paste("Media", ruolo)
  
  # variabili da mostrare (ordine fisso)
  ordine_variabili <- c("dec_index", "acc_index", "sprint_dist", "HID", "topspeed", "workrate")
  
  # filtra SOLO queste statistiche
  dati_giocatore <- df_long %>% 
    filter(player == giocatore, statistica %in% ordine_variabili)
  
  media_ruolo <- df_long %>% 
    filter(player == media_ruolo_label, statistica %in% ordine_variabili)
  
  # fattorizza e drop di eventuali NA
  dati_giocatore$statistica <- factor(dati_giocatore$statistica, levels = ordine_variabili)
  media_ruolo$statistica    <- factor(media_ruolo$statistica,    levels = ordine_variabili)
  
  dati_plot <- bind_rows(
    dati_giocatore %>% mutate(tipo = giocatore),
    media_ruolo    %>% mutate(tipo = "Media Ruolo")
  ) %>% tidyr::drop_na(statistica)
  
  # mappa colori sul VALORE della legenda
  colori <- setNames(c("#FF2E2E", "white"), c(giocatore, "Media Ruolo"))
  
  ggplot(dati_plot, aes(x = valore, y = statistica, fill = tipo)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6, na.rm = TRUE) +
    geom_text(
      aes(label = round(valore, 1)),
      position = position_dodge(width = 0.7),
      vjust = -0.3, # etichette fuori dalla barra
      color = "white", size = 3.8, fontface = "bold",
      na.rm = TRUE
    ) +
    coord_cartesian(clip = "off") +
    coord_flip() +
    scale_fill_manual(values = colori, name = NULL, drop = FALSE) +
    scale_y_discrete(limits = ordine_variabili) +
    labs(
      title = paste("📊 Report Fisico –", giocatore),
      subtitle = paste("Confronto con Media Ruolo:", ruolo),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.background  = element_rect(fill = "#0b0b0b", color = NA),
      panel.background = element_rect(fill = "#0b0b0b", color = NA),
      panel.grid.major = element_line(color = "#222222"),
      panel.grid.minor = element_blank(),
      axis.text.y      = element_text(color = "white", face = "bold"),
      axis.text.x      = element_text(color = "white"),
      plot.title       = element_text(color = "#FF2E2E", face = "bold", size = 16),
      plot.subtitle    = element_text(color = "white"),
      legend.text      = element_text(color = "white", face = "bold"),
      legend.position  = "top",
      plot.margin      = margin(10, 40, 10, 10)
    )
}