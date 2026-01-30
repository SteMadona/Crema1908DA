#NUOVO CODICE REPORT 1

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(lubridate)
library(rlang)
library(here)

# ================
# 1) NORMALIZZA COLONNE + FILTRI BASE
# ================
prep_physical_context <- function(df_raw,
                                  players = NULL,           # vettore nomi giocatori (opzionale)
                                  tag_keep = "Full Session",
                                  exclude_gk = TRUE) {
  
  physical_data <- df_raw %>%
    transmute(
      date      = as.Date(Date),
      day       = Day,
      duration  = `Phase Duration (min)`,
      player    = `Player Name`,
      Tag,
      category  = `Position Category`,
      position  = Position,
      TopSpeed  = `Top Speed (m/s)`,
      DistanceTot = `Distance Covered (m)`,
      WorkRate  = `Work Rate (m/min)`,
      SpeedZ1_m = `Speed Zone - Distance Covered (m) Zone 1 [0-1.5(m/s)]`,
      SpeedZ2_m = `Speed Zone - Distance Covered (m) Zone 2 [1.5-2(m/s)]`,
      SpeedZ3_m = `Speed Zone - Distance Covered (m) Zone 3 [2-3(m/s)]`,
      SpeedZ4_m = `Speed Zone - Distance Covered (m) Zone 4 [3-4(m/s)]`,
      SpeedZ5_m = `Speed Zone - Distance Covered (m) Zone 5 [4-5.5(m/s)]`,
      SpeedZ6_m = `Speed Zone - Distance Covered (m) Zone 6 [5.5-7(m/s)]`,
      SpeedZ7_m = `Speed Zone - Distance Covered (m) Zone 7 [> 7(m/s)]`,
      AccZ1_m   = `Horizontal Acc Zones - Distance Covered (m) Zone 1 [0-0.4(m/s^2)]`,
      AccZ2_m   = `Horizontal Acc Zones - Distance Covered (m) Zone 2 [0.4-0.9(m/s^2)]`,
      AccZ3_m   = `Horizontal Acc Zones - Distance Covered (m) Zone 3 [0.9-1.5(m/s^2)]`,
      AccZ4_m   = `Horizontal Acc Zones - Distance Covered (m) Zone 4 [1.5-2(m/s^2)]`,
      AccZ5_m   = `Horizontal Acc Zones - Distance Covered (m) Zone 5 [2-3(m/s^2)]`,
      AccZ6_m   = `Horizontal Acc Zones - Distance Covered (m) Zone 6 [> 3(m/s^2)]`,
      DecZ1_m   = `Horizontal Decl Zones - Distance Covered (m) Zone 1 [0-0.6(m/s^2)]`,
      DecZ2_m   = `Horizontal Decl Zones - Distance Covered (m) Zone 2 [0.6-1.1(m/s^2)]`,
      DecZ3_m   = `Horizontal Decl Zones - Distance Covered (m) Zone 3 [1.1-1.6(m/s^2)]`,
      DecZ4_m   = `Horizontal Decl Zones - Distance Covered (m) Zone 4 [1.6-2.2(m/s^2)]`,
      DecZ5_m   = `Horizontal Decl Zones - Distance Covered (m) Zone 5 [2.2-3(m/s^2)]`,
      DecZ6_m   = `Horizontal Decl Zones - Distance Covered (m) Zone 6 [> 3(m/s^2)]`
    ) %>%
    filter(Tag == tag_keep) %>%
    { if (!is.null(players)) filter(., player %in% players) else . } %>%
    mutate(
      across(
        c(starts_with("SpeedZ"), starts_with("AccZ"), starts_with("DecZ"),
          "duration","DistanceTot","TopSpeed","WorkRate"),
        ~ suppressWarnings(as.numeric(.))
      ),
      date = as.Date(date),
      week = isoweek(date),
      year = isoyear(date),
      week_id = paste(year, week, sep = "-")
    )
  
  # ================
  # 2) REPORT PLAYER (ALL TIME)
  # ================
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
  
  df_for_avg <- if (exclude_gk) df_physicalreport %>% filter(category != "Goalkeepers") else df_physicalreport
  
  team_avg <- df_for_avg %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
    mutate(player = "Media Squadra", category = "All")
  
  role_avg <- df_for_avg %>%
    group_by(category) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = "drop") %>%
    mutate(player = paste("Media", category))
  
  df_extended <- bind_rows(df_physicalreport, team_avg, role_avg)
  
  df_long <- df_extended %>%
    pivot_longer(
      cols = c(HID, sprint_dist, topspeed, workrate, acc_index, dec_index),
      names_to = "statistica",
      values_to = "valore"
    )
  
  # ================
  # 3) WEEKLY TREND
  # ================
  df_physicalreport_week <- physical_data %>%
    group_by(player, week_id) %>%
    summarise(
      HID = mean((SpeedZ5_m + SpeedZ6_m) / duration, na.rm = TRUE),
      sprint_dist = sum(SpeedZ7_m, na.rm = TRUE) / pmax(n(), 1),
      topspeed = max(TopSpeed, na.rm = TRUE) * 3.6,
      workrate = mean(WorkRate, na.rm = TRUE),
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
    group_by(player) %>%
    mutate(
      acc_index = {
        m <- max(acc_index, na.rm = TRUE)
        if (!is.finite(m) || m == 0) NA_real_ else acc_index / m * 100
      },
      dec_index = {
        m <- max(dec_index, na.rm = TRUE)
        if (!is.finite(m) || m == 0) NA_real_ else dec_index / m * 100
      }
    ) %>%
    ungroup()
  
  df_week_long <- df_physicalreport_week %>%
    pivot_longer(
      cols = c(dec_index, acc_index, sprint_dist, HID, topspeed, workrate),
      names_to = "statistica",
      values_to = "valore"
    )
  
  # ritorno “contesto” pronto all’uso
  list(
    physical_data = physical_data,
    df_physicalreport = df_physicalreport,
    df_long = df_long,
    df_week_long = df_week_long
  )
}

report_fisico <- function(giocatore, ctx, theme_fn = theme_crema_pro) {
  
  ruolo <- ctx$df_physicalreport %>%
    filter(player == giocatore) %>%
    pull(category) %>%
    unique()
  
  if (length(ruolo) == 0) stop("Giocatore non trovato nel contesto passato in ctx.")
  
  media_ruolo_label <- paste("Media", ruolo)
  
  ordine_variabili <- c("dec_index", "acc_index", "sprint_dist", "HID", "topspeed", "workrate")
  
  dati_giocatore <- ctx$df_long %>%
    filter(player == giocatore, statistica %in% ordine_variabili)
  
  media_ruolo <- ctx$df_long %>%
    filter(player == media_ruolo_label, statistica %in% ordine_variabili)
  
  dati_plot <- bind_rows(
    dati_giocatore %>% mutate(tipo = giocatore),
    media_ruolo    %>% mutate(tipo = "Media Ruolo")
  ) %>%
    drop_na(statistica) %>%
    mutate(statistica = factor(statistica, levels = ordine_variabili))
  
  colori <- setNames(c("#FF2E2E", "black"), c(giocatore, "Media Ruolo"))
  
  ggplot(dati_plot, aes(x = valore, y = statistica, fill = tipo)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6, na.rm = TRUE) +
    geom_text(
      aes(label = round(valore, 1)),
      position = position_dodge(width = 0.7),
      vjust = -0.3,
      color = "black", size = 3.8, fontface = "bold",
      na.rm = TRUE
    ) +
    coord_cartesian(clip = "off") +
    coord_flip() +
    scale_fill_manual(values = colori, name = NULL, drop = FALSE) +
    labs(
      title = paste("📊 Report Fisico –", giocatore),
      subtitle = paste("Confronto con Media Ruolo:", ruolo),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme_fn()
}


trend_fisico <- function(giocatore, ctx,
                         colori_statistiche = c(
                           acc_index   = "black",
                           dec_index   = "purple",
                           sprint_dist = "#FF2E2E",
                           HID         = "cyan",
                           topspeed    = "green",
                           workrate    = "gray50"
                         ),
                         theme_fn = theme_crema_pro) {
  
  dati <- ctx$df_week_long %>% filter(player == giocatore)
  
  if (nrow(dati) == 0) stop("Giocatore non trovato nel contesto passato in ctx.")
  
  dati <- dati %>%
    mutate(week_id = factor(week_id, levels = sort(unique(week_id))))
  
  ggplot(dati, aes(x = week_id, y = valore, group = statistica, color = statistica)) +
    geom_line(size = 1.5) +
    geom_point(
      size = 2, shape = 21, stroke = 0.9,
      aes(fill = statistica),
      color = "black",
      show.legend = FALSE
    ) +
    scale_color_manual(values = colori_statistiche) +
    scale_fill_manual(values = colori_statistiche) +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
    labs(
      title = paste("📈 Trend Fisico Settimanale –", giocatore),
      x = "Settimana",
      y = NULL,
      color = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme_fn() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}


