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


trend_fisico <- function(
    giocatore,
    ctx,
    ordine_stat = c("acc_index", "dec_index", "sprint_dist", "HID", "topspeed", "workrate"),
    etichette_stat = c(
      acc_index   = "Indice accelerazioni",
      dec_index   = "Indice decelerazioni",
      sprint_dist = "Sprint distance",
      HID         = "HID",
      topspeed    = "Top speed",
      workrate    = "Work rate"
    ),
    colore_linea = "#B30000",
    colore_media = "#B8C0CC",
    theme_fn = theme_crema_pro
) {
  
  dati <- ctx$df_week_long %>%
    dplyr::filter(player == giocatore, statistica %in% ordine_stat) %>%
    dplyr::filter(!is.na(valore))
  
  if (nrow(dati) == 0) {
    stop("Giocatore non trovato nel contesto passato in ctx.")
  }
  
  # Ordine settimane stabile
  livelli_settimana <- sort(unique(dati$week_id))
  
  dati <- dati %>%
    dplyr::mutate(
      week_id = factor(week_id, levels = livelli_settimana),
      statistica = factor(
        statistica,
        levels = ordine_stat,
        labels = etichette_stat[ordine_stat]
      )
    ) %>%
    dplyr::arrange(statistica, week_id)
  
  # Media stagionale per ciascuna metrica
  medie_stat <- dati %>%
    dplyr::group_by(statistica) %>%
    dplyr::summarise(media = mean(valore, na.rm = TRUE), .groups = "drop")
  
  # Ultimo punto disponibile per etichetta finale
  ultimi_punti <- dati %>%
    dplyr::group_by(statistica) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(label_val = scales::number(valore, accuracy = 0.1))
  
  ggplot(dati, aes(x = week_id, y = valore, group = 1)) +
    geom_hline(
      data = medie_stat,
      aes(yintercept = media),
      inherit.aes = FALSE,
      linewidth = 0.7,
      linetype = "dashed",
      color = colore_media
    ) +
    geom_line(
      linewidth = 1.05,
      color = colore_linea,
      lineend = "round"
    ) +
    geom_point(
      size = 2.3,
      shape = 21,
      stroke = 0.9,
      fill = "white",
      color = colore_linea
    ) +
    geom_point(
      data = ultimi_punti,
      size = 3.1,
      shape = 21,
      stroke = 1.1,
      fill = colore_linea,
      color = colore_linea,
      show.legend = FALSE
    ) +
    geom_text(
      data = ultimi_punti,
      aes(label = label_val),
      nudge_y = 0,
      vjust = -0.9,
      size = 3.4,
      fontface = "bold",
      color = "#111111",
      show.legend = FALSE
    ) +
    facet_wrap(~ statistica, scales = "free_y", ncol = 2) +
    scale_x_discrete(
      breaks = function(x) x[seq(1, length(x), by = 2)]
    ) +
    labs(
      title = "Trend fisico settimanale",
      subtitle = giocatore,
      x = "Settimana",
      y = NULL,
      caption = "Linea tratteggiata = media stagionale della metrica"
    ) +
    coord_cartesian(clip = "off") +
    theme_fn(base_size = 12.5) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.title.x = element_text(face = "bold"),
      axis.text.y = element_text(face = "bold"),
      strip.text = element_text(face = "bold", size = 11.5),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "#555555"),
      plot.caption = element_text(color = "#6B7280")
    )
}