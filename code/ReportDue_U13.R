library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(stringr)

# =========================================================================
# HELPERS U13
# =========================================================================

build_report_long_u13 <- function(giocatore,
                                  agg_report_u13,
                                  exclude_gk_from_team_avg = FALSE) {
  
  player_row <- agg_report_u13 %>%
    filter(player == giocatore)
  
  if (nrow(player_row) == 0) {
    stop("Giocatore non trovato in agg_report_u13.")
  }
  
  team_tbl <- agg_report_u13
  
  if (exclude_gk_from_team_avg) {
    team_tbl <- team_tbl %>% filter(category != "Goalkeepers")
  }
  
  media_row <- team_tbl %>%
    summarise(
      possession_index      = mean(possession_index, na.rm = TRUE),
      touches_index         = mean(touches_index, na.rm = TRUE),
      rv_index              = mean(rv_index, na.rm = TRUE),
      meters_per_min        = mean(meters_per_min, na.rm = TRUE),
      share_touch_R         = mean(share_touch_R, na.rm = TRUE),
      share_touch_L         = mean(share_touch_L, na.rm = TRUE),
      avg_time_per_poss_sec = mean(avg_time_per_poss_sec, na.rm = TRUE),
      share_1T              = mean(share_1T, na.rm = TRUE),
      share_short           = mean(share_short, na.rm = TRUE),
      share_long            = mean(share_long, na.rm = TRUE)
    ) %>%
    mutate(
      player = "Media Squadra",
      category = NA_character_
    )
  
  pack <- function(tbl, tipo_label) {
    tbl %>%
      transmute(
        player,
        possession_index,
        touches_index,
        rv_index,
        meters_per_min,
        touch_right = share_touch_R * 100,
        touch_left  = share_touch_L * 100,
        avg_time_per_poss_sec,
        one_touch  = share_1T * 100,
        short_poss = share_short * 100,
        long_poss  = share_long * 100
      ) %>%
      mutate(tipo = tipo_label)
  }
  
  bind_rows(
    pack(player_row, giocatore),
    pack(media_row, "Media Squadra")
  ) %>%
    pivot_longer(
      -c(player, tipo),
      names_to = "metrica",
      values_to = "valore"
    ) %>%
    mutate(
      metrica = factor(
        metrica,
        levels = c(
          "possession_index",
          "touches_index",
          "rv_index",
          "meters_per_min",
          "touch_right",
          "touch_left",
          "avg_time_per_poss_sec",
          "one_touch",
          "short_poss",
          "long_poss"
        )
      )
    )
}

# =========================================================================
# REPORT TECNICO U13
# =========================================================================

report_tecnico_u13 <- function(giocatore,
                               agg_report_u13,
                               theme_fn = theme_crema_pro,
                               exclude_gk_from_team_avg = FALSE) {
  
  dati_plot <- build_report_long_u13(
    giocatore = giocatore,
    agg_report_u13 = agg_report_u13,
    exclude_gk_from_team_avg = exclude_gk_from_team_avg
  )
  
  label_metriche <- c(
    possession_index      = "Indice possesso",
    touches_index         = "Indice tocchi",
    rv_index              = "Indice RV",
    meters_per_min        = "Metri/min",
    touch_right           = "Tocchi dx",
    touch_left            = "Tocchi sx",
    avg_time_per_poss_sec = "Tempo medio poss.",
    one_touch             = "One-touch",
    short_poss            = "Possesso corto",
    long_poss             = "Possesso lungo"
  )
  
  dati_plot <- dati_plot %>%
    mutate(
      metrica_label = factor(
        label_metriche[as.character(metrica)],
        levels = label_metriche[c(
          "possession_index",
          "touches_index",
          "rv_index",
          "meters_per_min",
          "touch_right",
          "touch_left",
          "avg_time_per_poss_sec",
          "one_touch",
          "short_poss",
          "long_poss"
        )]
      )
    )
  
  lab_fun <- function(m, v) {
    if (m %in% c("touch_right", "touch_left", "one_touch", "short_poss", "long_poss")) {
      paste0(round(v, 1), "%")
    } else if (m == "avg_time_per_poss_sec") {
      paste0(round(v, 2), " s")
    } else if (m == "meters_per_min") {
      paste0(round(v, 1), " m/min")
    } else {
      round(v, 1)
    }
  }
  
  ggplot(dati_plot, aes(x = valore, y = metrica_label, fill = tipo)) +
    geom_col(
      position = position_dodge(width = 0.7),
      width = 0.62
    ) +
    geom_text(
      aes(label = mapply(lab_fun, as.character(metrica), valore)),
      position = position_dodge(width = 0.7),
      hjust = -0.15,
      size = 3.5,
      fontface = "bold",
      color = "black"
    ) +
    scale_fill_manual(
      values = c(
        setNames("#FF2E2E", giocatore),
        "Media Squadra" = "black"
      ),
      name = NULL
    ) +
    labs(
      title = paste("📊 Report Tecnico –", giocatore),
      subtitle = "Confronto con media squadra",
      x = NULL,
      y = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_minimal(base_size = 13) +
    theme_fn() +
    theme(
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "#555555"),
      legend.position = "top"
    )
}

# =========================================================================
# TREND PIEDE U13
# =========================================================================

trend_tecnico_piede_u13 <- function(giocatore,
                                    ctx_skill,
                                    theme_fn = theme_crema_pro) {
  
  dati <- ctx_skill$agg_week %>%
    filter(player == giocatore) %>%
    select(week_id, share_touch_R, share_touch_L) %>%
    pivot_longer(
      cols = starts_with("share_touch_"),
      names_to = "piede",
      values_to = "valore"
    ) %>%
    mutate(
      valore = valore * 100,
      week_id = factor(week_id, levels = sort(unique(week_id))),
      piede = factor(
        piede,
        levels = c("share_touch_R", "share_touch_L"),
        labels = c("Destro", "Sinistro")
      )
    )
  
  ggplot(dati, aes(x = week_id, y = valore, color = piede, group = piede)) +
    geom_line(linewidth = 1.4) +
    geom_point(
      size = 2.6,
      shape = 21,
      stroke = 0.9,
      aes(fill = piede),
      color = "black",
      show.legend = FALSE
    ) +
    scale_y_continuous(labels = function(x) paste0(round(x, 0), "%")) +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
    labs(
      title = paste("Uso del Piede –", giocatore),
      x = "Settimana",
      y = "Percentuale tocchi",
      color = NULL
    ) +
    theme_fn() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
    )
}

# =========================================================================
# TREND MIX POSSESSO U13
# =========================================================================

trend_tecnico_possesso_u13 <- function(giocatore,
                                       ctx_skill,
                                       theme_fn = theme_crema_pro) {
  
  dati <- ctx_skill$agg_week %>%
    filter(player == giocatore) %>%
    select(week_id, share_1T, share_short, share_long) %>%
    pivot_longer(
      cols = starts_with("share_"),
      names_to = "tipo",
      values_to = "valore"
    ) %>%
    mutate(
      valore = valore * 100,
      week_id = factor(week_id, levels = sort(unique(week_id))),
      tipo = factor(
        tipo,
        levels = c("share_1T", "share_short", "share_long"),
        labels = c("One-touch", "Possesso corto", "Possesso lungo")
      )
    )
  
  ggplot(dati, aes(x = week_id, y = valore, fill = tipo, group = tipo)) +
    geom_area(position = "fill", alpha = 0.8) +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
    labs(
      title = paste("Mix di Possesso –", giocatore),
      x = "Settimana",
      y = "Distribuzione % possesso",
      fill = NULL
    ) +
    theme_fn()
}