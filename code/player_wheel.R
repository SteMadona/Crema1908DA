library(dplyr)
library(tidyr)
library(ggplot2)

make_player_pcts <- function(physical_report, agg) {
  
  # 1) join + feature engineering
  base <- physical_report %>%
    transmute(
      player,
      category,
      HID = HID,
      topspeed = topspeed,
      workrate = workrate,
      accdec_mean = (acc_index + dec_index) / 2
    ) %>%
    inner_join(
      agg %>% transmute(
        player,
        category_agg = category,
        touch_per_min = touch_per_min,
        releases_per_min = releases_per_min,
        release_vel_avg = release_vel_avg,
        touches_total = touches_total,
        one_touch = one_touch,
        one_touch_pct = if_else(touches_total > 0, 100 * one_touch / touches_total, NA_real_)
      ),
      by = "player"
    ) %>%
    mutate(
      # se category è presente in entrambi, preferisco quella del physical, altrimenti uso agg
      category = coalesce(category, category_agg)
    ) %>%
    select(-category_agg)
  
  # 2) calcolo percentili su tutti i NON-portieri
  # percent_rank: 0..1 -> trasformo in 0..100
  base_no_gk <- base %>% filter(category != "Goalkeepers")
  
  pcts_no_gk <- base_no_gk %>%
    mutate(
      p_HID           = 100 * percent_rank(HID),
      p_topspeed       = 100 * percent_rank(topspeed),
      p_workrate       = 100 * percent_rank(workrate),
      p_accdec_mean    = 100 * percent_rank(accdec_mean),
      p_touch_per_min  = 100 * percent_rank(touch_per_min),
      p_rel_per_min    = 100 * percent_rank(releases_per_min),
      p_rel_vel_avg    = 100 * percent_rank(release_vel_avg),
      p_one_touch_pct  = 100 * percent_rank(one_touch_pct)
    )
  
  pcts_no_gk
}

plot_player_wheel <- function(pcts_df, player_name) {
  
  dfp <- pcts_df %>%
    filter(player == player_name) %>%
    select(
      player,
      p_workrate, p_topspeed, p_HID, p_accdec_mean,
      p_touch_per_min, p_rel_per_min, p_rel_vel_avg, p_one_touch_pct
    ) %>%
    pivot_longer(cols = -player, names_to = "metric", values_to = "value") %>%
    mutate(
      metric = recode(metric,
                      p_workrate      = "Workrate",
                      p_topspeed      = "Top speed",
                      p_HID          = "HID",
                      p_accdec_mean   = "Acc/Dec index",
                      p_touch_per_min = "Touch / min",
                      p_rel_per_min   = "Releases / min",
                      p_rel_vel_avg   = "Release vel avg",
                      p_one_touch_pct = "One-touch %"
      ),
      group = if_else(metric %in% c("Workrate","Top speed","HID","Acc/Dec index"),
                      "Physical", "On-ball"),
      metric = factor(metric, levels = c(
        "Workrate","Top speed","HID","Acc/Dec index",
        "Touch / min","Releases / min","Release vel avg","One-touch %"
      )),
      id = as.integer(metric),
      
      # colori spicchi
      fill = if_else(group == "Physical", "#FF2E2E", "#000000"),
      
      # testo numeri: nero sulla parte bianca, bianco sulla parte rossa
      txt = if_else(group == "Physical", "white", "#000000"),
      
      # gestione NA senza warning
      value_plot = coalesce(value, 0),
      value_lab  = if_else(is.na(value), "NA", sprintf("%d", round(value))),
      
      # angoli per etichette esterne
      angle = 90 - 360 * (id - 0.5) / n(),
      hjust = if_else(angle < -90, 1, 0),
      angle = if_else(angle < -90, angle + 180, angle)
    )
  
  # linee radiali (una per ogni spicchio) + separatore metà/ metà
  radial_lines <- tibble(x = seq(0.5, 8.5, by = 1))
  
  ggplot(dfp, aes(x = id, y = value_plot)) +
    
    # spicchi con bordi più netti
    geom_col(aes(fill = fill),
             width = 1,
             color = "#0a0a0a",
             linewidth = 1.1,
             show.legend = FALSE) +
    
    # anelli griglia
    geom_hline(yintercept = c(25, 50, 75, 100),
               linetype = c("dashed","dashed","dashed","solid"),
               color = "#2a2a2a",
               linewidth = c(0.55,0.55,0.55,1.0)) +
    
    # linee radiali per ogni spicchio (più nette)
    geom_vline(data = radial_lines, aes(xintercept = x),
               color = "#1a1a1a", linewidth = 0.9) +
    
    # linea di separazione tra le due metà (più evidente)
    geom_vline(xintercept = c(4.5, 8.5),
               color = "#444444", linewidth = 1.4) +
    
    # numeri: più grandi + con halo per leggibilità
    geom_text(aes(label = value_lab),
              color = "cyan", fontface = "bold", size = 4.6) +
    geom_text(aes(label = value_lab),
              color = "#000000", fontface = "bold", size = 4.6,
              alpha = 0.35, nudge_y = 0.15) +
    
    # etichette esterne: più fuori e più “tangenti” al cerchio
    geom_text(aes(y = 118, label = metric, angle = angle),
              hjust = 0.5, 
              color = "black", 
              size = 3.9, 
              fontface = "bold") +
    
    scale_fill_identity() +
    # subito prima di coord_polar()
    scale_x_continuous(
      limits = c(0.5, 8.5),
      breaks = 1:8,
      expand = c(0, 0)
    ) + 
    coord_polar(start = pi/2, clip = "off") +
    scale_y_continuous(limits = c(0, 130)) +
    
    labs(
      title = paste("Sintesi - ",player_name),
      subtitle = "Percentili vs tutti i giocatori"
    ) +
    
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title       = element_text(color = "#FF2E2E", face = "bold", size = 16),
      plot.subtitle    = element_text(color = "black"),
      plot.margin      = margin(10, 55, 10, 20)
    )
}


pcts <- make_player_pcts(df_physicalreport, agg)

plot_player_wheel(pcts, "N. Abba")
