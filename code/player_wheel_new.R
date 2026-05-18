make_player_pcts <- function(physical_report, agg, passes = NULL) {
  
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
    mutate(category = coalesce(category, category_agg)) %>%
    select(-category_agg)
  
  # 1b) aggiungo df_passes se presente
  if (!is.null(passes)) {
    base <- base %>%
      left_join(
        passes %>%
          transmute(
            player,
            category_pass = position,
            pass_perc = pass_perc,
            regains = regains
          ),
        by = "player"
      ) %>%
      mutate(category = coalesce(category, category_pass)) %>%
      select(-category_pass)
  } else {
    base <- base %>%
      mutate(pass_perc = NA_real_, regains = NA_real_)
  }
  
  # 2) percentili su tutti i NON-portieri
  base_no_gk <- base %>% filter(category != "Goalkeepers")
  
  pcts_no_gk <- base_no_gk %>%
    mutate(
      p_HID           = 100 * percent_rank(HID),
      p_topspeed      = 100 * percent_rank(topspeed),
      p_workrate      = 100 * percent_rank(workrate),
      p_accdec_mean   = 100 * percent_rank(accdec_mean),
      p_touch_per_min = 100 * percent_rank(touch_per_min),
      p_rel_per_min   = 100 * percent_rank(releases_per_min),
      p_rel_vel_avg   = 100 * percent_rank(release_vel_avg),
      p_one_touch_pct = 100 * percent_rank(one_touch_pct),
      p_pass_perc     = 100 * percent_rank(pass_perc),
      p_regains       = 100 * percent_rank(regains)
    )
  
  pcts_no_gk
}


plot_player_wheel <- function(pcts_df, player_name) {
  
  dfp <- pcts_df %>%
    filter(player == player_name) %>%
    select(
      player,
      p_workrate, p_topspeed, p_HID, p_accdec_mean,
      p_touch_per_min, p_rel_per_min, p_rel_vel_avg, p_one_touch_pct,
      p_pass_perc, p_regains
    ) %>%
    pivot_longer(cols = -player, names_to = "metric", values_to = "value") %>%
    mutate(
      metric = recode(metric,
                      p_workrate      = "Workrate",
                      p_topspeed      = "Top speed",
                      p_HID           = "HID",
                      p_accdec_mean   = "Acc/Dec index",
                      p_touch_per_min = "Touch / min",
                      p_rel_per_min   = "Releases / min",
                      p_rel_vel_avg   = "Release vel avg",
                      p_one_touch_pct = "One-touch %",
                      p_pass_perc     = "Pass %",
                      p_regains       = "Regains"
      ),
      
      group = case_when(
        metric %in% c("Workrate","Top speed","HID","Acc/Dec index") ~ "Physical",
        metric %in% c("Touch / min","Releases / min","Release vel avg","One-touch %") ~ "On-ball",
        TRUE ~ "Pass/Def"
      ),
      
      metric = factor(metric, levels = c(
        "Workrate","Top speed","HID","Acc/Dec index",
        "Touch / min","Releases / min","Release vel avg","One-touch %",
        "Pass %","Regains"
      )),
      
      id = as.integer(metric),
      
      fill = case_when(
        group == "Physical" ~ "#FF2E2E",
        group == "On-ball"  ~ "#000000",
        TRUE                ~ "#8d8d8d"
      ),
      
      value_plot = coalesce(value, 0),
      value_lab  = if_else(is.na(value), "NA", sprintf("%d", round(value))),
      
      # angolo per le etichette delle metriche
      angle_lab = 90 - 360 * (id - 0.5) / n(),
      hjust_lab = if_else(angle_lab < -90, 1, 0),
      angle_lab = if_else(angle_lab < -90, angle_lab + 180, angle_lab)
    )
  
  radial_lines <- tibble(x = seq(0.5, 10.5, by = 1))
  
  ggplot(dfp, aes(x = id, y = value_plot)) +
    
    # spicchi
    geom_col(
      aes(fill = fill),
      width = 1,
      color = "#0a0a0a",
      linewidth = 1.1,
      show.legend = FALSE
    ) +
    
    # cerchi percentili
    geom_hline(
      yintercept = c(25, 50, 75, 100),
      linetype = c("dashed", "dashed", "dashed", "solid"),
      color = "#2a2a2a",
      linewidth = c(0.55, 0.55, 0.55, 1.0)
    ) +
    
    # linee radiali
    geom_vline(
      data = radial_lines,
      aes(xintercept = x),
      color = "#1a1a1a",
      linewidth = 0.9
    ) +
    
    # separatori tra gruppi
    geom_vline(
      xintercept = c(4.5, 8.5, 10.5),
      color = "#444444",
      linewidth = 1.4
    ) +
    
    # lineette che collegano il cerchio al numero esterno
    geom_segment(
      aes(
        x = id,
        xend = id,
        y = 101,
        yend = 109
      ),
      color = "#1a1a1a",
      linewidth = 0.55,
      inherit.aes = FALSE
    ) +
    
    # numeri esterni dentro riquadro
    geom_label(
      aes(
        x = id,
        y = 116,
        label = value_lab
      ),
      fill = "white",
      color = "#00AFAF",
      fontface = "bold",
      size = 4.4,
      label.size = 0.35,
      label.r = unit(0.16, "lines"),
      label.padding = unit(0.18, "lines"),
      inherit.aes = FALSE
    ) +
    
    # nomi metriche più esterni
    geom_text(
      aes(
        y = 132,
        label = metric,
        angle = angle_lab,
        hjust = hjust_lab
      ),
      color = "#111111",
      size = 3.7,
      fontface = "bold"
    ) +
    
    scale_fill_identity() +
    coord_polar(start = pi / 2, clip = "off") +
    scale_y_continuous(
      limits = c(0, 145),
      expand = expansion(mult = c(0, 0))
    ) +
    
    labs(
      title = paste("Sintesi - Giocatore 1"),
      subtitle = "Percentili vs tutti i giocatori"
    ) +
    
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title       = element_text(
        color = "#FF2E2E",
        face = "bold",
        size = 16,
        margin = margin(b = 3)
      ),
      plot.subtitle    = element_text(
        color = "#111111",
        size = 10,
        margin = margin(b = 12)
      ),
      plot.margin      = margin(15, 70, 20, 70)
    )
}
pcts <- make_player_pcts(df_physicalreport, agg, df_passes)
