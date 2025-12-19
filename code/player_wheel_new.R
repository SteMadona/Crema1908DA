library(dplyr)
library(tidyr)
library(ggplot2)

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
                      p_pass_perc     = "Pass %",      # <- nuovo
                      p_regains       = "Regains"      # <- nuovo
      ),
      
      group = case_when(
        metric %in% c("Workrate","Top speed","HID","Acc/Dec index") ~ "Physical",
        metric %in% c("Touch / min","Releases / min","Release vel avg","One-touch %") ~ "On-ball",
        TRUE ~ "Pass/Def"  # <- spicchi grigi
      ),
      
      metric = factor(metric, levels = c(
        "Workrate","Top speed","HID","Acc/Dec index",
        "Touch / min","Releases / min","Release vel avg","One-touch %",
        "Pass %","Regains"
      )),
      
      id = as.integer(metric),
      
      # colori spicchi
      fill = case_when(
        group == "Physical" ~ "#FF2E2E",
        group == "On-ball"  ~ "white",
        TRUE                ~ "#8d8d8d"   # grigio
      ),
      
      # colore numeri (nero solo su bianco)
      txt = if_else(group == "On-ball", "#000000", "white"),
      
      value_plot = coalesce(value, 0),
      value_lab  = if_else(is.na(value), "NA", sprintf("%d", round(value))),
      
      angle = 90 - 360 * (id - 0.5) / n(),
      hjust = if_else(angle < -90, 1, 0),
      angle = if_else(angle < -90, angle + 180, angle)
    )
  
  # linee radiali (ora 10 spicchi)
  radial_lines <- tibble(x = seq(0.5, 10.5, by = 1))
  
  ggplot(dfp, aes(x = id, y = value_plot)) +
    
    geom_col(aes(fill = fill),
             width = 1,
             color = "#0a0a0a",
             linewidth = 1.1,
             show.legend = FALSE
    ) +
    
    geom_hline(yintercept = c(25, 50, 75, 100),
               linetype = c("dashed","dashed","dashed","solid"),
               color = "#2a2a2a",
               linewidth = c(0.55,0.55,0.55,1.0)
    ) +
    
    geom_vline(data = radial_lines, aes(xintercept = x),
               color = "#1a1a1a", linewidth = 0.9
    ) +
    
    # separatori: Physical | On-ball | Grey | fine
    geom_vline(xintercept = c(4.5, 8.5, 10.5),
               color = "#444444", linewidth = 1.4
    ) +
    
    # numeri (colore dinamico + halo)
    geom_text(aes(label = value_lab, color = txt),
              fontface = "bold", size = 4.6
    ) +
    geom_text(aes(label = value_lab),
              color = "cyan", fontface = "bold", size = 4.6,
              alpha = 0.35, nudge_y = 0.15
    ) +
    
    geom_text(aes(y = 118, label = metric, angle = angle, hjust = hjust),
              color = "white",
              size = 3.9,
              fontface = "bold"
    ) +
    
    scale_fill_identity() +
    scale_color_identity() +
    coord_polar(start = pi/2, clip = "off") +
    scale_y_continuous(limits = c(0, 130)) +
    labs(
      title = paste("Sintesi - ", player_name),
      subtitle = "Percentili vs tutti i giocatori"
    ) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "#0b0b0b", color = NA),
      panel.background = element_rect(fill = "#0b0b0b", color = NA),
      plot.title       = element_text(color = "#FF2E2E", face = "bold", size = 16),
      plot.subtitle    = element_text(color = "white"),
      plot.margin      = margin(10, 55, 10, 20)
    )
}

pcts <- make_player_pcts(df_physicalreport, agg, df_passes)
plot_player_wheel(pcts, "T. Serioli")
