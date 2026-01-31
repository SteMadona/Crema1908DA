library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)

build_compare_dataset_phy_tec <- function(
    ctx_lower, df_phys_lower,
    ctx_upper, df_phys_upper,
    exclude_gk = TRUE,
    squad_labels = c(lower = "Lower", upper = "Upper"),
    w_phys = c(z5 = 0.2, z6 = 0.2, sprint = 0.1, workrate = 0.5),
    w_qual = c(releases = 0.4, vel = 0.4, touches = 0.2),
    overall_weights = c(physical = 0.5, quality = 0.5)
) {
  
  # ---- helper: rescale 0-1 robusto (gestisce range costante) ----
  safe_rescale01 <- function(x) {
    r <- range(x, na.rm = TRUE)
    if (!is.finite(r[1]) || !is.finite(r[2]) || r[1] == r[2]) return(rep(0.5, length(x)))
    scales::rescale(x, to = c(0, 1), from = r)
  }
  
  # ---- 1) tocchi medi per squadra ----
  touch_avg_one <- function(ctx, squad_tag) {
    ctx$df_skill %>%
      mutate(touches_total = touch_L + touch_R) %>%
      group_by(player) %>%
      summarise(touches_total_avg = mean(touches_total, na.rm = TRUE), .groups = "drop") %>%
      mutate(squad = squad_tag)
  }
  
  df_touch <- bind_rows(
    touch_avg_one(ctx_lower, squad_labels["lower"]),
    touch_avg_one(ctx_upper, squad_labels["upper"])
  )
  
  # ---- 2) base tecnica (agg_all) per squadra ----
  tech_one <- function(ctx, squad_tag) ctx$agg_all %>% mutate(squad = squad_tag)
  
  df_tech <- bind_rows(
    tech_one(ctx_lower, squad_labels["lower"]),
    tech_one(ctx_upper, squad_labels["upper"])
  )
  
  # ---- 3) base fisica per squadra ----
  phys_one <- function(df_phys, squad_tag) {
    df_phys %>%
      mutate(squad = squad_tag) %>%
      select(player, squad, any_of(c("category","z5_min","z6_min","sprint_dist","workrate")))
  }
  
  df_phys <- bind_rows(
    phys_one(df_phys_lower, squad_labels["lower"]),
    phys_one(df_phys_upper, squad_labels["upper"])
  )
  
  # ---- 4) join su player+squad+category ----
  df_all <- df_tech %>%
    left_join(df_phys,  by = c("player","squad","category")) %>%
    left_join(df_touch, by = c("player","squad"))
  
  if (exclude_gk) df_all <- df_all %>% filter(category != "Goalkeepers")
  
  # ---- 5) indici su scala comune (range sull'unione) ----
  df_all <- df_all %>%
    mutate(
      z5_s     = safe_rescale01(z5_min),
      z6_s     = safe_rescale01(z6_min),
      sprint_s = safe_rescale01(sprint_dist),
      work_s   = safe_rescale01(workrate),
      
      rel_s   = safe_rescale01(releases_per_min),
      vel_s   = safe_rescale01(release_vel_avg),
      touch_s = safe_rescale01(touches_total_avg),
      
      physical_index_raw = z5_s*w_phys["z5"] + z6_s*w_phys["z6"] + sprint_s*w_phys["sprint"] + work_s*w_phys["workrate"],
      physical_index     = safe_rescale01(physical_index_raw),
      
      quality_index_raw  = rel_s*w_qual["releases"] + vel_s*w_qual["vel"] + touch_s*w_qual["touches"],
      quality_index      = safe_rescale01(quality_index_raw),
      
      overall = overall_weights["physical"] * physical_index +
        overall_weights["quality"]  * quality_index
    )
  
  # ---- 6) medie ruolo per entrambe le squadre (tutte le category) ----
  df_role_means <- df_all %>%
    group_by(squad, category) %>%
    summarise(
      physical_index = mean(physical_index, na.rm = TRUE),
      quality_index  = mean(quality_index,  na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- 7) top upper per ruolo (precalcolati per tutti i ruoli) ----
  upper_tag <- squad_labels["upper"]
  
  df_upper <- df_all %>% filter(squad == upper_tag)
  
  top_one <- function(metric) {
    if (nrow(df_upper) == 0) {
      return(tibble(category = character(), player = character(),
                    physical_index = numeric(), quality_index = numeric(), overall = numeric()))
    }
    
    df_upper %>%
      group_by(category) %>%
      slice_max(.data[[metric]], n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(category, player, physical_index, quality_index, overall)
  }
  
  df_top_upper <- list(
    overall  = top_one("overall"),
    quality  = top_one("quality_index"),
    physical = top_one("physical_index")
  )
  
  list(
    df_all = df_all,
    role_means = df_role_means,
    top_upper = df_top_upper,
    squad_labels = squad_labels
  )
}


plot_compare_phy_tec_player <- function(compare_ds, player_name, top_by = c("overall","quality","physical"), show_cloud = TRUE) {
  top_by <- match.arg(top_by)
  
  df_all <- compare_ds$df_all
  df_means <- compare_ds$role_means
  df_top   <- compare_ds$top_upper[[top_by]]
  
  lower_tag <- compare_ds$squad_labels["lower"]
  upper_tag <- compare_ds$squad_labels["upper"]
  
  # prendo riga giocatore (se appare in entrambe, preferisco lower)
  df_player <- df_all %>%
    filter(player == player_name) %>%
    arrange(desc(squad == lower_tag)) %>%
    slice(1)
  
  if (nrow(df_player) == 0) stop("Giocatore non trovato nel dataset unificato (controlla nome).")
  
  role <- df_player$category
  
  # medie ruolo lower/upper
  mean_lower <- df_means %>%
    filter(squad == lower_tag, category == role) %>%
    mutate(label = paste0("Media ruolo ", lower_tag),
           point_type = "Media squadra")
  
  mean_upper <- df_means %>%
    filter(squad == upper_tag, category == role) %>%
    mutate(label = paste0("Media ruolo ", upper_tag),
           point_type = "Media cat. superiore")
  
  # top upper nel ruolo
  top_upper_role <- df_top %>%
    filter(category == role) %>%
    mutate(
      label = ifelse(is.na(player) | player == "",
                     paste0("Top ", upper_tag),
                     paste0("Top ", upper_tag, ": ", player)),
      point_type = "Top cat. superiore"
    )
  
  # se upper non ha quel ruolo, fallback “vuoto” sul punto media upper (così non esplode)
  if (nrow(top_upper_role) == 0) {
    top_upper_role <- mean_upper %>%
      transmute(category, player = NA_character_,
                physical_index, quality_index,
                label = paste0("Top ", upper_tag),
                point_type = "Top cat. superiore")
  }
  
  player_point <- df_player %>%
    transmute(category,
              player,
              physical_index,
              quality_index,
              label = player_name,
              point_type = "Giocatore")
  
  df_points <- bind_rows(player_point, mean_lower, mean_upper, top_upper_role) %>%
    mutate(
      point_type = factor(point_type, levels = c("Giocatore","Media squadra","Media cat. superiore","Top cat. superiore"))
    )
  
  ggplot() +
    { if (show_cloud)
      geom_point(
        data = df_all,
        aes(x = physical_index, y = quality_index),
        shape = 16, size = 2.2, alpha = 0.12, color = "gray70"
      )
    } +
    
    # 4 punti focus: shape 21 (fill/border)
    geom_point(
      data = df_points,
      aes(x = physical_index, y = quality_index, color = point_type, fill = point_type),
      shape = 21,
      size = 7,
      stroke = 1.2,
      alpha = 1
    ) +
    
    ggrepel::geom_label_repel(
      data = df_points,
      aes(x = physical_index, y = quality_index, label = label),
      size = 4.5,
      fill = "white",
      color = "black",
      label.padding = grid::unit(0.25, "lines"),
      label.r = grid::unit(0.25, "lines"),
      box.padding = 0.5,
      point.padding = 0.35,
      max.overlaps = Inf
    ) +
    
    # bordo: rosso per giocatore + media squadra, nero per upper
    scale_color_manual(values = c(
      "Giocatore"            = "#FF2E2E",
      "Media squadra"        = "#FF2E2E",
      "Media cat. superiore" = "black",
      "Top cat. superiore"   = "black"
    )) +
    
    # fill: pieni per giocatore/top, vuoti per le medie
    scale_fill_manual(values = c(
      "Giocatore"            = "#FF2E2E",
      "Media squadra"        = "white",
      "Media cat. superiore" = "white",
      "Top cat. superiore"   = "black"
    )) +
    
    guides(
      fill = "none",
      color = guide_legend(
        override.aes = list(
          shape = rep(21, 4),
          fill  = c("#FF2E2E", "white", "white", "black"),
          color = c("#FF2E2E", "#FF2E2E", "black", "black"),
          size  = rep(5, 4),
          stroke = rep(1.2, 4)
        )
      )
    ) +
    
    scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25), expand = c(0,0)) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25), expand = c(0,0)) +
    
    labs(
      title = "📌 Confronto Fisico vs Tecnica (scala comune)",
      x = "Indice Qualità Fisica (0–1)",
      y = "Indice Qualità Tecnica (0–1)",
      color = NULL
    ) +
    theme_minimal(base_size = 14) +
    theme_crema_pro()
}

