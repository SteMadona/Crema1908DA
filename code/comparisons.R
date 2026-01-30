library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)

build_compare_scatter_phy_tec <- function(
    ctx_lower, df_phys_lower,
    ctx_upper, df_phys_upper,
    player_name,
    exclude_gk = TRUE,
    squad_labels = c(lower = "Lower", upper = "Upper"),
    # pesi indici (come i tuoi)
    w_phys = c(z5 = 0.2, z6 = 0.2, sprint = 0.1, workrate = 0.5),
    w_qual = c(releases = 0.4, vel = 0.4, touches = 0.2),
    # come selezionare il "top reparto" nella categoria superiore
    top_by = c("overall", "quality", "physical"),
    overall_weights = c(physical = 0.5, quality = 0.5)
) {
  top_by <- match.arg(top_by)
  
  # ---- helper: rescale 0-1 robusto (gestisce range costante) ----
  safe_rescale01 <- function(x) {
    r <- range(x, na.rm = TRUE)
    if (!is.finite(r[1]) || !is.finite(r[2]) || r[1] == r[2]) return(rep(0.5, length(x)))
    scales::rescale(x, to = c(0, 1), from = r)
  }
  
  # ---- 1) tocchi medi per ciascuna squadra ----
  touch_avg_one <- function(ctx, squad_tag) {
    ctx$df_skill %>%
      dplyr::mutate(touches_total = touch_L + touch_R) %>%
      dplyr::group_by(player) %>%
      dplyr::summarise(touches_total_avg = mean(touches_total, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(squad = squad_tag)
  }
  
  df_touch <- dplyr::bind_rows(
    touch_avg_one(ctx_lower, squad_labels["lower"]),
    touch_avg_one(ctx_upper, squad_labels["upper"])
  )
  
  # ---- 2) base tecnica (agg_all) per ciascuna squadra ----
  tech_one <- function(ctx, squad_tag) {
    ctx$agg_all %>%
      dplyr::mutate(squad = squad_tag)
  }
  
  df_tech <- dplyr::bind_rows(
    tech_one(ctx_lower, squad_labels["lower"]),
    tech_one(ctx_upper, squad_labels["upper"])
  )
  
  # ---- 3) base fisica per ciascuna squadra ----
  phys_one <- function(df_phys, squad_tag) {
    df_phys %>%
      dplyr::mutate(squad = squad_tag) %>%
      dplyr::select(player, squad, dplyr::any_of(c("category","z5_min","z6_min","sprint_dist","workrate")))
  }
  
  df_phys <- dplyr::bind_rows(
    phys_one(df_phys_lower, squad_labels["lower"]),
    phys_one(df_phys_upper, squad_labels["upper"])
  )
  
  # ---- 4) unifico (join su player+squad per sicurezza) ----
  df_all <- df_tech %>%
    dplyr::left_join(df_phys, by = c("player","squad","category")) %>%
    dplyr::left_join(df_touch, by = c("player","squad"))
  
  if (exclude_gk) {
    df_all <- df_all %>% dplyr::filter(category != "Goalkeepers")
  }
  
  # ---- 5) indici su scala comune (range calcolato sull'unione) ----
  df_all <- df_all %>%
    dplyr::mutate(
      z5_s      = safe_rescale01(z5_min),
      z6_s      = safe_rescale01(z6_min),
      sprint_s  = safe_rescale01(sprint_dist),
      work_s    = safe_rescale01(workrate),
      rel_s     = safe_rescale01(releases_per_min),
      vel_s     = safe_rescale01(release_vel_avg),
      touch_s   = safe_rescale01(touches_total_avg),
      
      physical_index_raw = z5_s*w_phys["z5"] + z6_s*w_phys["z6"] + sprint_s*w_phys["sprint"] + work_s*w_phys["workrate"],
      physical_index     = safe_rescale01(physical_index_raw),
      
      quality_index_raw  = rel_s*w_qual["releases"] + vel_s*w_qual["vel"] + touch_s*w_qual["touches"],
      quality_index      = safe_rescale01(quality_index_raw)
    )
  
  # ---- 6) individuo riga giocatore (preferisco Lower se esiste) ----
  df_player <- df_all %>%
    dplyr::filter(player == player_name) %>%
    dplyr::arrange(dplyr::desc(squad == squad_labels["lower"])) %>%
    dplyr::slice(1)
  
  if (nrow(df_player) == 0) stop("Giocatore non trovato nel dataset unificato (controlla nome).")
  
  player_role  <- df_player$category
  lower_tag    <- squad_labels["lower"]
  upper_tag    <- squad_labels["upper"]
  
  # ---- 7) medie ruolo per lower e upper ----
  mean_role <- function(df, squad_tag, role) {
    df %>%
      dplyr::filter(squad == squad_tag, category == role) %>%
      dplyr::summarise(
        physical_index = mean(physical_index, na.rm = TRUE),
        quality_index  = mean(quality_index,  na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(player = NA_character_, squad = squad_tag, category = role)
  }
  
  df_mean_lower <- mean_role(df_all, lower_tag, player_role)
  df_mean_upper <- mean_role(df_all, upper_tag, player_role)
  
  # ---- 8) top reparto upper (stesso ruolo) ----
  df_upper_role <- df_all %>% dplyr::filter(squad == upper_tag, category == player_role)
  
  if (nrow(df_upper_role) == 0) {
    df_top_upper <- df_mean_upper %>% dplyr::mutate(player = NA_character_)
  } else {
    df_upper_role <- df_upper_role %>%
      dplyr::mutate(
        overall = overall_weights["physical"] * physical_index + overall_weights["quality"] * quality_index
      )
    
    df_top_upper <- switch(
      top_by,
      overall  = df_upper_role %>% dplyr::slice_max(overall, n = 1, with_ties = FALSE),
      quality  = df_upper_role %>% dplyr::slice_max(quality_index, n = 1, with_ties = FALSE),
      physical = df_upper_role %>% dplyr::slice_max(physical_index, n = 1, with_ties = FALSE)
    )
  }
  
  # ---- 9) costruisco i 4 punti da plottare ----
  df_points <- dplyr::bind_rows(
    df_player %>% dplyr::mutate(point_type = "Giocatore", label = player_name),
    
    df_mean_lower %>% dplyr::mutate(
      point_type = "Media ruolo (squadra appart.)",
      label = paste0("Media ruolo ", lower_tag)
    ),
    
    df_mean_upper %>% dplyr::mutate(
      point_type = "Media ruolo (cat. superiore)",
      label = paste0("Media ruolo ", upper_tag)
    ),
    
    df_top_upper %>% dplyr::mutate(
      point_type = "Top reparto (cat. superiore)",
      label = ifelse(is.na(player), paste0("Top ", upper_tag), paste0("Top ", upper_tag, ": ", player))
    )
  ) %>%
    dplyr::select(label, point_type, squad, category, physical_index, quality_index)
  
  list(
    df_all = df_all,
    df_points = df_points,
    player_role = player_role
  )
}


plot_compare_phy_tec <- function(compare_data, show_cloud = TRUE) {
  df_all    <- compare_data$df_all
  df_points <- compare_data$df_points
  
  # ricodifico i 4 tipi nel modo che vuoi (nome + ordine legenda)
  df_points <- df_points %>%
    dplyr::mutate(
      point_type = dplyr::case_when(
        point_type == "Giocatore"                     ~ "Giocatore",
        point_type == "Media ruolo (squadra appart.)" ~ "Media squadra",
        point_type == "Media ruolo (cat. superiore)"  ~ "Media cat. superiore",
        point_type == "Top reparto (cat. superiore)"  ~ "Top cat. superiore",
        TRUE ~ point_type
      ),
      point_type = factor(
        point_type,
        levels = c("Giocatore", "Media squadra", "Media cat. superiore", "Top cat. superiore")
      )
    )
  
  p <- ggplot2::ggplot() +
    
    # nuvola leggera (se vuoi)
    { if (show_cloud)
      ggplot2::geom_point(
        data = df_all,
        ggplot2::aes(x = physical_index, y = quality_index),
        shape = 16, size = 2.2, alpha = 0.12, color = "gray70"
      )
    } +
    
    # --- 4 punti focus: cerchio sempre, cambia fill (pieno/vuoto) + colore bordo ---
    ggplot2::geom_point(
      data = df_points,
      ggplot2::aes(x = physical_index, y = quality_index, color = point_type, fill = point_type),
      shape = 21,
      size = 7,
      stroke = 1.2,
      alpha = 1
    ) +
    
    ggrepel::geom_label_repel(
      data = df_points,
      ggplot2::aes(x = physical_index, y = quality_index, label = label),
      size = 4.5,
      fill = "white",
      color = "black",
      label.padding = grid::unit(0.25, "lines"),
      label.r = grid::unit(0.25, "lines"),
      box.padding = 0.5,
      point.padding = 0.35,
      max.overlaps = Inf
    ) +
    
    # colori bordo: rosso per giocatore + media squadra, nero per upper
    ggplot2::scale_color_manual(values = c(
      "Giocatore"            = "#FF2E2E",
      "Media squadra"        = "#FF2E2E",
      "Media cat. superiore" = "black",
      "Top cat. superiore"   = "black"
    )) +
    
    # fill: pieni per giocatore/top, vuoti (bianchi) per le medie
    ggplot2::scale_fill_manual(values = c(
      "Giocatore"            = "#FF2E2E",
      "Media squadra"        = "white",
      "Media cat. superiore" = "white",
      "Top cat. superiore"   = "black"
    )) +
    
    # legenda “corretta”: una sola (color), con fill coerente per ogni voce
    ggplot2::guides(
      fill = "none",
      color = ggplot2::guide_legend(
        override.aes = list(
          shape = rep(21, 4),
          fill  = c("#FF2E2E", "white", "white", "black"),
          color = c("#FF2E2E", "#FF2E2E", "black", "black"),
          size  = rep(5, 4),
          stroke = rep(1.2, 4)
        )
      )
    ) +
    
    ggplot2::scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25), expand = c(0,0)) +
    ggplot2::scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25), expand = c(0,0)) +
    
    ggplot2::labs(
      title = "📌 Confronto Fisico vs Tecnica (scala comune)",
      x = "Indice Qualità Fisica (0–1)",
      y = "Indice Qualità Tecnica (0–1)",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    theme_crema_pro()
  
  p
}





phy_u19 <- prep_physical_context(df_u19, players_u19)
phy_u19_rep <- phy_u19$df_physicalreport

tech_u19 <- prep_skill_context(df_u19, players_u19, "session")

scat_u19 <- build_df_scatter_phy_tec(tech_u19, 
                                     phy_u19_rep, 
                                     players_u19)

phy_ft <- prep_physical_context(df_ft, players_ft)
phy_ft_rep <- phy_ft$df_physicalreport

tech_ft <- prep_skill_context(df_ft, players_ft, "session")

scat <- build_compare_scatter_phy_tec(tech_u19, phy_u19_rep, 
                                tech_ft, phy_ft_rep, 
                                "C. Barzi")

plot_compare_phy_tec(scat)
