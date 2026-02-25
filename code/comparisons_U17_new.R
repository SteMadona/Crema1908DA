build_compare_dataset_phy_tec_3 <- function(
    ctx_lower, df_phys_lower,
    ctx_mid,   df_phys_mid,
    ctx_upper, df_phys_upper,
    exclude_gk = TRUE,
    squad_labels = c(lower = "U17", mid = "U19", upper = "FT"),
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
      dplyr::mutate(touches_total = touch_L + touch_R) %>%
      dplyr::group_by(player) %>%
      dplyr::summarise(touches_total_avg = mean(touches_total, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(squad = squad_tag)
  }
  
  df_touch <- dplyr::bind_rows(
    touch_avg_one(ctx_lower, squad_labels["lower"]),
    touch_avg_one(ctx_mid,   squad_labels["mid"]),
    touch_avg_one(ctx_upper, squad_labels["upper"])
  )
  
  # ---- 2) base tecnica (agg_all) per squadra ----
  tech_one <- function(ctx, squad_tag) ctx$agg_all %>% dplyr::mutate(squad = squad_tag)
  
  df_tech <- dplyr::bind_rows(
    tech_one(ctx_lower, squad_labels["lower"]),
    tech_one(ctx_mid,   squad_labels["mid"]),
    tech_one(ctx_upper, squad_labels["upper"])
  )
  
  # ---- 3) base fisica per squadra ----
  phys_one <- function(df_phys, squad_tag) {
    df_phys %>%
      dplyr::mutate(squad = squad_tag) %>%
      dplyr::select(player, squad, dplyr::any_of(c("category","z5_min","z6_min","sprint_dist","workrate")))
  }
  
  df_phys <- dplyr::bind_rows(
    phys_one(df_phys_lower, squad_labels["lower"]),
    phys_one(df_phys_mid,   squad_labels["mid"]),
    phys_one(df_phys_upper, squad_labels["upper"])
  )
  
  # ---- 4) join su player+squad+category ----
  df_all <- df_tech %>%
    dplyr::left_join(df_phys,  by = c("player","squad","category")) %>%
    dplyr::left_join(df_touch, by = c("player","squad"))
  
  if (exclude_gk) df_all <- df_all %>% dplyr::filter(category != "Goalkeepers")
  
  # ---- 5) indici su scala comune (range sull'unione: LOWER+MID+UPPER) ----
  df_all <- df_all %>%
    dplyr::mutate(
      z5_s     = safe_rescale01(z5_min),
      z6_s     = safe_rescale01(z6_min),
      sprint_s = safe_rescale01(sprint_dist),
      work_s   = safe_rescale01(workrate),
      
      rel_s    = safe_rescale01(releases_per_min),
      vel_s    = safe_rescale01(release_vel_avg),
      touch_s  = safe_rescale01(touches_total_avg),
      
      physical_index_raw = z5_s*w_phys["z5"] + z6_s*w_phys["z6"] + sprint_s*w_phys["sprint"] + work_s*w_phys["workrate"],
      physical_index     = safe_rescale01(physical_index_raw),
      
      quality_index_raw  = rel_s*w_qual["releases"] + vel_s*w_qual["vel"] + touch_s*w_qual["touches"],
      quality_index      = safe_rescale01(quality_index_raw),
      
      overall = overall_weights["physical"] * physical_index +
        overall_weights["quality"]  * quality_index
    )
  
  # ---- 6) medie ruolo per tutte le squadre ----
  df_role_means <- df_all %>%
    dplyr::group_by(squad, category) %>%
    dplyr::summarise(
      physical_index = mean(physical_index, na.rm = TRUE),
      quality_index  = mean(quality_index,  na.rm = TRUE),
      overall        = mean(overall,        na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---- 7) top per ruolo per MID e UPPER (precalcolati per ogni metrica) ----
  top_by_squad <- function(df_squad, metric) {
    if (nrow(df_squad) == 0) {
      return(tibble::tibble(category = character(), player = character(),
                            physical_index = numeric(), quality_index = numeric(), overall = numeric()))
    }
    df_squad %>%
      dplyr::group_by(category) %>%
      dplyr::slice_max(.data[[metric]], n = 1, with_ties = FALSE) %>%
      dplyr::ungroup() %>%
      dplyr::select(category, player, physical_index, quality_index, overall)
  }
  
  mid_tag   <- squad_labels["mid"]
  upper_tag <- squad_labels["upper"]
  
  df_mid   <- df_all %>% dplyr::filter(squad == mid_tag)
  df_upper <- df_all %>% dplyr::filter(squad == upper_tag)
  
  df_top_mid <- list(
    overall  = top_by_squad(df_mid,   "overall"),
    quality  = top_by_squad(df_mid,   "quality_index"),
    physical = top_by_squad(df_mid,   "physical_index")
  )
  
  df_top_upper <- list(
    overall  = top_by_squad(df_upper, "overall"),
    quality  = top_by_squad(df_upper, "quality_index"),
    physical = top_by_squad(df_upper, "physical_index")
  )
  
  list(
    df_all = df_all,
    role_means = df_role_means,
    top_mid = df_top_mid,
    top_upper = df_top_upper,
    squad_labels = squad_labels
  )
}


plot_compare_phy_tec_player_3 <- function(
    compare_ds,
    player_name,
    top_by = c("overall","quality","physical"),
    show_cloud = TRUE,
    palette = list(
      lower = "#FF2E2E",  # U17
      mid   = "black",    # U19
      upper = "#1F78B4"   # FT
    )
) {
  top_by <- match.arg(top_by)
  
  df_all   <- compare_ds$df_all
  df_means <- compare_ds$role_means
  df_top_m <- compare_ds$top_mid[[top_by]]
  df_top_u <- compare_ds$top_upper[[top_by]]
  
  lower_tag <- compare_ds$squad_labels["lower"]
  mid_tag   <- compare_ds$squad_labels["mid"]
  upper_tag <- compare_ds$squad_labels["upper"]
  
  # --- prendo riga giocatore (deve essere LOWER; se è anche altrove, preferisco LOWER) ---
  df_player <- df_all %>%
    dplyr::filter(player == player_name) %>%
    dplyr::arrange(dplyr::desc(squad == lower_tag)) %>%
    dplyr::slice(1)
  
  if (nrow(df_player) == 0) stop("Giocatore non trovato nel dataset unificato (controlla nome).")
  
  # se vuoi forzare che sia U17:
  # if (df_player$squad[1] != lower_tag) stop("Il giocatore selezionato non appartiene alla squadra LOWER (U17).")
  
  role <- df_player$category[1]
  
  # --- medie ruolo (lower/mid/upper) ---
  mean_lower <- df_means %>%
    dplyr::filter(squad == lower_tag, category == role) %>%
    dplyr::mutate(label = paste0("Media ruolo ", lower_tag),
                  point_type = "Media ruolo U17",
                  squad_ref = "lower")
  
  mean_mid <- df_means %>%
    dplyr::filter(squad == mid_tag, category == role) %>%
    dplyr::mutate(label = paste0("Media ruolo ", mid_tag),
                  point_type = "Media ruolo U19",
                  squad_ref = "mid")
  
  mean_upper <- df_means %>%
    dplyr::filter(squad == upper_tag, category == role) %>%
    dplyr::mutate(label = paste0("Media ruolo ", upper_tag),
                  point_type = "Media ruolo FT",
                  squad_ref = "upper")
  
  # --- top ruolo mid (U19) ---
  top_mid_role <- df_top_m %>%
    dplyr::filter(category == role) %>%
    dplyr::mutate(
      label = ifelse(is.na(player) | player == "",
                     paste0("Top ", mid_tag),
                     paste0("Top ", mid_tag, ": ", player)),
      point_type = "Top ruolo U19",
      squad_ref  = "mid"
    )
  
  if (nrow(top_mid_role) == 0) {
    top_mid_role <- mean_mid %>%
      dplyr::transmute(category, player = NA_character_,
                       physical_index, quality_index, overall,
                       label = paste0("Top ", mid_tag),
                       point_type = "Top ruolo U19",
                       squad_ref  = "mid")
  }
  
  # --- top ruolo upper (FT) ---
  top_upper_role <- df_top_u %>%
    dplyr::filter(category == role) %>%
    dplyr::mutate(
      label = ifelse(is.na(player) | player == "",
                     paste0("Top ", upper_tag),
                     paste0("Top ", upper_tag, ": ", player)),
      point_type = "Top ruolo FT",
      squad_ref  = "upper"
    )
  
  if (nrow(top_upper_role) == 0) {
    top_upper_role <- mean_upper %>%
      dplyr::transmute(category, player = NA_character_,
                       physical_index, quality_index, overall,
                       label = paste0("Top ", upper_tag),
                       point_type = "Top ruolo FT",
                       squad_ref  = "upper")
  }
  
  # --- punto giocatore ---
  player_point <- df_player %>%
    dplyr::transmute(category,
                     player,
                     physical_index,
                     quality_index,
                     overall,
                     label = player_name,
                     point_type = "Giocatore U17",
                     squad_ref = "lower")
  
  df_points <- dplyr::bind_rows(
    player_point,
    mean_lower, mean_mid, top_mid_role, mean_upper, top_upper_role
  ) %>%
    dplyr::mutate(
      point_type = factor(point_type, levels = c(
        "Giocatore U17",
        "Media ruolo U17",
        "Media ruolo U19",
        "Top ruolo U19",
        "Media ruolo FT",
        "Top ruolo FT"
      )),
      border_col = dplyr::case_when(
        squad_ref == "lower" ~ palette$lower,
        squad_ref == "mid"   ~ palette$mid,
        squad_ref == "upper" ~ palette$upper,
        TRUE ~ "black"
      ),
      fill_col = dplyr::case_when(
        point_type %in% c("Giocatore U17", "Top ruolo U19", "Top ruolo FT") ~ border_col,
        TRUE ~ "white"
      )
    )
  
  ggplot2::ggplot() +
    { if (show_cloud)
      ggplot2::geom_point(
        data = df_all,
        ggplot2::aes(x = physical_index, y = quality_index),
        shape = 16, size = 2.2, alpha = 0.12, color = "gray70"
      )
    } +
    
    ggplot2::geom_point(
      data = df_points,
      ggplot2::aes(x = physical_index, y = quality_index,
                   color = point_type, fill = point_type),
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
    
    # colori/riempimenti per i 6 punti (derivati da palette e da "pieno/vuoto")
    ggplot2::scale_color_manual(
      values = stats::setNames(df_points$border_col, df_points$point_type) |> as.list() |> unlist()
    ) +
    ggplot2::scale_fill_manual(
      values = stats::setNames(df_points$fill_col, df_points$point_type) |> as.list() |> unlist()
    ) +
    
    ggplot2::guides(
      fill = "none",
      color = ggplot2::guide_legend(
        override.aes = list(
          shape  = rep(21, 6),
          size   = rep(5, 6),
          stroke = rep(1.2, 6),
          
          fill = c(
            palette$lower,  # Giocatore U17 (pieno)
            "white",        # Media ruolo U17 (vuoto)
            "white",        # Media ruolo U19 (vuoto)
            palette$mid,    # Top ruolo U19 (pieno)
            "white",        # Media ruolo FT (vuoto)
            palette$upper   # Top ruolo FT (pieno)
          ),
          
          colour = c(
            palette$lower,  # Giocatore U17
            palette$lower,  # Media ruolo U17
            palette$mid,    # Media ruolo U19
            palette$mid,    # Top ruolo U19
            palette$upper,  # Media ruolo FT
            palette$upper   # Top ruolo FT
          )
        )
      )
    ) +
    
    ggplot2::scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25), expand = c(0,0)) +
    ggplot2::scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25), expand = c(0,0)) +
    
    ggplot2::labs(
      title = "Confronto Fisico vs Tecnica",
      x = "Indice Qualità Fisica (0–1)",
      y = "Indice Qualità Tecnica (0–1)",
      color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 14) +
    theme_crema_pro()
}


