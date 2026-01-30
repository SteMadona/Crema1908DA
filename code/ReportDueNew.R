library(stringr)


prep_skill_context <- function(df_raw,
                               players = NULL,
                               data_type = c(NULL, "session", "pf", "match"),
                               matches_list = NULL, 
                               exclude_gk = TRUE,
                               DUR_1TOUCH = 0.4,
                               DUR_SHORT  = 2.0,
                               DUR_LONG   = 4.0,
                               RV_WEIGHTS = c(1,2,3,4,5),
                               W_POSSESSION = c(
                                 touch_per_min    = 0.40,
                                 releases_per_min = 0.25,
                                 pos_long_share   = 0.20,
                                 receives_norm    = 0.15
                               )) {
  
  # ---- 1) normalizzo colonne  ----
  df_skill <- df_raw %>%
    transmute(
      date        = as.Date(Date),
      Tag         = as.character(Tag),
      player      = stringr::str_trim(`Player Name`),
      category    = `Position Category`,
      position    = Position,
      
      touch_L     = `Left Leg Touches (#)`,
      touch_R     = `Right Leg Touches (#)`,
      touch_per_min  = `Touches per min (#/min)`,
      
      rv_z1       = `RV Zone 1 [0-5( m/s)]`,
      rv_z2       = `RV Zone 2 [5-10( m/s)]`,
      rv_z3       = `RV Zone 3 [10-15( m/s)]`,
      rv_z4       = `RV Zone 4 [15-20( m/s)]`,
      rv_z5       = `RV Zone 5 [20-25( m/s)]`,
      
      pos_long    = `Long possessions (#)`,
      pos_short   = `Short Possessions (#)`,
      
      one_touch   = `One-Touch (#)`,
      one_touch_R = `One-Touch Right (#)`,
      one_touch_L = `One-Touch Left (#)`,
      
      releases_per_min = `Releases per min (#/min)`,
      release_L        = `Releases Left (#)`,
      release_R        = `Releases Right (#)`,
      release_vel_avg  = `Release Velocity Avg (m/s)`,
      
      receives    = `Receives (#)`,
      receives_L  = `Receives Left (#)`,
      receives_R  = `Receives Right (#)`
    )
  
  if(data_type == "session"){
    df_skill <- df_skill %>% filter(Tag == "Full Session")
  }
  
  if(data_type == "pf"){
    df_skill <- df_skill %>% filter(str_detect(Tag, "Partita finale"))
  }
  
  if(data_type == "match"){
   df_skill <- df_skill %>% filter(Tag %in% matches_list) 
  }
  
  # cast numerici (Tag escluso!)
  num_cols <- setdiff(names(df_skill), c("date","Tag","player","category","position"))
  df_skill[num_cols] <- lapply(df_skill[num_cols], \(x) suppressWarnings(as.numeric(x)))
  
  # ---- 2) filtri (players / tag list / regex / expr) ----
  df_skill <- df_skill %>%
    { if (!is.null(players)) dplyr::filter(., player %in% players) else . } %>%
    mutate(
      week = isoweek(date),
      year = isoyear(date),
      week_id = paste(year, week, sep = "-")
    )
  
  
  # ---- helper: indici (con range sicuri) ----
  safe_rng <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) == 0) c(0, 1) else range(x, na.rm = TRUE)
  }
  
  add_indices <- function(agg_tbl, ref_tbl_non_gk) {
    
    rng_touch_per_min    <- safe_rng(ref_tbl_non_gk$touch_per_min)
    rng_releases_per_min <- safe_rng(ref_tbl_non_gk$releases_per_min)
    rng_receives         <- safe_rng(ref_tbl_non_gk$receives)
    rng_avg_time         <- safe_rng(ref_tbl_non_gk$avg_time_per_poss_sec)
    rng_rv_raw           <- safe_rng(ref_tbl_non_gk$rv_index_raw)
    rng_touches_total    <- safe_rng(ref_tbl_non_gk$touches_total)
    
    out <- agg_tbl %>%
      mutate(
        receives_norm = scales::rescale(receives, to = c(0,1), from = rng_receives),
        pos_long_share = share_long,
        possession_index_raw =
          W_POSSESSION["touch_per_min"]    * scales::rescale(touch_per_min,    to = c(0,1), from = rng_touch_per_min) +
          W_POSSESSION["releases_per_min"] * scales::rescale(releases_per_min, to = c(0,1), from = rng_releases_per_min) +
          W_POSSESSION["pos_long_share"]   * pos_long_share +
          W_POSSESSION["receives_norm"]    * receives_norm
      )
    
    max_pos <- max(out$possession_index_raw[out$category != "Goalkeepers"], na.rm = TRUE)
    eps <- 1e-6
    
    out %>%
      mutate(
        possession_index = 100 * (possession_index_raw + eps) / (max_pos + eps),
        touches_index    = scales::rescale(touches_total,        to = c(0,100), from = rng_touches_total),
        rv_index         = scales::rescale(rv_index_raw,         to = c(0,100), from = rng_rv_raw),
        time_ball_index  = scales::rescale(avg_time_per_poss_sec,to = c(0,100), from = rng_avg_time)
      )
  }
  
  # ---- helper: derivazioni comuni ----
  add_derivations <- function(tbl) {
    tbl %>%
      mutate(
        touches_total = touch_L + touch_R,
        poss_total = pmax(one_touch + pos_short + pos_long, 0),
        
        share_1T    = ifelse(poss_total > 0, one_touch/poss_total, NA_real_),
        share_short = ifelse(poss_total > 0, pos_short/poss_total, NA_real_),
        share_long  = ifelse(poss_total > 0, pos_long/poss_total, NA_real_),
        
        share_touch_R = ifelse(touches_total > 0, touch_R/touches_total, NA_real_),
        share_touch_L = ifelse(touches_total > 0, touch_L/touches_total, NA_real_),
        
        rv_total = rv_z1 + rv_z2 + rv_z3 + rv_z4 + rv_z5,
        rv_index_raw =
          (RV_WEIGHTS[1]*rv_z1 + RV_WEIGHTS[2]*rv_z2 + RV_WEIGHTS[3]*rv_z3 +
             RV_WEIGHTS[4]*rv_z4 + RV_WEIGHTS[5]*rv_z5)  / pmax(rv_total, 1),
        
        time_ball_total_sec = one_touch*DUR_1TOUCH + pos_short*DUR_SHORT + pos_long*DUR_LONG,
        avg_time_per_poss_sec = ifelse(poss_total > 0, time_ball_total_sec/poss_total, NA_real_)
      )
  }
  
  # ---- 3) AGG ALL TIME ----
  agg_all <- df_skill %>%
    group_by(player, category) %>%
    summarise(
      touch_L = sum(touch_L, na.rm = TRUE),
      touch_R = sum(touch_R, na.rm = TRUE),
      touch_per_min = mean(touch_per_min, na.rm = TRUE),
      
      releases_per_min = mean(releases_per_min, na.rm = TRUE),
      release_vel_avg  = mean(release_vel_avg,  na.rm = TRUE),
      
      rv_z1 = sum(rv_z1, na.rm = TRUE),
      rv_z2 = sum(rv_z2, na.rm = TRUE),
      rv_z3 = sum(rv_z3, na.rm = TRUE),
      rv_z4 = sum(rv_z4, na.rm = TRUE),
      rv_z5 = sum(rv_z5, na.rm = TRUE),

      pos_long  = sum(pos_long,  na.rm = TRUE),
      pos_short = sum(pos_short, na.rm = TRUE),
      one_touch = sum(one_touch, na.rm = TRUE),
      
      one_touch_R = sum(one_touch_R, na.rm = TRUE),
      one_touch_L = sum(one_touch_L, na.rm = TRUE),
      
      release_L = sum(release_L, na.rm = TRUE),
      release_R = sum(release_R, na.rm = TRUE),
      
      receives   = sum(receives,   na.rm = TRUE),
      receives_L = sum(receives_L, na.rm = TRUE),
      receives_R = sum(receives_R, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    add_derivations()
  
  ref_all <- if (exclude_gk) dplyr::filter(agg_all, category != "Goalkeepers") else agg_all
  agg_all <- add_indices(agg_all, ref_all)
  
  # ---- 4) ROLE AVG ----
  role_avg <- agg_all %>%
    filter(category != "Goalkeepers") %>%
    group_by(category) %>%
    summarise(
      possession_index = 100*mean(possession_index_raw, na.rm=TRUE),
      touches_index    = mean(touches_index,    na.rm=TRUE),
      share_touch_R    = mean(share_touch_R,    na.rm=TRUE),
      share_touch_L    = mean(share_touch_L,    na.rm=TRUE),
      rv_index         = mean(rv_index,         na.rm=TRUE),
      avg_time_per_poss_sec = mean(avg_time_per_poss_sec, na.rm=TRUE),
      share_1T    = mean(share_1T,    na.rm=TRUE),
      share_short = mean(share_short, na.rm=TRUE),
      share_long  = mean(share_long,  na.rm=TRUE),
      .groups = "drop"
    ) %>% mutate(player = paste("Media", category))
  
  # ---- 5) builder per report ----
  build_report_long <- function(player_name) {
    ruolo <- agg_all %>% filter(player == player_name) %>% pull(category) %>% unique()
    if (length(ruolo) == 0) stop("Giocatore non trovato nel contesto tecnico.")
    
    player_row <- agg_all %>% filter(player == player_name)
    media_row  <- role_avg %>% filter(category == ruolo)
    
    stopifnot(nrow(player_row)==1, nrow(media_row)==1)
    
    pack <- function(tbl, tipo_label) {
      tbl %>%
        transmute(
          player,
          possession_index, touches_index,
          touch_right = share_touch_R*100, touch_left = share_touch_L*100,
          rv_index,
          avg_time_per_poss_sec,
          one_touch = share_1T*100, short_poss = share_short*100, long_poss = share_long*100
        ) %>% mutate(tipo = tipo_label)
    }
    
    bind_rows(
      pack(player_row, player_name),
      pack(media_row,  "Media Ruolo")
    ) %>%
      pivot_longer(-c(player, tipo), names_to = "metrica", values_to = "valore") %>%
      mutate(metrica = factor(metrica, levels = c(
        "possession_index","touches_index","rv_index",
        "touch_right","touch_left",
        "avg_time_per_poss_sec",
        "one_touch","short_poss","long_poss"
      )))
  }
  
  # ---- 6) AGG WEEKLY ----
  agg_week <- df_skill %>%
    group_by(player, category, week_id) %>%
    summarise(
      touch_L = sum(touch_L, na.rm = TRUE),
      touch_R = sum(touch_R, na.rm = TRUE),
      touch_per_min = mean(touch_per_min, na.rm = TRUE),
      
      releases_per_min = mean(releases_per_min, na.rm = TRUE),
      release_vel_avg  = mean(release_vel_avg,  na.rm = TRUE),
      
      rv_z1 = sum(rv_z1, na.rm = TRUE),
      rv_z2 = sum(rv_z2, na.rm = TRUE),
      rv_z3 = sum(rv_z3, na.rm = TRUE),
      rv_z4 = sum(rv_z4, na.rm = TRUE),
      rv_z5 = sum(rv_z5, na.rm = TRUE),

      pos_long  = sum(pos_long,  na.rm = TRUE),
      pos_short = sum(pos_short, na.rm = TRUE),
      one_touch = sum(one_touch, na.rm = TRUE),
      
      one_touch_R = sum(one_touch_R, na.rm = TRUE),
      one_touch_L = sum(one_touch_L, na.rm = TRUE),
      
      release_L = sum(release_L, na.rm = TRUE),
      release_R = sum(release_R, na.rm = TRUE),
      
      receives   = sum(receives,   na.rm = TRUE),
      receives_L = sum(receives_L, na.rm = TRUE),
      receives_R = sum(receives_R, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    add_derivations()
  
  ref_week <- if (exclude_gk) dplyr::filter(agg_week, category != "Goalkeepers") else agg_week
  agg_week <- add_indices(agg_week, ref_week)
  
  list(
    df_skill = df_skill,
    agg_all = agg_all,
    role_avg = role_avg,
    build_report_long = build_report_long,
    agg_week = agg_week
  )
}


report_tecnico <- function(giocatore, ctx, theme_fn = theme_crema_pro) {
  
  ruolo <- ctx$agg_all %>% filter(player == giocatore) %>% pull(category) %>% unique()
  if (length(ruolo) == 0) stop("Giocatore non trovato nel contesto tecnico.")
  
  dati_plot <- ctx$build_report_long(giocatore)
  
  lab_fun <- function(m, v){
    if (m %in% c("touch_right","touch_left","one_touch","short_poss","long_poss")) {
      paste0(round(v,1),"%")
    } else if (m == "avg_time_per_poss_sec") {
      paste0(round(v,2)," s")
    } else {
      round(v,1)
    }
  }
  
  ggplot(dati_plot, aes(x = valore, y = metrica, fill = tipo)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_text(
      aes(label = mapply(lab_fun, metrica, valore)),
      position = position_dodge(width = 0.7),
      hjust = -0.15, color = "black", size = 3.6, fontface = "bold"
    ) +
    scale_fill_manual(values = c(setNames("#FF2E2E", giocatore), "Media Ruolo" = "black"), name = NULL) +
    labs(
      title    = paste("📊 Report Tecnico –", giocatore),
      subtitle = paste("Confronto con Media Ruolo:", ruolo),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme_fn()
}


trend_tecnico_indici <- function(giocatore, ctx, theme_fn = theme_crema_pro) {
  
  colori_indici <- c(
    "Indice Possesso"          = "#FF2E2E",
    "Indice Tocchi"            = "lightblue",
    "Indice Velocità Rilascio" = "green",
    "Indice Tempo p/Possesso"  = "black"
  )
  
  dati <- ctx$agg_week %>%
    filter(player == giocatore) %>%
    select(week_id, possession_index, touches_index, rv_index, time_ball_index) %>%
    pivot_longer(cols = c(possession_index, touches_index, rv_index, time_ball_index),
                 names_to = "metrica", values_to = "valore") %>%
    mutate(
      week_id = factor(week_id, levels = sort(unique(week_id))),
      metrica = factor(metrica,
                       levels = c("possession_index","touches_index","rv_index","time_ball_index"),
                       labels = c("Indice Possesso","Indice Tocchi","Indice Velocità Rilascio","Indice Tempo p/Possesso"))
    )
  
  ggplot(dati, aes(x = week_id, y = valore, group = metrica, color = metrica)) +
    geom_line(linewidth = 1.35, lineend = "round") +
    geom_point(size = 2.6, shape = 21, stroke = 0.9, aes(fill = metrica), color = "black") +
    scale_color_manual(values = colori_indici) +
    scale_fill_manual(values = colori_indici) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = expansion(mult = c(0.02, 0.05))) +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
    labs(
      title = paste("Trend Indici Tecnici —", giocatore),
      subtitle = "Aggregazione settimanale | Scala 0–100",
      x = "Settimana", y = "Indice"
    ) +
    guides(color = guide_legend(nrow = 1, byrow = TRUE), fill = "none") +
    theme_fn() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
}


trend_tecnico_piede <- function(giocatore, ctx, theme_fn = theme_crema_pro) {
  
  dati <- ctx$agg_week %>%
    filter(player == giocatore) %>%
    select(week_id, share_touch_R, share_touch_L) %>%
    pivot_longer(cols = starts_with("share_touch_"),
                 names_to = "piede", values_to = "valore") %>%
    mutate(
      valore = valore * 100,
      week_id = factor(week_id, levels = sort(unique(week_id))),
      piede = factor(piede,
                     levels = c("share_touch_R","share_touch_L"),
                     labels = c("Destro","Sinistro"))
    )
  
  ggplot(dati, aes(x = week_id, y = valore, color = piede, group = piede)) +
    geom_line(linewidth = 1.5) +
    geom_point(size = 2.6, shape = 21, stroke = 0.9, aes(fill = piede),
               color = "black", show.legend = FALSE) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
    labs(title = paste("Uso del Piede –", giocatore),
         x = "Settimana", y = "Percentuale tocchi", color = NULL) +
    theme_fn() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
}


trend_tecnico_possesso <- function(giocatore, ctx, theme_fn = theme_crema_pro) {
  
  dati <- ctx$agg_week %>%
    filter(player == giocatore) %>%
    select(week_id, share_1T, share_short, share_long) %>%
    pivot_longer(cols = starts_with("share_"),
                 names_to = "tipo", values_to = "valore") %>%
    mutate(
      valore = valore * 100,
      week_id = factor(week_id, levels = sort(unique(week_id))),
      tipo = factor(tipo,
                    levels = c("share_1T","share_short","share_long"),
                    labels = c("One-touch","Possesso corto","Possesso lungo"))
    )
  
  ggplot(dati, aes(x = week_id, y = valore, fill = tipo, group = tipo)) +
    geom_area(position = "fill", alpha = 0.8) +
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 2)]) +
    labs(
      title = paste("Mix di Possesso –", giocatore),
      x = "Settimana", y = "Distribuzione % possesso", fill = NULL
    ) +
    theme_fn()
}


