library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(rlang)
library(lubridate)
library(here)

df <- read_excel(here("data", "CremaFT_2401.xlsx"))


theme_crema_light <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "#FFFFFF", color = NA),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.border     = element_rect(color = "#E6E6E6", fill = NA, linewidth = 0.6),
      
      panel.grid.major = element_line(color = "grey", linewidth = 0.4),
      panel.grid.minor = element_line(color = "grey", linewidth = 0.4),
      
      text        = element_text(color = "#111111"),
      axis.text   = element_text(color = "#111111"),
      axis.title  = element_text(color = "#111111", face = "bold"),
      plot.title  = element_text(color = "#FF2E2E", face = "bold", size = 16),
      plot.subtitle = element_text(color = "#111111"),
      
      axis.ticks = element_line(color = "#CFCFCF"),
      axis.ticks.length = unit(3, "pt")
    )
}


df_skill <- df %>%
  transmute(
    date        = as.Date(Date),
    player      = `Player Name`,
    category    = `Position Category`,
    position    = Position,
    
    Touches_L   = `Left Leg Touches (#)`,
    Touches_R   = `Right Leg Touches (#)`,
    Touches_pm  = `Touches per min (#/min)`,
    
    RV_Z1       = `RV Zone 1 [0-5( m/s)]`,
    RV_Z2       = `RV Zone 2 [5-10( m/s)]`,
    RV_Z3       = `RV Zone 3 [10-15( m/s)]`,
    RV_Z4       = `RV Zone 4 [15-20( m/s)]`,
    RV_Z5       = `RV Zone 5 [20-25( m/s)]`,
    RV_Z6       = `RV Zone 6 [> 25( m/s)]`,
    
    LongPoss    = `Long possessions (#)`,
    ShortPoss   = `Short Possessions (#)`,
    
    OneTouch    = `One-Touch (#)`,
    OneTouch_R  = `One-Touch Right (#)`,
    OneTouch_L  = `One-Touch Left (#)`,
    
    Releases_pm = `Releases per min (#/min)`,
    Releases_L  = `Releases Left (#)`,
    Releases_R  = `Releases Right (#)`,
    ReleaseVel  = `Release Velocity Avg (m/s)`,
    
    Receives    = `Receives (#)`,
    Receives_L  = `Receives Left (#)`,
    Receives_R  = `Receives Right (#)`
  )


names(df_skill) <- c("date", "player", "category", "position",
                     "touch_L", "touch_R", "touch_per_min",
                     "rv_z1", "rv_z2", "rv_z3", "rv_z4", "rv_z5", "rv_z6",
                     "pos_long", "pos_short", "one_touch",
                     "one_touch_R", "one_touch_L",
                     "releases_per_min", "release_L", "release_R",
                     "release_vel_avg",
                     "receives", "receives_L", "receives_R")

num_cols <- setdiff(names(df_skill), c("date", "player","category","position"))
df_skill[num_cols] <- lapply(df_skill[num_cols], function(x) suppressWarnings(as.numeric(x)))


df_skill <- df_skill %>%
  mutate(
    week = isoweek(date), 
    year = isoyear(date), 
    week_id = paste(year, week, sep = "-")
  )


#PARAMETRI DA RIGUARDARE
# durate medie (sec) per tipo possesso
DUR_1TOUCH <- 0.4
DUR_SHORT  <- 2.0
DUR_LONG   <- 4.0

# pesi per indice velocità di rilascio (zone più alte valgono di più)
RV_WEIGHTS <- c(1, 2, 3, 4, 5, 6)

# pesi per indice di possesso (composito)
W_POSSESSION <- c(
  touch_per_min   = 0.40,
  releases_per_min= 0.25,
  pos_long_share  = 0.20,
  receives_norm   = 0.15
)


agg <- df_skill %>%
  group_by(player, category, position) %>%
  summarise(
    # volumi & tassi (uso medie per le metriche “/min”)
    touch_L = sum(touch_L, na.rm = TRUE),
    touch_R = sum(touch_R, na.rm = TRUE),
    touches_total = touch_L + touch_R,
    touch_per_min = mean(touch_per_min, na.rm = TRUE),
    
    releases_per_min = mean(releases_per_min, na.rm = TRUE),
    release_vel_avg  = mean(release_vel_avg, na.rm = TRUE),
    
    rv_z1 = sum(rv_z1, na.rm = TRUE),
    rv_z2 = sum(rv_z2, na.rm = TRUE),
    rv_z3 = sum(rv_z3, na.rm = TRUE),
    rv_z4 = sum(rv_z4, na.rm = TRUE),
    rv_z5 = sum(rv_z5, na.rm = TRUE),
    rv_z6 = sum(rv_z6, na.rm = TRUE),
    
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
  # derivazioni
  mutate(
    # mix possesso
    poss_total = pmax(one_touch + pos_short + pos_long, 0),
    share_1T   = ifelse(poss_total>0, one_touch/poss_total, NA_real_),
    share_short= ifelse(poss_total>0, pos_short/poss_total, NA_real_),
    share_long = ifelse(poss_total>0, pos_long /poss_total, NA_real_),
    
    # tocchi dx/sx
    share_touch_R = ifelse(touches_total>0, touch_R/touches_total, NA_real_),
    share_touch_L = ifelse(touches_total>0, touch_L/touches_total, NA_real_),
    
    # indice velocità di rilascio (0–100) pesato sulle zone
    rv_total  = rv_z1+rv_z2+rv_z3+rv_z4+rv_z5+rv_z6,
    rv_index_raw = (RV_WEIGHTS[1]*rv_z1 + RV_WEIGHTS[2]*rv_z2 + RV_WEIGHTS[3]*rv_z3 +
                      RV_WEIGHTS[4]*rv_z4 + RV_WEIGHTS[5]*rv_z5 + RV_WEIGHTS[6]*rv_z6) /
      pmax(rv_total, 1),
    # tempo con la palla (stima)
    time_ball_total_sec = one_touch*DUR_1TOUCH + pos_short*DUR_SHORT + pos_long*DUR_LONG,
    avg_time_per_poss_sec = ifelse(poss_total>0, time_ball_total_sec/poss_total, NA_real_)
  )


# normalizzazioni per confronto interno (min–max sui non-portieri, così l'indice è leggibile 0–100)
non_gk <- agg %>% filter(category != "Goalkeepers")

rng_rescale <- function(x, data = non_gk) rescale(x, to = c(0,100), from = range(data[[deparse(substitute(x))]], na.rm = TRUE))

# variabili di base per l’indice possesso
non_gk <- non_gk %>%
  mutate(
    receives_norm = rescale(receives, to = c(0,1), from = range(receives, na.rm = TRUE)),
    pos_long_share = share_long
  )

agg <- agg %>%
  left_join(non_gk %>% select(player, receives_norm, pos_long_share), by="player", relationship="many-to-many") %>%
  mutate(
    # indice possesso (composito, poi 0–100)
    possession_index_raw = W_POSSESSION["touch_per_min"]    * scales::rescale(touch_per_min, to=c(0,1), from=range(non_gk$touch_per_min, na.rm = TRUE)) +
      W_POSSESSION["releases_per_min"] * scales::rescale(releases_per_min, to=c(0,1), from=range(non_gk$releases_per_min, na.rm = TRUE)) +
      W_POSSESSION["pos_long_share"]   * pos_long_share +
      W_POSSESSION["receives_norm"]    * receives_norm,
    
    possession_index = rescale(possession_index_raw, to=c(0,100),
                               from=range(possession_index_raw[agg$category!="Goalkeepers"], na.rm = TRUE)),
    
    touches_index    = rescale(touches_total, to=c(0,100),
                               from=range(agg$touches_total[agg$category!="Goalkeepers"], na.rm = TRUE)),
    
    rv_index = rescale(rv_index_raw, to=c(0,100),
                       from=range(agg$rv_index_raw[agg$category!="Goalkeepers"], na.rm = TRUE)),
    
    time_ball_index = rescale(avg_time_per_poss_sec, to=c(0,100),
                              from=range(agg$avg_time_per_poss_sec[agg$category!="Goalkeepers"], na.rm = TRUE))
  )

# media ruolo (esclusi GK)
role_avg <- agg %>%
  filter(category != "Goalkeepers") %>%
  group_by(category) %>%
  summarise(
    possession_index = mean(possession_index, na.rm=TRUE),
    touches_index    = mean(touches_index, na.rm=TRUE),
    share_touch_R    = mean(share_touch_R, na.rm=TRUE),
    share_touch_L    = mean(share_touch_L, na.rm=TRUE),
    rv_index         = mean(rv_index, na.rm=TRUE),
    avg_time_per_poss_sec = mean(avg_time_per_poss_sec, na.rm=TRUE),
    share_1T   = mean(share_1T, na.rm=TRUE),
    share_short= mean(share_short, na.rm=TRUE),
    share_long = mean(share_long, na.rm=TRUE),
    .groups = "drop"
  ) %>% mutate(player = paste("Media", category))

# dataset “report” per giocatore selezionato + media ruolo
build_report_df <- function(player_name){
  ruolo <- agg %>% filter(player == player_name) %>% pull(category) %>% unique()
  media_ruolo_row <- role_avg %>% filter(category == ruolo)
  player_row <- agg %>% filter(player == player_name)
  
  stopifnot(nrow(player_row)==1, nrow(media_ruolo_row)==1)
  
  player_metrics <- player_row %>%
    transmute(player,
              possession_index, touches_index, 
              touch_right = share_touch_R*100, touch_left = share_touch_L*100,
              rv_index, 
              avg_time_per_poss_sec,
              one_touch = share_1T*100, short_poss = share_short*100, long_poss = share_long*100
    ) %>% mutate(tipo = player)
  
  role_metrics <- media_ruolo_row %>%
    transmute(player,
              possession_index, touches_index, 
              touch_right = share_touch_R*100, touch_left = share_touch_L*100,
              rv_index, 
              avg_time_per_poss_sec,
              one_touch = share_1T*100, short_poss = share_short*100, long_poss = share_long*100
    ) %>% mutate(tipo = "Media Ruolo")
  
  bind_rows(player_metrics, role_metrics) %>%
    pivot_longer(-c(player, tipo), names_to = "metrica", values_to = "valore") %>%
    mutate(metrica = factor(metrica, levels = c(
      "possession_index","touches_index","rv_index",
      "touch_right","touch_left",
      "avg_time_per_poss_sec",
      "one_touch","short_poss","long_poss"
    )))
}

report_tecnico <- function(giocatore){
  ruolo <- agg %>% filter(player == giocatore) %>% pull(category) %>% unique()
  dati_plot <- build_report_df(giocatore)
  
  # etichette più leggibili in base alla metrica
  lab_fun <- function(m, v){
    if(m %in% c("touch_right","touch_left","one_touch","short_poss","long_poss")) {
      paste0(round(v,1),"%")
    } else if(m %in% c("avg_time_per_poss_sec")) {
      paste0(round(v,2)," s")
    } else { # indici 0-100
      round(v,1)
    }
  }
  
  ggplot(dati_plot, aes(x = valore, y = metrica, fill = tipo)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    geom_text(aes(label = mapply(lab_fun, metrica, valore)),
              position = position_dodge(width = 0.7),
              hjust = -0.15, color = "black", size = 3.6, fontface = "bold") +
    scale_fill_manual(values = c(setNames("#FF2E2E", giocatore), "Media Ruolo" = "black"), name = NULL) +
    labs(
      title    = paste("📊 Report Tecnico –", giocatore),
      subtitle = paste("Confronto con Media Ruolo:", ruolo),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme_crema_light()
}
report_tecnico("N. Abba")
# df_skill ha già: date, player, category, position, week_id

DUR_1TOUCH <- 0.4
DUR_SHORT  <- 2.0
DUR_LONG   <- 4.0
RV_WEIGHTS <- c(1, 2, 3, 4, 5, 6)
W_POSSESSION <- c(
  touch_per_min    = 0.40,
  releases_per_min = 0.25,
  pos_long_share   = 0.20,
  receives_norm    = 0.15
)

# ---- AGGREGAZIONE SETTIMANALE ----
agg_week <- df_skill %>%
  group_by(player, category, position, week_id) %>%
  summarise(
    touch_L = sum(touch_L, na.rm = TRUE),
    touch_R = sum(touch_R, na.rm = TRUE),
    touches_total = touch_L + touch_R,
    touch_per_min = mean(touch_per_min, na.rm = TRUE),
    
    releases_per_min = mean(releases_per_min, na.rm = TRUE),
    release_vel_avg  = mean(release_vel_avg, na.rm = TRUE),
    
    rv_z1 = sum(rv_z1, na.rm = TRUE),
    rv_z2 = sum(rv_z2, na.rm = TRUE),
    rv_z3 = sum(rv_z3, na.rm = TRUE),
    rv_z4 = sum(rv_z4, na.rm = TRUE),
    rv_z5 = sum(rv_z5, na.rm = TRUE),
    rv_z6 = sum(rv_z6, na.rm = TRUE),
    
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
  # DERIVAZIONI
  mutate(
    # mix possesso
    poss_total = pmax(one_touch + pos_short + pos_long, 0),
    share_1T   = ifelse(poss_total > 0, one_touch / poss_total, NA_real_),
    share_short= ifelse(poss_total > 0, pos_short / poss_total, NA_real_),
    share_long = ifelse(poss_total > 0, pos_long  / poss_total, NA_real_),
    
    # tocchi dx/sx
    share_touch_R = ifelse(touches_total > 0, touch_R / touches_total, NA_real_),
    share_touch_L = ifelse(touches_total > 0, touch_L / touches_total, NA_real_),
    
    # indice velocità di rilascio (grezzo)
    rv_total  = rv_z1 + rv_z2 + rv_z3 + rv_z4 + rv_z5 + rv_z6,
    rv_index_raw = (RV_WEIGHTS[1]*rv_z1 + RV_WEIGHTS[2]*rv_z2 + RV_WEIGHTS[3]*rv_z3 +
                      RV_WEIGHTS[4]*rv_z4 + RV_WEIGHTS[5]*rv_z5 + RV_WEIGHTS[6]*rv_z6) /
      pmax(rv_total, 1),
    
    # tempo con la palla (stima)
    time_ball_total_sec = one_touch*DUR_1TOUCH + pos_short*DUR_SHORT + pos_long*DUR_LONG,
    avg_time_per_poss_sec = ifelse(poss_total > 0,
                                   time_ball_total_sec / poss_total,
                                   NA_real_)
  )

# ---- NORMALIZZAZIONI SU TUTTO IL CAMPIONATO (non GK) ----
non_gk_week <- agg_week %>% filter(category != "Goalkeepers")

# calcolo i range una volta sola (scalari)
rng_touch_per_min    <- range(non_gk_week$touch_per_min,        na.rm = TRUE)
rng_releases_per_min <- range(non_gk_week$releases_per_min,     na.rm = TRUE)
rng_receives         <- range(non_gk_week$receives,             na.rm = TRUE)
rng_avg_time         <- range(non_gk_week$avg_time_per_poss_sec,na.rm = TRUE)
rng_rv_raw           <- range(non_gk_week$rv_index_raw,         na.rm = TRUE)
rng_touches_total    <- range(non_gk_week$touches_total,        na.rm = TRUE)

agg_week <- agg_week %>%
  mutate(
    receives_norm = scales::rescale(
      receives,
      to   = c(0,1),
      from = rng_receives
    ),
    pos_long_share = share_long,
    
    possession_index_raw =
      W_POSSESSION["touch_per_min"]    * scales::rescale(
        touch_per_min,
        to   = c(0,1),
        from = rng_touch_per_min
      ) +
      W_POSSESSION["releases_per_min"] * scales::rescale(
        releases_per_min,
        to   = c(0,1),
        from = rng_releases_per_min
      ) +
      W_POSSESSION["pos_long_share"]   * pos_long_share +
      W_POSSESSION["receives_norm"]    * receives_norm
  )

# ora riscalo gli indici in 0–100 (sempre sui non GK)
rng_possession <- range(agg_week$possession_index_raw[agg_week$category!="Goalkeepers"], na.rm = TRUE)
rng_touches    <- rng_touches_total
rng_rv         <- rng_rv_raw
rng_timeball   <- rng_avg_time

agg_week <- agg_week %>%
  mutate(
    possession_index = scales::rescale(possession_index_raw, to = c(0,100), from = rng_possession),
    touches_index    = scales::rescale(touches_total,        to = c(0,100), from = rng_touches),
    rv_index         = scales::rescale(rv_index_raw,         to = c(0,100), from = rng_rv),
    time_ball_index  = scales::rescale(avg_time_per_poss_sec,to = c(0,100), from = rng_timeball)
  )


colori_indici <- c(
  "Indice Possesso"          = "#FF2E2E",
  "Indice Tocchi"            = "lightblue",
  "Indice Velocità Rilascio" = "green",
  "Indice Tempo p/Possesso"  = "white"
)


trend_tecnico_indici <- function(giocatore) {
  dati <- agg_week %>%
    filter(player == giocatore) %>%
    select(week_id, possession_index, touches_index, rv_index, time_ball_index) %>%
    tidyr::pivot_longer(
      cols = c(possession_index, touches_index, rv_index, time_ball_index),
      names_to = "metrica",
      values_to = "valore"
    ) %>%
    mutate(
      week_id = factor(week_id, levels = sort(unique(week_id))),
      metrica = factor(metrica,
                       levels = c("possession_index","touches_index","rv_index","time_ball_index"),
                       labels = c("Indice Possesso","Indice Tocchi","Indice Velocità Rilascio","Indice Tempo p/Possesso"))
    )
  
  ggplot(dati, aes(x = week_id, y = valore, group = metrica, color = metrica)) +
    geom_line(size = 1.5) +
    geom_point(size = 2) +
    scale_color_manual(values = colori_indici, name = NULL) +
    labs(
      title = paste("📈 Trend Indici Tecnici –", giocatore),
      x = "Settimana",
      y = "Indice (0–100)"
    ) +
    theme_minimal(base_size = 13) +
    theme_crema_light()
}


trend_tecnico_possesso <- function(giocatore) {
  dati <- agg_week %>%
    filter(player == giocatore) %>%
    select(week_id, share_1T, share_short, share_long) %>%
    tidyr::pivot_longer(
      cols = starts_with("share_"),
      names_to = "tipo",
      values_to = "valore"
    ) %>%
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
    labs(
      title = paste("Mix di Possesso –", giocatore),
      x = "Settimana",
      y = "Distribuzione % possesso",
      fill = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme_crema_light()
}

trend_tecnico_piede <- function(giocatore) {
  dati <- agg_week %>%
    filter(player == giocatore) %>%
    select(week_id, share_touch_R, share_touch_L) %>%
    tidyr::pivot_longer(
      cols = starts_with("share_touch_"),
      names_to = "piede",
      values_to = "valore"
    ) %>%
    mutate(
      valore = valore * 100,
      week_id = factor(week_id, levels = sort(unique(week_id))),
      piede = factor(piede,
                     levels = c("share_touch_R","share_touch_L"),
                     labels = c("Destro","Sinistro"))
    )
  
  ggplot(dati, aes(x = week_id, y = valore, color = piede, group = piede)) +
    geom_line(size = 1.5) +
    geom_point(size = 2) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = paste("Uso del Piede –", giocatore),
      x = "Settimana",
      y = "Percentuale tocchi",
      color = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme_crema_light()
}

