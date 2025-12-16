library(dplyr)
library(stringr)

df <- read_excel(here("data", "CremaFT_1910.xlsx"))

df_partita_finale <- df %>%
  filter(str_detect(Tag, "Partita finale"))


df_pf <- df_partita_finale %>%
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


names(df_pf) <- c("date", "player", "category", "position",
                     "touch_L", "touch_R", "touch_per_min",
                     "rv_z1", "rv_z2", "rv_z3", "rv_z4", "rv_z5", "rv_z6",
                     "pos_long", "pos_short", "one_touch",
                     "one_touch_R", "one_touch_L",
                     "releases_per_min", "release_L", "release_R",
                     "release_vel_avg",
                     "receives", "receives_L", "receives_R")


num_cols <- setdiff(names(df_pf), c("date", "player","category","position"))
df_pf[num_cols] <- lapply(df_pf[num_cols], function(x) suppressWarnings(as.numeric(x)))


df_pf <- df_pf %>%
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


agg_pf <- df_pf %>%
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
non_gk_pf <- agg_pf %>% filter(category != "Goalkeepers")

rng_rescale <- function(x, data = non_gk_pf) rescale(x, to = c(0,100), from = range(data[[deparse(substitute(x))]], na.rm = TRUE))

# variabili di base per l’indice possesso
non_gk_pf <- non_gk_pf %>%
  mutate(
    receives_norm = rescale(receives, to = c(0,1), from = range(receives, na.rm = TRUE)),
    pos_long_share = share_long
  )

agg_pf <- agg_pf %>%
  left_join(non_gk_pf %>% select(player, receives_norm, pos_long_share), by="player", relationship="many-to-many") %>%
  mutate(
    # indice possesso (composito, poi 0–100)
    possession_index_raw = W_POSSESSION["touch_per_min"]    * scales::rescale(touch_per_min, to=c(0,1), from=range(non_gk_pf$touch_per_min, na.rm = TRUE)) +
      W_POSSESSION["releases_per_min"] * scales::rescale(releases_per_min, to=c(0,1), from=range(non_gk_pf$releases_per_min, na.rm = TRUE)) +
      W_POSSESSION["pos_long_share"]   * pos_long_share +
      W_POSSESSION["receives_norm"]    * receives_norm,
    
    possession_index = rescale(possession_index_raw, to=c(0,100),
                               from=range(possession_index_raw[agg_pf$category!="Goalkeepers"], na.rm = TRUE)),
    
    touches_index    = rescale(touches_total, to=c(0,100),
                               from=range(agg_pf$touches_total[agg_pf$category!="Goalkeepers"], na.rm = TRUE)),
    
    rv_index = rescale(rv_index_raw, to=c(0,100),
                       from=range(agg_pf$rv_index_raw[agg_pf$category!="Goalkeepers"], na.rm = TRUE)),
    
    time_ball_index = rescale(avg_time_per_poss_sec, to=c(0,100),
                              from=range(agg_pf$avg_time_per_poss_sec[agg_pf$category!="Goalkeepers"], na.rm = TRUE))
  )

# media ruolo (esclusi GK)
role_avg_pf <- agg_pf %>%
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
build_report_df_pf <- function(player_name){
  ruolo <- agg_pf %>% filter(player == player_name) %>% pull(category) %>% unique()
  media_ruolo_row <- role_avg_pf %>% filter(category == ruolo)
  player_row <- agg_pf %>% filter(player == player_name)
  
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

report_tecnico_pf <- function(giocatore){
  ruolo <- agg_pf %>% filter(player == giocatore) %>% pull(category) %>% unique()
  dati_plot <- build_report_df_pf(giocatore)
  
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
              hjust = -0.15, color = "white", size = 3.6, fontface = "bold") +
    scale_fill_manual(values = c(setNames("#FF2E2E", giocatore), "Media Ruolo" = "white"), name = NULL) +
    labs(
      title    = paste("📊 Report Tecnico –", giocatore),
      subtitle = paste("Confronto con Media Ruolo:", ruolo),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.background   = element_rect(fill = "#0b0b0b", color = NA),
      panel.background  = element_rect(fill = "#0b0b0b", color = NA),
      panel.grid.major  = element_line(color = "#222222"),
      panel.grid.minor  = element_blank(),
      axis.text.y       = element_text(color = "white", face = "bold"),
      axis.text.x       = element_text(color = "white"),
      plot.title        = element_text(color = "#FF2E2E", face = "bold", size = 16),
      plot.subtitle     = element_text(color = "white"),
      legend.text       = element_text(color = "white", face = "bold"),
      legend.position   = "top",
      plot.margin       = margin(10, 40, 10, 10)
    )
}

report_tecnico_pf("L. Gramignoli")
