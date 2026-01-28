df_u19 <- read_excel(here("data", "CremaU19.xlsx"))


theme_crema_pro <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # griglia: leggera, “da report”
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#E9E9E9", linewidth = 0.4),
      panel.grid.major.x = element_line(color = "#F2F2F2", linewidth = 0.3),
      
      # bordo pannello sottile
      panel.border     = element_rect(color = "#E6E6E6", fill = NA, linewidth = 0.6),
      
      # testi
      plot.title    = element_text(face = "bold", size = base_size * 1.25, color = "#111111"),
      plot.subtitle = element_text(size = base_size * 0.95, color = "#444444"),
      plot.caption  = element_text(size = base_size * 0.8, color = "#666666"),
      
      axis.title = element_text(face = "bold", color = "#111111"),
      axis.text  = element_text(color = "#222222"),
      
      # legenda “media style”
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size * 0.9),
      legend.key = element_rect(fill = NA, color = NA),
      
      plot.margin = margin(10, 14, 10, 10)
    )
}


physical_data_u19 <- df_u19 %>% 
  transmute(
    date      = as.Date(Date),   
    day       = Day,
    duration  = `Phase Duration (min)`,
    player    = `Player Name`,
    Tag,
    category  = `Position Category`,
    position  = Position,
    TopSpeed  = `Top Speed (m/s)`,
    DistanceTot = `Distance Covered (m)`,
    WorkRate  = `Work Rate (m/min)`,
    SpeedZ1_m = `Speed Zone - Distance Covered (m) Zone 1 [0-1.5(m/s)]`,
    SpeedZ2_m = `Speed Zone - Distance Covered (m) Zone 2 [1.5-2(m/s)]`,
    SpeedZ3_m = `Speed Zone - Distance Covered (m) Zone 3 [2-3(m/s)]`,
    SpeedZ4_m = `Speed Zone - Distance Covered (m) Zone 4 [3-4(m/s)]`,
    SpeedZ5_m = `Speed Zone - Distance Covered (m) Zone 5 [4-5.5(m/s)]`,
    SpeedZ6_m = `Speed Zone - Distance Covered (m) Zone 6 [5.5-7(m/s)]`,
    SpeedZ7_m = `Speed Zone - Distance Covered (m) Zone 7 [> 7(m/s)]`,
    AccZ1_m   = `Horizontal Acc Zones - Distance Covered (m) Zone 1 [0-0.4(m/s^2)]`,
    AccZ2_m   = `Horizontal Acc Zones - Distance Covered (m) Zone 2 [0.4-0.9(m/s^2)]`,
    AccZ3_m   = `Horizontal Acc Zones - Distance Covered (m) Zone 3 [0.9-1.5(m/s^2)]`,
    AccZ4_m   = `Horizontal Acc Zones - Distance Covered (m) Zone 4 [1.5-2(m/s^2)]`,
    AccZ5_m   = `Horizontal Acc Zones - Distance Covered (m) Zone 5 [2-3(m/s^2)]`,
    AccZ6_m   = `Horizontal Acc Zones - Distance Covered (m) Zone 6 [> 3(m/s^2)]`,
    DecZ1_m   = `Horizontal Decl Zones - Distance Covered (m) Zone 1 [0-0.6(m/s^2)]`,
    DecZ2_m   = `Horizontal Decl Zones - Distance Covered (m) Zone 2 [0.6-1.1(m/s^2)]`,
    DecZ3_m   = `Horizontal Decl Zones - Distance Covered (m) Zone 3 [1.1-1.6(m/s^2)]`,
    DecZ4_m   = `Horizontal Decl Zones - Distance Covered (m) Zone 4 [1.6-2.2(m/s^2)]`,
    DecZ5_m   = `Horizontal Decl Zones - Distance Covered (m) Zone 5 [2.2-3(m/s^2)]`,
    DecZ6_m   = `Horizontal Decl Zones - Distance Covered (m) Zone 6 [> 3(m/s^2)]`
  )

colnames(physical_data_u19) <- c(
  "date", "day", "duration", "player", "Tag", "category", "position",
  "TopSpeed", "DistanceTot", "WorkRate",
  "SpeedZ1_m", "SpeedZ2_m", "SpeedZ3_m", "SpeedZ4_m", "SpeedZ5_m", "SpeedZ6_m", "SpeedZ7_m",
  "AccZ1_m", "AccZ2_m", "AccZ3_m", "AccZ4_m", "AccZ5_m", "AccZ6_m",
  "DecZ1_m", "DecZ2_m", "DecZ3_m", "DecZ4_m", "DecZ5_m", "DecZ6_m"
)

physical_data_u19 <- physical_data_u19 %>% 
  filter(Tag == "Full Session") %>% 
  mutate(
    across(c(starts_with("SpeedZ"), starts_with("AccZ"), starts_with("DecZ"),
             "duration","DistanceTot","TopSpeed","WorkRate"), 
           ~ suppressWarnings(as.numeric(.)))
  )

players_u19 <- c("C. Brazzorotto", "C. Barzi", "M. Bertazzoli", "C. Bertazzoni", "M. Bonizzoni", 
                 "G. Cerrato", "A. Cicarè", "W. De Maio", "M. D'Ischia", "I. Dimov",
                 "M. Fino", "V. Gerasym", "A. Jarid", "A. Nicotra", "F. Valletti",
                 "F. Montemezzani", "S. Schiavini", "B. Pape", "R. Tabal", "M. Tajeddine",
                 "F. Cerioli")

physical_data_u19 <- physical_data_u19 %>%
  mutate(
    date = as.Date(date), 
    week = isoweek(date), 
    year = isoyear(date), 
    week_id = paste(year, week, sep = "-")
  ) %>%
  filter(player %in% players_u19)

df_physicalreport_u19 <- physical_data_u19 %>%
  group_by(player) %>%
  summarise(
    HID = mean((SpeedZ5_m + SpeedZ6_m) / duration, na.rm = TRUE),
    z1_min = mean(SpeedZ1_m / duration, na.rm = TRUE), 
    z2_min = mean(SpeedZ2_m / duration, na.rm = TRUE), 
    z3_min = mean(SpeedZ3_m / duration, na.rm = TRUE), 
    z4_min = mean(SpeedZ4_m / duration, na.rm = TRUE), 
    z5_min = mean(SpeedZ5_m / duration, na.rm = TRUE), 
    z6_min = mean(SpeedZ6_m / duration, na.rm = TRUE), 
    sprint_dist = sum(SpeedZ7_m, na.rm = TRUE) / pmax(n(), 1), 
    topspeed = max(TopSpeed, na.rm = TRUE) * 3.6,  # m/s -> km/h
    workrate = mean(WorkRate, na.rm = TRUE),
    
    # pesi crescenti per Acc/Dec (1..6)
    acc_index = {
      acc_means <- colMeans(select(cur_data_all(), starts_with("AccZ")), na.rm = TRUE)
      sum((1:6) * acc_means)
    },
    dec_index = {
      dec_means <- colMeans(select(cur_data_all(), starts_with("DecZ")), na.rm = TRUE)
      sum((1:6) * dec_means)
    },
    .groups = "drop"
  ) %>%
  left_join(physical_data_u19 %>% select(player, category) %>% distinct(), by = "player") %>%
  mutate(
    acc_index = scales::rescale(acc_index, to = c(0,100)),
    dec_index = scales::rescale(dec_index, to = c(0,100))
  )

# Escludo portieri per medie
df_no_gk_u19 <- df_physicalreport_u19 %>% filter(category != "Goalkeepers")

df_skill_u19 <- df_u19 %>%
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


names(df_skill_u19) <- c("date", "player", "category", "position",
                         "touch_L", "touch_R", "touch_per_min",
                         "rv_z1", "rv_z2", "rv_z3", "rv_z4", "rv_z5", 
                         "pos_long", "pos_short", "one_touch",
                         "one_touch_R", "one_touch_L",
                         "releases_per_min", "release_L", "release_R",
                         "release_vel_avg",
                         "receives", "receives_L", "receives_R")

num_cols <- setdiff(names(df_skill_u19), c("date", "player","category","position"))
df_skill_u19[num_cols] <- lapply(df_skill_u19[num_cols], function(x) suppressWarnings(as.numeric(x)))


df_skill_u19 <- df_skill_u19 %>%
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
RV_WEIGHTS <- c(1, 2, 3, 4, 5)

# pesi per indice di possesso (composito)
W_POSSESSION <- c(
  touch_per_min   = 0.40,
  releases_per_min= 0.25,
  pos_long_share  = 0.20,
  receives_norm   = 0.15
)


agg_u19 <- df_skill_u19 %>%
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
    rv_total  = rv_z1+rv_z2+rv_z3+rv_z4+rv_z5,
    rv_index_raw = (RV_WEIGHTS[1]*rv_z1 + RV_WEIGHTS[2]*rv_z2 + RV_WEIGHTS[3]*rv_z3 +
                      RV_WEIGHTS[4]*rv_z4 + RV_WEIGHTS[5]*rv_z5) /
      pmax(rv_total, 1),
    # tempo con la palla (stima)
    time_ball_total_sec = one_touch*DUR_1TOUCH + pos_short*DUR_SHORT + pos_long*DUR_LONG,
    avg_time_per_poss_sec = ifelse(poss_total>0, time_ball_total_sec/poss_total, NA_real_)
  )


# normalizzazioni per confronto interno (min–max sui non-portieri, così l'indice è leggibile 0–100)
non_gk_u19 <- agg_u19 %>% filter(category != "Goalkeepers")

rng_rescale <- function(x, data = non_gk_u19) rescale(x, to = c(0,100), from = range(data[[deparse(substitute(x))]], na.rm = TRUE))

# variabili di base per l’indice possesso
non_gk_u19 <- non_gk_u19 %>%
  mutate(
    receives_norm = rescale(receives, to = c(0,1), from = range(receives, na.rm = TRUE)),
    pos_long_share = share_long
  )

agg_u19 <- agg_u19 %>%
  left_join(non_gk_u19 %>% select(player, receives_norm, pos_long_share), by="player", relationship="many-to-many") %>%
  mutate(
    # indice possesso (composito, poi 0–100)
    possession_index_raw = W_POSSESSION["touch_per_min"]    * scales::rescale(touch_per_min, to=c(0,1), from=range(non_gk_u19$touch_per_min, na.rm = TRUE)) +
      W_POSSESSION["releases_per_min"] * scales::rescale(releases_per_min, to=c(0,1), from=range(non_gk_u19$releases_per_min, na.rm = TRUE)) +
      W_POSSESSION["pos_long_share"]   * pos_long_share +
      W_POSSESSION["receives_norm"]    * receives_norm,
    
    possession_index = rescale(possession_index_raw, to=c(0,100),
                               from=range(possession_index_raw[agg_u19$category!="Goalkeepers"], na.rm = TRUE)),
    
    touches_index    = rescale(touches_total, to=c(0,100),
                               from=range(agg_u19$touches_total[agg_u19$category!="Goalkeepers"], na.rm = TRUE)),
    
    rv_index = rescale(rv_index_raw, to=c(0,100),
                       from=range(agg_u19$rv_index_raw[agg_u19$category!="Goalkeepers"], na.rm = TRUE)),
    
    time_ball_index = rescale(avg_time_per_poss_sec, to=c(0,100),
                              from=range(agg_u19$avg_time_per_poss_sec[agg_u19$category!="Goalkeepers"], na.rm = TRUE))
  )

agg_u19 <- agg_u19 %>% filter(category != "Goalkeepers")


u19_data <- agg_u19 %>%
  left_join(df_no_gk_u19, by = "player") %>%
  select(player, category.x, touch_per_min, releases_per_min, release_vel_avg, one_touch, 
         touches_total, HID, topspeed, workrate, acc_index, dec_index) %>%
  mutate(one_touch_perc = 100*(one_touch / touches_total)) %>%
  select(-c(one_touch, touches_total))


ft_data <- agg %>%
  left_join(df_no_gk, by = "player") %>%
  select(player, category.x, touch_per_min, releases_per_min, release_vel_avg, one_touch, 
         touches_total, HID, topspeed, workrate, acc_index, dec_index) %>%
  mutate(one_touch_perc = 100*(one_touch / touches_total)) %>%
  select(-c(one_touch, touches_total)) %>%
  filter(category.x != "Goalkeepers")


.euclid_dist <- function(A, b) sqrt(rowSums((A - matrix(b, nrow(A), ncol(A), byrow = T))^2))

.cosine_dist <- function(A, b){
  b_norm <- sqrt(sum(b^2))
  A_norm <- sqrt(rowSums(A^2))
  sim <- (A %*% b) / (A_norm *b_norm)
  as.numeric(1 - sim)
}



find_most_similar_u19 <- function(player_ft,
                                  data_ft, data_u19,
                                  same_category = FALSE,
                                  method = c("euclidean", "cosine"),
                                  top_n = 5,
                                  return_plot = TRUE) {
  method <- match.arg(method)
  
  ft  <- data_ft
  u19 <- data_u19
  
  # Feature usate per il matching (escludo player e category)
  feat <- setdiff(names(ft), c("player", "category.x"))
  
  # prendo la riga del player FT
  p <- ft %>% filter(player == player_ft)
  if (nrow(p) != 1) stop("player_ft non trovato (o duplicato) in data_ft")
  
  # filtro u19 per stessa category.x (es. ruolo) se richiesto
  if (same_category) {
    u19 <- u19 %>% filter(`category.x` == p$`category.x`)
  }
  if (nrow(u19) == 0) stop("Nessun giocatore u19 disponibile dopo il filtro di category.x")
  
  # --- imputazione semplice NA: mediana sul campione combinato ---
  comb <- bind_rows(ft %>% mutate(.grp = "FT"),
                    u19 %>% mutate(.grp = "u19"))
  
  med <- comb %>%
    summarise(across(all_of(feat), ~ median(.x, na.rm = TRUE)))
  
  fill_median <- function(df) {
    df %>%
      mutate(across(all_of(feat), ~ ifelse(is.na(.x), as.numeric(med[[cur_column()]]), .x)))
  }
  
  ft2  <- fill_median(ft)
  u192 <- fill_median(u19)
  p2   <- ft2 %>% filter(player == player_ft)
  
  #LO Z-SCORE è CALCOLATO SU TUTTI I GIOCATORI INSIEME
  
  comb2 <- bind_rows(
    ft2 %>% select(all_of(feat)),
    u192 %>% select(all_of(feat))
  )
  
  mu <- sapply(comb2, mean)
  sdv <- sapply(comb2, sd)
  sdv[sdv == 0] <- 1
  
  z <- function(df) {
    X <- as.matrix(df[, feat])
    scale(X, center = mu, scale = sdv)
  }
  
  U <- z(u192)
  b <- as.numeric(z(p2))
  
  # distanza
  dist <- if (method == "euclidean") .euclid_dist(U, b) else .cosine_dist(U, b)
  
  out <- u192 %>%
    mutate(distance = dist) %>%
    arrange(distance) %>%
    select(player, `category.x`, distance, all_of(feat)) %>%
    slice_head(n = top_n)
  
  best_u19 <- out$player[1]
  
  # plot confronto (z-score) FT vs best u19
  plt <- NULL
  # plot confronto (valori originali) FT vs best u19
  plt <- NULL
  if (return_plot) {
    
    lab_ft  <- paste0("FT: ", player_ft)
    lab_u19 <- paste0("u19: ", best_u19)
    
    # valori ORIGINALI (non z-score)
    ft_abs  <- p2 %>% select(all_of(feat)) %>% mutate(who = lab_ft)
    u19_abs <- u192 %>% filter(player == best_u19) %>% select(all_of(feat)) %>% mutate(who = lab_u19)
    
    plot_df <- bind_rows(ft_abs, u19_abs) %>%
      tidyr::pivot_longer(-who, names_to = "variable", values_to = "value") %>%
      mutate(who = factor(who, levels = c(lab_ft, lab_u19)))
    
    # ordine metriche: per differenza assoluta (più importanti in alto)
    ord <- plot_df %>%
      tidyr::pivot_wider(names_from = who, values_from = value) %>%
      mutate(absdiff = abs(.data[[lab_u19]] - .data[[lab_ft]])) %>%
      arrange(desc(absdiff)) %>%
      pull(variable)
    
    plot_df <- plot_df %>% mutate(variable = factor(variable, levels = ord))
    
    # etichette valori (formattazione "smart")
    plot_df <- plot_df %>%
      mutate(val_lbl = dplyr::case_when(
        abs(value) >= 100 ~ scales::number(value, accuracy = 1),
        abs(value) >= 10  ~ scales::number(value, accuracy = 0.1),
        TRUE              ~ scales::number(value, accuracy = 0.01)
      ))
    
    plt <- ggplot(plot_df, aes(x = who, y = value, fill = who)) +
      geom_col(width = 0.72, color = "white") +
      geom_text(aes(label = val_lbl), vjust = -0.35, color = "black",
                fontface = "bold", size = 3.2) +
      facet_wrap(~ variable, scales = "free_y", ncol = 2) +
      scale_fill_manual(values = setNames(c("black", "#FF2E2E"), c(lab_ft, lab_u19))) +
      labs(
        title = "Confronto FT vs u19 più simile (valori originali)",
        subtitle = paste0(lab_ft, " | ", lab_u19,
                          " | Distanza (matching su z-score): ", round(out$distance[1], 2)),
        x = NULL, y = NULL, fill = NULL
      ) +
      coord_cartesian(clip = "off") +
      theme_minimal(base_size = 12) +
      theme_crema_light()
  }
  
  
  
  
  list(
    player_ft = player_ft,
    category_ft = p$`category.x`,
    best_u19 = best_u19,
    ranking_u19 = out,
    plot = plt
  )
}


u19_data_phy <- u19_data %>%
  select(player, category.x, HID, topspeed, workrate, acc_index, dec_index)

u19_data_tec <- u19_data %>%
  select(player, category.x, touch_per_min, releases_per_min, release_vel_avg, one_touch_perc)

ft_data_phy <- ft_data %>%
  select(player, category.x, HID, topspeed, workrate, acc_index, dec_index)

ft_data_tec  <- ft_data %>%
  select(player, category.x, touch_per_min, releases_per_min, release_vel_avg, one_touch_perc)



library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)

# ---- helper: min-max robusto (0-1) su un vettore, ignorando NA ----
mm01 <- function(x, ref_min, ref_max) {
  ifelse(is.na(x), NA_real_,
         ifelse(ref_max - ref_min == 0, 0.5, (x - ref_min) / (ref_max - ref_min)))
}



df_touch_avg_u19 <- df_skill_u19 %>%
  mutate(touches_total = touch_L + touch_R) %>%
  group_by(player) %>%
  summarise(touches_total_avg = mean(touches_total, na.rm = TRUE), .groups = "drop")

df_physicalreport_u19 <- df_physicalreport_u19 %>%
  mutate(
    physical_index_raw =(
      scales::rescale(z5_min, to = c(0, 1), from = range(z5_min, na.rm = T)) * 0.2 + 
        scales::rescale(z6_min, to = c(0, 1), from = range(z6_min, na.rm = T)) * 0.2 + 
        scales::rescale(sprint_dist, to = c(0, 1), from = range(sprint_dist, na.rm = T)) * 0.1 + 
        scales::rescale(workrate, to = c(0, 1), from = range(workrate, na.rm = T))* 0.5
    ), 
    physical_index = rescale(physical_index_raw, to = c(0, 1))
  )

# Unisco parte tecnica (agg) e fisica (df_physicalreport) + tocchi medi
df_scatter_u19 <- agg_u19 %>%
  left_join(df_physicalreport %>% select(player, physical_index), by = "player") %>%
  left_join(df_touch_avg, by = "player") %>%
  filter(category != "Goalkeepers" & player %in% player_list)

# --- INDICE DI QUALITÀ TECNICA ----------------------------------------------

# Costruiamo un indice composito (0–100)
df_scatter_u19 <- df_scatter_u19 %>%
  mutate(
    one_touch_share = share_1T,  # % di possesso in tocchi di prima
    quality_index_raw = (
      scales::rescale(releases_per_min, to = c(0,1), from = range(releases_per_min, na.rm=TRUE)) * 0.4 +
        scales::rescale(release_vel_avg,  to = c(0,1), from = range(release_vel_avg, na.rm=TRUE))  * 0.4 +
        scales::rescale(touches_total_avg, to = c(0, 1), from = range(touches_total_avg, na.rm = T)) *0.2
    ),
    quality_index = rescale(quality_index_raw, to = c(0, 1))
  ) 





# ---- funzione principale ----
plot_4pt_u19_vs_ft <- function(player_name,
                               df_team,
                               df_upper,
                               team_name  = "U19",
                               upper_name = "Prima Squadra",
                               role_col   = "category.x",
                               exclude_goalkeepers = TRUE,
                               # colonne usate per indici (default coerente con il tuo u19_data)
                               phys_vars = c("HID", "topspeed", "workrate", "acc_index", "dec_index"),
                               tec_vars  = c("touch_per_min", "releases_per_min", "release_vel_avg", "one_touch_perc"),
                               # pesi (opzionali) per media pesata
                               phys_w = NULL,
                               tec_w  = NULL,
                               top_criterion = c("balanced", "physical", "technical")) {
  
  top_criterion <- match.arg(top_criterion)
  
  # ---- checks minimi ----
  needed <- c("player", role_col, phys_vars, tec_vars)
  miss_team  <- setdiff(needed, names(df_team))
  miss_upper <- setdiff(needed, names(df_upper))
  if (length(miss_team)  > 0) stop("df_team manca colonne: ", paste(miss_team, collapse=", "))
  if (length(miss_upper) > 0) stop("df_upper manca colonne: ", paste(miss_upper, collapse=", "))
  
  # etichette squadra
  team  <- df_team  %>% mutate(squadra = team_name)
  upper <- df_upper %>% mutate(squadra = upper_name)
  
  both <- bind_rows(team, upper)
  
  # escludo portieri se richiesto (sia per scaling sia per medie/top)
  if (exclude_goalkeepers) {
    both_ngk <- both %>% filter(.data[[role_col]] != "Goalkeepers")
  } else {
    both_ngk <- both
  }
  
  # ---- pesi: default uniformi ----
  if (is.null(phys_w)) phys_w <- rep(1, length(phys_vars))
  if (is.null(tec_w))  tec_w  <- rep(1, length(tec_vars))
  if (length(phys_w) != length(phys_vars)) stop("phys_w deve avere stessa lunghezza di phys_vars")
  if (length(tec_w)  != length(tec_vars))  stop("tec_w deve avere stessa lunghezza di tec_vars")
  phys_w <- phys_w / sum(phys_w)
  tec_w  <- tec_w  / sum(tec_w)
  
  # ---- scaling comune (min-max) sulle due squadre insieme ----
  ref_ranges <- both_ngk %>%
    summarise(across(all_of(c(phys_vars, tec_vars)),
                     list(min = ~min(.x, na.rm = TRUE),
                          max = ~max(.x, na.rm = TRUE)),
                     .names = "{.col}__{.fn}"))
  
  # applico scaling
  both2 <- both %>%
    rowwise() %>%
    mutate(
      across(all_of(c(phys_vars, tec_vars)),
             ~ {
               v <- cur_column()
               ref_min <- ref_ranges[[paste0(v, "__min")]]
               ref_max <- ref_ranges[[paste0(v, "__max")]]
               mm01(.x, ref_min, ref_max)
             },
             .names = "S__{.col}")
    ) %>%
    ungroup()
  
  # indici (0-1) come media (pesata) delle componenti già scalate
  both2 <- both2 %>%
    mutate(
      physical_index = as.numeric(as.matrix(select(., all_of(paste0("S__", phys_vars)))) %*% phys_w),
      quality_index  = as.numeric(as.matrix(select(., all_of(paste0("S__", tec_vars )))) %*% tec_w)
    )
  
  # ---- prendo il giocatore (deve essere nella squadra di appartenenza) ----
  p <- both2 %>% filter(squadra == team_name, player == player_name)
  if (nrow(p) == 0) stop("Giocatore non trovato in df_team (controlla nome).")
  role_player <- p[[role_col]][1]
  
  # ---- punti di riferimento ----
  team_role_mean <- both2 %>%
    filter(squadra == team_name, .data[[role_col]] == role_player) %>%
    summarise(physical_index = mean(physical_index, na.rm = TRUE),
              quality_index  = mean(quality_index,  na.rm = TRUE),
              .groups = "drop") %>%
    mutate(player = paste0("Media ruolo ", team_name, " (", role_player, ")"),
           squadra = team_name, tipo = "Media ruolo squadra")
  
  upper_role_mean <- both2 %>%
    filter(squadra == upper_name, .data[[role_col]] == role_player) %>%
    summarise(physical_index = mean(physical_index, na.rm = TRUE),
              quality_index  = mean(quality_index,  na.rm = TRUE),
              .groups = "drop") %>%
    mutate(player = paste0("Media ruolo ", upper_name, " (", role_player, ")"),
           squadra = upper_name, tipo = "Media ruolo superiore")
  
  # criterio top
  upper_role <- both2 %>% filter(squadra == upper_name, .data[[role_col]] == role_player)
  if (nrow(upper_role) == 0) stop("Nessun giocatore del ruolo in df_upper.")
  
  upper_role <- upper_role %>%
    mutate(score_top = case_when(
      top_criterion == "balanced"  ~ 0.5*physical_index + 0.5*quality_index,
      top_criterion == "physical"  ~ physical_index,
      top_criterion == "technical" ~ quality_index
    ))
  
  top_upper <- upper_role %>%
    arrange(desc(score_top)) %>%
    slice(1) %>%
    transmute(player = paste0("Top ", upper_name, " (", role_player, "): ", player),
              physical_index, quality_index,
              squadra = upper_name,
              tipo = "Top superiore")
  
  player_pt <- p %>%
    transmute(player = paste0(team_name, ": ", player),
              physical_index, quality_index,
              squadra = team_name,
              tipo = "Giocatore")
  
  plot_df <- bind_rows(player_pt, team_role_mean, upper_role_mean, top_upper) %>%
    mutate(tipo = factor(tipo, levels = c("Giocatore","Media ruolo squadra","Media ruolo superiore","Top superiore")))
  
  # ---- distanze (euclidee) utili per “gap” ----
  dist_euc <- function(a, b) sqrt((a$physical_index - b$physical_index)^2 + (a$quality_index - b$quality_index)^2)
  d_role_gap   <- dist_euc(team_role_mean, upper_role_mean)
  d_player_gap <- dist_euc(player_pt, upper_role_mean)
  d_to_top     <- dist_euc(player_pt, top_upper)
  
  # ---- grafico ----
  ggplot(plot_df, aes(physical_index, quality_index)) +
    geom_segment(data = plot_df %>% filter(tipo %in% c("Media ruolo squadra","Media ruolo superiore")),
                 aes(x = team_role_mean$physical_index, y = team_role_mean$quality_index,
                     xend = upper_role_mean$physical_index, yend = upper_role_mean$quality_index),
                 linewidth = 1, linetype = "dashed", color = "gray55") +
    geom_point(aes(shape = tipo), size = 6, stroke = 1, color = "black", fill = "white") +
    ggrepel::geom_text_repel(aes(label = player), size = 4.6, max.overlaps = Inf) +
    scale_shape_manual(values = c("Giocatore" = 21, "Media ruolo squadra" = 22, "Media ruolo superiore" = 24, "Top superiore" = 23)) +
    scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25), expand = c(0,0)) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25), expand = c(0,0)) +
    labs(
      title = "📈 Confronto 4 punti su scala comune (U19 vs Prima Squadra)",
      subtitle = paste0(
        "Giocatore: ", player_name, " — ruolo: ", role_player,
        " | Gap ruolo (U19→Prima): ", round(d_role_gap, 3),
        " | Gap giocatore→media Prima: ", round(d_player_gap, 3)
      ),
      caption = paste0("Distanza giocatore→top Prima (", top_criterion, "): ", round(d_to_top, 3)),
      x = "Indice Fisico (scala comune 0–1)",
      y = "Indice Tecnico (scala comune 0–1)"
    ) +
    theme_crema_pro()
}


plot_4pt_u19_vs_ft("C. Barzi", 
                   u19_data, 
                   ft_data)


