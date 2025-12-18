df_u17 <- read_excel(here("data", "CremaU17_2410.xlsx"))


physical_data_u17 <- df_u17 %>% 
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

colnames(physical_data_u17) <- c(
  "date", "day", "duration", "player", "Tag", "category", "position",
  "TopSpeed", "DistanceTot", "WorkRate",
  "SpeedZ1_m", "SpeedZ2_m", "SpeedZ3_m", "SpeedZ4_m", "SpeedZ5_m", "SpeedZ6_m", "SpeedZ7_m",
  "AccZ1_m", "AccZ2_m", "AccZ3_m", "AccZ4_m", "AccZ5_m", "AccZ6_m",
  "DecZ1_m", "DecZ2_m", "DecZ3_m", "DecZ4_m", "DecZ5_m", "DecZ6_m"
)

physical_data_u17 <- physical_data_u17 %>% 
  filter(Tag == "Full Session") %>% 
  mutate(
    across(c(starts_with("SpeedZ"), starts_with("AccZ"), starts_with("DecZ"),
             "duration","DistanceTot","TopSpeed","WorkRate"), 
           ~ suppressWarnings(as.numeric(.)))
  )


physical_data_u17 <- physical_data_u17 %>%
  mutate(
    date = as.Date(date), 
    week = isoweek(date), 
    year = isoyear(date), 
    week_id = paste(year, week, sep = "-")
  )

df_physicalreport_u17 <- physical_data_u17 %>%
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
  left_join(physical_data_u17 %>% select(player, category) %>% distinct(), by = "player") %>%
  mutate(
    acc_index = scales::rescale(acc_index, to = c(0,100)),
    dec_index = scales::rescale(dec_index, to = c(0,100))
  )

# Escludo portieri per medie
df_no_gk_u17 <- df_physicalreport_u17 %>% filter(category != "Goalkeepers")


#OCCHIO PERCHè MANCANO I RUOLI IN U17

df_skill_u17 <- df_u17 %>%
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


names(df_skill_u17) <- c("date", "player", "category", "position",
                     "touch_L", "touch_R", "touch_per_min",
                     "rv_z1", "rv_z2", "rv_z3", "rv_z4", "rv_z5", "rv_z6",
                     "pos_long", "pos_short", "one_touch",
                     "one_touch_R", "one_touch_L",
                     "releases_per_min", "release_L", "release_R",
                     "release_vel_avg",
                     "receives", "receives_L", "receives_R")

num_cols <- setdiff(names(df_skill_u17), c("date", "player","category","position"))
df_skill_u17[num_cols] <- lapply(df_skill_u17[num_cols], function(x) suppressWarnings(as.numeric(x)))


df_skill_u17 <- df_skill_u17 %>%
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


agg_u17 <- df_skill_u17 %>%
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
non_gk_u17 <- agg_u17 %>% filter(category != "Goalkeepers")

rng_rescale <- function(x, data = non_gk_u17) rescale(x, to = c(0,100), from = range(data[[deparse(substitute(x))]], na.rm = TRUE))

# variabili di base per l’indice possesso
non_gk_u17 <- non_gk_u17 %>%
  mutate(
    receives_norm = rescale(receives, to = c(0,1), from = range(receives, na.rm = TRUE)),
    pos_long_share = share_long
  )

agg_u17 <- agg_u17 %>%
  left_join(non_gk_u17 %>% select(player, receives_norm, pos_long_share), by="player", relationship="many-to-many") %>%
  mutate(
    # indice possesso (composito, poi 0–100)
    possession_index_raw = W_POSSESSION["touch_per_min"]    * scales::rescale(touch_per_min, to=c(0,1), from=range(non_gk_u17$touch_per_min, na.rm = TRUE)) +
      W_POSSESSION["releases_per_min"] * scales::rescale(releases_per_min, to=c(0,1), from=range(non_gk_u17$releases_per_min, na.rm = TRUE)) +
      W_POSSESSION["pos_long_share"]   * pos_long_share +
      W_POSSESSION["receives_norm"]    * receives_norm,
    
    possession_index = rescale(possession_index_raw, to=c(0,100),
                               from=range(possession_index_raw[agg_u17$category!="Goalkeepers"], na.rm = TRUE)),
    
    touches_index    = rescale(touches_total, to=c(0,100),
                               from=range(agg_u17$touches_total[agg_u17$category!="Goalkeepers"], na.rm = TRUE)),
    
    rv_index = rescale(rv_index_raw, to=c(0,100),
                       from=range(agg_u17$rv_index_raw[agg_u17$category!="Goalkeepers"], na.rm = TRUE)),
    
    time_ball_index = rescale(avg_time_per_poss_sec, to=c(0,100),
                              from=range(agg_u17$avg_time_per_poss_sec[agg_u17$category!="Goalkeepers"], na.rm = TRUE))
  )


agg_u17 <- agg_u17 %>% filter(category != "Goalkeepers")


u17_data <- agg_u17 %>%
  left_join(df_no_gk_u17, by = "player") %>%
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



find_most_similar_u17 <- function(player_ft,
                                  data_ft, data_u17,
                                  same_category = FALSE,
                                  method = c("euclidean", "cosine"),
                                  top_n = 5,
                                  return_plot = TRUE) {
  method <- match.arg(method)
  
  ft  <- data_ft
  u17 <- data_u17
  
  # Feature usate per il matching (escludo player e category)
  feat <- setdiff(names(ft), c("player", "category.x"))
  
  # prendo la riga del player FT
  p <- ft %>% filter(player == player_ft)
  if (nrow(p) != 1) stop("player_ft non trovato (o duplicato) in data_ft")
  
  # filtro U17 per stessa category.x (es. ruolo) se richiesto
  if (same_category) {
    u17 <- u17 %>% filter(`category.x` == p$`category.x`)
  }
  if (nrow(u17) == 0) stop("Nessun giocatore U17 disponibile dopo il filtro di category.x")
  
  # --- imputazione semplice NA: mediana sul campione combinato ---
  comb <- bind_rows(ft %>% mutate(.grp = "FT"),
                    u17 %>% mutate(.grp = "U17"))
  
  med <- comb %>%
    summarise(across(all_of(feat), ~ median(.x, na.rm = TRUE)))
  
  fill_median <- function(df) {
    df %>%
      mutate(across(all_of(feat), ~ ifelse(is.na(.x), as.numeric(med[[cur_column()]]), .x)))
  }
  
  ft2  <- fill_median(ft)
  u172 <- fill_median(u17)
  p2   <- ft2 %>% filter(player == player_ft)
  
  #LO Z-SCORE è CALCOLATO SU TUTTI I GIOCATORI INSIEME
  
  comb2 <- bind_rows(
    ft2 %>% select(all_of(feat)),
    u172 %>% select(all_of(feat))
  )
  
  mu <- sapply(comb2, mean)
  sdv <- sapply(comb2, sd)
  sdv[sdv == 0] <- 1
  
  z <- function(df) {
    X <- as.matrix(df[, feat])
    scale(X, center = mu, scale = sdv)
  }
  
  U <- z(u172)
  b <- as.numeric(z(p2))
  
  # distanza
  dist <- if (method == "euclidean") .euclid_dist(U, b) else .cosine_dist(U, b)
  
  out <- u172 %>%
    mutate(distance = dist) %>%
    arrange(distance) %>%
    select(player, `category.x`, distance, all_of(feat)) %>%
    slice_head(n = top_n)
  
  best_u17 <- out$player[1]
  
  # plot confronto (z-score) FT vs best U17
  plt <- NULL
  # plot confronto (valori originali) FT vs best U17
  plt <- NULL
  if (return_plot) {
    
    lab_ft  <- paste0("FT: ", player_ft)
    lab_u17 <- paste0("U17: ", best_u17)
    
    # valori ORIGINALI (non z-score)
    ft_abs  <- p2 %>% select(all_of(feat)) %>% mutate(who = lab_ft)
    u17_abs <- u172 %>% filter(player == best_u17) %>% select(all_of(feat)) %>% mutate(who = lab_u17)
    
    plot_df <- bind_rows(ft_abs, u17_abs) %>%
      tidyr::pivot_longer(-who, names_to = "variable", values_to = "value") %>%
      mutate(who = factor(who, levels = c(lab_ft, lab_u17)))
    
    # ordine metriche: per differenza assoluta (più importanti in alto)
    ord <- plot_df %>%
      tidyr::pivot_wider(names_from = who, values_from = value) %>%
      mutate(absdiff = abs(.data[[lab_u17]] - .data[[lab_ft]])) %>%
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
      geom_col(width = 0.72, color = "#0b0b0b") +
      geom_text(aes(label = val_lbl), vjust = -0.35, color = "white",
                fontface = "bold", size = 3.2) +
      facet_wrap(~ variable, scales = "free_y", ncol = 2) +
      scale_fill_manual(values = setNames(c("white", "#FF2E2E"), c(lab_ft, lab_u17))) +
      labs(
        title = "Confronto FT vs U17 più simile (valori originali)",
        subtitle = paste0(lab_ft, " | ", lab_u17,
                          " | Distanza (matching su z-score): ", round(out$distance[1], 2)),
        x = NULL, y = NULL, fill = NULL
      ) +
      coord_cartesian(clip = "off") +
      theme_minimal(base_size = 12) +
      theme(
        plot.background  = element_rect(fill = "#0b0b0b", color = NA),
        panel.background = element_rect(fill = "#0b0b0b", color = NA),
        panel.grid.major = element_line(color = "#222222"),
        panel.grid.minor = element_blank(),
        strip.text       = element_text(color = "white", face = "bold", size = 12),
        axis.text.x      = element_text(color = "white", face = "bold", size = 10),
        axis.text.y      = element_text(color = "white", size = 10),
        plot.title       = element_text(color = "#FF2E2E", face = "bold", size = 16),
        plot.subtitle    = element_text(color = "white"),
        legend.position  = "top",
        legend.text      = element_text(color = "white", face = "bold"),
        panel.spacing    = unit(1.1, "lines"),
        plot.margin      = margin(12, 12, 12, 12)
      )
  }
  
  
  
  
  list(
    player_ft = player_ft,
    category_ft = p$`category.x`,
    best_u17 = best_u17,
    ranking_u17 = out,
    plot = plt
  )
}


u17_data_phy <- u17_data %>%
  select(player, category.x, HID, topspeed, workrate, acc_index, dec_index)

u17_data_tec <- u17_data %>%
  select(player, category.x, touch_per_min, releases_per_min, release_vel_avg, one_touch_perc)

ft_data_phy <- ft_data %>%
  select(player, category.x, HID, topspeed, workrate, acc_index, dec_index)

ft_data_tec  <- ft_data %>%
  select(player, category.x, touch_per_min, releases_per_min, release_vel_avg, one_touch_perc)

