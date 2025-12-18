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
  left_join(physical_data %>% select(player, category) %>% distinct(), by = "player") %>%
  mutate(
    acc_index = scales::rescale(acc_index, to = c(0,100)),
    dec_index = scales::rescale(dec_index, to = c(0,100))
  )

# Escludo portieri per medie
df_no_gk <- df_physicalreport %>% filter(category != "Goalkeepers")


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

