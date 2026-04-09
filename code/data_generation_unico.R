library(readxl)
library(here)
library(dplyr)

# -------------------------------------------------------------------------
# SOURCE UNA SOLA VOLTA
# -------------------------------------------------------------------------

source(here("code", "ReportUnoNew.R"))
source(here("code", "ReportDueNew.R"))
source(here("code", "Temi_grafici.R"))
source(here("code", "phy_vs_tech_new.R"))
source(here("code", "player_wheel.R"))
source(here("code", "run_dynamics_new.R"))
source(here("code", "comparisons.R"))
source(here("code", "comparisons_U17_new.R"))

# -------------------------------------------------------------------------
# FILE UNICO
# -------------------------------------------------------------------------

file_unico <- here("data", "CremaAprile.xlsx")   
sheet_data <- 2                                  

df_all <- read_excel(file_unico, sheet = sheet_data)

# -------------------------------------------------------------------------
# ROSTER
# -------------------------------------------------------------------------

players_ft <- c(
  "G. Recino", "E. Maccherini", "L. Niculae", "S. Azzali", "A. Arpini",
  "T. Valdameri", "N. Abba", "M. Vailati", "J. Mozzanica", "A. Latini",
  "R. Tomella", "R. Pavesi", "E. Neci", "D. Fenotti", "L. Gramignoli",
  "T. Erman", "M. Varisco", "J. Cerasani", "T. Serioli", "V. Camilleri"
)

players_u19 <- c(
  "C. Brazzorotto", "C. Barzi", "M. Bertazzoli", "C. Bertazzoni", "M. Bonizzoni",
  "A. Cicarè", "W. De Maio", "M. D'Ischia", "I. Dimov",
  "M. Fino", "V. Gerasym", "A. Jarid", "A. Nicotra", "F. Valletti",
  "F. Montemezzani", "S. Schiavini", "B. Pape", "R. Tabal", "M. Tajeddine",
  "F. Cerioli"
)

players_u17 <- c(
  "A. Ajdini", "M. Bresciani", "A. Brusati", "T. Calia", "E. Di Cicco", "K. Gagliardi",
  "L. Invernizzi", "L. Kurici", "A. Laini", "M. Laini", "P. Marelli", "F. Massazza",
  "A. Mhilli", "S. Montoya", "F. Moruzzi", "F. Ndema", "R. Nicolini", "L. Pascale",
  "D. Pea", "A. Riboli", "D. Shiku", "M. Vanelli"
)

players_u13 <- c(
  "Barbera", "Cè", "Coletti", "Diana", "Esestime",
  "Filocamo", "Lungu", "Manera", "Mattiazzi", "Picciolo", "Quartieri", "Romano"
)

# -------------------------------------------------------------------------
# MATCH FINDER
# -------------------------------------------------------------------------

infer_match_tags <- function(df_raw, pattern = "CREMA vs") {
  df_raw %>%
    transmute(Tag = as.character(Tag)) %>%
    filter(!is.na(Tag), Tag != "") %>%
    distinct(Tag) %>%
    filter(stringr::str_detect(
      stringr::str_to_lower(Tag),
      stringr::str_to_lower(pattern)
    )) %>%
    pull(Tag) %>%
    sort()
}

num <- function(x) suppressWarnings(as.numeric(x))

sum2_na <- function(a, b) {
  a_num <- num(a)
  b_num <- num(b)
  out <- dplyr::coalesce(a_num, 0) + dplyr::coalesce(b_num, 0)
  both_na <- is.na(a_num) & is.na(b_num)
  out[both_na] <- NA_real_
  out
}


# -------------------------------------------------------------------------
# CONTROLLI
# -------------------------------------------------------------------------

all_players <- c(players_ft, players_u19, players_u17, players_u13)
dup_players <- sort(unique(all_players[duplicated(all_players)]))

if (length(dup_players) > 0) {
  warning(
    paste0(
      "Attenzione: questi giocatori compaiono in piu' roster: ",
      paste(dup_players, collapse = ", "),
      ". In questo caso conviene filtrare anche con una colonna squadra."
    )
  )
}

# -------------------------------------------------------------------------
# HELPER
# -------------------------------------------------------------------------

subset_by_players <- function(df, players, squad_name) {
  out <- df[df$`Player Name` %in% players, , drop = FALSE]
  
  if (nrow(out) == 0) {
    stop(
      paste0(
        "Nessuna riga trovata nel file unico per la squadra ", squad_name,
        ". Controlla i nomi in `Player Name`."
      )
    )
  }
  
  out
}

build_base_context <- function(df_raw, players, matches) {
  cph <- prep_physical_context(df_raw, players = NULL)
  
  ctx_session <- prep_skill_context(df_raw, players = NULL, data_type = "session")
  ctx_pf      <- prep_skill_context(df_raw, players = NULL, data_type = "pf")
  ctx_matches <- prep_skill_context(
    df_raw,
    players = NULL,
    data_type = "match",
    matches_list = matches
  )
  
  pcts <- make_player_pcts(cph$df_physicalreport, ctx_pf$agg_all)
  scatter <- build_df_scatter_phy_tec(ctx_pf, cph$df_physicalreport, players)
  run_ctx <- prep_run_context(df_raw, players)
  
  list(
    df = df_raw,
    cph = cph,
    ctx_session = ctx_session,
    ctx_pf = ctx_pf,
    ctx_matches = ctx_matches,
    pcts = pcts,
    scatter = scatter,
    run_ctx = run_ctx
  )
}

session_tag_u13 <- "Full Session U13"

build_u13_context <- function(df_raw, session_tag = "Full Session U13") {
  
  df_u13_session <- df_raw %>%
    filter(Tag == session_tag)
  
  if (nrow(df_u13_session) == 0) {
    stop(
      paste0(
        "Nessuna riga trovata per il tag U13: '", session_tag, "'."
      )
    )
  }
  
  # contesto tecnico sulle sole Full Session U13
  ctx_skill <- prep_skill_context(
    df_u13_session,
    players = NULL,
    data_type = "match",
    matches_list = unique(df_u13_session$Tag)
  )
  
  # contesto fisico sulle sole Full Session U13
  cph <- prep_physical_context(
    df_raw,
    players = NULL,
    tag_keep = session_tag
  )
  
  # una sola riga per player lato tecnico
  skill_u13_one_row <- ctx_skill$agg_all %>%
    group_by(player) %>%
    summarise(
      category              = first(na.omit(category)),
      possession_index      = mean(possession_index, na.rm = TRUE),
      touches_index         = mean(touches_index, na.rm = TRUE),
      rv_index              = mean(rv_index, na.rm = TRUE),
      share_touch_R         = mean(share_touch_R, na.rm = TRUE),
      share_touch_L         = mean(share_touch_L, na.rm = TRUE),
      avg_time_per_poss_sec = mean(avg_time_per_poss_sec, na.rm = TRUE),
      share_1T              = mean(share_1T, na.rm = TRUE),
      share_short           = mean(share_short, na.rm = TRUE),
      share_long            = mean(share_long, na.rm = TRUE),
      .groups = "drop"
    )
  
  # una sola riga per player lato fisico
  phys_u13_one_row <- cph$df_physicalreport %>%
    group_by(player) %>%
    summarise(
      meters_per_min = mean(workrate, na.rm = TRUE),
      .groups = "drop"
    )
  
  # dataset finale per report tecnico
  agg_report <- skill_u13_one_row %>%
    left_join(phys_u13_one_row, by = "player")
  
  list(
    ctx_skill = ctx_skill,
    cph = cph,
    agg_report = agg_report
  )
}


build_named_output <- function(code, ctx, extras = list()) {
  base_out <- list(
    df = ctx$df,
    cph = ctx$cph,
    ctx_session = ctx$ctx_session,
    ctx_pf = ctx$ctx_pf,
    ctx_matches = ctx$ctx_matches,
    scatter = ctx$scatter,
    run_ctx = ctx$run_ctx,
    pcts = ctx$pcts
  )
  
  names(base_out) <- paste0(names(base_out), "_", code)
  c(base_out, extras)
}

save_context <- function(obj, filename) {
  dir.create(here("data", "derived"), recursive = TRUE, showWarnings = FALSE)
  saveRDS(obj, here("data", "derived", filename))
}

# -------------------------------------------------------------------------
# PRE-SPLIT DEL RAW UNA SOLA VOLTA
# -------------------------------------------------------------------------

df_ft_raw  <- subset_by_players(df_all, players_ft,  "FT")
df_u19_raw <- subset_by_players(df_all, players_u19, "U19")
df_u17_raw <- subset_by_players(df_all, players_u17, "U17")
df_u13_raw <- subset_by_players(df_all, players_u13, "U13")

match_ft  <- infer_match_tags(df_ft_raw)
match_u19 <- infer_match_tags(df_u19_raw)
match_u17 <- infer_match_tags(df_u17_raw)

# -------------------------------------------------------------------------
# BUILD CONTEXTS
# Ordine importante:
# 1) FT
# 2) U19 (usa FT per comparazioni)
# 3) U17 (usa U19 + FT per comparazioni)
# -------------------------------------------------------------------------

ft_ctx  <- build_base_context(df_ft_raw,  players_ft,  match_ft)
u19_ctx <- build_base_context(df_u19_raw, players_u19, match_u19)
u17_ctx <- build_base_context(df_u17_raw, players_u17, match_u17)
u13_ctx <- build_u13_context(df_u13_raw, session_tag = session_tag_u13)

# -------------------------------------------------------------------------
# COMPARAZIONI U19 vs FT
# -------------------------------------------------------------------------

u19_extras <- list(
  scatter_u19_ft_session = build_compare_dataset_phy_tec(
    u19_ctx$ctx_session, u19_ctx$cph$df_physicalreport,
    ft_ctx$ctx_session,  ft_ctx$cph$df_physicalreport
  ),
  scatter_u19_ft_pf = build_compare_dataset_phy_tec(
    u19_ctx$ctx_pf, u19_ctx$cph$df_physicalreport,
    ft_ctx$ctx_pf,  ft_ctx$cph$df_physicalreport
  ),
  scatter_u19_ft_match = build_compare_dataset_phy_tec(
    u19_ctx$ctx_matches, u19_ctx$cph$df_physicalreport,
    ft_ctx$ctx_matches,  ft_ctx$cph$df_physicalreport
  )
)

# -------------------------------------------------------------------------
# COMPARAZIONI U17 vs U19 vs FT
# -------------------------------------------------------------------------

u17_extras <- list(
  scatter_u17_over_session = build_compare_dataset_phy_tec_3(
    ctx_lower = u17_ctx$ctx_session, df_phys_lower = u17_ctx$cph$df_physicalreport,
    ctx_mid   = u19_ctx$ctx_session, df_phys_mid   = u19_ctx$cph$df_physicalreport,
    ctx_upper = ft_ctx$ctx_session,  df_phys_upper = ft_ctx$cph$df_physicalreport,
    squad_labels = c(lower = "U17", mid = "U19", upper = "FT")
  ),
  scatter_u17_over_pf = build_compare_dataset_phy_tec_3(
    ctx_lower = u17_ctx$ctx_pf, df_phys_lower = u17_ctx$cph$df_physicalreport,
    ctx_mid   = u19_ctx$ctx_pf, df_phys_mid   = u19_ctx$cph$df_physicalreport,
    ctx_upper = ft_ctx$ctx_pf,  df_phys_upper = ft_ctx$cph$df_physicalreport,
    squad_labels = c(lower = "U17", mid = "U19", upper = "FT")
  ),
  scatter_u17_over_match = build_compare_dataset_phy_tec_3(
    ctx_lower = u17_ctx$ctx_matches, df_phys_lower = u17_ctx$cph$df_physicalreport,
    ctx_mid   = u19_ctx$ctx_matches, df_phys_mid   = u19_ctx$cph$df_physicalreport,
    ctx_upper = ft_ctx$ctx_matches,  df_phys_upper = ft_ctx$cph$df_physicalreport,
    squad_labels = c(lower = "U17", mid = "U19", upper = "FT")
  )
)

# -------------------------------------------------------------------------
# OUTPUT FINALI CON GLI STESSI NOMI DI PRIMA
# -------------------------------------------------------------------------

out_ft  <- build_named_output("ft",  ft_ctx)
out_u19 <- build_named_output("u19", u19_ctx, u19_extras)
out_u17 <- build_named_output("u17", u17_ctx, u17_extras)
out_u13 <- list(
  ctx_skill_u13 = u13_ctx$ctx_skill,
  cph_u13 = u13_ctx$cph,
  agg_report_u13 = u13_ctx$agg_report
)

# -------------------------------------------------------------------------
# SAVE
# -------------------------------------------------------------------------

save_context(out_ft,  "ft_context.rds")
save_context(out_u19, "u19_context.rds")
save_context(out_u17, "u17_context.rds")
save_context(out_u13, "u13_context.rds")


