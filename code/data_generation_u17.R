library(readxl)
library(here)

source(here("code", "ReportUnoNew.R"))
source(here("code", "ReportDueNew.R"))
source(here("code", "Temi_grafici.R"))
source(here("code", "phy_vs_tech_new.R"))
source(here("code", "player_wheel.R"))
source(here("code", "run_dynamics_new.R"))
source(here("code", "comparisons_U17_new.R"))


# UNDER 19 ----------------------------------------------------------------

df_u17 <- read_excel(here("data", "CremaU17.xlsx"))

players_u17 <- c("A. Ajdini", "M. Bresciani", "A. Brusati", "T. Calia", "E. Di Cicco", "K. Gagliardi", "L. Invernizzi",
                 "L. Kurici", "A. Laini", "M. Laini", "P. Marelli", "F. Massazza", "A. Mhilli", "S. Montoya", "F. Moruzzi",
                 "F. Ndema", "R. Nicolini", "L. Pascale", "D. Pea", "A. Riboli", "D. Shiku", "M. Vanelli")

match_u17 <- c("CREMA vs TREVIGLIESE U17", "CREMA vs BRUSAPORTO U17", "CREMA vs DESENZANO U17", "CREMA vs AURORA TRAVAGLIATO U17", 
               "VOLUNTAS vs CREMA U17", "VILLAVALLE vs CREMA U17", "CREMA vs PONTE SP MAPELLO U17")


cph_u17 <- prep_physical_context(df_u17, players = players_u17)

ctx_session_u17 <- prep_skill_context(df_u17, players = players_u17, data_type = "session")
ctx_pf_u17      <- prep_skill_context(df_u17, players = players_u17, data_type = "pf")
ctx_matches_u17 <- prep_skill_context(df_u17, players = players_u17, data_type = "match", matches_list = match_u17)

pcts_u17 <- make_player_pcts(cph_u17$df_physicalreport, ctx_pf_u17$agg_all)

scatter_u17 <- build_df_scatter_phy_tec(ctx_pf_u17, cph_u17$df_physicalreport, players_u17)

run_ctx_u17 <- prep_run_context(df_u17, players_u17)


scatter_u17_over_session <- build_compare_dataset_phy_tec_3(
  ctx_lower = ctx_session_u17, df_phys_lower = cph_u17$df_physicalreport, 
  ctx_mid   = ctx_session_u19, df_phys_mid   = cph_u19$df_physicalreport, 
  ctx_upper = ctx_session_ft, df_phys_upper = cph_ft$df_physicalreport, 
  squad_labels = c(lower="U17", mid="U19", upper="FT")
)


scatter_u17_over_pf <- build_compare_dataset_phy_tec_3(
  ctx_lower = ctx_pf_u17, df_phys_lower = cph_u17$df_physicalreport, 
  ctx_mid   = ctx_pf_u19, df_phys_mid   = cph_u19$df_physicalreport, 
  ctx_upper = ctx_pf_ft, df_phys_upper = cph_ft$df_physicalreport, 
  squad_labels = c(lower="U17", mid="U19", upper="FT")
)

scatter_u17_over_match <- build_compare_dataset_phy_tec_3(
  ctx_lower = ctx_matches_u17, df_phys_lower = cph_u17$df_physicalreport, 
  ctx_mid   = ctx_matches_u19, df_phys_mid   = cph_u19$df_physicalreport, 
  ctx_upper = ctx_matches_ft, df_phys_upper = cph_ft$df_physicalreport, 
  squad_labels = c(lower="U17", mid="U19", upper="FT")
)


out_u17 <- list(
  df_u17 = df_u17,
  cph_u17 = cph_u17,
  ctx_session_u17 = ctx_session_u17,
  ctx_pf_u17 = ctx_pf_u17,
  ctx_matches_u17 = ctx_matches_u17, 
  scatter_u17 = scatter_u17, 
  run_ctx_u17 = run_ctx_u17,
  pcts_u17 = pcts_u17, 
  scatter_u17_over_match = scatter_u17_over_match, 
  scatter_u17_over_pf = scatter_u17_over_pf, 
  scatter_u17_over_session = scatter_u17_over_session
)

out_path_u17 <- here::here("data", "derived", "u17_context.rds")
saveRDS(out_u17, out_path_u17)

