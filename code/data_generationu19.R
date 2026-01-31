library(readxl)
library(here)

source(here("code", "ReportUnoNew.R"))
source(here("code", "ReportDueNew.R"))
source(here("code", "Temi_grafici.R"))
source(here("code", "phy_vs_tech_new.R"))
source(here("code", "player_wheel.R"))
source(here("code", "run_dynamics_new.R"))



# UNDER 19 ----------------------------------------------------------------

df_u19 <- read_excel(here("data", "CremaU19.xlsx"))

players_u19 <- c("C. Brazzorotto", "C. Barzi", "M. Bertazzoli", "C. Bertazzoni", "M. Bonizzoni", 
                 "G. Cerrato", "A. Cicarè", "W. De Maio", "M. D'Ischia", "I. Dimov",
                 "M. Fino", "V. Gerasym", "A. Jarid", "A. Nicotra", "F. Valletti",
                 "F. Montemezzani", "S. Schiavini", "B. Pape", "R. Tabal", "M. Tajeddine",
                 "F. Cerioli")

match_u19 <- c("CREMA vs VARESE U19", "CREMA vs SANGIULIANO CITY U19", "CREMA VS PIACENZA U19", "CREMA vs FOLGORE CARATESE U19", 
               "Club Milano vs Crema u19", "PRO SESTO vs CREMA U19")


cph_u19 <- prep_physical_context(df_u19, players = players_u19)

ctx_session_u19 <- prep_skill_context(df_u19, players = players_u19, data_type = "session")
ctx_pf_u19      <- prep_skill_context(df_u19, players = players_u19, data_type = "pf")
ctx_matches_u19 <- prep_skill_context(df_u19, players = players_u19, data_type = "match", matches_list = match_u19)

pcts_u19 <- make_player_pcts(cph_u19$df_physicalreport, ctx_pf_u19$agg_all)

scatter_u19 <- build_df_scatter_phy_tec(ctx_pf_u19, cph_u19$df_physicalreport, players_u19)

run_ctx_u19 <- prep_run_context(df_u19, players_u19)

scatter_u19_ft_session <- build_compare_dataset_phy_tec(ctx_session_u19, cph_u19$df_physicalreport, ctx_session_ft, cph_ft$df_physicalreport)
scatter_u19_ft_pf <- build_compare_dataset_phy_tec(ctx_pf_u19, cph_u19$df_physicalreport, ctx_pf_ft, cph_ft$df_physicalreport)
scatter_u19_ft_match <- build_compare_dataset_phy_tec(ctx_matches_u19, cph_u19$df_physicalreport, ctx_matches_ft, cph_ft$df_physicalreport)

out_u19 <- list(
  df_u19 = df_u19,
  cph_u19 = cph_u19,
  ctx_session_u19 = ctx_session_u19,
  ctx_pf_u19 = ctx_pf_u19,
  ctx_matches_u19 = ctx_matches_u19, 
  scatter_u19 = scatter_u19, 
  run_ctx_u19 = run_ctx_u19,
  pcts_u19 = pcts_u19, 
  scatter_u19_ft_match = scatter_u19_ft_match, 
  scatter_u19_ft_pf = scatter_u19_ft_pf, 
  scatter_u19_ft_session = scatter_u19_ft_session
)

out_path_u19 <- here::here("data", "derived", "u19_context.rds")
saveRDS(out_u19, out_path)

