library(readxl)
library(here)

source(here("code", "ReportUnoNew.R"))
source(here("code", "ReportDueNew.R"))
source(here("code", "Temi_grafici.R"))
source(here("code", "phy_vs_tech_new.R"))
source(here("code", "player_wheel.R"))
source(here("code", "run_dynamics_new.R"))


# FIRST TEAM --------------------------------------------------------------

df_ft <- read_excel(here("data", "CremaFT_2401.xlsx"))

players_ft <- c("G. Recino", "E. Maccherini", "L. Niculae", "S. Azzali", "A. Arpini",
                "T. Valdameri", "N. Abba", "M. Vailati", "J. Mozzanica", "A. Latini",
                "R. Tomella", "R. Pavesi", "E. Neci", "D. Fenotti", "L. Gramignoli", 
                "T. Erman", "M. Varisco", "J. Ceserani", "T. Serioli", "V. Camilleri")


match_ft <- c("CREMA vs PRO PALAZZOLO 1a", "CREMA vs PISTOIESE", "CREMA vs SANTANGELO 1a",
              "CREMA vs LENTIGIONE")

cph_ft <- prep_physical_context(df_ft, players = players_ft)

ctx_session_ft <- prep_skill_context(df_ft, players = players_ft, data_type = "session")
ctx_pf_ft      <- prep_skill_context(df_ft, players = players_ft, data_type = "pf")
ctx_matches_ft <- prep_skill_context(df_ft, players = players_ft, data_type = "match", matches_list = match_ft)

pcts_ft <- make_player_pcts(cph_ft$df_physicalreport, ctx_pf_ft$agg_all)

scatter_ft <- build_df_scatter_phy_tec(ctx_pf_ft, cph_ft$df_physicalreport, players_ft)

run_ctx_ft <- prep_run_context(df_ft, players_ft)

out_ft <- list(
  df_ft = df_ft,
  cph_ft = cph_ft,
  ctx_session_ft = ctx_session_ft,
  ctx_pf_ft = ctx_pf_ft,
  ctx_matches_ft = ctx_matches_ft, 
  scatter_ft = scatter_ft, 
  run_ctx_ft = run_ctx_ft,
  pcts_ft = pcts_ft
)

out_path_ft <- here::here("data", "derived", "ft_context.rds")
saveRDS(out_ft, out_path_ft)





