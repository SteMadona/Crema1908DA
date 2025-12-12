library(ggrepel)  
library(scales)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(rlang)
library(lubridate)

# --- PREPARAZIONE DATI ------------------------------------------------------

# Calcolo la media dei tocchi totali per giocatore
df_touch_avg <- df_skill %>%
  mutate(touches_total = touch_L + touch_R) %>%
  group_by(player) %>%
  summarise(touches_total_avg = mean(touches_total, na.rm = TRUE), .groups = "drop")

df_physicalreport <- df_physicalreport %>%
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
df_scatter <- agg %>%
  left_join(df_physicalreport %>% select(player, physical_index), by = "player") %>%
  left_join(df_touch_avg, by = "player") %>%
  filter(category != "Goalkeepers" & !player %in% c("C. Barzi", "G. D'Errico", "F. Cerioli"))

# --- INDICE DI QUALITÀ TECNICA ----------------------------------------------

# Costruiamo un indice composito (0–100)
df_scatter <- df_scatter %>%
  mutate(
    one_touch_share = share_1T,  # % di possesso in tocchi di prima
    quality_index_raw = (
      scales::rescale(releases_per_min, to = c(0,1), from = range(releases_per_min, na.rm=TRUE)) * 0.4 +
        scales::rescale(release_vel_avg,  to = c(0,1), from = range(one_touch_share, na.rm=TRUE))  * 0.4 +
        scales::rescale(touches_total_avg, to = c(0, 1), from = range(touches_total_avg, na.rm = T)) *0.2
    ),
    quality_index = rescale(quality_index_raw, to = c(0, 1))
  )

# --- MEDIE PER LINEE TRATTEGGIATE -------------------------------------------

mean_physical <- mean(df_scatter$physical_index, na.rm = TRUE)
mean_quality  <- mean(df_scatter$quality_index, na.rm = TRUE)


# --- SCATTERPLOT ------------------------------------------------------------

