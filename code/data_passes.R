library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(rlang)
library(lubridate)
library(here)
library(stringr)

df <- read_excel(here("data", "CremaFT_1910.xlsx"))

df_passes <- df %>%
  filter(!(`Successfull passes (#)` == "N/A")) %>%
  select(`Player Name`, `Position Category`, `Successfull passes (#)`, 
         `Lost Possessions (#)`, `Total Possessions (#)`, `Regains (#)`) %>%
  group_by(`Player Name`, `Position Category`) %>%
  summarise(succ_pass = sum(as.numeric(`Successfull passes (#)`)),
            tot_poss = sum(as.numeric(`Total Possessions (#)`)), 
            lost_poss = sum(as.numeric(`Lost Possessions (#)`)), 
            regains = sum(as.numeric(`Regains (#)`))) %>%
  filter(`Position Category` != "Goalkeepers") 


names(df_passes) <- c("player", "position", "succ_pass", "tot_poss", "lost_poss", "regains")


df_passes <- df_passes %>%
  mutate(pass_perc = round((succ_pass/tot_poss)*100, 2), 
         loss_perc = round((lost_poss/tot_poss)*100, 2)) %>%
  filter(!(player %in% c("F. Cerioli", "G. D'Errico", "C. Barzi")))

mean_passes <- mean(df_passes$pass_perc, na.rm = T)
mean_regains <- mean(df_passes$regains, na.rm = T)

scatter_pass_regain <- ggplot(df_passes, aes(x = regains, y = pass_perc)) +
  geom_point(
    aes(fill = position),
    shape = 21,       # 🔹 Cerchio con bordo
    color = "white",  # 🔹 Bordo bianco
    size = 5,         # 🔹 Pallini più grandi
    alpha = 0.9,
    stroke = 0.7      # 🔹 Spessore del bordo
  ) +
  geom_text(
    aes(label = player),
    vjust = -1,
    color = "white",
    size = 3,
    alpha = 0.8
  ) +
  geom_vline(xintercept = mean_regains, linetype = "dashed", color = "gray60", size = 1.1) +
  geom_hline(yintercept = mean_passes,  linetype = "dashed", color = "gray60", size = 1.1) +
  
  scale_fill_manual(values = c(
    "Defenders" = "#FFFFFF",     # bianco
    "Midfielders" = "#FF2E2E",   # rosso
    "Forwards" = "gray30"        # grigio scuro
  )) +
  
  guides(
    fill = guide_legend(
      override.aes = list(color = "white")  # legenda con bordo bianco
    )
  ) +
  
  labs(
    title = "Passaggi vs Recuperi",
    subtitle = "Percentuale di passaggi completati vs Palloni recuperati",
    x = "Recuperi",
    y = "% Passaggi completati",
    fill = "Ruolo"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#111111", color = NA),
    panel.background = element_rect(fill = "#111111", color = NA),
    legend.background = element_rect(fill = "#111111", color = NA),
    panel.grid.major = element_line(color = "#333333"),
    panel.grid.minor = element_line(color = "#222222"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    plot.title = element_text(color = "#FF2E2E", face = "bold", size = 16),
    plot.subtitle = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  )


df_partita_finale <- df %>%
  filter(str_detect(Tag, "Partita finale"))

df_passes_part <- df_partita_finale %>%
  filter(!(`Successfull passes (#)` == "N/A")) %>%
  select(`Player Name`, `Position Category`, `Successfull passes (#)`, 
         `Lost Possessions (#)`, `Total Possessions (#)`, `Regains (#)`) %>%
  group_by(`Player Name`, `Position Category`) %>%
  summarise(succ_pass = sum(as.numeric(`Successfull passes (#)`)),
            tot_poss = sum(as.numeric(`Total Possessions (#)`)), 
            lost_poss = sum(as.numeric(`Lost Possessions (#)`)), 
            regains = sum(as.numeric(`Regains (#)`))) %>%
  filter(`Position Category` != "Goalkeepers")


names(df_passes_part) <- c("player", "position", "succ_pass", "tot_poss", "lost_poss", "regains")


df_passes_part <- df_passes_part %>%
  mutate(pass_perc = round((succ_pass/tot_poss)*100, 2), 
         loss_perc = round((lost_poss/tot_poss)*100, 2)) %>%
  filter(!(player %in% c("F. Cerioli", "G. D'Errico", "C. Barzi")))

mean_passes_part <- mean(df_passes_part$pass_perc, na.rm = T)
mean_regains_part <- mean(df_passes_part$regains, na.rm = T)

scatter_pass_regain_part <- ggplot(df_passes_part, aes(x = regains, y = pass_perc)) +
  geom_point(
    aes(fill = position),
    shape = 21,       # 🔹 Cerchio con bordo
    color = "white",  # 🔹 Bordo bianco
    size = 5,         # 🔹 Pallini più grandi
    alpha = 0.9,
    stroke = 0.7      # 🔹 Spessore del bordo
  ) +
  geom_text(
    aes(label = player),
    vjust = -1,
    color = "white",
    size = 3,
    alpha = 0.8
  ) +
  geom_vline(xintercept = mean_regains_part, linetype = "dashed", color = "gray60", size = 1.1) +
  geom_hline(yintercept = mean_passes_part,  linetype = "dashed", color = "gray60", size = 1.1) +
  
  scale_fill_manual(values = c(
    "Defenders" = "#FFFFFF",     # bianco
    "Midfielders" = "#FF2E2E",   # rosso
    "Forwards" = "gray30"        # grigio scuro
  )) +
  
  guides(
    fill = guide_legend(
      override.aes = list(color = "white")  # legenda con bordo bianco
    )
  ) +
  
  labs(
    title = "Passaggi vs Recuperi",
    subtitle = "Percentuale di passaggi completati vs Palloni recuperati",
    x = "Recuperi",
    y = "% Passaggi completati",
    fill = "Ruolo"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    plot.background = element_rect(fill = "#111111", color = NA),
    panel.background = element_rect(fill = "#111111", color = NA),
    legend.background = element_rect(fill = "#111111", color = NA),
    panel.grid.major = element_line(color = "#333333"),
    panel.grid.minor = element_line(color = "#222222"),
    text = element_text(color = "white"),
    axis.text = element_text(color = "white"),
    axis.title = element_text(color = "white", face = "bold"),
    plot.title = element_text(color = "#FF2E2E", face = "bold", size = 16),
    plot.subtitle = element_text(color = "white"),
    legend.text = element_text(color = "white"),
    legend.title = element_text(color = "white")
  )


