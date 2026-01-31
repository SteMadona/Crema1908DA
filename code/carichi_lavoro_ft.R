library(dplyr)
library(gt)
library(scales)

ctx <- readRDS(here::here("data", "derived", "ft_context.rds"))


df_physicalreport_week_ft <- cph_ft$physical_data %>%
  group_by(player, week_id) %>%
  summarise(
    HID = mean((SpeedZ5_m + SpeedZ6_m) / duration, na.rm = TRUE),
    sprint_dist = sum(SpeedZ7_m, na.rm = TRUE) / pmax(n(), 1),
    topspeed = max(TopSpeed, na.rm = TRUE) * 3.6,
    workrate = mean(WorkRate, na.rm = TRUE),
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
  left_join(cph_ft$physical_data %>% select(player, category) %>% distinct(), by = "player") %>%
  group_by(player) %>%
  mutate(
    acc_index = {
      m <- max(acc_index, na.rm = TRUE)
      if (!is.finite(m) || m == 0) NA_real_ else acc_index / m * 100
    },
    dec_index = {
      m <- max(dec_index, na.rm = TRUE)
      if (!is.finite(m) || m == 0) NA_real_ else dec_index / m * 100
    }
  ) %>%
  ungroup()

data_carichi_ft <- df_physicalreport_week_ft %>%
  group_by(week_id) %>%
  summarise(
    workrate   = mean(workrate, na.rm = TRUE),
    HID        = mean(HID, na.rm = TRUE),
    acc_index  = mean(acc_index, na.rm = TRUE),
    dec_index  = mean(dec_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(week_id) %>%
  rename(Week = week_id)

tb_carichi_lavoro_ft <- gt(data_carichi_ft) %>%
  tab_header(
    title = md("**Carichi di lavoro**"),
    subtitle = md("Medie settimanali (tutti i giocatori)")
  ) %>%
  fmt_number(columns = c(workrate, HID, acc_index, dec_index), decimals = 2) %>%
  cols_label(
    workrate  = "Media WorkRate",
    HID       = "Media HID",
    acc_index = "Media Acc. Index",
    dec_index = "Media Dec. Index"
  ) %>%
  tab_style(
    style = list(cell_text(weight = "bold", color = "#FF2E2E")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_options(
    table.background.color = "#111111",
    heading.background.color = "#111111",
    table.font.color = "white",
    row.striping.background_color = "#151515",
    row.striping.include_table_body = TRUE,
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    data_row.padding = px(6)
  ) %>%
  cols_width(
    Week ~ px(90),
    everything() ~ px(150)
  )
