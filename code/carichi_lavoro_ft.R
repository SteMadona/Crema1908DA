library(dplyr)
library(gt)
library(scales)

ctx <- readRDS(here::here("data", "derived", "ft_context.rds"))

theme_gt_crema <- function(tbl) {
  tbl %>%
    opt_row_striping() %>%
    tab_options(
      table.width = pct(100),
      heading.align = "left",
      table.background.color = "#ffffff",
      heading.background.color = "#ffffff",
      column_labels.background.color = "#f8fafc",
      table.font.color = "#16202a",
      table.border.top.color = "#e6eaf0",
      table.border.bottom.color = "#e6eaf0",
      heading.border.bottom.color = "#e6eaf0",
      table_body.hlines.color = "#eef1f5",
      row.striping.background_color = "#fafbfc",
      row.striping.include_table_body = TRUE,
      data_row.padding = px(9)
    ) %>%
    tab_style(
      style = cell_text(weight = "800", color = "#111827", size = px(22)),
      locations = cells_title(groups = "title")
    ) %>%
    tab_style(
      style = cell_text(color = "#667085", size = px(13)),
      locations = cells_title(groups = "subtitle")
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#f8fafc"),
        cell_text(weight = "700", color = "#475467")
      ),
      locations = cells_column_labels(everything())
    ) %>%
    opt_css(
      css = "
        .gt_table{
          border: 1px solid #e6eaf0 !important;
          border-radius: 18px !important;
          overflow: hidden;
          box-shadow: 0 6px 18px rgba(16,24,40,.05);
        }
        .gt_heading{
          border-bottom: 1px solid #e6eaf0;
        }
      "
    )
}

df_physicalreport_week_ft <- ctx$cph_ft$physical_data %>%
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
  left_join(
    ctx$cph_ft$physical_data %>%
      select(player, category) %>%
      distinct(),
    by = "player"
  ) %>%
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
    workrate  = mean(workrate, na.rm = TRUE),
    HID       = mean(HID, na.rm = TRUE),
    acc_index = mean(acc_index, na.rm = TRUE),
    dec_index = mean(dec_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(week_id) %>%
  rename(Week = week_id)

tb_carichi_lavoro_ft <- gt(data_carichi_ft) %>%
  tab_header(
    title = md("**Carichi di lavoro**"),
    subtitle = md("Medie settimanali di squadra")
  ) %>%
  fmt_number(
    columns = c(workrate, HID, acc_index, dec_index),
    decimals = 2
  ) %>%
  cols_label(
    Week      = "Week",
    workrate  = "WorkRate",
    HID       = "HID",
    acc_index = "Acc. Index",
    dec_index = "Dec. Index"
  ) %>%
  cols_align(
    align = "center",
    columns = c(Week, workrate, HID, acc_index, dec_index)
  ) %>%
  cols_width(
    Week ~ px(110),
    everything() ~ px(145)
  ) %>%
  theme_gt_crema() %>%
  tab_style(
    style = cell_text(weight = "600", color = "#344054"),
    locations = cells_body(columns = Week)
  )