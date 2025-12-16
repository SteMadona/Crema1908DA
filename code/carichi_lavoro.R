library(dplyr)
library(gt)
library(scales)


# supponiamo che il tuo dataset si chiami df (cambialo col nome reale)
data_carichi <- df_physicalreport_week %>%
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

tb_carichi_lavoro <- gt(data_carichi) %>%
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
