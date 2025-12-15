library(dplyr)
library(gt)
library(scales)

top5_table <- function(data, stat_col, stat_name = NULL, digits = 1, suffix = "") {
  stat_col <- rlang::ensym(stat_col)
  if (is.null(stat_name)) stat_name <- rlang::as_label(stat_col)
  
  out <- data %>%
    filter(!is.na(!!stat_col)) %>%
    arrange(desc(!!stat_col)) %>%
    slice_head(n = 5) %>%
    transmute(
      Giocatore = player,
      Valore = round(!!stat_col, digits)
    )
  
  gt(out) %>%
    tab_header(
      title = md(paste0("**", stat_name, "**")),
      subtitle = md("Top 5 giocatori")
    ) %>%
    fmt_number(columns = Valore, decimals = digits) %>%
    cols_label(Valore = paste0("Valore", suffix)) %>%
    tab_style(
      style = list(cell_text(weight = "bold", color = "#FF2E2E")),
      locations = cells_title(groups = "title")
    ) %>%
    tab_options(
      table.background.color = "#111111",
      heading.background.color = "#111111",
#      column_labels.background.color = "#111111",
      table.font.color = "white",
#      column_labels.font.color = "white",
      row.striping.background_color = "#151515",
      row.striping.include_table_body = TRUE,
      table.border.top.style = "hidden",
      table.border.bottom.style = "hidden",
      data_row.padding = px(6)
    )
}

data_topspeed <- physical_data %>%
  filter(!is.na(TopSpeed)) %>%
  arrange(desc(TopSpeed)) %>% 
  slice_head(n = 10) %>%
  transmute(
    Giocatore = player, 
    Valore = round(TopSpeed, 2)*3.6, 
    Data = date
  )

tb_topspeed <- gt(data_topspeed) %>%
  tab_header(
    title = md(paste0("**", "TopSpeed all time", "**")),
    subtitle = md("Top 10 giocatori")
  ) %>%
  fmt_number(columns = Valore, decimals = 2) %>%
  cols_label(Valore = paste0("Valore")) %>%
  tab_style(
    style = list(cell_text(weight = "bold", color = "#FF2E2E")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_options(
    table.background.color = "#111111",
    heading.background.color = "#111111",
    #      column_labels.background.color = "#111111",
    table.font.color = "white",
    #      column_labels.font.color = "white",
    row.striping.background_color = "#151515",
    row.striping.include_table_body = TRUE,
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    data_row.padding = px(6)
  )

data_workrate <- physical_data %>%
  filter(!is.na(WorkRate)) %>%
  group_by(player) %>%
  summarise(WorkRate = mean(WorkRate)) %>%
  arrange(desc(WorkRate)) %>% 
  slice_head(n = 10) %>%
  transmute(
    Giocatore = player, 
    Valore = round(WorkRate, 2), 
  )


tb_workrate <- gt(data_workrate) %>%
  tab_header(
    title = md(paste0("**", "WorkRate medio", "**")),
    subtitle = md("Top 10 giocatori")
  ) %>%
  fmt_number(columns = Valore, decimals = 2) %>%
  cols_label(Valore = paste0("Valore")) %>%
  tab_style(
    style = list(cell_text(weight = "bold", color = "#FF2E2E")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_options(
    table.background.color = "#111111",
    heading.background.color = "#111111",
    #      column_labels.background.color = "#111111",
    table.font.color = "white",
    #      column_labels.font.color = "white",
    row.striping.background_color = "#151515",
    row.striping.include_table_body = TRUE,
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    data_row.padding = px(6)
  )


data_touches <- df_skill %>%
  filter(is.na(rv_z1)) %>%
  group_by(player) %>%
  summarise(touches = mean(touch_per_min)) %>%
  arrange(desc(touches)) %>%
  slice_head(n = 10) %>%
  transmute(
    Giocatore = player, 
    Valore = round(touches, 2)
  )


tb_touches <- gt(data_touches) %>%
  tab_header(
    title = md(paste0("**", "Tocchi medi per minuto", "**")),
    subtitle = md("Top 10 giocatori")
  ) %>%
  fmt_number(columns = Valore, decimals = 2) %>%
  cols_label(Valore = paste0("Valore")) %>%
  tab_style(
    style = list(cell_text(weight = "bold", color = "#FF2E2E")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_options(
    table.background.color = "#111111",
    heading.background.color = "#111111",
    #      column_labels.background.color = "#111111",
    table.font.color = "white",
    #      column_labels.font.color = "white",
    row.striping.background_color = "#151515",
    row.striping.include_table_body = TRUE,
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    data_row.padding = px(6)
  )


data_releases <- df_skill %>%
  filter(is.na(rv_z1)) %>%
  group_by(player) %>%
  summarise(releases = mean(releases_per_min)) %>%
  arrange(desc(releases)) %>%
  slice_head(n = 10) %>%
  transmute(
    Giocatore = player, 
    Valore = round(releases, 2)
  )


tb_releases <- gt(data_releases) %>%
  tab_header(
    title = md(paste0("**", "Rilasci medi per minuto", "**")),
    subtitle = md("Top 10 giocatori")
  ) %>%
  fmt_number(columns = Valore, decimals = 2) %>%
  cols_label(Valore = paste0("Valore")) %>%
  tab_style(
    style = list(cell_text(weight = "bold", color = "#FF2E2E")),
    locations = cells_title(groups = "title")
  ) %>%
  tab_options(
    table.background.color = "#111111",
    heading.background.color = "#111111",
    #      column_labels.background.color = "#111111",
    table.font.color = "white",
    #      column_labels.font.color = "white",
    row.striping.background_color = "#151515",
    row.striping.include_table_body = TRUE,
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    data_row.padding = px(6)
  )
