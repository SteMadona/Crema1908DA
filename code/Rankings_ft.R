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

make_rank_table <- function(data, title, subtitle = "Top 10 giocatori", decimals = 2) {
  gt(data) %>%
    tab_header(
      title = md(paste0("**", title, "**")),
      subtitle = md(subtitle)
    ) %>%
    fmt_number(columns = Valore, decimals = decimals) %>%
    cols_align(
      align = "center",
      columns = everything()
    ) %>%
    cols_width(
      Giocatore ~ px(220),
      everything() ~ px(120)
    ) %>%
    cols_label(
      Giocatore = "Giocatore",
      Valore = "Valore"
    ) %>%
    theme_gt_crema()
}

top5_table <- function(data, stat_col, stat_name = NULL, digits = 1) {
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
  
  make_rank_table(
    data = out,
    title = stat_name,
    subtitle = "Top 5 giocatori",
    decimals = digits
  )
}

data_topspeed <- ctx$cph_ft$physical_data %>%
  filter(!is.na(TopSpeed)) %>%
  arrange(desc(TopSpeed)) %>%
  slice_head(n = 10) %>%
  transmute(
    Giocatore = player,
    Valore = TopSpeed * 3.6,
    Data = date
  )

tb_topspeed <- gt(data_topspeed) %>%
  tab_header(
    title = md("**TopSpeed all time**"),
    subtitle = md("Top 10 rilevazioni")
  ) %>%
  fmt_number(columns = Valore, decimals = 2) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  cols_width(
    Giocatore ~ px(200),
    Valore ~ px(110),
    Data ~ px(130)
  ) %>%
  cols_label(
    Giocatore = "Giocatore",
    Valore = "km/h",
    Data = "Data"
  ) %>%
  theme_gt_crema()

data_workrate <- ctx$cph_ft$physical_data %>%
  filter(!is.na(WorkRate)) %>%
  group_by(player) %>%
  summarise(WorkRate = mean(WorkRate, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(WorkRate)) %>%
  slice_head(n = 10) %>%
  transmute(
    Giocatore = player,
    Valore = WorkRate
  )

tb_workrate <- make_rank_table(
  data = data_workrate,
  title = "WorkRate medio",
  subtitle = "Top 10 giocatori",
  decimals = 2
)

data_touches <- ctx$ctx_session_ft$df_skill %>%
  group_by(player) %>%
  summarise(touches = mean(touch_per_min, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(touches)) %>%
  slice_head(n = 10) %>%
  transmute(
    Giocatore = player,
    Valore = touches
  )

tb_touches <- make_rank_table(
  data = data_touches,
  title = "Tocchi medi per minuto",
  subtitle = "Top 10 giocatori",
  decimals = 2
)

data_releases <- ctx$ctx_session_ft$df_skill %>%
  group_by(player) %>%
  summarise(releases = mean(releases_per_min, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(releases)) %>%
  slice_head(n = 10) %>%
  transmute(
    Giocatore = player,
    Valore = releases
  )

tb_releases <- make_rank_table(
  data = data_releases,
  title = "Rilasci medi per minuto",
  subtitle = "Top 10 giocatori",
  decimals = 2
)