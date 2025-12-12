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

top5_table(physical_data, TopSpeed)
