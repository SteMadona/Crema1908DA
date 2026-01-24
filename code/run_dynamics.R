df <- read_excel(here("data", "CremaFT_2401.xlsx"))

#THEME 
library(tidyverse)
library(lubridate)
library(ggrepel)
library(patchwork)
library(scales)
library(gt)


theme_crema_light <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background  = element_rect(fill = "#FFFFFF", color = NA),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.border     = element_rect(color = "#E6E6E6", fill = NA, linewidth = 0.6),
      
      panel.grid.major = element_line(color = "grey", linewidth = 0.4),
      panel.grid.minor = element_line(color = "grey", linewidth = 0.4),
      
      text        = element_text(color = "#111111"),
      axis.text   = element_text(color = "#111111"),
      axis.title  = element_text(color = "#111111", face = "bold"),
      plot.title  = element_text(color = "#FF2E2E", face = "bold", size = 16),
      plot.subtitle = element_text(color = "#111111"),
      
      axis.ticks = element_line(color = "#CFCFCF"),
      axis.ticks.length = unit(3, "pt")
    )
}



# OVERALL

df_dynamic <- df %>%
  transmute(
    date        = as.Date(Date),
    player      = `Player Name`,
    category    = `Position Category`,
    position    = Position,
    tag         = Tag,
    
    ocd_sx_mean = `Overall Contact Duration - Left Mean (sec)`,
    ocd_sx_std  = `Overall Contact Duration - Left std (sec)`,
    ocd_dx_mean = `Overall Contact Duration - Right Mean (sec)`,
    ocd_dx_std  = `Overall Contact Duration - Right std (sec)`, 
    ocd_sym_id  = `Overall Contact Duration - Symmetry Indx (%)`, 
    
    ofd_sx_mean = `Overall Flight Duration - Left Mean (sec)`, 
    ofd_sx_std  = `Overall Flight Duration - Left std (sec)`, 
    ofd_dx_mean = `Overall Flight Duration - Right Mean (sec)`, 
    ofd_dx_std  = `Overall Flight Duration - Right std (sec)`, 
    ofd_sym_id  = `Overall Flight Duration - Symmetry Indx (%)`, 
    
    osl_sx_mean  = `Overall Stride Length Left Mean (cm)`,
    osl_sx_std   = `Overall Stride Length Left std (cm)`, 
    osl_dx_mean  = `Overall Stride Length Right Mean (cm)`, 
    osl_dx_std   = `Overall Stride Length Right std (cm)`, 
    osl_sym_id   = `Overall Stride Length Symmetry Index (%)`
    
  )


num_cols <- setdiff(names(df_dynamic), c("date", "player","category","position", "tag"))
df_dynamic[num_cols] <- lapply(df_dynamic[num_cols], function(x) suppressWarnings(as.numeric(x)))

df_dynamic_match <- df_dynamic %>% 
  filter(tag %in% c("Partita finale martedi", "Partita finale mercoledi", 
                    "Match giovedi", "Partita finale venerdi"))


df_dynamic_report <- df_dynamic_match %>%
  group_by(player) %>%
  summarise(
    ocd_sx_mean = mean(ocd_sx_mean), 
    ocd_sx_std = mean(ocd_sx_std), 
    ocd_dx_mean = mean(ocd_dx_mean), 
    ocd_dx_std = mean(ocd_dx_std), 
    ocd_sym_id = mean(ocd_sym_id), 
    
    ofd_sx_mean = mean(ofd_sx_mean), 
    ofd_sx_std = mean(ofd_sx_std), 
    ofd_dx_mean = mean(ofd_dx_mean), 
    ofd_dx_std = mean(ofd_dx_std), 
    ofd_sym_id = mean(ofd_sym_id), 
    
    osl_sx_mean = mean(osl_sx_mean), 
    osl_sx_std = mean(osl_sx_std), 
    osl_dx_mean = mean(osl_dx_mean), 
    osl_dx_std = mean(osl_dx_std), 
    osl_sym_id = mean(osl_sym_id)
    
  ) %>%
  left_join(df_dynamic_match %>% select(player, category) %>% distinct(), by = "player") %>%
  filter(category != "Goalkeepers") 

# HID

df_dynamic_hid <- df %>%
  transmute(
    date        = as.Date(Date),
    player      = `Player Name`,
    category    = `Position Category`,
    position    = Position,
    tag         = Tag,
  
    hcd_sx_mean = `HID Contact Duration Left Mean (sec)`,
    hcd_sx_std  = `HID Contact Duration Left std (sec)`,
    hcd_dx_mean = `HID Contact Duration Right Mean (sec)`,
    hcd_dx_std  = `HID Contact Duration Right std (sec)`, 
    hcd_sym_id  = `HID Contact Duration Symmetry Index (%)`, 
    
    hfd_sx_mean = `HID Flight Duration Left Mean (sec)`, 
    hfd_sx_std  = `HID Flight Duration Left std (sec)`, 
    hfd_dx_mean = `HID Flight Duration Right Mean (sec)`, 
    hfd_dx_std  = `HID Flight Duration Right std (sec)`, 
    hfd_sym_id  = `HID Flight Duration Symmetry Index (%)`, 
    
    hsl_sx_mean  = `HID Stride Length Left Mean (cm)`,
    hsl_sx_std   = `HID Stride Length Left std (cm)`, 
    hsl_dx_mean  = `HID Stride Length Right Mean (cm)`, 
    hsl_dx_std   = `HID Stride Length Right std (cm)`, 
    hsl_sym_id   = `HID Stride Length Symmetry Index (%)`
    
  )


num_cols <- setdiff(names(df_dynamic_hid), c("date", "player","category","position", "tag"))
df_dynamic_hid[num_cols] <- lapply(df_dynamic_hid[num_cols], function(x) suppressWarnings(as.numeric(x)))

df_dynamic_hid_match <- df_dynamic_hid %>% 
  filter(tag %in% c("Partita finale martedi", "Partita finale mercoledi", 
                    "Match giovedi", "Partita finale venerdi"))


df_dynamic_hid_report <- df_dynamic_hid_match %>%
  group_by(player) %>%
  summarise(
    hcd_sx_mean = mean(hcd_sx_mean, na.rm = T), 
    hcd_sx_std = mean(hcd_sx_std, na.rm = T), 
    hcd_dx_mean = mean(hcd_dx_mean, na.rm = T), 
    hcd_dx_std = mean(hcd_dx_std, na.rm = T), 
    hcd_sym_id = mean(hcd_sym_id, na.rm = T), 
    
    hfd_sx_mean = mean(hfd_sx_mean, na.rm = T), 
    hfd_sx_std = mean(hfd_sx_std, na.rm = T), 
    hfd_dx_mean = mean(hfd_dx_mean, na.rm = T), 
    hfd_dx_std = mean(hfd_dx_std, na.rm = T), 
    hfd_sym_id = mean(hfd_sym_id, na.rm = T), 
    
    hsl_sx_mean = mean(hsl_sx_mean, na.rm = T), 
    hsl_sx_std = mean(hsl_sx_std, na.rm = T), 
    hsl_dx_mean = mean(hsl_dx_mean, na.rm = T), 
    hsl_dx_std = mean(hsl_dx_std, na.rm = T), 
    hsl_sym_id = mean(hsl_sym_id, na.rm = T)
    
  ) %>%
  left_join(df_dynamic_hid_match %>% select(player, category) %>% distinct(), by = "player") %>%
  filter(category != "Goalkeepers") 



#SPRINT
df_dynamic_sprint <- df %>%
  transmute(
    date        = as.Date(Date),
    player      = `Player Name`,
    category    = `Position Category`,
    position    = Position,
    tag         = Tag,
    
    scd_sx_mean = `Sprint Contact Duration Left Mean (sec)`,
    scd_sx_std  = `Sprint Contact Duration Left std (sec)`,
    scd_dx_mean = `Sprint Contact Duration Right Mean (sec)`,
    scd_dx_std  = `Sprint Contact Duration Right std (sec)`, 
    scd_sym_id  = `Sprint Contact Duration Symmetry Index (%)`, 
    
    sfd_sx_mean = `Sprint Flight Duration Left Mean (sec)`, 
    sfd_sx_std  = `Sprint Flight Duration Left std (sec)`, 
    sfd_dx_mean = `Sprint Flight Duration Right Mean (sec)`, 
    sfd_dx_std  = `Sprint Flight Duration Right std (sec)`, 
    sfd_sym_id  = `Sprint Flight Duration Symmetry Index (%)`, 
    
    ssl_sx_mean  = `Sprint Stride Length Left Mean (cm)`,
    ssl_sx_std   = `Sprint Stride Length Left std (cm)`, 
    ssl_dx_mean  = `Sprint Stride Length Right Mean (cm)`, 
    ssl_dx_std   = `Sprint Stride Length Right std (cm)`, 
    ssl_sym_id   = `Sprint Stride Length Symmetry Index (%)`
    
  )

num_cols <- setdiff(names(df_dynamic_sprint), c("date", "player","category","position", "tag"))
df_dynamic_sprint[num_cols] <- lapply(df_dynamic_sprint[num_cols], function(x) suppressWarnings(as.numeric(x)))

df_dynamic_sprint_match <- df_dynamic_sprint %>% 
  filter(tag %in% c("Partita finale martedi", "Partita finale mercoledi", 
                    "Match giovedi", "Partita finale venerdi"))


df_dynamic_sprint_report <- df_dynamic_sprint_match %>%
  group_by(player) %>%
  summarise(
    scd_sx_mean = mean(scd_sx_mean, na.rm = T), 
    scd_sx_std = mean(scd_sx_std, na.rm = T), 
    scd_dx_mean = mean(scd_dx_mean, na.rm = T), 
    scd_dx_std = mean(scd_dx_std, na.rm = T), 
    scd_sym_id = mean(scd_sym_id, na.rm = T), 
    
    sfd_sx_mean = mean(sfd_sx_mean, na.rm = T), 
    sfd_sx_std = mean(sfd_sx_std, na.rm = T), 
    sfd_dx_mean = mean(sfd_dx_mean, na.rm = T), 
    sfd_dx_std = mean(sfd_dx_std, na.rm = T), 
    sfd_sym_id = mean(sfd_sym_id, na.rm = T), 
    
    ssl_sx_mean = mean(ssl_sx_mean, na.rm = T), 
    ssl_sx_std = mean(ssl_sx_std, na.rm = T), 
    ssl_dx_mean = mean(ssl_dx_mean, na.rm = T), 
    ssl_dx_std = mean(ssl_dx_std, na.rm = T), 
    ssl_sym_id = mean(ssl_sym_id, na.rm = T)
    
  ) %>%
  left_join(df_dynamic_sprint_match %>% select(player, category) %>% distinct(), by = "player") %>%
  filter(category != "Goalkeepers") 

#sprint per il momento esclusa perchè sono praticamente solo valori mancanti



# VISUALIZZAZIONI

robust_z <- function(x) {
  med <- median(x, na.rm = TRUE)
  madv <- mad(x, constant = 1, na.rm = TRUE)
  if (is.na(madv) || madv == 0) return(rep(0, length(x)))
  0.6745 * (x - med) / madv
}

prep_hid_session <- function(df_dynamic_hid_match) {
  df_dynamic_hid_match %>%
    mutate(
      HCD_mean = rowMeans(cbind(hcd_sx_mean, hcd_dx_mean), na.rm = TRUE),
      HFD_mean = rowMeans(cbind(hfd_sx_mean, hfd_dx_mean), na.rm = TRUE),
      HSL_mean = rowMeans(cbind(hsl_sx_mean, hsl_dx_mean), na.rm = TRUE),
      
      HCD_stab = rowMeans(cbind(hcd_sx_std, hcd_dx_std), na.rm = TRUE),
      HFD_stab = rowMeans(cbind(hfd_sx_std, hfd_dx_std), na.rm = TRUE),
      HSL_stab = rowMeans(cbind(hsl_sx_std, hsl_dx_std), na.rm = TRUE)
    )
}

gt_hid_outlier_table <- function(df_dynamic_hid_match,
                                        player_name,
                                        metric = c("HCD", "HFD", "HSL"),
                                        out_th = 2.5,
                                        top_n = NULL,              # NULL = tutti
                                        show_baseline_row = TRUE) {
  
  metric <- match.arg(metric)
  
  metric_info <- list(
    HCD = list(col = "HCD_mean", label = "HCD (Contact Duration) — mean", unit = "sec"),
    HFD = list(col = "HFD_mean", label = "HFD (Flight Duration) — mean", unit = "sec"),
    HSL = list(col = "HSL_mean", label = "HSL (Stride Length) — mean", unit = "cm")
  )
  col_name <- metric_info[[metric]]$col
  lab      <- metric_info[[metric]]$label
  unit     <- metric_info[[metric]]$unit
  
  d <- prep_hid_session(df_dynamic_hid_match) %>%
    filter(player == player_name) %>%
    filter(!is.na(.data[[col_name]])) %>%
    transmute(
      type = "session",
      date = as.Date(date),
      tag  = as.character(tag),
      value = .data[[col_name]]
    )
  
  if (nrow(d) == 0) stop(paste0("Nessun dato disponibile per ", player_name, " su ", lab))
  
  baseline_mean <- mean(d$value, na.rm = TRUE)
  
  d2 <- d %>%
    mutate(
      baseline_mean = baseline_mean,
      delta = value - baseline_mean,
      z_robusto = robust_z(value),
      outlier_flag = abs(z_robusto) >= out_th
    )
  
  out_tbl <- d2 %>%
    filter(outlier_flag) %>%
    arrange(date) %>%  # ✅ ordine cronologico
    transmute(
      type,
      date,
      tag,
      value,
      baseline_mean,
      delta,
      z_robusto
    )
  
  # opzionale: limita N ma senza perdere la cronologia (prende le prime N nel tempo)
  if (!is.null(top_n)) {
    out_tbl <- out_tbl %>% slice_head(n = top_n)
  }
  
  if (show_baseline_row) {
    base_row <- tibble(
      type = "baseline",
      date = as.Date(NA),
      tag = NA_character_,
      value = NA_real_,
      baseline_mean = baseline_mean,
      delta = NA_real_,
      z_robusto = NA_real_
    )
    out_tbl <- bind_rows(base_row, out_tbl)
  }
  
  out_tbl %>%
    gt() %>%
    tab_header(
      title = md(paste0("**", player_name, " — ", lab, "**")),
      subtitle = md(paste0("Outlier HID (|z| ≥ ", out_th, ") — ordinati per data | unità: ", unit))
    ) %>%
    cols_label(
      type = "Tipo",
      date = "Data",
      tag = "Tag",
      value = "Valore",
      baseline_mean = "Baseline",
      delta = "Delta",
      z_robusto = "Z robusto"
    ) %>%
    fmt_date(columns = date, date_style = 3) %>%
    fmt_number(columns = c(value, baseline_mean, delta), decimals = 3) %>%
    fmt_number(columns = z_robusto, decimals = 2) %>%
    tab_style(
      style = list(cell_text(weight = "bold", color = "#FF2E2E")),
      locations = cells_title(groups = "title")
    ) 
}


plot_hid_box_kpi <- function(df_dynamic_hid_match,
                             player_name,
                             metric = c("HCD", "HFD", "HSL"),
                             out_th = 2.5,
                             use_light_theme = TRUE,
                             label_fmt = "%d/%m") {
  
  metric <- match.arg(metric)
  
  hid_obs <- prep_hid_session(df_dynamic_hid_match) %>%
    filter(player == player_name)
  
  metric_info <- list(
    HCD = list(col = "HCD_mean", label = "HCD (Contact Duration) — mean", unit = "sec"),
    HFD = list(col = "HFD_mean", label = "HFD (Flight Duration) — mean", unit = "sec"),
    HSL = list(col = "HSL_mean", label = "HSL (Stride Length) — mean", unit = "cm")
  )
  
  col_name <- metric_info[[metric]]$col
  lab <- metric_info[[metric]]$label
  unit <- metric_info[[metric]]$unit
  
  d <- hid_obs %>%
    filter(!is.na(.data[[col_name]])) %>%
    transmute(
      date, tag,
      value = .data[[col_name]]
    ) %>%
    mutate(
      z_robusto = robust_z(value),
      outlier_flag = abs(z_robusto) >= out_th,
      label = if_else(outlier_flag, format(date, label_fmt), NA_character_)
    )
  
  n_sessions <- nrow(d)
  n_outliers <- sum(d$outlier_flag, na.rm = TRUE)
  med <- median(d$value, na.rm = TRUE)
  q25 <- quantile(d$value, 0.25, na.rm = TRUE)
  q75 <- quantile(d$value, 0.75, na.rm = TRUE)
  
  kpi_txt <- paste0(
    "n: ", n_sessions,
    "  |  outlier: ", n_outliers,
    "  |  median: ", round(med, 3),
    "  |  IQR: ", round(q25, 3), "–", round(q75, 3)
  )
  
  # coordinate per posizionare testo in alto a destra (prima del flip)
  x_anno <- 1.25
  y_anno <- max(d$value, na.rm = TRUE)
  
  
  if (nrow(d) == 0) stop(paste0("Nessun dato disponibile per ", player_name, " su ", lab))
  
  mu <- mean(d$value, na.rm = TRUE)
  
  p <- ggplot(d, aes(x = "", y = value)) +
    geom_boxplot(
      width = 0.25, outlier.shape = NA,
      fill = ifelse(use_light_theme, "#F2F2F2", "#2A2A2A"),
      color = ifelse(use_light_theme, "#111111", "white"),
      linewidth = 0.9
    ) +
    geom_point(
      aes(fill = outlier_flag),
      shape = 21,
      color = ifelse(use_light_theme, "#111111", "white"),
      stroke = 0.7,
      size = 4,
      alpha = 0.95,
      position = position_jitter(width = 0.12, height = 0)
    ) +
    # etichette SOLO outlier
    ggrepel::geom_text_repel(
      data = d %>% filter(outlier_flag),
      aes(label = label),
      size = 3.8,
      color = ifelse(use_light_theme, "#111111", "white"),
      box.padding = 0.35,
      point.padding = 0.25,
      max.overlaps = 50,
      min.segment.length = 0,
      segment.color = "gray60"
    ) +
    geom_hline(yintercept = mu, linetype = "dashed", color = "gray60", linewidth = 1.1) +
    scale_fill_manual(values = c(`FALSE` = "#8A8A8A", `TRUE` = "#FF2E2E"), guide = "none") +
    labs(
      title = lab,
      subtitle = paste0(player_name, " — outlier |z| ≥ ", out_th, " (etichette = data)"),
      x = NULL,
      y = paste0("Valore (", unit, ")")
    ) + annotate(
      "label",
      x = x_anno, y = y_anno,
      label = kpi_txt,
      hjust = 1, vjust = 1,
      size = 3.6,
      label.size = 0.35,
      label.r = unit(0.15, "lines"),
      fill = "#FFFFFF",
      color = "#111111"
    ) +
  
    coord_flip() + 
    theme_crema_light(14) 
  
  
  p
}



