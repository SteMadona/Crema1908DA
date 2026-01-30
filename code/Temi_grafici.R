theme_crema_pro <- function(base_size = 12, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      
      # griglia: leggera, “da report”
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(color = "#E9E9E9", linewidth = 0.4),
      panel.grid.major.x = element_line(color = "#F2F2F2", linewidth = 0.3),
      
      # bordo pannello sottile
      panel.border     = element_rect(color = "#E6E6E6", fill = NA, linewidth = 0.6),
      
      # testi
      plot.title    = element_text(face = "bold", size = base_size * 1.25, color = "#111111"),
      plot.subtitle = element_text(size = base_size * 0.95, color = "#444444"),
      plot.caption  = element_text(size = base_size * 0.8, color = "#666666"),
      
      axis.title = element_text(face = "bold", color = "#111111"),
      axis.text  = element_text(color = "#222222"),
      
      # legenda “media style”
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = base_size * 0.9),
      legend.key = element_rect(fill = NA, color = NA),
      
      plot.margin = margin(10, 14, 10, 10)
    )
}
