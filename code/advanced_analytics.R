# ==============================================================================
# ADVANCED ANALYTICS 
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

source("code/ReportUno.R")
source("code/ReportDue.R")

merge <- function(physical_report, agg) {
  
  # 1) join + feature engineering
  base <- physical_report %>%
    transmute(
      player,
      category,
      HID = HID,
      topspeed = topspeed,
      workrate = workrate,
      accdec_mean = (acc_index + dec_index) / 2
    ) %>%
    inner_join(
      agg %>% transmute(
        player,
        category_agg = category,
        touch_per_min = touch_per_min,
        releases_per_min = releases_per_min,
        release_vel_avg = release_vel_avg,
        touches_total = touches_total,
        one_touch = one_touch,
        one_touch_pct = if_else(touches_total > 0, 100 * one_touch / touches_total, NA_real_)
      ),
      by = "player"
    ) %>%
    mutate(
      # se category è presente in entrambi, preferisco quella del physical, altrimenti uso agg
      category = coalesce(category, category_agg)
    ) %>%
    select(-category_agg) %>%
    filter(category != "Goalkeepers") %>%
    filter(player != "p 1")

  base
}

data_both <- merge(df_physicalreport, agg)

# 1) matrice numerica (solo feature)
X <- data_both %>%
  select(-c(player, category)) %>%
  mutate(across(everything(), as.numeric)) %>%
  as.data.frame()

# 2) PCA (centrata + scalata)
pca <- prcomp(X, center = TRUE, scale. = TRUE)

# 3) Scores sulle prime 2 componenti (coordinate dei punti)
scores <- as.data.frame(pca$x[, 1:2]) %>%
  rename(PC1 = -PC1, PC2 = PC2) %>%
  mutate(player = data_both$player)

# 4) k-means sulle coordinate PCA (qui k = 2 come nel tuo esempio)
set.seed(1)
km <- kmeans(scores[, c("PC1", "PC2")], centers = 4, nstart = 50)

scores <- scores %>%
  mutate(cluster = factor(km$cluster,
                          labels = c("Fisici", "Tecnici", 
                                     "Sotto Media", "Sopra Media")))

ggplot(scores, aes(-PC1, PC2, color = cluster, label = player)) +
  geom_point(size = 3) +
  geom_text_repel(size = 3, max.overlaps = Inf) +
  labs(title = "KMeans clusters sulle prime due PC", x = "PC1", y = "PC2",
       color = "Cluster") +
  theme_minimal(base_size = 13) +
  theme(
    plot.background  = element_rect(fill = "#0b0b0b", color = NA),
    panel.background = element_rect(fill = "#0b0b0b", color = NA),
    panel.grid.major = element_line(color = "#222222"),
    panel.grid.minor = element_blank(),
    axis.text.y      = element_text(color = "white", face = "bold"),
    axis.text.x      = element_text(color = "white"),
    plot.title       = element_text(color = "#FF2E2E", face = "bold", size = 16),
    legend.text      = element_text(color = "white", face = "bold"),
    legend.position  = "top"
  )


# 6) loadings 
loadings <- as.data.frame(pca$rotation[, 1:2]) %>%
  tibble::rownames_to_column("variabile")

top_PC1 <- loadings %>%
  mutate(absPC1 = abs(PC1)) %>%
  arrange(desc(absPC1)) %>%
  slice(1:10)

top_PC2 <- loadings %>%
  mutate(absPC2 = abs(PC2)) %>%
  arrange(desc(absPC2)) %>%
  slice(1:10)

top_PC1
top_PC2


var_exp <- (pca$sdev^2) / sum(pca$sdev^2)   # proporzione per ogni PC
perc_exp <- 100 * var_exp                  # in percentuale

perc_exp[1:10] 
