install.packages(c("ggchicklet", "showtext", "ggfx", "ggtext"))
# ggchicklet no siempre está en CRAN; si falla, instalar desde GitHub:
install.packages("remotes")
remotes::install_github("hrbrmstr/ggchicklet")


library(ggchicklet)
library(showtext)
library(ggfx)
library(ggtext)

font_add_google("Rajdhani", "rajdhani")
showtext_auto()
showtext_opts(dpi = 300)

color_fondo_claro <- "#F7F8FA"
color_azul_marino <- "#0B1E3D"
color_texto_sec   <- "#5A6472"
color_naranja     <- "#E8622C"
color_grilla      <- "#DDE2E8"

media_exp <- mean(occ$mean_exposure)

# Para geom_chicklet_histogram no existe directo -> armamos el histograma
# como conteo por bin y lo graficamos con geom_chicklet (barras)
bins_hist <- occ %>%
  mutate(bin = cut(mean_exposure, breaks = seq(0, 3, length.out = 41), include.lowest = TRUE)) %>%
  count(bin) %>%
  mutate(bin_num = as.numeric(bin))

ggplot(bins_hist, aes(x = bin_num, y = n)) +
  geom_chicklet(fill = color_azul_marino, radius = grid::unit(3, "pt"), width = 0.85) +
  with_outer_glow(
    geom_vline(xintercept = which.min(abs(seq(0,3,length.out=40) - media_exp)),
               color = color_naranja, linewidth = 1.4, linetype = "dashed"),
    colour = color_naranja, sigma = 8
  ) +
  labs(
    title = paste0("Distribución de la exposición · media <span style='color:", color_naranja, "'>**", round(media_exp,2), "**</span>"),
    subtitle = "Escala 0 (sin margen) a 3 (exposición extensa) · 114.452 ocupaciones-país",
    x = "Exposición media", y = "Frecuencia"
  ) +
  theme_minimal(base_size = 60, base_family = "rajdhani") +
  theme(
    plot.background  = element_rect(fill = color_fondo_claro, color = NA),
    panel.background = element_rect(fill = color_fondo_claro, color = NA),
    panel.grid.major = element_line(color = color_grilla, linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.title       = element_markdown(color = color_azul_marino, face = "bold", size = rel(1.05), hjust = 0),
    plot.subtitle    = element_text(color = color_texto_sec, size = rel(0.65), margin = margin(b = 30)),
    axis.title       = element_text(color = color_texto_sec, size = rel(0.6)),
    axis.text        = element_text(color = color_texto_sec, size = rel(0.55)),
    axis.ticks       = element_blank(),
    plot.margin      = margin(40, 45, 35, 35)
  )

ggsave(paste0(ruta_graficos, "hist_exposicion.png"), width = 14, height = 8,
       bg = color_fondo_claro, dpi = 300)









####################
# PALETA DE COLORES

library(tidyverse)
library(showtext)

font_add_google("Rajdhani", "rajdhani")
showtext_auto()
showtext_opts(dpi = 300)

color_fondo_claro <- "#F7F8FA"
color_azul_marino <- "#0B1E3D"
color_texto_sec   <- "#5A6472"
color_naranja     <- "#E8622C"
color_grilla      <- "#DDE2E8"

theme_claro_navy <- function() {
  theme_minimal(base_family = "rajdhani") +
    theme(
      plot.background  = element_rect(fill = color_fondo_claro, color = NA),
      panel.background = element_rect(fill = color_fondo_claro, color = NA),
      panel.grid.major = element_line(color = color_grilla, linewidth = 0.5),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(color = color_azul_marino, face = "bold", size = 34, hjust = 0),
      plot.subtitle    = element_text(color = color_texto_sec, size = 20, margin = margin(b = 25)),
      axis.title       = element_text(color = color_texto_sec, size = 18, face = "bold"),
      axis.text        = element_text(color = color_azul_marino, size = 16, face = "bold"),
      axis.ticks       = element_blank(),
      plot.margin      = margin(35, 40, 30, 30)
    )
}




library(showtext)
font_add_google("Rajdhani", "rajdhani")
showtext_auto()
showtext_opts(dpi = 300)

color_fondo_claro <- "#F7F8FA"
color_azul_marino <- "#0B1E3D"
color_texto_sec   <- "#5A6472"
color_naranja     <- "#E8622C"
color_grilla      <- "#DDE2E8"








#############
#GRAFICO 1


media_exp <- mean(occ$mean_exposure)

ggplot(occ, aes(x = mean_exposure)) +
  geom_histogram(bins = 40, fill = color_azul_marino, color = color_fondo_claro, linewidth = 0.8) +
  geom_vline(xintercept = media_exp, color = color_naranja, linewidth = 1.6, linetype = "dashed") +
  annotate("text", x = media_exp + 0.15, y = Inf,
           label = paste0("Media: ", round(media_exp, 2)),
           color = color_naranja, hjust = 0, vjust = 1.5, size = 10, fontface = "bold",
           family = "rajdhani") +
  labs(
    title = "Distribución de la exposición a la automatización",
    subtitle = "Escala 0 (sin exposición) a 3 (exposición extensa) · 114.452 ocupaciones-país",
    x = "Exposición media", y = "Frecuencia"
  ) +
  theme_minimal(base_family = "rajdhani") +
  theme(
    plot.background  = element_rect(fill = color_fondo_claro, color = NA),
    panel.background = element_rect(fill = color_fondo_claro, color = NA),
    panel.grid.major = element_line(color = color_grilla, linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.title       = element_text(color = color_azul_marino, face = "bold", size = 34, hjust = 0),
    plot.subtitle    = element_text(color = color_texto_sec, size = 20, margin = margin(b = 25)),
    axis.title       = element_text(color = color_texto_sec, size = 18),
    axis.text        = element_text(color = color_texto_sec, size = 16),
    axis.ticks       = element_blank(),
    plot.margin      = margin(35, 40, 30, 30)
  )

ggsave(paste0(ruta_graficos, "hist_exposicion.png"), width = 14, height = 8,
       bg = color_fondo_claro, dpi = 300)


#######################
#GRAFICO 2

tabla_soc %>%
  mutate(
    soc_label = fct_reorder(soc_label, exposicion_media),
    destacar = case_when(
      exposicion_media == max(exposicion_media) ~ "top",
      exposicion_media == min(exposicion_media) ~ "bottom",
      TRUE ~ "resto"
    )
  ) %>%
  ggplot(aes(x = exposicion_media, y = soc_label, fill = destacar)) +
  geom_col(width = 0.7, color = color_fondo_claro, linewidth = 0.8) +
  geom_text(aes(label = round(exposicion_media, 2)),
            hjust = -0.2, color = color_texto_sec, size = 6, family = "rajdhani", fontface = "bold") +
  scale_fill_manual(values = c(top = color_azul_marino, bottom = color_azul_marino, resto = color_azul_marino),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Exposición por grupo ocupacional",
    subtitle = "Exposición media a la automatización, 22 categorías SOC",
    x = "Exposición media", y = NULL
  ) +
  theme_claro_navy() + 
  theme(
    axis.text.x = element_text(face = "bold", color = color_azul_marino, size = 16) 
    ) +
  theme(
    axis.text.y = element_text(face = "bold", color = color_azul_marino, size = 16) 
  )
  
  

ggsave(paste0(ruta_graficos, "exposicion_por_grupo_soc.png"), width = 14, height = 10,
       bg = color_fondo_claro, dpi = 300)



#######################
#GRAFICO 4

matriz_cor_long <- matriz_cor_long %>%
  mutate(color_texto_celda = if_else(abs(correlacion) > 0.45, color_fondo_claro, color_azul_marino))

ggplot(matriz_cor_long, aes(x = var1, y = var2, fill = correlacion)) +
  geom_tile(color = color_fondo_claro, linewidth = 1.5) +
  geom_text(aes(label = round(correlacion, 2), color = color_texto_celda),
            size = 9, family = "rajdhani", fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradient2(
    low = "#8FA3BF", mid = "#DDE2E8", high = color_azul_marino,
    midpoint = 0, limits = c(-1, 1), name = "Correlación"
  ) +
  labs(
    title = "Correlograma de variables numéricas",
    subtitle = "Panel país-ocupación",
    x = NULL, y = NULL
  ) +
  theme_claro_navy() +
  theme(
    axis.text.x   = element_text(angle = 45, hjust = 1, size = 15),
    legend.text   = element_text(color = color_texto_sec, size = 14),
    legend.title  = element_text(color = color_azul_marino, size = 16)
  )

ggsave(paste0(ruta_graficos, "correlograma_variables.png"), width = 12, height = 9,
       bg = color_fondo_claro, dpi = 300)


################
#GRAFICO 5

occ %>%
  mutate(soc_label = fct_reorder(soc_label, mean_exposure, .fun = median)) %>%
  ggplot(aes(x = mean_exposure, y = soc_label)) +
  geom_boxplot(
    fill = color_azul_marino,
    color = color_azul_marino,
    outlier.color = color_naranja,
    outlier.size = 1.8,
    outlier.alpha = 0.6,
    width = 0.6,
    linewidth = 0.6,
    alpha = 0.85
  ) +
  stat_summary(
    fun = median, fun.min = median, fun.max = median,
    geom = "crossbar", width = 0.6,
    color = "white", linewidth = 0.7
  ) +
  stat_summary(
    fun = mean, geom = "point",
    shape = 18, size = 4.5, color = color_naranja
  ) +
  labs(
    title = "Dispersión de la exposición por grupo ocupacional",
    subtitle = "Caja = mediana y cuartiles · ◆ naranja = media de cada grupo",
    x = "Exposición media", y = NULL
  ) +
  theme_claro_navy() +
  theme(
    axis.text.y = element_text(face = "bold", color = color_azul_marino, size = 16)
  )

ggsave(paste0(ruta_graficos, "boxplot_exposicion_soc.png"), width = 14, height = 10,
       bg = color_fondo_claro, dpi = 300)





#########
#GRAFICO 1.1

media_exp <- mean(occ$mean_exposure)

ggplot(occ, aes(x = mean_exposure)) +
  geom_histogram(bins = 40, fill = color_azul_marino, color = color_fondo_claro, linewidth = 0.8) +
  geom_vline(xintercept = media_exp, color = color_naranja, linewidth = 1.6, linetype = "dashed") +
  annotate("text", x = media_exp + 0.15, y = Inf,
           label = paste0("Media: ", round(media_exp, 2)),
           color = color_naranja, hjust = 0, vjust = 1.5, size = 10, fontface = "bold",
           family = "rajdhani") +
  labs(
    title = "Distribución de la exposición a la automatización",
    subtitle = "Escala 0 (sin exposición) a 3 (exposición extensa) · 114.452 ocupaciones-país",
    x = "Exposición media", y = "Frecuencia"
  ) +
  theme_claro_navy()

ggsave(paste0(ruta_graficos, "hist_exposicion.png"), width = 14, height = 8,
       bg = color_fondo_claro, dpi = 300)

#########
#GRAFICO 2.1

tabla_soc %>%
  mutate(
    soc_label = fct_reorder(soc_label, exposicion_media),
    destacar = case_when(
      exposicion_media == max(exposicion_media) ~ "top",
      exposicion_media == min(exposicion_media) ~ "bottom",
      TRUE ~ "resto"
    )
  ) %>%
  ggplot(aes(x = exposicion_media, y = soc_label, fill = destacar)) +
  geom_col(width = 0.7, color = color_fondo_claro, linewidth = 0.8) +
  geom_text(aes(label = round(exposicion_media, 2)),
            hjust = -0.2, color = color_texto_sec, size = 6, family = "rajdhani", fontface = "bold") +
  scale_fill_manual(values = c(top = color_azul_marino, bottom = color_azul_marino, resto = color_azul_marino),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "Exposición por grupo ocupacional",
    subtitle = "Exposición media a la automatización, 22 categorías SOC",
    x = "Exposición media", y = NULL
  ) +
  theme_claro_navy()

ggsave(paste0(ruta_graficos, "exposicion_por_grupo_soc.png"), width = 14, height = 10,
       bg = color_fondo_claro, dpi = 300)

#########
#GRAFICO 3.1

matriz_cor_long <- matriz_cor_long %>%
  mutate(color_texto_celda = if_else(abs(correlacion) > 0.45, color_fondo_claro, color_azul_marino))

ggplot(matriz_cor_long, aes(x = var1, y = var2, fill = correlacion)) +
  geom_tile(color = color_fondo_claro, linewidth = 1.5) +
  geom_text(aes(label = round(correlacion, 2), color = color_texto_celda),
            size = 9, family = "rajdhani", fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradient2(
    low = "#8FA3BF", mid = "#DDE2E8", high = color_azul_marino,
    midpoint = 0, limits = c(-1, 1), name = "Correlación"
  ) +
  labs(
    title = "Correlograma de variables numéricas",
    subtitle = "Panel país-ocupación",
    x = NULL, y = NULL
  ) +
  theme_claro_navy() +
  theme(
    axis.text.x  = element_text(angle = 45, hjust = 1, size = 15, face = "bold", color = color_azul_marino),
    legend.text  = element_text(color = color_texto_sec, size = 14),
    legend.title = element_text(color = color_azul_marino, size = 16, face = "bold")
  )

ggsave(paste0(ruta_graficos, "correlograma_variables.png"), width = 12, height = 9,
       bg = color_fondo_claro, dpi = 300)

#########
#GRAFICO 4.1

occ %>%
  mutate(soc_label = fct_reorder(soc_label, mean_exposure, .fun = median)) %>%
  ggplot(aes(x = mean_exposure, y = soc_label)) +
  geom_boxplot(
    fill = color_azul_marino,
    color = color_azul_marino,
    outlier.color = color_naranja,
    outlier.size = 1.8,
    outlier.alpha = 0.6,
    width = 0.6,
    linewidth = 0.6,
    alpha = 0.85
  ) +
  stat_summary(
    fun = median, fun.min = median, fun.max = median,
    geom = "crossbar", width = 0.6,
    color = "white", linewidth = 0.7
  ) +
  stat_summary(
    fun = mean, geom = "point",
    shape = 18, size = 4.5, color = color_naranja
  ) +
  labs(
    title = "Dispersión de la exposición por grupo ocupacional",
    subtitle = "Caja = mediana y cuartiles · ◆ naranja = media de cada grupo",
    x = "Exposición media", y = NULL
  ) +
  theme_claro_navy()

ggsave(paste0(ruta_graficos, "boxplot_exposicion_soc.png"), width = 14, height = 10,
       bg = color_fondo_claro, dpi = 300)
