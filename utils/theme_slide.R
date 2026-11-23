# ==============================================================
# theme_slide(): tema ggplot para los gráficos de la presentación
# ==============================================================
# Requiere utils/configuracion_graficos.R

theme_slide <- function(t_axtitle = 18, t_axtext = 16) {
  theme_minimal(base_family = fam) +
    theme(
      plot.background  = element_rect(fill = color_fondo_claro, color = NA),
      panel.background = element_rect(fill = color_fondo_claro, color = NA),
      panel.grid.major = element_line(color = color_grilla, linewidth = 0.5),
      panel.grid.minor = element_blank(),
      axis.title       = element_text(color = color_texto_sec, size = t_axtitle, face = "bold"),
      axis.text        = element_text(color = color_azul_marino, size = t_axtext, face = "bold"),
      axis.ticks       = element_blank(),
      legend.position  = "top",
      legend.text      = element_text(color = color_azul_marino, size = t_axtext),
      legend.title     = element_text(color = color_azul_marino, size = t_axtext, face = "bold"),
      plot.margin      = margin(20, 30, 20, 20))
}
