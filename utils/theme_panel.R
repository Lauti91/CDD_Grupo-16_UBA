# ==============================================================
# theme_panel(): variante para gráficos de dos paneles (patchwork)
# ==============================================================

theme_panel <- function() {
  theme_slide(t_axtitle = 15, t_axtext = 13) +
    theme(plot.title = element_text(size = 15, face = "bold", color = color_azul_marino,
                                    margin = margin(b = 10)),
          plot.margin = margin(10, 30, 10, 10))
}
