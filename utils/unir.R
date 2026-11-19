# ==============================================================
# unir(): panel unido (patchwork) con el fondo de la diapositiva
# ==============================================================

unir <- function(p) p + patchwork::plot_annotation(theme = ggplot2::theme(
  plot.background = ggplot2::element_rect(fill = color_fondo_claro, color = NA),
  plot.margin = ggplot2::margin(20, 25, 15, 15)))
