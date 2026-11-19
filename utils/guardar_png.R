# ==============================================================
# guardar_png(): exporta con ggsave() a output/graficos/ (PNG, 300 dpi)
# ==============================================================

guardar_png <- function(p, nombre, w = 14, h = 6.7, bg = color_fondo_claro) {
  ruta <- here::here("output", "graficos", nombre)
  dir.create(dirname(ruta), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(ruta, p, width = w, height = h, dpi = 300, bg = bg, device = ragg::agg_png)
  message("Guardado: ", ruta)
  invisible(ruta)
}
