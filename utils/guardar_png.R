# ==============================================================
# guardar_png(): agrega título, subtítulo, fuente y nota al pie al gráfico y lo exporta
# con ggsave() a output/graficos/ (PNG, 300 dpi)
# ==============================================================
# ancho, alto: tamaño (pulgadas) con que el gráfico queda en la diapositiva.
# k: factor de tamaño del lienzo (el PNG mide ancho*k x alto*k); con k > 1 las letras, que están
#    dimensionadas para gráficos más grandes, quedan en la diapositiva más chicas (1/k) y con k < 1, más grandes.
# Los textos de título, subtítulo y fuente se dimensionan para leerse a 14 / 10,5 / 10 puntos en la diapositiva.
# Requiere haber cargado utils/configuracion_graficos.R (paleta, fuente y `fuente`).

guardar_png <- function(p, nombre, ancho, alto, k = 1, titulo = NULL, subtitulo = NULL, nota = NULL,
                        bg = color_fondo_claro) {
  t_tit <- 14 * k; t_sub <- 10.5 * k; t_not <- 10 * k
  car <- function(tam, f) floor((ancho * k - 0.7 * k) * 72 / (tam * f))   # caracteres por línea (aprox.)
  tit <- stringr::str_wrap(titulo, car(t_tit, 0.46))
  sub <- if (is.null(subtitulo)) NULL else stringr::str_wrap(subtitulo, car(t_sub, 0.42))
  cap <- stringr::str_wrap(paste(fuente, nota), car(t_not, 0.42))

  estilo <- ggplot2::theme(
    plot.title    = ggplot2::element_text(family = fam, face = "bold", size = t_tit, color = color_azul_marino,
                                          lineheight = 1, margin = ggplot2::margin(b = 0.3 * t_tit), hjust = 0),
    plot.subtitle = ggplot2::element_text(family = fam, size = t_sub, color = color_texto_sec,
                                          lineheight = 1.05, margin = ggplot2::margin(b = 0.7 * t_sub), hjust = 0),
    plot.caption  = ggplot2::element_text(family = fam, size = t_not, color = color_texto_sec,
                                          lineheight = 1.08, hjust = 0, margin = ggplot2::margin(t = 0.8 * t_not)),
    plot.title.position = "plot", plot.caption.position = "plot")
  if (inherits(p, "patchwork")) {
    p <- p + patchwork::plot_annotation(title = tit, subtitle = sub, caption = cap, theme = estilo)
  } else {
    p <- p + ggplot2::labs(title = tit, subtitle = sub, caption = cap) + estilo
  }

  ruta <- here::here("output", "graficos", nombre)
  dir.create(dirname(ruta), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(ruta, p, width = ancho * k, height = alto * k, dpi = 300, bg = bg, device = ragg::agg_png)
  message("Guardado: ", ruta)
  invisible(ruta)
}
