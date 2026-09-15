library(tidyverse)   # incluye ggplot2, dplyr, forcats (fct_reorder), tidyr
library(scales)      # para expansion() en scale_x_continuous
install.packages("showtext")
library(showtext)

font_add_google("Montserrat", "montserrat")   # o la fuente que usen en la PPT
showtext_auto()


# --- Paleta de la presentación ---
color_fondo       <- "#0B1E3D"   # azul marino oscuro (fondo)
color_texto       <- "#FFFFFF"   # blanco (títulos, texto principal)
color_texto_sec   <- "#B8C4D9"   # blanco grisáceo (subtítulos, ejes)
color_grilla      <- "#25406B"   # azul marino más claro (grillas, apenas visibles)
color_barra       <- "#FFFFFF"   # blanco hueso (barras estándar)
color_acento      <- "#E8B04B"   # dorado (para resaltar el dato principal de cada gráfico)
color_acento2     <- "#5FB3B3"   # verde azulado (segundo acento, si hace falta)
color_rojo   <- "#E53935"   # rojo para las barras
color_negro  <- "#000000"   # negro para la línea de la media
color_fondo_crema <- "#F5EFF0"   # beige/crema
color_turquesa     <- "#6FC2C2"   # turquesa suave (barras)
color_borde_barra  <- "#FFFFFF"   # blanco, para separar cada barra claramente
color_titulo       <- "#C1830E"   # amarillo quemado / ocre
color_texto_oscuro <- "#3A342A"   # texto general (marrón oscuro, no negro puro)
color_texto_sec    <- "#7A7263"   # subtítulos y ejes, más suave
color_naranja      <- "#E8622C"   # línea de la media
color_grilla_clara <- "#E3D9C4"   # grilla apenas visible sobre el crema
color_fondo_claro   <- "#F7F8FA"   # blanco hueso, casi blanco
color_azul_marino   <- "#0B1E3D"   # el mismo navy de antes, ahora como color de barra
color_borde_barra   <- "#F7F8FA"   # mismo que el fondo, para separar las barras entre sí
color_titulo        <- "#0B1E3D"   # título en el mismo navy
color_texto_sec     <- "#5A6472"   # gris azulado para subtítulo y ejes
color_naranja       <- "#E8622C"   # línea de la media
color_grilla        <- "#DDE2E8"   # grilla apenas visible

library(showtext)   # opcional, si quieren una tipografía distinta a la default
theme_navy_transparente <- function(base_size = 46) {   # subido de 36 a 46
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background   = element_rect(fill = "transparent", color = NA),
      panel.background  = element_rect(fill = "transparent", color = NA),
      panel.grid.major  = element_line(color = color_grilla, linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(color = color_texto, face = "bold",
                                       size = rel(1.1), hjust = 0, margin = margin(b = 12)),
      plot.subtitle     = element_text(color = color_texto_sec, size = rel(0.6),
                                       hjust = 0, margin = margin(b = 26)),
      axis.title        = element_text(color = color_texto_sec, size = rel(0.55)),
      axis.text         = element_text(color = color_texto_sec, size = rel(0.5)),
      axis.ticks        = element_blank(),
      plot.margin       = margin(35, 40, 30, 30)
    )
}

theme_crema <- function(base_size = 46) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background   = element_rect(fill = color_fondo_crema, color = NA),
      panel.background  = element_rect(fill = color_fondo_crema, color = NA),
      panel.grid.major  = element_line(color = color_grilla_clara, linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(color = color_titulo, face = "bold",
                                       size = rel(1.5), hjust = 0, margin = margin(b = 12)),
      plot.subtitle     = element_text(color = color_texto_sec, size = rel(0.6),
                                       hjust = 0, margin = margin(b = 26)),
      axis.title        = element_text(color = color_texto_sec, size = rel(0.55)),
      axis.text         = element_text(color = color_texto_oscuro, size = rel(0.5)),
      axis.ticks        = element_blank(),
      plot.margin       = margin(35, 40, 30, 30)
    )
}

theme_claro_navy <- function(base_size = 60) {   # salto grande, de 46 a 60
  theme_minimal(base_size = base_size) %+replace%
    theme(
      plot.background   = element_rect(fill = color_fondo_claro, color = NA),
      panel.background  = element_rect(fill = color_fondo_claro, color = NA),
      panel.grid.major  = element_line(color = color_grilla, linewidth = 0.5),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(color = color_titulo, face = "bold",
                                       size = rel(1.05), hjust = 0, margin = margin(b = 14)),
      plot.subtitle     = element_text(color = color_texto_sec, size = rel(0.65),
                                       hjust = 0, margin = margin(b = 30)),
      axis.title        = element_text(color = color_texto_sec, size = rel(0.6)),
      axis.text         = element_text(color = color_texto_sec, size = rel(0.55)),
      axis.ticks        = element_blank(),
      plot.margin       = margin(40, 45, 35, 35)
    )
}

#############################################################################
#GRAFICO 1
#############################################################################

media_exp <- mean(occ$mean_exposure)

ggplot(occ, aes(x = mean_exposure)) +
  geom_histogram(bins = 40, fill = color_azul_marino, color = color_borde_barra, linewidth = 0.8) +
  geom_vline(xintercept = media_exp, color = color_naranja, linewidth = 1.6, linetype = "dashed") +
  annotate("text", x = media_exp + 0.15, y = Inf, label = paste0("Media: ", round(media_exp, 2)),
           color = color_naranja, hjust = 0, vjust = 1.5, size = 20, fontface = "bold") +
  labs(
    title = "Distribución de la exposición a la automatización",
    subtitle = "Escala 0 (sin margen) a 3 (exposición extensa) · 114.452 ocupaciones-país",
    x = "Exposición media", y = "Frecuencia"
  ) +
  theme_claro_navy()

ggsave(paste0(ruta_graficos, "hist_exposicion.png"), width = 14, height = 8,
       bg = color_fondo_claro, dpi = 300)
