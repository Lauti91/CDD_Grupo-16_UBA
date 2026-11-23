# ==============================================================
# Configuración común de los gráficos: fuente Rajdhani, paleta y textos fijos
# ==============================================================
# No es una función: define constantes (colores, nombres de niveles). Se carga con
#   source(here::here("utils", "configuracion_graficos.R"))
# La fuente Rajdhani se toma de la computadora o, si no está instalada, de auxiliar/fuentes/

# --------------------------------------------------------------
# Fuente Rajdhani (la de las entregas anteriores)
# --------------------------------------------------------------
# Si Rajdhani no está instalada en la computadora, se registra para esta sesión de R desde
# auxiliar/fuentes/ (licencia SIL Open Font License, en OFL.txt): no hace falta instalar nada y los
# gráficos salen iguales en cualquier máquina. ragg (el dispositivo de guardar_png) usa ese registro.
fam <- "sans"
if (requireNamespace("systemfonts", quietly = TRUE)) {
  instalada  <- "Rajdhani" %in% systemfonts::system_fonts()$family
  registrada <- "Rajdhani" %in% systemfonts::registry_fonts()$family
  dir_fuentes <- here::here("auxiliar", "fuentes")
  if (!instalada && !registrada && file.exists(file.path(dir_fuentes, "Rajdhani-Regular.ttf"))) {
    systemfonts::register_font(name = "Rajdhani",
                               plain = file.path(dir_fuentes, "Rajdhani-Regular.ttf"),
                               bold = file.path(dir_fuentes, "Rajdhani-Bold.ttf"))
    registrada <- TRUE
  }
  if (instalada || registrada) fam <- "Rajdhani"
}

# --------------------------------------------------------------
# Paleta (la de la Instancia 1)
# --------------------------------------------------------------
color_fondo_claro <- "#F7F8FA"
color_azul_marino <- "#0B1E3D"
color_texto_sec   <- "#5A6472"
color_naranja     <- "#E8622C"
color_grilla      <- "#DDE2E8"
color_gris        <- "#B7BDC7"
color_turquesa    <- "#2E8F8F"
navy <- color_azul_marino; naranja <- color_naranja; gris_tx <- color_texto_sec
grilla <- color_grilla; gris_md <- "#9AA3AF"; gris_cl <- "#C9CFD7"; turq <- color_turquesa

col_nivel     <- c("1" = "#A9BCD3", "2" = "#4F78A3", "3" = navy)
col_nivel_s06 <- c("1" = "#8A94A3", "2" = naranja, "3" = navy)
col_tramo     <- c("1" = "#BAC2CC", "2" = naranja, "3" = navy)
nom_nivel <- c("1" = "Tramo bajo", "2" = "Tramo medio", "3" = "Tramo alto")
nom_tipo  <- c("1" = "Baja y sustitutiva", "2" = "Con más aumentación", "3" = "Alta e intensa")
col_tipo  <- c("1" = "#8A94A3", "2" = naranja, "3" = turq)
col_ing <- c("Low income" = "#F4C2A8", "Lower middle income" = "#EE9566",
             "Upper middle income" = "#C2502A", "High income" = "#5E2410")
nom_ing <- c("Low income" = "Bajo", "Lower middle income" = "Medio-bajo",
             "Upper middle income" = "Medio-alto", "High income" = "Alto")
niv_ing <- names(col_ing)

fuente <- "Fuente: elaboración propia en base a Global Automation Atlas (Garg, Crosta y Baier, 2026)."

# --------------------------------------------------------------
# Formato en castellano: coma decimal y punto de miles en los ejes y rótulos
# --------------------------------------------------------------
options(OutDec = ",")
