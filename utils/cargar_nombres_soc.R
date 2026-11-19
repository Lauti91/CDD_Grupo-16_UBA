# ==============================================================
# cargar_nombres_soc(): nombres de los 22 grupos SOC (idioma = "es" o "en")
# ==============================================================

cargar_nombres_soc <- function(idioma = "es") {
  tabla <- readr::read_csv(here::here("auxiliar", "grupos_ocupacionales.csv"),
                           col_types = readr::cols(soc_major = readr::col_character()))
  stats::setNames(tabla[[paste0("nombre_", idioma)]], tabla$soc_major)
}
