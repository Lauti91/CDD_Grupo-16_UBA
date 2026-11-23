# ==============================================================
# nombre_es(): nombre del país en español (tabla en auxiliar/nombres_paises_es.csv)
# ==============================================================
# Sin traducción: nombre original

nombre_es <- function(iso3, original) {
  tabla <- readr::read_csv(here::here("auxiliar", "nombres_paises_es.csv"), show_col_types = FALSE)
  dplyr::coalesce(unname(stats::setNames(tabla$nombre_es, tabla$iso3)[iso3]), original)
}
