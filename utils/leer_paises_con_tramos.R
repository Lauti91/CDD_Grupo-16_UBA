# ==============================================================
# leer_paises_con_tramos(): base de países + tramo del HCPC (scripts 05 a 08)
# ==============================================================

leer_paises_con_tramos <- function() {
  niv_ing <- c("Low income", "Lower middle income", "Upper middle income", "High income")
  readr::read_csv(here::here("input", "paises_analisis.csv"), show_col_types = FALSE) |>
    dplyr::left_join(readr::read_csv(here::here("input", "paises_tramos.csv"), show_col_types = FALSE) |>
                       dplyr::select(iso3, tramo), by = "iso3") |>
    dplyr::mutate(tramo = factor(tramo, levels = 1:3),
                  income_level = factor(income_level, levels = niv_ing))   # "Not classified" pasa a NA
}
