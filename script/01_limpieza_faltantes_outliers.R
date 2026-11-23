# ==============================================================
# 01 - Faltantes, outliers y limpieza de la base
# Diagnostica faltantes y atípicos, aplica el criterio de limpieza y guarda las bases para el análisis.
# Lee:   raw/country_occupation_panel.parquet, auxiliar/country_map_panel.csv, auxiliar/grupos_ocupacionales.csv
# Guarda: input/panel_analisis.csv, input/paises_analisis.csv, output/tablas/faltantes_por_variable.csv,
#         output/tablas/outliers_tukey_por_grupo.csv, output/tablas/outliers_resumen.csv,
#         output/tablas/limpieza_antes_despues.csv,
#         output/graficos/disp.png (diapositiva 7)
# ==============================================================
library(here)
library(tidyverse)

# Funciones propias y configuración (utils/)
source(here("utils", "coma.R"))
source(here("utils", "configuracion_graficos.R"))
source(here("utils", "theme_slide.R"))
source(here("utils", "guardar_png.R"))
source(here("utils", "cargar_nombres_soc.R"))

# ---- Entradas ----
# raw/: panel país x ocupación tal como se descargó del Atlas (parquet; se lee con el paquete arrow).
# auxiliar/: base de países del Atlas (ingreso, PBI per cápita, población, participaciones).
panel <- arrow::read_parquet(here("raw", "country_occupation_panel.parquet"))
paises_raw <- read_csv(here("auxiliar", "country_map_panel.csv"), show_col_types = FALSE)

# ---- 1. Faltantes: conteo por columna y unicidad de la clave país x ocupación ----
faltantes <- tibble(variable = names(panel), faltantes = colSums(is.na(panel)))
write_csv(faltantes, here("output", "tablas", "faltantes_por_variable.csv"))
n_duplicados <- sum(duplicated(panel[c("iso3", "onet_soc_code")]))
cat(sprintf("Faltantes en el panel: %d | duplicados en iso3 x ocupación: %d | filas: %d | países: %d\n",
            sum(faltantes$faltantes), n_duplicados, nrow(panel), n_distinct(panel$iso3)))
stopifnot(nrow(panel) == 114452, sum(faltantes$faltantes) == 0, n_duplicados == 0, n_distinct(panel$iso3) == 124)
panel <- panel %>% mutate(soc_major = str_sub(onet_soc_code, 1, 2))
soc_nombre <- cargar_nombres_soc("es")

# ---- 2. Outliers: regla de Tukey (1,5 x RIC) dentro de cada grupo ocupacional ----
# Se eligió Tukey porque no supone normalidad y la variable es poco asimétrica y acotada (0-3).
# Los atípicos son valores válidos de la escala (0 a 3): se mantienen (ver decisión abajo).

tukey_grupo <- panel %>% group_by(soc_major) %>%
  mutate(lo = quantile(mean_exposure, .25) - 1.5 * IQR(mean_exposure),
         hi = quantile(mean_exposure, .75) + 1.5 * IQR(mean_exposure)) %>%
  summarise(n = n(), atipicos = sum(mean_exposure < lo | mean_exposure > hi), .groups = "drop") %>%
  mutate(grupo = soc_nombre[soc_major], porcentaje = 100 * atipicos / n)
write_csv(tukey_grupo, here("output", "tablas", "outliers_tukey_por_grupo.csv"))

vars_hcpc <- c("share_exposed", "mean_task_exposure", "sub_only_share",
               "both_share", "aug_only_share", "task_exposure_sd", "share_high_exposure")

# ---- 3. Base de países (124) con variables derivadas ----
paises <- paises_raw %>%
  mutate(log_gdp_pc = log(gdp_per_capita),
         suma_exp = sub_only_share + both_share + aug_only_share,
         sub_rel = sub_only_share / suma_exp, both_rel = both_share / suma_exp,
         aug_rel = aug_only_share / suma_exp, int_rel = share_high_exposure / share_exposed,
         expuestas = share_exposed, reemplaza = sub_only_share + both_share,
         ratio = reemplaza / aug_only_share,
         sin_clasif = income_level == "Not classified")
stopifnot(nrow(paises) == 124, sum(is.na(paises[vars_hcpc])) == 0)


# ---- 4. Decisión de limpieza ----
# Etiopía y Venezuela no tienen clasificación de ingreso: se marcan y quedan fuera de todo análisis que
# use el ingreso (Kruskal-Wallis, chi cuadrado, regresión). ANOVA y HCPC no lo usan: conservan los 124.
# Las 42 observaciones del techo (exposición = 3) son valores válidos del modelo: se mantienen.
iso_sin_clasif <- paises$iso3[paises$sin_clasif]
panel_limpio <- filter(panel, !iso3 %in% iso_sin_clasif)
panel <- panel %>% mutate(con_ingreso = !iso3 %in% iso_sin_clasif)
write_csv(panel, here("input", "panel_analisis.csv"))
write_csv(paises, here("input", "paises_analisis.csv"))

# ---- 5. Descriptivas antes y después de la limpieza; sensibilidad al techo ----
desc <- function(x) tibble(media = mean(x), mediana = median(x), desvio = sd(x),
                           p10 = quantile(x, .1), p90 = quantile(x, .9), razon_p90_p10 = quantile(x, .9) / quantile(x, .1), maximo = max(x))
antes   <- desc(panel$mean_exposure)
despues <- desc(panel_limpio$mean_exposure)
limpieza <- bind_rows(mutate(antes, base = "Antes", filas = nrow(panel), paises = n_distinct(panel$iso3)),
                      mutate(despues, base = "Después", filas = nrow(panel_limpio), paises = n_distinct(panel_limpio$iso3)))
write_csv(limpieza, here("output", "tablas", "limpieza_antes_despues.csv")); print(as.data.frame(limpieza))
q <- quantile(panel$mean_exposure, c(.25, .75)); iqr <- diff(q)
tukey <- panel %>% group_by(soc_major) %>% mutate(lo = quantile(mean_exposure, .25) - 1.5 * IQR(mean_exposure),
                                                   hi = quantile(mean_exposure, .75) + 1.5 * IQR(mean_exposure)) %>%
  filter(mean_exposure < lo | mean_exposure > hi) %>% ungroup()
cat(sprintf("Atípicos Tukey por grupo SOC: %d filas (%.2f%%); techo (=3): %d filas, %d de la misma ocupación\n",
            nrow(tukey), 100 * nrow(tukey) / nrow(panel), sum(panel$mean_exposure == 3),
            max(table(panel$occupation_title[panel$mean_exposure == 3]))))
techo <- desc(panel$mean_exposure[panel$mean_exposure < 3])
cat(sprintf("Sin el techo: media %.4f -> %.4f | filas %d\n", antes$media, techo$media, sum(panel$mean_exposure < 3)))
cat(sprintf("Países sin clasificación: %s\n", paste(iso_sin_clasif, collapse = ", ")))


# ---- 6. Gráfico: dispersión por grupo y atípicos (diapositiva 7) ----
est <- panel %>% group_by(soc_major) %>%
  summarise(q1 = quantile(mean_exposure, .25), med = median(mean_exposure), q3 = quantile(mean_exposure, .75),
            media = mean(mean_exposure), .groups = "drop") %>%
  mutate(iqr = q3 - q1, lo_f = q1 - 1.5 * iqr, hi_f = q3 + 1.5 * iqr, grupo = soc_nombre[soc_major])
lim <- panel %>% left_join(est %>% select(soc_major, lo_f, hi_f), by = "soc_major")
bigotes <- lim %>% filter(mean_exposure >= lo_f, mean_exposure <= hi_f) %>%
  group_by(soc_major) %>% summarise(w_lo = min(mean_exposure), w_hi = max(mean_exposure), .groups = "drop")
atip <- lim %>% filter(mean_exposure < lo_f | mean_exposure > hi_f) %>% left_join(select(est, soc_major, grupo), by = "soc_major")
est <- est %>% left_join(bigotes, by = "soc_major") %>% arrange(media) %>% mutate(grupo = factor(grupo, levels = grupo))
atip$grupo <- factor(atip$grupo, levels = levels(est$grupo))

p <- ggplot(est, aes(y = grupo)) +
  geom_segment(aes(x = w_lo, xend = w_hi, yend = grupo), color = navy, linewidth = 0.5) +
  geom_rect(aes(xmin = q1, xmax = q3, ymin = as.integer(grupo) - 0.33, ymax = as.integer(grupo) + 0.33),
            fill = "#2A3B5C", color = navy, linewidth = 0.5) +
  geom_segment(aes(x = med, xend = med, y = as.integer(grupo) - 0.33, yend = as.integer(grupo) + 0.33),
               color = "white", linewidth = 1.4) +
  geom_point(data = atip, aes(mean_exposure), color = naranja, alpha = 0.55, size = 1.8) +
  geom_point(aes(media), shape = 18, color = naranja, size = 4.2) +
  scale_x_continuous(breaks = 0:3, limits = c(0, 3.05), expand = expansion(mult = c(0.01, 0.02))) +
  labs(x = "Exposición media", y = NULL) +
  theme_slide(t_axtitle = 12, t_axtext = 10.5) + theme(panel.grid.major.y = element_blank())
guardar_png(p, "disp.png", ancho = 8.1, alto = 4.88, k = 1.05,
            titulo = "Dispersión de la exposición dentro de cada grupo ocupacional",
            subtitulo = "Cada observación es una ocupación en un país. Caja: rango intercuartil; línea blanca: mediana; rombo: media; naranja: outliers de Tukey.",
            nota = paste0("Regla de Tukey (1,5 × RIC dentro de cada grupo SOC): ", nrow(atip), " outliers (", coma(100 * nrow(atip) / nrow(panel)),
                          "%), valores válidos del modelo que se mantienen. Las ", sum(panel$mean_exposure == 3),
                          " observaciones en el techo de la escala (3) quedan dentro del rango de su grupo."))

# ---- 7. Resumen de atípicos: Tukey dentro de cada grupo frente a Tukey sobre todo el panel (diapositiva 7) ----
# Dentro de cada grupo SOC (lo que muestra disp.png) se marcan más casos porque cada grupo tiene su propio rango.
# Sobre el panel completo, la regla de Tukey solo marca el techo de la escala (= 3).
q_glob <- quantile(panel$mean_exposure, c(.25, .75))
cota_inf <- q_glob[[1]] - 1.5 * diff(q_glob); cota_sup <- q_glob[[2]] + 1.5 * diff(q_glob)
atip_glob <- panel$mean_exposure < cota_inf | panel$mean_exposure > cota_sup
outliers_resumen <- tibble(
  criterio = c("Tukey (1,5 x RIC) dentro de cada grupo SOC", "Tukey (1,5 x RIC) sobre todo el panel"),
  atipicos = c(nrow(tukey), sum(atip_glob)),
  porcentaje = 100 * c(nrow(tukey), sum(atip_glob)) / nrow(panel),
  en_el_techo = c(sum(tukey$mean_exposure == 3), sum(panel$mean_exposure[atip_glob] == 3)),
  cota_superior_global = c(NA, cota_sup))
write_csv(outliers_resumen, here("output", "tablas", "outliers_resumen.csv")); print(as.data.frame(outliers_resumen))
