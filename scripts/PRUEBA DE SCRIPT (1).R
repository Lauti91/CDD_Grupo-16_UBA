getwd()

if (!dir.exists("output")) dir.create("output")
if (!dir.exists("output/graficos")) dir.create("output/graficos", recursive = TRUE)
if (!dir.exists("output/tablas")) dir.create("output/tablas", recursive = TRUE)

# ==============================================================
# EDA - Global Automation Atlas (country x occupation)
# Instancia 1 - TP Ciencia de Datos FCE-UBA
# Grupo: [completar integrantes]
# ==============================================================

# --- 0. Librerías ---
library(arrow)
library(tidyverse)
library(janitor)
library(skimr)

# --- 0.1 Rutas del proyecto ---
# No hace falta setwd(): al abrir CDD_Grupo-_UBA.Rproj, RStudio ya
# fija el working directory a la raíz del repo automáticamente,
# sin importar en qué computadora ni usuario se abra (funciona igual
# para los 3 integrantes del grupo).

ruta_datos    <- "datos/"
ruta_scripts  <- "scripts/"
ruta_graficos <- "output/graficos/"
ruta_tablas   <- "output/tablas/"

# --- 1. Presentación de la base ---
#
# Fuente: Global Automation Atlas (Garg, Crosta & Baier, 2026)
# repo: prashgarg/global-automation-atlas | web: automationatlas.org
#
# IMPORTANTE: esta base NO mide automatización observada. Cada tarea
# O*NET x país fue clasificada por un LLM bajo un protocolo de prompt
# fijo, que asigna un exposure_level (0-3) según cuánto de esa tarea
# podría automatizar la tecnología disponible en 2026 en ese país.
# Los autores validan esta medida contra proxies externos (uso real
# de ChatGPT, índices AIOE, IMF AIPI, etc.), con correlaciones de
# 0.41 a 0.90 según el indicador.
#
# Unidad de observación: ocupación O*NET dentro de un país
# Sin dimensión temporal (snapshot, año de referencia 2026)
#
# TEMÁTICA E HIPÓTESIS DE TRABAJO:
# Estudiamos la relación entre exposición a la automatización y
# polarización del mercado laboral. Hipótesis: la relación entre
# exposición a la automatización y salarios tiene forma de U -
# ocupaciones de exposición intermedia muestran salarios relativamente
# más bajos que las de baja y alta exposición.
#
# El paper original NO mide esto: la palabra "polarización" solo
# aparece en su bibliografía (citando a Autor & Dorn, Goos & Manning),
# nunca en su propio análisis. Los autores reconocen explícitamente
# esta limitación: "this measure captures feasible exposure under
# stated country contexts, not realised adoption, employment effects,
# wage effects or productivity changes" -> nuestro aporte llena un
# vacío reconocido por los propios autores, no replica nada existente.
#
# En próximas instancias se incorporará como base COMPLEMENTARIA los
# datos de ingresos por ocupación de ILOSTAT (EAR_4MTH_SEX_OCU_CUR_NB),
# lo cual va a requerir un crosswalk O*NET-SOC -> ISCO-08.

occ <- read_parquet(paste0(ruta_datos, "country_occupation_panel.parquet"))

dim(occ)
glimpse(occ)

# --- 1.1 Unicidad de la unidad de observación ---
# La unidad de observación (país x ocupación) no debería repetirse.
# Verificado: TRUE -> sin duplicados en las 114.452 filas.
n_distinct(paste(occ$iso3, occ$onet_soc_code)) == nrow(occ)

# --- 2. Descripción de las variables ---

# Nulos por columna
colSums(is.na(occ))

# Resumen general
skim(occ)

# --- 2.0 Diccionario de variables (como objeto, no solo texto en la PPT) ---
diccionario <- tribble(
  ~variable,            ~descripcion,                                                          ~tipo,        ~nulos,
  "iso3",               "Código ISO 3166-1 alpha-3 del país",                                  "Categórica", 0,
  "country_name",       "Nombre del país",                                                     "Categórica", 0,
  "onet_soc_code",      "Código de ocupación O*NET-SOC",                                        "Categórica", 0,
  "occupation_title",   "Nombre de la ocupación O*NET",                                         "Categórica", 0,
  "task_count",         "Cantidad de tareas O*NET que componen la ocupación",                   "Numérica",   0,
  "mean_exposure",      "Exposición promedio a la automatización (escala 0-3, generada por LLM)","Numérica",  0,
  "substitution_share", "IDÉNTICA a sub_only_share (inconsistencia verificada en la fuente)",    "Numérica",   0,
  "augmentation_share", "IDÉNTICA a aug_only_share (inconsistencia verificada en la fuente)",    "Numérica",   0,
  "sub_only_share",     "Share de tareas con sustitución únicamente",                            "Numérica",   0,
  "both_share",         "Share de tareas con sustitución y aumentación a la vez",                "Numérica",   0,
  "aug_only_share",     "Share de tareas con aumentación únicamente",                            "Numérica",   0,
  "neither_share",      "Share de tareas sin margen de automatización creíble",                  "Numérica",   0
)
print(diccionario, n = Inf)
write_csv(diccionario, paste0(ruta_tablas, "diccionario_variables.csv"))

# --- 2.1 Chequeo de calidad de datos: shares redundantes ---
# El data dictionary oficial define substitution_share como una
# categoría MÁS AMPLIA que sub_only_share (debería incluir los casos
# "both"). En la práctica, verificamos que son idénticas:
identical(occ$substitution_share, occ$sub_only_share)
identical(occ$augmentation_share, occ$aug_only_share)

# Confirmado también en el repositorio de replicación original de los
# autores (prashgarg/global-automation-atlas): es una inconsistencia
# de la fuente, no un error de nuestra copia. Trabajamos entonces con
# sub_only_share / aug_only_share como referencia.

# Chequeo de partición: las 4 shares deben sumar 1 por fila
resumen_particion <- occ %>%
  mutate(suma_shares = sub_only_share + both_share + aug_only_share + neither_share) %>%
  summarise(min = min(suma_shares), max = max(suma_shares))
resumen_particion

# --- 2.2 Grupo ocupacional SOC (2 dígitos) ---
# ACLARACIÓN: esto es el "grupo mayor" de la clasificación SOC de
# EEUU (a partir de los primeros 2 dígitos del código O*NET-SOC),
# NO es ISCO-08. El paper original reporta sus figuras a nivel
# ISCO-08 (vía un crosswalk SOC->ISCO), distinto de esta agrupación.
# Fuente: U.S. Bureau of Labor Statistics, "2018 Standard Occupational
# Classification System" - https://www.bls.gov/soc/2018/major_groups.htm
# (23 grupos oficiales; el 55-0000 "Military Specific" no aparece en
# O*NET, por eso esta base tiene 22). Verificado contra ocupaciones
# reales de la base: los 22 códigos coinciden con sus etiquetas.
soc_labels <- tibble::tribble(
  ~soc_major_group, ~soc_label,
  "11", "Management",
  "13", "Business & Financial",
  "15", "Computer & Mathematical",
  "17", "Architecture & Engineering",
  "19", "Life, Physical & Social Science",
  "21", "Community & Social Service",
  "23", "Legal",
  "25", "Education",
  "27", "Arts, Design, Media",
  "29", "Healthcare Practitioners",
  "31", "Healthcare Support",
  "33", "Protective Service",
  "35", "Food Preparation & Serving",
  "37", "Building & Grounds Maintenance",
  "39", "Personal Care & Service",
  "41", "Sales",
  "43", "Office & Administrative Support",
  "45", "Farming, Fishing & Forestry",
  "47", "Construction & Extraction",
  "49", "Installation, Maintenance & Repair",
  "51", "Production",
  "53", "Transportation & Material Moving"
)

occ <- occ %>%
  mutate(soc_major_group = str_sub(onet_soc_code, 1, 2)) %>%
  left_join(soc_labels, by = "soc_major_group")

occ %>% count(soc_label, sort = TRUE)

# --- 2.3 Coeficiente de variación por variable ---
# El CV (sd/media) permite comparar dispersión entre variables de
# distinta escala: ¿cuál de nuestras shares es más desigual entre
# ocupaciones, en términos relativos?
cv_tabla <- occ %>%
  summarise(across(
    c(mean_exposure, sub_only_share, both_share, aug_only_share, task_count),
    list(media = mean, sd = sd, cv = ~ sd(.x) / mean(.x)),
    .names = "{.col}__{.fn}"
  )) %>%
  pivot_longer(everything(), names_to = c("variable", "estadistico"), names_sep = "__") %>%
  pivot_wider(names_from = estadistico, values_from = value) %>%
  arrange(desc(cv))

cv_tabla

# --- 3. Exploración de los datos ---

## 3.1 Estadísticas descriptivas generales
tabla_desc_general <- occ %>%
  summarise(across(
    c(mean_exposure, sub_only_share, aug_only_share, both_share,
      neither_share, task_count),
    list(media = mean, mediana = median, sd = sd, min = min, max = max),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(everything())

tabla_desc_general
write_csv(tabla_desc_general, paste0(ruta_tablas, "estadisticas_descriptivas.csv"))

## 3.1b Categorización de mean_exposure (niveles con sentido económico)
occ <- occ %>%
  mutate(nivel_exposicion = cut(
    mean_exposure,
    breaks = c(-Inf, 0.75, 1.5, Inf),
    labels = c("Baja (<0.75)", "Media (0.75-1.5)", "Alta (>1.5)")
  ))

table(occ$nivel_exposicion)

# Tabla cruzada: nivel de exposición por grupo SOC (más legible que
# solo mirar el histograma)
tabla_nivel_soc <- occ %>%
  count(soc_label, nivel_exposicion) %>%
  pivot_wider(names_from = nivel_exposicion, values_from = n, values_fill = 0)

tabla_nivel_soc
write_csv(tabla_nivel_soc, paste0(ruta_tablas, "nivel_exposicion_por_soc.csv"))

## 3.2 Distribución de la exposición media
ggplot(occ, aes(x = mean_exposure)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribución de la exposición media a la automatización",
    x = "mean_exposure (escala 0-3)", y = "Frecuencia"
  )

ggsave(paste0(ruta_graficos, "hist_exposicion.png"), width = 8, height = 5)

## 3.3 Exposición por grupo ocupacional (SOC)
tabla_soc <- occ %>%
  group_by(soc_label) %>%
  summarise(
    exposicion_media = mean(mean_exposure),
    exposicion_mediana = median(mean_exposure),
    n = n()
  ) %>%
  arrange(desc(exposicion_media))

tabla_soc
write_csv(tabla_soc, paste0(ruta_tablas, "exposicion_por_grupo_soc.csv"))

tabla_soc %>%
  mutate(soc_label = fct_reorder(soc_label, exposicion_media)) %>%
  ggplot(aes(x = exposicion_media, y = soc_label)) +
  geom_col() +
  labs(
    title = "Exposición media a la automatización por grupo ocupacional (SOC)",
    x = "Exposición media", y = NULL
  )

ggsave(paste0(ruta_graficos, "exposicion_por_grupo_soc.png"), width = 9, height = 7)

## 3.3b Ratios de percentiles: cuantificar la polarización
# p90/p10 y p75/p25 ponen un número a la desigualdad/polarización,
# en vez de solo describirla con el gráfico de barras.
ratio_percentiles <- function(x) {
  x <- x[!is.na(x)]
  p <- quantile(x, probs = c(0.10, 0.25, 0.50, 0.75, 0.90))
  tibble(
    p10 = p[[1]], p25 = p[[2]], p50 = p[[3]], p75 = p[[4]], p90 = p[[5]],
    ratio_90_10 = p[[5]] / p[[1]],
    ratio_75_25 = p[[4]] / p[[2]]
  )
}

ratio_percentiles(occ$mean_exposure)

# El mismo ratio, calculado dentro de cada grupo SOC (con n >= 8 para
# que el p90/p10 sea confiable)
ratios_por_soc <- occ %>%
  group_by(soc_label) %>%
  filter(n() >= 8) %>%
  group_modify(~ ratio_percentiles(.x$mean_exposure)) %>%
  ungroup() %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  arrange(desc(ratio_90_10))

ratios_por_soc
write_csv(ratios_por_soc, paste0(ruta_tablas, "ratios_percentiles_por_soc.csv"))

## 3.4 Margen dominante por ocupación (sustitución / aumentación / ambas / ninguna)
occ <- occ %>%
  mutate(margen_dominante = case_when(
    sub_only_share >= both_share & sub_only_share >= aug_only_share & sub_only_share >= neither_share ~ "Sustitución sola",
    both_share >= aug_only_share & both_share >= neither_share ~ "Ambas (sust. + aum.)",
    aug_only_share >= neither_share ~ "Aumentación sola",
    TRUE ~ "Ninguna"
  ))

tabla_margen <- occ %>%
  count(margen_dominante, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 1))

tabla_margen
write_csv(tabla_margen, paste0(ruta_tablas, "margen_dominante.csv"))

tabla_margen %>%
  mutate(margen_dominante = fct_reorder(margen_dominante, n)) %>%
  ggplot(aes(x = n, y = margen_dominante)) +
  geom_col(fill = "#2166AC") +
  geom_text(aes(label = paste0(pct, "%")), hjust = -0.1) +
  labs(
    title = "Margen de automatización predominante por ocupación-país",
    x = "Cantidad de observaciones (ocupación x país)", y = NULL
  ) +
  xlim(0, max(tabla_margen$n) * 1.15) +
  theme_minimal()

ggsave(paste0(ruta_graficos, "margen_dominante.png"), width = 8, height = 5)

# ¿El margen dominante varía sistemáticamente por grupo SOC?
tabla_margen_soc <- occ %>%
  count(soc_label, margen_dominante) %>%
  pivot_wider(names_from = margen_dominante, values_from = n, values_fill = 0)

tabla_margen_soc
write_csv(tabla_margen_soc, paste0(ruta_tablas, "margen_dominante_por_soc.csv"))

## 3.5 Ocupaciones más y menos expuestas (outliers formales, regla IQR)
bp_exposicion <- boxplot.stats(occ$mean_exposure)
bp_exposicion$stats   # bigote inf, Q1, mediana, Q3, bigote sup

outliers_exposicion <- occ %>%
  filter(mean_exposure %in% bp_exposicion$out) %>%
  select(country_name, occupation_title, soc_label, mean_exposure) %>%
  arrange(desc(mean_exposure))

nrow(outliers_exposicion)   # cuántas observaciones son atípicas
head(outliers_exposicion, 15)
write_csv(outliers_exposicion, paste0(ruta_tablas, "outliers_exposicion.csv"))

# Top/bottom 10 a nivel ocupación (promediando entre países)
top10_ocupaciones <- occ %>%
  group_by(occupation_title) %>%
  summarise(mean_exposure = mean(mean_exposure), .groups = "drop") %>%
  slice_max(mean_exposure, n = 10)

bottom10_ocupaciones <- occ %>%
  group_by(occupation_title) %>%
  summarise(mean_exposure = mean(mean_exposure), .groups = "drop") %>%
  slice_min(mean_exposure, n = 10)

top10_ocupaciones
bottom10_ocupaciones
write_csv(top10_ocupaciones, paste0(ruta_tablas, "top10_ocupaciones_expuestas.csv"))
write_csv(bottom10_ocupaciones, paste0(ruta_tablas, "bottom10_ocupaciones_expuestas.csv"))

## 3.6 Correlograma de variables numéricas
# Se excluyen substitution_share/augmentation_share (idénticas a
# sub_only_share/aug_only_share) y neither_share (combinación lineal
# exacta de las otras tres, ya que las 4 shares suman 1).
vars_corr <- occ %>%
  select(mean_exposure, task_count, sub_only_share, both_share, aug_only_share)

matriz_cor <- cor(vars_corr)
round(matriz_cor, 2)

matriz_cor_long <- as.data.frame(matriz_cor) %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlacion")

ggplot(matriz_cor_long, aes(x = var1, y = var2, fill = correlacion)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlacion, 2)), size = 3) +
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC",
    midpoint = 0, limits = c(-1, 1)
  ) +
  labs(
    title = "Correlograma - variables numéricas del panel país-ocupación",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(paste0(ruta_graficos, "correlograma_variables.png"), width = 7, height = 6)

# Tabla larga, ordenada por fuerza de correlación (más fácil de leer
# que el heatmap solo)
cor_largo <- as.data.frame(as.table(matriz_cor)) %>%
  as_tibble() %>%
  rename(var1 = Var1, var2 = Var2, r = Freq) %>%
  filter(var1 != var2) %>%
  mutate(par = map2_chr(as.character(var1), as.character(var2),
                        ~ paste(sort(c(.x, .y)), collapse = " - "))) %>%
  distinct(par, .keep_all = TRUE) %>%
  arrange(desc(abs(r))) %>%
  mutate(r = round(r, 2)) %>%
  select(par, r)

cor_largo
write_csv(cor_largo, paste0(ruta_tablas, "correlaciones_ordenadas.csv"))

# Pearson vs. Spearman: nuestras variables son proporciones acotadas
# (0-1), no necesariamente lineales. Si difieren mucho, hay no
# linealidad que el heatmap de Pearson no está mostrando.
matriz_spearman <- cor(vars_corr, method = "spearman")
round(matriz_spearman - matriz_cor, 2)

# --- 4. Primeros hallazgos ---
# 1. La base no tiene valores nulos en ninguna de sus 12 columnas, y
#    la unidad de observación (país x ocupación) es única (114.452
#    combinaciones, sin duplicados).
# 2. mean_exposure (escala 0-3, NO 0-1) presenta asimetría hacia la
#    derecha (media 1.16 > mediana 1.14).
# 3. sub_only_share y aug_only_share son categorías mutuamente
#    excluyentes (junto con both_share y neither_share suman 1) y
#    tienen correlación negativa moderada (-0.22).
# 4. Diferencias marcadas entre grupos SOC: de 2.00 (Office & Admin.
#    Support) a 0.635 (Community & Social Service). El ratio p90/p10
#    de mean_exposure cuantifica esta polarización con un número
#    concreto [completar tras correr ratio_percentiles(occ$mean_exposure)].
# 5. substitution_share/augmentation_share son idénticas a
#    sub_only_share/aug_only_share pese a estar documentadas como
#    conceptos distintos -> inconsistencia verificada en la fuente
#    original (repo de replicación de los autores).
# 6. mean_exposure correlaciona fuerte con sub_only_share y both_share
#    (r=0.72 en ambos), pero mucho más débil con aug_only_share
#    (r=0.19). task_count no muestra asociación relevante.
# 7. El propio paper reconoce explícitamente que su medida "no capta
#    efectos salariales" -> nuestra hipótesis (relación en U entre
#    exposición y salarios) llena un vacío reconocido por los autores,
#    no replica ningún análisis existente.
#
# Preguntas para próximas etapas:
# - ¿Cómo se relaciona la exposición con los salarios? (requiere
#   ILOSTAT + reclasificación a ISCO-08, ya que ILOSTAT reporta por
#   ISCO y esta base usa códigos O*NET-SOC)
# - ¿La relación exposición-salario depende del income_level del país?