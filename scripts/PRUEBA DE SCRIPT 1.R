# ==============================================================
# EDA - Global Automation Atlas (country x occupation)
# Instancia 1 - TP Ciencia de Datos FCE-UBA
# ==============================================================

# --- 0. Librerías ---
library(arrow)
library(tidyverse)
library(janitor)
library(skimr)

# --- 1. Presentación de la base ---
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

occ <- read_parquet("datos/country_occupation_panel.parquet")

dim(occ)
glimpse(occ)

# --- 2. Descripción de las variables ---

# Nulos por columna
colSums(is.na(occ))

# Resumen general
skim(occ)

# --- 2.1 Chequeo de calidad de datos: shares redundantes ---
# El data dictionary oficial define substitution_share como una
# categoría MÁS AMPLIA que sub_only_share (debería incluir los casos
# "both"). En la práctica, verificamos que son idénticas:
identical(occ$substitution_share, occ$sub_only_share)
identical(occ$augmentation_share, occ$aug_only_share)

# Esto se confirmó también en el repositorio de replicación original
# de los autores (prashgarg/global-automation-atlas): es una
# inconsistencia de la fuente, no un error de nuestra copia.
# Por lo tanto, para el análisis tratamos substitution_share y
# augmentation_share como redundantes y trabajamos con
# sub_only_share / aug_only_share.

# Chequeo de partición: las 4 shares deben sumar 1 por fila
resumen_particion <- occ %>%
  mutate(suma_shares = sub_only_share + both_share + aug_only_share + neither_share) %>%
  summarise(min = min(suma_shares), max = max(suma_shares))
resumen_particion

# --- 2.2 Grupo ocupacional SOC (2 dígitos) ---
# ACLARACIÓN: esto es el "grupo mayor" de la clasificación SOC de
# EEUU (a partir de los primeros 2 dígitos del código O*NET-SOC),
# NO es ISCO-08. El paper original reporta sus figuras a nivel
# ISCO-08 (vía un crosswalk SOC->ISCO del Bureau of Labor Statistics),
# que es distinto de esta agrupación. Para Instancia 1 esta
# categorización SOC es válida y suficiente; si en Instancia 2 se
# mergea con ILOSTAT (que usa ISCO-08), va a hacer falta un
# crosswalk O*NET-SOC -> ISCO-08 (ver Azuara et al. 2024, IDB).
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

# --- 3. Exploración de los datos ---

## 3.1 Estadísticas descriptivas generales
occ %>%
  summarise(across(
    c(mean_exposure, sub_only_share, aug_only_share, both_share,
      neither_share, task_count),
    list(media = mean, mediana = median, sd = sd, min = min, max = max),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(everything())

## 3.2 Distribución de la exposición media
ggplot(occ, aes(x = mean_exposure)) +
  geom_histogram(bins = 40) +
  labs(
    title = "Distribución de la exposición media a la automatización",
    x = "mean_exposure (escala 0-3)", y = "Frecuencia"
  )

## 3.3 Exposición por grupo ocupacional (SOC)
# Con 22 categorías, barras ordenadas se leen mejor que un boxplot
tabla_soc <- occ %>%
  group_by(soc_label) %>%
  summarise(
    exposicion_media = mean(mean_exposure),
    exposicion_mediana = median(mean_exposure),
    n = n()
  ) %>%
  arrange(desc(exposicion_media))

tabla_soc

tabla_soc %>%
  mutate(soc_label = fct_reorder(soc_label, exposicion_media)) %>%
  ggplot(aes(x = exposicion_media, y = soc_label)) +
  geom_col() +
  labs(
    title = "Exposición media a la automatización por grupo ocupacional (SOC)",
    x = "Exposición media", y = NULL
  )

## 3.4 Sustitución vs. aumentación (partición mutuamente excluyente)
# Nota: usamos sub_only_share y aug_only_share (no substitution_share/
# augmentation_share, que son idénticas a estas por el bug de la fuente)
ggplot(occ, aes(x = sub_only_share, y = aug_only_share)) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Relación entre share de sustitución-solo y aumentación-solo",
    x = "sub_only_share", y = "aug_only_share"
  )

cor(occ$sub_only_share, occ$aug_only_share)


## 3.5 Ocupaciones más y menos expuestas
occ %>%
  group_by(occupation_title) %>%
  summarise(mean_exposure = mean(mean_exposure), .groups = "drop") %>%
  slice_max(mean_exposure, n = 10)

occ %>%
  group_by(occupation_title) %>%
  summarise(mean_exposure = mean(mean_exposure), .groups = "drop") %>%
  slice_min(mean_exposure, n = 10)

# --- 4. Primeros hallazgos ---
# 1. La base no tiene valores nulos en ninguna de sus 12 columnas.
# 2. mean_exposure (escala 0-3, NO 0-1) presenta asimetría hacia la
#    derecha (media 1.16 > mediana 1.14).
# 3. sub_only_share y aug_only_share son categorías mutuamente
#    excluyentes (junto con both_share y neither_share suman 1) y
#    tienen correlación negativa moderada (-0.22): la automatización
#    tiende a sustituir O aumentar, no ambas cosas por igual.
# 4. Diferencias marcadas entre grupos SOC: de 2.00 (Office & Admin.
#    Support) a 0.635 (Community & Social Service) -> patrón de
#    tareas rutinarias vs. interpersonales.
# 5. Las columnas substitution_share/augmentation_share son idénticas
#    a sub_only_share/aug_only_share pese a estar documentadas como
#    conceptos distintos -> inconsistencia verificada en la fuente
#    original (repo de replicación de los autores), no en nuestra copia.
#
# Preguntas para próximas etapas:
# - ¿Cómo se relaciona la exposición con los salarios? (requiere
#   ILOSTAT + un crosswalk O*NET-SOC -> ISCO-08, ya que ILOSTAT
#   reporta por ISCO y esta base usa códigos O*NET-SOC)
# - ¿La relación exposición-salario depende del income_level del país?


## 3.6 Correlograma de variables numéricas
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

ggsave("output/correlograma_variables.png", width = 7, height = 6)