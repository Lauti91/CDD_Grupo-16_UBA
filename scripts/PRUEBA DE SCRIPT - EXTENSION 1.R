# ==============================================================
# EXPANSIÓN - Desagregación SOC + heterogeneidad país/ocupación
# Instancia 1 (opcional/bonus) - TP Ciencia de Datos FCE-UBA
#
# DEPENDE del Script 1: antes de correr esto, tené corridas al menos
# las secciones 0 (librerías + rutas), 1 (carga de `occ`), 2.2
# (creación de `soc_label`), 3.3b (función `ratio_percentiles`) y
# 3.4 (columna `margen_dominante`) del script 01_eda_occupation_panel.R
# ==============================================================

library(readxl)

# --- 1. Crosswalk completo SOC 2018 (grupo menor + ocupación amplia) ---
# Fuente: U.S. Bureau of Labor Statistics, "2018 SOC Structure"
# https://www.bls.gov/soc/2018/soc_structure_2018.xlsx
# BLS devuelve 403 Forbidden a descargas automatizadas (incluso con
# User-Agent seteado), así que el archivo se descargó manualmente y
# se subió a datos/ - ya está versionado en el repo.

destino_soc <- paste0(ruta_datos, "soc_structure_2018.xlsx")

excel_sheets(destino_soc)   # "2018 Structure"

# El header real está en la fila 8 (columnas: Major/Minor/Broad/Detailed
# Group + título en la columna E) - confirmado visualmente contra el
# archivo. Se salta hasta ahí con skip = 8.
soc_estructura_raw <- read_excel(destino_soc, skip = 8,
                                 col_names = c("major", "minor", "broad", "detailed", "titulo"))

soc_estructura_raw <- soc_estructura_raw %>%
  filter(!(is.na(major) & is.na(minor) & is.na(broad) & is.na(detailed)))

dim(soc_estructura_raw)   # 1447 = 23 + 98 + 459 + 867

# Clasificar cada fila según en qué columna está el código (ANTES de
# rellenar hacia abajo)
soc_nivel <- soc_estructura_raw %>%
  mutate(nivel = case_when(
    !is.na(detailed) ~ "detailed",
    !is.na(broad)    ~ "broad",
    !is.na(minor)    ~ "minor",
    !is.na(major)    ~ "major"
  ))

table(soc_nivel$nivel)   # 23 major / 98 minor / 459 broad / 867 detailed

# Completar la jerarquía hacia abajo (fill), para que cada fila sepa
# a qué major/minor/broad pertenece
soc_completo <- soc_nivel %>%
  fill(major, minor, broad, .direction = "down")

# --- 2. Armar los crosswalks de nombres, por nivel ---
crosswalk_minor <- soc_completo %>%
  filter(nivel == "minor") %>%
  transmute(soc_minor_group = minor, minor_label = titulo)

crosswalk_broad <- soc_completo %>%
  filter(nivel == "broad") %>%
  transmute(soc_broad_group = broad, broad_label = titulo)

nrow(crosswalk_minor)   # 98
nrow(crosswalk_broad)   # 459

write_csv(crosswalk_minor, paste0(ruta_datos, "crosswalk_soc_minor.csv"))
write_csv(crosswalk_broad, paste0(ruta_datos, "crosswalk_soc_broad.csv"))

# Tabla puente: ocupación detallada (867) -> minor/broad a la que pertenece
soc_detallada_completa <- soc_completo %>%
  filter(nivel == "detailed") %>%
  select(soc_detailed = detailed, soc_minor_group = minor, soc_broad_group = broad)

nrow(soc_detallada_completa)   # 867

# --- 3. Unir todo a `occ` ---
# El onet_soc_code tiene formato XX-YYYY.ZZ; los primeros 7 caracteres
# (antes del punto) son el código SOC detallado "oficial" de BLS.
# Varios códigos O*NET (con distinto sufijo .ZZ) comparten el mismo
# código SOC detallado -> por eso 923 O*NET vs 867 SOC.
occ <- occ %>%
  mutate(soc_detailed = str_extract(onet_soc_code, "^\\d{2}-\\d{4}")) %>%
  left_join(soc_detallada_completa, by = "soc_detailed") %>%
  left_join(crosswalk_minor, by = "soc_minor_group") %>%
  left_join(crosswalk_broad, by = "soc_broad_group")

# Chequeo: no debería haber NAs en las columnas nuevas
sum(is.na(occ$minor_label))   # 0
sum(is.na(occ$broad_label))   # 0

n_distinct(occ$minor_label)   # 95 (de 98 posibles)
n_distinct(occ$broad_label)   # 439 (de 459 posibles)

# --- 4. Agregación intermedia oficial de BLS (13 grupos) ---
# Fuente: BLS, "Standard Occupational Classification and Coding
# Structure, 2018 SOC", Tabla 5 (agregación intermedia a 13 grupos)
# https://www.bls.gov/soc/2018/soc_2018_class_and_coding_structure.pdf
occ <- occ %>%
  mutate(soc_intermedio = case_when(
    soc_major_group %in% c("11","13") ~ "Management, Business, and Financial",
    soc_major_group %in% c("15","17","19") ~ "Computer, Engineering, and Science",
    soc_major_group %in% c("21","23","25","27") ~ "Education, Legal, Community Service, Arts, and Media",
    soc_major_group == "29" ~ "Healthcare Practitioners and Technical",
    soc_major_group %in% c("31","33","35","37","39") ~ "Service Occupations",
    soc_major_group == "41" ~ "Sales and Related",
    soc_major_group == "43" ~ "Office and Administrative Support",
    soc_major_group == "45" ~ "Farming, Fishing, and Forestry",
    soc_major_group == "47" ~ "Construction and Extraction",
    soc_major_group == "49" ~ "Installation, Maintenance, and Repair",
    soc_major_group == "51" ~ "Production",
    soc_major_group == "53" ~ "Transportation and Material Moving",
    TRUE ~ NA_character_
  ))

n_distinct(occ$soc_intermedio)   # 12 (Military no aparece en O*NET)

# --- 5. Funciones genéricas de análisis, parametrizadas por nivel ---

stats_por_grupo <- function(data, var_grupo) {
  data %>%
    group_by(grupo = .data[[var_grupo]]) %>%
    summarise(
      media = mean(mean_exposure), mediana = median(mean_exposure),
      sd = sd(mean_exposure), min = min(mean_exposure), max = max(mean_exposure),
      n = n(), .groups = "drop"
    ) %>%
    arrange(desc(media))
}

ratios_por_grupo <- function(data, var_grupo, min_n = 8) {
  data %>%
    group_by(grupo = .data[[var_grupo]]) %>%
    filter(n() >= min_n) %>%
    group_modify(~ ratio_percentiles(.x$mean_exposure)) %>%
    ungroup() %>%
    mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
    arrange(desc(ratio_90_10))
}

margen_por_grupo <- function(data, var_grupo) {
  data %>%
    count(grupo = .data[[var_grupo]], margen_dominante) %>%
    pivot_wider(names_from = margen_dominante, values_from = n, values_fill = 0)
}

grafico_barras_grupo <- function(tabla, titulo, top_n = NULL) {
  datos_grafico <- tabla
  if (!is.null(top_n)) {
    datos_grafico <- bind_rows(
      tabla %>% slice_max(media, n = top_n),
      tabla %>% slice_min(media, n = top_n)
    )
  }
  datos_grafico %>%
    mutate(grupo = fct_reorder(grupo, media)) %>%
    ggplot(aes(x = media, y = grupo)) +
    geom_col() +
    labs(title = titulo, x = "Exposición media", y = NULL) +
    theme_minimal()
}

# --- 6. Aplicar a los 3 niveles ---
stats_13 <- stats_por_grupo(occ, "soc_intermedio")
stats_minor <- stats_por_grupo(occ, "minor_label")
stats_broad <- stats_por_grupo(occ, "broad_label")

ratios_13 <- ratios_por_grupo(occ, "soc_intermedio")
ratios_minor <- ratios_por_grupo(occ, "minor_label")
ratios_broad <- ratios_por_grupo(occ, "broad_label")

margen_13 <- margen_por_grupo(occ, "soc_intermedio")
margen_minor <- margen_por_grupo(occ, "minor_label")
margen_broad <- margen_por_grupo(occ, "broad_label")

write_csv(stats_13, paste0(ruta_tablas, "stats_13grupos.csv"))
write_csv(stats_minor, paste0(ruta_tablas, "stats_minor.csv"))
write_csv(stats_broad, paste0(ruta_tablas, "stats_broad.csv"))
write_csv(ratios_13, paste0(ruta_tablas, "ratios_13grupos.csv"))
write_csv(ratios_minor, paste0(ruta_tablas, "ratios_minor.csv"))
write_csv(ratios_broad, paste0(ruta_tablas, "ratios_broad.csv"))

# Gráficos: 13 y minor se muestran completos; broad solo top/bottom 20
# (439 barras es ilegible en cualquier tamaño)
grafico_barras_grupo(stats_13, "Exposición media - agregación intermedia BLS (13 grupos)")
ggsave(paste0(ruta_graficos, "exposicion_13grupos.png"), width = 9, height = 6)

grafico_barras_grupo(stats_minor, "Exposición media por grupo menor SOC (minor group)")
ggsave(paste0(ruta_graficos, "exposicion_minor.png"), width = 11, height = 14, limitsize = FALSE)

grafico_barras_grupo(stats_broad, "Exposición media - top/bottom 20 ocupaciones amplias (broad)", top_n = 20)
ggsave(paste0(ruta_graficos, "exposicion_broad_top_bottom.png"), width = 9, height = 10)

# --- 7. Heterogeneidad interna de cada grupo mayor (22) ---
# ¿Qué tan parecidos son los minor groups DENTRO de cada grupo mayor?
heterogeneidad_por_major <- occ %>%
  group_by(soc_label, minor_label) %>%
  summarise(exp_minor = mean(mean_exposure), .groups = "drop") %>%
  group_by(soc_label) %>%
  summarise(
    n_subgrupos = n(),
    exp_promedio = mean(exp_minor),
    rango_interno = max(exp_minor) - min(exp_minor),
    cv_interno = sd(exp_minor) / mean(exp_minor),
    .groups = "drop"
  ) %>%
  arrange(desc(cv_interno))

# Con solo 2 subgrupos el CV no es una medida confiable de dispersión
heterogeneidad_por_major %>%
  filter(n_subgrupos >= 3) %>%
  arrange(desc(cv_interno))

write_csv(heterogeneidad_por_major, paste0(ruta_tablas, "heterogeneidad_por_grupo_mayor.csv"))

# --- 8. Descomposición de varianza (ANOVA = regresión con dummies) ---
# El % de varianza explicada (R², a veces llamado eta² en el contexto
# de ANOVA) que aporta cada forma de agrupar mean_exposure.

anova_soc <- aov(mean_exposure ~ soc_label, data = occ)
r2_soc <- summary.lm(anova_soc)$r.squared
r2_soc   # ~0.25 (22 grupos)

anova_minor <- aov(mean_exposure ~ minor_label, data = occ)
r2_minor <- summary.lm(anova_minor)$r.squared
r2_minor            # ~0.33 (95 grupos)
r2_minor - r2_soc   # cuánto suma bajar de 22 a 95 categorías (~0.07)

anova_pais <- aov(mean_exposure ~ iso3, data = occ)
r2_pais <- summary.lm(anova_pais)$r.squared
r2_pais   # ~0.44 -> el país explica MÁS que la ocupación

anova_pais_sub <- aov(sub_only_share ~ iso3, data = occ)
summary.lm(anova_pais_sub)$r.squared   # ~0.10

anova_pais_aug <- aov(aug_only_share ~ iso3, data = occ)
summary.lm(anova_pais_aug)$r.squared   # ~0.14

# --- 8b. Modelo combinado país + ocupación ---
# ADVERTENCIA DE MEMORIA: usar minor_label (95), NO onet_soc_code (923)
# ni broad_label (439) - con 923 categorías la matriz de diseño pesa
# ~1GB y cuelga la sesión (probado). Con minor_label (95) es manejable
# pero igual quedó cerca del límite de memoria de Posit Cloud gratuito;
# en RStudio nativo debería ir más holgado.

gc()   # liberar memoria antes de correr el modelo pesado

anova_pais_minor <- aov(mean_exposure ~ iso3 + minor_label, data = occ)
r2_pais_minor <- summary.lm(anova_pais_minor)$r.squared
r2_pais_minor   # ~0.77 -> casi aditivo: 0.44 (país) + 0.33 (minor)

rm(anova_pais_minor)   # liberar la memoria del modelo apenas se usa el número
gc()

# --- 9. Bubble chart de sub-categorías (minor group) ---
# ACLARACIÓN IMPORTANTE: el tamaño de cada burbuja (n) es la cantidad
# de FILAS del panel (combinaciones país x ocupación) que caen en esa
# categoría - NO es empleo real ni cantidad de trabajadores. Una
# categoría con muchos códigos O*NET definidos sale "grande" en este
# gráfico aunque emplee poca gente en la realidad, y viceversa.
occ %>%
  group_by(soc_label, minor_label) %>%
  summarise(exp_media = mean(mean_exposure), n = n(), .groups = "drop") %>%
  ggplot(aes(x = exp_media, y = n, size = n, color = soc_label)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Exposición media por sub-categoría ocupacional (minor group)",
    subtitle = "El tamaño refleja cantidad de combinaciones país-ocupación en la base, no empleo real",
    x = "Exposición media", y = "Cantidad de observaciones (país x ocupación)",
    color = "Grupo mayor SOC"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

ggsave(paste0(ruta_graficos, "bubble_minor.png"), width = 10, height = 7)



gc()
anova_pais_ocup_completo <- aov(mean_exposure ~ iso3 + onet_soc_code, data = occ)
r2_pais_ocup_completo <- summary.lm(anova_pais_ocup_completo)$r.squared
r2_pais_ocup_completo
rm(anova_pais_ocup_completo); gc()
