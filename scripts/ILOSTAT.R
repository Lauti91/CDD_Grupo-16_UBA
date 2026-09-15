install.packages("Rilostat")
library(Rilostat)
library(dplyr)

# Bajar el índice completo de indicadores y filtrar los de earnings por ocupación
toc <- get_ilostat_toc()

toc %>%
  filter(grepl("EAR_", id), grepl("OCU", id)) %>%
  select(id, indicator.label, freq, data.start, data.end, n.records) %>%
  print(n = Inf)

salarios <- get_ilostat("EAR_4MTH_SEX_OCU_CUR_NB_A")


library(dplyr)

toc <- read_csv(paste0(ruta_datos, "table_of_contents_en-20260914T0004.csv"))

# Todos los indicadores de earnings
toc %>% 
  filter(grepl("^EAR", id)) %>% 
  select(id, indicator.label, freq, n.ref_area, data.start, data.end) %>% 
  print(n = Inf)

toc %>% 
  filter(grepl("^EAR", id), grepl("OCU", id)) %>% 
  select(id, indicator.label, freq, n.ref_area, data.start, data.end)

# ==============================================================
# ILOSTAT - Salarios por ocupación (insumo para Instancia 2)
# EAR_EMTA_SEX_OCU_CUR_NB_A : promedio mensual por sexo, ocupación y moneda
# EAR_EMTG_SEX_OCU_NB_A     : Gini de salarios mensuales por ocupación
# Fuente: ILOSTAT bulk download facility
# ==============================================================

sal_prom <- get_ilostat("EAR_EMTA_SEX_OCU_CUR_NB_A")
sal_gini <- get_ilostat("EAR_EMTG_SEX_OCU_NB_A")

# Ver TODAS las clasificaciones que trae, no solo las primeras filas
unique(sal_prom$classif1)

# Separar por tipo de clasificación
table(substr(sal_prom$classif1, 1, 10))

sal_prom %>%
  mutate(tipo_clasif = case_when(
    grepl("SKILL", classif1) ~ "Skill level",
    grepl("ISCO08", classif1) ~ "ISCO-08",
    TRUE ~ "Otro"
  )) %>%
  group_by(tipo_clasif) %>%
  summarise(n_paises = n_distinct(ref_area), n_filas = n())

dim(sal_prom)
dim(sal_gini)
glimpse(sal_prom)

# --- Chequeo 1: nivel de desagregación ISCO ---
unique(sal_prom$classif1)

# --- Chequeo 2: opciones de moneda ---
unique(sal_prom$classif2)

# --- Chequeo 3: cobertura de países y años ---
n_distinct(sal_prom$ref_area)
range(sal_prom$time)

# --- Chequeo 4 (el decisivo): solapamiento con los 124 países del Atlas ---
paises_atlas <- unique(occ$iso3)
paises_ilostat <- unique(sal_prom$ref_area)
length(intersect(paises_atlas, paises_ilostat))

# ¿Cuáles del Atlas NO tienen datos de salarios?
setdiff(paises_atlas, paises_ilostat)

# --- Guardar para no re-descargar cada sesión ---
write_csv(sal_prom, paste0(ruta_datos, "ilostat_salarios_promedio.csv"))
write_csv(sal_gini, paste0(ruta_datos, "ilostat_salarios_gini.csv"))











# Todos los indicadores de earnings
toc %>% filter(grepl("^EAR", id)) %>% select(id, indicator.label, n.ref_area)

# Solo los que tienen desagregación por ocupación
toc %>% filter(grepl("^EAR", id), grepl("OCU", id)) %>% select(id, indicator.label, n.ref_area)





# ==============================================================
# ANÁLISIS A - Exposición vs. salarios, 124 países × 10 grupos ISCO-08
# Insumos:
#   - atlas_country_isco2_panel.csv (repo prashgarg/global-automation-atlas,
#     carpeta outputs/source_data/ilostat_gender/table_B8_inputs/)
#     -> descargar manualmente y subir a datos/
#   - EAR_EMTA_SEX_OCU_CUR_NB_A (ILOSTAT, ya bajado)
#   - EAR_EMTG_SEX_OCU_NB_A (ILOSTAT, ya bajado)
#
# NOTA: este panel usa la métrica `exposed` (share de tareas expuestas,
# 0-1), distinta a `mean_exposure` (escala 0-3) del Script 1. El
# crosswalk O*NET-SOC -> ISCO-08 es el publicado por los autores del
# Atlas; ver Opción 2 (crosswalk propio vía BLS) como refinamiento futuro.
# ==============================================================

library(tidyverse)

# --- 1. Lado Atlas: agregar de ISCO-2 a ISCO-1 ---
atlas_isco2 <- read_csv(paste0(ruta_datos, "atlas_country_isco2_panel.csv"))

unique(atlas_isco2$isco2)

# Los códigos < 10 son fuerzas armadas (grupo 0, sin cero inicial)
# Se agregan sumando numeradores y denominador, NO promediando shares
atlas_isco1 <- atlas_isco2 %>%
  mutate(isco1 = as.integer(substr(isco2, 1, 1))) %>%
  group_by(iso3, country_name, income_level, isco1) %>%
  summarise(
    transport_mass = sum(transport_mass),
    across(c(exposed_num, sub_num, both_num, aug_num), sum),
    .groups = "drop"
  ) %>%
  mutate(
    exposed = exposed_num / transport_mass,
    sub     = sub_num / transport_mass,
    both    = both_num / transport_mass,
    aug     = aug_num / transport_mass
  )

dim(atlas_isco1)              # esperado: 1240 = 124 países × 10 grupos
table(atlas_isco1$isco1)      # esperado: 124 en cada grupo del 0 al 9

# --- 2. Lado ILOSTAT: salarios promedio en PPP, último año por país ---
sal_isco1 <- sal_prom %>%
  filter(
    grepl("^OCU_ISCO08_", classif1),
    !classif1 %in% c("OCU_ISCO08_TOTAL", "OCU_ISCO08_X"),   # excluir total y "no clasificado"
    classif2 == "CUR_TYPE_PPP",
    sex == "SEX_T",
    !is.na(obs_value)
  ) %>%
  mutate(
    isco1 = as.integer(str_remove(classif1, "OCU_ISCO08_")),
    anio  = as.integer(time)
  ) %>%
  # quedarnos con el año más reciente disponible por país
  group_by(iso3 = ref_area) %>%
  filter(anio == max(anio)) %>%
  ungroup() %>%
  select(iso3, isco1, anio, salario_ppp = obs_value)

# --- 3. Gini de salarios (mismo criterio) ---
gini_isco1 <- sal_gini %>%
  filter(
    grepl("^OCU_ISCO08_", classif1),
    !classif1 %in% c("OCU_ISCO08_TOTAL", "OCU_ISCO08_X"),
    sex == "SEX_T",
    !is.na(obs_value)
  ) %>%
  mutate(
    isco1 = as.integer(str_remove(classif1, "OCU_ISCO08_")),
    anio_gini = as.integer(time)
  ) %>%
  group_by(iso3 = ref_area) %>%
  filter(anio_gini == max(anio_gini)) %>%
  ungroup() %>%
  select(iso3, isco1, anio_gini, gini = obs_value)

# --- 4. Merge ---
panel_A <- atlas_isco1 %>%
  inner_join(sal_isco1, by = c("iso3", "isco1")) %>%
  left_join(gini_isco1, by = c("iso3", "isco1"))

# --- 5. Chequeos de viabilidad ---
dim(panel_A)
n_distinct(panel_A$iso3)                    # países que sobrevivieron al merge
panel_A %>% count(isco1)                    # observaciones por grupo ISCO
range(panel_A$anio)                         # rango de años
sum(!is.na(panel_A$gini))                   # cuántas filas tienen Gini

# ¿Qué países del Atlas se perdieron?
setdiff(unique(atlas_isco1$iso3), unique(panel_A$iso3))


panel_A %>% count(anio) %>% arrange(anio) %>% print(n = Inf)
panel_A %>% distinct(iso3, anio) %>% count(anio >= 2018)

panel_A <- panel_A %>% filter(anio >= 2018)
dim(panel_A)
n_distinct(panel_A$iso3)

# --- 6. ¿Aparece la U? ---
panel_A <- panel_A %>% mutate(log_salario = log(salario_ppp))

ggplot(panel_A, aes(x = exposed, y = log_salario)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE, color = "#B2182B") +
  labs(
    title = "Exposición a la automatización vs. salario (PPP)",
    subtitle = "86 países × 10 grupos ISCO-08, datos 2018-2025",
    x = "Share de tareas expuestas", y = "log(salario mensual PPP)"
  ) +
  theme_minimal()

ggsave(paste0(ruta_graficos, "exposicion_vs_salario.png"), width = 8, height = 6)

# Test formal del término cuadrático
modelo_u <- lm(log_salario ~ exposed + I(exposed^2), data = panel_A)
summary(modelo_u)

# Con efectos fijos de país
modelo_u_fe <- lm(log_salario ~ exposed + I(exposed^2) + factor(iso3), data = panel_A)
summary(modelo_u_fe)$coefficients[1:3, ]
