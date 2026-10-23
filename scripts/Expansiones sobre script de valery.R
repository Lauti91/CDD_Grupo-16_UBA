# ==============================================================
# EXPANSIONES SOBRE EL SCRIPT DE VALERY (VALERY_borrador.R)
# TP Ciencia de Datos FCE-UBA - Grupo 16 - Instancia 2
#
# Qué es: tres análisis nuevos que salen de una observación sobre el
# gráfico de clusters de Valery (países sobre los dos primeros
# componentes): pasando Dim1 = -0,5 el gráfico se abre en DOS ramas.
#   - Rama "Arriba": más aumentación pura y menos tareas de exposición muy alta.
#   - Rama "Abajo" : exposición más intensa (más tareas de exposición muy alta).
#
#   OPCIÓN 1. ¿Qué distingue a las dos ramas? (variables EXTERNAS al PCA,
#             con control por nivel de exposición, errores robustos HC3).
#   OPCIÓN 2. HCPC de países sobre la COMPOSICIÓN del margen (tipo de
#             automatización, no solo nivel). Responde a la segunda
#             sugerencia del profesor y a la crítica de colinealidad.
#   OPCIÓN 3. ¿En qué ocupaciones se produce la diferencia? Comparación
#             por ocupación y por grupo SOC entre ramas (con intervalos
#             de bootstrap sobre países) + comparación bilateral ilustrativa
#             (Croacia/Costa Rica vs. Corea).
#
# REGLAS (iguales al script de Valery):
#  - AUTOCONTENIDO: no hace falta correr antes el script de Valery.
#  - SOLO LEE datos/. SOLO ESCRIBE archivos NUEVOS con prefijo "i2x_" en
#    output/tablas/ y output/graficos/. No pisa nada del grupo.
#  - Antes de correr: Session > Set Working Directory > To Project Directory
#    (o abrir el .Rproj).
#
# AVISOS DE HONESTIDAD (para la diapositiva):
#  - Dim2 explica solo 6,3% de la varianza: es un hallazgo SECUNDARIO.
#  - Las ramas se definieron MIRANDO el gráfico (umbral elegido a ojo):
#    es análisis exploratorio, no confirmatorio. Se prueba sensibilidad
#    al umbral y se corrigen los tests de regiones por comparaciones múltiples.
#  - Las ramas se definen con variables que entran al PCA (aumentación e
#    intensidad): compararlas en esas mismas variables sería CIRCULAR
#    (separan perfecto por construcción). Por eso los TESTS usan variables
#    externas (PBI, internet, manufactura, comercio, región).
#  - Que la aumentación crezca con el ingreso YA está en el paper
#    (Tabla B.1): no se presenta como hallazgo propio.
#  - El Atlas mide exposición FACTIBLE juzgada por un modelo de lenguaje
#    condicionado al país: no hay causalidad ni automatización observada.
# ==============================================================


# --------------------------------------------------------------
# 0. PREPARACIÓN (replica lo de Valery para que las ramas sean las mismas)
# --------------------------------------------------------------
getwd()   # tiene que terminar en CDD_Grupo-_UBA (NO en /datos)
stopifnot("Falta fijar el directorio del proyecto (ver arriba)" =
            file.exists("datos/country_map_panel.csv"))

paquetes <- c("tidyverse", "arrow", "FactoMineR", "factoextra",
              "ggrepel", "sandwich", "lmtest")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)

library(tidyverse)
library(arrow)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(sandwich)   # vcovHC: errores estándar robustos a heterocedasticidad
library(lmtest)     # coeftest

dir.create("output/tablas",   showWarnings = FALSE, recursive = TRUE)
dir.create("output/graficos", showWarnings = FALSE, recursive = TRUE)

# --- Paleta del grupo (la de la Instancia 1) -------------------
c_navy    <- "#0B1E3D"
c_naranja <- "#E8622C"
c_gris    <- "#B7BDC7"
c_celeste <- "#6FA3C7"
c_texto   <- "#5A6472"
c_fondo   <- "#F7F8FA"

theme_i2x <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(plot.background   = element_rect(fill = c_fondo, color = NA),
          panel.background  = element_rect(fill = c_fondo, color = NA),
          plot.title        = element_text(face = "bold", color = c_navy, size = base + 3),
          plot.subtitle     = element_text(color = c_texto),
          axis.title        = element_text(color = c_texto, face = "bold"),
          axis.text         = element_text(color = c_navy),
          panel.grid.minor  = element_blank(),
          legend.position   = "top")
}

# --- Base de países y variables del HCPC de Valery -------------
vars_hcpc <- c("share_exposed", "mean_task_exposure", "sub_only_share",
               "both_share", "aug_only_share", "task_exposure_sd",
               "share_high_exposure")

paises <- read_csv("datos/country_map_panel.csv", show_col_types = FALSE) %>%
  mutate(
    log_gdp_pc = log(gdp_per_capita),
    # Composición del margen DENTRO de las tareas expuestas (suman 1).
    # Se normaliza por sub+both+aug y no por share_exposed, porque
    # share_exposed difiere de esa suma hasta ~0,02 en algunos países.
    suma_exp = sub_only_share + both_share + aug_only_share,
    sub_rel  = sub_only_share / suma_exp,
    both_rel = both_share / suma_exp,
    aug_rel  = aug_only_share / suma_exp,   # aumentación pura sobre tareas expuestas
    int_rel  = share_high_exposure / share_exposed  # intensidad: tareas de exposición muy alta sobre expuestas
  )
stopifnot(sum(is.na(paises[vars_hcpc])) == 0)

# --- PCA + HCPC igual que en el script de Valery (k = 3) -------
datos_hcpc <- paises %>%
  select(iso3, all_of(vars_hcpc)) %>%
  as.data.frame() %>%
  column_to_rownames("iso3")

res_pca   <- PCA(datos_hcpc, scale.unit = TRUE, ncp = 3, graph = FALSE)
res_hcpc  <- HCPC(res_pca, nb.clust = 3, graph = FALSE)

# Coordenadas de cada país en Dim1 y Dim2 (el signo del PCA es arbitrario:
# se ordena para que Dim1 crezca con la exposición y Dim2 con la aumentación)
coord <- as.data.frame(res_pca$ind$coord[, 1:2]) %>%
  rownames_to_column("iso3") %>%
  rename(dim1 = Dim.1, dim2 = Dim.2)
if (cor(coord$dim1, paises$mean_task_exposure[match(coord$iso3, paises$iso3)]) < 0) coord$dim1 <- -coord$dim1
if (cor(coord$dim2, paises$aug_only_share[match(coord$iso3, paises$iso3)]) < 0)     coord$dim2 <- -coord$dim2

# Etiquetar los clusters de menor a mayor exposición mediana
# (la numeración que devuelve HCPC es arbitraria)
ordenar_clusters <- function(cl, x) {
  med <- tapply(x, cl, median)
  nuevo <- rank(med, ties.method = "first")
  factor(unname(nuevo[as.character(cl)]), levels = seq_along(med))
}

asig_valery <- tibble(iso3 = rownames(res_hcpc$data.clust),
                      cl_raw = res_hcpc$data.clust$clust)
paises <- paises %>%
  left_join(coord, by = "iso3") %>%
  left_join(asig_valery, by = "iso3") %>%
  mutate(cluster_valery = ordenar_clusters(cl_raw, mean_task_exposure)) %>%
  select(-cl_raw)

# Control: tienen que reproducirse los tamaños del script de Valery
table(paises$cluster_valery)   # esperado: 23 / 33 / 68

# Índice de Rand ajustado (igual que en el script de Valery)
ari <- function(a, b) {
  t <- table(a, b); n <- sum(t); s <- function(x) sum(choose(x, 2))
  sa <- s(rowSums(t)); sb <- s(colSums(t)); sij <- s(t); e <- sa * sb / choose(n, 2)
  (sij - e) / ((sa + sb) / 2 - e)
}


# ==============================================================
# OPCIÓN 1. ¿QUÉ DISTINGUE A LAS DOS RAMAS?
# ==============================================================

## 1.0 Definición de las ramas (umbrales elegidos mirando el gráfico)
umbral_dim1 <- -0.5   # desde acá el gráfico se abre en dos ramas
umbral_dim2 <-  0.4   # separa la rama de arriba de la de abajo

paises <- paises %>%
  mutate(rama = case_when(
    dim1 >  umbral_dim1 & dim2 >  umbral_dim2 ~ "Arriba",
    dim1 >  umbral_dim1                       ~ "Abajo",
    TRUE                                      ~ "Baja"      # exposición baja (Dim1 <= -0.5)
  ))
table(paises$rama)   # esperado: Arriba 20 / Abajo 56 / Baja 48

paises %>% filter(rama == "Arriba") %>%
  arrange(desc(dim2)) %>%
  select(iso3, country_name, region, income_level, dim1, dim2,
         aug_rel, int_rel, mean_task_exposure) %>%
  print(n = Inf)
write_csv(paises %>% select(iso3, country_name, region, income_level, dim1, dim2,
                            rama, cluster_valery, aug_rel, int_rel, mean_task_exposure),
          "output/tablas/i2x_paises_por_rama.csv")

## 1.1 Perfil DESCRIPTIVO (no inferencial) con las variables del PCA
# CUIDADO: estas variables definen las ramas, así que NO se les hace test
# (daría diferencias "perfectas" por construcción). Solo se describen.
perfil_ramas <- paises %>%
  filter(rama != "Baja") %>%
  group_by(rama) %>%
  summarise(n_paises = n(),
            across(c(mean_task_exposure, share_exposed, sub_rel, both_rel,
                     aug_rel, int_rel),
                   ~ round(median(.x), 3)),
            .groups = "drop")
print(perfil_ramas, width = Inf)
write_csv(perfil_ramas, "output/tablas/i2x_perfil_ramas_descriptivo.csv")
# Esperado (medianas): Arriba exp 1,33 | aug_rel ~0,16 | int_rel ~0,14
#                      Abajo  exp 1,49 | aug_rel ~0,09 | int_rel ~0,25

## 1.2 Tests con variables EXTERNAS al PCA
datos_rama <- paises %>% filter(rama != "Baja")

# (a) Mann-Whitney crudo + tamaño del efecto (correlación biserial por rangos)
test_mw <- function(d, variable) {
  a <- d[[variable]][d$rama == "Arriba"]; a <- a[is.finite(a)]
  b <- d[[variable]][d$rama == "Abajo"];  b <- b[is.finite(b)]
  w <- wilcox.test(a, b, exact = FALSE)
  tibble(variable = variable,
         n_arriba = length(a), n_abajo = length(b),
         mediana_arriba = median(a), mediana_abajo = median(b),
         p_mann_whitney = w$p.value,
         biserial = 2 * unname(w$statistic) / (length(a) * length(b)) - 1)
}

# (b) Diferencia entre ramas CONTROLANDO por nivel de exposición (Dim1),
#     con errores robustos HC3 (n chico y heterocedasticidad probable).
#     Esto resuelve que las ramas no tienen exactamente la misma exposición.
coef_rama <- function(d, variable) {
  d <- d %>% filter(is.finite(.data[[variable]])) %>%
    mutate(arriba = as.numeric(rama == "Arriba"))
  mod <- lm(as.formula(paste(variable, "~ arriba + dim1")), data = d)
  ct  <- coeftest(mod, vcov. = vcovHC(mod, type = "HC3"))
  tibble(variable = variable, n = nrow(d),
         coef_arriba = ct["arriba", "Estimate"],
         ee_hc3      = ct["arriba", "Std. Error"],
         p_hc3       = ct["arriba", "Pr(>|t|)"])
}

vars_externas <- c("log_gdp_pc", "internet_users_pct",
                   "manufacturing_value_added_pct_gdp", "trade_pct_gdp")

tabla_ext <- map_dfr(vars_externas, ~ left_join(test_mw(datos_rama, .x),
                                                coef_rama(datos_rama, .x),
                                                by = c("variable")))
print(tabla_ext, width = Inf)
write_csv(tabla_ext, "output/tablas/i2x_ramas_variables_externas.csv")
# Esperado: log PBI p.c.: coef_arriba ~ -0,58 (p ~ 0,0001) ; internet ~ -6,3 pp (p ~ 0,015);
#           manufactura y comercio: sin diferencia (p > 0,5).
# LECTURA: la rama de arriba tiene menor PBI e internet que la de abajo A IGUAL
# EXPOSICIÓN, pero no se explica por manufactura ni por apertura comercial.

## 1.3 ¿Se concentran las ramas por región? (exploratorio, con corrección)
# Se prueban TODAS las regiones y se corrige por comparaciones múltiples
# (Holm), porque las dos regiones destacadas se eligieron mirando el gráfico.
tabla_regiones <- map_dfr(sort(unique(datos_rama$region)), function(r) {
  tb <- table(factor(datos_rama$region == r, levels = c(FALSE, TRUE)),
              factor(datos_rama$rama == "Arriba", levels = c(FALSE, TRUE)))
  tibble(region = r,
         n_region = sum(tb[2, ]),
         n_arriba = tb[2, 2],
         pct_arriba = round(100 * tb[2, 2] / sum(tb[2, ]), 0),
         p_fisher = fisher.test(tb)$p.value)
}) %>%
  mutate(p_holm = p.adjust(p_fisher, method = "holm")) %>%
  arrange(p_fisher)
print(tabla_regiones, n = Inf)
write_csv(tabla_regiones, "output/tablas/i2x_ramas_por_region.csv")
# Esperado: América Latina y Caribe 8 de 15 arriba (p Fisher ~ 0,018);
# Asia Oriental y Pacífico 0 de 12 arriba (p ~ 0,029); NINGUNA sobrevive a Holm
# (p_holm ~ 0,13 y ~ 0,18) -> se presenta como patrón sugerente, no como prueba.

## 1.4 Regresión con errores robustos: ¿qué explica la aumentación relativa?
# (no es causal; descriptivo con n = 124 países)
reg_robusta <- function(formula, data) {
  mod <- lm(formula, data = data)
  ct  <- coeftest(mod, vcov. = vcovHC(mod, type = "HC3"))
  list(mod = mod, tabla = as.data.frame(unclass(ct)) %>% rownames_to_column("termino"))
}
m_aug1 <- reg_robusta(aug_rel ~ mean_task_exposure + log_gdp_pc, paises)
m_aug2 <- reg_robusta(aug_rel ~ mean_task_exposure + log_gdp_pc + region, paises)
m_int1 <- reg_robusta(int_rel ~ mean_task_exposure + log_gdp_pc, paises)
m_aug1$tabla; summary(m_aug1$mod)$r.squared
m_aug2$tabla; summary(m_aug2$mod)$r.squared
m_int1$tabla; summary(m_int1$mod)$r.squared
bind_rows(m_aug1$tabla %>% mutate(modelo = "aug_rel ~ exposición + log PBI"),
          m_aug2$tabla %>% mutate(modelo = "aug_rel ~ exposición + log PBI + región"),
          m_int1$tabla %>% mutate(modelo = "int_rel ~ exposición + log PBI")) %>%
  write_csv("output/tablas/i2x_regresiones_hc3.csv")
# RESULTADO (corrida de Lautaro, 23/10):
#  - aug_rel ~ exposición + log PBI: el log PBI NO suma (coef ~ 0,0001; t ~ 0,02): a igual
#    exposición, el ingreso no explica la aumentación relativa (R2 ~ 0,21; solo pesa la exposición).
#  - aug_rel con región: América Latina y Caribe +0,035 (p ~ 0,002) y Europa y Asia Central
#    +0,017 (p ~ 0,03) respecto de la categoría base (Asia Oriental y Pacífico), controlando por
#    exposición y PBI (R2 ~ 0,30). Es un efecto regional, no de ingreso.
#  - int_rel ~ exposición + log PBI: el log PBI SÍ suma (+0,019; t ~ 3,9; R2 ~ 0,88): a igual
#    exposición, los países más ricos tienen una proporción mayor de tareas de exposición muy alta.
# LECTURA: la brecha de PBI entre ramas viene del lado de la INTENSIDAD (int_rel), no de la
# aumentación; la aumentación relativa alta se asocia más con la región (América Latina).
# (Las comparaciones de aumentación con el ingreso que reporta el paper no se contradicen: acá
# se controla por el nivel de exposición.)

## 1.5 Sensibilidad a AMBOS umbrales (Dim1 y Dim2)
# Dim2: entre los países con Dim1 > -0,5 hay un hueco natural (ningún país entre
#       -0,07 y 0,83), por eso 0,3/0,4/0,5 dan lo mismo.
# Dim1: la rama de arriba en realidad empieza antes (cerca de -2,5, como se ve en el
#       gráfico); -0,5 deja afuera 8 países con Dim2 alto y exposición baja (Senegal,
#       Camboya, Zimbabue, Benín, Ruanda, Bolivia, Cuba, Nicaragua). Se prueban umbrales
#       más amplios para ver si la conclusión cambia.
grilla <- expand_grid(umbral_dim1_s = c(-2.5, -2.0, -1.5, -1.0, -0.5),
                      umbral_dim2_s = c(0.3, 0.4, 0.5))
sens <- pmap_dfr(grilla, function(umbral_dim1_s, umbral_dim2_s) {
  d <- paises %>%
    filter(dim1 > umbral_dim1_s) %>%
    mutate(rama = if_else(dim2 > umbral_dim2_s, "Arriba", "Abajo"))
  bind_cols(tibble(umbral_dim1 = umbral_dim1_s, umbral_dim2 = umbral_dim2_s,
                   n_arriba = sum(d$rama == "Arriba"), n_total = nrow(d)),
            coef_rama(d, "log_gdp_pc") %>% select(coef_arriba, p_hc3))
})
print(sens, n = Inf)
write_csv(sens, "output/tablas/i2x_sensibilidad_umbral.csv")
# Esperado: coef_arriba entre ~ -0,38 y -0,58 con p < 0,02 en TODA la grilla (n_arriba de 20 a 30):
# la diferencia de PBI a igual exposición no depende del umbral elegido.

## 1.6 Gráficos de la Opción 1
col_rama <- c("Arriba" = c_naranja, "Abajo" = c_navy, "Baja" = c_gris)

p_ramas <- ggplot(paises, aes(dim1, dim2, color = rama)) +
  geom_hline(yintercept = 0, color = "grey85") +
  geom_vline(xintercept = 0, color = "grey85") +
  geom_vline(xintercept = umbral_dim1, linetype = "dashed", color = c_texto) +
  geom_hline(yintercept = umbral_dim2, linetype = "dotted", color = c_texto) +
  geom_point(size = 2.8, alpha = 0.9) +
  geom_text_repel(data = filter(paises, rama == "Arriba"),
                  aes(label = country_name), size = 3, color = "grey20",
                  max.overlaps = Inf, box.padding = 0.4, min.segment.length = 0) +
  scale_color_manual(values = col_rama,
                     labels = c("Arriba" = "Rama de arriba: más aumentación",
                                "Abajo"  = "Rama de abajo: exposición más intensa",
                                "Baja"   = "Exposición baja (Dim1 ≤ -0,5)"),
                     name = NULL) +
  labs(title = "Dos ramas de países a partir de Dim1 = -0,5",
       subtitle = "Dim2 (6,3% de la varianza) separa aumentación pura (+) de tareas de exposición muy alta (−)",
       x = "Dim 1 · cuán expuesto es el país (89%)",
       y = "Dim 2 · aumentación pura (+) / tareas muy expuestas (−)") +
  theme_i2x()
p_ramas
ggsave("output/graficos/i2x_ramas_pca.png", p_ramas, width = 11, height = 7,
       dpi = 300, bg = c_fondo)

p_ext <- datos_rama %>%
  filter(is.finite(log_gdp_pc)) %>%
  ggplot(aes(x = rama, y = log_gdp_pc, color = rama)) +
  geom_boxplot(aes(fill = rama), alpha = 0.18, outlier.shape = NA, width = 0.5) +
  geom_jitter(width = 0.15, size = 2.4, alpha = 0.9) +
  scale_color_manual(values = col_rama, guide = "none") +
  scale_fill_manual(values = col_rama, guide = "none") +
  scale_x_discrete(labels = c("Arriba" = "Rama de arriba", "Abajo" = "Rama de abajo")) +
  labs(title = "La rama de arriba tiene menor PBI per cápita",
       subtitle = "Boxplot sin ajustar. Controlando por nivel de exposición (Dim1, errores HC3): diferencia de −0,58 en log(PBI p.c.)",
       x = NULL, y = "log(PBI per cápita)") +
  theme_i2x()
p_ext
ggsave("output/graficos/i2x_ramas_pbi.png", p_ext, width = 8, height = 6,
       dpi = 300, bg = c_fondo)


# ==============================================================
# OPCIÓN 2. HCPC DE PAÍSES SOBRE LA COMPOSICIÓN DEL MARGEN
# ==============================================================
# IDEA: el HCPC de Valery ordena por NIVEL de exposición (Dim1 = 89%) porque
# sus 7 variables son casi la misma (correlaciones 0,8-0,99; share_exposed es
# casi la suma de sub + both + aug). Acá se clasifica por TIPO de automatización:
#  - composición de las tareas expuestas (sustitución sola / ambas / aumentación
#    sola) con transformación log-ratio centrada (clr), el tratamiento estándar
#    para datos composicionales (suman 1 y no pueden tratarse como libres), y
#  - intensidad: proporción de tareas de exposición muy alta entre las expuestas.
# Es la "segunda opción" que sugirió el profesor (clasificar por las shares).

## 2.1 Armado de las variables
stopifnot(all(paises$sub_rel > 0, paises$both_rel > 0, paises$aug_rel > 0))

comp <- paises %>%
  mutate(l_sub = log(sub_rel), l_both = log(both_rel), l_aug = log(aug_rel),
         centro = (l_sub + l_both + l_aug) / 3,
         clr_sub = l_sub - centro, clr_both = l_both - centro, clr_aug = l_aug - centro)

datos_comp <- comp %>%
  select(iso3, clr_sub, clr_both, clr_aug, int_rel) %>%
  as.data.frame() %>%
  column_to_rownames("iso3")

round(cor(datos_comp), 2)   # ahora las variables ya NO son casi idénticas entre sí

## 2.2 PCA + HCPC
res_pca_c <- PCA(datos_comp, scale.unit = TRUE, ncp = 3, graph = FALSE)
res_pca_c$eig                 # esperado: Dim1 ~69%, Dim2 ~21%, Dim3 ~9% (mucho más repartido que el 89% de Valery)
write_csv(as.data.frame(res_pca_c$eig) %>% rownames_to_column("componente"),
          "output/tablas/i2x_comp_pca_autovalores.csv")

# Altura de las últimas fusiones, para justificar k
arbol_c <- HCPC(res_pca_c, nb.clust = 3, graph = FALSE)$call$t$tree
round(head(sort(arbol_c$height, decreasing = TRUE), 8), 1)
# esperado: ~20,9 | 14,5 | 11,0 | 8,3 | 4,9 ...  (saltos menos nítidos que en el original:
# k = 3 es razonable pero hay que mostrar sensibilidad)

k_comp <- 3
hc_c <- HCPC(res_pca_c, nb.clust = k_comp, graph = FALSE)
asig_c <- tibble(iso3 = rownames(hc_c$data.clust), cl_raw = hc_c$data.clust$clust)
comp <- comp %>%
  left_join(asig_c, by = "iso3") %>%
  mutate(cluster_comp = ordenar_clusters(cl_raw, mean_task_exposure)) %>%
  select(-cl_raw)
table(comp$cluster_comp)   # esperado (aprox.): 33 / 37 / 54 (el orden es por exposición mediana)

## 2.3 Perfil de cada cluster (medianas) y relación con ingreso y región
perfil_comp <- comp %>%
  group_by(cluster_comp) %>%
  summarise(n_paises = n(),
            across(c(mean_task_exposure, sub_rel, both_rel, aug_rel, int_rel,
                     gdp_per_capita, internet_users_pct),
                   ~ median(.x, na.rm = TRUE)),
            .groups = "drop")
print(perfil_comp, width = Inf)
write_csv(perfil_comp, "output/tablas/i2x_comp_perfil_clusters.csv")
# Esperado: un cluster de exposición baja (PBI p.c. ~1.150), otro de exposición alta e
# INTENSA (int_rel ~0,25; PBI ~30.000) y un tercero con MÁS AUMENTACIÓN relativa
# (aug_rel ~0,14) y exposición intermedia (PBI ~4.600): es el "tipo" que el HCPC original no distinguía.

tab_ing <- table(comp$cluster_comp, comp$income_level); tab_ing
tab_reg <- table(comp$cluster_comp, comp$region);       tab_reg
write_csv(as.data.frame(tab_ing), "output/tablas/i2x_comp_cluster_ingreso.csv")
write_csv(as.data.frame(tab_reg), "output/tablas/i2x_comp_cluster_region.csv")

## 2.4 Comparación con el HCPC de Valery y sensibilidad a k
tab_vs_valery <- table(composicion = comp$cluster_comp, valery = comp$cluster_valery)
tab_vs_valery
write_csv(as.data.frame(tab_vs_valery), "output/tablas/i2x_comp_vs_valery.csv")
ari(comp$cluster_comp, comp$cluster_valery)   # esperado ~0,44: particiones MUY distintas (es lo buscado)

cl_k <- function(k) HCPC(res_pca_c, nb.clust = k, graph = FALSE)$data.clust$clust
sens_k <- tibble(iso3 = rownames(hc_c$data.clust),
                 k2 = cl_k(2), k3 = cl_k(3), k4 = cl_k(4))
ari_k <- tibble(comparacion = c("k=3 vs k=2", "k=3 vs k=4"),
                ari = c(ari(sens_k$k3, sens_k$k2), ari(sens_k$k3, sens_k$k4)))
ari_k
write_csv(ari_k, "output/tablas/i2x_comp_sensibilidad_k.csv")

write_csv(comp %>% select(iso3, country_name, region, income_level,
                          cluster_valery, cluster_comp, sub_rel, both_rel, aug_rel, int_rel),
          "output/tablas/i2x_comp_paises_por_cluster.csv")

# Países de referencia
comp %>%
  filter(iso3 %in% c("ARG", "BRA", "CHL", "MEX", "COL", "KOR", "CHN", "USA", "DEU", "HRV", "CRI")) %>%
  select(iso3, country_name, cluster_valery, cluster_comp, aug_rel, int_rel) %>%
  print(n = Inf)

## 2.5 Gráficos de la Opción 2
col_comp <- c("1" = c_celeste, "2" = c_naranja, "3" = c_navy)
coord_c <- as.data.frame(res_pca_c$ind$coord[, 1:2]) %>%
  rownames_to_column("iso3") %>%
  rename(c1 = Dim.1, c2 = Dim.2) %>%
  left_join(comp %>% select(iso3, country_name, cluster_comp), by = "iso3")
eig_c <- res_pca_c$eig[, 2]

p_comp <- ggplot(coord_c, aes(c1, c2, color = cluster_comp, fill = cluster_comp)) +
  geom_hline(yintercept = 0, color = "grey85") +
  geom_vline(xintercept = 0, color = "grey85") +
  stat_ellipse(geom = "polygon", level = 0.9, alpha = 0.10, linewidth = 0.6) +
  geom_point(size = 2.6, shape = 21, color = "white", stroke = 0.6) +
  geom_text_repel(data = filter(coord_c, country_name %in%
                                  c("Argentina", "Brazil", "Korea, Rep.", "China",
                                    "United States", "Croatia", "Costa Rica", "India",
                                    "South Sudan", "Nigeria")),
                  aes(label = country_name), color = "grey15", size = 3.3,
                  max.overlaps = Inf, box.padding = 0.5, min.segment.length = 0) +
  scale_color_manual(values = col_comp, name = NULL) +
  scale_fill_manual(values = col_comp, name = NULL) +
  labs(title = "Clusters de países por tipo de automatización",
       subtitle = "HCPC sobre composición del margen (log-ratio) e intensidad, k = 3",
       x = paste0("Dim 1 (", round(eig_c[1], 1), "%)"),
       y = paste0("Dim 2 (", round(eig_c[2], 1), "%)")) +
  theme_i2x()
p_comp
ggsave("output/graficos/i2x_comp_clusters.png", p_comp, width = 10, height = 7,
       dpi = 300, bg = c_fondo)

p_comp_vars <- fviz_pca_var(res_pca_c, repel = TRUE, col.var = c_navy) +
  labs(title = "PCA de composición: variables sobre los dos primeros componentes")
ggsave("output/graficos/i2x_comp_pca_variables.png", p_comp_vars, width = 8, height = 6,
       dpi = 300, bg = "white")

# Cruce HCPC original vs. HCPC de composición (muestra que clasifican distinto)
p_cruce <- as.data.frame(tab_vs_valery) %>%
  ggplot(aes(x = valery, y = composicion, fill = Freq)) +
  geom_tile(color = c_fondo, linewidth = 1.2) +
  geom_text(aes(label = Freq), color = "white", fontface = "bold", size = 6) +
  scale_fill_gradient(low = c_celeste, high = c_navy, guide = "none") +
  labs(title = "Los dos HCPC clasifican distinto",
       subtitle = "Cantidad de países según cluster de nivel (Valery) y cluster de composición",
       x = "Cluster por nivel de exposición (Valery)", y = "Cluster por tipo de margen") +
  theme_i2x() + theme(panel.grid = element_blank())
p_cruce
ggsave("output/graficos/i2x_comp_cruce_con_valery.png", p_cruce, width = 8, height = 5.5,
       dpi = 300, bg = c_fondo)


# ==============================================================
# OPCIÓN 3. ¿EN QUÉ OCUPACIONES SE PRODUCE LA DIFERENCIA?
# ==============================================================
# Se vuelve al panel país x ocupación. Se compara el promedio por ocupación
# de las dos ramas (Arriba: 20 países; Abajo: 56 países). Es el mismo tipo de
# comparación que la de Corea vs. Argentina de la Instancia 1, pero entre grupos.

occ <- read_parquet("datos/country_occupation_panel.parquet",
                    col_select = c("iso3", "onet_soc_code", "occupation_title",
                                   "task_count", "mean_exposure", "sub_only_share",
                                   "both_share", "aug_only_share"))

soc_labels <- tibble::tribble(
  ~soc_major, ~soc_label,
  "11", "Management", "13", "Business & Financial", "15", "Computer & Mathematical",
  "17", "Architecture & Engineering", "19", "Life, Physical & Social Science",
  "21", "Community & Social Service", "23", "Legal", "25", "Education",
  "27", "Arts, Design, Media", "29", "Healthcare Practitioners",
  "31", "Healthcare Support", "33", "Protective Service",
  "35", "Food Preparation & Serving", "37", "Building & Grounds Maintenance",
  "39", "Personal Care & Service", "41", "Sales",
  "43", "Office & Administrative Support", "45", "Farming, Fishing & Forestry",
  "47", "Construction & Extraction", "49", "Installation, Maintenance & Repair",
  "51", "Production", "53", "Transportation & Material Moving"
)

occ <- occ %>%
  mutate(soc_major = str_sub(onet_soc_code, 1, 2)) %>%
  left_join(soc_labels, by = "soc_major") %>%
  left_join(paises %>% select(iso3, rama), by = "iso3")
occ_g <- occ %>% filter(rama %in% c("Arriba", "Abajo"))
n_distinct(occ_g$iso3[occ_g$rama == "Arriba"])   # 20
n_distinct(occ_g$iso3[occ_g$rama == "Abajo"])    # 56

## 3.1 Diferencia por ocupación (923 ocupaciones): Arriba menos Abajo
por_ocup <- occ_g %>%
  group_by(rama, onet_soc_code, occupation_title, soc_label) %>%
  summarise(across(c(mean_exposure, sub_only_share, both_share, aug_only_share), mean),
            .groups = "drop") %>%
  pivot_wider(names_from = rama,
              values_from = c(mean_exposure, sub_only_share, both_share, aug_only_share),
              names_glue = "{.value}_{rama}") %>%
  mutate(dif_exposicion = mean_exposure_Arriba - mean_exposure_Abajo,
         dif_aug        = aug_only_share_Arriba - aug_only_share_Abajo,
         dif_both       = both_share_Arriba - both_share_Abajo,
         dif_sub        = sub_only_share_Arriba - sub_only_share_Abajo)

cor(por_ocup$mean_exposure_Arriba, por_ocup$mean_exposure_Abajo)
# esperado ~0,98: el RANKING de ocupaciones es casi idéntico en las dos ramas;
# las ramas difieren en el nivel y el tipo de margen, no en qué ocupaciones son más expuestas.

por_ocup %>% summarise(across(c(dif_exposicion, dif_aug, dif_both, dif_sub),
                              list(media = mean, mediana = median)))
write_csv(por_ocup, "output/tablas/i2x_ocupaciones_dif_ramas.csv")

top_aug <- por_ocup %>% slice_max(dif_aug, n = 15) %>%
  select(occupation_title, soc_label, dif_aug, dif_exposicion)
top_exp <- por_ocup %>% slice_min(dif_exposicion, n = 15) %>%
  select(occupation_title, soc_label, dif_exposicion, dif_sub)
print(top_aug, n = Inf); print(top_exp, n = Inf)
write_csv(top_aug, "output/tablas/i2x_ocupaciones_top_dif_aumentacion.csv")
write_csv(top_exp, "output/tablas/i2x_ocupaciones_top_dif_exposicion.csv")
# Esperado: la mayor diferencia en aumentación está en ocupaciones profesionales
# (médicos, ingenierías, ciencias ambientales, TI); la mayor diferencia (negativa) de
# exposición, en ocupaciones manuales/rutinarias (ferroviarias, dragado, hornos, servicios).

## 3.2 Por grupo SOC, con intervalos de bootstrap sobre PAÍSES
# Se remuestrean países dentro de cada rama (20 y 56 países): la incertidumbre
# relevante es cuántos países hay, no cuántas ocupaciones.
por_pais_soc <- occ_g %>%
  group_by(iso3, rama, soc_major) %>%
  summarise(aug = mean(aug_only_share), expo = mean(mean_exposure),
            sub = mean(sub_only_share), .groups = "drop")

matriz <- function(v) {
  por_pais_soc %>%
    select(iso3, soc_major, all_of(v)) %>%
    pivot_wider(names_from = soc_major, values_from = all_of(v)) %>%
    column_to_rownames("iso3") %>%
    as.matrix()
}
M <- list(aug = matriz("aug"), expo = matriz("expo"), sub = matriz("sub"))
iso_arr <- paises$iso3[paises$rama == "Arriba"]
iso_abj <- paises$iso3[paises$rama == "Abajo"]

dif_medias <- function(Mat, a, b) colMeans(Mat[a, , drop = FALSE]) - colMeans(Mat[b, , drop = FALSE])
punto <- lapply(M, dif_medias, a = iso_arr, b = iso_abj)

set.seed(123)
B <- 2000
boot <- lapply(M, function(Mat) {
  replicate(B, dif_medias(Mat, sample(iso_arr, replace = TRUE), sample(iso_abj, replace = TRUE)))
})
ic <- function(x) apply(x, 1, quantile, probs = c(0.025, 0.975))

tabla_soc <- tibble(soc_major = names(punto$aug)) %>%
  left_join(soc_labels, by = "soc_major") %>%
  mutate(dif_aug = punto$aug, aug_lo = ic(boot$aug)[1, ], aug_hi = ic(boot$aug)[2, ],
         dif_expo = punto$expo, expo_lo = ic(boot$expo)[1, ], expo_hi = ic(boot$expo)[2, ],
         dif_sub = punto$sub, sub_lo = ic(boot$sub)[1, ], sub_hi = ic(boot$sub)[2, ]) %>%
  arrange(desc(dif_aug))
print(tabla_soc %>% mutate(across(where(is.numeric), ~ round(.x, 3))), n = Inf, width = Inf)
write_csv(tabla_soc, "output/tablas/i2x_soc_dif_ramas_bootstrap.csv")
# Esperado: dif_aug positiva (IC sin cruzar 0) en los 22 grupos, pero MUY desigual: mayor en
# Computer & Math (+0,08), Life/Physical/Social Science, Architecture & Engineering,
# Management, Healthcare Practitioners (+0,06 a +0,07) y mínima en Production/Transportation/
# Office (~ +0,01); dif_expo muy negativa (~ -0,24 a -0,30) en Farming, Food Prep,
# Construction, Transportation, Production (IC sin cruzar 0 en ~18 de 22 grupos).
# Es decir, DOS mecanismos distintos: en el trabajo profesional la rama de arriba tiene más
# aumentación; en el trabajo manual/rutinario tiene menos exposición.

largo_soc <- bind_rows(
  tabla_soc %>% transmute(soc_label, medida = "Aumentación pura (dif. de share)",
                          dif = dif_aug, lo = aug_lo, hi = aug_hi),
  tabla_soc %>% transmute(soc_label, medida = "Exposición media (dif.)",
                          dif = dif_expo, lo = expo_lo, hi = expo_hi)
)
orden_soc <- tabla_soc %>% arrange(dif_aug) %>% pull(soc_label)
p_soc <- largo_soc %>%
  mutate(soc_label = factor(soc_label, levels = orden_soc)) %>%
  ggplot(aes(x = dif, y = soc_label)) +
  geom_vline(xintercept = 0, color = c_texto) +
  geom_pointrange(aes(xmin = lo, xmax = hi), color = c_navy, size = 0.45) +
  facet_wrap(~ medida, scales = "free_x") +
  labs(title = "Dónde difieren las dos ramas",
       subtitle = "Rama de arriba menos rama de abajo, por grupo SOC (IC 95% por bootstrap de países)",
       x = NULL, y = NULL) +
  theme_i2x(11) +
  theme(strip.text = element_text(face = "bold", color = c_navy))
p_soc
ggsave("output/graficos/i2x_soc_dif_ramas.png", p_soc, width = 12, height = 7.5,
       dpi = 300, bg = c_fondo)

## 3.3 Comparación bilateral: Croacia y Costa Rica vs. Corea del Sur
# Misma idea que Corea vs. Argentina (Instancia 1). OJO: cada ocupación tiene
# ~26 tareas, así que las shares son múltiplos de 1/n y una diferencia de 0,3-0,4
# equivale a pocas tareas: se filtran ocupaciones con >= 20 tareas y las
# diferencias por ocupación son ILUSTRATIVAS; lo sólido es el resumen por SOC.
bilateral <- function(a, b, min_tareas = 20) {
  x <- occ %>% filter(iso3 == a) %>%
    select(onet_soc_code, occupation_title, soc_label, task_count,
           aug_a = aug_only_share, exp_a = mean_exposure)
  y <- occ %>% filter(iso3 == b) %>%
    select(onet_soc_code, aug_b = aug_only_share, exp_b = mean_exposure)
  inner_join(x, y, by = "onet_soc_code") %>%
    filter(task_count >= min_tareas) %>%
    mutate(par = paste(a, "-", b),
           dif_aug = aug_a - aug_b, dif_exp = exp_a - exp_b)
}

bil <- bind_rows(bilateral("HRV", "KOR"), bilateral("CRI", "KOR"))

bil_soc <- bil %>%
  group_by(par, soc_label) %>%
  summarise(n_ocup = n(), dif_aug = mean(dif_aug), dif_exp = mean(dif_exp), .groups = "drop") %>%
  arrange(par, desc(dif_aug))
print(bil_soc, n = Inf)
write_csv(bil_soc, "output/tablas/i2x_bilateral_por_soc.csv")

bil_top <- bil %>% group_by(par) %>% slice_max(dif_aug, n = 10) %>% ungroup() %>%
  select(par, occupation_title, soc_label, dif_aug, dif_exp)
print(bil_top, n = Inf)
write_csv(bil_top, "output/tablas/i2x_bilateral_top_ocupaciones.csv")

bil %>% group_by(par) %>% summarise(dif_aug_medio = mean(dif_aug), dif_exp_medio = mean(dif_exp))
# Esperado: ~ +0,05 de aumentación pura y ~ -0,13 a -0,15 de exposición media (Croacia y
# Costa Rica respecto de Corea), con la aumentación concentrada en ocupaciones profesionales.


# ==============================================================
# CIERRE: QUÉ PONER EN LAS DIAPOSITIVAS (resultados esperados, a verificar al correr)
# ==============================================================
# 1) Hallazgo: pasando cierto nivel de exposición, los países se abren en dos
#    ramas. La de arriba (20 países; sobre todo América Latina, Medio Oriente y
#    Europa del Este) tiene más aumentación relativa y menos exposición intensa;
#    la de abajo (56; Asia Oriental, Norteamérica, Europa Occidental) más intensidad.
# 2) A igual exposición, la rama de arriba tiene menor PBI (−0,58 en log) e internet
#    (−6 pp); NO se explica por manufactura ni comercio. Regiones: sugerente, no
#    significativo tras Holm.
# 3) Reclasificar por composición del margen da otra tipología (ARI ~0,44 contra
#    el HCPC por nivel): un cluster de aumentación relativa alta que el HCPC original no veía.
# 4) La diferencia entre ramas viene de dos lugares: más aumentación en el trabajo
#    profesional y menos exposición en el trabajo manual/rutinario.
# LIMITACIONES: Dim2 = 6,3% de la varianza; umbrales elegidos a ojo (exploratorio);
# 20 países en la rama chica; exposición factible juzgada por un LLM condicionado al
# país (el contexto del país entra en el juicio); aumentación-ingreso ya está en el paper;
# nada es causal.