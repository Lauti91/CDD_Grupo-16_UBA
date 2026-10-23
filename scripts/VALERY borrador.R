# ==============================================================
# INSTANCIA 2 - Valery
# TP Ciencia de Datos FCE-UBA - Grupo 16
# Segunda entrega: faltantes, outliers, ANOVA (unidad 2, inferencia),
# HCPC de países (unidad 3, cuantitativo) y un contraste final de los
# clusters con el nivel de ingreso.
#
# Archivo AUTOCONTENIDO: no depende de ningún otro script.
#  - SOLO LEE de datos/ y de output/tablas/ (archivos que ya existen).
#  - SOLO ESCRIBE archivos NUEVOS con prefijo "i2_" en
#    output/tablas/ y output/graficos/. No pisa nada del grupo.
#
# Cómo correrlo: línea por línea con Ctrl+Enter (o seleccionando
# un bloque y Ctrl+Enter). Los comentarios "PARA LA DIAPOSITIVA"
# dicen qué ir anotando para la presentación.
# ==============================================================


# --------------------------------------------------------------
# 0. PREPARACIÓN
# --------------------------------------------------------------

# ANTES DE CORRER: menú Session > Set Working Directory > To Project Directory
# (la consola venía parada en la carpeta datos/, y así no funcionan las rutas)
getwd()   # tiene que terminar en CDD_Grupo-_UBA (NO en /datos)
stopifnot("Falta fijar el directorio del proyecto (ver arriba)" =
            file.exists("datos/country_map_panel.csv"))

paquetes <- c("tidyverse", "arrow", "FactoMineR", "factoextra")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)

library(tidyverse)
library(arrow)        # para leer el parquet del panel país-ocupación
library(FactoMineR)   # PCA() y HCPC()
library(factoextra)   # gráficos de PCA y clusters

# Base a nivel PAÍS (una fila por país, 124 filas)
paises <- read_csv("datos/country_map_panel.csv", show_col_types = FALSE)
dim(paises)   # 124 filas x 53 columnas


# --------------------------------------------------------------
# 1. VALORES FALTANTES
# --------------------------------------------------------------
# PARA ENTENDER: la base principal (país x ocupación) no tiene
# nulos. En la base de países sí hay algunos, sobre todo en las
# variables de contexto (comercio, manufactura, internet).

## 1.1 Cuántos faltantes tiene cada variable
tabla_faltantes <- paises %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_faltantes") %>%
  filter(n_faltantes > 0) %>%
  mutate(pct = round(100 * n_faltantes / nrow(paises), 1)) %>%
  arrange(desc(n_faltantes))

print(tabla_faltantes, n = Inf)
write_csv(tabla_faltantes, "output/tablas/i2_faltantes_por_variable.csv")

# Las variables de comercio de servicios faltan en ~76% de los países:
# no sirven. Hay que descartarlas (PARA LA DIAPOSITIVA: "eliminar variable").

## 1.2 Dónde se concentran los faltantes de las variables de contexto
vars_control <- c("manufacturing_value_added_pct_gdp", "trade_pct_gdp", "internet_users_pct")

faltantes_por_ingreso <- paises %>%
  mutate(algun_faltante = if_any(all_of(vars_control), is.na)) %>%
  group_by(income_level) %>%
  summarise(n_paises = n(),
            con_faltante = sum(algun_faltante),
            pct = round(100 * mean(algun_faltante), 1),
            .groups = "drop")
faltantes_por_ingreso
write_csv(faltantes_por_ingreso, "output/tablas/i2_faltantes_por_ingreso.csv")

# Qué países son:
paises %>%
  filter(if_any(all_of(vars_control), is.na)) %>%
  select(iso3, country_name, income_level, all_of(vars_control))

## 1.3 Variables que NO varían (valen lo mismo en todos los países)
vars_constantes <- paises %>%
  select(where(is.numeric)) %>%
  select(where(~ n_distinct(.x) == 1)) %>%
  names()
vars_constantes
# Son las variables de tipo de tecnología y de implementación
# (llm_dominant_share, turnkey_share, etc.): valen 0 en los 124 países.
# No aportan información -> se excluyen del análisis.

## 1.4 Variables que usaremos en el clustering (HCPC)
vars_hcpc <- c("share_exposed", "mean_task_exposure", "sub_only_share",
               "both_share", "aug_only_share", "task_exposure_sd",
               "share_high_exposure")
# neither_share se deja afuera: las 4 shares (sub, both, aug, neither)
# suman 1, así que una es redundante.
stopifnot(sum(is.na(paises[vars_hcpc])) == 0)   # 0 faltantes en estas variables

# PARA LA DIAPOSITIVA (decisión sobre faltantes):
#  - Panel país-ocupación: 0 faltantes, no se hace nada.
#  - Variables del HCPC: 0 faltantes, no se imputa nada.
#  - Variables de contexto (manufactura, comercio, internet): se mantienen
#    los NA y solo se usan en tests que los admiten (se excluyen caso a caso).
#    No se imputa porque faltan países puntuales (ver tabla 1.2): imputarlos
#    inventaría datos justo en los países con menor información.
#  - Comercio de servicios: se descarta por tener ~76% de faltantes.


# --------------------------------------------------------------
# 2. OUTLIERS
# --------------------------------------------------------------

## 2.1 Panel país-ocupación (ya detectados en la Instancia 1)
out_occ <- read_csv("output/tablas/outliers_exposicion.csv", show_col_types = FALSE)
nrow(out_occ)                               # 42 observaciones
table(out_occ$mean_exposure)                # todas valen exactamente 3 (techo de la escala 0-3)
out_occ %>% count(occupation_title, sort = TRUE)   # casi todas: la misma ocupación
round(100 * nrow(out_occ) / 114452, 3)      # % del total de observaciones

# PARA LA DIAPOSITIVA: criterio = regla IQR (boxplot). Son valores genuinos
# (techo de la escala), concentrados en una ocupación, y correlacionan con el
# PBI del país (r = 0,87, hallazgo de la Instancia 1) -> NO son errores de carga.
# Decisión: se mantienen. Representan 0,04% de las observaciones.

## 2.2 Variables a nivel país
detectar_iqr <- function(x) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  i <- q[2] - q[1]
  x < q[1] - 1.5 * i | x > q[2] + 1.5 * i
}

paises <- paises %>%
  mutate(log_gdp_pc = log(gdp_per_capita),
         log_pob    = log(population))

resumen_outliers <- tibble(
  variable = c("mean_task_exposure", "gdp_per_capita", "log_gdp_pc",
               "population", "log_pob", "trade_pct_gdp",
               "manufacturing_value_added_pct_gdp")
) %>%
  mutate(n_outliers = map_int(variable, ~ sum(detectar_iqr(paises[[.x]]), na.rm = TRUE)))
resumen_outliers
write_csv(resumen_outliers, "output/tablas/i2_outliers_pais.csv")

# Cuáles son los países outlier en PBI per cápita (valores genuinos: países ricos)
paises %>%
  filter(detectar_iqr(gdp_per_capita)) %>%
  select(iso3, country_name, gdp_per_capita) %>%
  arrange(desc(gdp_per_capita))

# PARA ENTENDER: el PBI per cápita tiene cola larga a la derecha (media muy
# por encima de la mediana). En logaritmos la distribución se acomoda.
# Decisión: no se elimina ningún país (son reales); para los tests se usa el
# logaritmo.
p_gdp <- paises %>%
  select(gdp_per_capita, log_gdp_pc) %>%
  pivot_longer(everything(), names_to = "escala", values_to = "valor") %>%
  ggplot(aes(x = "", y = valor)) +
  geom_boxplot(outlier.color = "#E8622C") +
  facet_wrap(~ escala, scales = "free_y") +
  labs(title = "PBI per cápita: escala original vs. logaritmo",
       x = NULL, y = NULL) +
  theme_minimal()
p_gdp
ggsave("output/graficos/i2_boxplot_gdp_original_vs_log.png", p_gdp, width = 8, height = 5)


# --------------------------------------------------------------
# 3. MÉTODO DE INFERENCIA (unidad 2): ANOVA
# --------------------------------------------------------------
# Pregunta: ¿cuánto de la variación de la exposición (mean_exposure) se
# explica por el país y cuánto por el tipo de ocupación?
# H0: la exposición media es la misma en todos los grupos (países, o
#     grupos de ocupaciones).
# H1: al menos un grupo tiene una exposición media distinta.
# El tamaño del efecto es el R² (= eta cuadrado): % de la varianza de la
# exposición que explica cada agrupación. Con 114.452 observaciones el
# p-valor da ~0 siempre; lo que importa es el tamaño del efecto.
# Se leen solo 3 columnas del parquet para no gastar memoria.

occ <- read_parquet("datos/country_occupation_panel.parquet",
                    col_select = c("iso3", "onet_soc_code", "mean_exposure"))
occ <- occ %>%
  mutate(soc_major = str_sub(onet_soc_code, 1, 2),   # grupo mayor SOC (22 grupos)
         iso3      = factor(iso3),
         soc_major = factor(soc_major))
nrow(occ)   # 114452

## 3.1 Descomposición de la varianza (eta cuadrado) por agrupación
# Como cada país tiene exactamente las mismas 923 ocupaciones (diseño
# balanceado), país y ocupación son independientes y la suma de cuadrados
# se puede calcular directo, sin armar un modelo pesado.
stopifnot(length(unique(table(occ$iso3))) == 1)   # balanceado

gm  <- mean(occ$mean_exposure)
sct <- sum((occ$mean_exposure - gm)^2)            # variación total
ss_grupo <- function(g) {
  sum(tapply(occ$mean_exposure, g, function(x) length(x) * (mean(x) - gm)^2))
}

tabla_eta <- tibble(
  fuente = c("Grupo mayor SOC (22)", "País (124)",
             "Ocupación O*NET (923)", "Interacción país x ocupación"),
  eta2   = c(ss_grupo(occ$soc_major) / sct,
             ss_grupo(occ$iso3) / sct,
             ss_grupo(occ$onet_soc_code) / sct,
             NA_real_)
)
tabla_eta$eta2[4] <- 1 - tabla_eta$eta2[2] - tabla_eta$eta2[3]
tabla_eta <- tabla_eta %>% mutate(eta2 = round(eta2, 3))
tabla_eta
write_csv(tabla_eta, "output/tablas/i2_anova_eta2.csv")

# Resultado esperado (verificado): país ~0,44 | ocupación O*NET ~0,49 |
# interacción ~0,07 | grupo mayor SOC ~0,25.
# OJO PARA LA DIAPOSITIVA: "el país explica más que la ocupación" vale
# frente a los 22 grupos SOC (0,25) o 95 subgrupos (0,33), pero con las
# 923 ocupaciones completas la ocupación explica un poco MÁS que el país.
# País + ocupación explican ~93%: no es sobreajuste (son pocos parámetros
# frente a 114 mil observaciones); queda solo ~7% de interacción.

p_eta <- ggplot(tabla_eta, aes(x = reorder(fuente, eta2), y = eta2)) +
  geom_col(fill = "#0B1E3D", width = 0.7) +
  geom_text(aes(label = eta2), hjust = -0.2, color = "#5A6472") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "¿Qué explica la exposición a la automatización?",
       subtitle = "Proporción de la varianza explicada (eta cuadrado)",
       x = NULL, y = NULL) +
  theme_minimal()
p_eta
ggsave("output/graficos/i2_anova_eta2.png", p_eta, width = 9, height = 5)

## 3.2 Verificación de supuestos del ANOVA (factor: país)
# Supuestos: (a) normalidad de los residuos, (b) varianzas iguales entre
# grupos, (c) observaciones independientes.
modelo_pais <- lm(mean_exposure ~ iso3, data = occ)

set.seed(123)
res_muestra <- sample(residuals(modelo_pais), 5000)   # shapiro.test admite hasta 5000
shapiro.test(res_muestra)           # p chico = no normal (con N grande casi siempre)

png("output/graficos/i2_anova_qq_residuos.png", width = 800, height = 600)
qqnorm(res_muestra, main = "QQ-plot de residuos (muestra de 5.000)")
qqline(res_muestra)
dev.off()
# Cómo leerlo: si los puntos siguen la línea, hay normalidad. Desvíos en las
# colas = hay efecto techo/piso (la variable está acotada entre 0 y 3).

desvios <- occ %>% group_by(iso3) %>% summarise(sd = sd(mean_exposure), .groups = "drop")
summary(desvios$sd)
max(desvios$sd) / min(desvios$sd)   # cuántas veces cambia el desvío entre países
fligner.test(mean_exposure ~ iso3, data = occ)   # p chico = varianzas distintas

# (c) Independencia: NO se cumple del todo. Cada ocupación aparece repetida
# en los 124 países (mediciones relacionadas). Es una limitación a declarar.

## 3.3 Qué hacer si no se cumplen: alternativas robustas
welch <- oneway.test(mean_exposure ~ iso3, data = occ, var.equal = FALSE)  # ANOVA de Welch
welch
kw_pais <- kruskal.test(mean_exposure ~ iso3, data = occ)                  # no paramétrico
kw_pais

supuestos <- tibble(
  prueba = c("Shapiro-Wilk (muestra 5000)", "Fligner-Killeen (homogeneidad)",
             "ANOVA de Welch", "Kruskal-Wallis"),
  p_valor = c(shapiro.test(res_muestra)$p.value,
              fligner.test(mean_exposure ~ iso3, data = occ)$p.value,
              welch$p.value, kw_pais$p.value)
)
write_csv(supuestos, "output/tablas/i2_anova_supuestos.csv")

# PARA LA DIAPOSITIVA: pregunta, H0/H1 en palabras, supuestos y qué se hizo
# (Welch y Kruskal-Wallis dan la misma conclusión que el ANOVA clásico),
# eta cuadrado como medida del efecto, e interpretación.
# LIMITACIONES: observaciones no independientes (misma ocupación en 124
# países); variable acotada 0-3 con efecto techo; el país "pesa" en parte
# porque el modelo de IA juzgó cada tarea condicionado al país; no es causal.


# --------------------------------------------------------------
# 4. MÉTODO CUANTITATIVO (unidad 3): HCPC DE PAÍSES
# --------------------------------------------------------------
# PARA ENTENDER: HCPC = Hierarchical Clustering on Principal Components.
#  1) PCA resume las 7 variables (muy correlacionadas entre sí) en pocos
#     "componentes" que capturan casi toda la información.
#  2) Se agrupan los países parecidos en esos componentes (clustering
#     jerárquico) y se corta el árbol (dendrograma) en k grupos.
# Es la sugerencia del docente: clasificar países según sus características
# de automatización.

datos_hcpc <- paises %>%
  select(iso3, all_of(vars_hcpc)) %>%
  as.data.frame() %>%
  column_to_rownames("iso3")

## 4.1 PCA (variables estandarizadas: scale.unit = TRUE)
res_pca <- PCA(datos_hcpc, scale.unit = TRUE, ncp = 3, graph = FALSE)

res_pca$eig   # autovalores y % de varianza explicada por componente
# Se espera que el componente 1 explique ~89% (eje "cuán expuesto es el país")
# y el 2 ~6% (relacionado con aumentación).

write_csv(as.data.frame(res_pca$eig) %>% rownames_to_column("componente"),
          "output/tablas/i2_pca_autovalores.csv")
write_csv(as.data.frame(res_pca$var$coord) %>% rownames_to_column("variable"),
          "output/tablas/i2_pca_cargas.csv")

p_scree <- fviz_screeplot(res_pca, addlabels = TRUE) +
  labs(title = "PCA: % de varianza explicada por componente")
ggsave("output/graficos/i2_pca_scree.png", p_scree, width = 8, height = 5)

p_vars <- fviz_pca_var(res_pca, repel = TRUE) +
  labs(title = "PCA: variables sobre los dos primeros componentes")
ggsave("output/graficos/i2_pca_variables.png", p_vars, width = 8, height = 6)

## 4.2 Clustering jerárquico sobre los componentes
# Con k_hcpc <- -1 el método elige solo la cantidad: con estos datos elige 9
# clusters (uno es South Sudan solo), demasiado fragmentado. Se fija k = 3
# (el salto grande del dendrograma). PRUEBA DE SENSIBILIDAD: probar también 4.
k_hcpc <- 3
res_hcpc <- HCPC(res_pca, nb.clust = k_hcpc, graph = FALSE)

k_elegido <- length(unique(res_hcpc$data.clust$clust))
k_elegido     # cantidad de clusters

p_dend <- fviz_dend(res_hcpc, rect = TRUE, show_labels = FALSE) +
  labs(title = "Dendrograma de países (HCPC)")
ggsave("output/graficos/i2_hcpc_dendrograma.png", p_dend, width = 9, height = 6)

p_clus <- fviz_cluster(res_hcpc, repel = TRUE, show.clust.cent = TRUE, labelsize = 8) +
  labs(title = "Clusters de países sobre los dos primeros componentes")
ggsave("output/graficos/i2_hcpc_clusters.png", p_clus, width = 10, height = 7)

## 4.3 Asignar el cluster a cada país
asignacion <- tibble(iso3 = rownames(res_hcpc$data.clust),
                     cluster = res_hcpc$data.clust$clust)
paises <- paises %>% left_join(asignacion, by = "iso3")

## 4.4 Perfil de cada cluster (medianas)
perfil <- paises %>%
  group_by(cluster) %>%
  summarise(n_paises = n(),
            across(all_of(c(vars_hcpc, "gdp_per_capita", "internet_users_pct")),
                   ~ median(.x, na.rm = TRUE)),
            .groups = "drop")
print(perfil, width = Inf)
write_csv(perfil, "output/tablas/i2_hcpc_perfil_clusters.csv")

write_csv(paises %>% select(iso3, country_name, region, income_level, cluster),
          "output/tablas/i2_paises_por_cluster.csv")

# Dónde caen Argentina, Colombia y otros países de referencia
paises %>%
  filter(iso3 %in% c("ARG", "COL", "BRA", "CHL", "MEX", "USA", "CHN", "IND", "DEU")) %>%
  select(iso3, country_name, cluster, mean_task_exposure, aug_only_share, income_level)

## 4.5 Rangos de exposición por cluster (qué significa cada grupo)
rangos_cluster <- paises %>%
  group_by(cluster) %>%
  summarise(min = min(mean_task_exposure),
            mediana = median(mean_task_exposure),
            max = max(mean_task_exposure), .groups = "drop")
rangos_cluster   # los clusters son, en la práctica, tramos de exposición media
write_csv(rangos_cluster, "output/tablas/i2_hcpc_rangos_exposicion.csv")

## 4.6 PRUEBA DE SENSIBILIDAD: ¿qué pasa con 4 clusters?
res4 <- HCPC(res_pca, nb.clust = 4, graph = FALSE)
cl4 <- tibble(iso3 = rownames(res4$data.clust), cluster_k4 = res4$data.clust$clust)
comp <- paises %>%
  select(iso3, country_name, income_level, cluster_k3 = cluster) %>%
  left_join(cl4, by = "iso3")

tabla_k3_k4 <- table(k3 = comp$cluster_k3, k4 = comp$cluster_k4)
tabla_k3_k4
write_csv(as.data.frame(tabla_k3_k4), "output/tablas/i2_hcpc_sensibilidad_k3_k4.csv")

# Índice de Rand ajustado: 1 = misma partición, 0 = como al azar
ari <- function(a, b) {
  t <- table(a, b); n <- sum(t); s <- function(x) sum(choose(x, 2))
  sa <- s(rowSums(t)); sb <- s(colSums(t)); sij <- s(t); e <- sa * sb / choose(n, 2)
  (sij - e) / ((sa + sb) / 2 - e)
}
ari(comp$cluster_k3, comp$cluster_k4)   # resultado obtenido: 0,64

# Resultado: con k = 4 aparece un grupo de TRANSICIÓN (23 países) entre el medio
# y el alto; 98 de 124 países (79%) mantienen su grupo original.
# PARA LA DIAPOSITIVA: la exposición es un continuo; k = 3 se mantiene por
# simplicidad, reconociendo una zona de frontera.
grupo_transicion <- comp %>% filter(cluster_k4 == 3) %>% arrange(cluster_k3, country_name)
print(grupo_transicion, n = Inf)
write_csv(grupo_transicion, "output/tablas/i2_hcpc_grupo_transicion.csv")
count(grupo_transicion, income_level)   # casi todos de ingreso medio

## 4.7 Países que se salen del patrón ingreso-exposición (k = 3)
excepciones <- paises %>%
  filter((cluster == 3 & income_level == "Lower middle income") |
           (cluster == 2 & income_level == "Upper middle income") |
           (cluster == 2 & income_level == "Low income")) %>%
  select(iso3, country_name, income_level, cluster, mean_task_exposure,
         gdp_per_capita, internet_users_pct, manufacturing_value_added_pct_gdp, trade_pct_gdp) %>%
  arrange(cluster, desc(mean_task_exposure))
print(excepciones, width = Inf)
write_csv(excepciones, "output/tablas/i2_hcpc_excepciones_ingreso.csv")
# 13 excepciones; 8 caen en el grupo de transición (India, Vietnam, Filipinas,
# Jordania, Egipto, Túnez, Argelia, Guatemala). Las genuinas (centro del
# cluster 2): Irak, Turkmenistán, Cuba, Libia, Ruanda.
# Internet, manufactura y comercio NO las explican (ver tabla): queda como
# pregunta abierta. CUIDADO: el Atlas mide exposición factible juzgada por un
# modelo según el contexto del país, no realidad observada.

# PARA LA DIAPOSITIVA: variables usadas y por qué (7 de exposición, sin neither_share
# ni las 8 constantes), estandarizadas; PCA primero; k elegido con el dendrograma
# (salto de inercia); interpretación de cada cluster con la tabla de perfiles.
# LIMITACIONES: el componente 1 absorbe casi todo (las variables están muy
# correlacionadas) -> los clusters ordenan sobre todo por "nivel de exposición";
# sensible a las variables elegidas y a k; clusters de tamaño muy desigual.


# --------------------------------------------------------------
# 5. CONTRASTE (complemento): ¿los clusters coinciden con el ingreso?
# --------------------------------------------------------------
# Une los dos métodos: el ANOVA dijo que el país importa mucho; el HCPC
# agrupó países; ahora se mira si esos grupos se parecen a los niveles de
# ingreso (2da pregunta de la Instancia 1).
# CUIDADO (regla del profesor): el paper de los autores ya reporta la
# asociación de la exposición con ingreso/PBI/internet (Tabla B.1, r=0,914).
# Esto se presenta como CONFIRMACIÓN citando la fuente, no como hallazgo propio.
# Lo propio es el agrupamiento de países y ver qué países se salen del patrón.

## 5.1 Chi-cuadrado de independencia: cluster x nivel de ingreso
# H0: el cluster al que pertenece un país es independiente de su nivel de ingreso.
# H1: hay asociación entre cluster y nivel de ingreso.
base_chi <- paises %>% filter(income_level != "Not classified")   # 2 países sin clasificar
tabla_ci <- table(base_chi$cluster, base_chi$income_level)
tabla_ci

chi_asint <- suppressWarnings(chisq.test(tabla_ci))
chi_asint
round(chi_asint$expected, 1)   # frecuencias esperadas (SUPUESTO: >= 5 en la mayoría)
sum(chi_asint$expected < 5)    # cuántas celdas incumplen
round(chi_asint$stdres, 1)     # residuos estandarizados: dónde está la asociación

# Si hay celdas con esperada < 5, se usa p-valor por simulación (Monte Carlo):
set.seed(123)
chi_sim <- chisq.test(tabla_ci, simulate.p.value = TRUE, B = 10000)
chi_sim

# Tamaño del efecto: V de Cramér (0 = sin asociación; 1 = asociación perfecta)
n_ci <- sum(tabla_ci)
v_cramer <- sqrt(unname(chi_asint$statistic) / (n_ci * (min(dim(tabla_ci)) - 1)))
v_cramer

write_csv(as.data.frame(tabla_ci), "output/tablas/i2_tabla_cluster_ingreso.csv")

## 5.2 Kruskal-Wallis: ¿difieren los clusters en variables que NO se usaron
##     para armarlos? (evita el argumento circular)
# H0: la distribución de la variable es la misma en todos los clusters.
# H1: al menos un cluster difiere de los demás.
paises <- paises %>% mutate(cluster = factor(cluster))

# Cantidad de países por cluster (todos tienen que tener >= 3 con PBI)
paises %>% group_by(cluster) %>%
  summarise(n = n(), con_pbi = sum(is.finite(log_gdp_pc)), .groups = "drop")

# Supuestos (por qué Kruskal-Wallis y no ANOVA): normalidad por grupo
# y homogeneidad de varianzas
paises %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    con_pbi = sum(is.finite(log_gdp_pc)),
    shapiro_p = if (con_pbi >= 3) shapiro.test(log_gdp_pc[is.finite(log_gdp_pc)])$p.value else NA_real_,
    .groups = "drop"
  )
fligner.test(log_gdp_pc ~ cluster, data = paises)   # p chico = varianzas distintas

# Kruskal-Wallis + tamaño del efecto (epsilon cuadrado) para una variable
test_kw <- function(d, variable) {
  d <- d %>% filter(is.finite(.data[[variable]]))
  f <- as.formula(paste(variable, "~ cluster"))
  kw <- kruskal.test(f, data = d)
  tibble(variable  = variable,
         n         = nrow(d),
         H         = unname(kw$statistic),
         gl        = unname(kw$parameter),
         p_valor   = kw$p.value,
         epsilon2  = unname(kw$statistic) / (nrow(d) - 1),   # tamaño del efecto
         fligner_p = fligner.test(f, data = d)$p.value)
}

resultados_kw <- bind_rows(test_kw(paises, "log_gdp_pc"),
                           test_kw(paises, "internet_users_pct"))
print(resultados_kw, width = Inf)
write_csv(resultados_kw, "output/tablas/i2_kruskal_wallis.csv")

# Comparaciones de a pares (post-hoc, corrección de Holm)
pairwise.wilcox.test(paises$log_gdp_pc, paises$cluster,
                     p.adjust.method = "holm", exact = FALSE)

p_kw <- ggplot(paises, aes(x = cluster, y = log_gdp_pc)) +
  geom_boxplot(fill = "#0B1E3D", alpha = 0.85, outlier.color = "#E8622C") +
  labs(title = "PBI per cápita (log) por cluster de exposición",
       x = "Cluster (HCPC)", y = "log(PBI per cápita)") +
  theme_minimal()
p_kw
ggsave("output/graficos/i2_boxplot_pbi_por_cluster.png", p_kw, width = 8, height = 5)

# PARA LA DIAPOSITIVA (interpretación y límites):
#  - p-valor chico = hay asociación, pero NO es causal ni dice por qué.
#  - Mirar el tamaño del efecto (V de Cramér, epsilon2), no solo el p-valor.
#  - Los clusters salen de la exposición, y la exposición correlaciona
#    fuerte con el PBI -> es esperable que se asocien. Lo interesante es
#    ver qué países se salen del patrón (por ejemplo, clusters que
#    mezclan niveles de ingreso).
#  - Muestra chica (124 países) y categorías de ingreso con pocos casos.
#  - Si Fligner da p chico, las varianzas son distintas: Kruskal-Wallis
#    sigue siendo válido, pero se interpreta como diferencia de
#    distribuciones, no solo de medianas.


# --------------------------------------------------------------
# 6. PENDIENTE (ampliación con salarios de ILOSTAT)
# --------------------------------------------------------------
# - El primer modelo de scripts/ILOSTAT.R (log salario ~ exposed + exposed²)
#   fue descartado por el grupo: errores sin clusterizar y colinealidad con
#   calificación. Lautaro tiene una especificación corregida (exposición ~
#   rango salarial dentro del país, con efectos fijos y errores clusterizados).
#   Se corre y valida antes de usarla; si no llega a tiempo, queda como
#   "próximos pasos" en la diapositiva final.
# - Faltantes del cruce: 124 -> 86 países (año >= 2018; se pierden AUS, CAN,
#   JPN, CHN, NZL, entre otros). Cuando estén los CSV de salarios, armar la
#   tabla de países perdidos por nivel de ingreso para la sección 1.

# ==============================================================
# 7. GRÁFICOS PRESENTABLES (pegar AL FINAL del script, después de la sección 5)
# Rehace con más color y orden los gráficos principales. Guarda con los MISMOS
# nombres i2_*.png, así que reemplaza a los anteriores en output/graficos/.
# Necesita los objetos ya creados más arriba: paises (con cluster), tabla_eta,
# occ, modelo_pais, res_muestra, res_pca, res_hcpc, datos_hcpc.
# Solo usa paquetes que ya cargamos (tidyverse, factoextra); ggrepel viene con factoextra.
# ==============================================================
library(ggrepel)
library(grid)

# --- Estilo común ---------------------------------------------
col_cl <- c("1" = "#3B8FC4", "2" = "#F2A83B", "3" = "#D1495B")   # azul = baja, amarillo = media, rojo = alta
nom_cl <- c("1" = "Cluster 1 · exposición baja",
            "2" = "Cluster 2 · media",
            "3" = "Cluster 3 · alta")
col_ing <- c("Low income" = "#8E5BA8", "Lower middle income" = "#3B8FC4",
             "Upper middle income" = "#F2A83B", "High income" = "#2E9E6F")
nom_ing <- c("Low income" = "Bajo", "Lower middle income" = "Medio-bajo",
             "Upper middle income" = "Medio-alto", "High income" = "Alto")
azul_osc <- "#1F3A5F"

theme_i2 <- function(base = 12) {
  theme_minimal(base_size = base) +
    theme(plot.title      = element_text(face = "bold", size = base + 2),
          plot.subtitle   = element_text(color = "grey35"),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.title      = element_text(color = "grey25"),
          legend.position = "top")
}

paises <- paises %>% mutate(cluster = factor(cluster))

# --- 6.1 PBI per cápita: original vs. logaritmo, con atípicos en rojo ---
base_gdp <- paises %>%
  filter(!is.na(gdp_per_capita)) %>%
  select(iso3, country_name, `PBI per cápita (US$)` = gdp_per_capita,
         `log(PBI per cápita)` = log_gdp_pc) %>%
  pivot_longer(c(`PBI per cápita (US$)`, `log(PBI per cápita)`),
               names_to = "escala", values_to = "valor") %>%
  group_by(escala) %>%
  mutate(atipico = detectar_iqr(valor)) %>%
  ungroup() %>%
  mutate(escala = factor(escala, levels = c("PBI per cápita (US$)", "log(PBI per cápita)")))

etq_gdp <- base_gdp %>%
  filter(atipico, escala == "PBI per cápita (US$)") %>%
  slice_max(valor, n = 4)

p_gdp2 <- ggplot(base_gdp, aes(x = "", y = valor)) +
  geom_boxplot(aes(fill = escala), width = 0.45, alpha = 0.55,
               outlier.shape = NA, color = azul_osc) +
  geom_jitter(aes(color = atipico, size = atipico), width = 0.12, alpha = 0.7) +
  geom_text_repel(data = etq_gdp, aes(label = country_name), nudge_x = 0.35,
                  size = 3, segment.color = "grey60") +
  facet_wrap(~ escala, scales = "free_y") +
  scale_fill_manual(values = c("#F2A83B", "#3B8FC4"), guide = "none") +
  scale_color_manual(values = c("FALSE" = azul_osc, "TRUE" = "#D1495B"), guide = "none") +
  scale_size_manual(values = c("FALSE" = 1.6, "TRUE" = 2.8), guide = "none") +
  labs(title = "Pasar a logaritmo elimina los atípicos del PBI per cápita",
       subtitle = "Puntos rojos = atípicos según la regla IQR (9 en escala original, 0 en logaritmo)",
       x = NULL, y = NULL) +
  theme_i2() +
  theme(strip.text = element_text(face = "bold"))
p_gdp2
ggsave("output/graficos/i2_boxplot_gdp_original_vs_log.png", p_gdp2,
       width = 9, height = 5, dpi = 300, bg = "white")

# --- 6.2 Eta cuadrado ------------------------------------------
tabla_eta2 <- tabla_eta %>%
  mutate(fuente = factor(fuente, levels = rev(fuente)),
         etiqueta = paste0(format(eta2, nsmall = 3), "  (", round(eta2 * 100, 1), "%)"))

p_eta2 <- ggplot(tabla_eta2, aes(x = fuente, y = eta2, fill = fuente)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = etiqueta), hjust = -0.08, fontface = "bold", size = 4) +
  coord_flip() +
  scale_fill_manual(values = setNames(c("#F2A83B", "#1F3A5F", "#3B8FC4", "#8FB8DE"),
                                      levels(tabla_eta2$fuente)), guide = "none") +
  scale_y_continuous(limits = c(0, 0.62), expand = c(0, 0)) +
  labs(title = "¿Qué explica la variación en la exposición?",
       subtitle = "η²: proporción de la varianza de mean_exposure explicada por cada agrupación",
       x = NULL, y = NULL) +
  theme_i2()
p_eta2
ggsave("output/graficos/i2_anova_eta2.png", p_eta2,
       width = 9, height = 4.2, dpi = 300, bg = "white")

# --- 6.3 Residuos: QQ-plot + histograma, lado a lado -----------
dat_res <- tibble(res = as.numeric(res_muestra))

p_qq <- ggplot(dat_res, aes(sample = res)) +
  stat_qq(color = "#3B8FC4", alpha = 0.5, size = 1) +
  stat_qq_line(color = "#D1495B", linewidth = 1) +
  labs(title = "QQ-plot de residuos", x = "Cuantiles teóricos (normal)",
       y = "Cuantiles de los residuos") +
  theme_i2(11)

p_hist <- ggplot(dat_res, aes(x = res)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60,
                 fill = "#3B8FC4", color = "white", alpha = 0.85) +
  stat_function(fun = dnorm, args = list(mean = mean(dat_res$res), sd = sd(dat_res$res)),
                color = "#D1495B", linewidth = 1) +
  labs(title = "Histograma de residuos",
       x = "Residuo (exposición − media del país)", y = NULL) +
  theme_i2(11)

png("output/graficos/i2_anova_qq_residuos.png", width = 3000, height = 1250, res = 300)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(p_qq,   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p_hist, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

# --- 6.4 Dendrograma con ramas de color + altura de las últimas fusiones ---
arbol  <- res_hcpc$call$t$tree                    # hclust que usa HCPC
corte  <- cutree(arbol, k = k_hcpc)               # cluster según el corte (antes de consolidar)
orden  <- unique(corte[arbol$order])              # orden en que aparecen en el dibujo
dom    <- sapply(orden, function(g) {             # cluster final dominante de cada rama
  ids <- names(corte)[corte == g]
  names(which.max(table(paises$cluster[match(ids, paises$iso3)])))
})
h_corte <- mean(sort(arbol$height, decreasing = TRUE)[k_hcpc - 1:0])

p_dend2 <- fviz_dend(arbol, k = k_hcpc, k_colors = unname(col_cl[dom]),
                     show_labels = FALSE, rect = FALSE, lwd = 0.7,
                     ggtheme = theme_i2()) +
  geom_hline(yintercept = h_corte, linetype = "dashed", color = azul_osc) +
  labs(title = "Dendrograma de países (HCPC)",
       subtitle = "Las ramas se colorean según el corte en k = 3; cuanto más arriba se unen, más distintos son los grupos",
       x = NULL, y = "Altura de fusión")
p_dend2
ggsave("output/graficos/i2_hcpc_dendrograma.png", p_dend2,
       width = 10, height = 5.5, dpi = 300, bg = "white")

# Altura de las últimas fusiones (para justificar el k elegido)
alturas <- tibble(altura = head(sort(arbol$height, decreasing = TRUE), 8)) %>%
  mutate(fusion = factor(paste(row_number() + 1, "→", row_number()),
                         levels = rev(paste(seq_len(8) + 1, "→", seq_len(8)))),
         destacado = row_number() <= 2)
p_alt <- ggplot(alturas, aes(x = fusion, y = altura, fill = destacado)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(altura, 1)), hjust = -0.15, size = 3.5) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = azul_osc, "FALSE" = "#B7BDC7"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Las últimas 8 fusiones",
       subtitle = "Dos saltos grandes y después una caída suave: justifica k = 3",
       x = "Clusters (antes → después)", y = "Altura de la fusión") +
  theme_i2()
p_alt
ggsave("output/graficos/i2_hcpc_alturas_fusion.png", p_alt,
       width = 6, height = 4, dpi = 300, bg = "white")

# --- 6.5 Clusters sobre los dos primeros componentes -----------
coord <- as.data.frame(res_pca$ind$coord[, 1:2]) %>%
  rownames_to_column("iso3") %>%
  rename(dim1 = Dim.1, dim2 = Dim.2) %>%
  left_join(paises %>% select(iso3, country_name, cluster), by = "iso3")

# El signo de los ejes del PCA es arbitrario: lo ordenamos para que
# a la derecha esté la exposición alta y arriba la aumentación pura.
if (cor(coord$dim1, datos_hcpc[coord$iso3, "mean_task_exposure"]) < 0) coord$dim1 <- -coord$dim1
if (cor(coord$dim2, datos_hcpc[coord$iso3, "aug_only_share"]) < 0)     coord$dim2 <- -coord$dim2

eig <- res_pca$eig[, 2]   # % de varianza por componente
paises_etq <- c("SSD", "COL", "IND", "USA", "CHN", "DEU")

p_clus2 <- ggplot(coord, aes(dim1, dim2, color = cluster, fill = cluster)) +
  geom_hline(yintercept = 0, color = "grey85") +
  geom_vline(xintercept = 0, color = "grey85") +
  stat_ellipse(geom = "polygon", level = 0.9, alpha = 0.12, linewidth = 0.6) +
  geom_point(size = 2.6, shape = 21, color = "white", stroke = 0.6) +
  geom_text_repel(data = filter(coord, iso3 %in% paises_etq),
                  aes(label = country_name), color = "grey15", size = 3.3,
                  min.segment.length = 0, box.padding = 0.6, max.overlaps = Inf) +
  scale_fill_manual(values = col_cl, labels = nom_cl, name = NULL) +
  scale_color_manual(values = col_cl, labels = nom_cl, name = NULL) +
  labs(title = "Países en los dos primeros componentes, por cluster",
       x = paste0("Dim 1 · \"cuán expuesto es el país\" (", round(eig[1], 1), "%)"),
       y = paste0("Dim 2 · aumentación pura (+) / tareas muy expuestas (−) (",
                  round(eig[2], 1), "%)")) +
  theme_i2() +
  theme(panel.grid.major.x = element_line(color = "grey93"))
p_clus2
ggsave("output/graficos/i2_hcpc_clusters.png", p_clus2,
       width = 10, height = 7, dpi = 300, bg = "white")

# --- 6.6 PBI por cluster, puntos coloreados por nivel de ingreso ---
base_pbi <- paises %>% filter(!is.na(log_gdp_pc), !is.na(income_level))

p_kw2 <- ggplot(base_pbi, aes(x = cluster, y = log_gdp_pc)) +
  geom_boxplot(aes(fill = cluster), width = 0.55, alpha = 0.35,
               outlier.shape = NA, color = azul_osc) +
  geom_jitter(aes(color = income_level), width = 0.18, size = 2.4, alpha = 0.95) +
  scale_fill_manual(values = col_cl, guide = "none") +
  scale_color_manual(values = col_ing, labels = nom_ing, name = "Nivel de ingreso") +
  scale_x_discrete(labels = function(x) paste0(nom_cl[x], "\n(n = ",
                                               table(paises$cluster)[x], ")")) +
  labs(title = "Los clusters de exposición siguen el nivel de ingreso",
       x = NULL, y = "log(PBI per cápita)") +
  theme_i2() +
  theme(legend.position = "top")
p_kw2
ggsave("output/graficos/i2_boxplot_pbi_por_cluster.png", p_kw2,
       width = 9, height = 6, dpi = 300, bg = "white")

