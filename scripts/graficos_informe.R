# ==============================================================
# GRÁFICOS DEL INFORME (PDF de la Instancia 2) - versión en R
# TP Ciencia de Datos FCE-UBA - Grupo 16 - Valery
#
# Script AUTOCONTENIDO: lee solo de datos/, recalcula lo que necesita
# (ANOVA por sumas de cuadrados, PCA, HCPC) y guarda los 6 gráficos del
# informe en output/graficos_informe/ (carpeta nueva: no pisa nada).
#
# Cómo correrlo: abrir el proyecto (.Rproj) o fijar
# Session > Set Working Directory > To Project Directory, y correr de
# arriba hacia abajo con Ctrl+Enter (o Source).
# ==============================================================

stopifnot("Falta fijar el directorio del proyecto" =
            file.exists("datos/country_map_panel.csv"))

paquetes <- c("tidyverse", "arrow", "FactoMineR", "factoextra", "ggrepel")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)

library(tidyverse)
library(arrow)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(grid)

dir.create("output/graficos_informe", showWarnings = FALSE, recursive = TRUE)
guardar <- function(nombre) paste0("output/graficos_informe/", nombre)

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
    theme(plot.title         = element_text(face = "bold", size = base + 2),
          plot.subtitle      = element_text(color = "grey35"),
          panel.grid.minor   = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.title         = element_text(color = "grey25"),
          legend.position    = "top")
}

detectar_iqr <- function(x) {
  q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  i <- q[2] - q[1]
  x < q[1] - 1.5 * i | x > q[2] + 1.5 * i
}

# ==============================================================
# A. DATOS Y CÁLCULOS BASE
# ==============================================================
paises <- read_csv("datos/country_map_panel.csv", show_col_types = FALSE) %>%
  mutate(log_gdp_pc = log(gdp_per_capita))

vars_hcpc <- c("share_exposed", "mean_task_exposure", "sub_only_share",
               "both_share", "aug_only_share", "task_exposure_sd",
               "share_high_exposure")
stopifnot(sum(is.na(paises[vars_hcpc])) == 0)

datos_hcpc <- paises %>%
  select(iso3, all_of(vars_hcpc)) %>%
  as.data.frame() %>%
  column_to_rownames("iso3")

# PCA + HCPC (k = 3, igual que en el script principal)
k_hcpc   <- 3
res_pca  <- PCA(datos_hcpc, scale.unit = TRUE, ncp = 3, graph = FALSE)
res_hcpc <- HCPC(res_pca, nb.clust = k_hcpc, graph = FALSE)

# Cluster por país, numerado de menor a mayor exposición media
# (así 1 = baja, 2 = media, 3 = alta, sin depender del orden interno de HCPC)
asig <- tibble(iso3 = rownames(res_hcpc$data.clust),
               cl_orig = as.character(res_hcpc$data.clust$clust)) %>%
  left_join(paises %>% select(iso3, mean_task_exposure), by = "iso3")
orden_cl <- asig %>% group_by(cl_orig) %>%
  summarise(m = mean(mean_task_exposure), .groups = "drop") %>%
  arrange(m) %>% mutate(cluster = as.character(row_number()))
asig <- asig %>% left_join(orden_cl %>% select(cl_orig, cluster), by = "cl_orig")
paises <- paises %>%
  left_join(asig %>% select(iso3, cluster), by = "iso3") %>%
  mutate(cluster = factor(cluster, levels = c("1", "2", "3")))
print(table(paises$cluster))   # esperado: 23 / 33 / 68

# Panel país x ocupación (solo 3 columnas, para no gastar memoria)
occ <- read_parquet("datos/country_occupation_panel.parquet",
                    col_select = c("iso3", "onet_soc_code", "mean_exposure")) %>%
  mutate(soc_major = factor(str_sub(onet_soc_code, 1, 2)),
         iso3      = factor(iso3))
stopifnot(length(unique(table(occ$iso3))) == 1)   # diseño balanceado

# eta cuadrado por sumas de cuadrados (sin armar un modelo pesado)
gm  <- mean(occ$mean_exposure)
sct <- sum((occ$mean_exposure - gm)^2)
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
print(tabla_eta)   # esperado: 0.253 | 0.439 | 0.488 | 0.073

# Residuos del modelo "exposición ~ país" (media por país, sin lm pesado)
res_pais <- occ$mean_exposure - ave(occ$mean_exposure, occ$iso3)
set.seed(123)
res_muestra <- sample(res_pais, 5000)

# ==============================================================
# FIGURA 1. PBI per cápita: original vs. logaritmo
# ==============================================================
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

fig1 <- ggplot(base_gdp, aes(x = "", y = valor)) +
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
fig1
ggsave(guardar("fig1_pbi_original_vs_log.png"), fig1,
       width = 9, height = 5, dpi = 300, bg = "white")

# ==============================================================
# FIGURA 2. Eta cuadrado
# ==============================================================
tabla_eta2 <- tabla_eta %>%
  mutate(fuente = factor(fuente, levels = rev(fuente)),
         etiqueta = paste0(format(eta2, nsmall = 3), "  (", round(eta2 * 100, 1), "%)"))

fig2 <- ggplot(tabla_eta2, aes(x = fuente, y = eta2, fill = fuente)) +
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
fig2
ggsave(guardar("fig2_eta2.png"), fig2, width = 9, height = 4.2, dpi = 300, bg = "white")

# ==============================================================
# FIGURA 3. Residuos: QQ-plot + histograma
# ==============================================================
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

png(guardar("fig3_residuos_qq_hist.png"), width = 3000, height = 1250, res = 300)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(p_qq,   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p_hist, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

# ==============================================================
# FIGURA 4. Dendrograma + franja de clusters finales + últimas fusiones
# ==============================================================
arbol <- res_hcpc$call$t$tree                 # hclust que usa HCPC
n_p   <- length(arbol$order)
corte <- cutree(arbol, k = k_hcpc)            # corte del árbol (antes de consolidar)
orden <- unique(corte[arbol$order])           # orden de aparición en el dibujo
dom   <- sapply(orden, function(g) {          # cluster final dominante de cada rama
  ids <- names(corte)[corte == g]
  names(which.max(table(paises$cluster[match(ids, paises$iso3)])))
})
h_corte <- mean(sort(arbol$height, decreasing = TRUE)[k_hcpc - 1:0])

p_dend <- fviz_dend(arbol, k = k_hcpc, k_colors = unname(col_cl[dom]),
                    show_labels = FALSE, rect = FALSE, lwd = 0.7,
                    ggtheme = theme_i2()) +
  geom_hline(yintercept = h_corte, linetype = "dashed", color = azul_osc) +
  labs(title = "Dendrograma de países (HCPC)",
       subtitle = "Ramas coloreadas según el corte en k = 3",
       x = NULL, y = "Altura de fusión")

# franja: cluster final de cada país, en el orden de las hojas
franja <- tibble(pos = seq_len(n_p),
                 iso3 = arbol$labels[arbol$order]) %>%
  left_join(paises %>% select(iso3, cluster), by = "iso3")
p_franja <- ggplot(franja, aes(x = pos, y = 1, fill = cluster)) +
  geom_tile(height = 1) +
  scale_fill_manual(values = col_cl, labels = nom_cl, name = NULL) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(caption = "Franja: cluster final de cada país (tras la consolidación k-means de HCPC)") +
  theme_void() +
  theme(legend.position = "bottom", plot.caption = element_text(size = 8, color = "grey35"))

alturas <- tibble(altura = head(sort(arbol$height, decreasing = TRUE), 8)) %>%
  mutate(fusion = factor(paste(row_number() + 1, "→", row_number()),
                         levels = rev(paste(seq_len(8) + 1, "→", seq_len(8)))),
         destacado = row_number() <= 2)
p_alt <- ggplot(alturas, aes(x = fusion, y = altura, fill = destacado)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(altura, 1)), hjust = -0.15, size = 3.2) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = azul_osc, "FALSE" = "#B7BDC7"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Las últimas 8 fusiones", x = NULL, y = "Altura de la fusión") +
  theme_i2(10)

png(guardar("fig4_dendrograma.png"), width = 3300, height = 1750, res = 300)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2, widths = unit(c(4.2, 1.4), "null"),
                                           heights = unit(c(10, 1.6), "null"))))
print(p_dend,   vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p_franja, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(p_alt,    vp = viewport(layout.pos.row = 1:2, layout.pos.col = 2))
dev.off()

# ==============================================================
# FIGURA 5. Clusters sobre los dos primeros componentes
# ==============================================================
coord <- as.data.frame(res_pca$ind$coord[, 1:2]) %>%
  rownames_to_column("iso3") %>%
  rename(dim1 = Dim.1, dim2 = Dim.2) %>%
  left_join(paises %>% select(iso3, country_name, cluster), by = "iso3")

# El signo de los ejes del PCA es arbitrario: se ordena para que a la derecha
# esté la exposición alta y arriba la aumentación pura.
if (cor(coord$dim1, datos_hcpc[coord$iso3, "mean_task_exposure"]) < 0) coord$dim1 <- -coord$dim1
if (cor(coord$dim2, datos_hcpc[coord$iso3, "aug_only_share"]) < 0)     coord$dim2 <- -coord$dim2

eig <- res_pca$eig[, 2]   # % de varianza por componente
paises_etq <- c("SSD", "COL", "IND", "USA", "CHN", "DEU")

fig5 <- ggplot(coord, aes(dim1, dim2, color = cluster, fill = cluster)) +
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
fig5
ggsave(guardar("fig5_clusters_pca.png"), fig5, width = 10, height = 7, dpi = 300, bg = "white")

# ==============================================================
# FIGURA 6. PBI por cluster, puntos coloreados por nivel de ingreso
# ==============================================================
base_pbi <- paises %>% filter(!is.na(log_gdp_pc), !is.na(income_level))
n_cl <- table(paises$cluster)

fig6 <- ggplot(base_pbi, aes(x = cluster, y = log_gdp_pc)) +
  geom_boxplot(aes(fill = cluster), width = 0.55, alpha = 0.35,
               outlier.shape = NA, color = azul_osc) +
  geom_jitter(aes(color = income_level), width = 0.18, size = 2.4, alpha = 0.95) +
  scale_fill_manual(values = col_cl, guide = "none") +
  scale_color_manual(values = col_ing, labels = nom_ing, name = "Nivel de ingreso") +
  scale_x_discrete(labels = function(x) paste0(nom_cl[x], "\n(n = ", n_cl[x], ")")) +
  labs(title = "Los clusters de exposición siguen el nivel de ingreso",
       x = NULL, y = "log(PBI per cápita)") +
  theme_i2()
fig6
ggsave(guardar("fig6_pbi_por_cluster.png"), fig6, width = 9, height = 6, dpi = 300, bg = "white")

message("Listo: 6 gráficos en output/graficos_informe/")
