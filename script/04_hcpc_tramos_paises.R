# ==============================================================
# 04 - Método 2: HCPC (tramos de países por perfil de exposición)
# ACP sobre 7 variables, Ward + k-means, k = 3 (partición 23 / 33 / 68 con FactoMineR 2.9).
# Lee:   input/paises_analisis.csv
# Guarda: input/paises_tramos.csv; output/tablas/robustez_ari.csv, composicion_vs_nivel.csv, hcpc_sensibilidad_k.csv;
#         output/graficos/kdendro.png, mapa.png, fronteras.png, robust.png, compos.png
# ==============================================================
library(here)
library(tidyverse)
library(FactoMineR)
library(ggrepel)
library(cluster)
library(patchwork)
library(scales)

# Funciones propias y configuración (utils/)
source(here("utils", "configuracion_graficos.R"))
source(here("utils", "coma.R"))
source(here("utils", "theme_slide.R"))
source(here("utils", "theme_panel.R"))
source(here("utils", "unir.R"))
source(here("utils", "guardar_png.R"))
source(here("utils", "ari.R"))
source(here("utils", "ordenar_clusters.R"))
source(here("utils", "nombre_es.R"))
source(here("utils", "pca_exacta.R"))

vars_hcpc <- c("share_exposed", "mean_task_exposure", "sub_only_share",
               "both_share", "aug_only_share", "task_exposure_sd", "share_high_exposure")

# ---- Entradas ----
paises <- read_csv(here("input", "paises_analisis.csv"), show_col_types = FALSE)
stopifnot(nrow(paises) == 124, sum(is.na(paises[vars_hcpc])) == 0)

# ---------------- D. HCPC de países (k = 3) ----------------
datos_hcpc <- paises %>% select(iso3, all_of(vars_hcpc)) %>%
  as.data.frame() %>% column_to_rownames("iso3")
res_pca  <- pca_exacta(datos_hcpc, ncp = 3)   # SVD exacta: mismo resultado en cualquier versión de FactoMineR
res_hcpc <- HCPC(res_pca, nb.clust = 3, graph = FALSE)
paises <- paises %>%
  left_join(tibble(iso3 = rownames(res_hcpc$data.clust), cl_raw = res_hcpc$data.clust$clust), by = "iso3") %>%
  mutate(tramo = ordenar_clusters(cl_raw, mean_task_exposure))
stopifnot("La partición no es 23 / 33 / 68: revisar datos y versión de FactoMineR (2.9)" =
            all(as.vector(table(paises$tramo)) == c(23, 33, 68)))

coord_pca <- res_pca$ind$coord
d_pca     <- dist(coord_pca)
cl_orden  <- paises$tramo[match(rownames(coord_pca), paises$iso3)]
eig       <- res_pca$eig[, 2]

coord <- as.data.frame(coord_pca[, 1:2]) %>% rownames_to_column("iso3") %>% rename(dim1 = Dim.1, dim2 = Dim.2)
if (cor(coord$dim1, paises$mean_task_exposure[match(coord$iso3, paises$iso3)]) < 0) coord$dim1 <- -coord$dim1
if (cor(coord$dim2, paises$aug_only_share[match(coord$iso3, paises$iso3)]) < 0)     coord$dim2 <- -coord$dim2
coord <- coord %>% left_join(select(paises, iso3, country_name, tramo), by = "iso3") %>%
  mutate(etq = nombre_es(iso3, country_name))

# Silueta por k, por país y grupo de transición (k = 4)
sil_k <- map_dfr(2:6, function(k) {
  cl_k <- HCPC(res_pca, nb.clust = k, graph = FALSE)$data.clust$clust
  tibble(k = k, silueta_media = mean(silhouette(as.integer(cl_k), d_pca)[, "sil_width"]))
})
sil <- silhouette(as.integer(cl_orden), d_pca)
coord$silueta <- sil[match(coord$iso3, rownames(coord_pca)), "sil_width"]
cl4 <- HCPC(res_pca, nb.clust = 4, graph = FALSE)$data.clust$clust
tab43 <- table(cl4, cl_orden)
id_trans <- names(which.min(apply(tab43, 1, max) / rowSums(tab43)))
coord$transicion <- coord$iso3 %in% rownames(coord_pca)[as.character(cl4) == id_trans]
stopifnot(sum(coord$transicion) == 23)

# Dos ramas desde el tramo medio (k-lines)
d_ramas <- coord %>% filter(tramo != "1")
rama <- ifelse(d_ramas$dim2 > 0.25, "arriba", "abajo")
for (it in 1:100) {
  m_arr <- lm(dim2 ~ dim1, data = d_ramas[rama == "arriba", ])
  m_aba <- lm(dim2 ~ dim1, data = d_ramas[rama == "abajo", ])
  nueva <- ifelse(abs(d_ramas$dim2 - predict(m_arr, d_ramas)) < abs(d_ramas$dim2 - predict(m_aba, d_ramas)), "arriba", "abajo")
  if (all(nueva == rama)) break
  rama <- nueva
}
d_ramas$rama <- rama
stopifnot(sum(rama == "arriba") == 31)
lineas <- d_ramas %>% group_by(rama) %>% summarise(x0 = min(dim1), x1 = max(dim1), .groups = "drop") %>%
  mutate(y0 = if_else(rama == "arriba", predict(m_arr, tibble(dim1 = x0)), predict(m_aba, tibble(dim1 = x0))),
         y1 = if_else(rama == "arriba", predict(m_arr, tibble(dim1 = x1)), predict(m_aba, tibble(dim1 = x1))))

# ---------------- E. HCPC por composición del margen ----------------
comp <- paises %>%
  mutate(l_sub = log(sub_rel), l_both = log(both_rel), l_aug = log(aug_rel),
         centro = (l_sub + l_both + l_aug) / 3,
         clr_sub = l_sub - centro, clr_both = l_both - centro, clr_aug = l_aug - centro)
datos_comp <- comp %>% select(iso3, clr_sub, clr_both, clr_aug, int_rel) %>%
  as.data.frame() %>% column_to_rownames("iso3")
res_pca_c <- pca_exacta(datos_comp, ncp = 3)
hc_c <- HCPC(res_pca_c, nb.clust = 3, graph = FALSE)
comp <- comp %>%
  left_join(tibble(iso3 = rownames(hc_c$data.clust), cl_raw_c = hc_c$data.clust$clust), by = "iso3") %>%
  mutate(cluster_comp = ordenar_clusters(cl_raw_c, mean_task_exposure))
stopifnot(which.max(tapply(comp$aug_rel, comp$cluster_comp, median)) == 2,
          which.max(tapply(comp$int_rel, comp$cluster_comp, median)) == 3)
ari_comp_nivel <- ari(comp$cluster_comp, paises$tramo[match(comp$iso3, paises$iso3)])

# ---------------- G. Robustez del HCPC a las variables elegidas ----------------
hc_cons <- function(vars, k = 3) {
  Z <- scale(paises[, vars]); cl_ <- cutree(hclust(dist(Z), method = "ward.D2"), k)
  centros <- rowsum(Z, cl_) / as.numeric(table(cl_)); kmeans(Z, centers = centros)$cluster
}
robustez <- tibble(
  variables = c("Solo exposición media", "Exposición media + desvío + tareas muy expuestas",
                "Solo composición (sust., ambas, aum.)"),
  ari = c(ari(paises$tramo, hc_cons("mean_task_exposure")),
          ari(paises$tramo, hc_cons(c("mean_task_exposure", "task_exposure_sd", "share_high_exposure"))),
          ari(paises$tramo, hc_cons(c("sub_only_share", "both_share", "aug_only_share")))))


# ---- Guardar resultados para los scripts siguientes (05 a 08) ----
tramos_out <- paises %>% select(iso3, tramo) %>%
  left_join(select(comp, iso3, cluster_comp), by = "iso3") %>%
  left_join(select(coord, iso3, dim1, dim2, silueta, transicion), by = "iso3") %>%
  left_join(select(d_ramas, iso3, rama), by = "iso3")
write_csv(tramos_out, here("input", "paises_tramos.csv"))
write_csv(robustez, here("output", "tablas", "robustez_ari.csv"))

# ---- Elección de k (diapositiva 12) ----
arbol <- res_hcpc$call$t$tree
alturas <- tibble(altura = head(sort(arbol$height, decreasing = TRUE), 8)) %>%
  mutate(fusion = factor(paste0("De ", row_number() + 1, " a ", row_number()),
                         levels = rev(paste0("De ", seq_len(8) + 1, " a ", seq_len(8)))),
         destacado = row_number() <= 2)
p_alt <- ggplot(alturas, aes(altura, fusion, fill = destacado)) +
  geom_col(width = 0.7, color = color_fondo_claro, linewidth = 0.8) +
  geom_text(aes(label = coma(altura)), hjust = -0.15, color = gris_tx, size = 5.5, family = fam, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = navy, "FALSE" = color_gris), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Altura de las últimas fusiones del árbol", x = "Altura de fusión", y = NULL) +
  theme_panel() + theme(panel.grid.major.y = element_blank())
p_sil <- ggplot(sil_k, aes(k, silueta_media)) +
  geom_line(color = navy, linewidth = 1.2) + geom_point(aes(color = k == 3), size = 5) +
  geom_text(aes(label = coma(silueta_media), vjust = ifelse(k == 4, 2.2, -1.3)),
            color = navy, family = fam, fontface = "bold", size = 6) +
  scale_color_manual(values = c("TRUE" = naranja, "FALSE" = navy), guide = "none") +
  scale_x_continuous(breaks = 2:6) + scale_y_continuous(limits = c(0.4, 0.68)) +
  labs(title = "Silueta media según k", x = "Cantidad de clusters (k)", y = NULL) + coord_cartesian(clip = "off") + theme_panel()
guardar_png(unir(p_alt + p_sil + plot_layout(widths = c(1.2, 1))), "kdendro.png", ancho = 8.1, alto = 4.88, k = 1.25,
            titulo = "Elección del número de grupos de países (HCPC)",
            subtitulo = "Izquierda: altura de las últimas 8 fusiones del árbol de Ward (las dos mayores, destacadas). Derecha: silueta media según k; k = 3 en naranja.",
            nota = paste0("HCPC sobre las componentes principales de 7 variables de exposición, 124 países. Elegimos k = 3 (silueta media ",
                          coma(sil_k$silueta_media[sil_k$k == 3]), "); k = 2 tiene mayor silueta (", coma(sil_k$silueta_media[sil_k$k == 2]), ")."))
print(sil_k)

# ---- Mapa de los países (diapositiva 13; gráfico de la instancia 3) ----
etq_front <- coord %>% filter(silueta < 0.25)                       # 15 países de frontera
etq_ref   <- coord %>% filter(iso3 %in% c("SSD", "ARG", "KOR", "USA", "CHN"))
stopifnot(nrow(etq_front) == 15)
# Rótulos del mapa: cuatro casos de referencia, con posición manual (lx, ly)
# para que ninguno tape un punto; cada rótulo se une a su país con una línea fina.
pos_map <- tibble::tribble(
  ~iso3, ~lx,  ~ly,
  "SSD", -5.6, -1.5, "ETH", -4.8, -1.1, "TZA", -3.7, -1.1, "CMR", -2.5, -1.0,
  "NPL", -0.9, -0.75, "TGO", -4.3,  0.62, "LAO", -3.3,  0.62, "BEN", -3.3,  1.05,
  "LKA", -1.3,  1.5,  "PRY",  0.25, 1.5,  "DOM",  1.75, 1.72, "SLV",  1.6,  0.6,
  "MAR",  0.0,  0.4,  "EGY",  1.4,  0.1, "PER",  0.3, -0.62, "IRN",  0.5, -0.95,
  "ARG",  2.65, 0.12,  "KOR",  3.7, -0.35, "USA",  4.35, -1.4, "CHN",  3.9, -1.7,
  "NER", -4.6, -1.0, "SEN", -2.6, 1.45, "NGA", -2.2, -1.0)
etq_map <- inner_join(filter(coord, iso3 %in% c("ARG", "USA", "KOR", "DOM", "NER", "SEN", "NGA")), pos_map, by = "iso3")   # casos destacados
stopifnot(nrow(etq_map) == 7)
# La línea termina en el borde del rótulo (no en su centro): caja aproximada según la cantidad de letras
etq_map <- etq_map %>% mutate(
  dx = dim1 - lx, dy = dim2 - ly,
  hw = 0.127 * nchar(etq) / 2 + 0.04, hh = 0.085 + 0.03,
  tt = pmin(1, pmin(hw / pmax(abs(dx), 1e-9), hh / pmax(abs(dy), 1e-9))),
  ex = lx + dx * tt, ey = ly + dy * tt)
lab_tr <- setNames(paste0(nom_nivel, " (", table(coord$tramo), " países)"), names(nom_nivel))
xa <- lineas$x0[lineas$rama == "abajo"]

p <- ggplot(coord, aes(dim1, dim2)) +
  geom_hline(yintercept = 0, color = grilla, linewidth = 0.4) +
  geom_vline(xintercept = 0, color = grilla, linewidth = 0.4) +
  stat_ellipse(aes(color = tramo, fill = tramo), geom = "polygon", level = 0.9,
               alpha = 0.07, linewidth = 0.3, show.legend = FALSE) +
  geom_segment(data = lineas, aes(x = x0, y = y0, xend = x1, yend = y1), color = "black", linewidth = 0.45) +
  annotate("text", x = lineas$x1[lineas$rama == "arriba"] - 0.15, y = lineas$y1[lineas$rama == "arriba"] + 0.5,
           label = paste0("Rama de aumentación\n31 países · pendiente ", coma(coef(m_arr)[2], signo = TRUE)),
           hjust = 1, vjust = 0, size = 3.0, family = fam, fontface = "bold", lineheight = 0.9) +
  annotate("text", x = xa + 0.6, y = -1.3,
           label = paste0("Rama de exposición intensa\n70 países · pendiente ", coma(coef(m_aba)[2], signo = TRUE)),
           hjust = 0, vjust = 1, size = 3.0, family = fam, fontface = "bold", lineheight = 0.9) +
  geom_point(data = filter(coord, transicion), shape = 21, size = 3.2, stroke = 0.5, color = gris_tx, fill = NA) +
  geom_point(aes(color = tramo, size = pmax(silueta, 0)), alpha = 0.9) +
  geom_segment(data = etq_map, aes(x = dim1, y = dim2, xend = ex, yend = ey), color = gris_tx, linewidth = 0.25) +
  geom_text(data = etq_map, aes(x = lx, y = ly, label = etq), size = 3.0, family = fam, fontface = "bold", color = navy) +
  scale_color_manual(values = col_tramo, labels = lab_tr, name = "Tramo de exposición (HCPC)") +
  scale_fill_manual(values = col_tramo, guide = "none") +
  scale_size_continuous(range = c(0.4, 2.2), breaks = c(0, 0.25, 0.5, 0.75),
                        labels = label_number(accuracy = 0.01, decimal.mark = ","), name = "Silueta") +
  scale_x_continuous(breaks = seq(-6, 4, 2), expand = expansion(mult = c(0.03, 0.2))) +
  scale_y_continuous(labels = label_number(accuracy = 0.1, decimal.mark = ","), expand = expansion(mult = c(0.05, 0.16))) +
  guides(color = guide_legend(order = 1, override.aes = list(size = 2)), size = guide_legend(order = 2)) +
  labs(x = paste0("Dimensión 1 del ACP: nivel de exposición (puntaje estandarizado; ", coma(eig[1], 0), "% de la varianza)"),
       y = paste0("Dimensión 2: aumentación pura (+) frente a\ntareas muy expuestas (−) (", coma(eig[2], 1), "%)")) +
  theme_minimal(base_family = fam, base_size = 10) +
  theme(axis.title = element_text(size = 9.5, color = gris_tx, face = "bold"),
        axis.text = element_text(size = 9, color = navy), panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = grilla, linewidth = 0.3), legend.position = "right",
        legend.title = element_text(size = 9, face = "bold", color = navy), legend.text = element_text(size = 8.5, color = navy),
        plot.background = element_rect(fill = "white", color = NA), plot.margin = margin(12, 12, 10, 12))
guardar_png(p, "mapa.png", ancho = 8.7, alto = 4.95, k = 0.9, bg = "white",
            titulo = "Mapa de los 124 países según su perfil de exposición a la IA",
            subtitulo = "Cada punto es un país (estimaciones para 2026). Color: tramo del HCPC (k = 3); elipses: 90% de cada tramo; tamaño: silueta; anillo gris: grupo de transición con k = 4; líneas: dos tendencias por MCO desde el tramo medio.",
            nota = paste0("ACP sobre 7 variables estandarizadas: proporción de tareas expuestas, exposición media y su desvío, proporción de alta exposición y margen (solo sustitución, ambas, solo aumentación). ",
                          "Se rotulan siete casos de referencia: Níger, Senegal, Nigeria, República Dominicana, Argentina, Corea del Sur y Estados Unidos. La exposición la estima un modelo de lenguaje por tarea."))

# ---- Fronteras entre tramos (anexo A5) ----
# Puntos huecos: los 15 países con silueta < 0,25 (frontera). Rótulos: países de referencia del trabajo y
# Laos, el único con silueta negativa (mal clasificado).
coord <- coord %>% mutate(frontera = if_else(silueta < 0.25, "Silueta < 0,25 (frontera)", "Silueta ≥ 0,25"))
# Los rótulos del tramo alto se sacan fuera de la nube para que se vea a qué punto apunta cada uno.
etq_fr <- coord %>% filter(iso3 %in% c("ARG", "USA", "CHN", "IND", "BRA", "LAO")) %>%
  mutate(nx = case_when(iso3 == "IND" ~ -0.45, iso3 == "ARG" ~ -0.35, iso3 == "BRA" ~ 0.1,
                        iso3 == "USA" ~ 0.25, iso3 == "CHN" ~ -0.75, TRUE ~ 0),
         ny = case_when(iso3 == "IND" ~ -0.65, iso3 == "ARG" ~ 0.6, iso3 == "BRA" ~ -0.6,
                        iso3 == "USA" ~ 0.75, iso3 == "CHN" ~ -0.05, TRUE ~ 0))
p <- ggplot(coord, aes(dim1, dim2)) +
  geom_point(data = filter(coord, transicion), shape = 21, size = 6.5, color = gris_tx, stroke = 1.5) +
  geom_point(aes(color = tramo, shape = frontera), fill = "white", size = 3.6, stroke = 1.6, alpha = 0.95) +
  geom_label_repel(data = etq_fr, aes(label = etq), nudge_x = etq_fr$nx, nudge_y = etq_fr$ny, color = navy,
                   fill = alpha(color_fondo_claro, 0.9), label.size = 0, label.padding = unit(0.12, "lines"),
                   size = 5, family = fam, fontface = "bold", min.segment.length = 0, max.overlaps = Inf,
                   segment.color = navy, segment.size = 0.5, point.padding = 0, box.padding = 0.3,
                   force = 0.5, seed = 7) +
  scale_color_manual(values = col_nivel_s06, labels = nom_nivel, name = NULL) +
  scale_shape_manual(values = c("Silueta ≥ 0,25" = 16, "Silueta < 0,25 (frontera)" = 21), name = NULL) +
  guides(color = guide_legend(order = 1, override.aes = list(size = 4, shape = 16)),
         shape = guide_legend(order = 2, override.aes = list(size = 4, color = gris_tx))) +
  labs(x = "Dim 1 · nivel de exposición", y = "Dim 2 · aumentación pura (+)") +
  theme_slide(t_axtitle = 15) + theme(panel.grid.major.x = element_line(color = grilla, linewidth = 0.5))
sil_tr <- tapply(coord$silueta, coord$tramo, mean)
guardar_png(p, "fronteras.png", ancho = 8.6, alto = 4.95, k = 1.4,
            titulo = "Países en la frontera entre tramos",
            subtitulo = paste0("Plano del ACP. Color: tramo; punto hueco: los ", sum(coord$silueta < 0.25),
                               " países con silueta < 0,25; anillo: grupo de transición que aparece con k = 4."),
            nota = paste0("Rótulos: países de referencia y Laos, el único con silueta negativa. Silueta media ", coma(mean(coord$silueta)),
                          " (tramo bajo / medio / alto: ", paste(coma(sil_tr, 3), collapse = " / "), ")."))

# ---- Robustez frente a las variables elegidas (diapositiva 15) ----
rob <- robustez %>% mutate(variables = factor(variables, levels = rev(variables)),
                           tipo = c("base", "base", "comp"), etiqueta = coma(ari))
p <- ggplot(rob, aes(ari, variables, fill = tipo)) +
  geom_col(width = 0.6, color = color_fondo_claro, linewidth = 0.8) +
  geom_text(aes(label = etiqueta), hjust = -0.15, color = gris_tx, size = 7, family = fam, fontface = "bold") +
  scale_fill_manual(values = c(base = navy, comp = turq), guide = "none") +
  scale_x_continuous(limits = c(0, 1.12), expand = c(0, 0), breaks = seq(0, 1, 0.25),
                     labels = label_number(accuracy = 0.01, decimal.mark = ",")) +
  labs(x = "Índice de Rand ajustado frente al HCPC de 7 variables", y = NULL) +
  theme_slide() + theme(panel.grid.major.y = element_blank())
guardar_png(p, "robust.png", ancho = 8.6, alto = 4.95, k = 1.3,
            titulo = "Robustez de la partición a las variables utilizadas",
            subtitulo = "Índice de Rand ajustado (ARI) entre el HCPC base de 3 tramos (7 variables) y variantes con menos variables; 1 = partición idéntica.",
            nota = "ARI sobre los 124 países. Composición = proporciones de solo sustitución, ambas y solo aumentación.")

# ---- HCPC por composición (anexo A4) ----
coord_c <- as.data.frame(res_pca_c$ind$coord[, 1:2]) %>% rownames_to_column("iso3") %>%
  rename(c1 = Dim.1, c2 = Dim.2) %>%
  left_join(select(comp, iso3, country_name, cluster_comp, aug_rel, int_rel), by = "iso3") %>%
  mutate(etq = nombre_es(iso3, country_name))
if (cor(coord_c$c1, coord_c$aug_rel) < 0) coord_c$c1 <- -coord_c$c1
if (cor(coord_c$c2, coord_c$int_rel) < 0) coord_c$c2 <- -coord_c$c2
eig_c <- res_pca_c$eig[, 2]
lab_tipo <- setNames(paste0(nom_tipo, " (", table(comp$cluster_comp), ")"), names(nom_tipo))
etq_c <- c("ARG", "NGA", "USA", "CRI", "CHN")   # casos destacados

# Rótulos con posición manual (fuera de las nubes) para que no tapen puntos
pos_c <- inner_join(filter(coord_c, iso3 %in% etq_c),
  tibble(iso3 = c("ARG", "NGA", "CHN", "USA", "KOR", "CRI", "HRV", "SSD"),
         lx = c(-2.3, -2.3, -1.0, 1.9, 2.2, 1.2, 3.3, -3.0), ly = c(1.15, 0.7, 1.85, 1.85, 0.75, 0.3, -1.6, -1.65)), by = "iso3")
p_comp <- ggplot(coord_c, aes(c1, c2, color = cluster_comp, fill = cluster_comp)) +
  geom_hline(yintercept = 0, color = grilla) + geom_vline(xintercept = 0, color = grilla) +
  stat_ellipse(geom = "polygon", level = 0.9, alpha = 0.14, linewidth = 0.6) +
  geom_point(size = 2.6, shape = 21, color = color_fondo_claro, stroke = 0.5) +
  geom_segment(data = pos_c, aes(x = c1, y = c2, xend = lx, yend = ly), inherit.aes = FALSE, color = navy, linewidth = 0.35) +
  geom_label(data = pos_c, aes(x = lx, y = ly, label = etq), inherit.aes = FALSE, color = navy, fill = alpha("white", 0.85),
             label.size = 0, label.padding = unit(0.1, "lines"), size = 4.4, family = fam, fontface = "bold") +
  scale_fill_manual(values = col_tipo, labels = lab_tipo, name = NULL) +
  scale_color_manual(values = col_tipo, labels = lab_tipo, name = NULL) +
  guides(fill = guide_legend(nrow = 2), color = guide_legend(nrow = 2)) +
  labs(title = "Clusters por tipo de exposición",
       x = paste0("Dim 1 · aumentación relativa (+) (", coma(eig_c[1], 1), "%)"),
       y = paste0("Dim 2 · intensidad (+) (", coma(eig_c[2], 1), "%)")) +
  theme_panel() + theme(legend.position = "top", legend.text = element_text(size = 13))

tab_cr <- table(tipo = comp$cluster_comp, nivel = comp$tramo[match(comp$iso3, paises$iso3)])
write_csv(as.data.frame(tab_cr), here("output", "tablas", "composicion_vs_nivel.csv"))
df_cr <- as.data.frame(tab_cr) %>% mutate(col_txt = ifelse(Freq > 25, color_fondo_claro, navy))
p_cruce <- ggplot(df_cr, aes(nivel, tipo, fill = Freq)) +
  geom_tile(color = color_fondo_claro, linewidth = 1.5) +
  geom_text(aes(label = Freq, color = col_txt), size = 9, family = fam, fontface = "bold") +
  scale_color_identity() + scale_fill_gradient(low = grilla, high = navy, guide = "none") +
  scale_x_discrete(labels = c("1" = "Tramo\nbajo", "2" = "Tramo\nmedio", "3" = "Tramo\nalto")) +
  scale_y_discrete(labels = setNames(str_wrap(nom_tipo, 12), names(nom_tipo))) +
  labs(title = "Cruce con los tramos de nivel", x = "Tramo de exposición", y = NULL) +
  theme_panel() + theme(panel.grid = element_blank())
guardar_png(unir(p_comp + p_cruce + plot_layout(widths = c(1.35, 1))), "compos.png", ancho = 8.1, alto = 4.88, k = 1.25,
            titulo = "Agrupar los países por la composición de su exposición",
            subtitulo = "Izquierda: clusters del HCPC sobre log-cocientes de solo sustitución, ambas y solo aumentación (plano del ACP). Derecha: países de cada cluster según su tramo de nivel.",
            nota = paste0("Los clusters por composición cruzan los tramos de nivel (ARI frente a los tramos: ", coma(ari_comp_nivel), ")."))
cat(sprintf("ARI composición vs nivel: %.2f | tipo 3 y nivel 3 coinciden: %d de %d\n",
            ari_comp_nivel, tab_cr["3", "3"], sum(tab_cr["3", ])))

# ---- Sensibilidad de la elección de k (diapositivas 12 y 14, anexo A6) ----
# nb.clust = -1: FactoMineR elige solo dónde cortar el árbol (según la ganancia de inercia entre grupos).
# Ese corte automático cambia con la versión del paquete (3 grupos con FactoMineR 2.9; 9 con la 2.18,
# uno de ellos Sudán del Sur solo): por eso k se fija a mano en 3 y la tabla registra la versión usada.
# ARI entre k = 3 y k = 4: cuánto cambia la partición al agregar un grupo (el de transición).
k_auto <- nlevels(HCPC(res_pca, nb.clust = -1, graph = FALSE)$data.clust$clust)
hcpc_sensibilidad_k <- tibble(
  medida = c("Cantidad de grupos con corte automático (nb.clust = -1)",
             "Índice de Rand ajustado entre k = 3 y k = 4"),
  valor = c(k_auto, ari(cl_orden, cl4)),
  version_factominer = as.character(packageVersion("FactoMineR")))
write_csv(hcpc_sensibilidad_k, here("output", "tablas", "hcpc_sensibilidad_k.csv")); print(as.data.frame(hcpc_sensibilidad_k))
