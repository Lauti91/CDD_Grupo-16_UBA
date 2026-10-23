# ==============================================================
# GRÁFICOS FINALES PARA LAS DIAPOSITIVAS - INSTANCIA 2
# TP Ciencia de Datos FCE-UBA - Grupo 16
#
# Estética de la Instancia 1: tema claro navy (fondo #F7F8FA, navy #0B1E3D,
# naranja #E8622C como acento), fuente Rajdhani, título 34 / subtítulo 20 /
# ejes 18-16, PNG de 14 x 8 pulgadas a 300 dpi.
#
# Qué hace: recalcula todo lo que necesita (ANOVA, HCPC de Valery, HCPC por
# composición, silueta, robustez) y guarda los 9 gráficos del cuerpo de la
# presentación, uno por diapositiva, en output/graficos_slides/ (carpeta nueva:
# no pisa nada). Cada título es una CONCLUSIÓN (pedido del profesor).
#
#   s03_eta2.png               ANOVA: país y ocupación explican 93%
#   s04_residuos.png           ANOVA: supuestos (QQ + histograma, modelo aditivo)
#   s05_k_dendro_silueta.png   HCPC: elección de k (dendrograma + silueta)
#   s06_mapa_pca.png           HCPC: tres tramos de exposición
#   s07_pbi_por_cluster.png    HCPC: clusters vs. ingreso
#   s08_cluster_soc.png        ANOVA x HCPC: la distancia depende de la ocupación
#   s09_fronteras.png          HCPC: dos fronteras entre clusters
#   s10_robustez_ari.png       HCPC: con una variable se reproduce el 87%
#   s11_composicion.png        HCPC por composición vs. HCPC de nivel
#
# DECISIONES DE ESTILO (cambiar acá si el grupo prefiere otra cosa):
#  - Los 3 tramos de exposición de Valery (ordinales: baja < media < alta)
#    van en una escala navy: celeste grisáceo -> azul medio -> navy.
#    El naranja queda SOLO como acento (dato destacado).
#  - Los 3 clusters de composición (tipo de margen) usan otra paleta
#    (gris / naranja / turquesa) y nombres propios, para no confundirlos
#    con los tramos de nivel. Los nombres son una propuesta: salen de los
#    perfiles medianos (ver Opción 2 del script de expansiones).
#  - Decimales con punto, como en los gráficos de la Instancia 1.
#
# Antes de correr: abrir el .Rproj (o Session > Set Working Directory >
# To Project Directory). Necesita internet la primera vez (fuente Rajdhani).
# Los valores "esperado" salen de reproducir todo en Python con los mismos datos.
# ==============================================================

stopifnot("Falta fijar el directorio del proyecto" =
            file.exists("datos/country_map_panel.csv"))

paquetes <- c("tidyverse", "arrow", "FactoMineR", "factoextra", "ggrepel",
              "cluster", "showtext", "patchwork")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)

library(tidyverse)
library(arrow)
library(FactoMineR)
library(factoextra)
library(ggrepel)
library(cluster)     # silhouette()
library(showtext)
library(patchwork)   # para unir dos gráficos en una diapositiva

dir.create("output/graficos_slides", showWarnings = FALSE, recursive = TRUE)

# --- Fuente (si no hay internet, usa la fuente por defecto) -----
fam <- "rajdhani"
tryCatch(font_add_google("Rajdhani", "rajdhani"),
         error = function(e) { fam <<- "sans"; message("Sin internet: se usa la fuente por defecto") })
showtext_auto()
showtext_opts(dpi = 300)

# --- Paleta (la de la Instancia 1) ------------------------------
color_fondo_claro <- "#F7F8FA"
color_azul_marino <- "#0B1E3D"
color_texto_sec   <- "#5A6472"
color_naranja     <- "#E8622C"
color_grilla      <- "#DDE2E8"
color_gris        <- "#B7BDC7"
color_turquesa    <- "#2E8F8F"   # acento secundario (solo para "tipo de margen")

# Tramos de exposición (ordinal): baja -> media -> alta
col_nivel <- c("1" = "#A9BCD3", "2" = "#4F78A3", "3" = color_azul_marino)
# Solo para el mapa de la diapositiva 6: gris / naranja / azul marino (más contraste)
col_nivel_s06 <- c("1" = "#8A94A3", "2" = color_naranja, "3" = color_azul_marino)
nom_nivel <- c("1" = "Nivel 1 · baja", "2" = "Nivel 2 · media", "3" = "Nivel 3 · alta")
# Tipos de margen (categórico)
nom_tipo  <- c("1" = "Baja y sustitutiva", "2" = "Con más aumentación", "3" = "Alta e intensa")
col_tipo  <- c("1" = "#8A94A3", "2" = color_naranja, "3" = color_turquesa)
# Nivel de ingreso del país (ordinal, escala naranja: no se confunde con los tramos azules)
col_ing <- c("Low income" = "#F4C2A8", "Lower middle income" = "#EE9566",
             "Upper middle income" = "#E8622C", "High income" = "#9C3410")
nom_ing <- c("Low income" = "Bajo", "Lower middle income" = "Medio-bajo",
             "Upper middle income" = "Medio-alto", "High income" = "Alto")

# --- Tema de las diapositivas (mismos tamaños que la Instancia 1) ---
theme_slide <- function(t_title = 34, t_sub = 20, t_axtitle = 18, t_axtext = 16) {
  theme_minimal(base_family = fam) +
    theme(
      plot.background  = element_rect(fill = color_fondo_claro, color = NA),
      panel.background = element_rect(fill = color_fondo_claro, color = NA),
      panel.grid.major = element_line(color = color_grilla, linewidth = 0.5),
      panel.grid.minor = element_blank(),
      plot.title       = element_text(color = color_azul_marino, face = "bold", size = t_title, hjust = 0),
      plot.subtitle    = element_text(color = color_texto_sec, size = t_sub, margin = margin(b = 25)),
      axis.title       = element_text(color = color_texto_sec, size = t_axtitle, face = "bold"),
      axis.text        = element_text(color = color_azul_marino, size = t_axtext, face = "bold"),
      axis.ticks       = element_blank(),
      plot.title.position = "plot",   # título y subtítulo alineados al borde izquierdo
      legend.position  = "top",
      legend.text      = element_text(color = color_azul_marino, size = t_axtext),
      legend.title     = element_text(color = color_azul_marino, size = t_axtext, face = "bold"),
      plot.margin      = margin(35, 40, 30, 30)
    )
}
# versión para paneles chicos (cuando se unen dos gráficos)
theme_panel <- function() {
  theme_slide(t_title = 20, t_axtitle = 15, t_axtext = 13) +
    theme(plot.title = element_text(size = 20, face = "bold", color = color_azul_marino,
                                    margin = margin(b = 10)),
          plot.margin = margin(10, 15, 10, 10))
}

# Nombres en español para los países rotulados
nombres_es <- c(SSD = "Sudán del Sur", COL = "Colombia", IND = "India", USA = "Estados Unidos",
                CHN = "China", DEU = "Alemania", ARG = "Argentina", KOR = "Corea del Sur",
                CRI = "Costa Rica", HRV = "Croacia", NGA = "Nigeria", LAO = "Laos",
                TZA = "Tanzania", LKA = "Sri Lanka", SLV = "El Salvador", PRY = "Paraguay",
                IRN = "Irán", TGO = "Togo", CMR = "Camerún", NPL = "Nepal", MAR = "Marruecos",
                PER = "Perú", ETH = "Etiopía", EGY = "Egipto")
nombre_es <- function(iso3, original) coalesce(unname(nombres_es[iso3]), original)

tit <- function(x) str_wrap(x, 50)    # títulos largos pasan a dos líneas
subt <- function(x) str_wrap(x, 85)

# Título y subtítulo para gráficos armados con patchwork
anotar <- function(p, titulo, subtitulo) {
  p + plot_annotation(
    title = tit(titulo), subtitle = subt(subtitulo),
    theme = theme(
      plot.background = element_rect(fill = color_fondo_claro, color = NA),
      plot.title    = element_text(color = color_azul_marino, face = "bold", size = 34, family = fam),
      plot.subtitle = element_text(color = color_texto_sec, size = 20, family = fam, margin = margin(b = 15)),
      plot.margin   = margin(35, 40, 30, 30)))
}

guardar_slide <- function(p, nombre, w = 14, h = 8) {
  ggsave(paste0("output/graficos_slides/", nombre), p, width = w, height = h,
         dpi = 300, bg = color_fondo_claro)
}

ari <- function(a, b) {
  t <- table(a, b); n <- sum(t); s <- function(x) sum(choose(x, 2))
  sa <- s(rowSums(t)); sb <- s(colSums(t)); sij <- s(t); e <- sa * sb / choose(n, 2)
  (sij - e) / ((sa + sb) / 2 - e)
}
ordenar_clusters <- function(cl, x) {
  med <- tapply(x, cl, median)
  nuevo <- rank(med, ties.method = "first")
  factor(unname(nuevo[as.character(cl)]), levels = seq_along(med))
}

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


# ==============================================================
# A. DATOS Y CÁLCULOS BASE (replica lo de Valery y lo de las expansiones)
# ==============================================================
vars_hcpc <- c("share_exposed", "mean_task_exposure", "sub_only_share",
               "both_share", "aug_only_share", "task_exposure_sd",
               "share_high_exposure")

paises <- read_csv("datos/country_map_panel.csv", show_col_types = FALSE) %>%
  mutate(log_gdp_pc = log(gdp_per_capita),
         suma_exp = sub_only_share + both_share + aug_only_share,
         sub_rel  = sub_only_share / suma_exp,
         both_rel = both_share / suma_exp,
         aug_rel  = aug_only_share / suma_exp,
         int_rel  = share_high_exposure / share_exposed)
stopifnot(sum(is.na(paises[vars_hcpc])) == 0)

datos_hcpc <- paises %>%
  select(iso3, all_of(vars_hcpc)) %>%
  as.data.frame() %>%
  column_to_rownames("iso3")

# --- HCPC de Valery (k = 3): tramos de exposición ----------------
res_pca  <- PCA(datos_hcpc, scale.unit = TRUE, ncp = 3, graph = FALSE)
res_hcpc <- HCPC(res_pca, nb.clust = 3, graph = FALSE)
asig <- tibble(iso3 = rownames(res_hcpc$data.clust), cl_raw = res_hcpc$data.clust$clust)
paises <- paises %>%
  left_join(asig, by = "iso3") %>%
  mutate(cluster_valery = ordenar_clusters(cl_raw, mean_task_exposure))
table(paises$cluster_valery)   # esperado: 23 / 33 / 68

coord_pca <- res_pca$ind$coord
d_pca     <- dist(coord_pca)
cl_orden  <- paises$cluster_valery[match(rownames(coord_pca), paises$iso3)]

# Dim1 / Dim2 para graficar (el signo del PCA es arbitrario: derecha = más exposición,
# arriba = más aumentación pura, igual que en el gráfico de Valery)
coord2 <- as.data.frame(coord_pca[, 1:2]) %>%
  rownames_to_column("iso3") %>%
  rename(dim1 = Dim.1, dim2 = Dim.2)
if (cor(coord2$dim1, paises$mean_task_exposure[match(coord2$iso3, paises$iso3)]) < 0) coord2$dim1 <- -coord2$dim1
if (cor(coord2$dim2, paises$aug_only_share[match(coord2$iso3, paises$iso3)]) < 0)     coord2$dim2 <- -coord2$dim2
coord2 <- coord2 %>%
  left_join(paises %>% select(iso3, country_name, cluster_valery), by = "iso3") %>%
  mutate(etq = nombre_es(iso3, country_name))
eig <- res_pca$eig[, 2]

# --- Silueta por k, por país y grupo de transición ---------------
sil_k <- map_dfr(2:6, function(k) {
  cl_k <- HCPC(res_pca, nb.clust = k, graph = FALSE)$data.clust$clust
  tibble(k = k, silueta_media = mean(silhouette(as.integer(cl_k), d_pca)[, "sil_width"]))
})
sil <- silhouette(as.integer(cl_orden), d_pca)
sil_tab <- tibble(iso3 = rownames(coord_pca), ancho_silueta = sil[, "sil_width"])

cl4_raw <- HCPC(res_pca, nb.clust = 4, graph = FALSE)$data.clust$clust
tab43 <- table(k4 = cl4_raw, k3 = cl_orden)
id_trans <- names(which.min(apply(tab43, 1, max) / rowSums(tab43)))   # el cluster k=4 más "mezclado"
trans_iso <- rownames(coord_pca)[as.character(cl4_raw) == id_trans]
stopifnot(length(trans_iso) == 23)   # esperado: 23 países (igual que Valery)
sil_tab <- sil_tab %>% mutate(grupo_transicion = iso3 %in% trans_iso)

# --- HCPC por composición del margen (Opción 2 de las expansiones) ---
stopifnot(all(paises$sub_rel > 0, paises$both_rel > 0, paises$aug_rel > 0))
comp <- paises %>%
  mutate(l_sub = log(sub_rel), l_both = log(both_rel), l_aug = log(aug_rel),
         centro = (l_sub + l_both + l_aug) / 3,
         clr_sub = l_sub - centro, clr_both = l_both - centro, clr_aug = l_aug - centro)
datos_comp <- comp %>%
  select(iso3, clr_sub, clr_both, clr_aug, int_rel) %>%
  as.data.frame() %>%
  column_to_rownames("iso3")
res_pca_c <- PCA(datos_comp, scale.unit = TRUE, ncp = 3, graph = FALSE)
hc_c <- HCPC(res_pca_c, nb.clust = 3, graph = FALSE)
asig_c <- tibble(iso3 = rownames(hc_c$data.clust), cl_raw_c = hc_c$data.clust$clust)
comp <- comp %>%
  left_join(asig_c, by = "iso3") %>%
  mutate(cluster_comp = ordenar_clusters(cl_raw_c, mean_task_exposure))
table(comp$cluster_comp)   # esperado: 33 / 37 / 54
# Control de los nombres de los clusters de composición (salen de los perfiles):
stopifnot(which.max(tapply(comp$aug_rel, comp$cluster_comp, median)) == 2,   # el 2 tiene más aumentación
          which.max(tapply(comp$int_rel, comp$cluster_comp, median)) == 3)   # el 3 tiene más intensidad
ari_comp_valery <- ari(comp$cluster_comp, comp$cluster_valery)   # esperado ~0.44

# --- ANOVA: matriz país x ocupación (124 x 923) y residuos aditivos ---
occ <- read_parquet("datos/country_occupation_panel.parquet",
                    col_select = c("iso3", "onet_soc_code", "mean_exposure"))
M <- occ %>%
  pivot_wider(names_from = onet_soc_code, values_from = mean_exposure) %>%
  as.data.frame() %>%
  column_to_rownames("iso3") %>%
  as.matrix()
stopifnot(!anyNA(M), nrow(M) == 124, ncol(M) == 923)
rm(occ)

gm      <- mean(M)
ss_tot  <- sum((M - gm)^2)
m_pais  <- rowMeans(M)
m_ocup  <- colMeans(M)
soc_col <- str_sub(colnames(M), 1, 2)
eta_pais <- ncol(M) * sum((m_pais - gm)^2) / ss_tot
eta_ocup <- nrow(M) * sum((m_ocup - gm)^2) / ss_tot
eta_soc  <- sum(sapply(split(seq_along(soc_col), soc_col),
                       function(ix) length(ix) * nrow(M) * (mean(M[, ix]) - gm)^2)) / ss_tot
R <- M - outer(m_pais, rep(1, ncol(M))) - outer(rep(1, nrow(M)), m_ocup) + gm
eta_res <- sum(R^2) / ss_tot
stopifnot(abs(eta_pais + eta_ocup + eta_res - 1) < 1e-8)

r <- as.numeric(R)
m2 <- mean((r - mean(r))^2)
asimetria       <- mean((r - mean(r))^3) / m2^1.5     # esperado -0.16
curtosis_exceso <- mean((r - mean(r))^4) / m2^2 - 3   # esperado 1.13
razon_sd <- max(apply(R, 1, sd)) / min(apply(R, 1, sd))   # esperado 2.75
set.seed(123)
res_muestra <- sample(r, 5000)

# Medias por cluster de Valery y grupo SOC (para la mancuerna)
Ms <- sapply(split(seq_along(soc_col), soc_col), function(ix) rowMeans(M[, ix, drop = FALSE]))
cl_ms <- paises$cluster_valery[match(rownames(Ms), paises$iso3)]
stopifnot(!anyNA(cl_ms))

# --- Robustez del HCPC a las variables elegidas -------------------
hc_cons <- function(vars, k = 3) {
  Z   <- scale(paises[, vars])
  cl_ <- cutree(hclust(dist(Z), method = "ward.D2"), k)
  centros <- rowsum(Z, cl_) / as.numeric(table(cl_))
  kmeans(Z, centers = centros)$cluster
}
robustez <- tibble(
  variables = c("Las 7 variables de Valery", "Solo exposición media",
                "Exposición media + desvío + tareas muy expuestas", "Solo composición (sust., ambas, aum.)"),
  ari = c(ari(paises$cluster_valery, hc_cons(vars_hcpc)),
          ari(paises$cluster_valery, hc_cons("mean_task_exposure")),
          ari(paises$cluster_valery, hc_cons(c("mean_task_exposure", "task_exposure_sd", "share_high_exposure"))),
          ari(paises$cluster_valery, hc_cons(c("sub_only_share", "both_share", "aug_only_share")))))
robustez   # esperado: ~1.00 | 0.87 | 0.85 | 0.46


# ==============================================================
# DIAPOSITIVA 3. ANOVA: país y ocupación explican 93%
# ==============================================================
tab_eta <- tibble(
  fuente = c("Grupo mayor SOC (22)", "País (124)", "Ocupación O*NET (923)",
             "País + ocupación (aditivo)", "Residual (interacción + error)"),
  eta2   = c(eta_soc, eta_pais, eta_ocup, eta_pais + eta_ocup, eta_res),
  tipo   = c("base", "base", "base", "dest", "resto")) %>%
  mutate(fuente   = factor(fuente, levels = rev(fuente)),
         etiqueta = paste0(sprintf("%.3f", eta2), "  (", sprintf("%.1f", eta2 * 100), "%)"))

p_eta <- ggplot(tab_eta, aes(x = eta2, y = fuente, fill = tipo)) +
  geom_col(width = 0.7, color = color_fondo_claro, linewidth = 0.8) +
  geom_text(aes(label = etiqueta), hjust = -0.1, color = color_texto_sec,
            size = 6, family = fam, fontface = "bold") +
  scale_fill_manual(values = c(base = color_azul_marino, dest = color_naranja, resto = color_gris),
                    guide = "none") +
  scale_x_continuous(limits = c(0, 1.15), expand = c(0, 0), breaks = seq(0, 1, 0.25)) +
  labs(title = tit("País y ocupación explican el 93% de la exposición"),
       subtitle = subt("Proporción de la varianza explicada por cada agrupación · 124 países × 923 ocupaciones"),
       x = NULL, y = NULL) +
  theme_slide() +
  theme(panel.grid.major.y = element_blank())
p_eta
guardar_slide(p_eta, "s03_eta2.png", h = 7)


# ==============================================================
# DIAPOSITIVA 4. ANOVA: supuestos (residuos del modelo aditivo)
# ==============================================================
dat_qq <- tibble(res = res_muestra)
p_qq <- ggplot(dat_qq, aes(sample = res)) +
  stat_qq(color = color_azul_marino, alpha = 0.35, size = 1.3) +
  stat_qq_line(color = color_naranja, linewidth = 1.2) +
  labs(title = "QQ-plot", x = "Cuantiles teóricos (normal)", y = "Cuantiles de los residuos") +
  theme_panel()
p_hist <- ggplot(dat_qq, aes(x = res)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60, fill = color_azul_marino,
                 color = color_fondo_claro, linewidth = 0.4) +
  stat_function(fun = dnorm, args = list(mean = mean(dat_qq$res), sd = sd(dat_qq$res)),
                color = color_naranja, linewidth = 1.3) +
  labs(title = "Histograma", x = "Residuo del modelo país + ocupación", y = NULL) +
  theme_panel()
p_res <- anotar(p_qq + p_hist,
                "Residuos casi simétricos, con colas algo pesadas",
                sprintf("Modelo aditivo país + ocupación · muestra de 5.000 residuos · asimetría %.2f, curtosis en exceso %.2f · el desvío del residuo varía %.1f veces entre países",
                        asimetria, curtosis_exceso, razon_sd))
p_res
guardar_slide(p_res, "s04_residuos.png")


# ==============================================================
# DIAPOSITIVA 5. HCPC: elección de k (dendrograma + silueta)
# ==============================================================
arbol <- res_hcpc$call$t$tree
# El dendrograma completo se aplasta (casi todas las fusiones son muy bajas), así que se
# muestran las alturas de las últimas fusiones, que es lo que justifica el corte.
alturas <- tibble(altura = head(sort(arbol$height, decreasing = TRUE), 8)) %>%
  mutate(fusion = factor(paste0("De ", row_number() + 1, " a ", row_number()),
                         levels = rev(paste0("De ", seq_len(8) + 1, " a ", seq_len(8)))),
         destacado = row_number() <= 2)
p_alt <- ggplot(alturas, aes(x = altura, y = fusion, fill = destacado)) +
  geom_col(width = 0.7, color = color_fondo_claro, linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", altura)), hjust = -0.15, color = color_texto_sec,
            size = 5.5, family = fam, fontface = "bold") +
  scale_fill_manual(values = c("TRUE" = color_azul_marino, "FALSE" = color_gris), guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Altura de las últimas fusiones", x = "Altura de fusión", y = NULL) +
  theme_panel() +
  theme(panel.grid.major.y = element_blank())

p_silk <- ggplot(sil_k, aes(k, silueta_media)) +
  geom_line(color = color_azul_marino, linewidth = 1.2) +
  geom_point(aes(color = k == 3), size = 5) +
  geom_text(aes(label = sprintf("%.2f", silueta_media), vjust = ifelse(k == 4, 2.2, -1.3)),
            color = color_azul_marino, family = fam, fontface = "bold", size = 6) +
  scale_color_manual(values = c("TRUE" = color_naranja, "FALSE" = color_azul_marino), guide = "none") +
  scale_x_continuous(breaks = 2:6) +
  scale_y_continuous(limits = c(0.4, 0.68)) +
  labs(title = "Silueta media según k", x = "Cantidad de clusters (k)", y = NULL) +
  theme_panel()

p_k <- anotar(p_alt + p_silk + plot_layout(widths = c(1.2, 1)),
              "k = 3 es razonable; k = 4 es la peor opción",
              "Izq.: alturas de las últimas fusiones del árbol (dos saltos y luego caída suave) · Der.: ancho de silueta medio (k = 2 es la mejor, k = 3 la segunda, k = 4 la peor)")
p_k
guardar_slide(p_k, "s05_k_dendro_silueta.png")


# ==============================================================
# DIAPOSITIVA 6. HCPC: tres tramos de exposición
# ==============================================================
n_niv <- table(paises$cluster_valery)
lab_niv <- setNames(paste0(nom_nivel, " (n = ", n_niv, ")"), names(nom_nivel))
paises_etq <- c("SSD", "COL", "USA", "CHN", "ARG", "KOR")   # (India y Alemania se pisaban con otros puntos)

p_mapa <- ggplot(coord2, aes(dim1, dim2, color = cluster_valery, fill = cluster_valery)) +
  geom_hline(yintercept = 0, color = color_grilla) +
  geom_vline(xintercept = 0, color = color_grilla) +
  stat_ellipse(geom = "polygon", level = 0.9, alpha = 0.14, linewidth = 0.6) +
  geom_point(size = 3.4, shape = 21, color = color_fondo_claro, stroke = 0.7) +
  geom_text_repel(data = filter(coord2, iso3 %in% paises_etq), aes(label = etq),
                  color = color_azul_marino, size = 5.5, family = fam, fontface = "bold",
                  min.segment.length = 0, box.padding = 0.6, max.overlaps = Inf) +
  scale_fill_manual(values = col_nivel_s06, labels = lab_niv, name = NULL) +
  scale_color_manual(values = col_nivel_s06, labels = lab_niv, name = NULL) +
  labs(title = tit("Los países se ordenan en tres tramos de exposición"),
       subtitle = subt(paste0("HCPC, k = 3 · Dim 1 es el nivel de exposición (", round(eig[1], 1),
                              "% de la varianza); Dim 2, aumentación pura frente a tareas muy expuestas (",
                              round(eig[2], 1), "%)")),
       x = paste0("Dim 1 · nivel de exposición (", round(eig[1], 1), "%)"),
       y = paste0("Dim 2 · aumentación pura (+) (", round(eig[2], 1), "%)")) +
  theme_slide() +
  theme(panel.grid.major.x = element_line(color = color_grilla, linewidth = 0.5))
p_mapa
guardar_slide(p_mapa, "s06_mapa_pca.png", h = 8.5)


# ==============================================================
# DIAPOSITIVA 7. HCPC: clusters vs. ingreso
# ==============================================================
base_pbi <- paises %>%
  filter(is.finite(log_gdp_pc), income_level %in% names(col_ing)) %>%
  mutate(income_level = factor(income_level, levels = names(col_ing)))
n_sin_clasif <- sum(is.finite(paises$log_gdp_pc) & !(paises$income_level %in% names(col_ing)))
kw_pbi <- kruskal.test(log_gdp_pc ~ cluster_valery, data = filter(paises, is.finite(log_gdp_pc)))
eps2_pbi <- unname(kw_pbi$statistic) / (sum(is.finite(paises$log_gdp_pc)) - 1)   # esperado ~0.73
n_cl <- table(base_pbi$cluster_valery)

p_pbi <- ggplot(base_pbi, aes(x = cluster_valery, y = log_gdp_pc)) +
  geom_boxplot(aes(fill = cluster_valery), width = 0.55, alpha = 0.35,
               outlier.shape = NA, color = color_azul_marino, linewidth = 0.7) +
  geom_jitter(aes(color = income_level), width = 0.18, size = 3.2, alpha = 0.95) +
  scale_fill_manual(values = col_nivel, guide = "none") +
  scale_color_manual(values = col_ing, labels = nom_ing, name = "Nivel de ingreso") +
  scale_x_discrete(labels = function(x) paste0(nom_nivel[x], "\n(n = ", n_cl[x], ")")) +
  labs(title = tit("Los clusters de exposición siguen el nivel de ingreso"),
       subtitle = subt(sprintf("Cada punto es un país, coloreado por nivel de ingreso · Kruskal-Wallis: epsilon cuadrado = %.2f%s",
                               eps2_pbi,
                               ifelse(n_sin_clasif > 0, sprintf(" · se omiten %d países sin clasificar", n_sin_clasif), ""))),
       x = NULL, y = "log(PBI per cápita)") +
  theme_slide() +
  theme(panel.grid.major.x = element_blank())
p_pbi
guardar_slide(p_pbi, "s07_pbi_por_cluster.png")


# ==============================================================
# DIAPOSITIVA 8. ANOVA x HCPC: la distancia entre clusters depende de la ocupación
# ==============================================================
medias_cs <- sapply(c("1", "2", "3"), function(c) colMeans(Ms[cl_ms == c, , drop = FALSE]))
tab_cs <- as_tibble(medias_cs, rownames = "soc_major") %>%
  left_join(soc_labels, by = "soc_major") %>%
  mutate(brecha = `3` - `1`) %>%
  arrange(brecha)
orden_soc <- tab_cs$soc_label
dat_pts <- tab_cs %>%
  pivot_longer(c(`1`, `2`, `3`), names_to = "cluster", values_to = "media") %>%
  mutate(soc_label = factor(soc_label, levels = orden_soc))
tab_cs <- tab_cs %>% mutate(soc_label = factor(soc_label, levels = orden_soc))

p_cs <- ggplot() +
  geom_segment(data = tab_cs, aes(y = soc_label, yend = soc_label, x = `1`, xend = `3`),
               color = color_gris, linewidth = 1.6) +
  geom_point(data = dat_pts, aes(x = media, y = soc_label, color = cluster), size = 4.2) +
  scale_color_manual(values = col_nivel, labels = nom_nivel, name = NULL) +
  labs(title = tit("La distancia entre clusters cambia según el tipo de ocupación"),
       subtitle = subt(sprintf("Exposición media por grupo SOC y tramo de países · brecha entre nivel 3 y 1: de +%.2f a +%.2f puntos",
                               min(tab_cs$brecha), max(tab_cs$brecha))),
       x = "Exposición media (escala 0 a 3)", y = NULL) +
  theme_slide() +
  theme(axis.text.y = element_text(size = 15))
p_cs
guardar_slide(p_cs, "s08_cluster_soc.png", h = 9)
# esperado: brecha de +0.70 (Community & Social Service) a +1.23 (Production)


# ==============================================================
# DIAPOSITIVA 9. HCPC: dos fronteras entre clusters
# ==============================================================
dat_front <- coord2 %>%
  left_join(sil_tab, by = "iso3")
etq_sil <- dat_front %>% filter(ancho_silueta < 0.2)   # esperado: 13 países

p_front <- ggplot(dat_front, aes(dim1, dim2)) +
  geom_point(aes(color = cluster_valery, size = pmax(ancho_silueta, 0)), alpha = 0.9) +
  geom_point(data = filter(dat_front, grupo_transicion), shape = 21, size = 6.5,
             color = color_naranja, stroke = 1.1) +
  geom_text_repel(data = etq_sil, aes(label = etq), color = color_azul_marino,
                  size = 5, family = fam, fontface = "bold",
                  min.segment.length = 0, max.overlaps = Inf, box.padding = 0.5) +
  scale_color_manual(values = col_nivel, labels = nom_nivel, name = NULL) +
  scale_size_continuous(range = c(1.5, 5.5), name = "Ancho de silueta") +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  labs(title = tit("Hay dos fronteras entre clusters, no una"),
       subtitle = subt("Tamaño = ancho de silueta · círculo naranja = grupo de transición (k = 4) · rótulos: silueta < 0.2"),
       x = "Dim 1 · nivel de exposición", y = "Dim 2 · aumentación pura (+)") +
  theme_slide() +
  theme(panel.grid.major.x = element_line(color = color_grilla, linewidth = 0.5))
p_front
guardar_slide(p_front, "s09_fronteras.png", h = 8.5)


# ==============================================================
# DIAPOSITIVA 10. HCPC: con una variable se reproduce el 87%
# ==============================================================
rob <- robustez %>%
  mutate(variables = factor(variables, levels = rev(variables)),
         tipo = c("control", "base", "base", "comp"),   # 1ª barra: las 7 variables (control)
         etiqueta = sprintf("%.2f", ari))
p_rob <- ggplot(rob, aes(x = ari, y = variables, fill = tipo)) +
  geom_col(width = 0.65, color = color_fondo_claro, linewidth = 0.8) +
  geom_text(aes(label = etiqueta), hjust = -0.15, color = color_texto_sec,
            size = 7, family = fam, fontface = "bold") +
  scale_fill_manual(values = c(control = color_gris, base = color_azul_marino, comp = color_turquesa), guide = "none") +
  scale_x_continuous(limits = c(0, 1.12), expand = c(0, 0), breaks = seq(0, 1, 0.25)) +
  labs(title = tit("Con una sola variable se reproduce el 87% del HCPC"),
       subtitle = subt("Índice de Rand ajustado (1 = misma partición) frente al HCPC de las 7 variables"),
       x = NULL, y = NULL) +
  theme_slide() +
  theme(panel.grid.major.y = element_blank())
p_rob
guardar_slide(p_rob, "s10_robustez_ari.png", h = 6.5)


# ==============================================================
# DIAPOSITIVA 11. HCPC por composición vs. HCPC de nivel
# ==============================================================
coord_c <- as.data.frame(res_pca_c$ind$coord[, 1:2]) %>%
  rownames_to_column("iso3") %>%
  rename(c1 = Dim.1, c2 = Dim.2) %>%
  left_join(comp %>% select(iso3, country_name, cluster_comp, aug_rel, int_rel), by = "iso3") %>%
  mutate(etq = nombre_es(iso3, country_name))
# signo: derecha = más aumentación relativa, arriba = más intensidad
if (cor(coord_c$c1, coord_c$aug_rel) < 0) coord_c$c1 <- -coord_c$c1
if (cor(coord_c$c2, coord_c$int_rel) < 0) coord_c$c2 <- -coord_c$c2
eig_c <- res_pca_c$eig[, 2]
n_tipo <- table(comp$cluster_comp)
lab_tipo <- setNames(paste0(nom_tipo, " (", n_tipo, ")"), names(nom_tipo))
etq_c <- c("CRI", "HRV", "ARG", "KOR", "USA", "CHN", "NGA", "SSD")

p_comp <- ggplot(coord_c, aes(c1, c2, color = cluster_comp, fill = cluster_comp)) +
  geom_hline(yintercept = 0, color = color_grilla) +
  geom_vline(xintercept = 0, color = color_grilla) +
  stat_ellipse(geom = "polygon", level = 0.9, alpha = 0.14, linewidth = 0.6) +
  geom_point(size = 3, shape = 21, color = color_fondo_claro, stroke = 0.7) +
  geom_text_repel(data = filter(coord_c, iso3 %in% etq_c), aes(label = etq),
                  color = color_azul_marino, size = 4.8, family = fam, fontface = "bold",
                  min.segment.length = 0, box.padding = 0.5, max.overlaps = Inf) +
  scale_fill_manual(values = col_tipo, labels = lab_tipo, name = NULL) +
  scale_color_manual(values = col_tipo, labels = lab_tipo, name = NULL) +
  labs(title = "Clusters por tipo de margen",
       x = paste0("Dim 1 · aumentación relativa (+) (", round(eig_c[1], 1), "%)"),
       y = paste0("Dim 2 · intensidad (+) (", round(eig_c[2], 1), "%)")) +
  theme_panel() +
  theme(legend.position = "top", legend.text = element_text(size = 12))

tab_cr <- table(tipo = comp$cluster_comp, nivel = comp$cluster_valery)
coincide <- tab_cr["3", "3"]; n_tipo3 <- sum(tab_cr["3", ])
df_cr <- as.data.frame(tab_cr) %>%
  mutate(color_txt = ifelse(Freq > 25, color_fondo_claro, color_azul_marino))
p_cruce <- ggplot(df_cr, aes(x = nivel, y = tipo, fill = Freq)) +
  geom_tile(color = color_fondo_claro, linewidth = 1.5) +
  geom_text(aes(label = Freq, color = color_txt), size = 9, family = fam, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradient(low = color_grilla, high = color_azul_marino, guide = "none") +
  scale_x_discrete(labels = c("1" = "Nivel 1\nbaja", "2" = "Nivel 2\nmedia", "3" = "Nivel 3\nalta")) +
  scale_y_discrete(labels = setNames(str_wrap(nom_tipo, 12), names(nom_tipo))) +
  labs(title = "Cruce con el HCPC de nivel", x = "Tramo de exposición (Valery)", y = NULL) +
  theme_panel() +
  theme(panel.grid = element_blank())

p_c <- anotar(p_comp + p_cruce + plot_layout(widths = c(1.35, 1)),
              "Clasificar por tipo de margen da otra partición",
              sprintf("ARI entre las dos clasificaciones: %.2f · el grupo de mayor exposición casi coincide (%d de %d), difieren los niveles bajo y medio",
                      ari_comp_valery, coincide, n_tipo3))
p_c
guardar_slide(p_c, "s11_composicion.png")

message("Listo: 9 gráficos en output/graficos_slides/")