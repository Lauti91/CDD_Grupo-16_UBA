# ==============================================================
# 08 - Brecha de exposición entre tramos alto y bajo por grupo ocupacional (soc.png)
# Lee:   input/panel_analisis.csv, input/paises_analisis.csv, input/paises_tramos.csv, auxiliar/grupos_ocupacionales.csv
# Guarda: output/tablas/brecha_por_grupo_soc.csv, interaccion_tramo_soc.csv; output/graficos/soc.png
# ==============================================================
library(here)
library(tidyverse)
library(scales)

# Funciones propias y configuración (utils/)
source(here("utils", "configuracion_graficos.R"))
source(here("utils", "coma.R"))
source(here("utils", "theme_slide.R"))
source(here("utils", "theme_panel.R"))
source(here("utils", "unir.R"))
source(here("utils", "guardar_png.R"))
source(here("utils", "leer_paises_con_tramos.R"))
source(here("utils", "cargar_nombres_soc.R"))

# ---- Entradas (scripts 01 y 04) ----
panel <- read_csv(here("input", "panel_analisis.csv"), show_col_types = FALSE,
                  col_types = cols(soc_major = col_character()))
paises <- leer_paises_con_tramos()
paises_ing <- filter(paises, !sin_clasif)      # 122 países con clasificación de ingreso

soc_es <- cargar_nombres_soc("es")
# Matriz país x grupo SOC (exposición media)
mat <- panel %>% select(iso3, onet_soc_code, mean_exposure) %>%
  pivot_wider(names_from = onet_soc_code, values_from = mean_exposure) %>%
  as.data.frame() %>% column_to_rownames("iso3") %>% as.matrix()
soc_col <- str_sub(colnames(mat), 1, 2)
mat_soc <- sapply(split(seq_along(soc_col), soc_col), function(ix) rowMeans(mat[, ix, drop = FALSE]))

soc_pais <- tibble(iso3 = rep(rownames(mat_soc), ncol(mat_soc)), soc_major = rep(colnames(mat_soc), each = nrow(mat_soc)),
                   expo = as.vector(mat_soc)) %>% left_join(select(paises, iso3, tramo), by = "iso3")
brecha <- soc_pais %>% filter(tramo %in% c("1", "3")) %>%
  group_by(soc_major, tramo) %>% summarise(m = mean(expo), .groups = "drop") %>%
  pivot_wider(names_from = tramo, values_from = m, names_prefix = "t") %>%
  mutate(dif = t3 - t1, grupo = soc_es[soc_major]) %>% arrange(dif) %>%
  mutate(grupo = factor(grupo, levels = grupo), rk = rank(-dif),
         destacado = case_when(rk <= 5 ~ "max", rk >= 18 ~ "min", TRUE ~ "resto"))
write_csv(brecha %>% select(soc_major, grupo, t1, t3, dif), here("output", "tablas", "brecha_por_grupo_soc.csv"))
fondo <- soc_pais %>% filter(tramo %in% c("1", "3")) %>% mutate(grupo = factor(soc_es[soc_major], levels = levels(brecha$grupo)))

col_borde   <- c(max = "#A8410F", min = navy,      resto = "black")
col_relleno <- c(max = naranja,  min = "#4D6087", resto = "#C9CFD7")
col_etq     <- c(max = naranja,  min = navy,      resto = gris_tx)
top5 <- filter(brecha, destacado == "max"); bot5 <- filter(brecha, destacado == "min")
fila_top <- slice_max(brecha, dif, n = 1)
lab_y <- nrow(brecha) + 0.62
orden_dest <- brecha$destacado[match(levels(brecha$grupo), brecha$grupo)]

p <- ggplot(brecha, aes(y = grupo)) +
  geom_point(data = filter(fondo, tramo == "1"), aes(x = expo), color = "#AEB5BF", size = 0.5, alpha = 0.3,
             position = position_jitter(height = 0.16, seed = 1)) +
  geom_point(data = filter(fondo, tramo == "3"), aes(x = expo), color = "#7F8FA8", size = 0.5, alpha = 0.3,
             position = position_jitter(height = 0.16, seed = 2)) +
  geom_segment(aes(x = t1, xend = t3, yend = grupo), color = unname(col_borde[brecha$destacado]), linewidth = 1.6) +
  geom_segment(aes(x = t1, xend = t3, yend = grupo), color = unname(col_relleno[brecha$destacado]), linewidth = 1.0) +
  geom_point(aes(x = t1), shape = 21, fill = "white", color = "black", size = 2.4, stroke = 0.8) +
  geom_point(aes(x = t3), shape = 21, fill = navy, color = navy, size = 2.6, stroke = 0.6) +
  annotate("text", x = fila_top$t1, y = lab_y, label = "Países de\nexposición baja", size = 3.1, family = fam,
           fontface = "bold", color = gris_tx, lineheight = 0.85, vjust = 0) +
  annotate("text", x = fila_top$t3, y = lab_y, label = "Países de\nexposición alta", size = 3.1, family = fam,
           fontface = "bold", color = navy, lineheight = 0.85, vjust = 0) +
  annotate("text", x = 2.98, y = which(levels(brecha$grupo) == "Gastronomía") - 0.1, hjust = 1, vjust = 1,
           label = paste0("Las 5 brechas más grandes:\nde ", coma(min(top5$dif), signo = TRUE), " a ", coma(max(top5$dif), signo = TRUE), " puntos"),
           size = 3.3, family = fam, fontface = "bold", color = naranja, lineheight = 0.9) +
  annotate("text", x = 1.62, y = which(levels(brecha$grupo) == "Arte, diseño y medios") + 0.45, hjust = 0, vjust = 1,
           label = paste0("Las 5 más chicas:\nde ", coma(min(bot5$dif), signo = TRUE), " a ", coma(max(bot5$dif), signo = TRUE), " puntos"),
           size = 3.3, family = fam, fontface = "bold", color = navy, lineheight = 0.9) +
  scale_x_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5), labels = label_number(accuracy = 0.1, decimal.mark = ","),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_y_discrete(expand = expansion(add = c(0.6, 2.5))) +
  labs(x = "Exposición media a la IA (escala de 0 a 3)", y = NULL) +
  theme_minimal(base_family = fam, base_size = 10) +
  theme(axis.title.x = element_text(size = 9.5, color = gris_tx, face = "bold", margin = margin(t = 6)),
        axis.text.y = element_text(size = 9.5, color = unname(col_etq[orden_dest]), face = ifelse(orden_dest == "resto", "plain", "bold")),
        axis.text.x = element_text(size = 9, color = gris_tx), panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_line(color = grilla, linewidth = 0.3),
        plot.background = element_rect(fill = "white", color = NA), plot.margin = margin(12, 14, 10, 12))
guardar_png(p, "soc.png", ancho = 8.0, alto = 4.95, k = 0.9, bg = "white",
            titulo = "Brecha de exposición por grupo ocupacional",
            subtitulo = paste0("Exposición media (escala 0 a 3) por grupo SOC en los ", sum(paises$tramo == "1"), " países de exposición baja (círculo blanco) y los ",
                               sum(paises$tramo == "3"), " de exposición alta (azul marino). Puntos tenues: cada país."),
            nota = "Grupos de países: HCPC de los 124 países según su perfil de exposición; se omite el tramo medio (33). Brecha en puntos de la escala de 0 a 3. La exposición la estima un modelo de lenguaje por tarea.")
cat(sprintf("Brecha: de %+.2f a %+.2f puntos | PBI mediano tramo bajo %.0f, alto %.0f\n", min(brecha$dif), max(brecha$dif),
            median(paises$gdp_per_capita[paises$tramo == "1"]), median(paises$gdp_per_capita[paises$tramo == "3"])))

# ---- Interacción tramo x grupo SOC: test de permutación (interaccion_tramo_soc.csv) ----
# ANOVA tramo x grupo sobre la matriz 124 x 22; solo la interacción (el efecto tramo es circular)
# Permuta las etiquetas de tramo entre países; sin normalidad ni varianzas iguales; test aproximado
tramo_pais <- paises$tramo[match(rownames(mat_soc), paises$iso3)]
stopifnot(!anyNA(tramo_pais), ncol(mat_soc) == 22)
ss_interaccion <- function(mat_soc, cl) {
  n         <- as.numeric(table(cl))
  medias_tg <- rowsum(mat_soc, cl) / n          # medias tramo x grupo
  fila      <- rowMeans(medias_tg)              # media de cada tramo
  col       <- colSums(medias_tg * n) / sum(n)  # media de cada grupo
  ajust     <- outer(fila, rep(1, ncol(medias_tg))) + outer(rep(1, nrow(medias_tg)), col - mean(mat_soc))
  sum(n * (medias_tg - ajust)^2)
}
ss_obs <- ss_interaccion(mat_soc, tramo_pais)
set.seed(123)
b_perm <- 3000
ss_perm <- replicate(b_perm, ss_interaccion(mat_soc, sample(tramo_pais)))
interaccion <- tibble(ss_interaccion_observada = ss_obs, ss_media_permutaciones = mean(ss_perm),
                      permutaciones = b_perm, superan_la_observada = sum(ss_perm >= ss_obs),
                      p_valor = (sum(ss_perm >= ss_obs) + 1) / (b_perm + 1))
write_csv(interaccion, here("output", "tablas", "interaccion_tramo_soc.csv")); print(as.data.frame(interaccion))
