# ==============================================================
# 02 - Estadísticas descriptivas
# Lee:   input/panel_analisis.csv, auxiliar/grupos_ocupacionales.csv
# Guarda: output/graficos/hist.png, barras.png, corr.png; output/tablas/descriptivas_variables.csv,
#         output/tablas/descriptivas_tramos_expo.csv, output/tablas/correlaciones_pearson_spearman.csv
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
source(here("utils", "cargar_nombres_soc.R"))

# ---- Entradas: guardadas por 01_limpieza_faltantes_outliers.R ----
panel <- read_csv(here("input", "panel_analisis.csv"), show_col_types = FALSE,
                  col_types = cols(soc_major = col_character()))

soc_nombre <- cargar_nombres_soc("es")

# ---- Histograma (diapositiva 5) ----
media <- mean(panel$mean_exposure)
p <- ggplot(panel, aes(mean_exposure)) +
  geom_histogram(bins = 42, fill = navy, color = color_fondo_claro, linewidth = 0.6) +
  geom_vline(xintercept = media, color = naranja, linewidth = 1.4, linetype = "dashed") +
  annotate("text", x = media + 0.12, y = Inf, vjust = 1.6, hjust = 0, label = paste0("Media: ", coma(media)),
           color = naranja, size = 6.5, family = fam, fontface = "bold") +
  scale_x_continuous(breaks = 0:3, labels = c("0", "1", "2", "3")) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","), expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Exposición media (escala 0 a 3)", y = "Frecuencia") +
  theme_slide(t_axtitle = 12, t_axtext = 13) + theme(panel.grid.major.x = element_blank())
guardar_png(p, "hist.png", ancho = 6.2, alto = 3.2, k = 1.25,
            titulo = "Distribución de la exposición a la IA",
            nota = paste0("Exposición media (escala 0 a 3) de cada par país × ocupación (n = ", coma(nrow(panel), 0),
                          "). La exposición la estima un modelo de lenguaje por tarea."))

# ---- Exposición media por grupo ocupacional (diapositiva 5) ----
d <- panel %>% group_by(soc_major) %>% summarise(m = mean(mean_exposure), .groups = "drop") %>%
  mutate(grupo = soc_nombre[soc_major]) %>% arrange(m) %>% mutate(grupo = factor(grupo, levels = grupo))
p <- ggplot(d, aes(m, grupo)) +
  geom_col(fill = navy, width = 0.68) +
  geom_text(aes(label = coma(m)), hjust = -0.15, color = gris_tx, size = 4.6, family = fam, fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.14)), breaks = seq(0, 2, 0.5),
                     labels = label_number(accuracy = 0.1, decimal.mark = ",")) +
  labs(x = "Exposición media", y = NULL) +
  theme_slide(t_axtitle = 11, t_axtext = 11) + theme(panel.grid.major.y = element_blank())
guardar_png(p, "barras.png", ancho = 5.73, alto = 5.0, k = 1.1,
            titulo = "Exposición por grupo ocupacional",
            nota = "Exposición media (escala 0 a 3) por grupo SOC, con los 124 países. Promedio simple de ocupaciones O*NET, sin ponderar por empleo.")

# ---- Correlograma (anexo A2) ----
vars <- c("aug_only_share", "both_share", "mean_exposure", "sub_only_share", "task_count")
etq_vars <- c(aug_only_share = "Solo aumentación", both_share = "Ambas", mean_exposure = "Exposición media",
              sub_only_share = "Solo sustitución", task_count = "Cantidad de tareas")
cm <- cor(panel[, vars], method = "pearson")
d <- as.data.frame(as.table(cm)) %>% rename(x = Var1, y = Var2, r = Freq) %>%
  mutate(x = factor(x, levels = vars), y = factor(y, levels = vars),
         txt = ifelse(abs(r - 1) < 1e-9, "1", sub("-", "−", coma(r))),
         col_txt = ifelse(abs(r) > 0.6, "white", navy))
p <- ggplot(d, aes(x, y, fill = r)) +
  geom_tile(color = color_fondo_claro, linewidth = 1.6) +
  geom_text(aes(label = txt, color = col_txt), size = 7, family = fam, fontface = "bold") +
  scale_color_identity() +
  scale_x_discrete(labels = etq_vars) + scale_y_discrete(labels = etq_vars) +
  scale_fill_gradient2(low = "#8FA3C2", mid = "#E6E9EE", high = navy, midpoint = 0, limits = c(-1, 1),
                       name = "Correlación") +
  labs(x = NULL, y = NULL) +
  theme_slide(t_axtext = 13) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 40, hjust = 1, face = "plain"),
        axis.text.y = element_text(face = "plain"), legend.position = "right")
guardar_png(p, "corr.png", ancho = 8.1, alto = 4.88, k = 1.15,
            titulo = "Correlaciones entre las medidas de exposición",
            subtitulo = paste0("Correlación de Pearson en el panel país × ocupación (n = ", coma(nrow(panel), 0), ")."),
            nota = "Solo aumentación, Ambas y Solo sustitución: proporción de las tareas de la ocupación en cada categoría; Exposición media: de 0 a 3.")

# ---- Tablas descriptivas del anexo A1 (panel completo, antes de limpiar) ----
vars <- c("mean_exposure", "sub_only_share", "both_share", "aug_only_share", "neither_share", "task_count")
dv <- map_dfr(vars, function(v) { x <- panel[[v]]
  tibble(variable = v, media = mean(x), mediana = median(x), desvio = sd(x), min = min(x), max = max(x), cv = sd(x) / mean(x)) })
write_csv(dv, here("output", "tablas", "descriptivas_variables.csv")); print(as.data.frame(dv))
tr <- panel %>% mutate(nivel = cut(mean_exposure, c(-Inf, 0.75, 1.5, Inf), labels = c("Baja (≤ 0,75)", "Media (> 0,75 y ≤ 1,5)", "Alta (> 1,5)"))) %>%
  count(nivel) %>% mutate(pct = 100 * n / sum(n))
write_csv(tr, here("output", "tablas", "descriptivas_tramos_expo.csv")); print(as.data.frame(tr))

# ---- Pearson frente a Spearman (anexo A2) ----
# Las variables son proporciones acotadas: si Pearson y Spearman difieren poco, no hay no linealidad relevante.
# Mismas 5 variables del correlograma (más arriba, vars se redefine para las tablas del anexo).
vars_corr <- c("aug_only_share", "both_share", "mean_exposure", "sub_only_share", "task_count")
cp <- cor(panel[, vars_corr], method = "pearson")
cs <- cor(panel[, vars_corr], method = "spearman")
pares <- which(upper.tri(cp), arr.ind = TRUE)
pearson_spearman <- tibble(var_1 = vars_corr[pares[, 1]], var_2 = vars_corr[pares[, 2]],
                           pearson = cp[pares], spearman = cs[pares], diferencia = spearman - pearson) %>%
  arrange(desc(abs(diferencia)))
write_csv(pearson_spearman, here("output", "tablas", "correlaciones_pearson_spearman.csv"))
cat(sprintf("Pearson vs. Spearman: diferencia máxima en valor absoluto %.3f (%s y %s)\n",
            max(abs(pearson_spearman$diferencia)), pearson_spearman$var_1[1], pearson_spearman$var_2[1]))
