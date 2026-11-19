# ==============================================================
# 02 - Estadísticas descriptivas
# Lee:   input/panel_analisis.csv, auxiliar/grupos_ocupacionales.csv
# Guarda: output/graficos/hist.png, barras.png, corr.png; output/tablas/descriptivas_variables.csv,
#         output/tablas/descriptivas_tramos_expo.csv
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

soc_en <- cargar_nombres_soc("en")

# ---- Histograma (diapositiva 5) ----
media <- mean(panel$mean_exposure)
p <- ggplot(panel, aes(mean_exposure)) +
  geom_histogram(bins = 42, fill = navy, color = color_fondo_claro, linewidth = 0.6) +
  geom_vline(xintercept = media, color = naranja, linewidth = 1.4, linetype = "dashed") +
  annotate("text", x = media + 0.12, y = Inf, vjust = 1.6, hjust = 0, label = paste0("Media: ", coma(media)),
           color = naranja, size = 9, family = fam, fontface = "bold") +
  scale_x_continuous(breaks = 0:3, labels = c("0", "1", "2", "3")) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","), expand = expansion(mult = c(0, 0.08))) +
  labs(x = "Exposición media (escala 0 a 3)", y = "Frecuencia") +
  theme_slide() + theme(panel.grid.major.x = element_blank())
guardar_png(p, "hist.png", w = 8, h = 3.67)

# ---- Exposición media por grupo ocupacional (diapositiva 5) ----
d <- panel %>% group_by(soc_major) %>% summarise(m = mean(mean_exposure), .groups = "drop") %>%
  mutate(grupo = soc_en[soc_major]) %>% arrange(m) %>% mutate(grupo = factor(grupo, levels = grupo))
p <- ggplot(d, aes(m, grupo)) +
  geom_col(fill = navy, width = 0.68) +
  geom_text(aes(label = coma(m)), hjust = -0.15, color = gris_tx, size = 4.6, family = fam, fontface = "bold") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.14)), breaks = seq(0, 2, 0.5),
                     labels = label_number(accuracy = 0.1, decimal.mark = ",")) +
  labs(x = "Exposición media", y = NULL) +
  theme_slide(t_axtext = 11) + theme(panel.grid.major.y = element_blank())
guardar_png(p, "barras.png", w = 7, h = 3.73)

# ---- Correlograma (anexo A2) ----
vars <- c("aug_only_share", "both_share", "mean_exposure", "sub_only_share", "task_count")
cm <- cor(panel[, vars], method = "pearson")
d <- as.data.frame(as.table(cm)) %>% rename(x = Var1, y = Var2, r = Freq) %>%
  mutate(x = factor(x, levels = vars), y = factor(y, levels = vars),
         txt = ifelse(abs(r - 1) < 1e-9, "1", sub("-", "−", coma(r))),
         col_txt = ifelse(abs(r) > 0.6, "white", navy))
p <- ggplot(d, aes(x, y, fill = r)) +
  geom_tile(color = color_fondo_claro, linewidth = 1.6) +
  geom_text(aes(label = txt, color = col_txt), size = 7, family = fam, fontface = "bold") +
  scale_color_identity() +
  scale_fill_gradient2(low = "#8FA3C2", mid = "#E6E9EE", high = navy, midpoint = 0, limits = c(-1, 1),
                       name = "Correlación") +
  scale_x_discrete(position = "bottom") +
  labs(x = NULL, y = NULL) +
  theme_slide(t_axtext = 13) +
  theme(panel.grid = element_blank(), axis.text.x = element_text(angle = 40, hjust = 1, face = "plain"),
        axis.text.y = element_text(face = "plain"), legend.position = "right")
guardar_png(p, "corr.png", w = 8, h = 3.84)

# ---- Tablas descriptivas del anexo A1 (panel completo, antes de limpiar) ----
vars <- c("mean_exposure", "sub_only_share", "both_share", "aug_only_share", "neither_share", "task_count")
dv <- map_dfr(vars, function(v) { x <- panel[[v]]
  tibble(variable = v, media = mean(x), mediana = median(x), desvio = sd(x), min = min(x), max = max(x), cv = sd(x) / mean(x)) })
write_csv(dv, here("output", "tablas", "descriptivas_variables.csv")); print(as.data.frame(dv))
tr <- panel %>% mutate(nivel = cut(mean_exposure, c(-Inf, 0.75, 1.5, Inf), labels = c("Baja (≤ 0,75)", "Media (> 0,75 y ≤ 1,5)", "Alta (> 1,5)"))) %>%
  count(nivel) %>% mutate(pct = 100 * n / sum(n))
write_csv(tr, here("output", "tablas", "descriptivas_tramos_expo.csv")); print(as.data.frame(tr))


