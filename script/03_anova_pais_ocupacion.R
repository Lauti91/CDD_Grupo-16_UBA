# ==============================================================
# 03 - Método 1: ANOVA (cuánto explican el país y la ocupación)
# Lee:   input/panel_analisis.csv
# Guarda: output/tablas/anova_eta2.csv, anova_supuestos.csv; output/graficos/eta2.png, qq.png
# ==============================================================
library(here)
library(tidyverse)
library(scales)
library(patchwork)

# Funciones propias y configuración (utils/)
source(here("utils", "configuracion_graficos.R"))
source(here("utils", "coma.R"))
source(here("utils", "theme_slide.R"))
source(here("utils", "theme_panel.R"))
source(here("utils", "unir.R"))
source(here("utils", "guardar_png.R"))

# ---- Entradas: guardadas por 01_limpieza_faltantes_outliers.R ----
panel <- read_csv(here("input", "panel_analisis.csv"), show_col_types = FALSE,
                  col_types = cols(soc_major = col_character()))

# ---- ANOVA de dos factores sin réplica: eta cuadrado por sumas de cuadrados ----
# Diseño balanceado 124 países x 923 ocupaciones (una observación por celda).
M <- panel %>% select(iso3, onet_soc_code, mean_exposure) %>%
  pivot_wider(names_from = onet_soc_code, values_from = mean_exposure) %>%
  as.data.frame() %>% column_to_rownames("iso3") %>% as.matrix()
stopifnot(!anyNA(M), nrow(M) == 124, ncol(M) == 923)
gm <- mean(M); ss_tot <- sum((M - gm)^2)
m_pais <- rowMeans(M); m_ocup <- colMeans(M); soc_col <- str_sub(colnames(M), 1, 2)
eta_pais <- ncol(M) * sum((m_pais - gm)^2) / ss_tot
eta_ocup <- nrow(M) * sum((m_ocup - gm)^2) / ss_tot
eta_soc  <- sum(sapply(split(seq_along(soc_col), soc_col),
                       function(ix) length(ix) * nrow(M) * (mean(M[, ix]) - gm)^2)) / ss_tot
R <- M - outer(m_pais, rep(1, ncol(M))) - outer(rep(1, nrow(M)), m_ocup) + gm
eta_res <- sum(R^2) / ss_tot
stopifnot(abs(eta_pais + eta_ocup + eta_res - 1) < 1e-8)
r <- as.numeric(R); m2 <- mean((r - mean(r))^2)
asimetria <- mean((r - mean(r))^3) / m2^1.5
curtosis_exceso <- mean((r - mean(r))^4) / m2^2 - 3
sd_res_pais <- apply(R, 1, sd); razon_sd <- max(sd_res_pais) / min(sd_res_pais)
set.seed(123); res_muestra <- sample(r, 5000)

# ---- Eta cuadrado (diapositiva 10) ----
tab_eta <- tibble(
  fuente = c("Grupo mayor SOC (22)", "País (124)", "Ocupación O*NET (923)",
             "País + ocupación (aditivo)", "Residual (interacción + error)"),
  eta2 = c(eta_soc, eta_pais, eta_ocup, eta_pais + eta_ocup, eta_res),
  tipo = c("base", "base", "base", "dest", "resto")) %>%
  mutate(fuente = factor(fuente, levels = rev(fuente)),
         etiqueta = paste0(coma(eta2, 3), "  (", coma(eta2 * 100, 1), "%)"))
write_csv(tab_eta %>% select(fuente, eta2), here("output", "tablas", "anova_eta2.csv"))

p <- ggplot(tab_eta, aes(eta2, fuente, fill = tipo)) +
  geom_col(width = 0.7, color = color_fondo_claro, linewidth = 0.8) +
  geom_text(aes(label = etiqueta), hjust = -0.1, color = gris_tx, size = 6, family = fam, fontface = "bold") +
  scale_fill_manual(values = c(base = navy, dest = naranja, resto = color_gris), guide = "none") +
  scale_x_continuous(limits = c(0, 1.15), expand = c(0, 0), breaks = seq(0, 1, 0.25),
                     labels = label_number(accuracy = 0.01, decimal.mark = ",")) +
  labs(x = "Eta cuadrado (proporción de la varianza explicada)", y = NULL) +
  theme_slide() + theme(panel.grid.major.y = element_blank())
guardar_png(p, "eta2.png", ancho = 8.1, alto = 4.88, k = 1.45,
            titulo = paste0("País y ocupación explican el ", coma((eta_pais + eta_ocup) * 100, 0), "% de la varianza de la exposición"),
            subtitulo = "Proporción de la varianza de la exposición media que explica cada factor (eta cuadrado). ANOVA de dos factores, 124 países × 923 ocupaciones.",
            nota = "País y ocupación suman porque el diseño es balanceado; el residuo reúne la interacción y el error. El grupo SOC se muestra como referencia.")

# ---- Supuestos: residuos del modelo aditivo (diapositiva 11) ----
dat <- tibble(res = res_muestra)
p_qq <- ggplot(dat, aes(sample = res)) +
  stat_qq(color = navy, alpha = 0.35, size = 1.3) + stat_qq_line(color = naranja, linewidth = 1.2) +
  labs(title = "QQ-plot", x = "Cuantiles teóricos (normal)", y = "Cuantiles de los residuos") + theme_panel()
p_hist <- ggplot(dat, aes(res)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60, fill = navy, color = color_fondo_claro, linewidth = 0.4) +
  stat_function(fun = dnorm, args = list(mean = mean(dat$res), sd = sd(dat$res)), color = naranja, linewidth = 1.3) +
  labs(title = "Histograma", x = "Residuo del modelo país + ocupación", y = NULL) + theme_panel()
guardar_png(unir(p_qq + p_hist), "qq.png", ancho = 8.1, alto = 4.88, k = 1.25,
            titulo = "Supuestos del ANOVA: normalidad de los residuos",
            subtitulo = "Residuos del modelo aditivo país + ocupación. Naranja: referencia normal (recta a la izquierda, densidad a la derecha).",
            nota = "Muestra aleatoria de 5.000 residuos (semilla 123) para el gráfico Q-Q y el histograma.")
cat(sprintf("asimetría %.2f | curtosis en exceso %.2f | razón de desvíos %.2f (mín %s, máx %s)\n",
            asimetria, curtosis_exceso, razon_sd,
            names(which.min(sd_res_pais)), names(which.max(sd_res_pais))))

# ---- Supuestos y alternativas robustas (diapositiva 11 y anexo A6) ----
# Shapiro-Wilk sobre la muestra de 5.000 residuos (el test admite hasta 5.000).
# Fligner-Killeen: homogeneidad de varianzas entre países (no supone normalidad).
# Welch (sin suponer varianzas iguales) y Kruskal-Wallis (por rangos) contrastan el efecto país:
# si coinciden con el ANOVA clásico, la desigualdad de varianzas no cambia la conclusión.
sh  <- shapiro.test(res_muestra)
fk  <- fligner.test(mean_exposure ~ iso3, data = panel)
wel <- oneway.test(mean_exposure ~ iso3, data = panel, var.equal = FALSE)
kw  <- kruskal.test(mean_exposure ~ iso3, data = panel)
anova_supuestos <- tibble(
  prueba = c("Shapiro-Wilk (muestra de 5.000 residuos)", "Fligner-Killeen (varianzas iguales entre países)",
             "ANOVA de Welch (efecto país)", "Kruskal-Wallis (efecto país)"),
  estadistico = c(sh$statistic, fk$statistic, wel$statistic, kw$statistic),
  gl = c(NA, fk$parameter, paste(round(wel$parameter, 1), collapse = " y "), kw$parameter),
  p_valor = c(sh$p.value, fk$p.value, wel$p.value, kw$p.value))
write_csv(anova_supuestos, here("output", "tablas", "anova_supuestos.csv")); print(as.data.frame(anova_supuestos))
