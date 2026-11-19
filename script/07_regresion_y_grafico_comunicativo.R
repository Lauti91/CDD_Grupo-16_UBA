# ==============================================================
# 07 - Método 4: regresión lineal (errores HC3) y gráfico comunicativo (instancia 3)
# Países con clasificación de ingreso y al menos 40% de tareas expuestas.
# Lee:   input/paises_analisis.csv, input/paises_tramos.csv
# Guarda: output/tablas/regresion_modelos.csv, regresion_sensibilidad.csv; output/graficos/comu.png
# ==============================================================
library(here)
library(tidyverse)
library(ggrepel)
library(scales)
library(patchwork)
library(sandwich)
library(lmtest)

# Funciones propias y configuración (utils/)
source(here("utils", "configuracion_graficos.R"))
source(here("utils", "coma.R"))
source(here("utils", "theme_slide.R"))
source(here("utils", "theme_panel.R"))
source(here("utils", "unir.R"))
source(here("utils", "guardar_png.R"))
source(here("utils", "leer_paises_con_tramos.R"))
source(here("utils", "nombre_es.R"))

# ---- Entradas: input/paises_analisis.csv (script 01) e input/paises_tramos.csv (script 04) ----
paises <- leer_paises_con_tramos()
paises_ing <- filter(paises, !sin_clasif)      # 122 países con clasificación de ingreso

# ---- Gráfico comunicativo (instancia 3) y regresión con errores HC3 (diapositivas 17 y 18) ----
umbral <- 0.40
d <- paises_ing %>% filter(expuestas >= umbral) %>% mutate(etq = nombre_es(iso3, country_name))
m1 <- lm(expuestas ~ log(gdp_per_capita), d)
m2 <- lm(log(ratio) ~ log(gdp_per_capita), d)
r2_nivel <- summary(m1)$r.squared; r2_tipo <- summary(m2)$r.squared
hc <- function(m) coeftest(m, vcov = vcovHC(m, type = "HC3"))
tab_reg <- bind_rows(
  tibble(modelo = "1 · % de tareas expuestas", n = nobs(m1), pendiente = coef(m1)[2], ee_hc3 = hc(m1)[2, 2], p = hc(m1)[2, 4], r2 = r2_nivel),
  tibble(modelo = "2 · log(razón reemplazo / asistencia)", n = nobs(m2), pendiente = coef(m2)[2], ee_hc3 = hc(m2)[2, 2], p = hc(m2)[2, 4], r2 = r2_tipo))
write_csv(tab_reg, here("output", "tablas", "regresion_modelos.csv")); print(tab_reg)

etq_c2 <- d %>% filter(iso3 %in% c("ARG", "USA", "IND", "CRI", "ECU", "CHN", "JOR"))
tema_c <- function() theme_minimal(base_family = fam, base_size = 10) +
  theme(axis.title = element_text(size = 9.5, color = gris_tx, face = "bold"), axis.text = element_text(size = 9, color = navy),
        panel.grid.minor = element_blank(), panel.grid.major = element_line(color = grilla, linewidth = 0.3),
        plot.title = element_text(size = 12.5, face = "bold", color = navy), plot.margin = margin(4, 8, 4, 4))
base_c2 <- function(y, ylab, r2, ttl, col_lin, ybreaks, ylabels, ylog = FALSE) {
  p <- ggplot(d, aes(gdp_per_capita, .data[[y]])) +
    geom_point(aes(color = income_level), size = 2.2, alpha = 0.95) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = col_lin, linewidth = 1.1) +
    geom_text_repel(data = etq_c2, aes(label = etq), size = 2.9, family = fam, color = navy,
                    min.segment.length = 0, segment.size = 0.2, box.padding = 0.25, seed = 2) +
    annotate("label", x = 1100, y = Inf, vjust = 1.2, hjust = 0, size = 4.2, family = fam, fontface = "bold",
             color = col_lin, fill = "white", label.size = 0,
             label = if (r2 < 0.01) "El ingreso no explica\nnada (R cuadrado < 1%)" else paste0("El ingreso explica\nel ", coma(r2 * 100, 0), "% de las diferencias")) +
    scale_color_manual(values = col_ing, labels = nom_ing, name = "Nivel de ingreso", drop = FALSE) +
    scale_x_log10(breaks = c(1e3, 1e4, 1e5), labels = c("1.000", "10.000", "100.000"), limits = c(1000, 130000)) +
    labs(title = ttl, x = "PBI per cápita (US$, escala log.)", y = ylab) + tema_c()
  if (ylog) p + scale_y_log10(breaks = ybreaks, labels = ylabels, expand = expansion(mult = c(0.03, 0.25)))
  else p + scale_y_continuous(breaks = ybreaks, labels = ylabels, expand = expansion(mult = c(0.03, 0.25)))
}
p_niv  <- base_c2("expuestas", "Tareas expuestas a la IA (%)", r2_nivel, "Cuánto: % de tareas expuestas", naranja,
                  seq(0.4, 0.65, 0.05), label_percent(accuracy = 1))
p_tipo <- base_c2("ratio", "Tareas que podría reemplazar por cada\nuna en que solo asistiría (escala log.)", r2_tipo,
                  "De qué forma: reemplazo por asistencia", navy, c(4, 6, 8, 10, 15, 20, 30), function(x) coma(x, 0), ylog = TRUE)
p <- (p_niv | p_tipo) + plot_layout(guides = "collect") & theme(legend.position = "top", legend.title = element_text(size = 9, face = "bold", color = navy), legend.text = element_text(size = 9, color = navy))
p <- p + plot_annotation(theme = theme(plot.background = element_rect(fill = "white", color = NA),
                                                       plot.margin = margin(8, 10, 6, 10)))
guardar_png(p, "comu.png", w = 8.9, h = 4.1, bg = "white")
cat(sprintf("n = %d países | R2 nivel = %.3f | R2 tipo = %.4f\n", nrow(d), r2_nivel, r2_tipo))

# ---- Supuestos y sensibilidad de la regresión (diapositiva 17 y anexo A6) ----
datos_reg <- function(base, umbral = 0.40) base %>% filter(expuestas >= umbral)
ajustar <- function(d) {
  m1 <- lm(expuestas ~ log(gdp_per_capita), d); m2 <- lm(log(ratio) ~ log(gdp_per_capita), d)
  m3 <- lm(ratio ~ log(gdp_per_capita), d)
  hc <- function(m) coeftest(m, vcov = vcovHC(m, type = "HC3"))
  list(m1 = m1, m2 = m2, m3 = m3, hc1 = hc(m1), hc2 = hc(m2), hc3 = hc(m3))
}
d75 <- datos_reg(paises_ing); f <- ajustar(d75)
cat(sprintf("\nREGRESIÓN n = %d\n", nrow(d75)))
cat(sprintf("M1: constante %.3f pendiente %.4f EE-HC3 %.4f p %.2g R2 %.3f\n", coef(f$m1)[1], coef(f$m1)[2], f$hc1[2, 2], f$hc1[2, 4], summary(f$m1)$r.squared))
cat(sprintf("M2: constante %.3f pendiente %.4f EE-HC3 %.4f p %.2f R2 %.4f\n", coef(f$m2)[1], coef(f$m2)[2], f$hc2[2, 2], f$hc2[2, 4], summary(f$m2)$r.squared))
cat(sprintf("Razón sin log: pendiente %.3f p %.2f R2 %.4f\n", coef(f$m3)[2], f$hc3[2, 4], summary(f$m3)$r.squared))
cat(sprintf("Supuestos M1: Shapiro p %.3f | Breusch-Pagan p %.3f | Cook máx %.2f\n",
            shapiro.test(resid(f$m1))$p.value, bptest(f$m1)$p.value, max(cooks.distance(f$m1))))
cat(sprintf("Supuestos M2: Shapiro p %.2g | Breusch-Pagan p %.2g\n", shapiro.test(resid(f$m2))$p.value, bptest(f$m2)$p.value))
sp <- cor.test(log(d75$gdp_per_capita), log(d75$ratio), method = "spearman", exact = FALSE)
cat(sprintf("Spearman (log PBI, razón): %.2f (p %.3f)\n", sp$estimate, sp$p.value))
cat(sprintf("Razón bimodal: %d países < 8 y %d con 8 o más\n", sum(d75$ratio < 8), sum(d75$ratio >= 8)))
completa <- paises_ing; fc <- ajustar(completa)
cat(sprintf("Muestra completa (n = %d): R2 M1 %.3f | R2 M2 %.3f | razón sin log R2 %.3f\n", nrow(completa),
            summary(fc$m1)$r.squared, summary(fc$m2)$r.squared, summary(fc$m3)$r.squared))
write_csv(tibble(
  especificacion = c("% expuesto ~ log PBI", "log(razón) ~ log PBI", "razón (sin log) ~ log PBI",
                     "% expuesto ~ log PBI (muestra completa)", "log(razón) ~ log PBI (muestra completa)"),
  paises = c(nrow(d75), nrow(d75), nrow(d75), nrow(completa), nrow(completa)),
  r2 = c(summary(f$m1)$r.squared, summary(f$m2)$r.squared, summary(f$m3)$r.squared, summary(fc$m1)$r.squared, summary(fc$m2)$r.squared),
  pendiente = c(coef(f$m1)[2], coef(f$m2)[2], coef(f$m3)[2], NA, NA),
  p = c(f$hc1[2, 4], f$hc2[2, 4], f$hc3[2, 4], NA, NA)), here("output", "tablas", "regresion_sensibilidad.csv"))
# efecto de duplicar el PBI per cápita (modelo 1)
cat(sprintf("Duplicar el PBI: %+.1f puntos de tareas expuestas\n", 100 * coef(f$m1)[2] * log(2)))


