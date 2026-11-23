# ==============================================================
# 07 - Método 4: regresión lineal (errores HC3) y gráfico comunicativo (instancia 3)
# Países con ingreso clasificado y >= 40% de tareas expuestas
# Lee:   input/paises_analisis.csv, input/paises_tramos.csv, auxiliar/nombres_paises_es.csv
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

# ---- Entradas (scripts 01 y 04) ----
paises <- leer_paises_con_tramos()
paises_ing <- filter(paises, !sin_clasif)      # 122 países con clasificación de ingreso

# ---- Regresión con errores HC3 y gráfico comunicativo (comu.png) ----
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

# Versión comunicativa: 4 casos destacados, resto en gris
casos <- c("IND", "ARG", "CRI", "USA")
gris_pt <- "#C3C9D2"
tema_c <- function() theme_minimal(base_family = fam, base_size = 10) +
  theme(axis.title = element_text(size = 9.5, color = gris_tx, face = "bold"), axis.text = element_text(size = 9, color = navy),
        panel.grid.minor = element_blank(), panel.grid.major = element_line(color = grilla, linewidth = 0.3),
        plot.title = element_text(size = 12.5, face = "bold", color = navy), plot.margin = margin(4, 8, 4, 4))
base_c3 <- function(y, ylab, ttl, nota_panel, ybreaks, ylabels, ylog = FALSE, pos, ypx) {
  # pos: posición manual de cada rótulo (iso3, lx, ly)
  dc <- inner_join(filter(d, iso3 %in% casos), pos, by = "iso3")
  # Línea hasta el borde del rótulo; ypx = unidades de y por píxel (log10 si ylog)
  fy <- if (ylog) log10 else identity; fyi <- if (ylog) function(v) 10^v else identity
  dc <- dc %>% mutate(
    dx = (log10(gdp_per_capita) - log10(lx)), dy = fy(.data[[y]]) - fy(ly),
    hw = (nchar(etq) * 4.1 + 3) * 0.00273, hh = 11 * ypx,
    tt = pmin(1, pmin(hw / pmax(abs(dx), 1e-9), hh / pmax(abs(dy), 1e-9))),
    ex = 10^(log10(lx) + dx * tt), ey = fyi(fy(ly) + dy * tt))
  p <- ggplot(d, aes(gdp_per_capita, .data[[y]])) +
    geom_point(color = gris_pt, size = 1.6) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = naranja, linewidth = 1.2) +
    geom_segment(data = dc, aes(xend = ex, yend = ey), color = gris_tx, linewidth = 0.3) +
    geom_point(data = dc, color = navy, size = 2.8) +
    geom_text(data = dc, aes(x = lx, y = ly, label = etq), size = 3.6, family = fam, color = navy, fontface = "bold") +
    annotate("text", x = 1100, y = Inf, vjust = 1.4, hjust = 0, size = 3.7, family = fam, fontface = "bold", color = naranja, label = nota_panel) +
    scale_x_log10(breaks = c(1e3, 1e4, 1e5), labels = c("1.000", "10.000", "100.000"), limits = c(1000, 130000)) +
    labs(title = ttl, x = "PBI per cápita (US$)", y = ylab) + tema_c()
  if (ylog) p + scale_y_log10(breaks = ybreaks, labels = ylabels, expand = expansion(mult = c(0.03, 0.25)))
  else p + scale_y_continuous(breaks = ybreaks, labels = ylabels, expand = expansion(mult = c(0.03, 0.25)))
}
p_niv  <- base_c3("expuestas", "Tareas expuestas a la IA (%)", "Cuánto: % de tareas expuestas",
                  "Más ingreso, más tareas expuestas", seq(0.4, 0.65, 0.05), label_percent(accuracy = 1),
                  pos = tibble(iso3 = c("IND", "ARG", "CRI", "USA"), lx = c(1700, 30000, 8500, 58000), ly = c(0.455, 0.46, 0.635, 0.64)), ypx = 0.00038)
p_tipo <- base_c3("ratio", "Tareas que podría reemplazar por cada\nuna en que solo asistiría", "De qué forma: reemplazo por asistencia",
                  "El ingreso no cambia la forma", c(4, 6, 8, 10, 15, 20, 30), function(x) coma(x, 0), ylog = TRUE,
                  pos = tibble(iso3 = c("IND", "ARG", "CRI", "USA"), lx = c(1900, 14000, 18500, 52000), ly = c(14, 15.5, 4.1, 14.5)), ypx = 0.00106)
p <- (p_niv | p_tipo)
p <- p + plot_annotation(theme = theme(plot.background = element_rect(fill = "white", color = NA),
                                                       plot.margin = margin(8, 10, 6, 10)))
guardar_png(p, "comu.png", ancho = 8.9, alto = 4.95, k = 0.9, bg = "white",
            titulo = "El ingreso predice cuánto se expone un país a la IA, no de qué forma",
            subtitulo = paste0(nrow(d), " países con al menos 40% de tareas expuestas. Cada punto es un país; destacamos cuatro casos."),
            nota = "Reemplazar = solo sustitución + ambas; solo asistir = solo aumentación. Se excluyen los países con menos de 40% de tareas expuestas, donde casi no hay asistencia pura.")
cat(sprintf("n = %d países | R2 nivel = %.3f | R2 tipo = %.4f\n", nrow(d), r2_nivel, r2_tipo))

# ---- Supuestos y sensibilidad de la regresión (tabla regresion_sensibilidad.csv) ----
datos_reg <- function(base, umbral = 0.40) base %>% filter(expuestas >= umbral)
ajustar <- function(d) {
  m1 <- lm(expuestas ~ log(gdp_per_capita), d); m2 <- lm(log(ratio) ~ log(gdp_per_capita), d)
  m3 <- lm(ratio ~ log(gdp_per_capita), d)
  hc <- function(m) coeftest(m, vcov = vcovHC(m, type = "HC3"))
  list(m1 = m1, m2 = m2, m3 = m3, hc1 = hc(m1), hc2 = hc(m2), hc3 = hc(m3))
}
d74 <- datos_reg(paises_ing); f <- ajustar(d74)
cat(sprintf("\nREGRESIÓN n = %d\n", nrow(d74)))
cat(sprintf("M1: constante %.3f pendiente %.4f EE-HC3 %.4f p %.2g R2 %.3f\n", coef(f$m1)[1], coef(f$m1)[2], f$hc1[2, 2], f$hc1[2, 4], summary(f$m1)$r.squared))
cat(sprintf("M2: constante %.3f pendiente %.4f EE-HC3 %.4f p %.2f R2 %.4f\n", coef(f$m2)[1], coef(f$m2)[2], f$hc2[2, 2], f$hc2[2, 4], summary(f$m2)$r.squared))
cat(sprintf("Razón sin log: pendiente %.3f p %.2f R2 %.4f\n", coef(f$m3)[2], f$hc3[2, 4], summary(f$m3)$r.squared))
cat(sprintf("Supuestos M1: Shapiro p %.3f | Breusch-Pagan p %.3f | Cook máx %.2f\n",
            shapiro.test(resid(f$m1))$p.value, bptest(f$m1)$p.value, max(cooks.distance(f$m1))))
cat(sprintf("Supuestos M2: Shapiro p %.2g | Breusch-Pagan p %.2g\n", shapiro.test(resid(f$m2))$p.value, bptest(f$m2)$p.value))
sp <- cor.test(log(d74$gdp_per_capita), log(d74$ratio), method = "spearman", exact = FALSE)
cat(sprintf("Spearman (log PBI, razón): %.2f (p %.3f)\n", sp$estimate, sp$p.value))
cat(sprintf("Razón bimodal: %d países < 8 y %d con 8 o más\n", sum(d74$ratio < 8), sum(d74$ratio >= 8)))
completa <- paises_ing; fc <- ajustar(completa)
cat(sprintf("Muestra completa (n = %d): R2 M1 %.3f | R2 M2 %.3f | razón sin log R2 %.3f\n", nrow(completa),
            summary(fc$m1)$r.squared, summary(fc$m2)$r.squared, summary(fc$m3)$r.squared))
write_csv(tibble(
  especificacion = c("% expuesto ~ log PBI", "log(razón) ~ log PBI", "razón (sin log) ~ log PBI",
                     "% expuesto ~ log PBI (muestra completa)", "log(razón) ~ log PBI (muestra completa)"),
  paises = c(nrow(d74), nrow(d74), nrow(d74), nrow(completa), nrow(completa)),
  r2 = c(summary(f$m1)$r.squared, summary(f$m2)$r.squared, summary(f$m3)$r.squared, summary(fc$m1)$r.squared, summary(fc$m2)$r.squared),
  pendiente = c(coef(f$m1)[2], coef(f$m2)[2], coef(f$m3)[2], NA, NA),
  p = c(f$hc1[2, 4], f$hc2[2, 4], f$hc3[2, 4], NA, NA)), here("output", "tablas", "regresion_sensibilidad.csv"))
# Efecto de duplicar el PBI per cápita (modelo 1)
cat(sprintf("Duplicar el PBI: %+.1f puntos de tareas expuestas\n", 100 * coef(f$m1)[2] * log(2)))


