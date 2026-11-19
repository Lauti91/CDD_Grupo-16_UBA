# ==============================================================
# 05 - Método 3: Kruskal-Wallis y chi cuadrado (tramos del HCPC vs. ingreso)
# Solo países con clasificación de ingreso (122).
# Lee:   input/paises_analisis.csv, input/paises_tramos.csv
# Guarda: output/tablas/ingreso_kw_chi2.csv; output/graficos/ingreso.png
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
source(here("utils", "nombre_es.R"))

# ---- Entradas: input/paises_analisis.csv (script 01) e input/paises_tramos.csv (script 04) ----
paises <- leer_paises_con_tramos()
paises_ing <- filter(paises, !sin_clasif)      # 122 países con clasificación de ingreso

base <- paises_ing %>% filter(is.finite(log_gdp_pc))
n_cl <- table(base$tramo)
kw <- kruskal.test(log_gdp_pc ~ tramo, data = base)
eps2 <- unname(kw$statistic) / (nrow(base) - 1)
tab <- table(base$tramo, base$income_level)
set.seed(1); chi <- suppressWarnings(chisq.test(tab, simulate.p.value = TRUE, B = 10000))
cramer <- sqrt(unname(chi$statistic) / (sum(tab) * (min(dim(tab)) - 1)))
write_csv(tibble(n_paises = nrow(base), n_bajo = n_cl[1], n_medio = n_cl[2], n_alto = n_cl[3],
                 eps2_kw = eps2, p_kw = kw$p.value, cramer_v = cramer, p_chi2_sim = chi$p.value),
          here("output", "tablas", "ingreso_kw_chi2.csv"))
cat(sprintf("n = %d (%s) | KW eps2 = %.3f | V de Cramer = %.3f\n", nrow(base), paste(n_cl, collapse = "/"), eps2, cramer))

set.seed(42)
p <- ggplot(base, aes(tramo, log_gdp_pc)) +
  geom_boxplot(aes(fill = tramo), width = 0.55, alpha = 0.35, outlier.shape = NA, color = navy, linewidth = 0.7) +
  geom_jitter(aes(color = income_level), width = 0.18, height = 0, size = 3.2, alpha = 0.95) +
  scale_fill_manual(values = col_nivel, guide = "none") +
  scale_color_manual(values = col_ing, labels = nom_ing, name = "Nivel de ingreso") +
  scale_x_discrete(labels = function(x) paste0(nom_nivel[x], "\n(n = ", n_cl[x], ")")) +
  labs(x = NULL, y = "log(PBI per cápita)") +
  theme_slide() + theme(panel.grid.major.x = element_blank())
guardar_png(p, "ingreso.png", w = 14, h = 6.54)

# ---- Supuestos del chi cuadrado y países fuera del patrón ----
base <- paises_ing
tab <- table(base$tramo, base$income_level)
esperadas <- suppressWarnings(chisq.test(tab)$expected)
patron <- (base$tramo == "1" & base$income_level %in% c("Low income", "Lower middle income")) |
          (base$tramo == "2" & base$income_level == "Lower middle income") |
          (base$tramo == "3" & base$income_level %in% c("Upper middle income", "High income"))
cat(sprintf("\nKW/χ²: n = %d | celdas con esperada < 5: %d | países fuera del patrón: %d (%s)\n", nrow(base), sum(esperadas < 5),
            sum(!patron), paste(nombre_es(base$iso3[!patron], base$country_name[!patron]), collapse = ", ")))
print(tab)


