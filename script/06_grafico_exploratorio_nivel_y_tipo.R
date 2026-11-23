# ==============================================================
# 06 - Gráfico exploratorio (instancia 3): cuánto y de qué forma se exponen los países
# Lee:   input/paises_analisis.csv, input/paises_tramos.csv, auxiliar/nombres_paises_es.csv
# Guarda: output/tablas/exploratorio_medianas.csv; output/graficos/expl.png
# ==============================================================
library(here)
library(tidyverse)
library(ggrepel)
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

# ---- Entradas (scripts 01 y 04) ----
paises <- leer_paises_con_tramos()
paises_ing <- filter(paises, !sin_clasif)      # 122 países con clasificación de ingreso

d <- paises_ing %>% mutate(etq = nombre_es(iso3, country_name))
stopifnot(all(d$aug_only_share > 0))
med_x <- median(d$expuestas); med_y <- median(d$ratio)
write_csv(tibble(n = nrow(d), mediana_expuestas = med_x, mediana_cociente = med_y), here("output", "tablas", "exploratorio_medianas.csv"))
# Rótulos: posición manual (lx = % expuestas, ly = cociente); línea hasta el borde del texto
pos_etq <- tibble::tribble(
  ~iso3, ~lx, ~ly,
  "BRA", 0.569, 12.095, "CHL", 0.5209, 9.055, "DEU", 0.5759, 7.968,
  "RUS", 0.5355, 14.299, "USA", 0.5977, 14.04, "ARG", 0.5008, 8.2,
  "MEX", 0.4949, 14.484, "IND", 0.4722, 9.543, "KOR", 0.625, 6.974,
  "CRI", 0.5839, 4.197, "SVK", 0.6418, 4.807, "DOM", 0.488, 7.15,
  "JOR", 0.519, 4.6, "CHN", 0.646, 11.75, "ECU", 0.4605, 4.6,
  "NGA", 0.334, 14.26, "MMR", 0.256, 31.162, "NER", 0.1026, 26.883,
  "SSD", 0.0803, 19.967, "YEM", 0.1814, 49.508, "SEN", 0.337, 5.55)
etq1 <- inner_join(filter(d, iso3 %in% c("IND", "ARG", "CRI", "USA", "CHN", "NER", "SEN")), pos_etq, by = "iso3") %>% mutate(
  dxl = expuestas - lx, dyl = log10(ratio) - log10(ly),
  hw = 0.00604 * nchar(etq) / 2 + 0.001, hh = 9.5 / 539 + 0.002,
  tt = pmin(1, pmin(hw / pmax(abs(dxl), 1e-9), hh / pmax(abs(dyl), 1e-9))),
  ex = lx + dxl * tt, ey = 10^(log10(ly) + dyl * tt))
stopifnot(nrow(etq1) == 7)
p <- ggplot(d, aes(expuestas, ratio)) +
  geom_vline(xintercept = med_x, linetype = "dashed", color = gris_md, linewidth = 0.4) +
  geom_hline(yintercept = med_y, linetype = "dashed", color = gris_md, linewidth = 0.4) +
  geom_vline(xintercept = 0.40, color = naranja, linewidth = 0.6, linetype = "dashed") +
  annotate("text", x = 0.405, y = 50, hjust = 0, vjust = 1, size = 2.9, family = fam, fontface = "bold",
           color = naranja, lineheight = 0.9, label = "Corte del gráfico\ncomunicativo (40%)") +
  annotate("text", x = 0.04, y = 50, hjust = 0, vjust = 1, size = 2.8, family = fam, color = gris_tx,
           label = "Poca exposición,\nsobre todo reemplazo") +
  annotate("text", x = 0.64, y = 50, hjust = 1, vjust = 1, size = 2.8, family = fam, color = gris_tx,
           label = "Mucha exposición,\nsobre todo reemplazo") +
  annotate("text", x = 0.04, y = 3.35, hjust = 0, vjust = 0, size = 2.8, family = fam, color = gris_tx,
           label = "Poca exposición, más asistencia") +
  annotate("text", x = 0.65, y = 3.35, hjust = 1, vjust = 0, size = 2.8, family = fam, color = gris_tx,
           label = "Mucha exposición, más asistencia") +
  geom_point(aes(fill = income_level, size = population), shape = 21, color = "white", stroke = 0.3, alpha = 0.95) +
  geom_segment(data = etq1, aes(x = expuestas, y = ratio, xend = ex, yend = ey), color = gris_tx, linewidth = 0.25) +
  geom_text(data = etq1, aes(x = lx, y = ly, label = etq), size = 3.0, family = fam, fontface = "bold", color = navy) +
  scale_x_continuous(labels = label_percent(accuracy = 1), limits = c(0.02, 0.65), breaks = seq(0, 0.6, 0.1)) +
  scale_y_log10(breaks = c(5, 7.5, 10, 15, 20, 30, 50), labels = c("5", "7,5", "10", "15", "20", "30", "50"), limits = c(3.3, 52)) +
  scale_fill_manual(values = col_ing, labels = nom_ing, name = "Nivel de ingreso") +
  scale_size_continuous(range = c(0.8, 4.6), trans = "sqrt", breaks = c(1e7, 1e8, 1e9), labels = c("10 M", "100 M", "1.000 M"), name = "Población") +
  guides(fill = guide_legend(override.aes = list(size = 3.5), order = 1),
         size = guide_legend(order = 2, override.aes = list(fill = gris_md, color = gris_md))) +
  labs(x = "Tareas expuestas a la IA (% del total de tareas del país)",
       y = "Tareas que la IA podría reemplazar\npor cada una en que solo asistiría (escala log.)") +
  theme_minimal(base_family = fam, base_size = 10) +
  theme(axis.title = element_text(size = 9.5, color = gris_tx, face = "bold"), axis.text = element_text(size = 9, color = navy),
        panel.grid.minor = element_blank(), panel.grid.major = element_line(color = grilla, linewidth = 0.3),
        legend.position = "right", legend.title = element_text(size = 9, face = "bold", color = navy),
        legend.text = element_text(size = 8.5, color = navy),
        plot.background = element_rect(fill = "white", color = NA), plot.margin = margin(10, 12, 8, 12))
guardar_png(p, "expl.png", ancho = 8.9, alto = 4.95, k = 0.9, bg = "white",
            titulo = "Nivel y tipo de exposición a la IA por país",
            subtitulo = paste0("Tareas expuestas (x) frente a tareas que la IA podría reemplazar por cada una en que solo asistiría (y, escala log.). ", nrow(d), " países; color: nivel de ingreso; tamaño: población."),
            nota = paste0("Reemplazar = solo sustitución + ambas; solo asistir = solo aumentación. Líneas grises: medianas. ",
                          "La exposición la estima un modelo de lenguaje por tarea; no se pondera por empleo. Se excluyen Etiopía y Venezuela (sin clasificación de ingreso)."))
cat(sprintf("n = %d | mediana %% expuestas = %.1f%% | mediana cociente = %.1f a 1\n", nrow(d), med_x * 100, med_y))
print(d %>% filter(iso3 %in% c("CRI", "USA")) %>% select(iso3, expuestas, ratio))
