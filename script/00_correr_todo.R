# ==============================================================
# 00 - Correr todo el proyecto en orden
# Cada script es autónomo (lee lo que dejó guardado el anterior), así que también
# se puede correr uno solo. Abrir antes el .Rproj (o confiar en here()).
# ==============================================================
library(here)
scripts <- c("01_limpieza_faltantes_outliers.R", "02_descriptivas.R", "03_anova_pais_ocupacion.R",
             "04_hcpc_tramos_paises.R", "05_kruskal_chi2_tramos_ingreso.R",
             "06_grafico_exploratorio_nivel_y_tipo.R", "07_regresion_y_grafico_comunicativo.R",
             "08_brecha_por_grupo_ocupacional.R")
for (s in scripts) {
  message("\n>>> ", s)
  source(here("script", s), local = new.env(), encoding = "UTF-8")
}
message("\nListo: gráficos en output/graficos/ y tablas en output/tablas/")
