<div align="center">

# Atlas de automatización con IA

### ¿Qué explica el ingreso de un país sobre cuánto, y de qué forma, lo alcanza la inteligencia artificial?

**Trabajo final · Ciencia de Datos para Economía y Negocios · FCE-UBA · 2.º cuatrimestre de 2026**
Docente: Nicolás Sidicaro · **Grupo 16**

![R](https://img.shields.io/badge/R-4.3-276DC3?logo=r&logoColor=white)
![Países](https://img.shields.io/badge/países-124-E8622C)
![Observaciones](https://img.shields.io/badge/observaciones-114.452-0B1E3D)
![Scripts](https://img.shields.io/badge/scripts-8-2E8F8F)

</div>

---

## Contenido

1. [Integrantes](#1-integrantes)
2. [Objetivo e hipótesis](#2-objetivo-e-hipótesis)
3. [Base de datos](#3-base-de-datos)
4. [Descripción del análisis](#4-descripción-del-análisis)
5. [Estructura del repositorio](#5-estructura-del-repositorio)
6. [Instrucciones de reproducción](#6-instrucciones-de-reproducción)
7. [Qué hace cada script](#7-qué-hace-cada-script)
8. [Conclusiones principales](#8-conclusiones-principales)

---

## 1. Integrantes

| Integrante |
|:---|
| Lautaro González |
| Esteban Quirino |
| Valery Vanegas |

## 2. Objetivo e hipótesis

Estudiar cómo se expone el trabajo de **124 países** a la inteligencia artificial según el Global Automation Atlas, y si el **ingreso** del país ordena esa exposición.

| | Hipótesis | Resultado |
|:---|:---|:---|
| **H1 · Nivel** | A mayor PBI per cápita, mayor porcentaje de tareas expuestas a la IA. | ✅ **Se confirma.** El ingreso explica el 61% de las diferencias (R² = 0,61; n = 74). |
| **H2 · Forma** | El ingreso no predice *cómo* llega la IA: cuántas tareas reemplazaría por cada una que solo asistiría. | ✅ **Se sostiene con matices.** R² < 0,001; con los 122 países sube a 0,24, pero ahí el cociente es inestable. |

> **Alcance.** El trabajo es descriptivo, no causal. La exposición la estima un modelo de lenguaje por tarea, condicionado al contexto de cada país: se describe cómo juzga el modelo, no efectos reales sobre el empleo.

## 3. Base de datos

| | |
|:---|:---|
| **Fuente** | Global Automation Atlas (Garg, Crosta y Baier, 2026) · [automationatlas.org](https://automationatlas.org) · [código y datos](https://github.com/prashgarg/global-automation-atlas) |
| **Unidad de observación** | País × ocupación O\*NET (panel principal) y país (base auxiliar) |
| **Tamaño** | 114.452 filas = 124 países × 923 ocupaciones · 12 variables · sin faltantes ni duplicados |
| **Período** | Una sola foto: estimaciones para 2026 (sin dimensión temporal) |
| **Limitaciones** | No es automatización observada. Cada ocupación pesa igual (no se pondera por empleo). La escala (0 a 3) tiene piso y techo. |
| **Por qué esta base** | Mide la exposición con un mismo método para 124 países y 923 ocupaciones, en el contexto de cada país. La elegimos para cruzarla con salarios (ILOSTAT); como ese cruce no llegó a hacerse, el trabajo se centra en el ingreso de cada país. |

## 4. Descripción del análisis

| Paso | Método | Qué se encontró |
|:---:|:---|:---|
| **1** | **Limpieza** · faltantes y atípicos (regla de Tukey) | El panel no tiene faltantes. Los 251 atípicos de Tukey dentro de cada grupo (0,22%) y las 42 observaciones del techo de la escala (las únicas atípicas con Tukey sobre todo el panel) son valores válidos del modelo y se mantienen (el filtro de países mueve la media en 0,0038). Etiopía y Venezuela, sin clasificación de ingreso, quedan fuera de los análisis con ingreso: 122 países (74 en la regresión). |
| **2** | **ANOVA** de dos factores *(unidad 2)* | País y ocupación explican el 93% de la varianza (η² = 0,44 y 0,49): un país sube o baja todas sus ocupaciones en bloque, así que tiene sentido agrupar países. Welch, Fligner-Killeen y Kruskal-Wallis por país no cambian la conclusión. |
| **3** | **HCPC** · ACP + Ward + k-means *(unidad 3)* | Tres tramos de 23 / 33 / 68 países. Los límites son difusos y el agrupamiento responde sobre todo al nivel de exposición (ARI = 0,87 usando solo la exposición media). ARI entre k = 3 y k = 4: 0,64. El corte automático de FactoMineR cambia con la versión (3 grupos con la 2.9, 9 con la 2.18), por eso k se fija en 3. |
| **4** | **Kruskal-Wallis y χ²** *(unidad 2)* | Los tramos siguen al ingreso: ε² = 0,72 y V de Cramér = 0,79. |
| **5** | **Regresión lineal**, errores HC3 *(unidad 2)* | % de tareas expuestas ~ log(PBI): R² = 0,61 (pendiente 0,038; EE 0,004). Razón reemplazo/asistencia ~ log(PBI): R² < 0,001 (p = 0,96). |

## 5. Estructura del repositorio

```text
atlas_ia_grupo16/
│
├── raw/                  Datos crudos, sin modificar
│   └── country_occupation_panel.parquet panel país × ocupación (114.452 filas)
│
├── auxiliar/             Bases complementarias y archivos de apoyo
│   ├── country_map_panel.csv            base de países del Atlas (ingreso, PBI, población)
│   ├── grupos_ocupacionales.csv         nombres de los 22 grupos SOC
│   ├── nombres_paises_es.csv            nombres de países en español
│   └── fuentes/                         fuente Rajdhani de los gráficos y su licencia (OFL)
│
├── input/                Datos procesados (los generan los scripts)
│   ├── panel_analisis.csv
│   ├── paises_analisis.csv
│   └── paises_tramos.csv
│
├── output/
│   ├── tablas/           18 tablas de resultados (CSV)
│   └── graficos/         15 gráficos de la presentación (PNG, 300 dpi)
│
├── script/               Scripts numerados en orden de ejecución (00 a 08)
├── utils/                Funciones propias, una por archivo, y configuración de gráficos
├── atlas_ia_grupo16.Rproj
└── README.md
```

> **Nota sobre `raw/`.** El panel está en el formato original en que lo publica el Atlas (parquet), sin modificar. Se lee con el paquete `arrow`.

## 6. Instrucciones de reproducción

**Paso 1 · Instalar R y RStudio.** R 4.3 o posterior ([CRAN](https://cran.r-project.org/)) y [RStudio](https://posit.co/download/rstudio-desktop/). No hace falta instalar nada más a mano: ni paquetes ni la fuente de los gráficos.

**Paso 2 · Descargar el repositorio completo.** Dos opciones:

- Desde GitHub: botón verde **Code** → **Download ZIP**, y descomprimir el archivo.
- Con Git:

  ```bash
  git clone https://github.com/Lauti91/CDD_Grupo-16_UBA.git
  ```

Se necesitan todas las carpetas (`raw/`, `auxiliar/`, `script/`, `utils/`), no solo los scripts.

**Paso 3 · Abrir el proyecto.** Doble clic en `atlas_ia_grupo16.Rproj` (o en RStudio: **File → Open Project**). RStudio toma esa carpeta como raíz y todas las rutas son relativas a ella (con `here()`); no hay `setwd()` ni rutas absolutas, así que el nombre de la carpeta no importa.

**Paso 4 · Correr todo.** En la consola de RStudio:

```r
source("script/00_correr_todo.R")
```

La primera vez instala los paquetes que falten (requiere internet y tarda unos minutos): `here`, `arrow`, `tidyverse`, `FactoMineR`, `ggrepel`, `cluster`, `patchwork`, `scales`, `sandwich`, `lmtest`, `ragg` y `systemfonts`. Después corre los scripts 01 a 08 en orden.

**Paso 5 · Verificar.** La corrida termina con el mensaje `Listo: gráficos en output/graficos/ y tablas en output/tablas/`. Quedan regenerados los 3 archivos de `input/`, las 18 tablas de `output/tablas/` y los 15 gráficos de `output/graficos/`.

Cada script también se puede correr solo, en orden: lee lo que dejó guardado el anterior en `input/`.

> ⚠️ **Versión de FactoMineR.** La partición de países en tres tramos (23 / 33 / 68) se verificó con **FactoMineR 2.9 y 2.18**. Las versiones recientes calculan el ACP con un método aproximado que en matrices chicas puede no converger; por eso el script 04 usa `pca_exacta()`, que fuerza la SVD exacta. Si con otra versión la partición no coincide, el script 04 se detiene con un mensaje.
>
> 🔤 **Fuente.** Los gráficos usan [Rajdhani](https://fonts.google.com/specimen/Rajdhani). Si no está instalada, `utils/configuracion_graficos.R` la carga desde `auxiliar/fuentes/` (licencia SIL Open Font License), así que los gráficos salen idénticos a los de la presentación sin instalar nada.

## 7. Qué hace cada script

| # | Script | Qué hace | Lee | Guarda |
|:---:|:---|:---|:---|:---|
| 00 | `00_correr_todo.R` | Corre los ocho scripts en orden | — | — |
| 01 | `01_limpieza_faltantes_outliers.R` | Diagnóstico de faltantes y atípicos, decisión de limpieza, descriptivas antes y después | `raw/`, `auxiliar/` | `input/panel_analisis.csv`, `input/paises_analisis.csv` · `disp.png` · 4 tablas |
| 02 | `02_descriptivas.R` | Histograma, exposición por grupo ocupacional, correlograma (Pearson frente a Spearman), tablas descriptivas | `input/panel_analisis.csv` | `hist.png`, `barras.png`, `corr.png` · 3 tablas |
| 03 | `03_anova_pais_ocupacion.R` | **Método 1.** ANOVA de dos factores (η²) y supuestos (Shapiro-Wilk, Fligner-Killeen, Welch, Kruskal-Wallis) | `input/panel_analisis.csv`, `input/paises_analisis.csv` | `eta2.png`, `qq.png` · 2 tablas |
| 04 | `04_hcpc_tramos_paises.R` | **Método 2.** ACP + HCPC (k = 3), validación, sensibilidad de k y HCPC por composición | `input/paises_analisis.csv` | `input/paises_tramos.csv` · `kdendro.png`, `mapa.png`, `fronteras.png`, `robust.png`, `compos.png` · 3 tablas |
| 05 | `05_kruskal_chi2_tramos_ingreso.R` | **Método 3.** Kruskal-Wallis y χ² de los tramos contra el ingreso | `input/paises_*.csv` | `ingreso.png` · 1 tabla |
| 06 | `06_grafico_exploratorio_nivel_y_tipo.R` | Gráfico exploratorio (instancia 3) | `input/paises_*.csv` | `expl.png` · 1 tabla |
| 07 | `07_regresion_y_grafico_comunicativo.R` | **Método 4.** Regresión con errores HC3, supuestos, sensibilidad y gráfico comunicativo (instancia 3) | `input/paises_*.csv` | `comu.png` · 2 tablas |
| 08 | `08_brecha_por_grupo_ocupacional.R` | Brecha de exposición entre tramos alto y bajo por grupo de ocupaciones y test de permutación de la interacción tramo × grupo (anexo) | `input/*.csv` | `soc.png` · 2 tablas |

**Gráficos.** Todos los gráficos están en español, con título, subtítulo, ejes, leyendas, fuente y notas al pie dentro de la imagen. `guardar_png(p, nombre, ancho, alto, k, titulo, subtitulo, nota)` agrega esos textos y exporta con `ggsave()` a 300 dpi; el tamaño (`ancho`, `alto`, `k`) es el de la diapositiva, de modo que la fuente se lee al 100%.

**Funciones propias (`utils/`, una por archivo):** `coma`, `nombre_es`, `ari`, `ordenar_clusters`, `pca_exacta`, `cargar_nombres_soc`, `leer_paises_con_tramos`, `unir`, `theme_slide`, `theme_panel`, `guardar_png`.
`configuracion_graficos.R` no es una función: define la paleta y los textos fijos de los gráficos.

## 8. Conclusiones principales

- 📈 El ingreso ordena **cuánto** se expone un país a la IA (R² = 0,61), pero casi no explica **de qué forma** llega (R² < 0,001).
- 🧭 Los tramos de países son cortes de un continuo, no grupos separados. Clasificar por composición de la exposición separa países de nivel parecido (ARI = 0,44 frente al nivel).
- ⚖️ Todo describe cómo el modelo de lenguaje juzga las tareas; no hay causalidad.
- 🔜 **Próximos pasos:** sumar salarios (ILOSTAT, ISCO-08), ponderar por empleo, entender la bimodalidad del cociente y probar otros cortes y otro modelo de lenguaje.

---

<div align="center">

*Grupo 16 · FCE-UBA · 2026*

</div>
