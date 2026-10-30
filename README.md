# MATlite

Paquete de R de la asignatura **Métodos Aplicados al Análisis del Entorno Empresarial** (Máster Universitario en Iniciativa Empresarial: Análisis y Estrategias, UCLM).

Acompaña al libro **R-Stars Lite**. Cada paso de las prácticas se hace con **una línea de código**, para que el tiempo se dedique a interpretar los resultados. Los resultados coinciden con los scripts de [R-Stars: La Guía](https://teckel71.github.io/RStars-book/).

## Instalación

```r
install.packages("remotes")                  # solo la primera vez
remotes::install_github("teckel71/MATlite")  # instala también MATrstars
```

## Primeros pasos

```r
library(MATlite)
lite_comprueba()   # ¿está todo instalado?
lite_datos()       # descarga los ficheros de datos del curso
```

Todas las funciones empiezan por `lite_`: al escribirlo en RStudio, el autocompletado las muestra todas.

| Capítulo Lite | Funciones |
|---|---|
| 1–2. Introducción y datos | `lite_comprueba()`, `lite_datos()`, `lite_importa()`, `lite_resumen()` |
| 3. Análisis previo | `lite_na()`, `lite_outliers()`, `lite_describe()`, `lite_correlaciones()` |
| 4. Componentes principales | `lite_acp()`, `lite_acp_ranking()` |
| 5. Clúster jerárquico | `lite_cluster_ward()` |
| 6. Logit binomial | *en desarrollo* |
| 7. Tablas de contingencia | *en desarrollo* |
| 8. Escalado multidimensional | *en desarrollo* |

## Cómo funcionan los resultados

Las funciones de análisis devuelven un objeto que se muestra entero al escribir su nombre, o pieza a pieza con `$`:

```r
componentes <- lite_acp(seleccion_so)
componentes          # todo: KMO, varianza, cargas, retención...
componentes$cargas   # solo las cargas
```

Los scripts de las prácticas están en `inst/scripts/` (y, una vez instalado el paquete, en `system.file("scripts", package = "MATlite")`).
