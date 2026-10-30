## =============================================================================
## R-Stars Lite. Capítulo 3. Análisis previo de datos.
## (Equivale a "previo_rstars.R" de R-Stars: La Guía, capítulo 5)
## =============================================================================

# Limpiando el Global Environment
rm(list = ls())

# Cargando el paquete de la asignatura (carga todo lo necesario)
library(MATlite)

# Importando datos desde Excel
interestelar_100 <- lite_importa("interestelar_100.xlsx")
lite_resumen(interestelar_100)

## ANÁLISIS DE UNA VARIABLE: RENECO -------------------------------------------

# Valores ausentes: diagnóstico y eliminación
muestra <- lite_na(interestelar_100, RENECO, accion = "eliminar",
                   titulo    = "Rentabilidad Económica: valores ausentes",
                   subtitulo = "Transporte de mercancías interestelar")

# Outliers: diagnóstico y eliminación (regla 1,5·IQR)
muestra_so <- lite_outliers(muestra, RENECO, accion = "eliminar",
                            titulo    = "Rentabilidad Económica",
                            subtitulo = "Empresas de transporte interestelar")

# Descripción completa de la variable
rentabilidad <- lite_describe(muestra_so, RENECO,
                              etiqueta  = "Rentabilidad Económica (%)",
                              titulo    = "Rentabilidad Económica",
                              subtitulo = "Empresas TMI (sin outliers)")
rentabilidad               # todo junto

rentabilidad$frecuencias   # tabla de frecuencias
rentabilidad$estadisticos  # estadísticos
rentabilidad$normalidad    # contraste de Shapiro-Wilk
rentabilidad$resumen       # los 4 gráficos básicos

## ANÁLISIS DE MÚLTIPLES VARIABLES: RENECO, ACTIVO, MARGEN, RES ---------------

muestra2 <- lite_na(interestelar_100, c(RENECO, ACTIVO, MARGEN, RES),
                    accion    = "eliminar",
                    titulo    = "Variables económico-financieras: valores ausentes",
                    subtitulo = "Transporte de mercancías interestelar")

# Outliers multivariantes (distancia de Mahalanobis)
muestra2_so <- lite_outliers(muestra2, c(RENECO, ACTIVO, MARGEN, RES),
                             accion    = "eliminar",
                             titulo    = "DISTANCIA DE MAHALANOBIS",
                             subtitulo = "RENECO, ACTIVO, MARGEN, RES. Empresas TMI.")

# Correlaciones sin outliers, comparadas con las obtenidas con outliers
correlaciones <- lite_correlaciones(muestra2_so, c(RENECO, ACTIVO, MARGEN, RES),
                                    comparar_con = muestra2)
correlaciones

# Fin del script :)
