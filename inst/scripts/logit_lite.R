## =============================================================================
## R-Stars Lite. Capítulo 6. Modelo logit binomial.
## Licencias de operación en la ruta Coruscant–Arrakis.
## (Equivale a "logit_rstars.R" de R-Stars: La Guía, capítulo 10)
## =============================================================================

# Limpiando el Global Environment
rm(list = ls())

# Cargando el paquete de la asignatura
library(MATlite)

## 1. DATOS --------------------------------------------------------------------

LICENCIAS <- lite_importa("licencias_100.xlsx")
lite_resumen(LICENCIAS)

LICENCIAS <- lite_na(LICENCIAS, accion = "eliminar",
                     titulo    = "Modelo logit binomial.",
                     subtitulo = "Licencias de operación - Ruta Coruscant–Arrakis")

# Variables categóricas como factores (la primera categoría es la referencia)
LICENCIAS <- lite_factor(LICENCIAS, EFLO, niveles = c("ANTIGUA", "MADURA", "RENOVADA"))
LICENCIAS <- lite_factor(LICENCIAS, LICENCIA)   # 0 = denegada, 1 = concedida

## 2. ANÁLISIS DESCRIPTIVO PREVIO ---------------------------------------------

descriptivo <- lite_logit_descriptivo(LICENCIAS, LICENCIA,
                                      etiquetas = c("0" = "Denegada", "1" = "Concedida"))
descriptivo

## 3. MUESTRA DE ESTIMACIÓN Y MUESTRA DE TEST ---------------------------------

muestras <- lite_particion(LICENCIAS, n_estimacion = 80, dependiente = LICENCIA)
muestras

## 4. MODELO LOGIT COMPLETO ---------------------------------------------------

completo <- lite_logit(muestras$estimacion, LICENCIA,
                       explicativas = c(SOLVENCIA, IPUNT, TINCID, IFIDE,
                                        FLOTA, RUTAS, EXPERIENCIA, EFLO),
                       test = muestras$test)
completo                   # todo junto

completo$coeficientes      # coeficientes y contraste de Wald individual
completo$wald_factores     # contraste de Wald conjunto para EFLO
completo$odds              # odd ratios
completo$ajuste            # bondad del ajuste
completo$devianza          # análisis de la devianza
completo$aic               # AIC frente al modelo nulo
completo$confusion         # capacidad predictiva en la muestra de estimación
completo$confusion_test    # capacidad predictiva en la muestra de test

## 5. SIMULACIÓN DE ESCENARIOS ------------------------------------------------

lite_logit_escenarios(completo, "escenarios_logit_rstars.xlsx")

## 6. SELECCIÓN AUTOMÁTICA DE VARIABLES ---------------------------------------

reducido <- lite_logit_seleccion(completo)
reducido                   # modelo reducido y comparación con el completo

reducido$comparacion

# Fin del script :)
