## =============================================================================
## R-Stars Lite. Capítulo 8. Escalado multidimensional no métrico.
## Mapa perceptual del sector interestelar.
## (Equivale a "emd_rstars.R" de R-Stars: La Guía, capítulo 12)
## =============================================================================

# Limpiando el Global Environment
rm(list = ls())

# Cargando el paquete de la asignatura
library(MATlite)

## DATOS -----------------------------------------------------------------------

# Valoraciones medias (Likert 1-7) de 15 compañías en 6 atributos de servicio
PERCEPCION <- lite_importa("percepcion_interestelar.xlsx")
lite_resumen(PERCEPCION)

## EXPLORACIÓN Y NÚMERO DE DIMENSIONES -----------------------------------------

exploracion <- lite_emd_dimensiones(PERCEPCION)
exploracion

exploracion$tabla_perfiles  # valoraciones de cada compañía
exploracion$perfiles        # perfiles (coordenadas paralelas)
exploracion$distancias      # matriz de distancias
exploracion$stress          # Stress para 1 a 6 dimensiones
exploracion$codo            # gráfico de codo

## SOLUCIÓN EN DOS DIMENSIONES ---------------------------------------------------

mapa <- lite_emd(PERCEPCION, ndim = 2,
                 titulo    = "SECTOR INTERESTELAR DE TRANSPORTE DE MERCANCÍAS",
                 subtitulo = "Mapa perceptual - Configuración de opiniones de clientes")
mapa                        # todo junto

mapa$ajuste                 # Stress de la solución
mapa$coordenadas            # coordenadas de las compañías
mapa$shepard                # diagrama de Shepard
mapa$mapa                   # mapa perceptual
mapa$correlaciones          # interpretación de las dimensiones
mapa$biplot                 # mapa con los vectores de los atributos

## AMPLIACIÓN: SOLUCIÓN EN TRES DIMENSIONES -------------------------------------

mapa3 <- lite_emd(PERCEPCION, ndim = 3)
mapa3$correlaciones
mapa3$mapas

# Fin del script :)
