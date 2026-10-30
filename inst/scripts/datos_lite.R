## =============================================================================
## R-Stars Lite. Capítulo 2. Almacenando y manipulando datos.
## (Equivale a "explora_rstars.R" de R-Stars: La Guía, capítulo 2)
## =============================================================================

# Limpiando el Global Environment
rm(list = ls())

# Cargando el paquete de la asignatura
library(MATlite)

## DATOS -----------------------------------------------------------------------

# Importando datos desde Excel (hoja "Datos"; "n.d." se lee como NA)
interestelar_100 <- lite_importa("interestelar_100.xlsx")

# Resumen visual de las variables
lite_resumen(interestelar_100)

## MANIPULANDO DATOS -----------------------------------------------------------

# Seleccionando variables
interestelar_100A <- select(interestelar_100, ACTIVO, FPIOS, LIQUIDEZ)
interestelar_100A

# Eliminando una variable
interestelar_100B <- select(interestelar_100, -FJUR)

# Ordenando casos (de menor a mayor y de mayor a menor)
interestelar_100C <- select(interestelar_100, RENECO, EFLO, ACTIVO)
interestelar_100C <- arrange(interestelar_100C, RENECO)
interestelar_100C
interestelar_100D <- arrange(interestelar_100C, desc(RENECO))
interestelar_100D

# El operador pipe: seleccionar y ordenar en una sola secuencia
interestelar_100C <- interestelar_100 %>%
  select(RENECO, EFLO, ACTIVO) %>%
  arrange(RENECO)

# Seleccionando casos
interestelar_100 %>%
  filter(RES >= 500) %>%
  select(RES, RENECO)

interestelar_100 %>%
  filter(RES >= 500 & RENECO < 40) %>%
  select(RES, RENECO, ACTIVO)

# Creando variables a partir de otras variables
interestelar_100 <- mutate(interestelar_100, RATIO = RES / ACTIVO)
select(interestelar_100, RES, ACTIVO, RATIO)

# Fin del script :)
