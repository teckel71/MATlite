## =============================================================================
## R-Stars Lite. Capítulo 5. Análisis clúster jerárquico (Ward).
## (Equivale a "cluster_rstars.R" de R-Stars: La Guía, capítulo 7)
## =============================================================================

# Limpiando el Global Environment
rm(list = ls())

# Cargando el paquete de la asignatura
library(MATlite)

# Importando datos y seleccionando las variables clasificadoras
interestelar_25 <- lite_importa("interestelar_25.xlsx")
seleccion <- interestelar_25 %>% select(IDIVERSE, IFIDE, IDIG)
lite_resumen(seleccion)

# Valores ausentes
seleccion <- lite_na(seleccion, accion = "eliminar")

# Outliers: solo se documentan (no se eliminan)
lite_outliers(seleccion,
              titulo    = "DISTANCIA DE MAHALANOBIS",
              subtitulo = "IDIVERSE, IFIDE, IDIG. Empresas TMI.")

# Fase 1. Exploración: distancias, dendrograma y número de grupos sugerido
exploracion <- lite_cluster_ward(seleccion)
exploracion

# Fase 2. Resultados con el número de grupos decidido
grupos <- lite_cluster_ward(seleccion, k = 5, subtitulo_graficos = "Empresas TMI.")
grupos                       # todo junto

grupos$dendrograma           # dendrograma con los grupos
grupos$centroides            # medias de cada grupo
grupos$graficos_centroides   # gráficos de medias
grupos$composicion           # empresas de cada grupo
grupos$dispersion            # gráficos de dispersión por pares de variables

# Data frame con el grupo de cada empresa
seleccion_grupos <- grupos$datos

# Fin del script :)
