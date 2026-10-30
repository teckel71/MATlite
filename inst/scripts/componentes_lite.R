## =============================================================================
## R-Stars Lite. Capítulo 4. Componentes principales.
## (Equivale a "componentes_rstars.R" de R-Stars: La Guía, capítulo 6)
## =============================================================================

# Limpiando el Global Environment
rm(list = ls())

# Cargando el paquete de la asignatura
library(MATlite)

# Importando datos y seleccionando las variables del análisis
interestelar_100 <- lite_importa("interestelar_100.xlsx")
seleccion <- interestelar_100 %>% select(IDIVERSE, IFIDE, IDIG)
lite_resumen(seleccion)

# Valores ausentes y outliers (Mahalanobis)
seleccion <- lite_na(seleccion, accion = "eliminar",
                     titulo    = "Indicadores: Diversificación, Fidelidad, Digitalización",
                     subtitulo = "Transporte de mercancías interestelar")

seleccion_so <- lite_outliers(seleccion, accion = "eliminar",
                              titulo    = "DISTANCIA DE MAHALANOBIS",
                              subtitulo = "IDIVERSE, IFIDE, IDIG. Empresas TMI.")

# Correlaciones
lite_correlaciones(seleccion_so, titulo = "Matriz de Correlación sin outliers")

# Componentes principales
componentes <- lite_acp(seleccion_so, subtitulo = "Empresas TMI")
componentes               # todo junto

componentes$kmo           # adecuación muestral (KMO y MSA)
componentes$varianza      # varianza explicada
componentes$cargas        # cargas
componentes$retencion     # cuántas componentes retener (autovalor > 1)

# Puntuaciones (scores) y ranking
lite_acp_ranking(componentes)                     # casos sin outliers
lite_acp_ranking(componentes, datos = seleccion)  # puntuando también los outliers

# Efecto de los outliers: componentes calculadas CON outliers
componentes2 <- lite_acp(seleccion, subtitulo = "Empresas TMI (con outliers)")
lite_acp_ranking(componentes2)

# Fin del script :)
