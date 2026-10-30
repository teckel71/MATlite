## =============================================================================
## R-Stars Lite. Capítulo 7. Tablas de contingencia y asociación.
## (Equivale a "contingencia_rstars.R" de R-Stars: La Guía, apartado 11.2)
## =============================================================================

# Limpiando el Global Environment
rm(list = ls())

# Cargando el paquete de la asignatura
library(MATlite)

## DATOS -----------------------------------------------------------------------

interestelar_300 <- lite_importa("interestelar_300.xlsx")
seleccion <- interestelar_300 %>% select(GALAXIA, FJUR, EFLO)
lite_resumen(seleccion)

seleccion <- lite_na(seleccion, accion = "eliminar",
                     titulo    = "Tablas de contingencia.",
                     subtitulo = "Transporte de mercancías interestelar")

# Abreviando los nombres de las galaxias (demasiado largos)
seleccion <- lite_recodifica(seleccion, GALAXIA,
  c("Gran Nube de Magallanes"    = "GN Magallanes",
    "Pequeña Nube de Magallanes" = "PN Magallanes",
    "Galaxia de Andrómeda"       = "Andrómeda",
    "Galaxia del Triángulo"      = "Triángulo"))

## GALAXIA x FJUR ---------------------------------------------------------------

galaxia_fjur <- lite_contingencia(seleccion, GALAXIA, FJUR,
                                  titulo    = "Galaxia y Forma Jurídica",
                                  subtitulo = "Empresas TMI")
galaxia_fjur               # todo junto

galaxia_fjur$tabla         # tabla de contingencia
galaxia_fjur$mosaico       # gráfico de mosaico
galaxia_fjur$barras        # frecuencias marginales
galaxia_fjur$chi2          # contraste de independencia
galaxia_fjur$esperadas     # frecuencias esperadas bajo independencia
galaxia_fjur$residuos      # residuos estandarizados ajustados
galaxia_fjur$asociacion    # gráfico de asociación
galaxia_fjur$medidas       # medidas de asociación (C, V de Cramér)

## FJUR x EFLO ------------------------------------------------------------------

fjur_eflo <- lite_contingencia(seleccion, FJUR, EFLO,
                               titulo    = "Forma Jurídica y Estado de la Flota",
                               subtitulo = "Empresas TMI")
fjur_eflo                  # todo junto

fjur_eflo$tabla
fjur_eflo$mosaico
fjur_eflo$chi2
fjur_eflo$asociacion
fjur_eflo$medidas

# Fin del script :)
