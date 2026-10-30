#' MATlite: funciones simplificadas para el libro R-Stars Lite
#'
#' Paquete de la asignatura *Métodos Aplicados al Análisis del Entorno
#' Empresarial*. Todas sus funciones empiezan por `lite_`: al escribirlo en
#' RStudio, el autocompletado las muestra todas.
#'
#' Funciones de datos (devuelven un data frame): [lite_importa()],
#' [lite_na()], [lite_outliers()], [lite_factor()], [lite_recodifica()].
#'
#' Funciones de análisis (devuelven un objeto [matlite]): [lite_describe()],
#' [lite_correlaciones()], [lite_acp()], [lite_acp_ranking()],
#' [lite_cluster_ward()], [lite_logit_descriptivo()], [lite_particion()],
#' [lite_logit()], [lite_logit_seleccion()], [lite_logit_escenarios()],
#' [lite_contingencia()], [lite_emd_dimensiones()], [lite_emd()].
#'
#' Utilidades: [lite_comprueba()], [lite_datos()], [lite_resumen()],
#' [lite_tabla()], [lite_patchwork()].
#'
#' @keywords internal
#' @importFrom rlang .data
#' @importFrom patchwork plot_annotation
#' @importFrom ggplot2 ggplot
"_PACKAGE"
