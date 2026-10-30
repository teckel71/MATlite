# =============================================================================
# Reexportaciones y envoltorios de MATrstars con prefijo lite_
# =============================================================================

#' Operador pipe
#'
#' Reexportado de \pkg{magrittr}: `x %>% f(y)` equivale a `f(x, y)`.
#' @name %>%
#' @rdname pipe
#' @importFrom magrittr %>%
#' @export
NULL

#' @importFrom dplyr select
#' @export
dplyr::select

#' @importFrom dplyr filter
#' @export
dplyr::filter

#' @importFrom dplyr arrange
#' @export
dplyr::arrange

#' @importFrom dplyr mutate
#' @export
dplyr::mutate

#' @importFrom dplyr rename
#' @export
dplyr::rename

#' @importFrom dplyr slice
#' @export
dplyr::slice

#' @importFrom dplyr desc
#' @export
dplyr::desc

#' @importFrom dplyr group_by
#' @export
dplyr::group_by

#' @importFrom dplyr summarise
#' @export
dplyr::summarise

#' Explora (y opcionalmente elimina) valores ausentes
#'
#' Versión con prefijo `lite_` de [MATrstars::explora_na()]: mismo código y
#' mismos resultados. Muestra el mapa de valores ausentes y la tabla de casos
#' afectados y, con `accion = "eliminar"`, devuelve el data frame sin ellos.
#'
#' @param datos Data frame.
#' @param variables Variables a analizar, sin comillas (`RENECO`,
#'   `c(RENECO, ACTIVO)`...). Si se omite, se analizan todas.
#' @param accion `"documentar"` (por defecto) solo muestra el diagnóstico;
#'   `"eliminar"` además quita los casos con algún valor ausente.
#' @param titulo,subtitulo Títulos del gráfico.
#' @param ... Otros argumentos de [MATrstars::explora_na()].
#' @return El data frame (sin cambios, o sin los casos con `NA`).
#' @examples
#' \dontrun{
#' muestra <- lite_na(muestra, RENECO, accion = "eliminar")
#' }
#' @export
lite_na <- function(datos, variables = NULL, accion = "documentar",
                    titulo = "Valores ausentes", subtitulo = NULL, ...) {
  MATrstars::explora_na(datos, {{ variables }}, accion = accion,
                        titulo = titulo, subtitulo = subtitulo, ...)
}

#' Explora (y opcionalmente elimina) outliers
#'
#' Versión con prefijo `lite_` de [MATrstars::explora_outliers()]: mismo
#' código y mismos resultados. Con una variable aplica la regla de 1,5·IQR;
#' con varias, la aplica sobre la distancia de Mahalanobis.
#'
#' @inheritParams lite_na
#' @param escala `"auto"`, `"lineal"` o `"pseudo_log"` (eje del box-plot).
#' @param ... Otros argumentos de [MATrstars::explora_outliers()].
#' @return El data frame (sin cambios, o sin los outliers).
#' @examples
#' \dontrun{
#' muestra_so <- lite_outliers(muestra, RENECO, accion = "eliminar")
#' seleccion_so <- lite_outliers(seleccion, accion = "eliminar") # Mahalanobis
#' }
#' @export
lite_outliers <- function(datos, variables = NULL, accion = "documentar",
                          titulo = NULL, subtitulo = NULL, escala = "auto", ...) {
  MATrstars::explora_outliers(datos, {{ variables }}, accion = accion,
                              titulo = titulo, subtitulo = subtitulo,
                              escala = escala, ...)
}

#' Tabla con el estilo del libro
#'
#' Versión con prefijo `lite_` de [MATrstars::kable_rstars()].
#'
#' @param x Data frame o matriz.
#' @param ... Argumentos de [MATrstars::kable_rstars()] (`caption`,
#'   `col.names`, `digits`...).
#' @return Tabla `knitr_kable`.
#' @export
lite_tabla <- function(x, ...) MATrstars::kable_rstars(x, ...)

#' Composiciones 2x2 de gráficos
#'
#' Versión con prefijo `lite_` de [MATrstars::create_patchwork()].
#'
#' @param graficos Lista de gráficos `ggplot`.
#' @return Lista de composiciones (se muestran todas al imprimirla).
#' @export
lite_patchwork <- function(graficos) {
  nueva_lista(MATrstars::create_patchwork(graficos))
}
