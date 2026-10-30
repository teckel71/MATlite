# =============================================================================
# Funciones generales (capitulos Lite 1 y 2)
# =============================================================================

# Ficheros de datos del curso (carpeta download/ del libro R-Stars).
.lite_ficheros <- c(
  interestelar_100        = "interestelar_100.xlsx",
  interestelar_25         = "interestelar_25.xlsx",
  interestelar_300        = "interestelar_300.xlsx",
  licencias_100           = "licencias_100.xlsx",
  escenarios_logit_rstars = "escenarios_logit_rstars.xlsx",
  percepcion_interestelar = "percepcion_interestelar.xlsx"
)

.lite_url_datos <- "https://raw.githubusercontent.com/teckel71/RStars-book/main/download/"

# Paquetes necesarios para seguir el curso completo.
.lite_paquetes <- c("MATrstars", "readxl", "dplyr", "ggplot2", "patchwork",
                    "gtExtras", "knitr", "kableExtra", "visdat", "GGally",
                    "moments", "psych", "factoextra", "cluster", "vcd",
                    "smacof", "ggrepel", "tidyr")

#' Comprueba que todo está listo para trabajar
#'
#' Revisa la versión de R, que estén instalados los paquetes que usa el
#' curso y qué ficheros de datos hay en la carpeta de trabajo. Para cada
#' problema indica la instrucción que lo resuelve.
#'
#' @return Data frame con el resultado de cada comprobación (invisible).
#' @examples
#' \dontrun{
#' lite_comprueba()
#' }
#' @export
lite_comprueba <- function() {
  filas <- list()
  agrega <- function(elemento, ok, detalle) {
    filas[[length(filas) + 1]] <<- data.frame(
      Elemento = elemento, Estado = if (ok) "OK" else "FALTA",
      Detalle = detalle, stringsAsFactors = FALSE)
  }

  ver_r <- getRversion()
  agrega("R", ver_r >= "4.1.0",
         if (ver_r >= "4.1.0") paste("Versi\u00f3n", ver_r)
         else paste("Versi\u00f3n", ver_r, "- instala R 4.1 o superior desde https://cran.r-project.org"))

  for (p in .lite_paquetes) {
    ok <- requireNamespace(p, quietly = TRUE)
    det <- if (ok) paste("Versi\u00f3n", utils::packageVersion(p))
           else if (p == "MATrstars") 'remotes::install_github("teckel71/MATrstars")'
           else sprintf('install.packages("%s")', p)
    agrega(paste("Paquete", p), ok, det)
  }

  carpeta <- getwd()
  presentes <- .lite_ficheros[file.exists(file.path(carpeta, .lite_ficheros))]
  agrega("Carpeta de trabajo", TRUE, carpeta)
  agrega("Ficheros de datos", length(presentes) > 0,
         if (length(presentes) > 0) paste(presentes, collapse = ", ")
         else "Ninguno. Desc\u00e1rgalos con lite_datos()")

  res <- do.call(rbind, filas)
  print(res, row.names = FALSE, right = FALSE)
  n_mal <- sum(res$Estado == "FALTA")
  if (n_mal == 0) message("\nTodo listo. \u00a1A trabajar!")
  else message("\nHay ", n_mal, " elemento(s) por resolver: ejecuta la instrucci\u00f3n de la columna Detalle.")
  invisible(res)
}

#' Descarga los ficheros de datos del curso
#'
#' Descarga a la carpeta de trabajo los ficheros Excel que se usan en las
#' prácticas del libro R-Stars Lite.
#'
#' @param nombre Nombre de uno o varios ficheros, con o sin `.xlsx`
#'   (por ejemplo `"interestelar_100"`). Si se omite, se descargan todos.
#' @param destino Carpeta de destino. Por defecto, la carpeta de trabajo.
#' @return Rutas de los ficheros descargados (invisible).
#' @examples
#' \dontrun{
#' lite_datos()                    # todos
#' lite_datos("interestelar_100")  # uno
#' }
#' @export
lite_datos <- function(nombre = NULL, destino = ".") {
  if (is.null(nombre)) {
    ficheros <- unname(.lite_ficheros)
  } else {
    nombre <- sub("\\.xlsx$", "", nombre)
    desconocidos <- setdiff(nombre, names(.lite_ficheros))
    if (length(desconocidos) > 0) {
      stop("No conozco el fichero: ", paste(desconocidos, collapse = ", "),
           ".\nLos disponibles son: ", paste(names(.lite_ficheros), collapse = ", "),
           call. = FALSE)
    }
    ficheros <- unname(.lite_ficheros[nombre])
  }
  rutas <- file.path(destino, ficheros)
  for (i in seq_along(ficheros)) {
    utils::download.file(paste0(.lite_url_datos, ficheros[i]), rutas[i],
                         mode = "wb", quiet = TRUE)
    message("Descargado: ", ficheros[i])
  }
  message("Ficheros guardados en: ", normalizePath(destino))
  invisible(rutas)
}

#' Importa una hoja de Excel
#'
#' Lee una hoja de un fichero Excel y usa la primera columna (el nombre de
#' cada caso) como nombres de fila. Equivale a `readxl::read_excel()`
#' seguido de `data.frame(..., row.names = 1)`, como en los scripts de
#' R-Stars: La Guía.
#'
#' @param archivo Nombre (o ruta) del fichero Excel.
#' @param hoja Nombre o número de la hoja. Por defecto, `"Datos"`.
#' @param na Texto(s) que deben leerse como valor ausente. Por defecto,
#'   `"n.d."`.
#' @param nombres_fila Si `TRUE` (por defecto), la primera columna se usa
#'   como nombres de fila.
#' @return Data frame.
#' @examples
#' \dontrun{
#' interestelar_100 <- lite_importa("interestelar_100.xlsx")
#' }
#' @export
lite_importa <- function(archivo, hoja = "Datos", na = "n.d.",
                         nombres_fila = TRUE) {
  if (!file.exists(archivo)) {
    stop("No encuentro el fichero \"", archivo, "\" en la carpeta de trabajo:\n  ",
         getwd(), "\nComprueba el nombre, cambia la carpeta de trabajo ",
         "(Session > Set Working Directory) o desc\u00e1rgalo con lite_datos().",
         call. = FALSE)
  }
  datos <- readxl::read_excel(archivo, sheet = hoja, na = na)
  datos <- if (nombres_fila) data.frame(datos, row.names = 1) else data.frame(datos)
  message("Importados ", nrow(datos), " casos y ", ncol(datos),
          " variables de \"", basename(archivo), "\" (hoja \"", hoja, "\").")
  datos
}

#' Resumen visual de un data frame
#'
#' Tabla con una fila por variable (tipo, gráfico de distribución, valores
#' ausentes, media, mediana y desviación típica). Envoltorio de
#' `gtExtras::gt_plt_summary()`.
#'
#' @param datos Data frame.
#' @param titulo Título de la tabla. Por defecto, el nombre del data frame.
#' @return Tabla `gt`.
#' @examples
#' \dontrun{
#' lite_resumen(interestelar_100)
#' }
#' @export
lite_resumen <- function(datos, titulo = NULL) {
  if (is.null(titulo)) titulo <- deparse(substitute(datos))
  suppressWarnings(gtExtras::gt_plt_summary(datos, title = titulo))
}
