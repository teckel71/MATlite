# =============================================================================
# Clase "matlite": contenedor de resultados de las funciones de analisis.
# =============================================================================

#' Resultados de las funciones de MATlite
#'
#' Las funciones de análisis de MATlite (`lite_describe()`, `lite_acp()`,
#' `lite_cluster_ward()`, etc.) devuelven un objeto de clase `matlite`: una
#' lista con nombre cuyos elementos son tablas, gráficos o datos.
#'
#' - Escribir el nombre del objeto (o llamar a `print()`) muestra, en el
#'   orden de la práctica, sus resultados principales y termina con un
#'   índice de todo lo que contiene.
#' - Cada pieza puede verse por separado con `$`, por ejemplo
#'   `componentes$cargas`.
#'
#' En RStudio, las tablas de un objeto completo se reúnen en una única
#' página del panel *Viewer*, y los gráficos van al panel *Plots* (con las
#' flechas del panel se puede navegar entre ellos).
#'
#' @param x Objeto de clase `matlite`.
#' @param ... No se utiliza.
#' @return `x`, de forma invisible.
#' @name matlite
#' @export
print.matlite <- function(x, ...) {
  mostrar <- attr(x, "mostrar")
  if (is.null(mostrar)) mostrar <- names(x)
  mostrar <- intersect(mostrar, names(x))

  if (lite_en_knit()) {
    for (nm in mostrar) lite_mostrar(x[[nm]])
    return(invisible(x))
  }

  # Sesion interactiva: graficos al panel Plots, tablas juntas al Viewer.
  tablas <- list()
  for (nm in mostrar) {
    el <- x[[nm]]
    if (es_tabla(el)) {
      tablas[[length(tablas) + 1]] <- el
    } else if (inherits(el, "matlite_lista") && all(vapply(el, es_tabla, logical(1)))) {
      tablas <- c(tablas, unclass(el))
    } else {
      lite_mostrar(el)
    }
  }
  if (length(tablas) > 0) mostrar_tablas_juntas(tablas, attr(x, "titulo"))

  lite_indice(x)
  invisible(x)
}

#' @export
print.matlite_lista <- function(x, ...) {
  for (el in x) lite_mostrar(el)
  invisible(x)
}

#' @export
`[.matlite_lista` <- function(x, i) {
  structure(unclass(x)[i], class = c("matlite_lista", "list"))
}

# --- Constructores internos -------------------------------------------------

nuevo_matlite <- function(componentes, mostrar = names(componentes),
                          titulo = NULL, clase = NULL) {
  structure(componentes,
            class   = c(clase, "matlite", "list"),
            mostrar = mostrar,
            titulo  = titulo)
}

nueva_lista <- function(x) {
  structure(x, class = c("matlite_lista", "list"))
}

# --- Utilidades internas ----------------------------------------------------

lite_en_knit <- function() isTRUE(getOption("knitr.in.progress"))

es_tabla <- function(x) inherits(x, c("knitr_kable", "kableExtra"))

# Muestra un elemento cualquiera respetando el contexto (knit o interactivo).
lite_mostrar <- function(el) {
  if (is.null(el)) return(invisible())
  if (es_tabla(el)) {
    if (lite_en_knit()) cat("\n\n", as.character(el), "\n\n", sep = "")
    else print(el)
  } else if (inherits(el, "matlite_lista")) {
    for (e in el) lite_mostrar(e)
  } else if (inherits(el, c("gg", "ggplot", "ggmatrix", "patchwork", "gtable",
                            "grob", "gt_tbl", "lite_grafico"))) {
    sin_avisos(print, el)
  } else if (is.data.frame(el) || is.atomic(el)) {
    # Los datos no se imprimen al mostrar el objeto completo.
    invisible()
  } else {
    print(el)
  }
  invisible()
}

mostrar_tablas_juntas <- function(tablas, titulo = NULL) {
  piezas <- lapply(tablas, function(t) htmltools::HTML(as.character(t)))
  cabecera <- if (!is.null(titulo)) htmltools::tags$h3(titulo) else NULL
  pagina <- htmltools::tagList(
    rmarkdown::html_dependency_bootstrap("default"),
    htmltools::tags$div(style = "padding: 10px 20px;", cabecera, piezas)
  )
  print(htmltools::browsable(pagina))
}

lite_indice <- function(x) {
  nm <- names(x)
  cat("\n", if (!is.null(attr(x, "titulo"))) paste0(attr(x, "titulo"), ". "),
      "Contenido (cada pieza se ve con $):\n  ",
      paste0("$", nm, collapse = "  "), "\n", sep = "")
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# Ejecuta f(...) silenciando los avisos de funciones obsoletas de otros
# paquetes (factoextra, ggpubr...), que no dependen del estudiante.
sin_avisos <- function(f, ...) {
  withCallingHandlers(f(...), warning = function(w) {
    m <- conditionMessage(w)
    if (grepl("deprecated|Length of color vector|obsolet", m, ignore.case = TRUE))
      invokeRestart("muffleWarning")
  })
}
