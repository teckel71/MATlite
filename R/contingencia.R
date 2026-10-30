# =============================================================================
# Tablas de contingencia y asociacion (capitulo Lite 7 <- Guia 11.1-11.2)
# =============================================================================

# Grafico de base "grid" (vcd) guardado como funcion: se dibuja al imprimirlo.
grafico_grid <- function(f) structure(list(dibujar = f), class = "lite_grafico")

#' @export
print.lite_grafico <- function(x, ...) {
  grid::grid.newpage()
  x$dibujar()
  invisible(x)
}

#' Recodifica las categorías de una variable
#'
#' Cambia el nombre de algunas categorías de una variable categórica (por
#' ejemplo, para abreviar nombres largos). Las categorías no indicadas se
#' mantienen.
#'
#' @param datos Data frame.
#' @param variable Variable, sin comillas.
#' @param cambios Vector con nombre: `c("nombre actual" = "nombre nuevo", ...)`.
#' @return El data frame con la variable recodificada.
#' @examples
#' \dontrun{
#' seleccion <- lite_recodifica(seleccion, GALAXIA,
#'   c("Gran Nube de Magallanes" = "GN Magallanes",
#'     "Galaxia de Andrómeda"    = "Andrómeda"))
#' }
#' @export
lite_recodifica <- function(datos, variable, cambios) {
  var <- rlang::as_name(rlang::ensym(variable))
  if (!var %in% names(datos)) stop("La variable ", var, " no existe en el data frame.", call. = FALSE)
  if (is.null(names(cambios))) stop("`cambios` debe ser un vector con nombre: c(\"actual\" = \"nuevo\").", call. = FALSE)
  x <- datos[[var]]
  era_factor <- is.factor(x)
  x <- as.character(x)
  no_estan <- setdiff(names(cambios), unique(x))
  if (length(no_estan) > 0) {
    message("Aviso: estas categor\u00edas no aparecen en ", var, ": ",
            paste(no_estan, collapse = ", "), ".")
  }
  i <- match(x, names(cambios))
  x[!is.na(i)] <- cambios[i[!is.na(i)]]
  datos[[var]] <- if (era_factor) factor(x) else x
  message(var, ": ", sum(!is.na(i)), " casos recodificados. Categor\u00edas: ",
          paste(sort(unique(x)), collapse = ", "), ".")
  datos
}

#' Tabla de contingencia y asociación entre dos atributos
#'
#' Reproduce el análisis de una tabla de contingencia bidimensional de la
#' Guía: tabla con frecuencias marginales, gráfico de mosaico, gráficos de
#' barras de cada atributo según el otro, contraste de independencia
#' ji-cuadrado de Pearson, residuos estandarizados ajustados y gráfico de
#' asociación que resalta las celdas significativas. Añade las frecuencias
#' esperadas (con aviso si alguna es menor que 5) y las medidas de
#' asociación: Phi (solo en tablas 2x2), coeficiente de contingencia C (con
#' su máximo posible) y V de Cramér.
#'
#' @param datos Data frame.
#' @param fila,columna Atributos (variables categóricas), sin comillas.
#' @param alfa Nivel de significación. Por defecto, 0,05.
#' @param titulo Título de los gráficos de mosaico y asociación. Por
#'   defecto, `"<fila> y <columna>"`.
#' @param subtitulo Subtítulo de los gráficos.
#' @return Objeto `matlite` con `$tabla`, `$mosaico`, `$barras`, `$chi2`,
#'   `$residuos`, `$asociacion`, `$esperadas` y `$medidas`.
#' @examples
#' \dontrun{
#' lite_contingencia(seleccion, GALAXIA, FJUR, subtitulo = "Empresas TMI")
#' }
#' @export
lite_contingencia <- function(datos, fila, columna, alfa = 0.05,
                              titulo = NULL, subtitulo = NULL) {
  f <- rlang::as_name(rlang::ensym(fila))
  cc <- rlang::as_name(rlang::ensym(columna))
  for (v in c(f, cc)) if (!v %in% names(datos)) stop("La variable ", v, " no existe en el data frame.", call. = FALSE)
  titulo <- titulo %||% paste(f, "y", cc)
  d <- datos[stats::complete.cases(datos[, c(f, cc)]), c(f, cc)]
  tab <- table(d[[f]], d[[cc]], dnn = c(f, cc))
  if (any(dim(tab) < 2)) stop("Cada atributo debe tener al menos dos categor\u00edas.", call. = FALSE)
  z <- stats::qnorm(1 - alfa / 2)

  # --- Tabla con marginales -------------------------------------------------
  cab <- c(1, ncol(tab), 1); names(cab) <- c(f, cc, " ")
  t_tab <- MATrstars::kable_rstars(as.data.frame.matrix(stats::addmargins(tab)),
                                   caption = paste("Tabla de contingencia:", f, "x", cc)) %>%
    kableExtra::add_header_above(cab, bold = TRUE, line = TRUE) %>%
    kableExtra::column_spec(1, bold = TRUE)

  # --- Graficos -------------------------------------------------------------
  mosaico <- grafico_grid(function() {
    vcd::mosaic(tab, main = titulo, sub = subtitulo, gp = vcd::shading_Marimekko(tab),
                labeling_args = list(rot_labels = c(0, 90), gp_labels = grid::gpar(fontsize = 10)),
                main_gp = grid::gpar(fontsize = 16), sub_gp = grid::gpar(fontsize = 14))
  })

  g1 <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[f]], fill = .data[[cc]])) +
    ggplot2::geom_bar() + ggplot2::ggtitle(f, subtitle = subtitulo) +
    ggplot2::ylab("Frecuencias") + ggplot2::xlab(f)
  g2 <- ggplot2::ggplot(d, ggplot2::aes(x = .data[[cc]], fill = .data[[f]])) +
    ggplot2::geom_bar() + ggplot2::ggtitle(cc, subtitle = subtitulo) +
    ggplot2::ylab("Frecuencias") + ggplot2::xlab(cc)
  if (nrow(tab) > 3) g1 <- g1 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))
  if (ncol(tab) > 3) g2 <- g2 + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))
  barras <- (g1 + g2) + patchwork::plot_annotation(
    title = "Frecuencias Marginales.",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 14)))

  # --- Contraste ji-cuadrado ------------------------------------------------
  prueba <- suppressWarnings(stats::chisq.test(tab))
  chi <- data.frame(Contraste = "Ji-cuadrado de Pearson",
                    Chi2 = unname(prueba$statistic), gl = unname(prueba$parameter),
                    p = prueba$p.value,
                    Decision = decision(prueba$p.value, alfa, "independencia"))
  t_chi <- MATrstars::kable_rstars(chi, caption = paste("Contraste de independencia:", f, "x", cc),
                                   col.names = c("Contraste", "\u03c7\u00b2", "g.l.", "p-valor", "Decisi\u00f3n"),
                                   digits = c(NA, 3, 0, 4, NA))

  # --- Residuos estandarizados ajustados ------------------------------------
  res_std <- prueba$stdres
  sig <- abs(res_std) > z
  celdas <- matrix(kableExtra::cell_spec(formatC(res_std, format = "f", digits = 2),
                                         background = ifelse(sig, "orange", "white"),
                                         bold = sig),
                   nrow = nrow(res_std), dimnames = dimnames(res_std))
  t_res <- MATrstars::kable_rstars(as.data.frame.matrix(celdas), escape = FALSE,
                                   caption = sprintf("Residuos estandarizados ajustados (naranja: |r| > %.2f)", z)) %>%
    kableExtra::column_spec(1, bold = TRUE)

  colores <- ifelse(sig, "orange", "lightgray")
  asociacion <- grafico_grid(function() {
    vcd::assoc(tab, main = titulo,
               sub = sprintf("Residuos de Pearson Tipificados. Naranja: significativos con sig. = %s",
                             format(alfa, decimal.mark = ",")),
               compress = FALSE, gp = grid::gpar(fill = colores),
               labeling_args = list(rot_labels = c(0, 90), gp_labels = grid::gpar(fontsize = 10)),
               main_gp = grid::gpar(fontsize = 16), sub_gp = grid::gpar(fontsize = 14))
  })

  # --- Frecuencias esperadas ------------------------------------------------
  esp <- prueba$expected
  n_bajas <- sum(esp < 5)
  t_esp <- MATrstars::kable_rstars(as.data.frame.matrix(esp), digits = 2,
                                   caption = "Frecuencias esperadas bajo independencia") %>%
    kableExtra::column_spec(1, bold = TRUE)
  if (n_bajas > 0) {
    message("Aviso: ", n_bajas, " celda(s) (", round(100 * n_bajas / length(esp)),
            " %) tienen frecuencia esperada menor que 5; el contraste ji-cuadrado ",
            "puede no ser fiable. Valora agrupar categor\u00edas.")
  }

  # --- Medidas de asociacion ------------------------------------------------
  n <- sum(tab); chi2 <- unname(prueba$statistic)
  k <- min(dim(tab))
  C <- sqrt(chi2 / (chi2 + n)); Cmax <- sqrt((k - 1) / k)
  V <- sqrt(chi2 / (n * (k - 1)))
  med <- data.frame(Medida = c("Coeficiente de contingencia C", "C m\u00e1ximo posible en esta tabla",
                               "C corregido (C / C m\u00e1ximo)", "V de Cram\u00e9r"),
                    Valor = c(C, Cmax, C / Cmax, V))
  if (all(dim(tab) == 2)) {
    phi <- (tab[1, 1] * tab[2, 2] - tab[1, 2] * tab[2, 1]) /
      sqrt(prod(rowSums(tab)) * prod(colSums(tab)))
    med <- rbind(data.frame(Medida = "Phi", Valor = phi), med)
  }
  t_med <- MATrstars::kable_rstars(med, caption = paste("Medidas de asociaci\u00f3n:", f, "x", cc),
                                   digits = c(NA, 3), row.names = FALSE)

  message("Contraste ji-cuadrado: \u03c7\u00b2 = ", round(chi2, 3), ", g.l. = ",
          prueba$parameter, ", p-valor = ", signif(prueba$p.value, 4), ". ",
          decision(prueba$p.value, alfa, "independencia"), ".")

  nuevo_matlite(
    list(tabla = t_tab, mosaico = mosaico, barras = barras, chi2 = t_chi,
         residuos = t_res, asociacion = asociacion, esperadas = t_esp, medidas = t_med,
         valores = list(tabla = tab, chi2 = chi2, gl = unname(prueba$parameter),
                        p = prueba$p.value, residuos = res_std, esperadas = esp,
                        C = C, Cmax = Cmax, V = V)),
    mostrar = c("tabla", "mosaico", "barras", "chi2", "residuos", "asociacion",
                "esperadas", "medidas"),
    titulo = paste("Contingencia", f, "x", cc))
}
