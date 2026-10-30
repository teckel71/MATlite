# =============================================================================
# Analisis previo de datos (capitulo Lite 3 <- Guia 5)
# =============================================================================

# Selecciona columnas con tidy-select; si no se indica nada, las numericas.
seleccionar <- function(datos, vars_quo, solo_numericas = TRUE) {
  if (rlang::quo_is_null(vars_quo) || rlang::quo_is_missing(vars_quo)) {
    sub <- if (solo_numericas) datos[, vapply(datos, is.numeric, logical(1)), drop = FALSE] else datos
  } else {
    sub <- dplyr::select(datos, !!vars_quo)
  }
  if (solo_numericas) {
    no_num <- names(sub)[!vapply(sub, is.numeric, logical(1))]
    if (length(no_num) > 0) {
      stop("Estas variables no son num\u00e9ricas: ", paste(no_num, collapse = ", "),
           ". Selecciona solo variables m\u00e9tricas.", call. = FALSE)
    }
  }
  sub
}

quitar_na <- function(sub) {
  completos <- stats::complete.cases(sub)
  if (!all(completos)) {
    message("Aviso: hay ", sum(!completos), " caso(s) con valores ausentes, ",
            "que no se tienen en cuenta. Para tratarlos, usa antes lite_na().")
  }
  sub[completos, , drop = FALSE]
}

decision <- function(p, alfa = 0.05, h0) {
  if (p < alfa) sprintf("Se rechaza H0 (%s) al %s %%", h0, format(100 * alfa))
  else sprintf("No se rechaza H0 (%s) al %s %%", h0, format(100 * alfa))
}

#' Describe una variable métrica
#'
#' Reproduce el análisis de una variable del capítulo de análisis previo:
#' distribución de frecuencias agrupadas en intervalos, histograma, gráfico
#' de densidad frente a la curva normal, box-plot, QQ-plot, principales
#' estadísticos y contraste de normalidad de Shapiro-Wilk, más un resumen
#' con los cuatro gráficos.
#'
#' @param datos Data frame.
#' @param variable Variable a describir, sin comillas.
#' @param intervalos Número de intervalos de la tabla de frecuencias y del
#'   histograma: `"sturges"` (por defecto, como en la Guía), `"fd"`
#'   (Freedman-Diaconis) o un número.
#' @param etiqueta Texto para los ejes (por ejemplo,
#'   `"Rentabilidad Económica (%)"`). Por defecto, el nombre de la variable.
#' @param titulo,subtitulo Título y subtítulo del resumen gráfico.
#' @return Objeto `matlite` con `$frecuencias`, `$estadisticos`,
#'   `$normalidad`, `$histograma`, `$densidad`, `$boxplot`, `$qq` y
#'   `$resumen`.
#' @examples
#' \dontrun{
#' rentabilidad <- lite_describe(muestra_so, RENECO,
#'                               etiqueta = "Rentabilidad Económica (%)")
#' rentabilidad
#' rentabilidad$normalidad
#' }
#' @export
lite_describe <- function(datos, variable, intervalos = "sturges",
                          etiqueta = NULL, titulo = NULL, subtitulo = NULL) {
  var <- rlang::as_name(rlang::ensym(variable))
  if (!var %in% names(datos)) stop("La variable ", var, " no existe en el data frame.", call. = FALSE)
  if (!is.numeric(datos[[var]])) stop("La variable ", var, " no es num\u00e9rica.", call. = FALSE)
  etiqueta <- etiqueta %||% var
  titulo   <- titulo %||% etiqueta

  df <- quitar_na(datos[, var, drop = FALSE])
  # Mismo orden que el script de la Guia (por la variable y por nombre).
  df <- df[order(df[[var]], row.names(df)), , drop = FALSE]
  x  <- df[[var]]

  k <- if (is.numeric(intervalos)) intervalos
       else switch(intervalos,
                   sturges = grDevices::nclass.Sturges(x),
                   fd      = grDevices::nclass.FD(x),
                   stop("`intervalos` debe ser \"sturges\", \"fd\" o un n\u00famero.", call. = FALSE))

  # --- Tabla de frecuencias -------------------------------------------------
  conteo <- as.data.frame(table(cut(x, breaks = k, include.lowest = TRUE)))
  colnames(conteo) <- c("Intervalo", "Frecuencia")
  N <- sum(conteo$Frecuencia)
  conteo$Frecuencia_acum   <- cumsum(conteo$Frecuencia)
  conteo$Frecuencia_R      <- conteo$Frecuencia / N
  conteo$Frecuencia_R_acum <- cumsum(conteo$Frecuencia_R)
  t_frec <- MATrstars::kable_rstars(
    conteo, caption = paste("Dist. frecuencias agrupadas:", etiqueta),
    col.names = c("Intervalo", "n(i)", "N(i)", "f(i)", "F(i)"),
    digits = c(NA, 0, 0, 2, 2))

  # --- Graficos -------------------------------------------------------------
  media <- mean(x)
  g1 <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[var]])) +
    ggplot2::geom_histogram(bins = k, colour = "red", fill = "orange", alpha = 0.7) +
    ggplot2::geom_vline(xintercept = media, color = "dark blue", linewidth = 1.2, alpha = 0.8) +
    ggplot2::ggtitle("Histograma") + ggplot2::xlab(etiqueta) + ggplot2::ylab("Frecuencias")

  g2 <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[var]])) +
    ggplot2::geom_density(colour = "red", fill = "orange", alpha = 0.7) +
    ggplot2::geom_vline(xintercept = media, color = "dark blue", linewidth = 0.8, alpha = 0.8) +
    ggplot2::stat_function(fun = stats::dnorm, args = list(mean = media, sd = stats::sd(x)),
                           geom = "area", color = "darkblue", fill = "yellow", alpha = 0.2) +
    ggplot2::ggtitle("Gr\u00e1fico de densidad vs curva normal") +
    ggplot2::xlab(etiqueta) + ggplot2::ylab("Densidad")

  g3 <- ggplot2::ggplot(df, ggplot2::aes(x = "", y = .data[[var]])) +
    ggplot2::geom_boxplot(color = "red", fill = "orange", outlier.shape = NA) +
    ggplot2::stat_summary(fun = "mean", geom = "point", size = 3, col = "darkblue") +
    ggplot2::geom_jitter(width = 0.1, size = 1, col = "darkred", alpha = 0.50) +
    ggplot2::ggtitle("Box-Plot") + ggplot2::xlab("") + ggplot2::ylab(etiqueta)

  g4 <- ggplot2::ggplot(df, ggplot2::aes(sample = .data[[var]])) +
    ggplot2::stat_qq(colour = "red") +
    ggplot2::stat_qq_line(colour = "dark blue") +
    ggplot2::ggtitle("QQ-Plot")

  resumen <- (g1 | g2) / (g3 | g4)
  resumen <- resumen + patchwork::plot_annotation(
    title = titulo, subtitle = subtitulo,
    theme = ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12)))

  # --- Estadisticos ---------------------------------------------------------
  est <- data.frame(Media = mean(x), DT = stats::sd(x), Minimo = min(x),
                    Mediana = stats::median(x), Maximo = max(x),
                    Asimetria = moments::skewness(x),
                    Curtosis = moments::kurtosis(x) - 3)
  t_est <- MATrstars::kable_rstars(
    est, caption = paste("Principales Estad\u00edsticos:", etiqueta),
    col.names = c("Media", "Desviaci\u00f3n T\u00edpica", "Valor m\u00ednimo", "Mediana",
                  "Valor M\u00e1ximo", "C. Asimetr\u00eda Fisher", "C. Curtosis Fisher"),
    digits = rep(2, 7))

  # --- Normalidad -----------------------------------------------------------
  sw <- stats::shapiro.test(x)
  norm <- data.frame(Contraste = "Shapiro-Wilk", W = unname(sw$statistic),
                     p = sw$p.value,
                     Decision = decision(sw$p.value, h0 = "normalidad"))
  t_norm <- MATrstars::kable_rstars(
    norm, caption = paste("Contraste de normalidad:", etiqueta),
    col.names = c("Contraste", "Estad\u00edstico W", "p-valor", "Decisi\u00f3n"),
    digits = c(NA, 4, 4, NA))

  nuevo_matlite(
    list(frecuencias = t_frec, estadisticos = t_est, normalidad = t_norm,
         histograma = g1, densidad = g2, boxplot = g3, qq = g4,
         resumen = resumen,
         valores = list(n = length(x), intervalos = k, est = est,
                        W = unname(sw$statistic), p = sw$p.value)),
    mostrar = c("frecuencias", "estadisticos", "normalidad", "resumen"),
    titulo = paste("Descripci\u00f3n de", var))
}

#' Matriz de correlaciones
#'
#' Matriz gráfica de correlaciones de Pearson con estrellas de significación
#' (`GGally::ggpairs()`), con el estilo de la Guía. Si se indica
#' `comparar_con` (por ejemplo, el data frame que aún conserva los
#' outliers), presenta las dos matrices lado a lado.
#'
#' @param datos Data frame.
#' @param variables Variables, sin comillas. Si se omiten, todas las
#'   numéricas.
#' @param comparar_con Data frame opcional para la comparación.
#' @param titulo Título de la matriz principal. Por defecto,
#'   `"Matriz de Correlación"` o, si hay comparación,
#'   `"Matriz de Correlación (sin outliers)"`.
#' @param titulo_comparacion Título de la segunda matriz. Por defecto,
#'   `"Matriz de Correlación (con outliers)"`.
#' @return Objeto `matlite` con `$matriz` (tabla de coeficientes),
#'   `$grafico` y, si hay comparación, `$grafico_comparacion` y
#'   `$comparacion` (ambas matrices juntas).
#' @examples
#' \dontrun{
#' lite_correlaciones(muestra2_so, c(RENECO, ACTIVO, MARGEN, RES),
#'                    comparar_con = muestra2)
#' }
#' @export
lite_correlaciones <- function(datos, variables = NULL, comparar_con = NULL,
                               titulo = NULL,
                               titulo_comparacion = NULL) {
  titulo_comparacion <- titulo_comparacion %||% "Matriz de Correlaci\u00f3n (con outliers)"
  sub <- quitar_na(seleccionar(datos, rlang::enquo(variables)))
  if (ncol(sub) < 2) stop("Se necesitan al menos dos variables num\u00e9ricas.", call. = FALSE)
  titulo <- titulo %||% if (is.null(comparar_con)) "Matriz de Correlaci\u00f3n"
                        else "Matriz de Correlaci\u00f3n (sin outliers)"

  pares <- function(d, tit) {
    GGally::ggpairs(d, lower = list(continuous = GGally::wrap(
      "cor", size = 4.5, method = "pearson", stars = TRUE)), title = tit)
  }
  g <- pares(sub, titulo)
  r <- stats::cor(sub)
  t_r <- MATrstars::kable_rstars(as.data.frame(r), caption = paste(titulo, "(Pearson)"),
                                 digits = 3)

  res <- list(matriz = t_r, grafico = g)
  mostrar <- c("matriz", "grafico")
  if (!is.null(comparar_con)) {
    sub2 <- quitar_na(comparar_con[, names(sub), drop = FALSE])
    g2 <- pares(sub2, titulo_comparacion)
    juntas <- patchwork::wrap_elements(GGally::ggmatrix_gtable(g)) |
              patchwork::wrap_elements(GGally::ggmatrix_gtable(g2))
    juntas <- juntas + patchwork::plot_annotation(title = "CORRELACIONES")
    res$grafico_comparacion <- g2
    res$comparacion <- juntas
    mostrar <- c("matriz", "comparacion")
  }
  res$valores <- list(r = r)
  nuevo_matlite(res, mostrar = mostrar, titulo = "Correlaciones")
}
