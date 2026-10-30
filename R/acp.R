# =============================================================================
# Componentes principales (capitulo Lite 4 <- Guia 6, sin el metodo de Hubert)
# =============================================================================

#' Análisis de componentes principales
#'
#' Reproduce el análisis de componentes principales de la Guía: adecuación
#' muestral (KMO global y MSA de cada variable, con la escala de Kaiser),
#' obtención de las componentes con variables tipificadas
#' (`prcomp(scale = TRUE)`), varianza explicada, cargas, autovalores y
#' gráficos para decidir cuántas componentes retener (criterio del
#' autovalor mayor que 1).
#'
#' @param datos Data frame (habitualmente, sin valores ausentes ni outliers).
#' @param variables Variables, sin comillas. Si se omiten, todas las
#'   numéricas.
#' @param subtitulo Subtítulo de los gráficos.
#' @return Objeto `matlite` con `$kmo`, `$varianza`, `$cargas`,
#'   `$autovalores`, `$retencion`, `$grafico_autovalores`,
#'   `$grafico_acumulada`, `$n_retener` (número de componentes con autovalor
#'   >= 1) y `$modelo` (objeto `prcomp`).
#' @examples
#' \dontrun{
#' componentes <- lite_acp(seleccion_so)
#' componentes
#' componentes$cargas
#' }
#' @export
lite_acp <- function(datos, variables = NULL, subtitulo = NULL) {
  sub <- quitar_na(seleccionar(datos, rlang::enquo(variables)))
  if (ncol(sub) < 2) stop("Se necesitan al menos dos variables num\u00e9ricas.", call. = FALSE)

  # --- Adecuacion muestral (KMO / MSA) --------------------------------------
  kmo <- psych::KMO(stats::cor(sub))
  kaiser <- cut(kmo$MSA, breaks = c(0, 0.5, 0.6, 0.7, 0.8, 0.9, Inf),
                labels = c("inaceptable", "bajo", "mediocre", "aceptable",
                           "notable", "excelente"), right = FALSE)
  adecuacion <- data.frame(
    Valor = c(kmo$MSA, kmo$MSAi),
    Interpretacion = c(as.character(kaiser),
                       ifelse(kmo$MSAi >= 0.5, "Adecuada", "Revisar su inclusi\u00f3n")),
    row.names = c("KMO global", paste("MSA:", names(kmo$MSAi))))
  t_kmo <- MATrstars::kable_rstars(
    adecuacion, caption = "Adecuaci\u00f3n muestral del PCA (KMO y MSA)",
    col.names = c("Medida", "Valor", "Interpretaci\u00f3n"), digits = c(3, 0)) %>%
    kableExtra::column_spec(1, bold = TRUE, extra_css = "text-align: left;")

  # --- Componentes ----------------------------------------------------------
  modelo <- stats::prcomp(sub, scale = TRUE)
  importancia <- t(as.data.frame(summary(modelo)$importance))
  t_var <- MATrstars::kable_rstars(
    importancia, caption = "Resumen de Componentes",
    col.names = c("Componente", "Desv. t\u00edpica",
                  "Proporci\u00f3n de varianza (comunalidad)",
                  "Proporci\u00f3n de varianza (comunalidad) acumulada"),
    digits = c(2, 2, 2)) %>%
    kableExtra::column_spec(1, bold = TRUE, extra_css = "text-align: center;")

  t_cargas <- MATrstars::kable_rstars(
    modelo$rotation, caption = "Cargas de las componentes obtenidas",
    digits = rep(3, ncol(modelo$rotation))) %>%
    kableExtra::column_spec(1, bold = TRUE, extra_css = "text-align: left;")

  # --- Retencion (autovalor > 1) --------------------------------------------
  autovalores <- data.frame(orden = seq_len(ncol(sub)), autovalor = modelo$sdev^2)
  autovalores$variacum <- 100 * cumsum(autovalores$autovalor / nrow(autovalores))
  autovalores$checkcp <- ifelse(autovalores$autovalor >= 1, "CP", "NCP")
  n_retener <- sum(autovalores$autovalor >= 1)

  g_auto <- ggplot2::ggplot(autovalores, ggplot2::aes(x = .data$orden, y = .data$autovalor)) +
    ggplot2::geom_bar(stat = "identity", colour = "red", fill = "orange", alpha = 0.7) +
    ggplot2::scale_x_continuous(breaks = autovalores$orden) +
    ggplot2::geom_hline(yintercept = 1, colour = "dark blue") +
    ggplot2::geom_text(ggplot2::aes(label = round(.data$autovalor, 2)),
                       vjust = 1, colour = "dark blue", size = 3) +
    ggplot2::ggtitle("AUTOVALORES DE LAS COMPONENTES", subtitle = subtitulo) +
    ggplot2::xlab("N\u00famero de componente") + ggplot2::ylab("Autovalor")

  g_acum <- ggplot2::ggplot(autovalores, ggplot2::aes(x = .data$orden, y = .data$variacum)) +
    ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = .data$checkcp),
                      colour = "red", alpha = 0.7) +
    ggplot2::scale_x_continuous(breaks = autovalores$orden) +
    ggplot2::geom_text(ggplot2::aes(label = round(.data$variacum, 2)),
                       vjust = 1, colour = "dark blue", size = 3) +
    ggplot2::ggtitle("COMUNALIDAD ACUMULADA POR COMPONENTES", subtitle = subtitulo) +
    ggplot2::xlab("N\u00famero de componente") + ggplot2::ylab("Varianza acumulada") +
    ggplot2::labs(fill = "checkcp")

  retencion <- (g_auto / g_acum) + patchwork::plot_annotation(
    title = "Retenci\u00f3n de componentes (Autovalor >1)", subtitle = subtitulo,
    theme = ggplot2::theme(
      plot.title    = ggplot2::element_text(size = 16, face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 12)))

  t_auto <- MATrstars::kable_rstars(
    autovalores[, c("orden", "autovalor", "variacum", "checkcp")],
    caption = "Autovalores y varianza acumulada",
    col.names = c("Componente", "Autovalor", "Varianza acumulada (%)", "Retener"),
    digits = c(0, 3, 2, NA))

  message("Criterio del autovalor mayor que 1: se retiene(n) ", n_retener,
          " componente(s).")

  nuevo_matlite(
    list(kmo = t_kmo, varianza = t_var, cargas = t_cargas, autovalores = t_auto,
         retencion = retencion, grafico_autovalores = g_auto,
         grafico_acumulada = g_acum, n_retener = n_retener, modelo = modelo,
         datos = sub,
         valores = list(kmo = kmo$MSA, msa = kmo$MSAi, sdev = modelo$sdev,
                        importancia = importancia, cargas = modelo$rotation)),
    mostrar = c("kmo", "varianza", "cargas", "retencion"),
    titulo = "Componentes principales", clase = "matlite_acp")
}

#' Puntuaciones y ranking de una componente
#'
#' Calcula las puntuaciones (*scores*) de los casos en una componente y
#' presenta el ranking de los `n` primeros.
#'
#' - Sin `datos`, usa las puntuaciones de los casos con que se calcularon
#'   las componentes.
#' - Con `datos` (por ejemplo, la selección que aún conserva los
#'   outliers), puntúa esos casos con las componentes ya calculadas
#'   (`predict()`).
#'
#' @param acp Resultado de [lite_acp()].
#' @param datos Data frame opcional con los casos que se quieren puntuar.
#' @param componente Número de la componente. Por defecto, 1.
#' @param n Número de casos del ranking. Por defecto, 10.
#' @param etiquetas Nombres que se mostrarán para las variables en la
#'   tabla. Por defecto, sus nombres.
#' @param caption Título de la tabla.
#' @return Objeto `matlite` con `$ranking`, `$grafico` y `$puntuaciones`
#'   (data frame con la puntuación de todos los casos, ordenado).
#' @examples
#' \dontrun{
#' lite_acp_ranking(componentes)
#' lite_acp_ranking(componentes, datos = seleccion)
#' }
#' @export
lite_acp_ranking <- function(acp, datos = NULL, componente = 1, n = 10,
                             etiquetas = NULL, caption = NULL) {
  if (!inherits(acp, "matlite_acp")) stop("`acp` debe ser el resultado de lite_acp().", call. = FALSE)
  vars <- rownames(acp$modelo$rotation)
  if (is.null(datos)) {
    base <- acp$datos
    sc <- acp$modelo$x[, componente]
    origen <- "casos del c\u00e1lculo"
  } else {
    faltan <- setdiff(vars, names(datos))
    if (length(faltan) > 0) stop("Faltan variables en `datos`: ", paste(faltan, collapse = ", "), call. = FALSE)
    base <- quitar_na(datos[, vars, drop = FALSE])
    sc <- stats::predict(acp$modelo, newdata = base)[, componente]
    origen <- "casos de los datos indicados"
  }
  pc <- paste0("PC", componente)
  punt <- cbind(data.frame(scores = sc), base[, vars, drop = FALSE])
  punt <- punt[order(-punt$scores), , drop = FALSE]
  top <- utils::head(punt, n)

  caption <- caption %||% sprintf("Puntuaciones en %s (Top-%d, %s)", pc, n, origen)
  t_rank <- MATrstars::kable_rstars(
    top, caption = caption,
    col.names = c("Caso", "Puntuaci\u00f3n", etiquetas %||% vars),
    digits = rep(3, ncol(top))) %>%
    kableExtra::column_spec(1, bold = TRUE, extra_css = "text-align: left;")

  top$caso <- factor(rownames(top), levels = rev(rownames(top)))
  g <- ggplot2::ggplot(top, ggplot2::aes(x = .data$caso, y = .data$scores)) +
    ggplot2::geom_bar(stat = "identity", colour = "red", fill = "orange", alpha = 0.7) +
    ggplot2::geom_text(ggplot2::aes(label = round(.data$scores, 3)),
                       hjust = 1.1, colour = "dark blue", size = 3) +
    ggplot2::coord_flip() +
    ggplot2::ggtitle(paste0("Ranking por puntuaci\u00f3n en ", pc), subtitle = paste0("Top-", n)) +
    ggplot2::xlab("") + ggplot2::ylab("Puntuaci\u00f3n")

  names(punt)[1] <- pc
  nuevo_matlite(list(ranking = t_rank, grafico = g, puntuaciones = punt),
                mostrar = c("ranking", "grafico"),
                titulo = paste("Ranking", pc))
}
