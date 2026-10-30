# =============================================================================
# Escalado multidimensional no metrico (capitulo Lite 8 <- Guia 12)
# =============================================================================

calidad_stress <- function(s) {
  as.character(cut(s, breaks = c(-Inf, 0.025, 0.05, 0.10, 0.20, Inf),
                   labels = c("Perfecta", "Excelente", "Buena", "Razonable", "Pobre"),
                   right = TRUE))
}

emd_sin_avisos <- function(...) sin_avisos(smacof::mds, ...)

#' Escalado multidimensional: exploración y número de dimensiones
#'
#' Primera parte del escalado multidimensional no métrico de la Guía:
#' tabla y gráfico de los perfiles (coordenadas paralelas), matriz de
#' distancias euclídeas entre los casos (mapa de calor) y valores del Stress
#' para 1, 2, ... `max_dim` dimensiones (algoritmo SMACOF), con su
#' calificación según la escala de Kruskal y el gráfico de codo.
#'
#' @param datos Data frame con los atributos (variables métricas) de cada
#'   caso.
#' @param variables Atributos, sin comillas. Si se omiten, todas las
#'   numéricas.
#' @param max_dim Número máximo de dimensiones. Por defecto, 6 (o el número
#'   de atributos, si es menor).
#' @param tipo Tipo de escalado de [smacof::mds()]: `"ordinal"` (no
#'   métrico, por defecto) o `"ratio"`/`"interval"` (métrico).
#' @return Objeto `matlite` con `$tabla_perfiles`, `$perfiles`,
#'   `$distancias`, `$stress` y `$codo`.
#' @examples
#' \dontrun{
#' lite_emd_dimensiones(PERCEPCION)
#' }
#' @export
lite_emd_dimensiones <- function(datos, variables = NULL, max_dim = 6, tipo = "ordinal") {
  sub <- quitar_na(seleccionar(datos, rlang::enquo(variables)))
  max_dim <- min(max_dim, ncol(sub), nrow(sub) - 1)

  t_perf <- MATrstars::kable_rstars(round(sub, 2), caption = "Valoraciones por caso y atributo")

  largo <- data.frame(CASO = rep(rownames(sub), times = ncol(sub)),
                      Atributo = rep(names(sub), each = nrow(sub)),
                      Valoracion = unlist(sub, use.names = FALSE))
  g_perf <- ggplot2::ggplot(largo, ggplot2::aes(x = .data$Atributo, y = .data$Valoracion,
                                                group = .data$CASO, color = .data$CASO)) +
    ggplot2::geom_line(linewidth = 0.7, alpha = 0.7) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = "Perfiles de valoraci\u00f3n",
                  subtitle = paste0("Coordenadas paralelas - ", ncol(sub), " atributos"),
                  x = NULL, y = "Valoraci\u00f3n media") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12),
                   plot.subtitle = ggplot2::element_text(size = 10),
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
                   legend.position = "right", legend.title = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(size = 7))

  prox <- stats::dist(sub)
  g_dist <- sin_avisos(factoextra::fviz_dist, prox, lab_size = 6) +
    ggplot2::ggtitle("Matriz de distancias entre casos") +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", hjust = 0.5))

  stress <- vapply(seq_len(max_dim), function(p) emd_sin_avisos(prox, ndim = p, type = tipo)$stress,
                   numeric(1))
  df <- data.frame(Dimensiones = seq_len(max_dim), Stress = stress, Calidad = calidad_stress(stress))
  t_stress <- MATrstars::kable_rstars(df, caption = "Stress por n\u00famero de dimensiones",
                                      digits = c(0, 4, NA))
  g_codo <- ggplot2::ggplot(df, ggplot2::aes(x = .data$Dimensiones, y = .data$Stress)) +
    ggplot2::geom_line(color = "#2C3E50", linewidth = 1) +
    ggplot2::geom_point(color = "#E74C3C", size = 3) +
    ggplot2::geom_text(ggplot2::aes(label = round(.data$Stress, 4)), vjust = -1, size = 3.5) +
    ggplot2::geom_hline(yintercept = 0.05, linetype = "dashed", color = "grey50") +
    ggplot2::annotate("text", x = max_dim - 0.5, y = 0.055, label = "Umbral 0,05 (bueno)",
                      color = "grey50", size = 3) +
    ggplot2::scale_x_continuous(breaks = seq_len(max_dim)) +
    ggplot2::labs(title = "Selecci\u00f3n de dimensiones: gr\u00e1fico de codo del Stress",
                  subtitle = paste0("EMD (SMACOF) - tipo ", tipo),
                  x = "N\u00famero de dimensiones (p)", y = "Stress") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12),
                   plot.subtitle = ggplot2::element_text(size = 10))

  nuevo_matlite(
    list(tabla_perfiles = t_perf, perfiles = g_perf, distancias = g_dist,
         stress = t_stress, codo = g_codo, valores = list(stress = stress)),
    mostrar = c("perfiles", "distancias", "stress", "codo"),
    titulo = "Escalado multidimensional (exploraci\u00f3n)")
}

#' Escalado multidimensional: solución y mapa perceptual
#'
#' Segunda parte del escalado multidimensional no métrico de la Guía:
#' solución en `ndim` dimensiones (algoritmo SMACOF), Stress, coordenadas,
#' diagrama de Shepard, correlaciones entre atributos y dimensiones (para
#' interpretarlas) y mapas perceptuales:
#'
#' - con 2 dimensiones, el mapa y el mapa con los vectores de los
#'   atributos (biplot);
#' - con 3 o más, los mapas de cada par de dimensiones.
#'
#' @inheritParams lite_emd_dimensiones
#' @param ndim Número de dimensiones. Por defecto, 2.
#' @param titulo,subtitulo Título y subtítulo del mapa perceptual.
#' @return Objeto `matlite` con `$ajuste`, `$coordenadas`, `$shepard`,
#'   `$correlaciones`, `$mapa` y `$biplot` (2 dimensiones) o `$mapas`
#'   (3 o más), y `$modelo` (objeto smacof).
#' @examples
#' \dontrun{
#' mapa <- lite_emd(PERCEPCION, ndim = 2)
#' mapa$mapa
#' }
#' @export
lite_emd <- function(datos, variables = NULL, ndim = 2, tipo = "ordinal",
                     titulo = "Mapa perceptual", subtitulo = NULL) {
  sub <- quitar_na(seleccionar(datos, rlang::enquo(variables)))
  if (ndim < 2) stop("`ndim` debe ser al menos 2 para dibujar un mapa.", call. = FALSE)
  prox <- stats::dist(sub)
  sol <- emd_sin_avisos(prox, ndim = ndim, type = tipo)
  dims <- paste0("D", seq_len(ndim))
  coords <- as.data.frame(sol$conf); names(coords) <- dims
  coords$CASO <- rownames(coords)

  ajuste <- data.frame(Dimensiones = ndim, Stress = sol$stress,
                       Calidad = calidad_stress(sol$stress), Iteraciones = sol$niter)
  t_aj <- MATrstars::kable_rstars(ajuste, caption = "Ajuste de la soluci\u00f3n",
                                  digits = c(0, 4, NA, 0))
  t_coord <- MATrstars::kable_rstars(round(sol$conf, 4),
                                     caption = paste("Coordenadas en", ndim, "dimensiones"))

  # Shepard
  sh <- data.frame(delta = as.vector(sol$delta), confdist = as.vector(sol$confdist),
                   dhat = as.vector(sol$dhat))
  sh <- sh[sh$delta > 0, ]
  sh <- sh[order(sh$delta), ]
  g_sh <- ggplot2::ggplot(sh) +
    ggplot2::geom_point(ggplot2::aes(x = .data$delta, y = .data$confdist),
                        color = "#E74C3C", size = 1.5, alpha = 0.6) +
    ggplot2::geom_step(ggplot2::aes(x = .data$delta, y = .data$dhat), color = "#2C3E50", linewidth = 0.8) +
    ggplot2::labs(title = "Diagrama de Shepard",
                  x = "Proximidades originales (distancias observadas)",
                  y = "Distancias en la configuraci\u00f3n MDS") +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12))

  # Correlaciones atributo-dimension
  cors <- stats::cor(sub, sol$conf)
  colnames(cors) <- paste0("Dim.", seq_len(ndim))
  t_cor <- MATrstars::kable_rstars(round(cors, 3), caption = "Correlaciones atributos-dimensiones")

  mapa_par <- function(dx, dy, tit, sub_t = NULL, tam = 3, grande = TRUE) {
    ggplot2::ggplot(coords, ggplot2::aes(x = .data[[dx]], y = .data[[dy]], label = .data$CASO)) +
      ggplot2::geom_point(color = "#E74C3C", size = tam, alpha = 0.8) +
      ggrepel::geom_label_repel(size = if (grande) 3 else 2, color = "black", alpha = if (grande) 0.6 else 0.5,
                                max.overlaps = 20, label.padding = if (grande) 0.25 else 0.15) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
      ggplot2::labs(title = tit, subtitle = sub_t,
                    x = if (grande) sub("D", "Dimensi\u00f3n ", dx) else dx,
                    y = if (grande) sub("D", "Dimensi\u00f3n ", dy) else dy) +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = if (grande) 14 else 10),
                     plot.subtitle = ggplot2::element_text(size = 10))
  }

  res <- list(ajuste = t_aj, coordenadas = t_coord, shepard = g_sh, correlaciones = t_cor)
  if (ndim == 2) {
    res$mapa <- mapa_par("D1", "D2", titulo, subtitulo)
    dfc <- as.data.frame(cors); dfc$Atributo <- rownames(dfc)
    esc <- max(abs(coords$D1), abs(coords$D2)) * 0.8
    res$biplot <- ggplot2::ggplot() +
      ggplot2::geom_point(data = coords, ggplot2::aes(x = .data$D1, y = .data$D2),
                          color = "#E74C3C", size = 3, alpha = 0.8) +
      ggrepel::geom_label_repel(data = coords, ggplot2::aes(x = .data$D1, y = .data$D2, label = .data$CASO),
                                size = 2.5, color = "black", alpha = 0.5, max.overlaps = 20) +
      ggplot2::geom_segment(data = dfc, ggplot2::aes(x = 0, y = 0, xend = .data$Dim.1 * esc,
                                                     yend = .data$Dim.2 * esc),
                            arrow = grid::arrow(length = grid::unit(0.2, "cm")),
                            color = "#7B99AD", linewidth = 0.8) +
      ggplot2::geom_text(data = dfc, ggplot2::aes(x = .data$Dim.1 * esc * 1.1, y = .data$Dim.2 * esc * 1.1,
                                                  label = .data$Atributo),
                         color = "#4A6B82", fontface = "bold", size = 3.5) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
      ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
      ggplot2::labs(title = "MAPA PERCEPTUAL CON VECTORES DE ATRIBUTOS",
                    subtitle = "Direcci\u00f3n y longitud indican correlaci\u00f3n con cada dimensi\u00f3n",
                    x = "Dimensi\u00f3n 1", y = "Dimensi\u00f3n 2") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13),
                     plot.subtitle = ggplot2::element_text(size = 10))
    mostrar <- c("ajuste", "shepard", "mapa", "correlaciones", "biplot")
  } else {
    pares <- utils::combn(dims, 2, simplify = FALSE)
    graf <- lapply(pares, function(p) {
      mapa_par(p[1], p[2], sprintf("Dimensi\u00f3n %s vs Dimensi\u00f3n %s",
                                   sub("D", "", p[1]), sub("D", "", p[2])),
               tam = 2, grande = FALSE)
    })
    res$mapas <- nueva_lista(MATrstars::create_patchwork(graf))
    mostrar <- c("ajuste", "shepard", "correlaciones", "mapas")
  }
  res$modelo <- sol
  res$valores <- list(stress = sol$stress, niter = sol$niter, correlaciones = cors,
                      coordenadas = sol$conf)
  nuevo_matlite(res, mostrar = mostrar,
                titulo = sprintf("Escalado multidimensional (%d dimensiones)", ndim))
}
