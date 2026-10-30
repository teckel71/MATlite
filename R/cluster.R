# =============================================================================
# Cluster jerarquico de Ward (capitulo Lite 5 <- Guia 7.3)
# =============================================================================

#' Análisis clúster jerárquico (método de Ward)
#'
#' Reproduce el análisis clúster jerárquico de la Guía en dos fases:
#'
#' 1. **Exploración** (`k = NULL`): tipifica las variables, calcula la
#'    matriz de distancias euclídeas (mapa de calor), aplica el método de
#'    Ward (`hclust(method = "ward.D2")`), dibuja el dendrograma y sugiere un
#'    número de grupos con el método de la silueta.
#' 2. **Resultados** (`k` = número de grupos decidido): dendrograma con los
#'    grupos marcados, asignación de cada caso a su grupo (numerados de
#'    izquierda a derecha según el dendrograma, como en la Guía), tabla de
#'    centroides, gráficos de medias por grupo, composición de cada grupo y
#'    gráficos de dispersión de cada par de variables coloreados por grupo.
#'
#' @param datos Data frame con las variables clasificadoras (sin valores
#'   ausentes).
#' @param variables Variables, sin comillas. Si se omiten, todas las
#'   numéricas.
#' @param k Número de grupos. Si es `NULL` (por defecto), solo se hace la
#'   fase de exploración.
#' @param k_max Número máximo de grupos que se prueba con la silueta.
#' @param titulo,subtitulo Título y subtítulo de los dendrogramas. Por
#'   defecto, "Método de Ward" y "Variables originales tipificadas.".
#' @param subtitulo_graficos Subtítulo de los gráficos de medias y dispersión.
#' @return Objeto `matlite`. En la fase de exploración: `$distancias`,
#'   `$dendrograma`, `$silueta`, `$k_sugerido`, `$modelo`. En la fase de
#'   resultados, además: `$centroides`, `$graficos_centroides`,
#'   `$composicion`, `$dispersion` y `$datos` (el data frame con la nueva
#'   columna `grupo`).
#' @examples
#' \dontrun{
#' exploracion <- lite_cluster_ward(seleccion)
#' grupos <- lite_cluster_ward(seleccion, k = 5)
#' grupos$centroides
#' }
#' @export
lite_cluster_ward <- function(datos, variables = NULL, k = NULL, k_max = 15,
                              titulo = NULL,
                              subtitulo = "Variables originales tipificadas.",
                              subtitulo_graficos = NULL) {
  titulo <- titulo %||% "M\u00e9todo de Ward"
  sub <- seleccionar(datos, rlang::enquo(variables))
  if (any(!stats::complete.cases(sub))) {
    stop("Hay valores ausentes en las variables clasificadoras. ",
         "Tr\u00e1talos antes con lite_na(..., accion = \"eliminar\").", call. = FALSE)
  }
  if (ncol(sub) < 2) stop("Se necesitan al menos dos variables num\u00e9ricas.", call. = FALSE)
  vars <- names(sub)

  zsub <- data.frame(scale(sub))
  d <- stats::dist(zsub)
  modelo <- stats::hclust(d, method = "ward.D2")

  g_dist <- sin_avisos(factoextra::fviz_dist, d, lab_size = 8)

  estilo_dend <- function(p) {
    p <- p + ggplot2::labs(title = titulo, subtitle = subtitulo) +
      ggplot2::theme_grey() + ggplot2::theme(legend.position = "none")
    p$layers[[1]]$aes_params$linewidth <- 0.2
    p
  }
  d1 <- estilo_dend(sin_avisos(factoextra::fviz_dend, modelo, cex = 0.6, lwd = 0.2, rect = FALSE,
                                          labels_track_height = 5.5))

  if (is.null(k)) {
    kmax <- min(k_max, nrow(zsub) - 1)
    sil <- sin_avisos(factoextra::fviz_nbclust, x = zsub, FUNcluster = factoextra::hcut,
                                    method = "silhouette", hc_method = "ward.D2",
                                    k.max = kmax)
    k_sug <- as.integer(as.character(sil$data$clusters[which.max(sil$data$y)]))
    message("M\u00e9todo de la silueta: el n\u00famero de grupos sugerido es k = ", k_sug,
            ". Decide k (tambi\u00e9n por interpretabilidad) y vuelve a ejecutar con k = ...")
    return(nuevo_matlite(
      list(distancias = g_dist, dendrograma = d1, silueta = sil,
           k_sugerido = k_sug, modelo = modelo),
      mostrar = c("distancias", "dendrograma", "silueta"),
      titulo = "Cl\u00faster jer\u00e1rquico (exploraci\u00f3n)", clase = "matlite_cluster"))
  }

  # --- Fase de resultados ---------------------------------------------------
  d2 <- estilo_dend(sin_avisos(factoextra::fviz_dend, modelo, cex = 0.6, lwd = 0.2, k = k,
                                          k_colors = "black", labels_track_height = 5.5,
                                          rect = TRUE, rect_border = "npg",
                                          rect_fill = TRUE))

  # Grupos numerados de izquierda a derecha segun el dendrograma (como la Guia).
  cl <- stats::cutree(modelo, k = k)
  ord_labels <- modelo$labels[modelo$order]
  pos <- match(names(cl), ord_labels)
  mins <- tapply(pos, cl, min)
  mapa <- stats::setNames(seq_along(mins), names(sort(mins)))
  grupo <- factor(mapa[as.character(cl)], levels = seq_len(k))

  datos_g <- datos
  datos_g$grupo <- grupo
  sub_g <- sub
  sub_g$grupo <- grupo

  # Centroides
  medias <- stats::aggregate(sub, by = list(grupo = grupo), FUN = mean)
  obs <- as.vector(table(grupo))
  centroides <- data.frame(grupo = medias$grupo, obs = obs, medias[, vars, drop = FALSE])
  t_cent <- MATrstars::kable_rstars(
    centroides, caption = sprintf("M\u00e9todo de Ward. %d grupos. Medias de variables", k),
    col.names = c("Cl\u00faster", "Observaciones", vars),
    digits = c(0, 0, rep(3, length(vars))), row.names = FALSE)

  graf_cent <- lapply(vars, function(v) {
    ggplot2::ggplot(centroides, ggplot2::aes(y = .data[[v]], x = .data$grupo)) +
      ggplot2::geom_bar(stat = "identity", colour = "red", fill = "orange", alpha = 0.7) +
      ggplot2::ggtitle(paste0(v, ". Media por grupos."), subtitle = subtitulo_graficos) +
      ggplot2::xlab("Grupo") + ggplot2::ylab(v)
  })
  names(graf_cent) <- paste0("grafico_", vars)

  # Composicion de cada grupo
  compo <- lapply(seq_len(k), function(g) {
    MATrstars::kable_rstars(sub[grupo == g, vars, drop = FALSE],
                            caption = paste("M\u00e9todo de Ward. Grupo ", g, "."),
                            digits = rep(3, length(vars)))
  })

  # Dispersion de cada par de variables
  combinaciones <- utils::combn(vars, 2, simplify = FALSE)
  graf_disp <- lapply(combinaciones, function(par) {
    ggplot2::ggplot(sub_g, ggplot2::aes(x = .data[[par[1]]], y = .data[[par[2]]],
                                         color = .data$grupo)) +
      ggplot2::geom_point() +
      ggplot2::labs(title = paste("GR\u00c1FICO", par[1], "-", par[2]),
                    subtitle = subtitulo_graficos, color = "grupo") +
      ggplot2::xlab(par[1]) + ggplot2::ylab(par[2]) +
      ggplot2::scale_color_brewer(palette = "Set1")
  })

  tam <- paste(sprintf("grupo %d: %d", seq_len(k), obs), collapse = ", ")
  message("Tama\u00f1o de los grupos (", tam, ").")

  nuevo_matlite(
    list(dendrograma = d2, centroides = t_cent,
         graficos_centroides = nueva_lista(MATrstars::create_patchwork(graf_cent)),
         composicion = nueva_lista(compo),
         dispersion = nueva_lista(MATrstars::create_patchwork(graf_disp)),
         datos = datos_g, distancias = g_dist, modelo = modelo,
         valores = list(tamanos = obs, centroides = centroides)),
    mostrar = c("dendrograma", "centroides", "graficos_centroides",
                "composicion", "dispersion"),
    titulo = sprintf("Cl\u00faster jer\u00e1rquico (Ward, k = %d)", k), clase = "matlite_cluster")
}
