# =============================================================================
# Modelo logit binomial (capitulo Lite 6 <- Guia 10)
# =============================================================================

.col_logit <- c("#E74C3C", "#2ECC71")

#' Convierte una variable en factor
#'
#' Convierte una variable categórica (o una dependiente 0/1) en factor,
#' fija el orden de sus niveles y la categoría de referencia, y muestra la
#' codificación (contrastes) con que entrará en un modelo.
#'
#' @param datos Data frame.
#' @param variable Variable, sin comillas.
#' @param niveles Orden de los niveles (opcional). El primero es la
#'   categoría de referencia.
#' @param referencia Categoría de referencia (opcional; alternativa a
#'   `niveles`).
#' @return El data frame con la variable convertida.
#' @examples
#' \dontrun{
#' LICENCIAS <- lite_factor(LICENCIAS, EFLO,
#'                          niveles = c("ANTIGUA", "MADURA", "RENOVADA"))
#' LICENCIAS <- lite_factor(LICENCIAS, LICENCIA)
#' }
#' @export
lite_factor <- function(datos, variable, niveles = NULL, referencia = NULL) {
  var <- rlang::as_name(rlang::ensym(variable))
  if (!var %in% names(datos)) stop("La variable ", var, " no existe en el data frame.", call. = FALSE)
  x <- datos[[var]]
  f <- if (is.null(niveles)) factor(x) else factor(x, levels = niveles)
  if (!is.null(niveles) && any(is.na(f) & !is.na(x))) {
    stop("Hay valores de ", var, " que no est\u00e1n en `niveles`: ",
         paste(unique(x[is.na(f) & !is.na(x)]), collapse = ", "), call. = FALSE)
  }
  if (!is.null(referencia)) f <- stats::relevel(f, ref = referencia)
  datos[[var]] <- f
  message(var, " es ahora un factor con ", nlevels(f), " niveles: ",
          paste(levels(f), collapse = ", "), ". Referencia: ", levels(f)[1], ".")
  if (nlevels(f) >= 2) {
    cat("Codificaci\u00f3n (contrastes) de ", var, ":\n", sep = "")
    print(stats::contrasts(f))
  }
  datos
}

#' Análisis descriptivo previo a un modelo logit
#'
#' Describe cómo se relaciona la variable dependiente (binaria) con las
#' explicativas, como en la Guía: distribución de la dependiente; para cada
#' explicativa categórica, tabla cruzada y gráfico de barras apiladas (en
#' proporción); para las métricas, tabla de medias por grupo y gráficos de
#' barras.
#'
#' @param datos Data frame.
#' @param dependiente Variable dependiente (dos categorías), sin comillas.
#' @param variables Explicativas a describir (tidy-select). Si se omiten, se
#'   usan todas; las categóricas con más de 10 niveles se descartan.
#' @param etiquetas Nombres para las categorías de la dependiente, por
#'   ejemplo `c("0" = "Denegada", "1" = "Concedida")`.
#' @return Objeto `matlite` con `$dependiente`, `$cruzadas`,
#'   `$graficos_categoricas`, `$medias` y `$graficos_metricas`.
#' @examples
#' \dontrun{
#' lite_logit_descriptivo(LICENCIAS, LICENCIA,
#'                        etiquetas = c("0" = "Denegada", "1" = "Concedida"))
#' }
#' @export
lite_logit_descriptivo <- function(datos, dependiente, variables = NULL,
                                   etiquetas = NULL) {
  dep <- rlang::as_name(rlang::ensym(dependiente))
  y <- factor(datos[[dep]])
  if (nlevels(y) != 2) stop("La dependiente debe tener exactamente dos categor\u00edas.", call. = FALSE)
  datos[[dep]] <- y
  niv <- levels(y)
  etiquetas <- etiquetas %||% stats::setNames(niv, niv)
  colores <- stats::setNames(.col_logit, niv)
  rotulo <- dep

  vq <- rlang::enquo(variables)
  expl <- if (rlang::quo_is_null(vq)) setdiff(names(datos), dep)
          else setdiff(names(dplyr::select(datos, !!vq)), dep)
  es_num <- vapply(datos[expl], is.numeric, logical(1))
  categ <- expl[!es_num]
  n_niv <- vapply(datos[categ], function(v) length(unique(v)), integer(1))
  if (rlang::quo_is_null(vq) && any(n_niv > 10)) {
    message("Se omiten por tener m\u00e1s de 10 categor\u00edas: ",
            paste(categ[n_niv > 10], collapse = ", "), ".")
    categ <- categ[n_niv <= 10]
  }
  metr <- expl[es_num]

  # Distribucion de la dependiente
  dist_y <- as.data.frame(table(y))
  names(dist_y) <- c(dep, "Frecuencia")
  dist_y$Porcentaje <- 100 * dist_y$Frecuencia / sum(dist_y$Frecuencia)
  t_dep <- MATrstars::kable_rstars(dist_y, caption = paste("Distribuci\u00f3n de", dep),
                                   digits = c(NA, 0, 2))

  # Categoricas
  cruzadas <- lapply(categ, function(v) {
    tb <- table(datos[[v]], y)
    df <- as.data.frame.matrix(tb)
    pct <- round(100 * df[[niv[2]]] / rowSums(df), 2)
    df <- cbind(df, pct)
    names(df) <- c(niv, paste0("% ", etiquetas[niv[2]]))
    MATrstars::kable_rstars(df, caption = paste(dep, "seg\u00fan", v), digits = c(0, 0, 2))
  })
  names(cruzadas) <- categ
  graf_cat <- lapply(categ, function(v) {
    g <- ggplot2::ggplot(datos, ggplot2::aes(x = .data[[v]], fill = .data[[dep]])) +
      ggplot2::geom_bar(position = "fill") +
      ggplot2::scale_y_continuous(labels = scales::percent_format()) +
      ggplot2::scale_fill_manual(values = colores, labels = etiquetas) +
      ggplot2::labs(title = paste(rotulo, "seg\u00fan", v), x = v, y = "Proporci\u00f3n",
                    fill = rotulo) +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11))
    if (length(unique(datos[[v]])) > 3)
      g <- g + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1, size = 8))
    g
  })
  g_cat <- if (length(graf_cat) > 0) {
    patchwork::wrap_plots(graf_cat) + patchwork::plot_layout(guides = "collect")
  } else NULL

  # Metricas
  t_medias <- NULL; g_met <- NULL
  if (length(metr) > 0) {
    medias <- stats::aggregate(datos[metr], by = list(y), FUN = mean, na.rm = TRUE)
    names(medias)[1] <- dep
    t_medias <- MATrstars::kable_rstars(medias, caption = paste("Medias por grupo de", dep),
                                        digits = c(NA, rep(2, length(metr))))
    graf_met <- lapply(metr, function(v) {
      dm <- data.frame(grupo = factor(niv, levels = niv),
                       media = tapply(datos[[v]], y, mean, na.rm = TRUE))
      ggplot2::ggplot(dm, ggplot2::aes(x = .data$grupo, y = .data$media, fill = .data$grupo)) +
        ggplot2::geom_col(width = 0.6) +
        ggplot2::geom_text(ggplot2::aes(label = round(.data$media, 1)), vjust = -0.5, size = 3) +
        ggplot2::scale_fill_manual(values = colores, labels = etiquetas) +
        ggplot2::labs(title = v, x = NULL, y = "Media", fill = rotulo) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 11, hjust = 0.5),
                       legend.position = "bottom",
                       legend.title = ggplot2::element_text(size = 9),
                       legend.text = ggplot2::element_text(size = 8))
    })
    g_met <- nueva_lista(MATrstars::create_patchwork(graf_met))
  }

  nuevo_matlite(
    list(dependiente = t_dep, cruzadas = nueva_lista(cruzadas),
         graficos_categoricas = g_cat, medias = t_medias, graficos_metricas = g_met),
    mostrar = c("dependiente", "cruzadas", "graficos_categoricas", "medias", "graficos_metricas"),
    titulo = paste("Descriptivo de", dep))
}

#' Divide los datos en muestra de estimación y muestra de test
#'
#' Por defecto la partición es secuencial (los `n_estimacion` primeros casos
#' para estimar y el resto para validar), como en la Guía. Con
#' `aleatoria = TRUE` los casos se eligen al azar (reproducible con
#' `semilla`).
#'
#' @param datos Data frame.
#' @param n_estimacion Número de casos de la muestra de estimación.
#' @param dependiente Variable dependiente (opcional), para mostrar su
#'   distribución en cada muestra.
#' @param aleatoria Si `TRUE`, partición aleatoria.
#' @param semilla Semilla para la partición aleatoria.
#' @return Objeto `matlite` con `$estimacion` y `$test` (data frames) y
#'   `$resumen` (tabla).
#' @examples
#' \dontrun{
#' muestras <- lite_particion(LICENCIAS, n_estimacion = 80, dependiente = LICENCIA)
#' muestras$estimacion
#' }
#' @export
lite_particion <- function(datos, n_estimacion, dependiente = NULL,
                           aleatoria = FALSE, semilla = 123) {
  n <- nrow(datos)
  if (n_estimacion >= n || n_estimacion < 1) {
    stop("`n_estimacion` debe estar entre 1 y ", n - 1, ".", call. = FALSE)
  }
  if (aleatoria) {
    set.seed(semilla)
    idx <- sort(sample(seq_len(n), n_estimacion))
  } else {
    idx <- seq_len(n_estimacion)
  }
  est <- datos[idx, , drop = FALSE]
  tst <- datos[-idx, , drop = FALSE]

  resumen <- data.frame(Muestra = c("Estimaci\u00f3n", "Test"), Casos = c(nrow(est), nrow(tst)))
  dq <- rlang::enquo(dependiente)
  if (!rlang::quo_is_null(dq)) {
    dep <- rlang::as_name(dq)
    niv <- levels(factor(datos[[dep]]))
    for (l in niv) {
      resumen[[paste0(dep, " = ", l)]] <- c(sum(est[[dep]] == l), sum(tst[[dep]] == l))
    }
  }
  message("Muestra de estimaci\u00f3n: ", nrow(est), " casos. Muestra de test: ",
          nrow(tst), " casos (partici\u00f3n ", if (aleatoria) "aleatoria" else "secuencial", ").")
  nuevo_matlite(
    list(resumen = MATrstars::kable_rstars(resumen, caption = "Partici\u00f3n de la muestra"),
         estimacion = est, test = tst),
    mostrar = "resumen", titulo = "Partici\u00f3n")
}

# --- Nucleo comun de lite_logit() y lite_logit_seleccion() -------------------

presentar_logit <- function(modelo, datos, test, umbral, titulo_coef) {
  sal <- MATrstars::presenta_logit(
    modelo, datos = datos, umbral = umbral,
    captions = c(titulo_coef, "Bondad del ajuste", "Odd ratios",
                 "Matriz de confusi\u00f3n (estimaci\u00f3n)"))

  dep <- names(modelo$model)[1]
  acierto <- function(d) {
    p <- stats::predict(modelo, newdata = d, type = "response")
    y <- factor(d[[dep]])
    pred <- ifelse(p > umbral, levels(y)[2], levels(y)[1])
    100 * mean(pred == as.character(y))
  }

  # Wald conjunto para cada factor con mas de dos niveles
  mm_assign <- attr(stats::model.matrix(modelo), "assign")
  terminos <- attr(stats::terms(modelo), "term.labels")
  b <- stats::coef(modelo); V <- stats::vcov(modelo)
  wald <- NULL
  for (j in seq_along(terminos)) {
    idx <- which(mm_assign == j)
    if (length(idx) > 1) {
      W <- as.numeric(t(b[idx]) %*% solve(V[idx, idx]) %*% b[idx])
      p <- stats::pchisq(W, df = length(idx), lower.tail = FALSE)
      wald <- rbind(wald, data.frame(Factor = terminos[j], Chi2 = W, gl = length(idx),
                                     p = p, Decision = decision(p, h0 = "coeficientes nulos")))
    }
  }
  t_wald <- if (!is.null(wald)) {
    MATrstars::kable_rstars(wald, caption = "Contraste de Wald conjunto para factores",
                            col.names = c("Factor", "\u03c7\u00b2", "g.l.", "p-valor", "Decisi\u00f3n"),
                            digits = c(NA, 4, 0, 4, NA), row.names = FALSE)
  } else NULL

  # Devianza (anova) y AIC frente al modelo nulo
  dev <- as.data.frame(stats::anova(modelo, test = "Chisq"))
  t_dev <- MATrstars::kable_rstars(dev, caption = "An\u00e1lisis de la devianza",
                                   col.names = c("Variable", "g.l.", "Devianza", "g.l. residuales",
                                                 "Devianza residual", "p-valor"),
                                   digits = c(0, 4, 0, 4, 4))
  nulo <- stats::glm(stats::as.formula(paste(dep, "~ 1")), data = modelo$model,
                     family = stats::binomial(link = "logit"))
  aic <- data.frame(Modelo = c("Nulo (solo constante)", "Estimado"),
                    gl = c(length(stats::coef(nulo)), length(stats::coef(modelo))),
                    AIC = c(stats::AIC(nulo), stats::AIC(modelo)))
  t_aic <- MATrstars::kable_rstars(aic, caption = "AIC: modelo estimado frente al nulo",
                                   col.names = c("Modelo", "Par\u00e1metros", "AIC"),
                                   digits = c(NA, 0, 4))

  res <- list(coeficientes = sal$coef, ajuste = sal$gof, odds = sal$or,
              wald_factores = t_wald, devianza = t_dev, aic = t_aic,
              confusion = sal$conf)
  ac_test <- NA
  if (!is.null(test)) {
    res$confusion_test <- MATrstars::presenta_logit(
      modelo, datos = test, umbral = umbral,
      captions = c("", "", "", "Matriz de confusi\u00f3n (TEST)"))$conf
    ac_test <- acierto(test)
  }

  n <- nrow(modelo$model)
  L1 <- as.numeric(stats::logLik(modelo)); L0 <- as.numeric(stats::logLik(nulo))
  nagel <- (1 - exp(-(2 / n) * (L1 - L0))) / (1 - exp((2 / n) * L0))

  res$modelo <- modelo
  res$datos_estimacion <- datos
  res$datos_test <- test
  res$valores <- list(formula = stats::formula(modelo), coef = summary(modelo)$coefficients,
                      aic = stats::AIC(modelo), aic_nulo = stats::AIC(nulo),
                      nagelkerke = nagel, wald = wald,
                      acierto_estimacion = acierto(datos), acierto_test = ac_test,
                      umbral = umbral)
  res
}

#' Estima un modelo logit binomial
#'
#' Estima el modelo logit de la variable dependiente (dos categorías) sobre
#' las explicativas indicadas y reúne todos los resultados de la Guía:
#' coeficientes con el contraste de Wald individual, bondad del ajuste
#' (R² de Nagelkerke, AIC, devianzas), *odd ratios*, contraste de Wald
#' conjunto para cada factor de más de dos categorías (calculado
#' automáticamente), análisis de la devianza, AIC frente al modelo nulo y
#' matrices de confusión en la muestra de estimación y, si se indica, en
#' la de test.
#'
#' La función construye la fórmula del modelo a partir de las variables
#' indicadas (en ese orden) y la muestra en la consola.
#'
#' @param datos Data frame de estimación.
#' @param dependiente Variable dependiente, sin comillas. La categoría
#'   modelizada ("éxito") es la segunda (por ejemplo, `1` en una 0/1).
#' @param explicativas Variables explicativas (tidy-select), por ejemplo
#'   `c(SOLVENCIA, IPUNT, EFLO)`.
#' @param test Data frame opcional de test, para evaluar la capacidad
#'   predictiva fuera de la muestra.
#' @param umbral Probabilidad a partir de la cual se clasifica un caso como
#'   "éxito". Por defecto, 0,5.
#' @return Objeto `matlite` con `$coeficientes`, `$ajuste`, `$odds`,
#'   `$wald_factores`, `$devianza`, `$aic`, `$confusion`,
#'   `$confusion_test` (si hay `test`), `$modelo` (objeto `glm`),
#'   `$datos_estimacion` y `$datos_test`.
#' @examples
#' \dontrun{
#' completo <- lite_logit(muestras$estimacion, LICENCIA,
#'                        explicativas = c(SOLVENCIA, IPUNT, TINCID, IFIDE,
#'                                         FLOTA, RUTAS, EXPERIENCIA, EFLO),
#'                        test = muestras$test)
#' completo
#' completo$odds
#' }
#' @export
lite_logit <- function(datos, dependiente, explicativas, test = NULL, umbral = 0.5) {
  dep <- rlang::as_name(rlang::ensym(dependiente))
  if (!dep %in% names(datos)) stop("La variable ", dep, " no existe en el data frame.", call. = FALSE)
  pos <- tidyselect::eval_select(rlang::enquo(explicativas), datos)
  expl <- setdiff(names(pos), dep)
  if (length(expl) == 0) stop("Indica al menos una variable explicativa.", call. = FALSE)

  y <- datos[[dep]]
  if (!is.factor(y)) y <- factor(y)
  if (nlevels(y) != 2) stop("La dependiente ", dep, " debe tener exactamente dos categor\u00edas.", call. = FALSE)
  datos[[dep]] <- y
  if (!is.null(test)) test[[dep]] <- factor(test[[dep]], levels = levels(y))
  for (v in expl) {
    if (is.character(datos[[v]])) {
      datos[[v]] <- factor(datos[[v]])
      if (!is.null(test)) test[[v]] <- factor(test[[v]], levels = levels(datos[[v]]))
      message("Aviso: ", v, " se ha convertido en factor (referencia: ",
              levels(datos[[v]])[1], "). Para elegir la referencia, usa lite_factor().")
    }
  }

  formula <- stats::as.formula(paste(dep, "~", paste(expl, collapse = " + ")))
  message("Modelo estimado: ", deparse1(formula), "\n",
          "Probabilidad modelizada: P(", dep, " = ", levels(y)[2], ").  ",
          "Casos de estimaci\u00f3n: ", nrow(datos),
          if (!is.null(test)) paste0(".  Casos de test: ", nrow(test)) else "", ".")

  modelo <- eval(bquote(stats::glm(.(formula), data = datos,
                                   family = stats::binomial(link = "logit"))))
  res <- presentar_logit(modelo, datos, test, umbral, "Modelo Logit Binomial")
  nuevo_matlite(res,
                mostrar = c("coeficientes", "ajuste", "odds", "wald_factores",
                            "devianza", "aic", "confusion", "confusion_test"),
                titulo = "Modelo logit", clase = "matlite_logit")
}

#' Selección automática de variables de un modelo logit
#'
#' Aplica la selección por pasos hacia atrás basada en el AIC (`step()`)
#' al modelo estimado con [lite_logit()] y presenta el modelo reducido con
#' las mismas tablas, más una comparación con el modelo de partida.
#'
#' @param logit Resultado de [lite_logit()].
#' @param mostrar_pasos Si `TRUE` (por defecto), muestra en la consola cada
#'   paso de la selección.
#' @return Objeto `matlite` como el de [lite_logit()] para el modelo
#'   reducido, más `$comparacion`.
#' @examples
#' \dontrun{
#' reducido <- lite_logit_seleccion(completo)
#' reducido$comparacion
#' }
#' @export
lite_logit_seleccion <- function(logit, mostrar_pasos = TRUE) {
  if (!inherits(logit, "matlite_logit")) stop("`logit` debe ser el resultado de lite_logit().", call. = FALSE)
  datos <- logit$datos_estimacion   # step() reevalua el modelo con este nombre
  test <- logit$datos_test
  umbral <- logit$valores$umbral
  reducido <- stats::step(logit$modelo, trace = as.integer(mostrar_pasos))
  message("Modelo seleccionado: ", deparse1(stats::formula(reducido)))
  res <- presentar_logit(reducido, datos, test, umbral, "Modelo reducido (step)")

  v0 <- logit$valores; v1 <- res$valores
  comp <- data.frame(
    Modelo = c("Completo", "Reducido (step)"),
    Variables = c(length(attr(stats::terms(logit$modelo), "term.labels")),
                  length(attr(stats::terms(reducido), "term.labels"))),
    AIC = c(v0$aic, v1$aic), R2 = c(v0$nagelkerke, v1$nagelkerke),
    Est = c(v0$acierto_estimacion, v1$acierto_estimacion))
  cn <- c("Modelo", "Variables", "AIC", "R\u00b2 Nagelkerke", "% aciertos (estimaci\u00f3n)")
  if (!is.null(test)) {
    comp$Test <- c(v0$acierto_test, v1$acierto_test)
    cn <- c(cn, "% aciertos (test)")
  }
  res$comparacion <- MATrstars::kable_rstars(comp, caption = "Modelo completo frente a reducido",
                                             col.names = cn,
                                             digits = c(NA, 0, 4, 4, 2, 2)[seq_along(cn)])
  nuevo_matlite(res,
                mostrar = c("comparacion", "coeficientes", "ajuste", "odds", "wald_factores",
                            "confusion", "confusion_test"),
                titulo = "Modelo logit reducido", clase = "matlite_logit")
}

#' Simulación de escenarios con un modelo logit
#'
#' Calcula la probabilidad estimada (con su intervalo de confianza) y la
#' clasificación de los perfiles definidos en una hoja de Excel. Envoltorio
#' de [MATrstars::predict_from_excel_scenarios()].
#'
#' @param logit Resultado de [lite_logit()] o [lite_logit_seleccion()].
#' @param archivo Fichero Excel con los escenarios.
#' @param hoja Hoja del Excel. Por defecto, `"Escenarios"`.
#' @param id Columna que identifica cada escenario. Por defecto,
#'   `"ESCENARIO"`.
#' @param umbral Umbral de clasificación. Por defecto, el del modelo.
#' @param nivel Nivel de confianza del intervalo. Por defecto, 0,95.
#' @return Objeto `matlite` con `$tabla` y `$datos`.
#' @examples
#' \dontrun{
#' lite_logit_escenarios(completo, "escenarios_logit_rstars.xlsx")
#' }
#' @export
lite_logit_escenarios <- function(logit, archivo, hoja = "Escenarios", id = "ESCENARIO",
                                  umbral = NULL, nivel = 0.95) {
  if (!inherits(logit, "matlite_logit")) stop("`logit` debe ser el resultado de lite_logit().", call. = FALSE)
  if (!file.exists(archivo)) stop("No encuentro el fichero \"", archivo, "\".", call. = FALSE)
  umbral <- umbral %||% logit$valores$umbral
  sim <- MATrstars::predict_from_excel_scenarios(
    model = logit$modelo, train_df = logit$datos_estimacion, excel_path = archivo,
    sheet = hoja, id_col = id, umbral = umbral, level = nivel,
    caption = "Simulaci\u00f3n de escenarios")
  nuevo_matlite(list(tabla = sim$table, datos = sim$data), mostrar = "tabla",
                titulo = "Escenarios")
}
