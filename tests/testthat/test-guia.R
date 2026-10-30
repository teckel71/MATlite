# Valores de control: los resultados de MATlite deben coincidir con los de
# R-Stars: La Guia (especificacion, seccion 4). Requiere los ficheros Excel
# del libro; si no estan disponibles, los tests se omiten.

dir_datos <- Sys.getenv("MATLITE_DATOS", unset = "")

ruta <- function(f) file.path(dir_datos, f)
hay_datos <- function(f) nzchar(dir_datos) && file.exists(ruta(f))

silencio <- function(expr) {
  utils::capture.output(res <- suppressMessages(expr))
  res
}

test_that("Cap. Lite 3 (Guia 5): analisis previo", {
  skip_if_not(hay_datos("interestelar_100.xlsx"))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  d <- silencio(lite_importa(ruta("interestelar_100.xlsx")))
  expect_equal(dim(d), c(104, 28))

  m  <- silencio(lite_na(d, RENECO, accion = "eliminar"))
  ms <- silencio(lite_outliers(m, RENECO, accion = "eliminar"))
  expect_equal(nrow(m), 103)
  expect_equal(nrow(ms), 101)
  expect_false("photon pack freight" %in% tolower(rownames(m)))
  expect_false(any(c("jovian logistics", "sandworm freight") %in% tolower(rownames(ms))))

  r <- silencio(lite_describe(ms, RENECO))
  expect_equal(r$valores$W, 0.9949, tolerance = 1e-4)
  expect_equal(r$valores$p, 0.9715, tolerance = 1e-4)

  vars <- c("RENECO", "ACTIVO", "MARGEN", "RES")
  m2  <- silencio(lite_na(d, dplyr::all_of(vars), accion = "eliminar"))
  m2s <- silencio(lite_outliers(m2, dplyr::all_of(vars), accion = "eliminar"))
  expect_equal(nrow(m2), 101)
  expect_equal(nrow(m2s), 91)
})

test_that("Cap. Lite 4 (Guia 6): componentes principales", {
  skip_if_not(hay_datos("interestelar_100.xlsx"))
  d <- silencio(lite_importa(ruta("interestelar_100.xlsx")))
  s  <- silencio(lite_na(d[, c("IDIVERSE", "IFIDE", "IDIG")], accion = "eliminar"))
  ss <- silencio(lite_outliers(s, accion = "eliminar"))
  expect_equal(nrow(s), 102)
  expect_equal(nrow(ss), 89)

  cp <- silencio(lite_acp(ss))
  expect_equal(unname(cp$valores$kmo), 0.688, tolerance = 1e-3)
  expect_equal(unname(cp$valores$msa), c(0.649, 0.773, 0.672), tolerance = 1e-3)
  expect_equal(cp$valores$sdev[1], 1.4614, tolerance = 1e-4)
  expect_equal(unname(cp$valores$importancia[1:2, 3]), c(0.7119, 0.8876), tolerance = 1e-4)
  expect_equal(unname(abs(cp$valores$cargas[, 1])), c(0.601, 0.544, 0.586), tolerance = 1e-3)
  expect_equal(cp$n_retener, 1)

  r1 <- silencio(lite_acp_ranking(cp))
  expect_equal(tolower(rownames(r1$puntuaciones)[1]), "shuttlepod movers")
  expect_equal(r1$puntuaciones$PC1[1], 2.662, tolerance = 1e-3)
  r2 <- silencio(lite_acp_ranking(cp, datos = s))
  expect_equal(tolower(rownames(r2$puntuaciones)[1]), "arrakis freight")
  expect_equal(r2$puntuaciones$PC1[1], 10.695, tolerance = 1e-3)
  r3 <- silencio(lite_acp_ranking(silencio(lite_acp(s))))
  expect_equal(r3$puntuaciones$PC1[1], 5.999, tolerance = 1e-3)
})

test_that("Cap. Lite 5 (Guia 7.3): cluster de Ward", {
  skip_if_not(hay_datos("interestelar_25.xlsx"))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  d <- silencio(lite_importa(ruta("interestelar_25.xlsx")))
  s <- d[, c("IDIVERSE", "IFIDE", "IDIG")]
  ex <- silencio(lite_cluster_ward(s))
  expect_equal(ex$k_sugerido, 8)
  gr <- silencio(lite_cluster_ward(s, k = 5))
  expect_equal(gr$valores$tamanos, c(2, 14, 7, 1, 1))
  g <- split(tolower(rownames(gr$datos)), gr$datos$grupo)
  expect_setequal(g[["1"]], c("hyperion star haulage", "event horizon haulage"))
  expect_equal(g[["4"]], "chakotay cargo systems")
  expect_equal(g[["5"]], "home one cargo")
})

test_that("Cap. Lite 6 (Guia 10): logit binomial", {
  skip_if_not(hay_datos("licencias_100.xlsx"))
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  L <- silencio(lite_importa(ruta("licencias_100.xlsx")))
  L <- silencio(lite_factor(L, EFLO, niveles = c("ANTIGUA", "MADURA", "RENOVADA")))
  L <- silencio(lite_factor(L, LICENCIA))
  mu <- silencio(lite_particion(L, 80, dependiente = LICENCIA))
  expect_equal(as.vector(table(mu$estimacion$LICENCIA)), c(52, 28))
  expect_equal(as.vector(table(mu$test$LICENCIA)), c(8, 12))

  co <- silencio(lite_logit(mu$estimacion, LICENCIA,
                            c(SOLVENCIA, IPUNT, TINCID, IFIDE, FLOTA, RUTAS, EXPERIENCIA, EFLO),
                            test = mu$test))
  v <- co$valores
  expect_equal(v$aic, 92.5858, tolerance = 1e-4)
  expect_equal(v$aic_nulo, 105.5915, tolerance = 1e-4)
  expect_equal(v$nagelkerke, 0.4425, tolerance = 1e-3)
  expect_equal(v$wald$Chi2, 0.1801, tolerance = 1e-3)
  expect_equal(v$wald$p, 0.9139, tolerance = 1e-3)
  expect_equal(c(v$acierto_estimacion, v$acierto_test), c(80, 80))
  expect_equal(unname(v$coef["TINCID", 4]), 0.0072, tolerance = 1e-2)

  es <- silencio(lite_logit_escenarios(co, ruta("escenarios_logit_rstars.xlsx")))
  expect_equal(es$datos$.prob, c(0.00197, 0.98547, 0.35276), tolerance = 1e-3)
  expect_equal(es$datos$.clasif, c(0, 1, 0))

  re <- silencio(lite_logit_seleccion(co, mostrar_pasos = FALSE))
  expect_equal(attr(stats::terms(re$modelo), "term.labels"),
               c("SOLVENCIA", "IPUNT", "TINCID", "IFIDE"))
  expect_equal(re$valores$aic, 85.7003, tolerance = 1e-4)
  expect_equal(c(re$valores$acierto_estimacion, re$valores$acierto_test), c(76.25, 80))
})

test_that("Cap. Lite 7 (Guia 11.2): tablas de contingencia", {
  skip_if_not(hay_datos("interestelar_300.xlsx"))
  I <- silencio(lite_importa(ruta("interestelar_300.xlsx")))[, c("GALAXIA", "FJUR", "EFLO")]
  c1 <- silencio(lite_contingencia(I, GALAXIA, FJUR))
  expect_equal(c1$valores$chi2, 18.209, tolerance = 1e-4)
  expect_equal(c1$valores$gl, 8)
  expect_equal(c1$valores$p, 0.0197, tolerance = 1e-2)
  expect_equal(c1$valores$C, 0.239, tolerance = 1e-2)
  expect_equal(c1$valores$V, 0.174, tolerance = 1e-2)
  c2 <- silencio(lite_contingencia(I, FJUR, EFLO))
  expect_equal(c2$valores$p, 0.4255, tolerance = 1e-3)
  I2 <- silencio(lite_recodifica(I, GALAXIA, c("Gran Nube de Magallanes" = "GN Magallanes")))
  expect_true("GN Magallanes" %in% I2$GALAXIA)
})

test_that("Cap. Lite 8 (Guia 12): escalado multidimensional", {
  skip_if_not(hay_datos("percepcion_interestelar.xlsx"))
  P <- silencio(lite_importa(ruta("percepcion_interestelar.xlsx")))
  e0 <- silencio(lite_emd_dimensiones(P))
  expect_equal(round(e0$valores$stress, 4), c(0.0422, 0.0196, 0.0038, 0.0024, 0.0010, 0.0000))
  e2 <- silencio(lite_emd(P))
  expect_equal(unname(e2$valores$correlaciones[c("PRECIO", "INNOVACION", "PUNTUALIDAD"), 1]),
               c(0.986, -0.980, -0.969), tolerance = 1e-3)
  expect_equal(e2$valores$niter, 20)
  e3 <- silencio(lite_emd(P, ndim = 3))
  expect_equal(e3$valores$stress, 0.0038, tolerance = 1e-2)
})
