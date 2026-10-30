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
