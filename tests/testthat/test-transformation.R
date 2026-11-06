test_that("filtrer_signal supprime la composante a 20 Hz", {
  s <- creer_signal(bruit = 0)
  y <- filtrer_signal(s$x, fs = 500, fc = 10)
  expect_equal(y, sin(2 * pi * 5 * s$t))
})

test_that("filtrer_signal est idempotent", {
  s <- creer_signal(graine = 1)
  y <- filtrer_signal(s$x, fs = 500, fc = 10)
  expect_equal(filtrer_signal(y, fs = 500, fc = 10), y)
})

test_that("filtrer_signal refuse une coupure trop haute", {
  expect_error(filtrer_signal(1:10, fs = 500, fc = 300))
})

test_that("seuiller_signal met a zero les petites valeurs", {
  expect_equal(seuiller_signal(c(-0.2, 0.05, 0.5, -1), 0.1),
               c(-0.2, 0, 0.5, -1))
})

test_that("seuiller_signal est idempotent", {
  x <- creer_signal(graine = 1)$x
  y <- seuiller_signal(x, 0.3)
  expect_equal(seuiller_signal(y, 0.3), y)
})

test_that("seuiller_signal refuse un seuil negatif", {
  expect_error(seuiller_signal(c(1, 2), -1))
})

test_that("extraire_harmonique isole la composante a 20 Hz", {
  s <- creer_signal(bruit = 0)
  h <- extraire_harmonique(s$x, fs = 500, f0 = 20)
  expect_equal(h, 0.5 * sin(2 * pi * 20 * s$t))
})

test_that("extraire_harmonique est idempotent", {
  s <- creer_signal(graine = 1)
  h <- extraire_harmonique(s$x, fs = 500, f0 = 20)
  expect_equal(extraire_harmonique(h, fs = 500, f0 = 20), h)
})

test_that("extraire_harmonique refuse une frequence trop haute", {
  expect_error(extraire_harmonique(1:10, fs = 500, f0 = 300))
})
