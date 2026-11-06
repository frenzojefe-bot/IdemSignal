test_that("reconnait une transformation idempotente", {
  s <- creer_signal(graine = 1)
  r <- verifier_idempotence(function(v) filtrer_signal(v, 500, 10), s$x)
  expect_true(r$idempotent)
  expect_lt(r$ecart_max, 1e-8)
})

test_that("reconnait une transformation non idempotente", {
  s <- creer_signal(graine = 1)
  r <- verifier_idempotence(function(v) v / 2, s$x)
  expect_false(r$idempotent)
  expect_equal(r$ecart_max, max(abs(s$x)) / 4)
})

test_that("seuillage et extraction sont idempotents", {
  s <- creer_signal(graine = 1)
  r1 <- verifier_idempotence(function(v) seuiller_signal(v, 0.3), s$x)
  r2 <- verifier_idempotence(function(v) extraire_harmonique(v, 500, 20), s$x)
  expect_true(r1$idempotent)
  expect_true(r2$idempotent)
})

test_that("la tolerance est respectee", {
  r <- verifier_idempotence(function(v) v / 2, c(1, 2), tol = 10)
  expect_true(r$idempotent)
})

test_that("les arguments invalides sont refuses", {
  expect_error(verifier_idempotence("abc", 1:3))
  expect_error(verifier_idempotence(abs, "a"))
})
