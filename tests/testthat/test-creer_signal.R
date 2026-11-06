test_that("le signal a la bonne forme", {
  s <- creer_signal()
  expect_equal(nrow(s), 500)
  expect_named(s, c("t", "x"))
  expect_equal(s$t[1], 0)
})

test_that("sans bruit, on retrouve la sinusoide exacte", {
  s <- creer_signal(frequences = 5, amplitudes = 1, bruit = 0)
  expect_equal(s$x[1], 0)
  expect_equal(s$x[26], 1)
})

test_that("la graine rend le signal reproductible", {
  expect_equal(creer_signal(graine = 1), creer_signal(graine = 1))
})

test_that("les arguments invalides sont refuses", {
  expect_error(creer_signal(frequences = 300, amplitudes = 1))
  expect_error(creer_signal(frequences = c(5, 20), amplitudes = 1))
})
