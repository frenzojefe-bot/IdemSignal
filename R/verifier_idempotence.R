#' Vérifier qu'une transformation est idempotente
#'
#' Applique la transformation f une fois puis deux fois au signal x,
#' et mesure le plus grand écart entre les deux résultats.
#'
#' @param f Une fonction qui prend un signal et renvoie un signal de
#'   même longueur.
#' @param x Vecteur numérique (le signal).
#' @param tol Écart maximal accepté pour déclarer f idempotente.
#'
#' @return Une liste de deux éléments : idempotent (TRUE ou FALSE)
#'   et ecart_max (le plus grand écart entre f(x) et f(f(x))).
#' @export
#'
#' @examples
#' s <- creer_signal(graine = 1)
#' verifier_idempotence(function(v) filtrer_signal(v, 500, 10), s$x)
#' verifier_idempotence(function(v) v / 2, s$x)
verifier_idempotence <- function(f, x, tol = 1e-8) {
  stopifnot(is.function(f), is.numeric(x), tol >= 0)
  y1 <- f(x)
  y2 <- f(y1)
  stopifnot(is.numeric(y1), length(y1) == length(y2))
  ecart <- max(abs(y2 - y1))
  list(idempotent = isTRUE(ecart <= tol), ecart_max = ecart)
}
