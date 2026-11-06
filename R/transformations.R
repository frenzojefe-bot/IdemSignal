#' Filtrer un signal (passe-bas idéal)
#'
#' Supprime toutes les fréquences supérieures à fc. Refiltrer un
#' signal déjà filtré ne change rien : le filtre est idempotent.
#'
#' @param x Vecteur numérique (le signal).
#' @param fs Fréquence d'échantillonnage en Hz.
#' @param fc Fréquence de coupure en Hz.
#'
#' @return Le signal filtré, de même longueur que x.
#' @export
#'
#' @examples
#' s <- creer_signal(graine = 1)
#' y <- filtrer_signal(s$x, fs = 500, fc = 10)
filtrer_signal <- function(x, fs, fc) {
  stopifnot(is.numeric(x), fs > 0, fc > 0, fc < fs / 2)
  n <- length(x)
  spectre <- stats::fft(x)
  k <- 0:(n - 1)
  freq <- pmin(k, n - k) * fs / n
  spectre[freq > fc] <- 0
  Re(stats::fft(spectre, inverse = TRUE)) / n
}

#' Seuiller un signal
#'
#' Met à 0 toutes les valeurs dont la valeur absolue est inférieure
#' à seuil. Une valeur déjà mise à 0 le reste : l'opération est
#' idempotente.
#'
#' @param x Vecteur numérique (le signal).
#' @param seuil Seuil (positif ou nul).
#'
#' @return Le signal seuillé.
#' @export
#'
#' @examples
#' seuiller_signal(c(-0.2, 0.05, 0.5, -1), seuil = 0.1)
seuiller_signal <- function(x, seuil) {
  stopifnot(is.numeric(x), seuil >= 0)
  x[abs(x) < seuil] <- 0
  x
}

#' Extraire une harmonique d'un signal
#'
#' Ne garde que la composante de fréquence f0 et supprime toutes les
#' autres. Cette projection est idempotente.
#'
#' @param x Vecteur numérique (le signal).
#' @param fs Fréquence d'échantillonnage en Hz.
#' @param f0 Fréquence à extraire en Hz.
#'
#' @return La composante de fréquence f0, de même longueur que x.
#' @export
#'
#' @examples
#' s <- creer_signal(graine = 1)
#' h <- extraire_harmonique(s$x, fs = 500, f0 = 20)
extraire_harmonique <- function(x, fs, f0) {
  stopifnot(is.numeric(x), fs > 0, f0 > 0, f0 < fs / 2)
  n <- length(x)
  spectre <- stats::fft(x)
  k <- 0:(n - 1)
  freq <- pmin(k, n - k) * fs / n
  spectre[abs(freq - f0) > fs / n / 2] <- 0
  Re(stats::fft(spectre, inverse = TRUE)) / n
}
