#' Créer un signal artificiel
#'
#' Génère un signal formé de la somme de sinusoïdes, auquel on ajoute
#' un bruit aléatoire gaussien.
#'
#' @param duree Durée du signal en secondes.
#' @param fs Fréquence d'échantillonnage en Hz (points par seconde).
#' @param frequences Fréquences des sinusoïdes, en Hz.
#' @param amplitudes Amplitudes des sinusoïdes (même longueur que
#'   frequences).
#' @param bruit Écart-type du bruit ajouté (0 pour aucun bruit).
#' @param graine Nombre fixant le hasard pour obtenir toujours le même
#'   signal (NULL pour ne rien fixer).
#'
#' @return Un data.frame de deux colonnes : t (temps en secondes) et
#'   x (valeur du signal).
#' @export
#'
#' @examples
#' s <- creer_signal(graine = 1)
#' head(s)
creer_signal <- function(duree = 1, fs = 500,
                         frequences = c(5, 20), amplitudes = c(1, 0.5),
                         bruit = 0.2, graine = NULL) {
  stopifnot(duree > 0, fs > 0, bruit >= 0,
            length(frequences) == length(amplitudes),
            all(frequences < fs / 2))
  if (!is.null(graine)) set.seed(graine)
  n <- round(duree * fs)
  t <- (0:(n - 1)) / fs
  x <- numeric(n)
  for (k in seq_along(frequences)) {
    x <- x + amplitudes[k] * sin(2 * pi * frequences[k] * t)
  }
  x <- x + bruit * stats::rnorm(n)
  data.frame(t = t, x = x)
}
