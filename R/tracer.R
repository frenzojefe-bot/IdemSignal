#' Tracer un signal
#'
#' Dessine un signal en fonction du temps, dans le périphérique
#' graphique actif (l'onglet Plots de RStudio).
#'
#' @param s Un data.frame avec les colonnes t et x, comme celui
#'   renvoyé par [creer_signal()].
#' @param titre Titre du graphique.
#' @param couleur Couleur de la courbe.
#'
#' @return Invisiblement le signal s.
#' @export
#'
#' @examples
#' tracer_signal(creer_signal(graine = 1))
tracer_signal <- function(s, titre = "Signal", couleur = "steelblue") {
  stopifnot(is.data.frame(s), all(c("t", "x") %in% names(s)))
  graphics::plot(s$t, s$x, type = "l", col = couleur, lwd = 2,
                 xlab = "Temps (s)", ylab = "Amplitude", main = titre)
  graphics::abline(h = 0, col = "grey70", lty = 3)
  invisible(s)
}

#' Comparer un signal avant et après traitement
#'
#' Superpose le signal initial (en gris) et le signal traité (en rouge).
#'
#' @param s Un data.frame avec les colonnes t et x.
#' @param y Le signal traité, de même longueur que s$x.
#' @param titre Titre du graphique.
#'
#' @return Invisiblement le signal traité y.
#' @export
#'
#' @examples
#' s <- creer_signal(graine = 1)
#' y <- filtrer_signal(s$x, fs = 500, fc = 10)
#' tracer_signal_filtre(s, y)
tracer_signal_filtre <- function(s, y,
                                 titre = "Signal avant et apr\u00e8s traitement") {
  stopifnot(is.data.frame(s), all(c("t", "x") %in% names(s)),
            is.numeric(y), length(y) == nrow(s))
  graphics::plot(s$t, s$x, type = "l", col = "grey60", lwd = 1,
                 xlab = "Temps (s)", ylab = "Amplitude", main = titre,
                 ylim = range(c(s$x, y), finite = TRUE))
  graphics::lines(s$t, y, col = "firebrick", lwd = 3)
  graphics::legend("topright",
                   legend = c("Signal initial", "Signal trait\u00e9"),
                   col = c("grey60", "firebrick"), lwd = c(1, 3), bty = "n")
  invisible(y)
}
#' Montrer l'effet de plusieurs applications d'une transformation
#'
#' Applique f une, deux, ..., n fois au signal et superpose les
#' résultats. Si f est idempotente, toutes les courbes sont identiques.
#'
#' @param f Une fonction qui prend un signal et renvoie un signal.
#' @param s Un data.frame avec les colonnes t et x.
#' @param n Nombre d'applications à tracer (entre 1 et 5).
#' @param titre Titre du graphique.
#'
#' @return Invisiblement la liste des n résultats.
#' @export
#'
#' @examples
#' s <- creer_signal(graine = 1)
#' tracer_iterations(function(v) filtrer_signal(v, 500, 10), s)
#' tracer_iterations(function(v) v / 2, s)
tracer_iterations <- function(f, s, n = 3,
                              titre = "R\u00e9sultat apr\u00e8s plusieurs applications") {
  stopifnot(is.function(f), is.data.frame(s), all(c("t", "x") %in% names(s)),
            n >= 1, n <= 5)
  resultats <- vector("list", n)
  y <- s$x
  for (i in seq_len(n)) {
    y <- f(y)
    resultats[[i]] <- y
  }
  couleurs <- c("steelblue", "firebrick", "darkgreen", "orange",
                "purple")[seq_len(n)]
  epaisseurs <- c(6, 4, 3, 2, 1)[seq_len(n)]
  graphics::plot(s$t, resultats[[1]], type = "l", col = couleurs[1],
                 lwd = epaisseurs[1], xlab = "Temps (s)", ylab = "Amplitude",
                 main = titre, ylim = range(unlist(resultats), finite = TRUE))
  for (i in seq_len(n)[-1]) {
    graphics::lines(s$t, resultats[[i]], col = couleurs[i],
                    lwd = epaisseurs[i], lty = 2)
  }
  graphics::legend("topright", legend = paste("Application", seq_len(n)),
                   col = couleurs, lwd = epaisseurs, bty = "n")
  invisible(resultats)
}
#' Tracer les harmoniques d'un signal
#'
#' Extrait chaque fréquence demandée avec [extraire_harmonique()] et
#' trace chaque composante dans son propre graphique.
#'
#' @param s Un data.frame avec les colonnes t et x.
#' @param frequences Fréquences à extraire, en Hz (de 1 à 4 valeurs).
#'
#' @return Invisiblement la liste des harmoniques extraites.
#' @export
#'
#' @examples
#' tracer_harmoniques(creer_signal(graine = 1), c(5, 20))
tracer_harmoniques <- function(s, frequences) {
  stopifnot(is.data.frame(s), all(c("t", "x") %in% names(s)),
            is.numeric(frequences), length(frequences) >= 1,
            length(frequences) <= 4)
  fs <- 1 / (s$t[2] - s$t[1])
  harmoniques <- lapply(frequences, function(f0) {
    extraire_harmonique(s$x, fs, f0)
  })
  ancien <- graphics::par(mfrow = c(length(frequences), 1), mar = c(4, 4, 2, 1))
  on.exit(graphics::par(ancien))
  for (i in seq_along(frequences)) {
    graphics::plot(s$t, harmoniques[[i]], type = "l", col = "steelblue",
                   lwd = 2, xlab = "Temps (s)", ylab = "Amplitude",
                   main = paste("Harmonique \u00e0", frequences[i], "Hz"))
    graphics::abline(h = 0, col = "grey70", lty = 3)
  }
  invisible(harmoniques)
}

#' Tracer le spectre d'un signal
#'
#' Montre l'amplitude de chaque fréquence présente dans le signal.
#'
#' @param s Un data.frame avec les colonnes t et x.
#' @param fmax Fréquence maximale affichée, en Hz.
#' @param titre Titre du graphique.
#'
#' @return Invisiblement un data.frame avec les colonnes frequence et
#'   amplitude.
#' @export
#'
#' @examples
#' tracer_spectre(creer_signal(graine = 1))
tracer_spectre <- function(s, fmax = 50, titre = "Spectre du signal") {
  stopifnot(is.data.frame(s), all(c("t", "x") %in% names(s)),
            nrow(s) > 2, fmax > 0)
  n <- nrow(s)
  fs <- 1 / (s$t[2] - s$t[1])
  X <- stats::fft(s$x)
  k <- 0:(n %/% 2)
  amplitude <- 2 * Mod(X[k + 1]) / n
  amplitude[1] <- amplitude[1] / 2
  if (n %% 2 == 0) {
    amplitude[length(amplitude)] <- amplitude[length(amplitude)] / 2
  }
  frequence <- k * fs / n
  graphics::plot(frequence, amplitude, type = "h", lwd = 2, col = "steelblue",
                 xlim = c(0, min(fmax, fs / 2)),
                 xlab = "Fr\u00e9quence (Hz)", ylab = "Amplitude", main = titre)
  invisible(data.frame(frequence = frequence, amplitude = amplitude))
}
