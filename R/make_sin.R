
make_sin <- function(freq, duration, sample = 44100) {
  f <- freq * pi * 2
  sin(seq.int(0, duration * f, f / sample))
}

#' Sum wave
#'
#' Utility function used to partially sum up two waves
#' @param wave1 A numeric vector longer than fade as made by \code{\link{make_wave}} for example
#' @param wave2 A numeric vector longer than fade as made by \code{\link{make_wave}} for example
#' @param nb_intersect A integer representing the number a of point that should be sum in both waves should be lower than \code{min(length(wave1),length(wave2))}
#' @export
#' @example
#' sum_wave(rep(1,5),rep(2,6),3)

sum_wave <- function(wave1, wave2, nb_intersect) {
  c(wave1[1:(length(wave1) - nb_intersect)],
    wave1[(length(wave1) - nb_intersect + 1):length(wave1)] +
      wave2[1:nb_intersect],
    wave2[(nb_intersect + 1):length(wave2)])
}
