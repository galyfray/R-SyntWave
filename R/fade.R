
#' Make fade
#'
#' A function to compute nice-ish fade in as a vector of number between 0 and 1
#' @param duration A float,double or int giving the duration of the fade in second
#' @param sample An int representing the sample rate in Hz. Standard sample rates are 11 025, 22 050, 44 100, 48 000 and 96 000
#' @aliases make_fade
#' @export
#' @examples
#' \dontrun{fade <- make_fade(1)}
#' \dontrun{fade_48k <- make_fade(0.75,48000)}

make_fade <- function(duration,sample =44100 ) {
  p <- sin(seq.int(1 / 6, 1 / 2, 1 / (sample * duration * 1.5)) * pi)
  c(1 - rev(p), p[-1])
}

#' Apply fade-in
#' 
#' Function used to apply a fade in effect to a wave
#' @param  wave A numeric vector longer than fade as made by \code{\link{make_wave}} for example
#' @param fade A numeric vector shorter than wave as made by \code{\link{make_fade}} for example
#' @export
#' @seealso \code{\link{apply_fade_out}} \code{\link{apply_fade_in_out}} 

apply_fade_in <- function(wave, fade) {
  wave * c(fade, rep(1, length(wave) - length(fade)))
}

#' Apply fade-out
#'
#' Function used to apply a fade out effect to a wave
#' @param  wave A numeric vector longer than fade as made by \code{\link{make_wave}} for example
#' @param fade A numeric vector shorter than wave as made by \code{\link{make_fade}} for example
#' @export
#' @seealso \code{\link{apply_fade_in}} \code{\link{apply_fade_in_out}} 

apply_fade_out <- function(wave, fade) {
  wave * c(rep(1, length(wave) - length(fade)), fade)
}

#' Apply fade-out and in
#'
#' Function used to apply a fade out and in effect to a wave
#' @param  wave A numeric vector longer than fade as made by \code{\link{make_wave}} for example
#' @param fade_in A numeric vector shorter than wave as made by \code{\link{make_fade}} for example
#' @param fade_out A numeric vector shorter than wave as made by \code{\link{make_fade}} for example
#' @export
#' @seealso \code{\link{apply_fade_in}} \code{\link{apply_fade_out}} 

apply_fade_in_out <- function(wave, fade_in, fade_out = rev(fade_in)) {
  wave * c(
    fade_in,
    rep(1, length(wave) - length(fade_in) - length(fade_out)),
    fade_out
  )
}
