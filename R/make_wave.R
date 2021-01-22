
#' Make Wave
#'
#' This function create waves, savable as a wave file with \code{\link{save.wave}} or playable with if possible depending on the audio package,
#' from a vector of string representing notes and a numeric vector representing how long are each notes
#' @param note_list vector of string containing the list of notes that should get played,
#'  should have the same length as \code{durations} .
#' @param durations numeric vector containing the list of the duration of each notes in second,
#' should have the same length as \code{note_list} .
#' @param harmonics numeric vector containing ratio between harmonics the rank is determine by the place of the number in the vector.
#' Harmonics vectors can be build using \code{\link{build_harmonic}} or be found in the data frame \code{harmonics} which come with this package.
#' @export
#' @example 
#' \dontrun{makeSound(c("B5","A5","G5","F#5","E5","D5","C#5","B4"),c(1,1,1,1,1,1,1,1))}

make_wave <- function(note_list, durations, harmonics = c(1)) {
  durations <- durations + c(rep(0.1, length(durations) - 1), 0)
  fade_in <- make_fade(0.1)
  fade_out <- rev(fade_in)

  sum_wav <- function(u, v) sum_wave(u, v, length(fade_in))

  base_freq <- get_freq(note_list)

  Reduce("+", lapply(seq_len(length(harmonics)), function(i) {
    harmonics[i] * Reduce(sum_wav, lapply(
      mapply(make_sin, i * base_freq, durations, SIMPLIFY = F),
      apply_fade_in_out, fade_in, fade_out
    ))
  }))
}
