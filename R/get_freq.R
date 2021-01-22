
#' Get frequency of a note
#' 
#' Yeahh I'll make that one later
#' @export

get_freq <- function(note){
  
  notes <- c("A" = 0,"B" = 2,"C" = 3,"D" = 5,"E" = 7,"F" = 8,"G" = 10)
  
  noteVal <- notes[substr(note,1,1)] + grepl("#",note) - grepl("b",note)
  
  octave <- suppressWarnings(as.numeric(substring(note,nchar(note))))
  
  octave[is.na(octave)] <- 4
  
  freq <- 27.5 * 2^(noteVal/12 + octave - (noteVal>=3))
  
  return(as.numeric(freq))
  
}